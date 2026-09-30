#requires -Version 5.1
<#
.SYNOPSIS
    TypR — installation en une commande (Windows).

.DESCRIPTION
    powershell -c "irm https://we-data-ch.github.io/typr.github.io/install/install.ps1 | iex"

    Le format est .zip et non .tar.gz : Expand-Archive est le seul outil
    d'extraction présent sur un Windows sans rien installer, alors qu'il
    faudrait tar.exe (livré à partir de Windows 10 1803, absent de Windows 7/8.1
    et de Windows Server 2012).

    Cible %LOCALAPPDATA%\Programs\typr : convention utilisateur moderne, aucun
    droit administrateur. Le PATH utilisateur est mis à jour par registre, donc
    il ne prend effet qu'au prochain terminal — le script le dit, et met
    également à jour $env:Path de la session courante pour que `typr --version`
    fonctionne immédiatement.

    Le binaire n'est pas signé. Un fichier posé par Expand-Archive ne porte pas
    l'attribut Mark-of-the-Web (seuls les navigateurs et les zip téléchargés en
    le posent) : pas de SmartScreen, contrairement à un exécutable téléchargé
    puis double-cliqué.

.PARAMETER Version
    Tag à installer (ex. v0.5.12). Par défaut, la dernière version stable.

.PARAMETER Channel
    stable (défaut) ou beta.

.PARAMETER DryRun
    Affiche ce qui serait fait, ne télécharge rien.

.PARAMETER Gnu
    Sans effet sur Windows ; accepté pour que la même ligne de commande
    fonctionne sur les deux scripts.

.NOTES
    Rien n'est écrit hors de %LOCALAPPDATA% et aucun privilège administrateur
    n'est requis.
#>

[CmdletBinding()]
param(
    [string] $Version,
    [ValidateSet('stable', 'beta')]
    [string] $Channel = 'stable',
    [switch] $DryRun,
    [switch] $Gnu
)

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

# ---------------------------------------------------------------------------
# Configuration
#
# Les variables d'environnement ci-dessous sont surchargées par les tests, qui
# servent une release locale : sans cela, tester le refus d'un SHA falsifié
# supposerait de falsifier une vraie release publiée.
# ---------------------------------------------------------------------------
$Repo  = if ($env:TYPR_INSTALL_REPO)  { $env:TYPR_INSTALL_REPO }  else { 'we-data-ch/typr' }
$Origin = if ($env:TYPR_INSTALL_ORIGIN) { $env:TYPR_INSTALL_ORIGIN } else { 'https://github.com' }
$Docs  = if ($env:TYPR_INSTALL_DOCS)  { $env:TYPR_INSTALL_DOCS }  else { 'https://we-data-ch.github.io/typr.github.io' }

$DownloadBase = "$Origin/$Repo/releases/download"
$LatestUrl    = "$Origin/$Repo/releases/latest"
$ReleasesFeed = "$Origin/$Repo/releases.atom"

$Verify = if ($env:TYPR_INSTALL_VERIFY) { $env:TYPR_INSTALL_VERIFY } else { '1' }
if ($Verify -notin @('0', '1')) {
    Write-Error "TYPR_INSTALL_VERIFY doit valoir 0 ou 1 (reçu : '$Verify')."
    exit 2
}

$script:TempDir = $null

# `exit` ne passe pas par le `trap`, qui ne voit que les erreurs : le dossier
# temporaire est donc nettoyé explicitement ici. Sans cela, chaque échec laissait
# une archive à moitié téléchargée dans %TEMP%.
function Stop-Install {
    param([string] $Message, [int] $Code = 1)
    Remove-TempDir
    Write-Host "typr-install : $Message" -ForegroundColor Red
    exit $Code
}

function Step { param([string] $Message) Write-Host "→ $Message" }
function Note { param([string] $Message) Write-Host "  $Message" }
function Warn {
    param([string] $Message)
    Write-Host "typr-install : attention : $Message" -ForegroundColor Yellow
}

function Remove-TempDir {
    if ($script:TempDir -and (Test-Path -LiteralPath $script:TempDir)) {
        Remove-Item -LiteralPath $script:TempDir -Recurse -Force -ErrorAction SilentlyContinue
    }
}

# Nettoyage du dossier temporaire en cas d'erreur, avec un code de retour non
# nul : sous `irm | iex`, un `exit 0` ferait croire à une installation réussie.
trap {
    Remove-TempDir
    Write-Error $_
    exit 1
}

# ---------------------------------------------------------------------------
# HTTP
#
# `HttpClient` plutôt que `Invoke-WebRequest` : la classe .NET Framework est
# présente à l'identique dans Windows PowerShell 5.1 et PowerShell 7, alors que
# les paramètres des cmdlets ont divergé entre les deux — `-UseBasicParsing`
# n'a pas le même sens partout, et `-MaximumRedirection 0` se comporte
# différemment, ou n'existe pas. Ici il n'y a aucun drapeau à se tromper.
function New-Client {
    param([bool] $AllowRedirect = $true)
    $handler = [System.Net.Http.HttpClientHandler]::new()
    $handler.AllowAutoRedirect = $AllowRedirect
    $client = [System.Net.Http.HttpClient]::new($handler)
    $client.DefaultRequestHeaders.UserAgent.ParseAdd('typr-install')
    return $client
}

function Get-RedirectLocation {
    param([string] $Url)

    # On veut l'en-tête de redirection, pas la destination : c'est lui qui porte
    # le tag, et `/releases/latest` exclut les prereleases — exactement la
    # sémantique du canal stable, sans dépendre de jq ni de l'API GitHub.
    $client = New-Client -AllowRedirect $false
    try {
        $request = [System.Net.Http.HttpRequestMessage]::new(
            [System.Net.Http.HttpMethod]::Head, $Url)
        $response = $client.SendAsync($request).GetAwaiter().GetResult()
        if ($null -ne $response.Headers.Location) {
            return $response.Headers.Location.ToString()
        }
        return $null
    }
    catch {
        return $null
    }
    finally {
        $client.Dispose()
    }
}

function Get-String {
    param([string] $Url)

    $client = New-Client
    try {
        $response = $client.GetAsync($Url).GetAwaiter().GetResult()
        $response.EnsureSuccessStatusCode() | Out-Null
        return $response.Content.ReadAsStringAsync().GetAwaiter().GetResult()
    }
    finally {
        $client.Dispose()
    }
}

# Renvoie $false au lieu d'appeler Stop-Install : à chaque appelant de dire ce
# que l'absence du fichier signifie.
function Try-Save-File {
    param([string] $Url, [string] $Path)

    # On écrit dans un fichier temporaire puis on renomme : si le téléchargement
    # est interrompu, il ne reste pas un fichier à moitié écrit sous le nom
    # définitif — l'archive est vérifiée avant d'être installée de toute façon.
    $partial = "$Path.part"
    $client = New-Client
    try {
        $response = $client.GetAsync($Url).GetAwaiter().GetResult()
        $response.EnsureSuccessStatusCode() | Out-Null

        # `$source` et non `$input` : ce dernier est une variable automatique
        # de PowerShell (l'ancien `$input` du pipeline), qui peut être réécrite
        # par du code tiers chargé avant nous — ou par nous, ce qui ferait lire
        # un $null à CopyTo et masquerait le téléchargement en « échec ».
        $source = $response.Content.ReadAsStreamAsync().GetAwaiter().GetResult()
        $target = [System.IO.File]::Create($partial)
        try { $source.CopyTo($target) }
        finally { $target.Dispose(); $source.Dispose() }

        if (Test-Path -LiteralPath $Path) { Remove-Item -LiteralPath $Path -Force }
        Move-Item -LiteralPath $partial -Destination $Path -Force

        # Explicite, et non implicite : une fonction PowerShell renvoie ce que
        # son `try` a émis, or un téléchargement réussi n'émet rien. Sans ce
        # `return`, Try-Save-File rendrait $null — donc falsy — et l'appelant
        # lirait un succès comme un échec.
        return $true
    }
    catch {
        if (Test-Path -LiteralPath $partial) {
            Remove-Item -LiteralPath $partial -Force -ErrorAction SilentlyContinue
        }
        return $false
    }
    finally {
        $client.Dispose()
    }
}

function Save-File {
    param([string] $Url, [string] $Path)

    if (-not (Try-Save-File $Url $Path)) {
        Stop-Install "téléchargement échoué : $Url`n  Vérifier le réseau, ou réessayer avec -Version vX.Y.Z."
    }
}

# ---------------------------------------------------------------------------
# Version
# ---------------------------------------------------------------------------
function Resolve-Version {
    if ($Version) {
        $tag = $Version -replace '^v', ''
        return "v$tag"
    }

    if ($Channel -eq 'stable') {
        $location = Get-RedirectLocation $LatestUrl
        if ($location -and $location -match '/tag/(v[^/?#]+)') { return $Matches[1] }
        Stop-Install "impossible de déterminer la dernière version stable.`n  URL interrogée : $LatestUrl`n  Pour épingler une version : -Version vX.Y.Z"
    }

    # Le canal beta doit lister les releases et filtrer. Le flux Atom en liste
    # les dix dernières, prereleases comprises, et se parse sans dépendance.
    # Les suffixes sont ceux que release.yml marque `prerelease`.
    $xml = Get-String $ReleasesFeed
    $tags = [regex]::Matches($xml, 'Repository/\d+/(v[^<]+)') |
            ForEach-Object { $_.Groups[1].Value }
    $prerelease = $tags | Where-Object { $_ -match '-(alpha|beta|rc)[.0-9]' } | Select-Object -First 1
    if (-not $prerelease) {
        Stop-Install "aucune prerelease trouvée sur $ReleasesFeed.`n  Le flux Atom ne liste que les dix dernières releases : si la dernière`n  prerelease est plus ancienne, épinglez-la avec -Channel beta -Version vX.Y.Z"
    }
    return $prerelease
}

function Assert-Tag {
    param([string] $Tag)
    if ($Tag -notmatch '^v[0-9]') {
        Stop-Install "tag illisible : '$Tag' (attendu : vX.Y.Z)." 2
    }
}

function Resolve-Target {
    # Surcharge de test. `PROCESSOR_ARCHITECTURE` n'existe que sous Windows, donc
    # sans cette variable le chemin download → SHA → extraction → PATH ne peut
    # être exercé que sur un runner Windows. install.sh n'a pas le même besoin :
    # `uname -m` existe partout où le script Unix tourne.
    if ($env:TYPR_INSTALL_TARGET) { return $env:TYPR_INSTALL_TARGET }

    # Sous WOW64 (PowerShell 32 bits sur Windows 64), PROCESSOR_ARCHITECTURE
    # vaut x86 et PROCESSOR_ARCHITEW6432 porte le vrai. Sur ARM64, on prend le
    # binaire ARM : Windows l'émule de toute façon s'il ne l'exécute pas.
    $arch = $env:PROCESSOR_ARCHITECTURE
    if ($arch -eq 'x86' -and $env:PROCESSOR_ARCHITEW6432) { $arch = $env:PROCESSOR_ARCHITEW6432 }

    switch ($arch) {
        'AMD64' { return 'x86_64-pc-windows-msvc' }
        'ARM64' { return 'aarch64-pc-windows-msvc' }
        'x86'   {
            Stop-Install "TypR ne publie pas de binaire 32 bits (x86).`n  Un Windows 64 bits a normalement PROCESSOR_ARCHITECTURE=AMD64 : si ce`n  n'est pas le cas, le PowerShell ouvert est 32 bits — fermez-le et rouvrez-le."
        }
        default {
            Stop-Install "architecture non supportée : '$arch'.`n  TypR publie des binaires x86_64 et aarch64 pour Windows."
        }
    }
}

function Get-OsName {
    if ($env:OS) { return $env:OS }
    # `$env:OS` n'existe que sous Windows ; sous PowerShell 7 sur Linux/macOS
    # (où le script est testé), on retombe sur la description .NET.
    if ([System.Runtime.InteropServices.RuntimeInformation]) {
        return [System.Runtime.InteropServices.RuntimeInformation]::OSDescription
    }
    return 'Windows'
}

function Resolve-InstallDir {
    if ($env:TYPR_INSTALL_DIR) { return $env:TYPR_INSTALL_DIR }
    if ($env:LOCALAPPDATA)    { return (Join-Path $env:LOCALAPPDATA 'Programs\typr') }
    Stop-Install "LOCALAPPDATA n'est pas défini : impossible de savoir où installer.`n  Définissez TYPR_INSTALL_DIR."
}

# ---------------------------------------------------------------------------
# SHA-256
# ---------------------------------------------------------------------------
# `Get-FileHash` est présent depuis PowerShell 4.0 : pas de module à charger,
# contrairement à `certutil -hashfile` qui n'est pas garanti sur une image
# serveur minimale.
#
# On isole la seule ligne de checksums.txt qui concerne notre archive, puis on
# compare les empreintes. `sha256sum -c` échouerait ici — l'équivalent Windows
# essaierait de vérifier les sept autres archives de la release, non
# téléchargées, en sortant un code d'erreur alors que la nôtre est valide.
function Test-Checksum {
    param([string] $Path, [string] $Name)

    Step 'Vérification du SHA-256'

    $sumsPath = Join-Path $script:TempDir 'checksums.txt'
    $sumsUrl = "$DownloadBase/$script:Tag/checksums.txt"
    # `Save-File` dirait « téléchargement échoué », ce qui ne dit pas *ce qui*
    # manque. Ici la réponse est toujours la même — checksums.txt est introuvable
    # ou le réseau est coupé — et l'utilisateur a besoin de la seconde phrase
    # pour distinguer une release mal publiée d'un pare-feu.
    if (-not (Try-Save-File $sumsUrl $sumsPath)) {
        Stop-Install "checksums.txt introuvable pour $($script:Tag).`n  URL : $sumsUrl`n  Le fichier liste les huit archives de la release ; son absence signifie`n  que la release est incomplète. Vérifier aussi le réseau."
    }

    # On filtre sur le NOM, pas sur le couple SHA + nom. Filtrer sur les deux
    # confondrait deux diagnostics très différents : « ce fichier ne parle pas
    # de notre archive » et « la ligne existe mais son empreinte est illisible ».
    # Le second est un fichier corrompu, le premier une release mal publiée.
    # install.sh procède dans le même ordre, pour que les deux scripts nomment la
    # même cause de la même façon.
    $line = Get-Content -LiteralPath $sumsPath |
            Where-Object { $_ -match ('\s+\*?' + [regex]::Escape($Name) + '$') } |
            Select-Object -First 1

    if (-not $line) {
        $received = (Get-Content -LiteralPath $sumsPath) -join "`n"
        Stop-Install "checksums.txt de $($script:Tag) ne contient aucune ligne pour $Name.`n  Fichier reçu :`n$received"
    }

    $expected = ($line.Trim() -split '\s+')[0]
    if ($expected -notmatch '^[0-9a-f]{64}$') {
        Stop-Install "ligne malformée dans checksums.txt pour $Name :`n  $line`n  Un SHA-256 fait 64 caractères hexadécimaux ; trouvé '$expected'."
    }

    $actual = (Get-FileHash -LiteralPath $Path -Algorithm SHA256).Hash.ToLowerInvariant()
    if ($expected -ne $actual) {
        Stop-Install "SHA-256 falsifié pour $Name.`n  attendu $expected`n  obtenu   $actual`n  L'archive a été supprimée ; rien n'est installé."
    }

    Note "SHA-256 correct ($actual)"
}

# ---------------------------------------------------------------------------
# PATH
# ---------------------------------------------------------------------------
function Test-OnPath {
    param([string] $Dir)

    $full = [System.IO.Path]::GetFullPath($Dir).TrimEnd('\')
    $separator = [System.IO.Path]::PathSeparator
    foreach ($entry in ($env:Path -split [regex]::Escape($separator))) {
        if (-not $entry) { continue }
        $normalized = [System.IO.Path]::GetFullPath($entry.Trim('"')).TrimEnd('\')
        if ($normalized -ieq $full) { return $true }
    }
    return $false
}

# Le PATH utilisateur est écrit dans le registre : il ne sera pris en compte
# qu'au prochain démarrage de terminal, ce qu'il faut dire explicitement. On
# met aussi à jour $env:Path de la session courante pour que `typr --version`
# fonctionne sans attendre.
function Add-ToUserPath {
    param([string] $Dir)

    $current = [Environment]::GetEnvironmentVariable('Path', 'User')
    if ($null -eq $current) { $current = '' }
    $separator = [System.IO.Path]::PathSeparator

    foreach ($entry in ($current -split [regex]::Escape($separator))) {
        if (-not $entry) { continue }
        if ([System.IO.Path]::GetFullPath($entry.Trim('"')).TrimEnd('\') -ieq $Dir.TrimEnd('\')) {
            return $false
        }
    }

    $new = if ([string]::IsNullOrWhiteSpace($current)) { $Dir } else { "$current$separator$Dir" }
    try {
        [Environment]::SetEnvironmentVariable('Path', $new, 'User')
    }
    catch {
        # Un PATH verrouillé par une stratégie de groupe refuse l'écriture. Ce n'est pas
        # fatal : le binaire est installé, seul son accès automatisé manque.
        Warn "le PATH utilisateur n'a pas pu être écrit : $($_.Exception.Message)"
        Warn "ajoutez manuellement : `$env:Path += ';' + '$Dir'"
        return $false
    }
    return $true
}

function Test-BinaryStart {
    param([string] $Path)

    Step 'Vérification de l''installation'
    $output = & $Path '--version' 2>&1
    if ($LASTEXITCODE -eq 0) {
        $output | ForEach-Object { Note "$_" }
        return
    }

    # Le cas mesuré en septembre 2026 : l'installation se termine sans erreur,
    # puis `typr --version` échoue. Le dire ici vaut mieux qu'un ticket.
    Write-Host ''
    Warn 'le binaire est installé mais ne démarre pas.'
    Note "Message du binaire : $output"
    Note 'Si celui-ci évoque une DLL introuvable (VCRUNTIME140.dll, ucrtbase.dll),'
    Note "l'artefact n'a pas été lié statiquement : lire $Docs/install pour le dépannage."
    exit 1
}

# ---------------------------------------------------------------------------
# Déroulement
# ---------------------------------------------------------------------------
$script:Tag = Resolve-Version
Assert-Tag $script:Tag
$target = Resolve-Target
$installDir = Resolve-InstallDir

$artifact = "typr-$($script:Tag)-$target.zip"
$url = "$DownloadBase/$($script:Tag)/$artifact"

Step "TypR $($script:Tag)"
Note "système   : $(Get-OsName) ($env:PROCESSOR_ARCHITECTURE)"
Note "cible     : $target"
Note "dossier   : $installDir"

if ($Verify -eq '0') { Warn 'vérification SHA-256 désactivée (TYPR_INSTALL_VERIFY=0).' }

if ($DryRun) {
    Step 'Simulation — rien ne sera téléchargé ni écrit'
    Note "artefact  : $artifact"
    Note "url       : $url"
    Note "sha256    : $DownloadBase/$($script:Tag)/checksums.txt"
    exit 0
}

if (-not (Test-Path -LiteralPath $installDir)) {
    New-Item -ItemType Directory -Path $installDir -Force | Out-Null
}
if (-not (Test-Path -LiteralPath $installDir)) {
    Stop-Install "impossible de créer $installDir."
}

$script:TempDir = Join-Path ([System.IO.Path]::GetTempPath()) ("typr-install-" + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory -Path $script:TempDir -Force | Out-Null

Step 'Téléchargement'
$archivePath = Join-Path $script:TempDir $artifact
Save-File $url $archivePath

if ($Verify -ne '0') {
    Test-Checksum $archivePath $artifact
}
else {
    Step 'Vérification du SHA-256'
    Note 'ignorée.'
}

Step 'Extraction'
$extractDir = Join-Path $script:TempDir 'extract'
New-Item -ItemType Directory -Path $extractDir -Force | Out-Null
try {
    Expand-Archive -LiteralPath $archivePath -DestinationPath $extractDir -Force
}
catch {
    Stop-Install "archive illisible : $artifact`n  $($_.Exception.Message)"
}

$exePath = Join-Path $extractDir 'typr.exe'
if (-not (Test-Path -LiteralPath $exePath)) {
    Stop-Install "l'archive $artifact ne contient aucun binaire 'typr.exe'."
}

$targetExe = Join-Path $installDir 'typr.exe'
# Remove-Item puis Copy-Item plutôt que Move-Item : un binaire en cours
# d'exécution sous Windows ne peut pas être écrasé, et l'échec serait
# confondu avec un problème d'écriture. L'opération reste idempotente.
if (Test-Path -LiteralPath $targetExe) {
    Remove-Item -LiteralPath $targetExe -Force
}
Copy-Item -LiteralPath $exePath -Destination $targetExe -Force

Write-Host ''
Step "TypR $($script:Tag) installé dans $installDir"

if (Test-OnPath $installDir) {
    Test-BinaryStart $targetExe
}
else {
    if (Add-ToUserPath $installDir) {
        $env:Path = "$installDir$([System.IO.Path]::PathSeparator)$env:Path"
        Warn "$installDir a été ajouté au PATH utilisateur."
        Note 'Il ne sera pris en compte qu''au prochain démarrage d''un terminal.'
        Note 'Celui-ci est à jour : `typr --version` fonctionne dès maintenant.'
    }
    Write-Host ''
    Note "Essayez d'abord : $targetExe --version"
}

Remove-TempDir

Write-Host ''
Note "Documentation : $Docs"
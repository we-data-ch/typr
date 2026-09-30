#!/usr/bin/env pwsh
# Tests de install/install.ps1, sans réseau.
#
# Le script ne parle qu'à une release : redirection de `/releases/latest`, flux
# Atom, archives .zip et `checksums.txt`. Le serveur de fixtures (voir
# fixture-server.py) reproduit ces quatre points, ce qui permet de tester le
# chemin heureux *et* les refus — notamment le SHA falsifié, qu'on ne peut pas
# produire sur une vraie release sans la compromettre.
#
#   pwsh install/tests/run-tests.ps1
#
# Le même script est exécuté par le job `install` de ci.yml.

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'

$Here       = Split-Path -Parent $MyInvocation.MyCommand.Path
$AnalyzerSettings = Join-Path $PSScriptRoot 'PSScriptAnalyzerSettings.psd1'
$InstallPs1 = Join-Path (Split-Path -Parent $Here) 'install.ps1'
$RepoPath   = 'we-data-ch/typr'
$StableTag  = 'v9.9.9'
$BetaTag    = 'v9.9.9-beta.1'

$script:Passed = 0
$script:Failed = 0
$script:Root   = $null
$script:Server = $null

# ---------------------------------------------------------------------------

function Ok {
    param([string] $Message)
    $script:Passed++
    Write-Host "  $([char]0x2713) $Message" -ForegroundColor Green
}

function Ko {
    param([string] $Message, [string] $Detail)
    $script:Failed++
    Write-Host "  $([char]0x2717) $Message" -ForegroundColor Red
    if ($Detail) { Write-Host "      $Detail" -ForegroundColor Red }
}

function Check-Rc {
    param([int] $Expected, [int] $Actual, [string] $Message)
    if ($Expected -eq $Actual) { Ok $Message }
    else { Ko $Message "code de retour $Actual, attendu $Expected" }
}

function Check-Contains {
    param([string] $Output, [string] $Needle, [string] $Message)
    if ($Output -and $Output.Contains($Needle)) { Ok $Message }
    else { Ko $Message "« $Needle » absent de la sortie : $Output" }
}

function Check-NotContains {
    param([string] $Output, [string] $Needle, [string] $Message)
    if ($Output -and $Output.Contains($Needle)) {
        Ko $Message "« $Needle » présent alors qu'il ne devait pas l'être"
    }
    else { Ok $Message }
}

function Check-Path {
    param([string] $Path, [bool] $ShouldExist, [string] $Message)
    if ([bool](Test-Path -LiteralPath $Path) -eq $ShouldExist) { Ok $Message }
    else { Ko $Message "$Path : existence inattendue" }
}

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

# Un « binaire » de trois lignes : le script l'extrait, le copie et l'exécute
# avec `--version`. Assez pour couvrir le chemin complet sans télécharger 11 Mo
# ni dépendre d'une release existante.
function Get-Targets {
    # Une archive par cible conceivable : le script résout sa cible à partir de
    # l'architecture de la machine, que les fixtures ne peuvent pas faire varier.
    return @(
        'x86_64-pc-windows-msvc', 'aarch64-pc-windows-msvc',
        'x86_64-unknown-linux-musl', 'x86_64-apple-darwin'
    )
}

function New-StubArchive {
    param([string] $Dir, [string] $Tag, [string] $Target)

    $name = "typr-$Tag-$Target.zip"
    $stage = Join-Path $Dir 'stage'
    if (Test-Path $stage) { Remove-Item $stage -Recurse -Force }
    New-Item -ItemType Directory -Path $stage -Force | Out-Null

    # Un script bat nommé typr.exe : c'est le seul format que PowerShell peut
    # exécuter sous Linux, où la suite tourne. Le script testé s'en fiche — il
    # copie et invoque le fichier sans regarder ce qu'il contient.
    Set-Content -LiteralPath (Join-Path $stage 'typr.exe') -Encoding Ascii -Value @(
        '@echo off',
        'if not "%1"=="--version" exit /b 2',
        'echo typr-cli 0.0.0-fixture'
    )

    # On zippe à la main : Compress-Archive n'est pas disponible sous Linux, et
    # System.IO.CompressionZipFile l'est partout.
    Add-Type -AssemblyName System.IO.Compression.FileSystem
    $zipPath = Join-Path $Dir $name
    if (Test-Path $zipPath) { Remove-Item $zipPath -Force }
    [System.IO.Compression.ZipFile]::CreateFromDirectory($stage, $zipPath)

    $sha = (Get-FileHash -LiteralPath $zipPath -Algorithm SHA256).Hash.ToLowerInvariant()
    Add-Content -LiteralPath (Join-Path $Dir 'checksums.txt') -Value "$sha  $name"
    Remove-Item $stage -Recurse -Force
}

function Set-FakeChecksum {
    param([string] $Target)
    $dir = Join-Path $script:Root "$RepoPath/releases/download/$StableTag"
    $sums = Join-Path $dir 'checksums.txt'
    $out = @()
    foreach ($line in (Get-Content -LiteralPath $sums)) {
        if ($line -match "\s+typr-$StableTag-$([regex]::Escape($Target))\.zip$") {
            $out += "0000000000000000000000000000000000000000000000000000000000000000  $($line -split '\s+' | Select-Object -Last 1)"
        }
        else {
            $out += $line
        }
    }
    Set-Content -LiteralPath $sums -Value $out
}

function Reset-Checksums {
    param([string] $Tag, [string] $Target)
    $dir = Join-Path $script:Root "$RepoPath/releases/download/$Tag"
    Remove-Item (Join-Path $dir 'checksums.txt') -Force -ErrorAction SilentlyContinue
    New-StubArchive $dir $Tag $Target
}

function New-FixtureTree {
    $script:Root = Join-Path ([System.IO.Path]::GetTempPath()) ("typr-ps-tests-" + [Guid]::NewGuid().ToString('N'))
    $rel = Join-Path $script:Root "$RepoPath/releases"
    New-Item -ItemType Directory -Path $rel -Force | Out-Null
    Set-Content -LiteralPath (Join-Path $rel 'LATEST') -Value $StableTag

    foreach ($t in (Get-Targets)) {
        New-StubArchive (Join-Path $rel "download/$StableTag") $StableTag $t
        New-StubArchive (Join-Path $rel "download/$BetaTag") $BetaTag $t
    }

    Set-Content -LiteralPath (Join-Path $script:Root "$RepoPath/releases.atom") -Encoding Ascii -Value @(
        '<?xml version="1.0" encoding="UTF-8"?>',
        '<feed xmlns="http://www.w3.org/2005/Atom">',
        '  <title>Release notes from typr</title>',
        "  <id>tag:github.com,2008:https://github.com/$RepoPath/releases</id>",
        "  <entry><id>tag:github.com,2008:Repository/1/$StableTag</id></entry>",
        "  <entry><id>tag:github.com,2008:Repository/1/$BetaTag</id></entry>",
        '  <entry><id>tag:github.com,2008:Repository/1/v0.1.0</id></entry>',
        '</feed>'
    )
}

function Start-FixtureServer {
    param([string] $Python)

    $psi = [System.Diagnostics.ProcessStartInfo]::new()
    $psi.FileName = $Python
    $psi.ArgumentList.Add((Join-Path $Here 'fixture-server.py'))
    $psi.ArgumentList.Add($script:Root)
    $psi.RedirectStandardOutput = $true
    $psi.UseShellExecute = $false
    return [System.Diagnostics.Process]::Start($psi)
}

# ---------------------------------------------------------------------------
# Lancement de l'installeur
#
# `pwsh -File` dans un processus séparé, et non `& $InstallPs1` : le script
# appelle `exit`, qui dans un appel in-process ne ferait que terminer le script
# appelé — le code de retour se perdrait, et « refus d'un SHA falsifié » et
# « refus d'un tag illisible » rendraient tous deux 0.
function Invoke-Install {
    param([string] $InstallDir, [string] $ExtraEnv = '', [string[]] $Options = @('-DryRun'))

    $envArgs = @(
        "`$env:TYPR_INSTALL_DIR='$InstallDir'",
        "`$env:TYPR_INSTALL_TARGET='x86_64-pc-windows-msvc'",
        "`$env:TYPR_INSTALL_ORIGIN='$($script:Origin)'"
    )
    if ($ExtraEnv) { $envArgs += $ExtraEnv }

    # `exit $LASTEXITCODE` explicite : `& script.ps1` ne propage pas le code de
    # sortie du script appelé vers le processus — sans cette ligne, un refus
    # 2 (usage) serait indiscernable d'un refus 1 (environnement).
    $command = ($envArgs -join '; ') + "; & '$InstallPs1' " + ($Options -join ' ') + " 2>&1; exit `$LASTEXITCODE"

    $out = & pwsh -NoProfile -Command $command 2>&1 | Out-String
    return @($out, $LASTEXITCODE)
}

# Le chemin réel de l'utilisateur est `irm … | iex`, où le script arrive comme
# texte et non comme fichier. Ce n'est pas la même exécution que -File : on
# vérifie que le texte survit à ce mode — notamment que son bloc `param` est
# accepté par `iex` et que l'échec s'y propère toujours.
#
# `iex` ne prend pas d'argument : les options ne sont pas transmissibles par
# cette voie, ce qui est aussi ce que verra l'utilisateur. Les variables
# d'environnement, elles, le sont.
function Invoke-InstallViaIex {
    param([string] $InstallDir, [string] $Origin)

    if (-not $Origin) { $Origin = $script:Origin }

    $command = @(
        "`$env:TYPR_INSTALL_DIR='$InstallDir'",
        "`$env:TYPR_INSTALL_TARGET='x86_64-pc-windows-msvc'",
        "`$env:TYPR_INSTALL_ORIGIN='$Origin'",
        "`$src = Get-Content -Raw -LiteralPath '$InstallPs1'",
        'iex $src 2>&1'
    ) -join '; '

    $out = & pwsh -NoProfile -Command $command 2>&1 | Out-String
    return @($out, $LASTEXITCODE)
}

function Work {
    param([string] $Name)
    $dir = Join-Path $script:Root "work-$Name"
    New-Item -ItemType Directory -Path $dir -Force | Out-Null
    return $dir
}

# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

function Test-Syntax {
    Write-Host 'Analyse statique'
    $errors = $null
    [System.Management.Automation.Language.Parser]::ParseFile(
        $InstallPs1, [ref]$null, [ref]$errors) | Out-Null
    if ($errors -and $errors.Count -gt 0) {
        Ko 'install.ps1 — analyse syntaxique' ($errors | ForEach-Object { $_.Message } | Out-String)
    }
    else {
        Ok 'install.ps1 — analyse syntaxique'
    }

    # Le BOM n'est pas une coquetterie : PowerShell 5.1 lit un .ps1 sans BOM
    # comme de l'ANSI, avec la page de code de la machine. Sur un Windows
    # français ou allemand — les deux cibles principales du projet — chaque
    # accent de ce fichier deviendrait un caractère illisible. Le BOM est donc
    # testé explicitement, sinon un reformatage l'effacerait sans bruit.
    $head = [System.IO.File]::ReadAllBytes($InstallPs1)[0..2]
    if ($head[0] -eq 0xEF -and $head[1] -eq 0xBB -and $head[2] -eq 0xBF) {
        Ok 'install.ps1 — BOM UTF-8 présent'
    }
    else {
        Ko 'install.ps1 — BOM UTF-8 présent' (
            'sans BOM, PowerShell 5.1 lit le fichier en ANSI et les accents cassent. ' +
            "Octets : $($head -join ' ')")
    }

    if (-not (Get-Module -ListAvailable PSScriptAnalyzer)) {
        Ko 'PSScriptAnalyzer est disponible' 'module absent de l image de test'
        return
    }
# `-Settings` avec un dictionnaire est accepté par l'API mais
    # silencieusement ignoré dans PSScriptAnalyzer 1.25 : le nombre de
    # constatations ne change pas, que la liste soit vide ou complète.
    # `-ExcludeRule` est le mécanisme vérifié — un fichier jetable contenant
    # `Write-Host` passe de 1 constatation à 0 avec, et reste à 1 sans.
    #
    # La liste vient du fichier de configuration, pour que les raisons soient
    # écrites à un seul endroit.
    $excluded = (Import-PowerShellDataFile -Path $AnalyzerSettings).Excluded
    $findings = Invoke-ScriptAnalyzer -Path $InstallPs1 -ExcludeRule $excluded
    if ($findings) {
        Ko 'install.ps1 — PSScriptAnalyzer' (
            ($findings | ForEach-Object {
                "ligne $($_.Line) [$($_.RuleName)] $($_.Message)"
            }) -join "`n")
    }
    else {
        Ok 'install.ps1 — PSScriptAnalyzer'
    }
}

function Test-Iex {
    Write-Host 'Mode irm | iex'
    # `irm … | iex` est la forme documentée pour Windows : le texte arrive dans
    # la portée de l'appelant, pas comme fichier. Deux différences réelles avec
    # `pwsh -File` sont vérifiées ici :
    #
    #   — `iex` ne transmet aucun argument. Les options (-DryRun, -Version…) sont
    #     inaccessibles par cette voie ; seules les variables d'environnement
    #     passent. Le test ne peut donc pas être « juste une simulation ».
    #   — un `exit` y termine le processus appelant. C'est voulu pour un
    #     échec, mais cela signifie que la session interactive est perdue si le
    #     script échoue. D'où un test d'échec : il doit bien rendre 1, et non
    #     laisser croire à un succès.
    $dir = Work 'iex'
    $r = Invoke-InstallViaIex $dir
    Check-Rc 0 $r[1] 'iex installe et sort en 0'
    Check-Contains $r[0] 'installé dans' 'iex installe réellement'
    Check-Path (Join-Path $dir 'typr.exe') $true 'iex pose le binaire'

    $badDir = Work 'iex-bad'
    $r = Invoke-InstallViaIex $badDir "$($script:Origin)/inexistant"
    Check-Rc 1 $r[1] 'iex propage un échec en 1'
    Check-Path (Join-Path $badDir 'typr.exe') $false 'iex en échec n installe rien'
}

function Test-DryRun {
    Write-Host ' -DryRun'
    $dir = Work 'dryrun'
    $r = Invoke-Install $dir
    Check-Rc 0 $r[1] '-DryRun sort en 0'
    Check-Contains $r[0] 'Simulation' '-DryRun annonce la simulation'
    Check-Contains $r[0] 'windows-msvc' '-DryRun résout une cible Windows'
    Check-Contains $r[0] $StableTag '-DryRun résout la dernière version stable'
    Check-NotContains $r[0] 'Extraction' '-DryRun n''extrait rien'
    $leftover = Get-ChildItem -LiteralPath $dir -ErrorAction SilentlyContinue
    if (-not $leftover) { Ok '-DryRun n''écrit rien' } else { Ko '-DryRun n''écrit rien' ($leftover.Name) }
}

function Test-HappyPath {
    Write-Host 'Installation réelle'
    $dir = Work 'default'
    $r = Invoke-Install $dir @() @()
    Check-Rc 0 $r[1] 'installation par défaut'
    Check-Contains $r[0] 'SHA-256 correct' 'le SHA-256 est vérifié'
    Check-Contains $r[0] $StableTag 'la version installée est annoncée'
    if (Test-Path (Join-Path $dir 'typr.exe')) { Ok 'le binaire est installé' }
    else { Ko 'le binaire est installé' $r[0] }

    # Idempotence : relancer ne doit ni échouer ni laisser de doublon.
    $r = Invoke-Install $dir @() @()
    Check-Rc 0 $r[1] 'seconde installation (idempotence)'
    if (Test-Path (Join-Path $dir 'typr.exe')) { Ok 'le binaire survit à la réinstallation' }
    else { Ko 'le binaire survit à la réinstallation' }
}

function Test-PinnedVersion {
    Write-Host 'Version épinglée'
    $r = Invoke-Install (Work 'pinned') @() @('-DryRun', '-Version', $StableTag)
    Check-Rc 0 $r[1] '-Version sort en 0'
    Check-Contains $r[0] $StableTag '-Version est respecté'

    # Sans le « v » initial, comme dans le README.
    $r = Invoke-Install (Work 'pinned2') @() @('-DryRun', '-Version', $StableTag.TrimStart('v'))
    Check-Contains $r[0] $StableTag '-Version sans le v initial est normalisé'

    $r = Invoke-Install (Work 'pinned3') @() @('-DryRun', '-Version', 'pas-un-tag')
    Check-Rc 2 $r[1] 'un tag illisible sort en 2'
    Check-Contains $r[0] 'tag illisible' 'le tag illisible est nommé'
}

function Test-BetaChannel {
    Write-Host 'Canal beta'
    $r = Invoke-Install (Work 'beta') @() @('-DryRun', '-Channel', 'beta')
    Check-Rc 0 $r[1] '-Channel beta résout une prerelease'
    Check-Contains $r[0] $BetaTag '-Channel beta choisit la prerelease, pas la stable'

    $r = Invoke-Install (Work 'beta2') @() @('-Channel', 'beta')
    Check-Rc 0 $r[1] 'installation depuis le canal beta'
    if (Test-Path (Join-Path $script:Root 'work-beta2/typr.exe')) { Ok 'le binaire beta est installé' }
    else { Ko 'le binaire beta est installé' $r[0] }
}

function Test-BadChecksum {
    Write-Host 'SHA-256 falsifié'
    Set-FakeChecksum 'x86_64-pc-windows-msvc'
    $dir = Work 'badsha'
    $r = Invoke-Install $dir @() @()
    Check-Rc 1 $r[1] 'le script refuse un SHA falsifié'
    Check-Contains $r[0] 'SHA-256 falsifié' 'le message nomme la cause'
    Check-Contains $r[0] "rien n'est installé" 'le message dit que rien n''est installé'
    if (-not (Test-Path (Join-Path $dir 'typr.exe'))) { Ok 'aucun binaire n''est laissé sur le disque' }
    else { Ko 'aucun binaire n''est laissé sur le disque' }

    Reset-Checksums $StableTag 'x86_64-pc-windows-msvc'
}

function Test-BadChecksumBypass {
    Write-Host "TYPR_INSTALL_VERIFY=0"
    Set-FakeChecksum 'x86_64-pc-windows-msvc'
    $dir = Work 'nosha'
    $r = Invoke-Install $dir "`$env:TYPR_INSTALL_VERIFY='0'" @()
    Check-Rc 0 $r[1] "TYPR_INSTALL_VERIFY=0 installe sans vérifier"
    Check-Contains $r[0] 'désactivée' "TYPR_INSTALL_VERIFY=0 est annoncé"
    Reset-Checksums $StableTag 'x86_64-pc-windows-msvc'
}

function Test-MissingChecksums {
    Write-Host 'checksums.txt absent ou malformé'
    $dir = Join-Path $script:Root "$RepoPath/releases/download/$StableTag"

    # Absent
    $saved = Get-Content -LiteralPath (Join-Path $dir 'checksums.txt')
    Remove-Item (Join-Path $dir 'checksums.txt') -Force
    $inst = Work 'nosum'
    $r = Invoke-Install $inst @() @()
    Check-Rc 1 $r[1] 'checksums.txt absent → échec'
    Check-Contains $r[0] 'checksums.txt introuvable' "l'absence est nommée explicitement"
    if (-not (Test-Path (Join-Path $inst 'typr.exe'))) { Ok "rien n'est installé sans checksums.txt" }
    else { Ko "rien n'est installé sans checksums.txt" }
    Set-Content -LiteralPath (Join-Path $dir 'checksums.txt') -Value $saved

    # Présent mais sans la ligne de cet artefact : c'est le cas de
    # `sha256sum -c`, qui tente de vérifier les sept autres archives.
    $out = @($saved | Where-Object { $_ -notmatch 'windows-msvc\.zip$' })
    $out += "1111111111111111111111111111111111111111111111111111111111111111  typr-$StableTag-x86_64-pc-windows-msvc.zip.exe"
    Set-Content -LiteralPath (Join-Path $dir 'checksums.txt') -Value $out
    $inst = Work 'nosum2'
    $r = Invoke-Install $inst @() @()
    Check-Rc 1 $r[1] 'checksums.txt sans la ligne attendue → échec'
    Check-Contains $r[0] 'ne contient aucune ligne pour' 'la ligne manquante est nommée'
    if (-not (Test-Path (Join-Path $inst 'typr.exe'))) { Ok "rien n'est installé sans ligne correspondante" }
    else { Ko "rien n'est installé sans ligne correspondante" }

    # Malformé : la ligne existe, le SHA est illisible.
    Set-Content -LiteralPath (Join-Path $dir 'checksums.txt') `
        -Value "pas-un-sha  typr-$StableTag-x86_64-pc-windows-msvc.zip"
    $inst = Work 'badline'
    $r = Invoke-Install $inst @() @()
    Check-Rc 1 $r[1] 'checksums.txt malformé → échec'
    Check-Contains $r[0] 'malform' 'la malformation est nommée'
    if (-not (Test-Path (Join-Path $inst 'typr.exe'))) { Ok "rien n'est installé sur une ligne malformée" }
    else { Ko "rien n'est installé sur une ligne malformée" }

    Reset-Checksums $StableTag 'x86_64-pc-windows-msvc'
}

function Test-MissingRelease {
    Write-Host 'Release absente'
    $r = Invoke-Install (Work 'ghost') @() @('-Version', 'v0.0.1-does-not-exist')
    Check-Rc 1 $r[1] 'un tag inexistant échoue'
    Check-Contains $r[0] 'téléchargement échoué' "l'échec de téléchargement est explicite"
}

function Test-ContainedWrites {
    Write-Host 'Écritures contenues'
    $dir = Work 'contained'
    $r = Invoke-Install $dir @() @()
    Check-Rc 0 $r[1] 'installation avec un dossier explicite'
    $stray = Get-ChildItem -LiteralPath $dir | Where-Object { $_.Name -ne 'typr.exe' }
    if (-not $stray) { Ok "le dossier d'installation ne contient que le binaire" }
    else { Ko "le dossier d'installation ne contient que le binaire" ($stray.Name -join ', ') }

    $leftovers = Get-ChildItem -LiteralPath ([System.IO.Path]::GetTempPath()) -Filter 'typr-install-*' -ErrorAction SilentlyContinue
    if (-not $leftovers) { Ok 'aucun dossier temporaire laissé derrière' }
    else { Ko 'aucun dossier temporaire laissé derrière' ($leftovers.Name -join ', ') }
}

# ---------------------------------------------------------------------------

function Main {
    Write-Host 'install.ps1 — tests'
    Write-Host ''

    $python = (Get-Command python3 -ErrorAction SilentlyContinue)
    if (-not $python) { Write-Error 'python3 est requis pour le serveur de fixtures.'; exit 1 }

Test-Syntax

    # Le port est choisi par le serveur ; on le récupère en lisant sa sortie.
    New-FixtureTree
    $script:Server = Start-FixtureServer $python.Source

    $portLine = $script:Server.StandardOutput.ReadLine()
    if ($portLine -notmatch 'PORT (\d+)') { Write-Error "le serveur n'a pas démarré : $portLine"; exit 1 }
    $script:Origin = "http://127.0.0.1:$($Matches[1])"

    try {
        Test-Iex
        Test-DryRun
        Test-HappyPath
        Test-PinnedVersion
        Test-BetaChannel
        Test-BadChecksum
        Test-BadChecksumBypass
        Test-MissingChecksums
        Test-MissingRelease
        Test-ContainedWrites
    }
    finally {
        if ($script:Server -and -not $script:Server.HasExited) { $script:Server.Kill() }
        Remove-Item -LiteralPath $script:Root -Recurse -Force -ErrorAction SilentlyContinue
    }

    Write-Host ''
    Write-Host "$($script:Passed) réussis, $($script:Failed) échoués"
    if ($script:Failed -gt 0) { exit 1 }
}

Main
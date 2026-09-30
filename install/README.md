# Installation

TypR s'installe en une commande. Le script vérifie l'empreinte SHA-256 de ce
qu'il télécharge avant de l'exécuter, et n'écrit rien en dehors de votre dossier
utilisateur.

## Linux, macOS

```sh
curl -fsSL https://we-data-ch.github.io/typr.github.io/install/install.sh | sh
```

Le script est un `sh` POSIX : ni bash, ni `jq`, ni `python`. Il s'exécute
n'importe où, y compris sur une Alpine sans glibc.

Sur Linux, il installe un binaire **musl**, lié statiquement, qui démarre sur
Rocky 9, Debian 12, Ubuntu 22.04 et Alpine alike. Pour un binaire **GNU**
(lié dynamiquement à glibc), ajoutez `--gnu`.

## Windows

PowerShell 5.1 est présent sur toute machine Windows 10 ou plus récente :

```powershell
irm https://we-data-ch.github.io/typr.github.io/install/install.ps1 | iex
```

Le script pose le binaire dans `%LOCALAPPDATA%\Programs\typr`, ajoute ce
dossier au PATH utilisateur, et ne demande aucun privilège administrateur. Il
n'est pas signé : PowerShell affichera *« Unrecognized script from
typr-install »*, avec son nom et son origine. Vérifiez que la ligne correspond
à ce dépôt avant de taper `O`.

Le PATH ne prend effet qu'au prochain terminal ; le script met aussi à jour la
session courante, donc `typr --version` fonctionne immédiatement.

## Options

Les deux scripts partagent la même ligne de commande.

| Option | Effet |
|---|---|
| `--version vX.Y.Z` | installe ce tag précis au lieu du dernier |
| `--channel beta` | installe la dernière prerelease |
| `--dry-run` | affiche ce qui serait fait, ne télécharge rien |
| `--help` | l'aide du script |
| `--gnu` | Unix seulement : cible glibc au lieu de musl |

Sur PowerShell, les options s'écrivent `-Version`, `-Channel`, `-DryRun`, `-Gnu`.
Sur la ligne `irm … | iex`, elles ne sont pas transmissibles : `iex` n'accepte
aucun argument. Utilisez alors les variables d'environnement, ou téléchargez le
script dans un fichier et lancez-le.

| Variable | Effet |
|---|---|
| `TYPR_INSTALL_DIR` | dossier d'installation |
| `TYPR_INSTALL_VERIFY=0` | saute la vérification SHA-256 (dépannage réseau) |

## Ce qui est écrit

| Système | Emplacement |
|---|---|
| Linux, macOS | `$HOME/.local/bin` |
| Windows | `%LOCALAPPDATA%\Programs\typr`, ajouté au PATH utilisateur par registre |

Sur Unix, le script **n'écrit dans aucun fichier de configuration** : si
`$HOME/.local/bin` n'est pas déjà dans votre PATH, il affiche la ligne exacte à
ajouter à `~/.profile` ou `~/.zshrc`. Le laisser modifier votre profil à votre
place serait une surprise, et un `.profile` réécrit à tort peut empêcher votre
shell de démarrer.

Sur Windows en revanche, le PATH est mis à jour dans le registre — c'est le
seul endroit où Windows le lit. Le changement ne s'applique qu'au prochain
terminal, et le script le dit.

Dans les deux cas, rien d'autre n'est écrit : pas de fichier système, aucun
privilège administrateur, et le dossier temporaire de téléchargement est
supprimé en cas d'échec comme de succès.

## Vérifier l'empreinte soi-même

Si vous ne faites confiance ni au script ni à TLS :

```sh
curl -fsSLO https://github.com/we-data-ch/typr/releases/latest/download/checksums.txt
grep "x86_64-unknown-linux-musl.tar.gz" checksums.txt
```

La ligne attendue commence par une empreinte SHA-256 de 64 caractères
hexadécimaux. C'est exactement la ligne que le script cherche, et il refuse de
continuer si elle est absente ou malformée — y compris lorsque le fichier existe
et contient bien le nom de l'archive. Un `checksums.txt` tronqué ou corrompu est
donc un échec, pas une installation sans vérification.

## Codes de sortie

| Code | Signification |
|---|---|
| `0` | installation réussie |
| `1` | environnement ou vérification : réseau coupé, release absente, SHA invalide |
| `2` | ligne de commande : option inconnue, argument manquant, tag illisible |

La distinction 1 / 2 est volontaire : un appelant peut réagir différemment à une
mauvaise commande et à une release cassée.

## Dépannage

**`GLIBC_2.39 not found`** — le binaire GNU exige une glibc plus récente que
celle de votre système. C'est ce qui se passait par défaut avant que musl ne
devienne le choix par défaut. Rejouez sans `--gnu`.

**`typr: command not found`** — le dossier `$HOME/.local/bin` n'est pas dans le
PATH de ce terminal. Ouvrez-en un nouveau, ou lancez `$HOME/.local/bin/typr`.

**Le téléchargement échoue alors que le réseau fonctionne**

`TYPR_INSTALL_VERIFY=0` désactive la vérification SHA-256. À n'utiliser que pour
confirmer qu'un problème de réseau est bien la cause : cela retire la seule
protection contre une archive altérée en transit.

**Alerte de sécurité PowerShell** — le script n'est pas signé. `irm … | iex`
exécute du code téléchargé ; c'est le compromis habituel du one-liner, contre la
signature Authenticode qui exigerait un certificat de signature de code à jour
pour chaque binaire publié.
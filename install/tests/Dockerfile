# Image de test pour install.ps1.
#
# Le dépôt ne fournit pas de runner Windows, et `pwsh` sous Linux ne suffit
# pas : Expand-Archive, le registre utilisateur et les chemins %LOCALAPPDATA%
# sont des chemins de code Windows. On teste donc ce qui est testable ici — la
# logique, les diagnostics, les codes de retour — et la CI conserve une matrice
# Windows réelle pour la validation d bout en bout.
#
# Le tag `7.4-ubuntu-22.04` est épinglé, pas `latest` : une mise à jour de
# base ne doit pas pouvoir faire échouer la CI sur un changement de runtime.
FROM mcr.microsoft.com/powershell:7.4-ubuntu-22.04

RUN apt-get update \
 && apt-get install --no-install-recommends -y python3 \
 && rm -rf /var/lib/apt/lists/*

# PSScriptAnalyzer est le équivalent PowerShell de shellcheck : il signale ce
# que l'analyse syntaxique laisse passer (variables inutilisées, pièges de
# comparaison, cmdlets obsolètes). Installé ici pour que la suite puisse le
# lancer sans téléchargement à chaque exécution.
RUN pwsh -NoProfile -Command \
      'Install-Module PSScriptAnalyzer -Scope CurrentUser -Force -AllowClobber -Repository PSGallery'

# shellcheck : le même outil est utilisé par la suite Unix, pour que les deux
# halves de la suite d'installation soient vérifiées pareil.
RUN apt-get update \
 && apt-get install --no-install-recommends -y shellcheck \
 && rm -rf /var/lib/apt/lists/*

WORKDIR /w
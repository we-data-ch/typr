# Image TypR — consomme le binaire produit par .github/workflows/release.yml.
#
# L'ancienne version installait Rust puis faisait `cargo install typr`, ce qui
# la rendait dépendante de la propagation de crates.io (et l'a laissée bloquée
# en 0.5.5 pendant que les releases étaient en 0.5.7). Elle part maintenant du
# même binaire que celui distribué en release : un seul artefact, une seule
# version.
#
# Le binaire est attendu dans ./bin/typr (contexte de build = ce dossier).
# En CI il est téléchargé depuis la release ; en local, `nu deploy.nu` le
# compile.
#
# Base : rocker/r-ver, et non r-base.
#
# `r-base:latest` est construite sur Debian unstable, dont l'index apt dérive en
# permanence par rapport aux bibliothèques figées dans l'image. Les paquets -dev
# y exigent une version exacte de leur bibliothèque d'exécution, que l'image ne
# porte plus : la résolution devient insoluble et le build casse sans qu'aucun
# fichier du dépôt n'ait changé. C'est ce qui a fait échouer la release v0.5.8
# (« libcurl4-openssl-dev Depends libcurl4t64 (= 8.21.0-2) — not installable »).
#
# rocker/r-ver repose sur Ubuntu LTS, dont les dépôts sont stables, et épingle
# un instantané CRAN : le même Dockerfile produit la même image dans six mois.
# La version de R est explicite ; la relever est un changement délibéré.
FROM rocker/r-ver:4.5.3

LABEL org.opencontainers.image.title="TypR"
LABEL org.opencontainers.image.description="A typed superset of R — transpiler and type checker"
LABEL org.opencontainers.image.source="https://github.com/we-data-ch/typr"
LABEL org.opencontainers.image.licenses="Apache-2.0"

ENV DEBIAN_FRONTEND=noninteractive

# Dépendances système des packages R que `typr init` installe (devtools, testthat).
#
# libuv1t64 est une dépendance d'exécution, pas de compilation : rocker/r-ver
# récupère des paquets R déjà compilés, qui se lient à des bibliothèques que
# l'image ne porte pas forcément. Sans elle, l'image se construisait très bien
# et `library(devtools)` échouait au premier lancement sur
# « libuv.so.1: cannot open shared object file ».
RUN apt-get update && apt-get install -y --no-install-recommends \
        ca-certificates \
        curl \
        libcurl4-openssl-dev \
        libssl-dev \
        libxml2-dev \
        libgit2-dev \
        libfontconfig1-dev \
        libharfbuzz-dev \
        libfribidi-dev \
        libuv1t64 \
    && rm -rf /var/lib/apt/lists/*

COPY bin/typr /usr/local/bin/typr
RUN chmod +x /usr/local/bin/typr

# Les packages R dont TypR a besoin, préinstallés : `typr init` devient inutile
# dans l'image, et un `typr build` fonctionne dès le premier démarrage.
# rocker/r-ver pointe deja sur un instantane CRAN date : on ne surcharge pas
# `repos`, sinon on perd la reproductibilite qu'apporte l'image.
RUN R -e "install.packages(c('devtools', 'testthat', 'roxygen2'))"

# Charger les packages, et pas seulement les installer : c'est le chargement qui
# révèle une bibliothèque système absente. Le build doit échouer ici plutôt que
# chez l'utilisateur.
RUN R -e "library(devtools); library(testthat); library(roxygen2)" \
    && R --version \
    && typr --version

WORKDIR /workspace

CMD ["/bin/bash"]

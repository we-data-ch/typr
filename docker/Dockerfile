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
# Épingler R : remplacer `r-base:latest` par `r-base:4.5.1` par exemple.
FROM r-base:latest

LABEL org.opencontainers.image.title="TypR"
LABEL org.opencontainers.image.description="A typed superset of R — transpiler and type checker"
LABEL org.opencontainers.image.source="https://github.com/we-data-ch/typr"
LABEL org.opencontainers.image.licenses="Apache-2.0"

ENV DEBIAN_FRONTEND=noninteractive

# Dépendances système des packages R que `typr init` installe (devtools, testthat)
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
    && rm -rf /var/lib/apt/lists/*

COPY bin/typr /usr/local/bin/typr
RUN chmod +x /usr/local/bin/typr

# Les packages R dont TypR a besoin, préinstallés : `typr init` devient inutile
# dans l'image, et un `typr build` fonctionne dès le premier démarrage.
RUN R -e "install.packages(c('devtools', 'testthat', 'roxygen2'), repos='https://cloud.r-project.org')"

RUN R --version && typr --version

WORKDIR /workspace

CMD ["/bin/bash"]

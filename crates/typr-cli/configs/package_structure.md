structure du package:
{{PACKAGE_NAME}}
├── R/                 # Code R généré
├── TypR/             # Code source TypR
│   └── main.ty
├── man/              # Documentation
├── tests/            # Tests
│   ├── testthat.R
│   └── testthat/
│       └── test-basic.R
├── data/             # Données du package
├── inst/             # Fichiers installés
├── src/              # Code source (C++, Fortran)
├── vignettes/        # Vignettes/tutoriels
├── DESCRIPTION       # Métadonnées du package
├── NAMESPACE         # Exports et imports
├── uvr.toml          # Dépendances R (uvr), uvr.lock les fige
├── README.md         # Documentation
├── .Rbuildignore     # Fichiers ignorés lors du build
├── .uvr/             # Bibliothèque R du projet (ignorée par git)
├── .gitignore        # Fichiers ignorés par git
└── {{PACKAGE_NAME}}.Rproj          # Projet RStudio", name, name);

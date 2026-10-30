# {{PACKAGE_NAME}}

A package generated from TypR code.

## Installation

You can install the development version from GitHub:

```r
# install.packages("devtools")
devtools::install_github("your-username/{{PACKAGE_NAME}}")
```

## Usage

```r
library({{PACKAGE_NAME}})

# Your functions will be available here
```

## Development

This package is generated from TypR source code located in the `TypR/` directory.

R dependencies are managed with [uvr](https://github.com/nbafrank/uvr):

```bash
uvr sync        # install the locked packages (after a clone)
typr use dplyr  # declare a package in DESCRIPTION and uvr.toml
uvr run script.R
```

To build the package:

```bash
# Check and build
typr check
typr build

# Run tests
typr test

# Build and run
typr run
```

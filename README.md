# cauva

**R Package for the Cauchy-Kovalevskaya Theorem**

The `cauva` package provides tools to verify the conditions of the Cauchy-Kovalevskaya theorem and to apply analytic or numerical solutions to certain partial differential equations.

## Installation

```r
# Install from GitHub
devtools::install_github("Umar-Elhaj/cauva")
```

## Usage
```r
library(cauva)
f <- function(ck_parameters){..}
result <- ck_conditions(function, parameters)  # or the exact name of your main function
print(result)
```

## Project Structure

cauva/
├── DESCRIPTION      # Package metadata (title, version, author, dependencies, license)
├── NAMESPACE        # Automatically managed by roxygen2: exports user functions and imports rnorm/sd from stats
├── LICENSE          # Required file for the MIT license (year + copyright holder)
├── LICENSE.md       # Full readable text of the MIT license (nicely displayed on GitHub)
├── cauva.Rproj      # RStudio project file
├── R/               # Contains all source code (10 functions)
│   ├── cauva-package.R  # Package-level documentation (enables ?cauva)
│   ├── imports.R        # External imports (@importFrom stats rnorm sd)
│   │
│   # Exported functions (available to users)
│   ├── ck_analycity.R
│   ├── ck_conditions.R      # Global/main function
│   ├── ck_initial.R
│   └── ck_surface.R
│   │
│   # Internal functions (not exported, for internal use only)
│   ├── verify_singularity.R
│   ├── verify_derivability.R
│   ├── verify_continuity.R
│   └── verify_taylor.R      # Mainly used by ck_analycity.R
│
└── man/             # Automatically generated documentation (.Rd files for exported functions)

## Website

Visit the package website: [https://Umar-Elhaj.github.io/cauva/](https://Umar-Elhaj.github.io/cauva/)

Features interactive examples and detailed tutorials.

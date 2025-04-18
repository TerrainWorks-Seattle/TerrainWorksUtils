# TerrainWorksUtils

<!-- badges: start -->
[![R-CMD-check](https://github.com/TerrainWorks-Seattle/TerrainWorksUtils/workflows/R-CMD-check/badge.svg)](https://github.com/TerrainWorks-Seattle/TerrainWorksUtils/actions)
[![Codecov test coverage](https://codecov.io/gh/TerrainWorks-Seattle/TerrainWorksUtils/branch/main/graph/badge.svg)](https://app.codecov.io/gh/TerrainWorks-Seattle/TerrainWorksUtils?branch=main)
<!-- badges: end -->

Utilities for building, assessing, and applying predictive models to spatial data. 

# Installation

Install from github: 

```
if (!require(remotes, quietly = TRUE)) {
  install.packages("remotes")
}
remotes::install_github("TerrainWorks-Seattle/TerrainWorksUtils")
```

# Contributing

## R package structure

This is an R package, and follows package guidelines as described in [R Packages](https://r-pkgs.org/). 

In brief, the basic package structure includes: 

### `DESCRIPTION`
The DESCRIPTION file includes important information about the package, including
the name and version of the package and package dependencies.


### `R/`
This directory includes all R functions which will be exported by the package. 
Each function is preceded by [roxygen](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) 
comments. Roxygen uses these comments to build the NAMESPACE and man files, 
explained below. Any functions that you want exposed to users should have 
the `@export` keyword in the roxygen comments. 

### `vignettes/` 
The vignettes directory includes package vignettes, Rmarkdown reports which 
include examples and use cases for using functions in the package. You can 
view the vignettes by installing the package with `build_vignettes = TRUE`, 
and then calling, for example, `vignette("create_training_data")`.

### `tests/`
Unit tests live here, and are based on the [testthat](https://testthat.r-lib.org/) 
framework. The [testing chapter](https://r-pkgs.org/tests.html) in the R Packages
book explains how testing fits into the package development workflow. 

### `NAMESPACE`
This is automatically generated by [roxygen](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html). Do not edit this file by hand! 
the NAMESPACE file includes information about what functions (defined in the 
R/ directory) should be exported. If a function is exported, that means that 
when a user loads the package, that function will then be loaded into the 
environment. 

### `man/`
These are documentation files, automatically generated by [roxygen](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html). Do not edit these files by hand! 

### `.Rbuildignore`
This file lists any files or directories that should NOT be included when 
building the package. 

## Other files

### `.github/` 
The `.github` directory includes files for github automation such as 
workflows for github actions. 

### `codecov.yml`
This includes instructions for running codecov and uploading results to 
https://app.codecov.io/gh/TerrainWorks-Seattle/TerrainWorksUtils

### `dev`
This is a directory for development code: experimental or in-progress code 
which is not ready to be added to the package yet. 

## Git practices

The `main` branch hosts the stable version of the package. Code only gets 
committed here when it is production-ready. 

The `develop` branch hosts the most current working version of the package, with 
the newest or experimental features that are not yet ready to get committed to 
`main`. 

Any other new features or updates should be made to a new branch off of `develop`. 
When they are working and tested, they can be merged into `develop`. 



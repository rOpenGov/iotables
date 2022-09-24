<!-- README.md is generated from README.Rmd. Please edit that file -->

# iotables <img src="man/figures/logo.png" align="right" />

<!-- <img src="/man/figures/logo.png" align="right" height="205 width="205"/>-->

![iotables](man/figures/logo20.png)
[![rOG-badge](https://ropengov.github.io/rogtemplate/reference/figures/ropengov-badge.svg)](http://ropengov.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-0.9.1-orange.svg?style=flat-square)](https://github.com/rOpenGov/iotables/commits/master)
[![R-CMD-check](https://github.com/rOpenGov/iotables/workflows/R-CMD-check/badge.svg)](https://github.com/rOpenGov/iotables/actions)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/iotables)](https://cran.r-project.org/package=iotables)
[![CRAN_time_from_release](https://www.r-pkg.org/badges/ago/iotables)](https://cran.r-project.org/package=iotables)
[![metacran
downloads](https://cranlogs.r-pkg.org/badges/iotables)](https://cran.r-project.org/package=iotables)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7109769.svg)](https://doi.org/10.5281/zenodo.7109769)
[![license](https://img.shields.io/badge/license-MIT%20+%20file%20LICENSE-lightgrey.svg)](https://choosealicense.com/)
[![codecov](https://codecov.io/gh/rOpenGov/iotables/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rOpenGov/iotables)
[![dataobservatory](https://img.shields.io/badge/ecosystem-dataobservatory.eu-3EA135.svg)](https://dataobservatory.eu/)
[![Follow
rOpenGov](https://img.shields.io/twitter/follow/ropengov.svg?style=social)](https://twitter.com/intent/follow?screen_name=ropengov)
[![Follow
author](https://img.shields.io/twitter/follow/antaldaniel.svg?style=social)](https://twitter.com/intent/follow?screen_name=antaldaniel)

The symmetric input-output tables (SIOTs) are complex statistical
products that present inter-related statistics in a predefined
structure. They are often found in spreadsheets that follow this
structure, or in the case Eurostat in a data repository. In both cases
they in reproducible research must be downloaded and restructured to
programmatically accessible form. Often these highly structured
statistics need to be analyzed together with other data, for example,
when employment effects and multipliers are calculated. In this case
processing the employment data to SIOT conforming format is a
significant preprocessing challenge.

The iotables are exactly designed for these tasks. Currently the package
downloads and processes standardized European SIOTs conforming to the
latest statistical regulations, i.e. SIOTs starting from the year 2010.

The aim of this introduction is not to introduce input-output economics,
or SIOTs in detail. The [Eurostat Manual of Supply, Use and Input-Output
Tables](https://ec.europa.eu/eurostat/en/web/products-manuals-and-guidelines/-/KS-RA-07-013)
and the [Eurostat thematic
page](https://ec.europa.eu/eurostat/web/esa-supply-use-input-tables/overview)
(for further reference: `Eurostat Manual`) in the documentation should
be consulted for further information about the data and the metadata.

In order to test the analytical functions of the package and to have a
manageable sized example data set, we use the real-life data from the
Eurostat manual. The `germany_1995` dataset is a simplified 6x6 sized
SIOT taken from the `Eurostat Manual` (`p481`). The package function
examples can be checked against [published results from Jörg
Beutel](https://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF/b0b3d71e-3930-4442-94be-70b36cea9b39?version=1.0).
These calculations can be followed in the [Introduction to
iotables](https://iotables.dataobservatory.eu/articles/intro.html)
vignette.

The calculation of induced effects (Type-II multipliers) are following
the [Input-Output Multipliers Specification Sheet and Supporting
Material, Spicosa Project
Report](http://www.coastal-saf.eu/output-step/pdf/Specification%20sheet%20I_O_final.pdf).
The analytical functions are tested against this example, too.

## Installation

You can install iotables 0.9 from CRAN or the latest 0.9.1 development
version with github:

``` r
# From CRAN:
install.packages("iotables")

# From Github (development version)
devtools::install_github("rOpenGov/iotables")

#with vignettes:
#devtools::install_github("rOpenGov/iotables", build_vignettes = TRUE)
```

You can download the manual in PDF for the [0.9 development
release](https://iotables.dataobservatory.eu/iotables_0.9.pdf). and
follow the changes on the
[Changelog/NEWS](https://iotables.dataobservatory.eu/news/index.html).

See also a long-form documentation (publication candidate) [iotables: an
R Package for Reproducible Input-Output Economics Analysis, Economic and
Environmental Impact Assessment with Empirical
Data](https://zenodo.org/record/5887038#.Ye2ovv7MLIU) on Zenodo.

## Vignettes

The [Introduction to
iotables](https://iotables.dataobservatory.eu/articles/intro.html)
vignette presents most of the examples of the [Eurostat Manual of
Supply, Use and Input-Output
Tables](https://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF/b0b3d71e-3930-4442-94be-70b36cea9b39?version=1.0)
(Eurostat Manual, Chapter 15.) This is a good introduction to understand
what will the functions do, and to check that they work correctly. From
0.9.9 it extended with the replication results from the [Handbook on
Supply and Use Tables and Input-Output Tables with Extensions and
Applications](https://unstats.un.org/unsd/nationalaccount/docs/SUT_IOT_HB_Final_Cover.pdf)
published by the United Nations.

The `testthat` infrastructure of the package checks the proper working
of the functions against the published results from the
`Eurostat Manual`.

The [Working with Eurostat
Data](https://iotables.dataobservatory.eu/articles/working_with_eurostat.html)
vignette shows how you can download, pre-process and use real data from
Eurostat.

The [United Kingdom Input-Output Analytical Tables
2010](https://webarchive.nationalarchives.gov.uk/20160114044923/http://www.ons.gov.uk/ons/rel/input-output/input-output-analytical-tables/2010/index.html)
are used for testing the `iotables` package, because they are
well-documented and detailed, organized data is available with them.
These calculations can be followed in the [United Kingdom Input-Output
Analytical
Tables](https://iotables.dataobservatory.eu/articles/united_kingdom_2010.html)
vignette.

## Acquiring data

Eurostat’s data can be downloaded in several tidy, long-form, files, and
a lot of filtering is needed to start working with it.

Currently the following Eurostat SIOTs can be used:

| Table type                     |              source code              |
|:-------------------------------|:-------------------------------------:|
| product x product SIOTs        | `naio_10_cp1700` or `naio_10_pyp1700` |
| industry x industry SIOTs      | `naio_10_cp1750` or`naio_10_pyp1750`  |
| use tables at basic prices     | `naio_10_cp1620` or `naio_10_pyp1610` |
| trade and transport margins    | `naio_10_cp1620` or `naio_10_pyp1620` |
| net taxes less subsidies       | `naio_10_cp1630` or `naio_10_pyp1630` |
| Supply table at basic prices   |            `naio_10_cp15`             |
| Use table at purchasers’ price |            `naio_10_cp16`             |

The `cp` element refers to basic prices and the `pyp` to previous years’
prices.

### Contribute

Contributions are very welcome:

-   [Issue tracker](https://github.com/ropengov/iotables/issues) for
    feedback and bug reports.
-   [Pull requests](https://github.com/ropengov/iotables/)
-   [Github page](https://github.com/ropengov/iotables/)

### Acknowledgements

**Kindly cite this work** as follows:

Daniel Antal. (2022. September 24.) rOpenGov/iotables: Importing and
Manipulating Symmetric Input-Output Tables (Version 0.9.1). Zenodo.
[https://doi.org/10.5281/zenodo.7109769](https://zenodo.org/record/5153006/)
for released version. Development version URL:
<https://ropengov.github.io/iotables/>

Thanks to [@KKulma](https://github.com/KKulma/) for setting up new and
improved continuous integration, and
[@pitkant](https://github.com/pitkant) for implementing many good
practices on improving the code. See
[contributors](https://github.com/ropengov/iotables/graphs/contributors).
This project is part of [rOpenGov](http://ropengov.org).

## Code of Conduct

Please note that the iotables project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

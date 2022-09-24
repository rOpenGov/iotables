# iotables 0.9.1
* This is a minor release that fixes a bug report on `indirect_effects_create()` and a  HTML version of manual issue on CRAN.

# iotables 0.9
* This is a patch for a vignette that has occassional building problems, and the last release before the major release 1.0 with a long-form publication.
* The `germany_1990` dataset was incorrectly named, because it refers to another year, it is now corrected to be `germany_1995`.

# iotables 0.4.9
* Extending replication results from the [Handbook on Supply and Use Tables and Input-Output Tables with Extensions and Applications](https://unstats.un.org/unsd/nationalaccount/docs/SUT_IOT_HB_Final_Cover.pdf) published by the United Nations.
* A new function, 'gosh_inverse_create()' for the creation of forward linkages.
* See also a long-form documentation (publication candidate) [iotables: an R Package for Reproducible Input-Output Economics Analysis, Economic and Environmental Impact Assessment with Empirical Data](https://zenodo.org/record/5887038#.Ye2ovv7MLIU) on Zenodo.

# iotablles 0.4.8
* A new function, 'output_coefficients_create()' creates a conforming vector of various air pollutants for European input-output tables.
* A new vignette, [Introduction to iotables](https://iotables.dataobservatory.eu/articles/intro.html) shows comparable results with the Eurostat Manual.
* The [Environmental Impacts](https://iotables.dataobservatory.eu/articles/environmental_impact.html) is not comparable, it will need to be reworked to be comparable with the Eurostat Manual.

# iotablles 0.4.7
* A new function, 'airpol_get()' creates a conforming vector of various air pollutants for European input-output tables.
* A new vignette, [Environmental Impacts](https://iotables.dataobservatory.eu/articles/environmental_impact.html) shows its use.
* Released on CRAN.

# iotables 0.4.6
* Eliminating deprecated tidyverse functions. 
* New website.
* Released on CRAN (2021.12.18.)

# iotables 0.4.5
* Documentation improvement and preparing for Rbloggers.
* Removing UTF-8 characters from datasets.

# iotables 0.4.4
* Very small documentation changes.
* Changing the code to dplyr 1.0+ and rlang in non-standard evaluation.
* New website, CI on Github. Renewed [![codecov](https://codecov.io/gh/rOpenGov/iotables/branch/master/graph/badge.svg)](https://app.codecov.io/gh/rOpenGov/iotables), documentation changed to gfm flavored markdown.
* Thanks to [\@KKulma](https://github.com/KKulma/) for setting up new and improved continuous integration, and [\@pitkant](https://github.com/pitkant) for implementing many good practices on improving the code. See [contributors](https://github.com/ropengov/iotables/graphs/contributors). This project is part of [rOpenGov](http://ropengov.org).
* 0.4.4 is released on CRAN.

# iotables 0.4.3
* Generally improved documentation. Improving source code readabilty and vignette readability.
* Better handling of temporary files in `tempdir()` for improved performance with very large source files, including the new exported function `iotables_read_tempdir()` and `iotables_metadata_get()`.
* Following changes in dependencies `dplyr` and `tidyr`.
* [Contributor Code of Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html) is added to the github repo.
* Released on CRAN.

# iotables 0.4.2
On CRAN 0.4.1 passed early tests, but on deployment it 
turned out that a dependency is not available for  i386-pc-solaris2.10 (32-bit). This dependency is `magick`, which was supposed to help printing vignettes in Word (a very rare case) for `kableExtra` is not available on Solaris.  I removed this dependency. 

# iotables 0.4.1
A small formatting error in the documentation of `netherlands_2006` threw up a note on CRAN.

# iotables 0.4

New vignette [United Kingdom Input-Output Analytical Tables](https://iotables.dataobservatory.eu/articles/united_kingdom_2010.html) to compare analytical function results with published results from the UK National Office for Statistics.

# iotables 0.3.9
This is a much improved, simplified pre-release version, with highly improved
performance, more consistent function interface and far more readable code. The last CRAN release is stable, the following version will be a stable, better release candidate.
* The Germany example is converted to the ESA2010 vocabulary. 
* The `output_get()` is now a wrapper around `primary_input_get()` and 
`gva_get()` is completely deprecated.
*  The `input_coefficient_matrix_create()` is now a wrapper around the more
 general `coefficient_matrix_create()`.
*  The `use_table_get()` function is now named `input_flow_get()` but it 
is seldom used, as the analytical function will call it as needed.
* A completely dataset, metadata and vignette was added to test the functions on the United Kingdom Input-Output Analytical Tables, 2010. article and dataset.	
* The new function `supplementary_add()` can add a supplementary account or row to the table, useful to add employment data, for example.
* The new function `total_tax_add()` adds a summary of product and production taxes among the primary inputs.
* Clearly divided analytical functions, `direct_effects_create()` , `indirect_effects_create()`, `input_multiplier_create()` create the direct, indirect and total effects matrix. 

# iotables 0.3.8
* Function `input_indicator_create()` has a new, optional parameter for naming the new indicators in the key column.
* New convenience function for nicer printing of structured SIOT tables and related matrix results.  Not yet exported, first will be used on vignettes only. 

# iotables 0.3.7
* New example `data(netherlands_2006)`, terminology and analytical improvements following  [Input-Output Multipliers Specification Sheet and Supporting Material, Spicosa Project Report](http://www.coastal-saf.eu/output-step/pdf/Specification sheet I_O_final.pdf). Results are checked against the publication.
* For terminological clarity, earlier `direct_supply_effects_create()` function is renamed `direct_supply_effects_create()`.
* The function `effects_create()` currently refers to the function returning direct effects of a change in demand.
* The function `multipliers_create()` refers to the function returning direct and indirect effects of a change in demand.
* The Germany 1990 vignette needs to be reviewed. Currently it may not be consistent with effects and multipliers, it will be included in the next development version.


# iotables 0.3.6
* New analytical function `coefficient_matrix_create()` which is a more general version of `input_coefficient_matrix_create()` and will eventually replace that 
function.
* Custom SIOTs are supported, but there is no vignette yet to describe their 
use in detail.  

# iotables 0.3.5
* New function `iotable_year_get()` which returns the available tables by year or time from the bulk database for a given country and currency unit. 
* Package now handles `Use table at basic prices` (naio_10_cp1610 and naio_10_pyp1610), `Supply table at basic prices incl. transformation into purchasers' prices` (naio_10_cp15), `Use table at purchasers' prices` (naio_10_cp16) after correcting a vocabulary error.
* This version is a release candidate for CRAN.


# iotables 0.3.4
* New function `direct_effects()` which calculates all input indicators that `input_indicator_create()` does individually.
* New vignette to [work with real Eurostat data](https://iotables.dataobservatory.eu/articles/working_with_eurostat.html).
* New [package website](https://iotables.dataobservatory.eu/).
* The [ReadMe](https://iotables.dataobservatory.eu/index.html) file shortened, most of the contents are moved now to the two vignettes.

# iotables 0.3.3
* New vignette *Germany 1990*, renamed in 0.4.6 [Introduction to iotables](https://iotables.dataobservatory.eu/articles/intro.html) with the [Eurostat Manual of Supply, Use and Input-Output Tables](https://ec.europa.eu/eurostat/documents/3859598/5902113/KS-RA-07-013-EN.PDF/b0b3d71e-3930-4442-94be-70b36cea9b39?version=1.0) example tables to show the working of the data processing and analytical functions.
* Finding an exception in the Eurostat vocabulary that preventing correctly identifying the  "Computer programming, consultancy and related services; Information services" industry in the rows. This is a critical problem in the Eurostat bulk files that prevents the creation of symmetric matrixes. 
* Minor bug fixes.
* New function `output_coefficient_matrix_create()`
* New function `output_multiplier_create()`
* New function `forward_linkages()`
* New function `backward_linkages()`

# iotables 0.3.1
* Correcting a bug that did not always remove the right columns from `use_table_get()`. Released on CRAN.

# iotables 0.3.2
* Better exception handling in `equation_solve()`.  Removes trivial erroneous, all-zero columns. Efficiency gains in indicator and multiplier creation. New function to create backward linkages. Released on CRAN.

# iotables 0.3.0
* Correctly handles some countries that differ from the standard SIOT structure. The Czech Republic does not include G47 and I68 imputed rents in the table, which caused problems with the ordering of the table and the creation of coefficients.
* Uses tidyr::nested() data structures which are far easier handled in highly structured data sets.

# iotables 0.2.9
Error corrected concerning creation of temporary files on non-Windows environments. No uses `file.path()` to create platform-independent files. Released on CRAN.

# iotables 0.2.8
Handles tables with purchasers' prices. Imports Eurostat employment data.

# iotables 0.2.7 
Default data directory for faster programmatic use and phasing out discontinued Eurostat statistics. A bit improved messages and documentation.

# iotables 0.2.6
* Download `naio_10_cp1620` (trade and transport margins) or `naio_10_cp1630` (net taxes) with the parameter `stk_flow="TOTAL"`.  Correctly builds on Windows and Mac OS (old and release version) but problem with the dependency `eurostat` in Linux on Travis.

# iotables 0.2.4-0.2.5
* Some documentation improvements. 

# iotables 0.2.3
* Eurostat withdrew naio_cp17_r2, and there is a temporary fix to work with archived versions. Some metadata errors and documentation
errors were corrected. 

# iotables 0.2.2
* Updated README file and new Croatia vignette. 

# iotables 0.2.1
* Correctly handles the total Eurostat national accounts vocabulary, and adds all Croatian (not fully conforming) tables to the package. 


# iotables 0.2 
The forthcoming update will rewrite and generalize some of the earlier functions. In particular, the following features will be available:
* Handling 7 types of Eurostat tables, not only the current price table.
* Closing off households, i.e. including endogenous demand for induced effects.
* Guidelines to work with OECD mass downloader. 
* Some earlier functions are phased out, because they are successfully generalized.
* The standard system.rda contains the metadata necessary to re-order the bulk files. This will require a lot of patient testing, and it can be always updated with new data sources.

At this point I will go with preparing a release the table on CRAN. The current version builds without warnings or notes on Windows and Linux. 

iotables 0.2 is moved to [rOpenGov](http://ropengov.org/).
 
 
# iotables 0.1.4
New functions are added which enable a very simple IO analysis. As an important milestone, an analytical solution comparable to the Eurostat manual, the German employment indicators are correctly calculated (see `README`)
* The `input_indicator_create()` creates the indicators.
* The `equation_solve()` solves the basic input-output equations on real-life data, i.e. correctly prepared the two sides of the equation product- or industrywise. 
* The `multiplier_create()` is a wrapper around the more general `equation_solve()`
* The `README` contains examples to use this new functions. 
* Some new tests are added to the  `testthat` functions.
* Further example data is added from Croatia, together with the `employment_aggregate()` helper function to match SIOT data with non-conforming employment statistics.
* There is a vignette called _Working with Croatian Symmetric Input-Output Tables_ that shows some real-life uses. 
* Testing on Travis (ubuntu and OSX tests.)

# iotables 0.1.3

* Added a `NEWS.md` file to track changes to the package.
* Added Contributor Code of Conduct in `CONDUCT.md`
* Some functions have parallel versions, and will be brought to [rOpenSci](https://github.com/ropensci/software-review) naming conventions (`object_verb()`)
* Asked to join [rOpenGov](http://ropengov.github.io/) - excited but inexperienced in this!
# iotables 0.1.3

* Added a `NEWS.md` file to track changes to the package.
* Added Contributor Code of Conduct in `CONDUCT.md`
* Some functions have paralell versions, and will be brought to [rOpenSci](https://github.com/ropensci/onboarding) naming conventions (`object_verb()`)
* Asked to join [rOpenGov](http://ropengov.github.io/) - excited but inexperienced in this!

# iotables 0.1.4
New functions are added which enable a very simple IO analysis. As an important milestone, an analytical solution comparable to the Eurostat manual, the German employment indicators are correctly calculated (see `README`)
* The `input_indicator_create()` creates the indicators.
* The `equation_solve()` solves the basic input-output equations on real-life data, i.e. correctly prepared the two sides of the equation product- or industrywise. 
* The `multiplier_create()` is a wrapper around the more general `equation_solve()`
* The `README` contains examples to use this new functions. 
* Some new tests are added to the  `testthat` functions.
* Further example data is added from Croatia, together with the `employment_aggregate()` helper function to match SIOT data with non-conforming employment statistics.
* There is a vignette called _Working with Crotian Symmetric Input-Output Tables_ that shows some real-life uses. 
* Testing on Travis (ubuntu and OSX tests.)

# iotables 0.2 
The forthcoming update will rewrite and generalize some of the earlier functions. In particular, the following features will be available:
* Handling 7 types of Eurostat tables, not only the current price table.
* Closing off households, i.e. including endogenous demand for induced effects.
* Guidelines to work with OECD mass downloader. 
* Some earlier functions are phased out, because they are successfully generalized.
* The standard system.rda contains the metadata necessary to re-order the bulk files. This will require a lot of patient testing, and it can be always updated with new data sources.

At this point I will go with prepareing a release the table on CRAN. The current version builds without warnings or notes on Windows and Linux. 

iotables 0.2 is moved to rOpenGov.

# iotables 0.2.1
* Correctly handles the total Eurostat national accounts vocabulary, and adds all Croatian (not fully conforming) tables to the package.  

# iotables 0.2.2
* Updated README file and new Croatia vignette. 

# iotables 0.2.3
* Eurostat withdrew naio_cp17_r2, and there is a temporary fix to work with archived versions. Some metadata errors and documentation
errors were corrected. 

# iotables 0.2.4
* Some documentation improvements. This version was submitted for CRAN for release.

# iotables 0.2.5
* Some documentation improvements.

# iotables 0.2.6
* Download `naio_10_cp1620` (trade and transport margins) or `naio_10_cp1630` (net taxes) with the parameter `stk_flow="TOTAL"`.  Correctly builds on Windows and Mac OS (old and release version) but problem with the dependency `eurostat` in Linux on Travis.

# iotables 0.2.7 
Default data directory for faster programmatic use and phasing out discountinued Eurostat statistics. A bit improved messages and documentation.

# iotables 0.2.8
Handles tables with purchasers' prices. Imports Eurostat employment data.

# iotables 0.2.9
Error corrected concerning creation of temporary files on non-Windows environments. No uses `file.path()` to create platform-independent files. Released on CRAN.

# iotables 0.3.0
* Correctly handles some countries that differ from the standard SIOT structure. The Czech Republic does not include G47 and I68 imputed rents in the table, which caused problems with the ordering of the table and the creation of coefficients.
* Uses tidyr::nested() data structures which are far easier handled in highly structured data sets.

# iotables 0.3.1
* Correcting a bug that did not always remove the right columns from `use_table_get()`. Released on CRAN.

# iotables 0.3.2
* Better exception handling in `equation_solve()`.  Removes trivial erroneous, all-zero columns. Efficiency gains in indicator and multiplier creation. New function to create backward linkages. Released on CRAN.

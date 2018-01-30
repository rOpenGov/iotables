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

#Forthcoming 0.3 version 
For the 0.3 version the OECD downloader will work, too. The sysdata.rda has to be updated with correct metadata info for OECD bulk downloaded files.


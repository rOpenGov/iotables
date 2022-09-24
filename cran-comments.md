## Test environments
* local Window 10 install, r-version 4.1.2


## testthat results
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 87 ]

## R CMD check results
0 errors v | 0 warnings v | 0 notes v

## Notes
1. I received an email from CRAN about "Found the following HTML validation
problems" NOTEs in the "HTML version of manual" check for at least some
of the r-devel checks results. ... R 4.2.0 switched to use HTML5 for documentation pages.  Now validation using HTML Tidy finds problems in the HTML generated from your Rd
files. 

2. A small reported bug was fixed in indirect_effects_create() and an extra unit test was added.
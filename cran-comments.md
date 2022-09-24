## Test environments
* x86_64-w64-mingw32 (64-bit), R version 4.2.1 (2022-06-23 ucrt)
* R-devel with win-builder.r-project.org, x86_64-w64-mingw32, R Under development (unstable) (2022-09-23 r82903 ucrt)
* -oldrelease with win-builder.r-project.org
* mac.r-project.org
* Appveyor R version 4.2.1 Patched (2022-09-23 r82903 ucrt)


## testthat results
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 87 ]

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Notes
1. I received an email from CRAN about "Found the following HTML validation
problems" NOTEs in the "HTML version of manual" check for at least some
of the r-devel checks results. ... R 4.2.0 switched to use HTML5 for documentation pages.  Now validation using HTML Tidy finds problems in the HTML generated from your Rd
files. 

2. A small reported bug was fixed in indirect_effects_create() and an extra unit test was added.
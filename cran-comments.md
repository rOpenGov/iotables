## Test environments
* local Window 10 install, r-version 4.10
* Microsoft Windows Server 2019 10.0.17763 on Github (r-version: release)
* Mac OS X 10.15.7 on Github (r-version: release)
* ubuntu 20.04 on Github (r-version: release)
* ubuntu 20.04 on Github (r-version: devel)
* r_hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* r_hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC

On Windows Server 2008 R2 SP1, R-devel, 32/64 bit bBuild failed during preparation, we believe that this is an r_hub setting error, as our local and github Windows tests had no issues.

## testthat results
FAIL 0 | WARN 0 | SKIP 0 | PASS 48 

## R CMD check results
0 errors √ | 0 warnings √ | 0 notes √

## Notes
I received a notification from CRAN that the package contains non-ASCII characters, and will be removed by 2 August 2021 if not fixed.  I work on a Windows system, adn my devtools::check() indicates that there are no non-ASCII characters in the R code or data files, and I also checked the vignettes, where I have found one.  I wrote a conversion function for all saved data files that explicitly changes factor variables and column headings to ASCII.

My tests continue to show no UTF8 characters, but if there are any, I will re-create the data files on a Linux system. I believe that they were creeping in from the data sources of the example data SIOTs.
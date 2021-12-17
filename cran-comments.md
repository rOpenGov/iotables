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
This version is eliminates deprecated functions from tidyverse, particularly
* dplyr mutate_if, mutate_at funs
* tidyselect one_of
* tidyr gather, and most instances of spread.

The empty_remove function was earlier an internal function but it has siginificant user value and is now exported (with new unit tests.)
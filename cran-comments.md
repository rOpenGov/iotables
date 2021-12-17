## Test environments
* local Window 10 install, r-version 4.1.2
* Microsoft Windows Server 2019 10.0.17763 on Github (r-version: release)
* Mac OS X 11.6.1 on Github (r-version: release)
* ubuntu 20.04.3 on Github (r-version: release)
* ubuntu 20.04.3 on Github (r-version: devel)
* r_hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* r_hub: Fedora Linux, R-devel, clang, gfortran
* r_hub: Windows Server 2022, R-devel, 64 bit

## testthat results
FAIL 0 | WARN 0 | SKIP 0 | PASS 50 

## R CMD check results
0 errors √ | 0 warnings √ | 0 notes √

## Notes
This version is eliminates deprecated functions from tidyverse, particularly
* dplyr mutate_if, mutate_at funs, group_by
* tidyselect one_of
* tidyr gather, and most instances of spread.

The empty_remove() function was earlier an internal function but it has siginificant user value and is now exported (with new unit tests.)
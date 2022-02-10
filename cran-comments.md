## Test environments
* local Window 10 install, r-version 4.1.2
* Microsoft Windows Server 2019 10.0.17763 on Github (r-version: release)
* ubuntu 20.04.3 on Github (r-version: release)
* ubuntu 20.04.3 on Github (r-version: devel)
* r_hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* r_hub: Fedora Linux, R-devel, clang, gfortran
* r_hub: r-release-macosx-arm64|4.1.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1|

## testthat results
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 86 ]

## R CMD check results
0 errors v | 0 warnings v | 0 notes v

## Notes
I received a note from CRAN that the fedora build had a broken link and the package will be removed by 12 Februrary 2022.  'Packages which use Internet resources should fail gracefully with an informative message if the resource is not available or has changed (and not give a check warning nor error).' The link was not broken, and I think that this problem was due to some latency on the Eurostat website, but the vignette no longer makes this external download.

There is a planned major release of verion 1.0 of this package with a publication in the near future, but we did not want to include new functionality in this patch release, only documentation improvements and the removal of the download that occassionally gets too slow.

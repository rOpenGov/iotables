## Test environments
* local Window 10 install, r-version 4.1.2
* Microsoft Windows Server 2019 10.0.17763 on Github (r-version: release)
* ubuntu 20.04.3 on Github (r-version: release)
* ubuntu 20.04.3 on Github (r-version: devel)
* r_hub: Ubuntu Linux 20.04.1 LTS, R-release, GCC
* r_hub: Fedora Linux, R-devel, clang, gfortran
* r_hub: r-release-macosx-arm64|4.1.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1|

## testthat results


## R CMD check results


## Notes
This is mainly a documentation release that prepares the package for a peer-reviewed journal publication, and mainly adds new unit tests that replicate published, reliable sources. The only real addition to the exported functionality is ghosh_inverse_create, which is, however, only a wrapper using supply-side name conventions for leontief_inverse_create.  
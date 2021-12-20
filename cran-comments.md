## Test environments
* local Window 10 install, r-version 4.1.2
* Microsoft Windows Server 2019 10.0.17763 on Github (r-version: release)
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
Apologies for the quick succession of submissions – I received an email from Prof Ripley that there was a problem in the 0.4.6. CRAN release of 2021-12-18.

0.4.6. has no functionality compared to 0.4.5. from August but eliminates many deprecated tidyverse functions. It passed all checks (and all CRAN builds are OK) but there was an error in the only not tested example.  This example in iotables_download() is very large - this is why it is excluded from checks.

I was not able to reproduce the error, the most likely reason is a temporary outage of the Eurostat data warehouse when the iotables_download() function could not download anything. Because the downloading from Eurostat is essential to this function, the following steps were made:
- A new example file was chosen that is significantly smaller on the Eurostat source.
- Several assertions were made that the download actually took place. In the unlikely event that this problem arrises again, it will be easier to pinpoint to a yet unknown problem. [This function had caused no problems in the previous CRAN releases for years and there is no reason to assume that it will.]

However, the current release canidate already includes new functionality that is not just a patch of this issue raised on CRAN. While all functions of 0.4.6. are unchanged, the vector_transpose() and airpol_get() functions, and the Environmental impact vignette are new additions.



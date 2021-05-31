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
This is not a major release.  We made many good practice changes in the code, and update the tidyverse dependencies to dplyr 1.0+, and added rlang .data pronouns for stricter non-standard evaluation. We moved the continous integration from Travis to Github.

We placed two function examples in \donttest{}. These functions (iotables_read_tempdir() nad iotables_metadata_get()) are helper functions for working with downloaded, real-life examples from Eurostat.  Eurostat's original raw data file (saved to tempdir() in the user session) is bigger than 15 MB and it is impractical test on CRAN, but we have tested the functions locally. 
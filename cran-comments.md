## Test environments
* Windows 10, x86_64-w64-mingw32, R version 4.3.2 (2023-10-31 ucrt)
* r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 on macbuilder


## testthat results
[ FAIL 0 | WARN 0 | SKIP 6 | PASS 234 ]

Skipped tests work with downloading large Eurostat datasets from the Eurostat 
warehouse for reproducing data samples.  They are not needed for testing the 
package functionality and due to their heavy payload are exempted from testing 
on CRAN.

## Notes
This is a minor release of iotables.

- Updates dependencies to current tidyverse (avoiding deprecated functions).
- Fixes small bugs in dataset reproducibility and documentation mismatches.
- Improves vignettes, terminology notes, and metadata alignment.
- Adds background source checks for built-in datasets (Eurostat, UN, OECD).
- Prepares for a forthcoming major release with extended SIOT coverage
  and new analytical functionality.

This release maintains backward compatibility and focuses on stability,
documentation, and quality control.
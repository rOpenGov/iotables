## Test environments
* Windows 10, x86_64-w64-mingw32, R version 4.3.2 (2023-10-31 ucrt)
* r-release-macosx-arm64|4.3.0|macosx|macOS 13.3.1 (22E261)|Mac mini|Apple M1||en_US.UTF-8|macOS 11.3|clang-1403.0.22.14.1|GNU Fortran (GCC) 12.2.0 on macbuilder
* Windows Server 2022 x64 (build 20348), R version 4.3.2 (2023-10-31 ucrt) https://win-builder.r-project.org/
* Windows Server 2022, R-devel, 64 bit on rhub
* Appveyor R version  4.3.2 Patched (2023-12-19 r85721 ucrt)
* R Under development (unstable) (2023-12-21 r85716) on GitHUb, x86_64-pc-linux-gnu
*  Ubuntu Linux 20.04.1 LTS, R version 4.3.2 (2023-10-31) on GitHub

## testthat results
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 87 ]

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Notes
- I received an email from CRAN about issues on various Linux platforms with documentation problems. These were related to changes in Roxygen that I fixed. 
- I also fixed the change in tidyverse reference to the . pronoun (.data$ -> $) that gave warnings in tests, and placing global variable definitions instead when needed.

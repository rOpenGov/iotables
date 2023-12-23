## Test environments
* Windows 10, x86_64-w64-mingw32, R version 4.3.2 (2023-10-31 ucrt)
* Ubuntu Linux 20.04.1 LTS, R-release, GCC on rhub
* Fedora Linux, R-devel, clang, gfortran on rhub
* Windows Server 2022 x64 (build 20348), R version 4.3.2 (2023-10-31 ucrt) https://win-builder.r-project.org/
* Windows Server 2022, R-devel, 64 bit on rhub
* Appveyor R version 4.2.1 Patched (2022-09-23 r82903 ucrt)

## testthat results
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 87 ]

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Notes
The changes from the last CRAN release are minimal and relate to the change in Roxygen and tidyverse syntax. I received an email from CRAN about issues on various Linux platforms with documentation problems. These were related to changes in Roxygen that I fixed. I also fixed the change in tidyverse reference to the . pronoun (.data$ -> $) that gave warnings in tests.

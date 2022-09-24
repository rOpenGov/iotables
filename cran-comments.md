## Test environments
* x86_64-w64-mingw32 (64-bit), R version 4.2.1 (2022-06-23 ucrt)
* R version 4.1.3 (2022-03-10) with oldrelease on win-builder.r-project.org
* R-devel with win-builder.r-project.org, x86_64-w64-mingw32, R Under development (unstable) (2022-09-23 r82903 ucrt) 
* r-release-macosx-arm64|4.2.1|macosx|macOS 11.5.2 (20G95)|Mac mini|Apple M1||en_US.UTF-8
* Fedora Linux, R-devel, clang, gfortran on <https://builder.r-hub.io/>
* Ubuntu Linux 20.04.1 LTS, R-release, GCC on <https://builder.r-hub.io/>
* Windows Server 2022, R-devel, 64 bit on <https://builder.r-hub.io/>
* Appveyor R version 4.2.1 Patched (2022-09-23 r82903 ucrt)
* Github actions <https://github.com/rOpenGov/iotables/actions>

## testthat results
[ FAIL 0 | WARN 0 | SKIP 0 | PASS 87 ]

## R CMD check results
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Notes
1. I received an email from CRAN about "Found the following HTML validation
problems" NOTEs in the 'HTML version of manual' check for at least some
of the r-devel checks results. ... 'R 4.2.0 switched to use HTML5 for documentation pages.  Now validation using HTML Tidy finds problems in the HTML generated from your Rd
files.'" -> fixed documentation issues, and at the same time reviewed outdated URLs.

2. A small reported bug was fixed in indirect_effects_create() and an extra unit test was added.

3. Minor changes to conform CRAN recommendations were implemented in the code.
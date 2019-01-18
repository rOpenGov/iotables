## Test environments
* local Window 10 install, R 3.5.1

with devtools::check_rhub() on https://builder.r-hub.io checked with 
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results
0 errors | 0 warnings | 0 notes

##Notes
On CRAN the earlier submission passed early tests, but on deployment it 
turned out that a dependency is not available for  i386-pc-solaris2.10 (32-bit). magick, which was supposed to help printing vignettes in Word (a very rare case) for kableExtra is not available on Solaris.  I removed this dependency. 
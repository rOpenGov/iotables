## Test environments
* local Window 10 install, R 3.5.1

with devtools::check_rhub() on https://builder.r-hub.io checked with 
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit
* Ubuntu Linux 16.04 LTS, R-release, GCC
* Fedora Linux, R-devel, clang, gfortran

## R CMD check results
0 errors | 0 warnings | 0 notes

##Notes
I tried to submit the package, and there were 1 NOTE that did no appear 
on during my testing. It was created an URL in sources which is malformatted
on Linux system. I removed the malformatted URL, because the problem was 
persisent on Linux systems however I tried to overcome it.

I also modified a warning since 0.4.0 but it has no effect on tests.
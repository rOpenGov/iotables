## Test environments
* local Window 10 install, R 3.5.1
* osx R 3.4.4 and 3.5.0 (on travis-ci)

## R CMD check results
There were no ERRORs or WARNINGs. 

NOTE: Previous submission threw a NOTE because an extdata file was 28Mb.
I removed much of that example and reduced the size of the data file, the 
reviewer comment was the total should not be more than 5Mb.  I cannot measure
the total size, but the external data file is now a bit less than 1Mb.
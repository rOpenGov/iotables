## Test environments
* local Window 10 install, R 3.5.1
* Windows Server 2008 R2 SP1, R-devel, 32/64 bit (R-hub builder)

## R CMD check results
There were no ERRORs or WARNINGs. 

There is one NOTE

':::' calls which should be '::':
However, this relates to another package's internal call, i.e. 'knitr:::is_html_output' 'knitr:::is_latex_output'.


* Ubuntu Linux 16.04 LTS, R-release, GCC (R-hub builder) had un unrelated error, 
because a dependency of the dependency Eurostat did not install properly. This
2nd level dependency is drawing maps for the Eurostat package, but this feauture is 
unrelated to iotables. Eurostat package provides a downloading interface to the 
Eurostat bulk download facility, that is absolutely necessary for iotables, but 
apart from downloading the correct files and labelling them, there is no further
dependence on Eurostat.
## Test environments
* Windows 11, R version 4.5.0 (2025-04-11 ucrt)
* rhub: linux, macOS (intel/arm64), Windows; 
* rhub: Windows Server 2022, old, current and new release

## testthat results
[ FAIL 0 | WARN 0 | SKIP 6 | PASS 234 ]

Skipped tests involve downloading large Eurostat datasets from the Eurostat 
warehouse for reproducing sample data.  
They are not required for package functionality and are exempt from CRAN testing 
due to their heavy payload.

## Notes
This is a **minor release** of **iotables**.

- Updates dependencies to the current tidyverse, avoiding deprecated functions.
- Fixes small bugs in dataset reproducibility and documentation mismatches.
- Improves vignettes, terminology notes, and metadata alignment.
- Adds background source checks for built-in datasets (Eurostat, UN, OECD).
- Prepares for a forthcoming **major release** with extended SIOT coverage and 
  new analytical functionality.

This release maintains backward compatibility and focuses on **stability,
documentation, and quality control**.

### External links
The package includes archived links to the UK National Archives 
(ONS Input-Output Analytical Tables 2010).  
These links return HTTP 202 *Accepted* but are valid and stable archival 
references.  
They are used **only in a vignette** for replication/validation and are not 
essential for package functionality.

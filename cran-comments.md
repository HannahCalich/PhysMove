## Submission - Package update

This is a minor update (1.2.4 -> 1.2.5), submitted promptly to ensure the
package's argument names and vignettes stay consistent with the
accompanying manuscript, which is currently in proof stage. It:
* Renames an argument in `plotDispPDF()` for spelling consistency
  (normalised -> normalise), matching the manuscript and vignettes
* Fixes an incorrect intercept in a reference line plotted by `randomise()`
  (was 1, should be 0), which affected the visual interpretation of results
* Corrects two documentation errors

Thank you for your time and review.

## Test environments
- Local: Windows (R 4.6.0)
- R-hub: Ubuntu (R 4.4.0, R-release, R-devel)
- GitHub Actions:
  - ubuntu-latest (R 4.4, release, devel)
  - macos-latest (R 4.4, release, devel)
  - windows-latest (R 4.4, release, devel)

## R CMD check results
There were no ERRORs or WARNINGs. There were three NOTES:

1. CRAN incoming feasibility: 
  * Days since last update is 1. 
  This update was submitted promptly to keep the package consistent with an 
  accompanying manuscript currently in proof stage (see above), and to fix a 
  plotting bug affecting statistical interpretation. 
  * This NOTE also flags that 'infomapecology' (listed in Suggests) 
  is not available from a mainstream repository. 
  This package is available via the Additional_repositories field and is 
  required only for optional functionality; core functionality, package
  loading, examples, and tests do not require it.
  
2. The installed package size is ~7.6 MB. Sub-directories of 1Mb or more include 
  data (2.8Mb) and doc (4.5Mb). 
  These files are needed to support examples and vignettes.

3. Future file timestamps unable to verify current time. 
  This is due to OneDrive syncing and does not affect package functionality.

Continuous integration via GitHub Actions confirms checks on multiple platforms 
and R versions.

## Downstream dependencies
There are no downstream dependencies.

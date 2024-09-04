### Package development and CRAN checks

### Dev history based on recommendations from:
# https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/r-package/
# https://github.com/ThinkR-open/prepare-for-cran

#===========================================================================================
### Package details
#===========================================================================================
devtools::document() # Update manual
# usethis::use_news_md(open = rlang::is_interactive()) # Create NEWS - Bump version manually and add list of changes
usethis::use_cran_comments(open = rlang::is_interactive()) # Update comments for CRAN
# usethis::use_version("patch") # Upgrade version number, options include: c("patch", "minor", "major", "dev")
# usethis::use_gpl_license() # Update license info - if applicable
devtools::spell_check() # Check spelling
attachment::att_amend_desc(pkg_ignore = c("infomapecology"), extra.suggests = c("infomapecology"), update.config = TRUE) # Amend dependencies in description
# Code above adds DEV folder which is non-standard and needs to be manually deleted

checkhelper::find_missing_tags() # check for missing return value for exported functions and missing @export tags
#all_files_remaining <- checkhelper::check_clean_userspace() # listed as "experimental" in cran docs so skipping this because it's giving unclear messages about temp files
#all_files_remaining
# checkhelper::check_as_cran()# listed as "experimental" in cran docs so skipping this because it's not checking the correct packages
urlchecker::url_check() # this will throw errors until github is public
# urlchecker::url_update() # this will throw errors until github is public

#===========================================================================================
### Package standard formatting checks
#===========================================================================================
goodpractice::gp() # Goodpractice check -- skipping for now, some good ideas but not essential
inteRgrate::check_pkg() # "installs package dependencies, builds & installs the package, before running package check"
inteRgrate::check_lintr() # check if code "adheres to standards" -- skipping this, some good ideas but not essential
inteRgrate::check_tidy_description() # Check description is tidy -- OK
inteRgrate::check_r_filenames() # Check file names are correct -- file names should be lower case but this makes things too hard to read
inteRgrate::check_gitignore() # Check .gitignore contains standard files -- OK

### Summary of standard formatting checks:
## goodpractice::gp() and inteRgrate::check_lintr() has some good ideas but they aren't essential
## File names should be converted to lower case but this makes the function names too hard to read so skipping this for now
## All others ok/pass

#===========================================================================================
### Run tests and examples
#===========================================================================================
# usethis::use_test("PhysMove") # set up test files (commented because this creates initial test files)
devtools::test() # Runs all tests in package -- Pass
devtools::test_coverage() # Determines the percentage of package covered by tests -- may need to restart r session for this to work
devtools::run_examples() # Check examples -- OK

### Summary of test and example checks:
## test_coverage not used because all troubleshooting was done before I learned about this option, but it will be used in future
## All others ok/pass

#===========================================================================================
### Check package as CRAN
#===========================================================================================
devtools::check() # Local R CMD check -- OK
devtools::check(remote = TRUE, manual = TRUE) # Remote CRAN check with manual -- some expected notes, see below
# rcmdcheck::rcmdcheck() # This is struggling to find the drat repos but they are working fine and devtools can find them, might be a bug.

#===========================================================================================
### Test drat repos work
install.packages("infomapecology", repos="https://HannahCalich.github.io/drat")
install.packages("emln", repos="https://HannahCalich.github.io/drat")
#===========================================================================================

# rhub::rhub_setup() # done already
# rhub::rhub_check() # moved to GitHub actions.
# Github actions yaml files updated to ignore infomapecology and emln based on bug reported here: https://github.com/r-hub/rhub2/issues/11

#===========================================================================================
### Summary of devtools notes
#===========================================================================================
## Summary of CRAN checks incl on other distributions:
## checking CRAN incoming feasibility ... NOTE
## Maintainer: 'Hannah J. Calich <hannah.calich@anu.edu.au>'
##
## New submission
##
## Suggests or Enhances not in mainstream repositories:
##   emln, infomapecology
## Availability using Additional_repositories specification:
##   emln             yes   https://HannahCalich.github.io/drat
## infomapecology   yes   https://HannahCalich.github.io/drat
##
## Found the following (possibly) invalid URLs:
##   URL: https://github.com/HannahCalich/PhysMove
## From: DESCRIPTION
## Status: 404
## Message: Not Found
## URL: https://github.com/HannahCalich/PhysMove/actions
## From: README.md
## Status: 404
## Message: Not Found
## URL: https://github.com/HannahCalich/PhysMove/issues
## From: DESCRIPTION
## Status: 404
## Message: Not Found
##
## Size of tarball: 5422399 bytes
##
## 0 errors ✔ | 0 warnings ✔ | 1 note ✖

#===========================================================================================
### Check reverse dependencies -- Not currently relevant but keeping for future
#===========================================================================================
# remotes::install_github("r-lib/revdepcheck")
# install.packages('revdepcheck', repos = 'https://r-lib.r-universe.dev')
# usethis::use_git_ignore("revdep/")
# usethis::use_build_ignore("revdep/")

# devtools::revdep()
# library(revdepcheck)
# # In another session
# id <- rstudioapi::terminalExecute("Rscript -e 'revdepcheck::revdep_check(num_workers = 4)'")
# rstudioapi::terminalKill(id)
# # See outputs
# revdep_details(revdep = "pkg")
# revdep_summary()                 # table of results by package
# revdep_report() # in revdep/
# # Clean up when on CRAN
# revdep_reset()

#===========================================================================================
### Release package
#===========================================================================================

## Update News.md
## Update README.md
## usethis::use_version("patch")
## devtools::check() # Run one last check

# PUSH CHANGES TO GITHUB

## devtools::release() # Verify you're ready for release, and release -- used to SUBMIT package to CRAN

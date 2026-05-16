### Package development and CRAN checks

### Dev history based on recommendations from:
### https://www.mzes.uni-mannheim.de/socialsciencedatalab/article/r-package/
### https://github.com/ThinkR-open/prepare-for-cran

#===========================================================================================
### Package details
#===========================================================================================
devtools::document() # Update manual
# usethis::use_news_md(open = rlang::is_interactive()) # Create NEWS - Bump version manually and add list of changes
# usethis::use_cran_comments(open = rlang::is_interactive()) # Update comments for CRAN
# usethis::use_version("minor") # Upgrade version number, options include: c("patch", "minor", "major", "dev")
# usethis::use_gpl_license() # Update license info - if applicable
devtools::spell_check() # Check spelling
# attachment::att_amend_desc(pkg_ignore = c("infomapecology"), extra.suggests = c("infomapecology"), update.config = TRUE) # Amend dependencies in description
# Code above adds DEV folder which is non-standard and needs to be manually deleted
checkhelper::find_missing_tags() # check for missing return value for exported functions and missing @export tags
urlchecker::url_check()

### RESULTS:
### PASS

#===========================================================================================
### Package standard formatting checks
#===========================================================================================
# goodpractice::gp() # Goodpractice check -- skipping for now, some good ideas but not essential
# inteRgrate::check_pkg() # "installs package dependencies, builds & installs the package, before running package check"
# inteRgrate::check_lintr() # check if code "adheres to standards" -- skipping this, some good ideas but not essential
# inteRgrate::check_tidy_description() # Check description is tidy -- OK
# inteRgrate::check_r_filenames() # Check file names are correct -- file names should be lower case but this makes things too hard to read
# inteRgrate::check_gitignore() # Check .gitignore contains standard files -- OK

### RESULTS:
### SKIPPED

#===========================================================================================
### Run tests and examples
#===========================================================================================
devtools::test() # Runs all tests in package -- Pass
devtools::run_examples() # Check examples -- OK

### RESULTS:
### PASS

#===========================================================================================
### Check package as CRAN
#===========================================================================================
devtools::check() # Local R CMD check
devtools::check(remote = TRUE, manual = TRUE) # Remote CRAN check with manual -- some expected notes, see below
rcmdcheck::rcmdcheck()
rcmdcheck::rcmdcheck(args = c("--no-manual"), build_args = c("--no-manual")) # Removed manual as I don't have LaTeX installed locally

### RESULTS:
### 3 Notes --
### checking installed package size (note added to cran comments)
### checking for future file timestamps (likely d/t OneDrive)
### Suggests or Enhances not in mainstream repositories (note added to cran comments)

#===========================================================================================
### Version checks (>=4.4)
#===========================================================================================
rhub::rhub_check(platform = "linux", r_version = "4.4")
rhub::rhub_check(platform = c("linux", "windows", "macos"))
rhub::rhub_check(platform = "linux", r_version = "devel")

### RESULTS:
### 3x PASS

#===========================================================================================
### Test drat repos work
#===========================================================================================
install.packages("infomapecology", repos="https://HannahCalich.github.io/drat")
install.packages("emln", repos="https://HannahCalich.github.io/drat")

### RESULTS:
### 2x INSTALLED

#===========================================================================================
### Rhub action checks
#===========================================================================================
# rhub::rhub_setup() # done already
# rhub::rhub_check() # moved to GitHub actions.
# Github actions yaml files updated to ignore infomapecology and emln based on bug reported here: https://github.com/r-hub/rhub2/issues/11

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

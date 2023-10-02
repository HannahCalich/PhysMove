### Prepare for CRAN ----

### Update license info - if applicable
# usethis::use_gpl_license()

### Run tests and examples
# usethis::use_test("PhysMove") # set up test files
devtools::test() #
devtools::run_examples()

### Check package as CRAN
rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran"))
devtools::check(remote = TRUE, manual = TRUE)

### Check content
# install.packages('checkhelper', repos = 'https://thinkr-open.r-universe.dev')
checkhelper::find_missing_tags()
# _Check that you let the house clean after the check, examples and tests
all_files_remaining <- checkhelper::check_clean_userspace()
all_files_remaining

### Check spelling
# usethis::use_spell_check()
spelling::spell_check_package()

### Check URL are correct
# install.packages('urlchecker', repos = 'https://r-lib.r-universe.dev')
urlchecker::url_check()
urlchecker::url_update()

### check on other distributions
devtools::check_rhub(platforms = "fedora-clang-devel") # _rhub
# Notes from rhub that are apparently associated with r-hub and not PhysMove
# N  checking for non-standard things in the check directory
# Found the following files/directories:
#  ''NULL''
# N  checking for detritus in the temp directory
# Found the following files/directories:
#  'lastMiKTeXException'

rhub::check_on_windows(check_args = "--force-multiarch")
rhub::check_on_solaris()
# devtools::check_win_devel() # _win devel
# This uploads to win-builder.r-project.org and isn't recommended for private packages
# "This service is intended for useRs who do not have Windows available for checking and building Windows binary package"

### Check reverse dependencies
# remotes::install_github("r-lib/revdepcheck")
# install.packages('revdepcheck', repos = 'https://r-lib.r-universe.dev')
# usethis::use_git_ignore("revdep/")
# usethis::use_build_ignore("revdep/")

devtools::revdep()
library(revdepcheck)
# In another session
id <- rstudioapi::terminalExecute("Rscript -e 'revdepcheck::revdep_check(num_workers = 4)'")
rstudioapi::terminalKill(id)
# See outputs
revdep_details(revdep = "pkg")
revdep_summary()                 # table of results by package
revdep_report() # in revdep/
# Clean up when on CRAN
revdep_reset()

### Update NEWS
# Bump version manually and add list of changes
usethis::use_news_md() #HJC Added

### Add comments for CRAN
usethis::use_cran_comments(open = rlang::is_interactive())

## Upgrade version number
usethis::use_version(which = c("patch", "minor", "major", "dev")[1])

# Verify you're ready for release, and release
# devtools::release()

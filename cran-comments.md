## Resubmission

- DESCRIPTION references: Added references describing the package's
  methods to the Description field, formatted as `authors (year) <doi:...>`.

- \dontrun{} usage: Replaced `\dontrun{}` with `\donttest{}` for
  examples that take longer than 5 seconds to run; all other examples
  have been unwrapped.

- Console messages: Replaced `print()`/`cat()` calls with
  `message()`/`warning()`/`stop()` across the flagged files, so console
  output can be suppressed by the user.

Thank you for your time and helpful feedback.

## Test environments
- Local: Windows (R 4.6.0)
- R-hub: Ubuntu (R 4.4.0, R-release, R-devel)
- GitHub Actions:
  - ubuntu-latest (R 4.4, release, devel)
  - macos-latest (R 4.4, release, devel)
  - windows-latest (R 4.4, release, devel)

## R CMD check results
There were no ERRORs or WARNINGs.

The package installs and loads cleanly on all tested platforms without errors or warnings.

There were three NOTES:

- The installed package size is ~7.6 MB. Sub-directories of 1Mb or more include data (2.8Mb) and doc (4.5Mb). 
  These files are needed to support examples and vignettes.

- The packages emln and infomapecology are listed in Suggests and are
  available via the Additional_repositories field. These packages are
  required only for optional functionality; core functionality, package
  loading, examples, and tests do not require them.
  All examples run successfully without requiring these suggested packages.

- Future file timestamps unable to verify current time. 
  This is due to OneDrive syncing and does not affect package functionality.

Continuous integration via GitHub Actions confirms checks on multiple platforms and R versions.

## Downstream dependencies
There are no downstream dependencies.

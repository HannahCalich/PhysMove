## Test environments
- Local: Windows (R 4.6.0)
- Ubuntu (R 4.4.0, R-release, R-devel) via R-hub
- GitHub Actions (ubuntu-latest, windows-latest, macos-latest)

## R CMD check results
There were no ERRORs or WARNINGs.

There were two NOTES:

- The installed package size is ~9.6 MB. The size of the 'doc' directory
  is due to multiple vignettes with figures intended to demonstrate
  methodology and usage. The included data are modest in size and required
  for examples and documentation.

- The packages emln and infomapecology are listed in Suggests and are
  available via the Additional_repositories field. These packages are
  required only for optional functionality; core functionality, package
  loading, examples, and tests do not require them. The package installs
  and checks correctly when these suggested packages are not installed.

## Downstream dependencies
There are no downstream dependencies.

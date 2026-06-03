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

- The installed package size is ~9.6 MB. The size of the 'doc' directory 
  is primarily due to vignette figures included to demonstrate methodology 
  and usage.

- The packages emln and infomapecology are listed in Suggests and are
  available via the Additional_repositories field. These packages are
  required only for optional functionality; core functionality, package
  loading, examples, and tests do not require them.
  All examples run successfully without requiring these suggested packages.

- Future file timestamps are due to OneDrive syncing and do not affect package functionality.

Continuous integration via GitHub Actions confirms checks on multiple platforms and R versions.

## Downstream dependencies
There are no downstream dependencies.

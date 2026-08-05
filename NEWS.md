## PhysMove 1.2.5 (August 2026)
- [FIX] Renamed `normalised` argument to `normalise` in `plotDispPDF()` for spelling consistency
- [FIX] Corrected intercept (was 1, now 0) for the 1:1 reference line plotted by `randomise()`
- [DOC] Corrected `randomise()` documentation describing the `lm` parameter's target/predictor roles and coefficient-extraction example
- [DOC] Corrected `turningAngles()` documentation search-window example (18–30 hours, not 18–32)

## PhysMove 1.2.4 (July 2026)
- [RELEASE] Published on CRAN
- [DOC] Added references and DOIs to description
- [FIX] Replaced dontrun with donttest for slow examples
- [FIX] Replaced print and cat calls with message/warning/stop where necessary
- [FIX] Updated how functions handle edge cases appearing at 180/90

## PhysMove 1.2.3 (June 2026)
- [FIX] Reduced package size to comply with CRAN requirements

## PhysMove 1.2.2 (June 2026)
- [RELEASE] First submission to CRAN
- [DOC] Minor updates to package documentation and GitHub actions

## PhysMove 1.2.1 (May 2026)
- [FIX] Removed URL redirection by updating Infomap installation link to final destination
- [DOC] Improved consistency across vignettes, README, and manuscript
- [DOC] Clarified wording and definitions of movement metrics and functions
- [DOC] Corrected formatting issues in dataset documentation and vignettes
- [MAINT] Minor documentation and formatting improvements
- [DOC] Re-ran all devHistory checks in preparation for CRAN submission (doc/devHistory)

## PhysMove 1.2 (May 2026)
- [PERF] Optimized `rms()` function for improved performance
- [UPDATE] Updated `fitDist()` to incorporate the poweRlaw package; results remain consistent with previous implementation

## PhysMove 1.1 (December 2025)
- [UPDATE] Updated PhysMove for compatibility with R version 4.4.0
- [DOC] Removed references to Infomap.exe to avoid confusion for macOS users
- [FIX] Added `sf` to imports (required via ggplot) to resolve CRAN check note

## PhysMove 1.0.1 (2021)
- [DOC] Initial package created

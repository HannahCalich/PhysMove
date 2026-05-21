# PhysMove <img src="vignettes/PhysMoveHexNew.png" align="right" width="130" />

[![R-CMD-check--as-cran](https://github.com/HannahCalich/PhysMove/workflows/R-CMD-check--as-cran/badge.svg)](https://github.com/HannahCalich/PhysMove/actions)
[![R CMD check (Windows, Ubuntu, macOS)](https://github.com/HannahCalich/PhysMove/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/HannahCalich/PhysMove/actions/workflows/R-CMD-check.yaml)

Authors: Hannah Calich, Jorge P. Rodríguez, Víctor M. Eguíluz & Ana Sequeira

Maintained by: Hannah Calich (hannah.calich@gmail.com)

## Overview

PhysMove contains a comprehensive collection of methods for documenting species' movement and space-use patterns from satellite telemetry data. The accompanying vignettes demonstrate how to calculate each of the methods and review all relevant functions and parameters. We demonstrate each function with a simulated telemetry dataset, called `tracks`, which is automatically loaded with PhysMove ([explore tracks dataset](introduction.html#explore-tracks-dataset)). Please see our corresponding manuscript for further details on our methods and interpreting results. 
PhysMove focuses on three major categories of movement data analyses, and each category is accompanied by method-specific functions:

1. **Characterization of movement patterns, including:**

  * Scale of movement: `rms()` 
  * Movement patterns across temporal scales: `calcDisp()` and `plotDispPDF()`
  * Search patterns: `fitDist()`, `compDist()`, and `plotDist()`
  * Influence of correlations on movement decisions: `randomise()` and `plotRandomTracks()`
  * Turning angles: `turningAngles()` and `plotAngles()`
  
2. **Identification of space-use patterns, including:**

  * Occupancy patterns: `occupancy()` and `plotPDF()`
  * Community-wide movements: `infomapCommunities()` and `communityMap()`

3. **Detection of variability in intraspecific movements, including:**

  * Track dispersion: `gyrationRad()` and `plotPDF()`
  * Track entropy: `entropy()` and `plotPDF()`
  * Track predictability: `predictability()` and `plotPDF()`

## Installation 

PhysMove passes all local CRAN checks and has been submitted to CRAN for review. In the meantime, we recommend users install the development version of PhysMove from GitHub using the devtools R package. 

```r
# Install the devtools package from CRAN (if required)
install.packages("devtools")

# Download the development version from GitHub:
devtools::install_github("HannahCalich/PhysMove", build_vignettes = TRUE, force = TRUE)
```

## Data formatting

PhysMove was designed to be user-friendly and most functions only require you to input a data frame containing standard telemetry data. 
The input data frame must only contain these four columns in the following order: *ref*, *lon*, *lat*, and *day*. 

Columns must be formatted as follows:

  * *ref*: the unique telemetry tag ID number for each animal in numeric format (note that characters are not accepted because 
  they can be slower to process than integers, so please convert all reference IDs to integers before proceeding)
  * *lon* and *lat*: the longitude (-180 to + 180) and latitude (-90 to +90) in decimal degrees of
    each position estimate, respectively, in numeric format, and
  * *day*: the datetime stamp for each location estimate in POSIXct
    format following '%Y-%m-%d %H:%M:%S'.

You can compare your data frame to our sample dataset `tracks` to ensure your data are formatted correctly.

## Usage

All of the information you need to apply the PhysMove methods can be found in our accompanying manuscript and vignettes, which are available here:

```r
library(PhysMove)

browseVignettes("PhysMove")
```

Current Version: 1.2.1
Last updated: May 2026

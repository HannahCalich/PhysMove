# PhysMove <img src="vignettes/PhysMoveHex.png" align="right" width="130" />

### Quantify animal movement patterns using statistical physics methods

Authors: Hannah Calich & Ana Sequeira || 
Maintained by: Hannah Calich (hannah.calich@gmail.com)

## Overview 

PhysMove contains a comprehensive collection of methods for documenting species' movement and space-use patterns from satellite telemetry data. Our accompanying vignette demonstrates how to calculate each of the methods and reviews all relevant functions and parameters. We demonstrate each function with a simulated telemetry dataset, called `tracks`, which is automatically loaded with PhysMove ([Explore tracks dataset](vignettes#Exploretracksdataset.html)). For further details on our methods, and for suggestions on how to interpret your results please see our corresponding manuscript. 

PhysMove focuses on three major categories of movement data analyses, and each category is accompanied by method-specific functions:

1. Characterization of movement patterns, including:

  * Scale of movement: `RMS()` 
  * Movement patterns across temporal scales: `CalcDisp()` and `PlotDispPDF()`
  * Search patterns: `FitDisp()`, `CompDist()`, and `PlotDist()`
  * Influence of correlations on movement decisions: `Randomise()` and `PlotRandomTracks()`
  * Turning angles: `TurningAngles()` and `PlotAngles()`
  
2. Identification of space-use patterns, including:

  * Occupancy patterns: `Occupancy()` and `PlotPDF()`
  * Community-wide movements: `InfomapCommunities()` and `CommunityMap()`

3. Detection of variability in intraspecific movements, including:

  * Track dispersion: `GyrationRad()` and `PlotPDF()`
  * Track entropy: `Entropy()` and `PlotPDF()`
  * Track predictability: `Predictability()` and `PlotPDF()`

  * [Track dispersion](intraspecific_movements.html#track-dispersion): `GyrationRad()` and `PlotPDF()`
  * [Track entropy](intraspecific_movements.html#track-entropy): `Entropy()` and `PlotPDF()`
  * [Track predictability](intraspecific_movements.html#track-predictability): `Predictability()` and `PlotPDF()`

## Installation 

PhysMove passes all local CRAN checks (i.e., `devtools::check()` does not result in errors, warnings, or notes), and the package has been submitted to CRAN for review. In the meantime, we recommend users install the development version of PhysMove from GitHub using the devtools R package. Note that the authentication token included below will only be required while the accompanying manuscript is under review, PhysMove will be open access on GitHub once the manuscript is accepted for publication.

```r
# Install the devtools package from CRAN (if required)
install.packages("devtools")

# Download the development version from GitHub:
devtools::install_github("HannahCalich/PhysMove", auth_token = "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", build_vignettes = TRUE, force = TRUE)
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
    format following yyyy-mm-dd hh:mm:ss.

You can compare your dataframe to our sample dataset `tracks` to ensure your data are formatted correctly.

## Usage

All of the information you need to apply the PhysMove methods can be found in our accompanying manuscript and package vignette, available here:

```r
library(PhysMove)

browseVignettes("PhysMove")
```
Current Version: 1.0.0
Last updated: June, 2023

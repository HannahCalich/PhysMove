PhysMove Tutorial
================
Hannah Calich
April 2023

This is a brief tutorial to accompany the PhysMove R package. Here, we
demonstrate how PhysMove can be used to calculate each of the methods
discussed in the main text and review all relevant functions and
parameters. We demonstrate each function with a simulated telemetry
dataset, called `tracks`, which is automatically loaded with PhysMove.
We provide sample code to replicate each of the results presented in the
main text.

## Outine

  - [Install PhysMove and explore tracks
    dataset](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#install-physmove-and-input-data)
      - [Plot tracks with
        `PlotTracks()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#create-a-map-of-the-tracks-dataset-with-plottracks)

**<ins>*Movement patterns*</ins>**

  - [Calculate displacements with
    `CalcDisp()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#calculate-displacements-with-calcdisp)
      - [Create a probability density function (pdf) plot with
        `PlotDispPDF()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#create-a-probability-density-function-pdf-plot-of-normalised-displacements-with-plotdisppdf)
  - [Quantify the scale of movement with
    `RMS()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#scale-of-movement-with-rms)
  - [Describe the influence of correlations on movement decisions with
    `Randomise()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#influence-of-correlations-on-movement-decisions-with-randomise)
  - [Identify turning angle patterns with
    `TurningAngles()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#turning-angles-with-turningangles)
      - [Plot angles with a circle plot with
        `PlotAngles()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#create-a-circle-plot-of-all-turning-angles-calculated-using-turningangles)
  - [Search patterns with `FitDist()`, `PlotDist()`, and
    `CompDist()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#search-patterns-with-fitdist-plotdist-and-compdist)

**<ins>*Space-Use Patterns*</ins>**

  - [Occupancy with
    `Occupancy()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#occupancy-with-occupancy)
  - [Infomap communities with
    `InfomapCommunities()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#community-wide-movements-with-infomapcommunities)

**<ins>*Intraspecific movement patterns*</ins>**

  - [Track gyration radius with
    `GyrationRad()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#dispersion-with-gyrationrad)
  - [Track entropy with
    `Entropy()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#entropy-with-entropy)
  - [Track predictability with
    `Predictability()`](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#predictability-with-predictability)

## *Install PhysMove and input data*

We recommend users install PhysMove through the devtools R package. The
code below will install devtools and PhysMove (including the
authentication token required to access PhysMove until the package is
released to the public) and load the PhysMove package.

``` r
# Install the devtools package from CRAN (if required)
install.packages("devtools")

# Download PhysMove 
devtools::install_github("HannahCalich/PhysMove",auth_token = "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")
```

``` r
# Load PhysMove
library(PhysMove)
```

#### Explore `tracks` dataset

PhysMove was designed to be user-friendly, and most functions only
require you to input a data frame containing telemetry data. The input
data frame must only contain 4 columns named *ref*, *lon*, *lat* and
*day* that are formatted as follows:

  - *ref*, the unique telemetry tag ID number for each animal in numeric
    format,

  - *lon* and *lat*, the longitude and latitude in decimal degrees of
    each position estimate, respectively, in numeric format, and

  - *day*, the datetime stamp for each location estimate in POSIXct
    format following yyyy-mm-dd hh:mm:ss.

To determine if the data frame is formatted correctly, the `tracks`
dataset can be used for comparison. The code below demonstrates how to
preview the `tracks` dataset using `head()` and confirm the dataset
structure using `str()`.

``` r
# Preview the first 6 rows of the tracks dataset
head(tracks)
```

    ##   ref       lon       lat                 day
    ## 1   1 0.5310173 0.5310173 2017-10-13 15:00:00
    ## 2   1 0.5156939 0.5500691 2017-10-14 15:00:00
    ## 3   1 0.5052581 0.5158941 2017-10-15 15:00:00
    ## 4   1 0.5247597 0.4555179 2017-10-16 15:00:00
    ## 5   1 0.5491573 0.2831650 2017-10-17 15:00:00
    ## 6   1 0.5670918 0.2875133 2017-10-18 15:00:00

``` r
# Determine the structure of the tracks dataset
str(tracks)
```

    ## 'data.frame':    15623 obs. of  4 variables:
    ##  $ ref: num  1 1 1 1 1 1 1 1 1 1 ...
    ##  $ lon: num  0.531 0.516 0.505 0.525 0.549 ...
    ##  $ lat: num  0.531 0.55 0.516 0.456 0.283 ...
    ##  $ day: POSIXct, format: "2017-10-13 15:00:00" "2017-10-14 15:00:00" ...

#### Create a map of the tracks dataset with `PlotTracks()`

A map of the data can be created using the `PlotTracks()` function from
PhysMove (Figure S 1). `PlotTracks()` has optional parameters that allow you
to plot specific tracks based on their reference IDs (`ref=NULL`, by
default), connect points with lines (`tracks=TRUE`, by default), and
edit the colours used in the map (`colours=rainbow`, by default). The
code used to make the `tracks` dataset is available in the PhysMove
data-raw folder on GitHub as “CreateTracks.R”.

``` r
PlotTracks(tracks)
```

![](Tutorial_markdown_files/figure-gfm/plot%20tracks-1.png)<!-- -->

**Figure** **S1** Map of the simulated tracking data included in the `tracks`
dataset, created with `PlotTracks()` default settings.

## *Movement patterns*

### Calculate displacements with `CalcDisp()`

The `CalcDisp()` function calculates displacements travelled in
kilometres over set time windows. `CalcDisp()` has four optional
parameters that allow you to change different aspects of the set time
windows, including setting the minimum and maximum times between
location estimates in hours (`min_hr=24` and `max_hr=240`, by default),
the time interval in hours, which creates a sequence of time windows
between the minimum and maximum times over a set time interval
(`interval_hr=24`, by default), and the range (`range_hr=6`, by
default), which allows the code to identify location estimates that are
close to, but not exactly separated by the `interval_hr` input value.
For example, by default, `CalcDisp()` calculates displacements between
location estimates separated by 10 time windows, 24 ± 6 hours, 48 ± 6
hours, etc., until 240 ± 6 hours. `CalcDisp()` outputs a list where each
list element contains the displacements calculated over a time window,
such that the first list element contains data from the first time
window and so on.

``` r
# Calculate displacements from the tracks dataset with default parameters
disp.all <- CalcDisp(tracks)
```

    ## [1] "15598 displacements in 24 +/- 6 hour(s)"
    ## [1] "15573 displacements in 48 +/- 6 hour(s)"
    ## [1] "15548 displacements in 72 +/- 6 hour(s)"
    ## [1] "15523 displacements in 96 +/- 6 hour(s)"
    ## [1] "15498 displacements in 120 +/- 6 hour(s)"
    ## [1] "15473 displacements in 144 +/- 6 hour(s)"
    ## [1] "15448 displacements in 168 +/- 6 hour(s)"
    ## [1] "15423 displacements in 192 +/- 6 hour(s)"
    ## [1] "15398 displacements in 216 +/- 6 hour(s)"
    ## [1] "15373 displacements in 240 +/- 6 hour(s)"

``` r
# Summarise displacements calculated over the first time window (24 ± 6 hours)
summary(unlist(disp.all[[1]]))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2605  2.7049  5.5149  8.2299 11.1227 76.8543

#### Create a probability density function (pdf) plot of normalised displacements with `PlotDispPDF()`

The `PlotDispPDF()` function calcualtes a probability density function
(pdf) of the displacements (Figure S 2 - Figure S 3; Figure 1 in main
text). The normalised parameter allows the data to be normalised before
plotting, which divides all displacements in a time window by the mean
displacement for that time window (`normalised=TRUE`, by default). If
displacements have been calculated over multiple time windows, we
recommend normalising them so you can compare results from different
time windows. We have also included optional parameters that allow
changes to the colours of the points (`colours=rainbow`, by default) and
the ability to add or remove a legend (`legend=TRUE`, by default).
`PlotDispPDF()` outputs all data used to create the plot, including the
pdf values (*pdf*), the displacements (*disp*), and the time windows
(*timeWindow*); note that if `normalised=TRUE`, the output displacements
are normalised values.

``` r
plot.data.disp <- PlotDispPDF(disp.all)
```

![](Tutorial_markdown_files/figure-gfm/plot%20all%20normalised%20displacements-1.png)<!-- -->

**Figure** **S2** Probability density function (pdf) plot of normalised
displacements from the `tracks` dataset calculated over 10 time windows,
24-240 hours at 24 ± 6-hour time intervals with `CalcDisp()`. Plot
created with `PlotDispPDF()` default parameters.

#### Create a probability density function (pdf) plot of displacements without normalizing with `PlotDispPDF()`

``` r
PlotDispPDF(disp.all, normalised=FALSE)
```

![](Tutorial_markdown_files/figure-gfm/plot%20all%20displacements%20\(not%20normalised\)-1.png)<!-- -->

**Figure** **S3** Probability density function (pdf) plot of displacements
from the `tracks` dataset calculated over 10 time windows, 24 to 240 hours
at 24 ± 6-hour time intervals with `CalcDisp()`. Plot created with
`PlotDispPDF()` where `normalised=FALSE`.

## Scale of movement with `RMS()`

The `RMS()` function provides insights into the scale of movement by
calculating the mean and root-mean-square (RMS) displacements and
plotting them over time (Figure S 4). `RMS()` has optional parameters
that allow you to change the time unit used to calculate the time
between locations (`timeUnit= “days”`, by default), the width of the
time bins used to calculate how frequently displacements occurred
(`wBins=1.1`, by default), if a scatterplot is created (`plot=TRUE`, by
default), and if a linear model is fit to the data to examine the
relationship between the root-mean-square displacement values and time
(`lm=TRUE`, by default). When `lm=TRUE`, a linear model object
*RMSlinearModel* is automatically exported to the local environment. The
slope of the linear model is used to make conclusions about the scale of
movement (see Table 3 in the main text for suggestions on interpreting
your results).

Note that because `RMS()` calculates all displacements in each track,
this function may take 10-20+ minutes, depending on your computer;
progress updates will appear when the calculations are 25%, 50%, 75%,
and 100% complete. `RMS()` outputs data in three columns, *timeWindow*,
including the binned time windows in days (or whatever unit was set
using timeUnit) that correspond with the *meanDisplacement* and
*rmsDisplacement* values in kilometres (km).  

``` r
# Calculate RMS values with default parameters
rms.result <- RMS(tracks)
```

    ## 25% complete

    ## 50% complete

    ## 75% complete

    ## Calculations complete

![](Tutorial_markdown_files/figure-gfm/calcualte%20rms-1.png)<!-- -->

**Figure** **S4** Scatter plot of mean (grey points) and root-mean-square
(RMS; black points) displacements (d) in kilometres (km) from `tracks`
dataset over time (T) in days, fit to a linear model (red line with
standard error shaded in grey). Plot created with `RMS()` default
parameters.

``` r
# Summarise RMS results
summary(rms.result)
```

    ##    timeWindow        meanDisplacements rmsDisplacements
    ##  Min.   :   0.9755   Min.   :  8.23    Min.   : 11.41  
    ##  1st Qu.:  16.2477   1st Qu.: 38.37    1st Qu.: 44.46  
    ##  Median :  64.6389   Median : 78.68    Median : 90.40  
    ##  Mean   : 190.3053   Mean   :111.46    Mean   :122.62  
    ##  3rd Qu.: 257.7394   3rd Qu.:154.68    3rd Qu.:174.23  
    ##  Max.   :1025.3728   Max.   :583.63    Max.   :584.91

``` r
# Summarise the linear model results and identify the Hurst exponent 
summary(RMSlinearModel)
```

    ## 
    ## Call:
    ## lm(formula = log(MyRMS$Sqrt_dRMS_per_count) ~ log(MyRMS$TimeWindows_log), 
    ##     data = MyRMS)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -0.13391 -0.01351 -0.00140  0.01524  0.50575 
    ## 
    ## Coefficients:
    ##                            Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                2.417708   0.029750   81.27   <2e-16 ***
    ## log(MyRMS$TimeWindows_log) 0.497345   0.006669   74.57   <2e-16 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 0.08833 on 57 degrees of freedom
    ## Multiple R-squared:  0.9899, Adjusted R-squared:  0.9897 
    ## F-statistic:  5561 on 1 and 57 DF,  p-value: < 2.2e-16

``` r
# Determine the Hurst exponent without displaying the full linear model summary 
RMSlinearModel$coefficients[2]
```

    ## log(MyRMS$TimeWindows_log) 
    ##                  0.4973449

## Influence of correlations on movement decisions with `Randomise()`

The `Randomise()` function can be used to gain insights into how
correlations influenced the movements and space-use of a species.
Optional parameters allow you to change the number of randomised tracks
created (`randTrack=500`, by default) and the grid cell size in degrees
(`gridCell=0.25`, by default). Results from `Randomise()` can be
visualised with a scatter plot (`plot=TRUE`, by default), and a linear
model can be fit to the average number of grid cells visited by the
randomised tracks and the number of grid cells visited by the original
tracks (`lm=TRUE`, by default). The slope of this model is used to make
conclusions about how correlations influence movement (see Table 3 in
the main text for suggestions on how to interpret results).
`Randomise()` outputs data in three columns, *ref* the reference id
numbers for each track, *CellsInOriginalTracks* the number of grid cells
visited by the original tracks, and *AvgCellsInRandomisedTracks* the
average number of grid cells visited by the randomised tracks. The
coordinates for the randomised tracks, *RandomisedLong* and
*RandomisedLat*, are automatically saved to the local environment
because this information is needed for the `PlotRandomTracks()`
function.

``` r
# Setting a seed enables the replication of results because Randomise() involves random number selection
set.seed(1)
# Randomise tracks from the tracks dataset with default parameters 
randomise.result <- Randomise(tracks)
```

![](Tutorial_markdown_files/figure-gfm/randomise%20tracks-1.png)<!-- -->

**Figure** **S5** Scatter plot illustrating the relationship between the
number of grid cells visited by the original tracks from the `tracks` dataset and the average
number of grid cells visited by the randomised tracks. The solid black
line represents the linear model fit to this data, the grey shaded area
reflects the standard error of the fit, and the dashed black line
represents a 1:1 relationship. Plot created with `Randomise()` default
parameters.

``` r
# Summarise RMS results
summary(randomise.result)
```

    ##       ref     CellsInOriginalTracks AvgCellsInRandomisedTracks
    ##  Min.   : 1   Min.   :27.00         Min.   : 25.21            
    ##  1st Qu.: 7   1st Qu.:39.00         1st Qu.: 39.59            
    ##  Median :13   Median :59.00         Median : 55.65            
    ##  Mean   :13   Mean   :57.32         Mean   : 56.48            
    ##  3rd Qu.:19   3rd Qu.:71.00         3rd Qu.: 70.81            
    ##  Max.   :25   Max.   :94.00         Max.   :100.77

``` r
# Determine the slope of the linear model 
summary(RandomiselinearModel)
```

    ## 
    ## Call:
    ## lm(formula = plot.df$AvgCellsInRandomisedTracks ~ plot.df$CellsInOriginalTracks, 
    ##     data = plot.df)
    ## 
    ## Residuals:
    ##      Min       1Q   Median       3Q      Max 
    ## -16.5452  -7.3282  -0.9386   6.6480  26.6015 
    ## 
    ## Coefficients:
    ##                               Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)                     5.7706     6.7382   0.856    0.401    
    ## plot.df$CellsInOriginalTracks   0.8846     0.1113   7.947 4.81e-08 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 10.83 on 23 degrees of freedom
    ## Multiple R-squared:  0.733,  Adjusted R-squared:  0.7214 
    ## F-statistic: 63.15 on 1 and 23 DF,  p-value: 4.808e-08

``` r
# Determine the slope without displaying the full linear model summary 
RandomiselinearModel$coefficients[2]
```

    ## plot.df$CellsInOriginalTracks 
    ##                     0.8846446

The `PlotRandomTracks()` function plots the randomised tracks created
with `Randomise()` (Figure S 6; Figure 1 in main text).
`PlotRandomTracks()` requires you to input a reference id of the track
to be mapped in the ref parameter and will automatically call on the
*RandomisedLat* and *RandomisedLong* objects previously exported from
`Randomise()`. Optional parameters allow you to change the number of
randomised tracks that are plotted (`numPlot=1:5`, by default numPlot
plots the first 5 randomised versions of each track). You can also
change how the map is visualized by: \* changing the colours of the
original and randomised location estimates (`colours=c(“black”,
“grey70”)`, respectively, by default), \* adding or removing lines
connecting the location estimates (`tracks=TRUE`, by default), \*
changing the colours of the starting and ending points of each track
(`startCol=“red”` and `endCol = “blue”`, respectively, by default), and
\* adding a legend (`legend=TRUE`, by default).

`PlotRandomTracks()` outputs the data used to create the map in three
columns, *randTrack*, the id number of the random track, *lon* and
*lat*, the longitude and latitude coordinates of the randomised tracks.

``` r
# Plot random tracks for tracks dataset reference id 1
plot.data.random.tracks <-PlotRandomTracks(tracks, ref=1)
```

``` r
# Plot random tracks for tracks dataset reference id 1
invisible({capture.output({plot.data.random.tracks <-PlotRandomTracks(tracks, ref=1)
})})
```

![](Tutorial_markdown_files/figure-gfm/plot%20random%20tracks-1.png)<!-- -->

**Figure** **S6** Map illustrating the original track for reference id 1
from the `tracks` dataset (black points and line) and the first 5
randomised tracks for track reference id 1 calculated using
`Randomise()` (grey points and lines). The starting and ending locations
are in red and blue, respectively. Plot created with
`PlotRandomTracks()` default parameters and `ref=1`.

## Turning angles with `TurningAngles()`

The `TurningAngles()` function calculates turning angles between a set
of three consecutive location estimates separated by set time windows to
describe how species explore their habitats (Figure S 7). Similarly to
`CalcDisp()`, four optional parameters allow you to change to the time
windows, including setting the minimum and maximum times between
location estimates in hours (`min_hr=24` and `max_hr=240`, by default),
the time interval in hours, which creates a sequence of time windows
between the minimum and maximum times over a set time interval
(`interval_hr=24`, by default), and the range `(range_hr=6`, by
default), which allows the code to identify location estimates that are
close to, but not exactly separated by the `interval_hr` input value.
For example, `TurningAngles()` calculates turning angles between sets of
three location estimates where the time window between each pair of
location estimates is defined using the optional time window parameters.
The `histPlot` parameter determines if a histogram is output (Figure S
7) and controls if “all” time windows are plotted or if only the first,
or second, or third etc. time window is plotted (`histPlot=c(TRUE,
“all”)`, by default). Results are output in a list where each list
element contains the angles calculated over a time window, such that the
first list element contains data from the first time window and so on.
See Table 3 in the main text for suggestions on how to interpret
results.

``` r
# Calculate turning angles in the tracks dataset using default parameters
angle.results <- TurningAngles(tracks)
```

    ## [1] "15573 angles in 24 +/- 6 hour(s)"
    ## [1] "15523 angles in 48 +/- 6 hour(s)"
    ## [1] "15473 angles in 72 +/- 6 hour(s)"
    ## [1] "15423 angles in 96 +/- 6 hour(s)"
    ## [1] "15373 angles in 120 +/- 6 hour(s)"
    ## [1] "15323 angles in 144 +/- 6 hour(s)"
    ## [1] "15273 angles in 168 +/- 6 hour(s)"
    ## [1] "15223 angles in 192 +/- 6 hour(s)"
    ## [1] "15173 angles in 216 +/- 6 hour(s)"
    ## [1] "15123 angles in 240 +/- 6 hour(s)"

![](Tutorial_markdown_files/figure-gfm/calcualte%20turning%20angles-1.png)<!-- -->

**Figure** **S7** Histogram of turning angles recorded from the `tracks` dataset
during ten time windows (24 to 240 hours over 24 ± 6 hour intervals).
Plot created with `TurningAngles()` default parameters.

``` r
# Summarise turning angles calculated over the first time window (24 ± 6 hours)
summary(angle.results[[1]])
```

    ##       Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
    ## -179.97980  -93.41046    0.79527   -0.01537   92.75782  179.99861

Results from `TurningAngles()` can be visualised with the `PlotAngles()`
function, which creates a circle plot (also known as a spider or radar
plot) showing the frequency of turning angles over each time window
(Figure S 8; Figure 1 in main text). Optional parameters allow you to:
\* Control if “all” time windows or only specific windows are plotted
(`timePlot=“all”`, by default), \* Change line colours
(`colours=rainbow`, by default), and \* Determine if a legend is
included (`legend=TRUE`, by default).

`PlotAngles()` outputs all data used to create the circle plot,
including the time windows (*timeWindows*), angle frequency
(*frequency*), and corresponding angles (*angle*).

#### Create a circle plot of all turning angles calculated using `TurningAngles()`

``` r
# Plot angles with a circle plot
plot.data.angles <- PlotAngles(angle.results)
```

![](Tutorial_markdown_files/figure-gfm/plot%20angles%20with%20a%20circle%20plot-1.png)<!-- -->

**Figure** **S8** Circle plot of turning angles recorded from the `tracks`
dataset during ten time windows (24 to 240 hours over 24 ± 6 hour
intervals). Plot created with `PlotAngles()` default parameters.

## Search patterns with `FitDist()`, `PlotDist()`, and `CompDist()`

After displacements are calculated you can identify the best-fit
distribution of displacements, which can provide insights into the
search pattern(s) a species may use to locate resources. Determining the
best-fit distribution for the displacements involves three functions
(Figure 1 in main text): 1. `FitDist()` 2. `PlotDist()`, and 3.
`CompDist()`

`FitDist()` fits cdfs of continuous power-law, exponential, and
lognormal distributions over the full range of displacements (i.e., full
distributions) or to displacements truncated by an x\_min (i.e.,
truncated distributions). `PlotDist()` uses the results from `FitDist()`
to plot ccdfs of the displacements with fit lines for each distribution.
Lastly, `CompDist()` compares distribution fits from `FitDist()` and
identifies the best-fit distribution for the displacements. See Figure S
9 for a methodological overview.

`FitDist()` requires you to consider four optional parameters, reviewed
here: 1. What distributions (`dist`) should be fit to the displacement
data? By default, `FitDist()` fits continuous power-law (“pl”), exponential
(“exp”), and lognormal (“lnorm”) distributions
(`dist=c("pl","exp","lnorm")`). 2. Should each distribution be fit over
the full range of displacements data (`full=TRUE`), or to the
displacements truncated by a x\_min (`full=FALSE`, by default)? 3. If
the distributions are fit to a truncated dataset (i.e., `full=FALSE`),
should the algorithm automatically identify the best-fit x\_min for each
distribution (`set_xmin=NULL`, by default), or do you want to manually
assign the x\_min value? 4. Should the displacements be normalised
before fitting distributions (`normalise=TRUE`, by default)? Note that
displacements should be normalised if they were calculated over multiple
temporal periods.

Outputs from `FitDist()` include the *distribution* (pl, exp, or lnorm),
*xmin* value, *parameter 1*, the first parameter for each distribution
(i.e., α, λ, or μ for pl, exp, and lnorm, respectively), *parameter 2*,
the second distribution parameter (i.e., σ, only applicable to lnorm),
and *nTail*, the number of values greater than or equal to x\_min.

Results from `FitDist()` can be plotted with the `PlotDist()` function
(Figure S 11; Figure 1 in main text). Within `PlotDist()`, optional
parameters allow you to add fit lines for each distribution
(`fitLines=TRUE`, by default), plot only specific distributions
(`setDist=NULL`, by default), change the colours of the fit lines
(`colours=c("red","gold2","blue")`, by default), and add a legend
(`legend=TRUE`, by default).

Lastly, `CompDist()` is used to compare distributions fits and identify
the best-fit distribution(s) for the displacements. Note that
`CompDist()` can only be used when all distributions are fit to the same
range of data (e.g., when `full=TRUE` or if `set_xmin≠NULL`), see Figure
S 9. By default, `CompDist()` compares distribution fits using weighted
AICc scores (AIC scores corrected for small sample sizes) when the
sample size of the displacements used to fit the model (i.e., *nTail*)
divided by the number of parameters in the model is less than or equal
to 40; else, weighed AIC scores are calculated, following Burnham and
Anderson (2004). However, `CompDist()` can be forced to calculate an
AICc using `force_AICc=TRUE` (default is FALSE). The highest wAIC or
wAICc score from each comparison indicates the best-fit distribution.
See Table 3 in the main text for suggestions on how to interpret
results.

In the examples below we begin by calculating displacements over 24 ± 6
hours (to reduce processing times), identifying the best-fit
distribution for the full range of displacements; then, demonstrating
how to identify the best-fit distribution when displacement datasets are
truncated by a best-fit x\_min. In general, we recommend fitting
distributions to full and truncated datasets to gain a comprehensive
understanding of displacement patterns (see Figure S 9).

![image](https://user-images.githubusercontent.com/73092681/223385586-b06b3dd6-d86a-4eeb-ae26-54484e6a4c28.png)
**Figure** **S9** Diagram outlining the procedure for identifying the
best-fit distribution of displacements.

We begin by calculating displacements over 24 ± 6 hours with
`CalcDisp()` and plotting a pdf of the displacements with
`PlotDispPDF()` (Figure S 10; Figure 1 in main text).

``` r
# Calculate displacements over 24 ± 6 hours
disp <- CalcDisp(tracks, max_hr=24)
```

    ## [1] "15598 displacements in 24 +/- 6 hour(s)"

``` r
# Summarise displacements
summary(unlist(disp))
```

    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##  0.2605  2.7049  5.5149  8.2299 11.1227 76.8543

``` r
# Plot displacements (as displacements were only calculated over one time window they do not need to be normalised)
plot.data.pdf <- PlotDispPDF(disp, normalised=FALSE)
```

![](Tutorial_markdown_files/figure-gfm/plot%2024%20hour%20displacements-1.png)<!-- -->

**Figure** **S10** Probability density function (pdf) plot of displacements
calculated using `CalcDisp()` with max\_hr=24. Plot created with
`PlotDispPDF()` and normalised=FALSE.

`FitDist()` is then used to fit the full range of distributions
calculated over 24 ± 6 hours to power-law, exponential, and lognormal
distributions (Figure S 11) and the fits are compared with `CompDist()`.
 

``` r
# Fit all distributions to the full range of displacement data 
distResults <- FitDist(disp, full=TRUE, normalise=FALSE) 
```

    ## Fitting a power law distribution

    ## Fitting an exponential distribution

    ## Fitting a log-normal distribution

``` r
distResults
```

    ##   distribution      xmin parameter1 parameter2 nTail
    ## 1           pl 0.2605242  1.3341746         NA 15598
    ## 2          exp 0.2605242  0.1254743         NA 15598
    ## 3        lnorm 0.2605242  1.6405463   1.044668 15598

``` r
# Create a ccdf plot of displacements with fit lines illustrating distributions fit to the full range of displacements
plot.data.all.pdf <- PlotDist(disp, distResults)
```

![](Tutorial_markdown_files/figure-gfm/plot%20full%20distributions-1.png)<!-- -->

**Figure** **S11** Complementary cumulative distribution function (ccdf) of
displacements (calculated using `CalcDisp()` with `max_hr=24`). Plot
includes fit lines for power-law (pl), exponential (exp), and lognormal
(lnorm) distributions based on results from `FitDist()` with
`full=TRUE`. Plot created using `PlotDist()` default parameters.

``` r
# Identify the best-fit distribution for the full range of displacement data
compResults <- CompDist(disp, distResults)
compResults
```

    ##   distribution      xmin parameter1 parameter2 nTail       AIC          wAIC
    ## 1           pl 0.2605242  1.3341746         NA 15598 116783.60  0.000000e+00
    ## 2          exp 0.2605242  0.1254743         NA 15598  95948.77  1.000000e+00
    ## 3        lnorm 0.2605242  1.6405463   1.044668 15598  96664.80 3.274395e-156

In comparison, the following example demonstrates the procedure for
fitting truncated distributions to the same displacements calculated
over the 24 ± 6 hour time window. `FitDist()` is used to identify the
best-fit d\_min for each distribution (Figure S 12).

``` r
# Fit all distributions and identify the best-fit dmin for each distribution
distResults.trunc <- FitDist(disp, full=FALSE, normalise=FALSE)
```

    ## Fitting a power law distribution

    ## Fitting an exponential distribution

    ## Fitting a log-normal distribution

``` r
distResults.trunc
```

    ##   distribution      xmin parameter1 parameter2 nTail
    ## 1           pl 20.874018  4.8875856         NA  1364
    ## 2          exp  5.459062  0.1221752         NA  7841
    ## 3        lnorm  1.973229  1.9043304  0.8276543 12622

``` r
# Create a ccdf plot of displacements with fit lines illustrating distributions fit to the best-fit xmin for each distribution 
plot.data.all.trunc <- PlotDist(disp, distResults.trunc)
```

![](Tutorial_markdown_files/figure-gfm/plot%20trunc%20distributions-1.png)<!-- -->
**Figure** **S12** Complementary cumulative distribution function (ccdf) of
displacements calculated using `CalcDisp()` with `max_hr=24` including
fit lines for power-law (pl), exponential (exp), and lognormal (lnorm)
distributions based on the best-fit x\_min results from `FitDist()`
(i.e., with `full=FALSE`). Plot created using `PlotDist()` default
parameters.

Note that these results cannot be put straight into `CompDist()` because
each distribution was fit to a different range of data (i.e., *nTail*
values range from 1,364 to 12,622). Instead, we must make pairwise
comparisons where `FitDist()` is re-run three time (once for each
distribution) and the `set_xmin` parameter is set to each of the
best-fit x\_min values in turn. Once distributions have been fit to the
same range of displacements, the fits of the distributions can be compared with
`CompDist()`.

``` r
# Fit all distributions using the xmin value for the power-law distribution
distResultsPl <- FitDist(disp, set_xmin=distResults.trunc$xmin[1], normalise=FALSE) 
distResultsPl
```

    ##   distribution     xmin parameter1 parameter2 nTail
    ## 1           pl 20.87402  4.8875856         NA  1364
    ## 2          exp 20.87402  0.1457233         NA  1364
    ## 3        lnorm 20.87402  2.4000922  0.5294741  1364

``` r
# Fit all distributions using the xmin value for the exponential distribution
distResultsExp <- FitDist(disp, set_xmin=distResults.trunc$xmin[2], normalise=FALSE) 
distResultsExp
```

    ##   distribution     xmin parameter1 parameter2 nTail
    ## 1           pl 5.459062  2.2866946         NA  7841
    ## 2          exp 5.459062  0.1221751         NA  7841
    ## 3        lnorm 5.459062  2.2038304  0.6861026  7841

``` r
# Fit all distributions using the xmin value for the lognormal distribution
distResultsLnorm <- FitDist(disp, set_xmin=distResults.trunc$xmin[3], normalise=FALSE)
distResultsLnorm
```

    ##   distribution     xmin parameter1 parameter2 nTail
    ## 1           pl 1.973229  1.7444386         NA 12622
    ## 2          exp 1.973229  0.1262535         NA 12622
    ## 3        lnorm 1.973229  1.9043317  0.8276533 12622

``` r
# Compare distribution fits based on the best-fit xmin value for the power-law distribution
compResultsPl <- CompDist(disp, distResultsPl)
compResultsPl
```

    ##   distribution     xmin parameter1 parameter2 nTail      AIC         wAIC
    ## 1           pl 20.87402  4.8875856         NA  1364 8016.717 1.164441e-08
    ## 2          exp 20.87402  0.1457233         NA  1364 7980.295 9.440862e-01
    ## 3        lnorm 20.87402  2.4000922  0.5294741  1364 7985.948 5.591380e-02

``` r
# Compare distribution fits based on the best-fit xmin value for the exponential distribution
compResultsExp <- CompDist(disp, distResultsExp)
compResultsExp
```

    ##   distribution     xmin parameter1 parameter2 nTail      AIC         wAIC
    ## 1           pl 5.459062  2.2866946         NA  7841 50535.45 0.000000e+00
    ## 2          exp 5.459062  0.1221751         NA  7841 48650.60 1.000000e+00
    ## 3        lnorm 5.459062  2.2038304  0.6861026  7841 48765.99 8.813929e-26

``` r
# Compare distribution fits based on the best-fit xmin value for the lognormal distribution 
compResultsLnorm <- CompDist(disp, distResultsLnorm)
compResultsLnorm
```

    ##   distribution     xmin parameter1 parameter2 nTail      AIC         wAIC
    ## 1           pl 1.973229  1.7444386         NA 12622 83763.88 0.000000e+00
    ## 2          exp 1.973229  0.1262535         NA 12622 77486.07 1.000000e+00
    ## 3        lnorm 1.973229  1.9043317  0.8276533 12622 77620.33 7.017338e-30

Once all distributions are fit using each of the best-fit x\_min values,
the distribution fits can be compared using `CompDist()`. An important
consideration for interpreting the `CompDist()` results from pairwise
comparisons is that if an x\_min was set to favour a specific
distribution, but the wAIC/ wAICc scores do not identify that
distribution as the best fit; the distribution corresponding to the
x\_min value is not the best-fit distribution for the displacements. For
example, in the first pairwise compassion the x\_min was set to the
best-fit x\_min for a power-law (20.87); however, the wAIC scores
identified an exponential distribution as the best fit. Therefore, we
conclude that a power-law is not the best-fit distribution for the data.
As both full and truncated distribution analyses identified an
exponential distribution as the best fit, we can report that these displacements
are best-fit to an exponential distribution compared with 
power-law and lognormal distributions. 

## *Space-use patterns*

### Occupancy with Occupancy()

The `Occupancy()` function helps describe species’ space-use patterns by
calculating the total number of location estimates within each grid cell
and dividing this sum by the grid cell’s area, calculated using
spherical coordinates (Figure S 13). Optional parameters allow you to:
\* Change the grid cell size in degrees (`gridCell=0.25`, by default),
\* Present results in a map (`map=TRUE`, by default, Figure 1 in main
text), and \* Edit the colours used in the map to indicate low,
moderate, and high occupancy, respectively, which are visualised using
scale\_fill\_gradientn() from the ggplot2 package (Wickham 2016)
(`colGrad=c(“blue”, “light blue”, “red”)`, by default).

`Occupancy()` outputs all data used to create the map, including the
*Latitude* and *Longitude* for the centre of each grid cell, the *Area*
of the grid cell, the number of location estimates recorded in the grid
cell (*Counts*), and the *Occupancy* per grid cell. See Table 3 in the
main text for suggestions on how to interpret results.

``` r
# Create an occupancy map based on the tracks dataset
invisible({capture.output({Occ <- Occupancy(tracks)
```

![](Tutorial_markdown_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

**Figure** **S13** Map of occupancy (total count of location estimates in
each grid cell per area) based on the `tracks` dataset. Map created with
`Occupancy()` default parameters.

``` r
# Summarise occupancy patterns
summary(Occ$Occupancy)
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## 0.001294 0.010357 0.025906 0.040936 0.054041 0.288579

A pdf of the results from `Occupancy()` can be plotted with the
`pdfPlot()` function when the desc parameter is set to “Occupancy”
(Figure S 14; Figure 1 in main text).

``` r
# Create a pdf plot of occupancy values
pdf.occ  <- pdfPlot(Occ$Occupancy, desc="Occupancy") 
```

![](Tutorial_markdown_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

**Figure** **S14** Probability density function (pdf) plot of occupancy per
km2 for tracks dataset determined with `Occupancy()` default parameters.
Plot created using `pdfPlot()` with desc set to “Occupancy”.

### Community-wide movements with `InfomapCommunities()`

To identify Infomap communities, PhysMove requires the [`infomapecology`
R
package](https://github.com/Ecological-Complexity-Lab/infomap_ecology_package)
and a stand-alone [Infomap
file](https://ecological-complexity-lab.github.io/infomap_ecology_package/installation)
that must be downloaded separately following:
<https://ecological-complexity-lab.github.io/infomap_ecology_package/installation>
(Farage et al. 2021). The following instructions assume both the
`infomapecology` R package and the stand-alone Infomap file have been
installed.

The `InfomapCommunities()` function identifies community-wide movements
in two steps. 1. `InfomapCommunities()` calculates the probability of
individuals moving between specific grid cells along their track within
a predetermined time window. `InfomapCommunities()` saves these results
as a transition probability matrix (which is also known as a “unipartite
edge list”); this matrix can be saved to the local environment as
*TransitionProbabilityMatrix* if `tpm,=TRUE` (`tpm=FALSE`, by default).
Optional parameters allow you to change: \* The grid cell size in
degrees (`gridCell=0.25`, by default) \* The number of hours between
location estimates (`hours=24`, by default), and \* Time range in hours
that will allow the algorithm to identify location estimates that are
close to, but not exactly separated by the set number of hour
(`range_hr=6`, by default). 2. If `infomap=TRUE`, `InfomapCommunities()`
feeds the transition probability matrix into the `infomapecology` package
to create an *Infomap monolayer object* that identifies movement
communities (`Infomap=TRUE`, by default). To ensure the algorithm
calculates movement patterns consistent with telemetry data, we adapted
the infomapecology functions to allow for directed movement, self-links
(i.e., individuals can remain in the same grid cell over time), and
hierarchical partitioning (i.e., the resulting communities are composed
of multiple levels). Because we allowed hierarchical partitioning, the
resulting communities are associated with different levels; level 1
communities are the most inclusive and have been used to identify
community-wide movements (following Rodríguez et al. 2017 and Calich et
al. 2021). See Farage et al. (2021) for tips on interpreting the *Infomap
monolayer object* and Table 3 in the main text for suggestions on how to
interpret results.

\*\* Before running `InfomapCommunities()` make sure your working
direction is set to the folder that contains the “Infomap.exe” file\*\*

``` r
# Identify community-wide movements from the tracks dataset with InfomapCommunities, 
# Note that the working directory must be set to the folder containing the Infomap file, 
# for more information, please visit:
# https://ecological-complexity-lab.github.io/infomap_ecology_package/installation
setwd("~/2023/PhysMove") ### Update here
infomap <- InfomapCommunities(tracks)
```

    ## Warning: package 'attempt' was built under R version 4.2.3

    ## Warning: package 'ggalluvial' was built under R version 4.2.3

    ## Warning: package 'rlang' was built under R version 4.2.3

    ## Warning: package 'igraph' was built under R version 4.2.3

    ## Warning: package 'vegan' was built under R version 4.2.3

    ## Warning: package 'permute' was built under R version 4.2.3

    ## [1] "Input: an unipartite edge list"

``` r
# Determine the structure of the Infomap monolayer object
str(infomap)
```

    ## List of 8
    ##  $ call     : chr "./infomap infomap.txt . --tree --seed 123 -N 100 -f directed --silent"
    ##  $ L        : num 2.3
    ##  $ m        : num 4
    ##  $ modules  : tibble [495 × 14] (S3: tbl_df/tbl/data.frame)
    ##   ..$ node_id      : num [1:495] 1 2 3 4 5 6 7 8 9 10 ...
    ##   ..$ node_name    : chr [1:495] "Node1" "Node2" "Node3" "Node4" ...
    ##   ..$ flow         : num [1:495] 0.000439 0.00222 0.00276 0.000209 0.001588 ...
    ##   ..$ levels       : num [1:495] 3 3 3 3 3 3 3 3 3 3 ...
    ##   ..$ module_level1: num [1:495] 3 3 3 3 3 3 3 3 3 3 ...
    ##   ..$ module_level2: num [1:495] 7 7 7 5 5 5 7 5 7 5 ...
    ##   ..$ module_level3: num [1:495] 3 3 1 5 5 3 1 1 2 1 ...
    ##   ..$ module_level4: num [1:495] 2 1 1 2 1 1 2 1 1 3 ...
    ##   ..$ module_level5: num [1:495] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ module_level6: num [1:495] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ module_level7: num [1:495] NA NA NA NA NA NA NA NA NA NA ...
    ##   ..$ cell         : num [1:495] 496083 497523 498963 497524 497525 ...
    ##   ..$ long         : num [1:495] 0.625 0.625 0.625 0.875 1.125 ...
    ##   ..$ lat          : num [1:495] -3.88 -3.62 -3.38 -3.62 -3.62 ...
    ##  $ edge_list:'data.frame':   2298 obs. of  3 variables:
    ##   ..$ from  : chr [1:2298] "Node2" "Node1" "Node2" "Node12" ...
    ##   ..$ to    : chr [1:2298] "Node1" "Node2" "Node2" "Node2" ...
    ##   ..$ weight: num [1:2298] 0.2 1 0.6 0.125 0.1 ...
    ##  $ L_sim    : NULL
    ##  $ m_sim    : NULL
    ##  $ pvalue   : NULL
    ##  - attr(*, "class")= chr "infomap_monolayer"

The `CommunityMap()` function is used to visualise results from
`InfomapCommunities()` by converting the Infomap monolayer object into a
map (Figure S 15; Figure 1 in main text). The optional
subset\_communities parameter allows you to indicate if you only want to
map specific communities. For example, `subset_communities = c(1,2,3)`
would plot communities 1, 2, and 3, but if subset\_communities is left
blank (default), all level 1 communities will be plotted. The colours
used on the map can be changed with `colours= “Dark2”` (by default).

``` r
# Create a map of the Infomap communities 
CommunityMap(infomap)
```

![](Tutorial_markdown_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

**Figure** **S15** Map illustrating level 1 Infomap communities for the
`tracks` dataset determined using `InfomapCommunities()` default
parameters. Map created with `CommunityMap()` default parameters.

## *Intraspecific movements*

### Dispersion with `GyrationRad()`

The `GyrationRad()` function calculates the dispersion (i.e., the
gyration radius) of each track in a dataset (Figure S16). Optional
parameters allow you to \* Create a map (map=TRUE, by default) \*
Control the colour of the points, indicating average track locations,
and circles, indicating how far each animal dispersed
(`mapCol=c(“Black”, “Red”)`, by default).

`GyrationRad()` outputs the data used to make each map, including *ref*,
the reference id for each track, *avg long* and *avg lat*, the average
longitude and latitude coordinates in degrees for each track, and *rG
(km)\*, the gyration radius in kilometres for each track. See Table 3 in
the main text for suggestions on how to interpret results.

``` r
# Calculate the dispersion of each track in the tracks dataset
GR <- GyrationRad(tracks)
```

![](Tutorial_markdown_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

**Figure** **S16** Map illustrating dispersion patterns for `tracks` dataset
using `GyrationRad()` default parameters. Black points represent the
mean location of each track and red circles represent how far each track
dispersed (i.e., their gyration radius).

``` r
# Preview the first 6 rows of the gyration radius dataset
head(GR)
```

    ##      ref   avg long    avg lat   rG (km)
    ## 1      1 -0.2276723 -1.1400612  92.80884
    ## 413    2  0.5514736 -1.3451059 132.77994
    ## 910    3  1.1250293 -0.3847730 108.45032
    ## 1568   4  1.7190069 -0.1784736 104.12900
    ## 2494   5  0.7674021 -0.1059346  68.01475
    ## 2855   6  2.3357980  0.7186781  94.56956

A pdf of the results from `GyrationRad()` can be plotted with the
`pdfPlot()` function when the desc parameter is set to “GyrationRad”
(Figure S17; Figure 1 in main text).

``` r
# Create a pdf plot of gyration radius values
pdf.gr <- pdfPlot((GR$`rG (km)`), desc="GyrationRad")
```

![](Tutorial_markdown_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

**Figure** **S17** Probability density function (pdf) plot of gyration
radius values for `tracks` dataset determined with `GyrationRad()` default
parameters. Plot created using `pdfPlot()` with desc set to
“GyrationRad”.

### Entropy with `Entropy()`

The `Entropy()` function calculates track randomness by documenting the
fraction of data points from each track within each grid cell (Figure S
18). The resulting entropy scores are then normalised so results can be
compared between individuals. Optional parameters allow you to change:
\* grid cell size in degrees (`gridCell=0.25`, by default) and \* output
a histogram (`histPlot=TRUE`, by default).

`Entropy()` outputs results in four columns, including *ref*, the
reference id for each track, the *normalisedEntropy* scores,
*indivEntropy*, the individual entropy scores before they were
normalised, and the number of *cellsVisited* by each track. See Table 3
in the main text for suggestions on how to interpret results.

``` r
# Calculate track entropy using default parameters
Ent <- Entropy(tracks)
```

![](Tutorial_markdown_files/figure-gfm/unnamed-chunk-12-1.png)<!-- -->
**Figure** **S18** Histogram of normalised entropy scores for `tracks` dataset
created using `Entropy()` default parameters.

``` r
# Preview the first 6 rows of the entropy results
head(Ent)
```

    ##   ref normalisedEntropy indivEntropy cellsVisited
    ## 1   1         0.8744141     3.328600           45
    ## 2   2         0.9255766     3.543701           46
    ## 3   3         0.9033790     3.683562           59
    ## 4   4         0.9050806     3.832208           69
    ## 5   5         0.9144473     3.079215           29
    ## 6   6         0.9121058     3.888015           71

A pdf of the results from `Entropy()` can be plotted with the
`pdfPlot()` function when the desc parameter is set to “Entropy” (Figure
S 19; Figure 1 in main text).

``` r
# Create a pdf plot of the entropy scores
pdf.ent <- pdfPlot(Ent$normalisedEntropy, "Entropy")
```

![](Tutorial_markdown_files/figure-gfm/unnamed-chunk-14-1.png)<!-- -->
**Figure** **S19** Probability density function (pdf) plot of normalised
entropy scores for `tracks` dataset determined with `Entropy()` default
parameters. Plot created using `pdfPlot()` with desc set to “Entropy”.

### Predictability with `Predictability()`

The `Predictability()` function calculates the limit of predictability
for each track based on their individual entropy scores (Figure S 20;
Figure 1). Optional parameters allow you to: \* Alter the starting value
used to find a root value for the limit of predictability equation
(startVal=0.99, by default) and \* Output a histogram (`histPlot=TRUE`,
by default).

`Predictability()` outputs results in two columns, *ref*, the reference
id for each track, and *Predictability*, the predictability scores for
each track. See Table 3 in the main text for suggestions on how to
interpret results.

``` r
# Track predictability using Predictability() default parameters and the output from Entropy() 
Pred <- Predictability(tracks, Ent)
```

![](Tutorial_markdown_files/figure-gfm/unnamed-chunk-15-1.png)<!-- -->
**Figure** **S20** Histogram of predictability scores for `tracks` dataset
determined using `Predictability()` default parameters and entropy
scores from `Entropy()`.

``` r
# Preview the first 6 rows of the predictability results
head(Pred)
```

    ##   ref Predictability
    ## 1   1      0.2761026
    ## 2   2      0.2008385
    ## 3   3      0.2237396
    ## 4   4      0.2152093
    ## 5   5      0.2419867
    ## 6   6      0.2038908

A pdf of the results from `Predictability()` can be plotted with the
`pdfPlot()` function when the desc parameter is set to “Predictability”
(Figure S 21; Figure 1 in main text).

``` r
# Create a pdf plot of the predictability scores
pdf.pred <- pdfPlot(Pred$Predictability, desc="Predictability") 
```

![](Tutorial_markdown_files/figure-gfm/unnamed-chunk-17-1.png)<!-- -->
**Figure** **S21** Probability density function (pdf) plot of predictability
scores for `tracks` dataset determined with `Predictability()` default
parameters and results from `Entropy()`. Plot created using `pdfPlot()`
with desc set to “Predictability”.

## Recommended resources

Méndez, V., *et al*. (2013). Stochastic Foundations in Movement Ecology
: Anomalous Diffusion, Front Propagation and Random Searches. Berlin,
Heidelberg, Germanu, Springer Berlin / Heidelberg.

Viswanathan, G. M., *et al*. (2011). The Physics of Foraging: An
Introduction to Biological Encounters and Random Searches. Cambridge,
Cambridge University Press.

## References

Burnham, K.P. & Anderson, D.R. (2004) Multimodel Inference:Understanding
AIC and BIC in Model Selection. *Sociological Methods & Research*, 33,
261-304.

Calich, H.J. *et al*. (2021) Comprehensive analytical approaches reveal
species-specific search strategies in sympatric apex predatory sharks.
*Ecography*, 44, 1544-1556.

Farage, C. *et al*. (2021) Identifying flow modules in ecological
networks using Infomap. *Methods in Ecology and Evolution*, 12, 778–786.

Rodríguez, J.P. *et al*. (2017) Big data analyses reveal patterns and
drivers of the movements of southern elephant seals. *Scientific
Reports*, 7, 1-10.

Wickham, H. (2016) ggplot2: Elegant Graphics for Data Analysis.
Springer-Verlag, New York.

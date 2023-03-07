# This file is a work in progress, please refer to manuscript SI for current tutorial

# PhysMove Tutorial
This is a brief tutorial to accompany the PhysMove R package. Here, we demonstrate how PhysMove can be used to calculate each of the methods discussed in the main text and review all relevant functions and parameters. 
We demonstrate each function with a simulated telemetry dataset, called `tracks`, that is automatically loaded with PhysMove. We provide sample code to replicate each of the results presented in the main text. 

## Outine
[Install PhysMove and explore tracks dataset](https://github.com/HannahCalich/PhysMove/blob/master/Tutorial.md#install-physmove-and-input-data)
_Movement patterns_
[Calculate displacements with `CalcDisp()`]


## *Install PhysMove and input data*
We recommend users install PhysMove through the devtools R package. The code below will install devtools and PhysMove (including the authentication token required to access PhysMove until the package is released to the public) and load the PhysMove package. 

```R
# Install the devtools package from CRAN (if required)
install.packages("devtools")

# Download PhysMove 
devtools::install_github("HannahCalich/PhysMove",auth_token = "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")

# Load PhysMove
library(PhysMove)
```
#### Explore ```tracks``` dataset
PhysMove was designed to be user-friendly, and most functions only require you to input a data frame containing telemetry data. 
The input data frame must only contain 4 columns named *ref*, *lon*, *lat* and *day* that are formatted as follows: 

  * *ref*, the unique telemetry tag ID number for each animal in numeric format, 

  * *lon* and *lat*, the longitude and latitude in decimal degrees of each position estimate, respectively in numeric format, and 

  * *day*, the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss. 

To determine if the data frame is formatted correctly, the tracks dataset can be used for comparison. 
The code below demonstrates how to preview the tracks dataset using `head()` and confirm the dataset structure using  `str()`.

```R
# Preview the first 6 rows of the tracks dataset
head(tracks)
```
![image](https://user-images.githubusercontent.com/73092681/223329374-d78716fd-f194-4f76-a182-c59240dbd4a8.png)

```R
# Determine the structure of the tracks dataset
str(tracks)
```
![image](https://user-images.githubusercontent.com/73092681/223329468-5bf9f80e-9890-4870-a781-06a3dc384172.png)

#### Create a map of the tracks dataset with `PlotTracks()` 
A map of the data can be created using the `PlotTracks()` function from PhysMove (Figure S 1). 
PlotTracks has optional parameters that allow you to plot specific tracks based on their reference IDs (`ref=NULL`, by default), 
connect points with lines (`tracks=TRUE`, by default), 
and edit the colours used in the map (`colours=rainbow`, by default). 
The code used to make the tracks dataset is available in the PhysMove data-raw folder on GitHub as “CreateTracks.R”.

```R 
PlotTracks(tracks)
```
![image](https://user-images.githubusercontent.com/73092681/223335151-1d62a8fa-c26f-4315-808f-a30b64f8ac38.png)

Figure S 1 Map of the simulated tracking data included in the tracks dataset, created with `PlotTracks()` default settings. 

## *Movement patterns*

### Calculate displacements with `CalcDisp()`
The `CalcDisp()` function calculates displacements travelled in kilometres over set time windows. 
`CalcDisp()` has four optional parameters that allow you to change different aspects of the set time windows, 
including setting the minimum and maximum times between location estimates in hours (`min_hr=24` and `max_hr=240`, by default), 
the time interval in hours, which creates a sequence of time windows between the minimum and maximum times over a set time interval 
(`interval_hr=24`, by default), and the range (`range_hr=6`, by default), which allows the code to identify location estimates that are 
close to, but not exactly separated by the `interval_hr` input value. For example, by default, `CalcDisp()` calculates displacements between 
location estimates separated by 10 time windows, 24 ± 6 hours, 48 ± 6 hours, etc., until 240 ± 6 hours. `CalcDisp()` outputs a list where each 
list element contains the displacements calculated over a time window, such that the first list element contains data from the first time window and so on. 

```R
# Calculate displacements from the tracks dataset with default parameters
disp.all <- CalcDisp(tracks)

# Summarise displacements calculated over the first time window (24 ± 6 hours)
summary(unlist(disp.all[[1]]))
```
![image](https://user-images.githubusercontent.com/73092681/223372948-7abeec1a-5aa0-4b87-bb93-b131c0ad2225.png)


#### Create a probability density function (pdf) plot of normalised displacements with `PlotDispPDF()`
The `PlotDispPDF()` function calcualtes a probability density function (pdf) of the displacements 
(Figure S 2 - Figure S 3; Figure 1 in main text). The normalised parameter allows the data to be normalised before plotting, 
which divides all displacements in a time window by the mean displacement for that time window (`normalised=TRUE`, by default). 
If displacements have been calculated over multiple time windows, we recommend normalising them so you can compare results from 
different time windows. We have also included optional parameters that allow changes to the colours of the points (`colours=rainbow`, 
by default) and the ability to add or remove a legend (`legend=TRUE`, by default). `PlotDispPDF()` outputs all data used to create the plot, 
including the pdf values (*pdf*), the displacements (*disp*), and the time windows (*timeWindow*); note that if `normalised=TRUE`, the 
output displacements are normalised values.

```R
PlotDispPDF(disp.all)
```
![image](https://user-images.githubusercontent.com/73092681/223372999-d319c948-25e1-4200-a885-7d30d02e1017.png) 

Figure S 2 Probability density function (pdf) plot of normalised displacements from the tracks dataset calculated over 10 time windows, 24-240 hours at 24 ± 6-hour time intervals with `CalcDisp()`. Plot created with `PlotDispPDF()` default parameters. 

#### Create a probability density function (pdf) plot of displacements without normalizing with `PlotDispPDF()`
```R
PlotDispPDF(disp.all, normalised=FALSE)
```
![image](https://user-images.githubusercontent.com/73092681/223373230-0585fad2-5c80-4b70-9e9a-4101decdc842.png)

Figure S 3 Probability density function (pdf) plot of displacements from the tracks dataset calculated over 10 time windows, 24 to 240 hours at 24 ± 6-hour time intervals with `CalcDisp()`. Plot created with `PlotDispPDF()` where `normalised=FALSE`. 

## Scale of movement with `RMS()`
The `RMS()` function provides insights into the scale of movement by calculating the mean and root-mean-square (RMS) displacements and plotting them over time (Figure S 4). `RMS()` has optional parameters that allow you to change the time unit used to calculate the time between locations (`timeUnit= “days”`, by default), the width of the time bins used to calculate how frequently displacements occurred (`wBins=1.1`, by default), if a scatterplot is created (`plot=TRUE`, by default), and if a linear model is fit to the data to examine the relationship between the root-mean-square displacement values and time (`lm=TRUE`, by default). When `lm=TRUE`, a linear model object *RMSlinearModel* is automatically exported to the local environment. The slope of the linear model is used to make conclusions about the scale of movement (see Table 3 in the main text for suggestions on interpreting your results). 

Note that because `RMS()` calculates all displacements in each track, this function may take 10-20+ minutes, depending on your computer; progress updates will appear when the calculations are 25%, 50%, 75%, and 100% complete. `RMS()` outputs data in three columns, *timeWindow*, including the binned time windows in days (or whatever unit was set using timeUnit) that correspond with the *meanDisplacement* and *rmsDisplacement* values in kilometres (km).  

```R
# Calculate RMS values with default parameters
rms.result <- RMS(tracks)
```
![image](https://user-images.githubusercontent.com/73092681/223377362-0241c7eb-4fcc-420a-bb22-d8a642310fb0.png)

Figure S 4 Scatter plot of mean (grey points) and root-mean-square (RMS; black points) displacements (d) in kilometres (km) from tracks dataset over time (T) in days, fit to a linear model (red line with standard error shaded in grey). Plot created with `RMS()` default parameters. 

```R
# Summarise RMS results
summary(rms.result)
```
![image](https://user-images.githubusercontent.com/73092681/223377656-a9c547a3-0bb2-4b24-b8af-ca375986ed86.png)

```R
# Summarise the linear model results and identify the Hurst exponent 
summary(RMSlinearModel)
```
![image](https://user-images.githubusercontent.com/73092681/223377697-55b74eb8-42f2-486a-9307-dc8dc0c1b815.png)

```R
# Determine the Hurst exponent without displaying the full linear model summary 
RMSlinearModel$coefficients[2]
``` 
![image](https://user-images.githubusercontent.com/73092681/223377755-1c79c442-46aa-42c2-859a-67376f0e2016.png)


## Influence of correlations on movement decisions with `Randomise()`
The `Randomise()` function can be used to gain insights into how correlations influenced the movements and space-use of a species. Optional parameters allow you to change the number of randomised tracks created (`randTrack=500`, by default) and the grid cell size in degrees (`gridCell=0.25`, by default). Results from `Randomise()` can be visualised with a scatter plot (`plot=TRUE`, by default), and a linear model can be fit to the average number of grid cells visited by the randomised tracks and the number of grid cells visited by the original tracks (`lm=TRUE`, by default). The slope of this model is used to make conclusions about how correlations influence movement (see Table 3 in the main text for suggestions on how to interpret results). `Randomise()` outputs data in three columns, *ref* the reference id numbers for each track, *CellsInOriginalTracks* the number of grid cells visited by the original tracks, and *AvgCellsInRandomisedTracks* the average number of grid cells visited by the randomised tracks. The coordinates for the randomised tracks, *RandomisedLong* and *RandomisedLat*, are automatically saved to the local environment because this information is needed for the `PlotRandomTracks()` function.

```R
# Setting a seed enables the replication of results because Randomise() involves random number selection
set.seed(1) 
# Randomise tracks from the tracks dataset with default parameters 
randomise.result <- Randomise(tracks)
```
![image](https://user-images.githubusercontent.com/73092681/223378574-1c9b5ccb-f804-4d2d-816f-2272bca2ac39.png)

Figure S 5 Scatter plot illustrating the relationship between the number of grid cells visited by the original tracks and the average number of grid cells visited by the randomised tracks. The solid black line represents the linear model fit to this data, the grey shaded area reflects the standard error of the fit, and the dashed black line represents a 1:1 relationship. Plot created with `Randomise()` default parameters. 

```R
# Summarise RMS results
summary(randomise.result)
```
![image](https://user-images.githubusercontent.com/73092681/223378690-a1badbd8-9435-4316-9c39-0941c9f21a90.png)

```R
# Determine the slope of the linear model 
summary(RandomiselinearModel)
```
![image](https://user-images.githubusercontent.com/73092681/223378726-4df6b1b5-6b23-49f5-bba8-11ff21eb98d1.png)


```R
# Determine the slope without displaying the full linear model summary 
RandomiselinearModel$coefficients[2]
```
![image](https://user-images.githubusercontent.com/73092681/223378765-2b1d0efe-07fc-4595-b618-81996cb25518.png)


The `PlotRandomTracks()` function plots the randomised tracks created with `Randomise()` (Figure S 6; Figure 1 in main text). `PlotRandomTracks()` requires you to input a reference id of the track to be mapped in the ref parameter and will automatically call on the *RandomisedLat* and *RandomisedLong* objects previously exported from `Randomise()`. Optional parameters allow you to change the number of randomised tracks that are plotted (`numPlot=1:5`, by default numPlot plots the first 5 randomised versions of each track). 
You can also change how the map is visualized by: 
  * changing the colours of the original and randomised location estimates (`colours=c(“black”, “grey70”)`, respectively, by default), 
  * adding or removing lines connecting the location estimates (`tracks=TRUE`, by default), 
  * changing the colours of the starting and ending points of each track (`startCol=“red”` and `endCol = “blue”`, respectively, by default), and 
  * adding a legend (`legend=TRUE`, by default). 
  
`PlotRandomTracks()` outputs the data used to create the map in three columns, *randTrack*, the id number of the random track, *lon* and *lat*, the longitude and latitude coordinates of the randomised tracks.

```R
# Plot random tracks for tracks dataset reference id 1
PlotRandomTracks(tracks, ref=1)
```
![image](https://user-images.githubusercontent.com/73092681/223382220-279cb160-5493-45d9-907a-a47b30642677.png)

Figure S 6 Map illustrating the original track for reference id 1 from the tracks dataset (black points and line) and the first 5 randomised tracks for track reference id 1 calculated using `Randomise()` (grey points and lines). The starting and ending locations are in red and blue, respectively. Plot created with `PlotRandomTracks()` default parameters and `ref=1`. 

## Turning angles with `TurningAngles()`
The `TurningAngles()` function calculates turning angles between a set of three consecutive location estimates separated by set time windows to describe how species explore their habitats (Figure S 7). Similarly to `CalcDisp()`, four optional parameters allow you to change to the time windows, including setting the minimum and maximum times between location estimates in hours (`min_hr=24` and `max_hr=240`, by default), the time interval in hours, which creates a sequence of time windows between the minimum and maximum times over a set time interval (`interval_hr=24`, by default), and the range `(range_hr=6`, by default), which allows the code to identify location estimates that are close to, but not exactly separated by the `interval_hr` input value. For example, `TurningAngles()` calculates turning angles between sets of three location estimates where the time window between each pair of location estimates is defined using the optional time window parameters. The `histPlot` parameter determines if a histogram is output (Figure S 7) and controls if “all” time windows are plotted or if only the first, or second, or third etc. time window is plotted (`histPlot=c(TRUE, “all”)`, by default). Results are output in a list where each list element contains the angles calculated over a time window, such that the first list element contains data from the first time window and so on. See Table 3 in the main text for suggestions on how to interpret results.

```R
# Calculate turning angles in the tracks dataset using default parameters
angle.results <- TurningAngles(tracks)
```
![image](https://user-images.githubusercontent.com/73092681/223382778-36e3df1c-ecfc-477b-976b-1213440349bb.png)

Figure S 7 Histogram of turning angles recorded from the tracks dataset during ten time windows (24 to 240 hours over 24 ± 6 hour intervals). Plot created with `TurningAngles()` default parameters. 

```R
# Summarise turning angles calculated over the first time window (24 ± 6 hours)
summary(angle.results[[1]])
```
![image](https://user-images.githubusercontent.com/73092681/223382894-abe09776-be65-45c7-90ac-3fecee9cea20.png)

Results from `TurningAngles()` can be visualised with the `PlotAngles()` function, which creates a circle plot (also known as a spider or radar plot) showing the frequency of turning angles over each time window (Figure S 8; Figure 1 in main text). Optional parameters allow you to: 
  * Control if “all” time windows or only specific windows are plotted (`timePlot=“all”`, by default), 
  * Change line colours (`colours=rainbow`, by default), and 
  * Determine if a legend is included (`legend=TRUE`, by default). 
  
`PlotAngles()` outputs all data used to create the circle plot, including the time windows (*timeWindows*), angle frequency (*frequency*), and corresponding angles (*angle*). 

#### Create a circle plot of all turning angles calculated using `TurningAngles()`
```R
PlotAngles(angle.results)
```
![image](https://user-images.githubusercontent.com/73092681/223383570-9581ba93-1d43-4375-89de-8fa6d050fd00.png)

Figure S 8 Circle plot of turning angles recorded from the tracks dataset during ten time windows (24 to 240 hours over 24 ± 6 hour intervals). Plot created with `PlotAngles()` default parameters. 

## Search patterns with `FitDist()`, `PlotDist()`, and `CompDist()`
After displacements are calculated you can identify the best-fit distribution of displacements, which can provide insights into the search pattern(s) a species may use to locate resources. Determining the best-fit distribution for the displacements involves three functions (Figure 1 in main text): 
  1. `FitDist()`
  2. `PlotDist()`, and 
  3. `CompDist()` 
`FitDist()` fits cdfs of continuous power-law, exponential, and lognormal distributions over the full range of displacements (i.e., full distributions) or to displacements truncated by an x_min (i.e., truncated distributions). `PlotDist()` uses the results from `FitDist()` to plot ccdfs of the displacements with fit lines for each distribution. Lastly, `CompDist()` compares distribution fits from `FitDist()` and identifies the best-fit distribution for the displacements. See Figure S 9 for a methodological overview. 

`FitDist()` requires you to consider four optional parameters, reviewed here:
  1. What distributions (`dist`) should be fit to the displacement data? By default, FitDist fits continuous power-law (“pl”), exponential (“exp”), and lognormal (“lnorm”) distributions (`dist=c("pl","exp","lnorm")`). 
  2. Should each distribution be fit over the full range of displacements data (`full=TRUE`), or to the displacements truncated by a x_min (`full=FALSE`, by default)? 
  3. If the distributions are fit to a truncated dataset (i.e., `full=FALSE`), should the algorithm automatically identify the best-fit x_min for each distribution (`set_xmin=NULL`, by default), or do you want to manually assign the x_min value? 
  4. Should the displacements be normalised before fitting distributions (`normalise=TRUE`, by default)? Note that displacements should be normalised if they were calculated over multiple temporal periods. 

Outputs from `FitDist()` include the *distribution* (pl, exp, or lnorm), *xmin* value, *parameter 1*, the first parameter for each distribution (i.e., α, λ, or μ for pl, exp, and lnorm, respectively), *parameter 2*, the second distribution parameter (i.e., σ, only applicable to lnorm), and *nTail*, the number of values greater than or equal to x_min. 

Results from `FitDist()` can be plotted with the `PlotDist()` function (Figure S 11; Figure 1 in main text). Within `PlotDist()`, optional parameters allow you to add fit lines for each distribution (`fitLines=TRUE`, by default), plot only specific distributions (`setDist=NULL`, by default), change the colours of the fit lines (`colours=c("red","gold2","blue")`, by default), and add a legend (`legend=TRUE`, by default). 

Lastly, `CompDist()` is used to compare distributions fits and identify the best-fit distribution(s) for the displacements. Note that `CompDist()` can only be used when all distributions are fit to the same range of data (e.g., when `full=TRUE` or if `set_xmin≠NULL`), see Figure S 9. By default, `CompDist()` compares distribution fits using weighted AICc scores (AIC scores corrected for small sample sizes) when the sample size of the displacements used to fit the model (i.e., *nTail*) divided by the number of parameters in the model is less than or equal to 40; else, weighed AIC scores are calculated, following Burnham and Anderson (2004). However, `CompDist()` can be forced to calculate an AICC using `force_AICc=TRUE` (default is FALSE). The highest WAIC or WAICC score from each comparison indicates the best-fit distribution. See Table 3 in the main text for suggestions on how to interpret results.

In the examples below we begin by calculating displacements over 24 ± 6 hours (to reduce processing times), identifying the best-fit distribution for the full range of displacements; then, demonstrating how to identify the best-fit distribution when displacement datasets are truncated by a best-fit x_min. In general, we recommend fitting distributions to full and truncated datasets to gain a comprehensive understanding of displacement patterns (see Figure S 9). 

![image](https://user-images.githubusercontent.com/73092681/223385586-b06b3dd6-d86a-4eeb-ae26-54484e6a4c28.png)

Figure S 9 Diagram outlining the procedure for identifying the best-fit distribution of displacements. 
We begin by calculating displacements over 24 ± 6 hours with `CalcDisp()` and plotting a pdf of the displacements with `PlotDispPDF()` (Figure S 10; Figure 1 in main text). 

```R
# Calculate displacements over 24 ± 6 hours
disp <- CalcDisp(tracks, max_hr=24)
# Summarise displacements
summary(unlist(disp))
```
![image](https://user-images.githubusercontent.com/73092681/223385728-89721570-b744-48a0-8231-1a647751c479.png)

```R
# Plot displacements (as displacements were only calculated over one time window they do not need to be normalised)
PlotDispPDF(disp, normalised=FALSE)
```
![image](https://user-images.githubusercontent.com/73092681/223385758-585b580b-97b9-4362-a5a6-fec2f6bdb3db.png)

Figure S 10 Probability density function (pdf) plot of displacements calculated using `CalcDisp()` with max_hr=24. Plot created with `PlotDispPDF()` and normalised=FALSE.

`FitDist()` is then used to fit the full range of distributions calculated over 24 ± 6 hours to power-law, exponential, and lognormal distributions (Figure S 11) and the fits are compared with `CompDist()`.  

```R
# Fit all distributions to the full range of displacement data 
distResults <- FitDist(disp, full=TRUE, normalise=FALSE) 
distResults
```
![image](https://user-images.githubusercontent.com/73092681/223387262-8051f6ff-16e6-49c8-aa54-891ff1649647.png)

```R
# Create a ccdf plot of displacements with fit lines illustrating distributions fit to the full 
# range of displacements
PlotDist(disp, distResults)
```
![image](https://user-images.githubusercontent.com/73092681/223387655-73c52245-01d7-4691-b4ed-39dfaa439e85.png)

Figure S 11 Complementary cumulative distribution function (ccdf) of displacements (calculated using `CalcDisp()` with `max_hr=24`). Plot includes fit lines for power-law (pl), exponential (exp), and lognormal (lnorm) distributions based on results from `FitDist()` with `full=TRUE`. Plot created using `PlotDist()` default parameters. 

```R
# Identify the best-fit distribution for the full range of displacement data
compResults <- CompDist(disp, distResults)
compResults
```
![image](https://user-images.githubusercontent.com/73092681/223387796-4ebcd81d-4416-4975-809c-1f2afe00cfba.png)

In comparison, the following example demonstrates the procedure for fitting truncated distributions to the same displacements calculated over the 24 ± 6 hour time window. `FitDist()` is used to identify the best-fit x_min for each distribution (Figure S 12). 

```R
# Fit all distributions and identify the best-fit xmin for each distribution
distResults.trunc <- FitDist(disp, full=FALSE, normalise=FALSE)
distResults.trunc
```
![image](https://user-images.githubusercontent.com/73092681/223387923-9eabe14d-c65c-4fcd-a369-5b0aa3350fba.png)

```R
# Create a ccdf plot of displacements with fit lines illustrating distributions fit to the 
# best-fit xmin for each distribution 
PlotDist(disp, distResults.trunc)
``` 
![image](https://user-images.githubusercontent.com/73092681/223387954-f5b0eab0-4379-4b4f-92f1-68f2ac71cb90.png)

Figure S 12 Complementary cumulative distribution function (ccdf) of displacements calculated using `CalcDisp()` with `max_hr=24` including fit lines for power-law (pl), exponential (exp), and lognormal (lnorm) distributions based on the best-fit x_min results from `FitDist()` (i.e., with `full=FALSE`). Plot created using `PlotDist()` default parameters. 

Note that these results cannot be put straight into `CompDist()` because each distribution was fit to a different range of data (i.e., *nTail* values range from 1,364 to 12,622). Instead, we must reate pairwise comparisons where `FitDist()` is re-run three time (once for each distribution) and the `set_xmin` parameter is set to each of the best-fit x_min values in turn. Once distributions have been fit to the same range of displacements, the distributions fits can be compared with `CompDist()`.

```R
# Fit all distributions using the xmin value for the power-law distribution
distResultsPl <- FitDist(disp, set_xmin=distResults.trunc$xmin[1], normalise=FALSE) 
distResultsPl
```
![image](https://user-images.githubusercontent.com/73092681/223388279-a289219e-86a3-4a5d-9c81-f74dd6e05895.png)

```R
# Fit all distributions using the xmin value for the exponential distribution
distResultsExp <- FitDist(disp, set_xmin=distResults.trunc$xmin[2], normalise=FALSE) 
distResultsExp
```
![image](https://user-images.githubusercontent.com/73092681/223388388-4b0ac776-5070-4719-a4d6-5fc2770ccce1.png)

```R
# Fit all distributions using the xmin value for the lognormal distribution
distResultsLnorm <- FitDist(disp, set_xmin=distResults.trunc$xmin[3], normalise=FALSE)
distResultsLnorm
```
![image](https://user-images.githubusercontent.com/73092681/223388454-1f22a419-5ae1-4e07-8a0e-8004ae285376.png)

```R  
# Compare distribution fits based on the best-fit xmin value for the power-law distribution
compResultsPl <- CompDist(disp, distResultsPl)
compResultsPl
```
![image](https://user-images.githubusercontent.com/73092681/223388553-5d74bbc2-8093-4ad7-a4a4-a57bec436aaf.png)

```R
# Compare distribution fits based on the best-fit xmin value for the exponential distribution
compResultsExp <- CompDist(disp, distResultsExp)
compResultsExp
``` 
![image](https://user-images.githubusercontent.com/73092681/223388649-fbe84659-ecf7-4011-8237-498b9afa46c9.png)

```R
# Compare distribution fits based on the best-fit xmin value for the lognormal distribution 
compResultsLnorm <- CompDist(disp, distResultsLnorm)
compResultsLnorm
```
![image](https://user-images.githubusercontent.com/73092681/223388741-7e56c152-c456-4d36-9c8a-83a9acc0f3cc.png)

Once all distributions are fit using each of the best-fit x_min values, the distribution fits can be compared using CompDist(). An important consideration for interpreting the `CompDist()` results from pairwise comparisons is that if an x_min was set to favour a specific distribution, but the WAIC/ WAICC scores do not identify that distribution as the best fit; the distribution corresponding to the x_min value is not the best-fit distribution for the displacements. For example, in the first pairwise compassion the x_min was set to the best-fit x_min for a power-law (20.87); however, the WAIC scores identified an exponential distribution as the best fit. Therefore, we conclude that a power-law is not the best-fit distribution for the data. As both full and truncated distribution analyses identified an exponential distribution as the best fit, we can report that between power-law, exponential and lognormal distributions, these displacements are best-fit to an exponential distribution.

## *Space-use patterns*
### Occupancy with Occupancy()
The `Occupancy()` function helps describe species’ space-use patterns by calculating the total number of location estimates within each grid cell and dividing this sum by the grid cell’s area, calculated using spherical coordinates (Figure S 13). Optional parameters allow you to:
  * change the grid cell size in degrees (`gridCell=0.25`, by default), 
  * present results in a map (`map=TRUE`, by default, Figure 1 in main text), and 
  * edit the colours used in the map to indicate low, moderate, and high occupancy, respectively, which are visualised using scale_fill_gradientn from the ggplot2 package (Wickham 2016) (`colGrad=c(“blue”, “light blue”, “red”)`, by default). 
  
`Occupancy()` outputs all data used to create the map, including the *Latitude* and *Longitude* for the centre of each grid cell, the *Area* of the grid cell, the number of location estimates recorded in the grid cell (*Counts*), and the *Occupancy* per grid cell. See Table 3 in the main text for suggestions on how to interpret results. 

```R
# Create an occupancy map based on the tracks dataset
Occ <- Occupancy(tracks)
```
![image](https://user-images.githubusercontent.com/73092681/223389282-3e1482d7-7032-4291-8bb0-01fa153c285b.png)

Figure S 13 Map of occupancy (total count of location estimates in each grid cell per area) based on the tracks dataset. Map created with `Occupancy()` default parameters.

```R
# Summarise occupancy patterns
summary(Occ$Occupancy)
```
![image](https://user-images.githubusercontent.com/73092681/223389327-1d3d2126-29b5-4b69-af48-58a640212b7c.png)

A pdf of the results from `Occupancy()` can be plotted with the `pdfPlot()` function when the desc parameter is set to “Occupancy” (Figure S 14; Figure 1 in main text). 
```R
# Create a pdf plot of occupancy values
pdfPlot(Occ$Occupancy, desc="Occupancy") 
```
![image](https://user-images.githubusercontent.com/73092681/223389505-7ed85d05-471b-42df-96ba-99ae20cceb21.png)

Figure S 14 Probability density function (pdf) plot of occupancy per km2 for tracks dataset determined with `Occupancy()` default parameters. Plot created using `pdfPlot()` with desc set to “Occupancy”.

### Community-wide movements with `InfomapCommunities()`
To identify Infomap communities, PhysMove requires the [`infomapecology` R package](https://github.com/Ecological-Complexity-Lab/infomap_ecology_package) and a stand-alone [Infomap file](https://ecological-complexity-lab.github.io/infomap_ecology_package/installation) that must be downloaded separately following: https://ecological-complexity-lab.github.io/infomap_ecology_package/installation (Farage et al. 2021). The following instructions assume both the `infomapecology` R package and the stand-alone Infomap file have been installed. 

The `InfomapCommunities()` function identifies community-wide movements in two steps. 
1. `InfomapCommunities()` calculates the probability of individuals moving between specific grid cells along their track within a predetermined time window. `InfomapCommunities()` saves these results as a transition probability matrix (which is also known as a “unipartite edge list”); this matrix can be saved to the local environment as *TransitionProbabilityMatrix* if `tpm,=TRUE` (`tpm=FALSE`, by default). Optional parameters allow you to change:
  * the grid cell size in degrees (`gridCell=0.25`, by default)
  * the number of hours between location estimates (`hours=24`, by default), and 
  * time range in hours that will allow the algorithm to identify location estimates that are close to, but not exactly separated by the set number of hour (`range_hr=6`, by default). 
2. If `infomap=TRUE`, `InfomapCommunities()` feeds the transition probability matrix into the infomapecology package to create an *Infomap monolayer object* that identifies movement communities (`Infomap=TRUE`, by default). To ensure the algorithm calculates movement patterns consistent with telemetry data, we adapted the infomapecology functions to allow for directed movement, self-links (i.e., individuals can remain in the same grid cell over time), and hierarchical partitioning (i.e., the resulting communities are composed of multiple levels). Because we allowed hierarchical partitioning, the resulting communities are associated with different levels; level 1 communities are the most inclusive and have been used to identify community-wide movements (following Rodríguez et al. 2017 and Calich et al. 2021). See Farage et al. (2021) for tips on interpreting the Infomap monolayer object and Table 3 in the main text for suggestions on how to interpret results.

```R
# Identify community-wide movements from the tracks dataset with InfomapCommunities, 
# Note that the working directory must be set to the folder containing the Infomap file, 
# for more information, please visit:
# https://ecological-complexity-lab.github.io/infomap_ecology_package/installation
infomap <- InfomapCommunities(tracks)
# Determine the structure of the Infomap monolayer object
str(infomap)
```
![image](https://user-images.githubusercontent.com/73092681/223391651-ddd4959d-ec5f-489f-8169-c1c3f5e2c7c3.png)

The `CommunityMap()` function is used to visualise results from `InfomapCommunities()` by converting the Infomap monolayer object into a map (Figure S 15; Figure 1 in main text). The optional subset_communities parameter allows you to indicate if you only want to map specific communities. For example, `subset_communities = c(1,2,3)` would plot communities 1, 2, and 3, but if subset_communities is left blank (default), all level 1 communities will be plotted. The colours used on the map can be changed with `colours= “Dark2”` (by default). 

```R
# Create a map of the Infomap communities 
CommunityMap(infomap)
```
![image](https://user-images.githubusercontent.com/73092681/223391708-063b4082-d4f3-44ac-804e-76f863ff6aaa.png)

Figure S 15 Map illustrating level 1 Infomap communities for the tracks dataset determined using `InfomapCommunities()` default parameters. Map created with `CommunityMap()` default parameters. 

## *Intraspecific movements*
### Dispersion with `GyrationRad()`
The `GyrationRad()` function calculates the dispersion (i.e., the gyration radius) of each track in a dataset (Figure S 16). Optional parameters allow you to
  * create a map (map=TRUE, by default) 
  * control the colour of the points, indicating average track locations, and circles, indicating how far each animal dispersed (`mapCol=c(“Black”, “Red”)`, by default). 
  
`GyrationRad()` outputs the data used to make each map, including *ref*, the reference id for each track, *avg long* and *avg lat, the average longitude and latitude coordinates in degrees for each track, and *rG (km)*, the gyration radius in kilometres for each track. See Table 3 in the main text for suggestions on how to interpret results. 

```R
# Calculate the dispersion of each track in the tracks dataset
GR <- GyrationRad(tracks)
```
![image](https://user-images.githubusercontent.com/73092681/223392432-a0f058a4-6ae0-4911-bba5-fb29557d98d6.png)
 
Figure S 16 Map illustrating dispersion patterns for tracks dataset using `GyrationRad()` default parameters. Black points represent the mean location of each track and red circles represent how far each track dispersed (i.e., their gyration radius). 

```R
# Preview the first 6 rows of the gyration radius dataset
head(GR)
```
![image](https://user-images.githubusercontent.com/73092681/223392466-c15c480d-a883-4a64-9ea1-8fd2ad27cca6.png)
 
A pdf of the results from `GyrationRad()` can be plotted with the `pdfPlot()` function when the desc parameter is set to “GyrationRad” (Figure S 17; Figure 1 in main text). 
```R
# Create a pdf plot of gyration radius values
pdfPlot((GR$`rG (km)`), desc="GyrationRad")
```
![image](https://user-images.githubusercontent.com/73092681/223392674-a36c07e4-ac37-4b0b-b15e-70608b3ea9b6.png)
 
Figure S 17 Probability density function (pdf) plot of gyration radius values for tracks dataset determined with `GyrationRad()` default parameters. Plot created using `pdfPlot()` with desc set to “GyrationRad”.

### Entropy with `Entropy()`
The `Entropy()` function calculates track randomness by documenting the fraction of data points from each track within each grid cell (Figure S 18). The resulting entropy scores are then normalised so results can be compared between individuals. Optional parameters allow you to change:
  * grid cell size in degrees (`gridCell=0.25`, by default) and 
  * output a histogram (`histPlot=TRUE`, by default). 
   
`Entropy()` outputs results in four columns, including *ref*, the reference id for each track, the *normalisedEntropy* scores, *indivEntropy*, the individual entropy scores before they were normalised, and the number of *cellsVisited* by each track. See Table 3 in the main text for suggestions on how to interpret results. 

```R
# Calculate track entropy using default parameters
Ent <- Entropy(tracks)
```
![image](https://user-images.githubusercontent.com/73092681/223393455-e7ed546a-c214-42d8-8006-9e1f868fbc50.png)

Figure S 18 Histogram of normalised entropy scores for tracks dataset created using `Entropy()` default parameters.

```R
# Preview the first 6 rows of the entropy results
head(Ent)
```
![image](https://user-images.githubusercontent.com/73092681/223393365-09581f97-5a86-4716-bea1-7162796448be.png)
 
A pdf of the results from `Entropy()` can be plotted with the `pdfPlot()` function when the desc parameter is set to “Entropy” (Figure S 19; Figure 1 in main text). 

```R
# Create a pdf plot of the entropy scores
pdfPlot(Ent$normalisedEntropy, "Entropy")
```
![image](https://user-images.githubusercontent.com/73092681/223393736-01202f8a-b2c9-4bcb-89d4-39ab77c69f5a.png)

Figure S 19 Probability density function (pdf) plot of normalised entropy scores for tracks dataset determined with `Entropy()` default parameters. Plot created using `pdfPlot()` with desc set to “Entropy”. 

### Predictability with `Predictability()`
The `Predictability()` function calculates the limit of predictability for each track based on their individual entropy scores (Figure S 20; Figure 1). Optional parameters allow you to:
  * alter the starting value used to find a root value for the limit of predictability equation (startVal=0.99, by default) and 
  * output a histogram (`histPlot=TRUE`, by default). 

`Predictability()` outputs results in two columns, *ref*, the reference id for each track, and *Predictability*, the predictability scores for each track. See Table 3 in the main text for suggestions on how to interpret results.

```R
# Track predictability using Predictability() default parameters and the output from Entropy() 
Pred <- Predictability(tracks, Ent) 
```
![image](https://user-images.githubusercontent.com/73092681/223394163-dcbe9d0d-6e6c-4772-b576-df3a2be67e17.png)

Figure S 20 Histogram of predictability scores for tracks dataset determined using `Predictability()` default parameters and entropy scores from `Entropy()`.

```R
# Preview the first 6 rows of the predictability results
head(Pred)
```
![image](https://user-images.githubusercontent.com/73092681/223394222-f673e850-233a-45c8-852e-b76bcb4079d6.png)

A pdf of the results from `Predictability()` can be plotted with the `pdfPlot()` function when the desc parameter is set to “Predictability” (Figure S 21; Figure 1 in main text). 

```R
# Create a pdf plot of the predictability scores
pdfPlot(Pred$Predictability, desc="Predictability") 
```
![image](https://user-images.githubusercontent.com/73092681/223394441-3c79b19a-abbc-492f-9478-01e291674cdd.png)

Figure S 21 Probability density function (pdf) plot of predictability scores for tracks dataset determined with `Predictability()` default parameters and results from `Entropy()`. Plot created using `pdfPlot()` with desc set to “Predictability”. 

## References
Burnham, K.P. & Anderson, D.R. (2004) Multimodel Inference:Understanding AIC and BIC in Model Selection. Sociological Methods & Research, 33, 261-304.

Calich, H.J. et al. (2021) Comprehensive analytical approaches reveal species-specific search strategies in sympatric apex predatory sharks. Ecography, 44, 1544-1556.

Farage, C. et al. (2021) Identifying flow modules in ecological networks using Infomap. Methods in Ecology and Evolution, 12, 778–786.

Rodríguez, J.P. et al. (2017) Big data analyses reveal patterns and drivers of the movements of southern elephant seals. Scientific Reports, 7, 1-10.

Wickham, H. (2016) ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag, New York.

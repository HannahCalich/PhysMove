# PhysMove Tutorial
This is a brief tutorial to accompany the PhysMove R package. Here, we demonstrate how PhysMove can be used to calculate each of the methods discussed in the main text and review all relevant functions and parameters. 
We demonstrate each function with a simulated telemetry dataset, called ```tracks```, that is automatically loaded with PhysMove. We provide sample code to replicate each of the results presented in the main text. 

## 1. Install PhysMove and input data
We recommend users install PhysMove through the devtools R package. The code below will install devtools and PhysMove (including the authentication token required to access PhysMove until the package is released to the public) and load the PhysMove package. 

```R
# Install the devtools package from CRAN (if required)
install.packages("devtools")

# Download PhysMove 
devtools::install_github("HannahCalich/PhysMove",auth_token = "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")

# Load PhysMove
library(PhysMove)
```
### Explore ```tracks``` dataset
PhysMove was designed to be user-friendly, and most functions only require you to input a data frame containing telemetry data. 
The input data frame must only contain 4 columns named “*ref*”, “*lon*”, “*lat*” and “*day*” that are formatted as follows: 

  * “*ref*”, the unique telemetry tag ID number for each animal in numeric format, 

  * “*lon*” and “*lat*”, the longitude and latitude in decimal degrees of each position estimate, respectively in numeric format, and 

  * “*day*”, the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss. 

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

### Create a map of the tracks dataset with ```PlotTracks()``` 
A map of the data can be created using the ```PlotTracks()``` function from PhysMove (Figure S 1). 
PlotTracks has optional parameters that allow you to plot specific tracks based on their reference IDs (```ref=NULL```, by default), 
connect points with lines (```tracks=TRUE```, by default), 
and edit the colours used in the map (```colours=rainbow```, by default). 
The code used to make the tracks dataset is available in the PhysMove data-raw folder on GitHub as “CreateTracks.R”.

```R 
PlotTracks(tracks)
```
![image](https://user-images.githubusercontent.com/73092681/223335151-1d62a8fa-c26f-4315-808f-a30b64f8ac38.png)

Figure S 1 Map of the simulated tracking data included in the tracks dataset, created with ```PlotTracks()``` default settings. 

## 2. Movement patterns

### Calculating displacements with ```CalcDisp()```
The ```CalcDisp()``` function calculates displacements travelled in kilometres over set time windows. 
```CalcDisp()``` has four optional parameters that allow you to change different aspects of the set time windows, 
including setting the minimum and maximum times between location estimates in hours (```min_hr=24``` and ```max_hr=240```, by default), 
the time interval in hours, which creates a sequence of time windows between the minimum and maximum times over a set time interval 
(```interval_hr=24```, by default), and the range (```range_hr=6```, by default), which allows the code to identify location estimates that are 
close to, but not exactly separated by the ```interval_hr``` input value. For example, by default, ```CalcDisp()``` calculates displacements between 
location estimates separated by 10 time windows, 24 ± 6 hours, 48 ± 6 hours, etc., until 240 ± 6 hours. ```CalcDisp()``` outputs a list where each 
list element contains the displacements calculated over a time window, such that the first list element contains data from the first time window and so on. 

```R
# Calculate displacements from the tracks dataset with default parameters
disp.all <- CalcDisp(tracks)

# Summarise displacements calculated over the first time window (24 ± 6 hours)
summary(unlist(disp.all[[1]]))
```

### Create a pdf plot of normalised displacements with ```PlotDispPDF()```
A probability density function (pdf) of the resulting displacements can be plotted using the ```PlotDispPDF()``` function 
(Figure S 2 - Figure S 3; Figure 1 in main text). The normalised parameter allows the data to be normalised before plotting, 
which divides all displacements in a time window by the mean displacement for that time window (```normalised=TRUE```, by default). 
If displacements have been calculated over multiple time windows, we recommend normalising them so you can compare results from 
different time windows. We have also included optional parameters that allow changes to the colours of the points (colours=rainbow, 
by default) and the ability to add or remove a legend (```legend=TRUE```, by default). ```PlotDispPDF()``` outputs all data used to create the plot, 
including the pdf values (“*pdf*”), the displacements (“*disp*”), and the time windows (“*timeWindow*”); note that if ```normalised=TRUE```, the 
output displacements are normalised values.
PlotDispPDF(disp.all)
 
Figure S 2 Probability density function (pdf) plot of normalised displacements from the tracks dataset calculated over 10 time windows, 24 to 240 hours at 24 ± 6-hour time intervals with CalcDisp(). Plot created with PlotDispPDF() default parameters. 

# Create a pdf of displacements (not normalised) 
PlotDispPDF(disp.all, normalised=FALSE)
 
Figure S 3 Probability density function (pdf) plot of displacements from the tracks dataset calculated over 10 time windows, 24 to 240 hours at 24 ± 6-hour time intervals with CalcDisp(). Plot created with PlotDispPDF() where normalised=FALSE. 
Scale of movement with RMS() 
The RMS() function provides insights into the scale of movement by calculating the mean and root-mean-square (RMS) displacements and plotting them over time (Figure S 4). RMS() has optional parameters that allow you to change the time unit used to calculate the time between locations (timeUnit= “days”, by default), the width of the time bins used to calculate how frequently displacements occurred (wBins=1.1, by default), if a scatterplot is created (plot=TRUE, by default), and if a linear model is fit to the data to examine the relationship between the root-mean-square displacement values and time (lm=TRUE, by default). When lm=TRUE, a linear model object “RMSlinearModel” is automatically exported to the local environment. The slope of the linear model is used to make conclusions about the scale of movement (see Table 3 in the main text for suggestions on interpreting your results). Because RMS() calculates all displacements in each track, this function may take 10-20+ minutes, depending on your computer; progress updates will appear when the calculations are 25%, 50%, 75%, and 100% complete. RMS() outputs data in three columns, “timeWindow”, including the binned time windows in days (or whatever unit was set using timeUnit) that correspond with the “meanDisplacement” and “rmsDisplacement” values in kilometres (km).  
# Calculate RMS values with default parameters
rms.result <- RMS(tracks)
 
Figure S 4 Scatter plot of mean (grey points) and root-mean-square (RMS; black points) displacements (d) in kilometres (km) from tracks dataset over time (T) in days, fit to a linear model (red line with standard error shaded in grey). Plot created with RMS() default parameters. 

# Summarise RMS results
summary(rms.result)
  
# Summarise the linear model results and identify the Hurst exponent 
summary(RMSlinearModel)
 
# Determine the Hurst exponent without displaying the full linear model summary 
RMSlinearModel$coefficients[2]
 

Influence of correlations on movement decisions with Randomise()
The Randomise() function can be used to gain insights into how correlations influenced the movements and space-use of a species. Optional parameters allow you to change the number of randomised tracks created (randTrack=500, by default) and the grid cell size in degrees (gridCell=0.25, by default). Results from Randomise() can be visualised with a scatter plot (plot=TRUE, by default), and a linear model can be fit to the average number of grid cells visited by the randomised tracks and the number of grid cells visited by the original tracks (lm=TRUE, by default). The slope of this model is used to make conclusions about how correlations influence movement (see Table 3 in the main text for suggestions on how to interpret results). Randomise() outputs data in three columns, “ref” the reference id numbers for each track, “CellsInOriginalTracks” the number of grid cells visited by the original tracks, and “AvgCellsInRandomisedTracks” the average number of grid cells visited by the randomised tracks. The coordinates for the randomised tracks, “RandomisedLong” and “RandomisedLat”, are automatically saved to the local environment because this information is needed for the PlotRandomTracks() functions.

# Setting a seed enables the replication of results because Randomise() 
# involves random number selection
set.seed(1) 
# Randomise tracks from the tracks dataset with default parameters 
randomise.result <- Randomise(tracks)
 
Figure S 5 Scatter plot illustrating the relationship between the number of grid cells visited by the original tracks and the average number of grid cells visited by the randomised tracks. The solid black line represents the linear model fit to this data, the grey shaded area reflects the standard error of the fit, and the dashed black line represents a 1:1 relationship. Plot created with Randomise() default parameters. 

# Summarise RMS results
summary(randomise.result)
  
# Determine the slope of the linear model 
summary(RandomiselinearModel)
 
# Determine the slope without displaying the full linear model summary 
RandomiselinearModel$coefficients[2]
 

The PlotRandomTracks() function plots the randomised tracks created with Randomise() (Figure S 6; Figure 1 in main text). PlotRandomTracks() requires you to input a reference id of the track to be mapped in the ref parameter and will automatically call on the “RandomisedLat” and “RandomisedLong” objects previously exported from Randomise(). Optional parameters allow you to change the number of randomised tracks that are plotted (numPlot=1:5, by default numPlot plots the first 5 randomised versions of each track). You can also change how the map is visualized by: changing the colours of the original and randomised location estimates (colours=c(“black”, “grey70”), respectively, by default), adding or removing lines connecting the location estimates (tracks=TRUE, by default), changing the colours of the starting and ending points of each track (startCol=“red” and endCol = “blue”, respectively, by default), and adding a legend (legend=TRUE, by default). PlotRandomTracks() outputs the data used to create the map in three columns, “randTrack”, the id number of the random track, “lon” and “lat”, the longitude and latitude coordinates of the randomised tracks.

# Plot random tracks for tracks dataset reference id 1
PlotRandomTracks(tracks, ref=1)
 
Figure S 6 Map illustrating the original track for reference id 1 from the tracks dataset (black points and line) and the first 5 randomised tracks for track reference id 1 calculated using Randomise() (grey points and lines). The starting and ending locations are in red and blue, respectively. Plot created with PlotRandomTracks() default parameters and ref=1. 

Turning angles with TurningAngles()
The TurningAngles() function calculates turning angles between a set of three consecutive location estimates separated by set time windows to describe how species explore their habitats (Figure S 7). Similarly to CalcDisp(), four optional parameters allow you to change to the time windows, including setting the minimum and maximum times between location estimates in hours (min_hr=24 and max_hr=240, by default), the time interval in hours, which creates a sequence of time windows between the minimum and maximum times over a set time interval (interval_hr=24, by default), and the range (range_hr=6, by default), which allows the code to identify location estimates that are close to, but not exactly separated by the interval_hr input value. For example, TurningAngles() calculates turning angles between sets of three location estimates where the time window between each pair of location estimates is defined using the optional time window parameters. The histPlot parameter determines if a histogram is output (Figure S 7) and controls if “all” time windows are plotted or if only the first, or second, or third etc. time window is plotted (histPlot=c(TRUE, “all”), by default). Results are output in a list where each list element contains the angles calculated over a time window, such that the first list element contains data from the first time window and so on. See Table 3 in the main text for suggestions on how to interpret results.

# Calculate turning angles in the tracks dataset using default parameters
angle.results <- TurningAngles(tracks)
 
Figure S 7 Histogram of turning angles recorded from the tracks dataset during ten time windows (24 to 240 hours over 24 ± 6 hour intervals). Plot created with TurningAngles() default parameters. 

# Summarise turning angles calculated over the first time window (24 ± 6 hours)
summary(angle.results[[1]])
 
Results from TurningAngles() can be visualised with the PlotAngles() function, which creates a circle plot (also known as a spider or radar plot) showing the frequency of turning angles over each time window (Figure S 8; Figure 1 in main text). Optional parameters allow you to: control if “all” time windows or only specific windows are plotted (timePlot=“all”, by default), change line colours (colours=rainbow, by default), and determine if a legend is included (legend=TRUE, by default). PlotAngles() outputs all data used to create the circle plot, including the time windows (“timeWindows”), angle frequency (“frequency”), and corresponding angles (“angle”). 

# Create a circle plot of all turning angles calculated using TurningAngles()
PlotAngles(angle.results)
 
Figure S 8 Circle plot of turning angles recorded from the tracks dataset during ten time windows (24 to 240 hours over 24 ± 6 hour intervals). Plot created with PlotAngles() default parameters. 

Search patterns with FitDist(), PlotDist(), and CompDist()
After displacements are calculated you can identify the best-fit distribution of displacements, which can provide insights into the search pattern(s) a species may use to locate resources. Determining the best-fit distribution for the displacements involves three functions: FitDist(), PlotDist(), and CompDist() (Figure 1 in main text), the functions are briefly defined here then explained in detail below. FitDist() fits cdfs of continuous power-law, exponential, and lognormal distributions over the full range of displacements (i.e., full distributions) or to displacements truncated by an x_min (i.e., truncated distributions). PlotDist() uses the results from FitDist() to plot ccdfs of the displacements with fit lines for each distribution. Lastly, CompDist() compares distribution fits from FitDist() and identifies the best-fit distribution for the displacements. See Figure S 9 for a methodological overview. 
FitDist() requires you to consider four optional parameters, reviewed here:
	What distributions (dist) should be fit to the displacement data? By default, FitDist fits continuous power-law (“pl”), exponential (“exp”), and lognormal (“lnorm”) distributions (dist=c("pl","exp","lnorm")). 
	Should each distribution be fit over the full range of displacements data (full=TRUE), or to the displacements truncated by a x_min (full=FALSE, by default)? 
	If the distributions are fit to a truncated dataset (i.e., full=FALSE), should the algorithm automatically identify the best-fit x_min for each distribution (set_xmin=NULL, by default), or do you want to manually assign the x_min value? 
	Should the displacements be normalised before fitting distributions (normalise=TRUE, by default)? Note that displacements should be normalised if they were calculated over multiple temporal periods. 
Outputs from FitDist() include the “distribution” (pl, exp, or lnorm), “xmin” value, “parameter 1”, the first parameter for each distribution (i.e., α, λ, or μ for pl, exp, and lnorm, respectively), “parameter 2”, the second distribution parameter (i.e., σ, only applicable to lnorm), and “nTail”, the number of values greater than or equal to x_min. 

Results from FitDist() can be plotted with the PlotDist() function (Figure S 11; Figure 1 in main text). Within PlotDist(), optional parameters allow you to add fit lines for each distribution (fitLines=TRUE, by default), plot only specific distributions (setDist=NULL, by default), change the colours of the fit lines (colours=c("red","gold2","blue"), by default), and add a legend (legend=TRUE, by default). 

Lastly, CompDist() is used to compare distributions fits and identify the best-fit distribution(s) for the displacements. Note that CompDist() can only be used when all distributions are fit to the same range of data (e.g., when full=TRUE or if set_xmin≠NULL), see Figure S 9. By default, CompDist() compares distribution fits using weighted AICc scores (AIC scores corrected for small sample sizes) when the sample size of the displacements used to fit the model (i.e., nTail) divided by the number of parameters in the model is less than or equal to 40; else, weighed AIC scores are calculated, following Burnham and Anderson (2004). However, CompDist() can be forced to calculate an AICC using force_AICc=TRUE (default is FALSE). The highest WAIC or WAICC score from each comparison indicates the best-fit distribution. See Table 3 in the main text for suggestions on how to interpret results.

In the examples below we begin by calculating displacements over 24 ± 6 hours (to reduce processing times), identifying the best-fit distribution for the full range of displacements; then, demonstrating how to identify the best-fit distribution when displacement datasets are truncated by a best-fit x_min. In general, we recommend fitting distributions to full and truncated datasets to gain a comprehensive understanding of displacement patterns (see Figure S 9). 
 
Figure S 9 Diagram outlining the procedure for identifying the best-fit distribution of displacements. 
We begin by calculating displacements over 24 ± 6 hours with CalcDisp() and plotting a pdf of the displacements with PlotDispPDF() (Figure S 10; Figure 1 in main text). 

# Calculate displacements over 24 + / - 6 hours
disp <- CalcDisp(tracks, max_hr=24)
# Summarise displacements
summary(unlist(disp))
 
# Plot displacements (as displacements were only calculated over one time window they 
# do not need to be normalised)
PlotDispPDF(disp, normalised=FALSE)
 
Figure S 10 Probability density function (pdf) plot of displacements calculated using CalcDisp() with max_hr=24. Plot created with PlotDispPDF() and normalised=FALSE.

FitDist() is then used to fit the full range of distributions calculated over 24 ± 6 hours to power-law, exponential, and lognormal distributions (Figure S 11) and the fits are compared with CompDist().  
# Fit all distributions to the full range of displacement data 
distResults <- FitDist(disp, full=TRUE, normalise=FALSE) 
distResults
 
# Create a ccdf plot of displacements with fit lines illustrating distributions fit to the full 
# range of displacements
PlotDist(disp, distResults)
 
Figure S 11 Complementary cumulative distribution function (ccdf) of displacements (calculated using CalcDisp() with max_hr=24). Plot includes fit lines for power-law (pl), exponential (exp), and lognormal (lnorm) distributions based on results from FitDist() with full=TRUE. Plot created using PlotDist() default parameters. 

# Identify the best-fit distribution for the full range of displacement data
compResults <- CompDist(disp, distResults)
compResults
  
In comparison, the following example demonstrates the procedure for fitting truncated distributions to the same displacements calculated over the 24 ± 6 hour time window. FitDist() is used to identify the best-fit x_min for each distribution (Figure S 12). 

# Fit all distributions and identify the best-fit xmin for each distribution
distResults.trunc <- FitDist(disp, full=FALSE, normalise=FALSE)
distResults.trunc
 
# Create a ccdf plot of displacements with fit lines illustrating distributions fit to the 
# best-fit xmin for each distribution 
PlotDist(disp, distResults.trunc)
 
Figure S 12 Complementary cumulative distribution function (ccdf) of displacements calculated using CalcDisp() with max_hr=24 including fit lines for power-law (pl), exponential (exp), and lognormal (lnorm) distributions based on the best-fit x_min results from FitDist() (i.e., with full=FALSE). Plot created using PlotDist() default parameters. 
These results cannot be put straight into CompDist() because each distribution was fit to a different range of data (i.e., nTail values range from 1,364 to 12,622). Instead, we create pairwise comparisons where FitDist() is re-run three time (once for each distribution) and the set_xmin parameter is set to each of the best-fit x_min values in turn. Once distributions have been fit to the same range of displacements, the distributions fits can be compared with CompDist().

# Fit all distributions using the xmin value for the power-law distribution
distResultsPl <- FitDist(disp, set_xmin=distResults.trunc$xmin[1], normalise=FALSE) 
distResultsPl
 

# Fit all distributions using the xmin value for the exponential distribution
distResultsExp <- FitDist(disp, set_xmin=distResults.trunc$xmin[2], normalise=FALSE) 
distResultsExp
 

# Fit all distributions using the xmin value for the lognormal distribution
distResultsLnorm <- FitDist(disp, set_xmin=distResults.trunc$xmin[3], normalise=FALSE)
distResultsLnorm
  
# Compare distribution fits based on the best-fit xmin value for the power-law distribution
compResultsPl <- CompDist(disp, distResultsPl)
compResultsPl
 

# Compare distribution fits based on the best-fit xmin value for the exponential distribution
compResultsExp <- CompDist(disp, distResultsExp)
compResultsExp
 

# Compare distribution fits based on the best-fit xmin value for the lognormal distribution 
compResultsLnorm <- CompDist(disp, distResultsLnorm)
compResultsLnorm
 

Once all distributions are fit using each of the best-fit x_min values, the distribution fits can be compared using CompDist(). An important consideration for interpreting the CompDist() results from pairwise comparisons is that if an x_min was set to favour a specific distribution, but the WAIC/ WAICC scores do not identify that distribution as the best fit; the distribution corresponding to the x_min value is not the best-fit distribution for the displacements. For example, in the first pairwise compassion the x_minwas set to the best-fit x_min for a power-law (20.87); however, the WAIC scores identified an exponential distribution as the best fit. Therefore, we conclude that a power-law is not the best-fit distribution for the data. As both full and truncated distribution analyses identified an exponential distribution as the best fit, we can report that between power-law, exponential and lognormal distributions, these displacements are best-fit to an exponential distribution.

Space-use patterns
	Occupancy with Occupancy()
The Occupancy() function helps describe species’ space-use patterns by calculating the total number of location estimates within each grid cell and dividing this sum by the grid cell’s area, calculated using spherical coordinates (Figure S 13). Optional parameters allow you to change the grid cell size in degrees (gridCell=0.25, by default), present results in a map (map=TRUE, by default, Figure 1 in main text), and edit the colours used in the map to indicate low, moderate, and high occupancy, respectively, which are visualised using scale_fill_gradientn from the ggplot2 package (Wickham 2016) (colGrad=c(“blue”, “light blue”, “red”), by default). Occupancy() outputs all data used to create the map, including the “Latitude” and “Longitude” for the centre of each grid cell, the “Area” of the grid cell, the number of location estimates recorded in the grid cell (“Counts”), and the “Occupancy” per grid cell. See Table 3 in the main text for suggestions on how to interpret results. 
# Create an occupancy map based on the tracks dataset
Occ <- Occupancy(tracks)
 
Figure S 13 Map of occupancy (total count of location estimates in each grid cell per area) based on the tracks dataset. Map created with Occupancy() default parameters.

# Summarise occupancy patterns
summary(Occ$Occupancy)
 

A pdf of the results from Occupancy() can be plotted with the pdfPlot() function when the desc parameter is set to “Occupancy” (Figure S 14; Figure 1 in main text). 
# Create a pdf plot of occupancy values
pdfPlot(Occ$Occupancy, desc="Occupancy") 
 
Figure S 14 Probability density function (pdf) plot of occupancy per km2 for tracks dataset determined with Occupancy() default parameters. Plot created using pdfPlot() with desc set to “Occupancy”.

Community-wide movements with InfomapCommunities()
To identify Infomap communities, PhysMove requires the infomapecology R package (available at: https://github.com/Ecological-Complexity-Lab/infomap_ecology_package) and a stand-alone Infomap file that must be downloaded separately following: https://ecological-complexity-lab.github.io/infomap_ecology_package/installation (Farage et al. 2021). The following instructions assume both the infomapecology R package and the stand-alone Infomap file have been installed. 

The InfomapCommunities() function identifies community-wide movements in two steps. First, it calculates the probability of individuals moving between specific grid cells along their track within a predetermined time window. InfomapCommunities() saves these results as a transition probability matrix (which is also known as a “unipartite edge list”); this matrix can be saved to the local environment as “TransitionProbabilityMatrix” if tpm,=TRUE (tpm=FALSE, by default). Optional parameters allow you to change the grid cell size in degrees (gridCell=0.25, by default), as well the number of hours between location estimates (hours=24, by default), and the time range in hours that will allow the algorithm to identify location estimates that are close to, but not exactly separated by the set number of hour (range_hr=6, by default). Second, if infomap=TRUE, InfomapCommunities() feeds the transition probability matrix into the infomapecology package to create an “Infomap monolayer object” that identifies movement communities (Infomap=TRUE, by default). To ensure the algorithm calculates movement patterns consistent with telemetry data, we adapted the infomapecology functions to allow for directed movement, self-links (i.e., individuals can remain in the same grid cell over time), and hierarchical partitioning (i.e., the resulting communities are composed of multiple levels). Because we allowed hierarchical partitioning, the resulting communities are associated with different levels; level 1 communities are the most inclusive and have been used to identify community-wide movements (following Rodríguez et al. 2017; Calich et al. 2021). See Farage et al. (2021) for tips on interpreting the Infomap monolayer object and Table 3 in the main text for suggestions on how to interpret results.

The CommunityMap() function is used to visualise results from InfomapCommunities() by converting the Infomap monolayer object into a map (Figure S 15; Figure 1 in main text). The optional subset_communities parameter allows you to indicate if you only want to map specific communities. For example, subset_communities = c(1,2,3) would plot communities 1, 2, and 3, but if subset_communities is left blank (by default), all level 1 communities will be plotted. The colours used on the map can be changed with colours (colours= “Dark2”, by default).  
# Identify community-wide movements from the tracks dataset with InfomapCommunities, 
# Note that the working directory must be set to the folder containing the Infomap file, 
# for more information, please visit:
# https://ecological-complexity-lab.github.io/infomap_ecology_package/installation
infomap <- InfomapCommunities(tracks)
# Determine the structure of the Infomap monolayer object
str(infomap)
  
# Create a map of the Infomap communities 
CommunityMap(infomap)
 

Figure S 15 Map illustrating level 1 Infomap communities for the tracks dataset determined using InfomapCommunities() default parameters. Map created with CommunityMap() default parameters. 

Intraspecific movements
	Dispersion with GyrationRad()
The GyrationRad() function calculates the dispersion (i.e., the gyration radius) of each track in a dataset (Figure S 16). Optional parameters allow you to create a map (map=TRUE, by default) and control the colour of the points, indicating average track locations, and circles, indicating how far each animal dispersed (mapCol=c(“Black”, “Red”), by default). GyrationRad() outputs the data used to make each map, including “ref”, the reference id for each track, “avg long” and “avg lat”, the average longitude and latitude coordinates in degrees for each track, and “rG (km)”, the gyration radius in kilometres for each track. See Table 3 in the main text for suggestions on how to interpret results. 
# Calculate the dispersion of each track in the tracks dataset
GR <- GyrationRad(tracks)

 
Figure S 16 Map illustrating dispersion patterns for tracks dataset using GyrationRad() default parameters. Black points represent the mean location of each track and red circles represent how far each track dispersed (i.e., their gyration radius). 

# Preview the first 6 rows of the gyration radius dataset
head(GR)
 

A pdf of the results from GyrationRad () can be plotted with the pdfPlot() function when the desc parameter is set to “GyrationRad” (Figure S 17; Figure 1 in main text). 
# Create a pdf plot of gyration radius values
pdfPlot((GR$`rG (km)`), desc="GyrationRad")

 
Figure S 17 Probability density function (pdf) plot of gyration radius values for tracks dataset determined with GyrationRad() default parameters. Plot created using pdfPlot() with desc set to “GyrationRad”.

Entropy with Entropy()
The Entropy() function calculates track randomness by documenting the fraction of data points from each track within each grid cell (Figure S 18). The resulting entropy scores are then normalised so results can be compared between individuals. Optional parameters allow changes to the grid cell size in degrees (gridCell=0.25, by default) and the ability to output a histogram of the results (histPlot=TRUE, by default). Entropy() outputs results in four columns, including “ref”, the reference id for each track, the “normalisedEntropy” scores, “indivEntropy”, the individual entropy scores before they were normalised, and the number of “cellsVisited” by each track. See Table 3 in the main text for suggestions on how to interpret results. 
# Calculate track entropy using default parameters
Ent <- Entropy(tracks)
# Preview the first 6 rows of the entropy results
head(Ent)
 

 
Figure S 18 Histogram of normalised entropy scores for tracks dataset created using Entropy() default parameters. 

A pdf of the results from Entropy() can be plotted with the pdfPlot() function when the desc parameter is set to “Entropy” (Figure S 19; Figure 1 in main text). 
# Create a pdf plot of the entropy scores
pdfPlot(Ent$normalisedEntropy, "Entropy")
 
Figure S 19 Probability density function (pdf) plot of normalised entropy scores for tracks dataset determined with Entropy() default parameters. Plot created using pdfPlot() with desc set to “Entropy”. 

Predictability with Predictability()
The Predictability() function calculates the limit of predictability for each track based on their individual entropy scores (Figure S 20; Figure 1). Optional parameters allow you to alter the starting value used to find a root value for the limit of predictability equation (startVal=0.99, by default) and output a histogram (histPlot=TRUE, by default). Predictability() outputs results in two columns, “ref”, the reference id for each track, and “Predictability”, the predictability scores for each track. See Table 3 in the main text for suggestions on how to interpret results.

# Track predictability using Predictability() default parameters and the output from Entropy() 
Pred <- Predictability(tracks, Ent) 
# Preview the first 6 rows of the predictability results
head(Pred)
 

  
Figure S 20 Histogram of predictability scores for tracks dataset determined using Predictability() default parameters and entropy scores from Entropy().

A pdf of the results from Predictability() can be plotted with the pdfPlot() function when the desc parameter is set to “Predictability” (Figure S 21; Figure 1 in main text). 
# Create a pdf plot of the predictability scores
pdfPlot(Pred$Predictability, desc="Predictability") 
 
Figure S 21 Probability density function (pdf) plot of predictability scores for tracks dataset determined with Predictability() default parameters and results from Entropy(). Plot created using pdfPlot() with desc set to “Predictability”. 
References
Burnham, K.P. & Anderson, D.R. (2004) Multimodel Inference:Understanding AIC and BIC in Model Selection. Sociological Methods & Research, 33, 261-304.
Calich, H.J. et al. (2021) Comprehensive analytical approaches reveal species-specific search strategies in sympatric apex predatory sharks. Ecography, 44, 1544-1556.
Farage, C. et al. (2021) Identifying flow modules in ecological networks using Infomap. Methods in Ecology and Evolution, 12, 778–786.
Rodríguez, J.P. et al. (2017) Big data analyses reveal patterns and drivers of the movements of southern elephant seals. Scientific Reports, 7, 1-10.
Wickham, H. (2016) ggplot2: Elegant Graphics for Data Analysis. Springer-Verlag, New York.

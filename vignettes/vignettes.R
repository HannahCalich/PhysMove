## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(dpi=300) # set all figures to 300 dpi to avoid pixelated images
knitr::opts_chunk$set(fig.align='center', fig.width = 7, fig.height = 5)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ---- echo=FALSE--------------------------------------------------------------
#htmltools::img(src = knitr::image_uri("../vignettes/PhysMoveHex.PNG"), 
htmltools::img(src = knitr::image_uri("../vignettes/PhysMoveHex.PNG"), 
              #alt = 'logo',  
              style = 'position:absolute; top:0; right:25; padding:10px; border:0',
              width = '250px', height = '250px',
              dpi=300)

## ----install physmove, echo=TRUE, message=FALSE, highlight=TRUE, clipboard=TRUE----
# Download PhysMove 
# Note that this authentication token will only be required while the manuscript is under review, PhysMove will be open access once the
# manuscript is accepted for publication
devtools::install_github("HannahCalich/PhysMove", auth_token = "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")

## ----load physmove, echo=TRUE, highlight=TRUE, clipboard=TRUE-----------------
# Load PhysMove
library(PhysMove)

## ----preview tracks, echo=TRUE------------------------------------------------
# Preview the first 6 rows of the tracks dataset
head(tracks)

## ----tracks structure, echo=TRUE----------------------------------------------
# Determine the structure of the tracks dataset
str(tracks)

## ----plot tracks, echo=TRUE, out.width="60%"----------------------------------
PlotTracks(tracks)

## ----calcualte rms, echo=TRUE, message=FALSE, out.width="60%"-----------------
# Calculate RMS values with default parameters
rms.result <- RMS(tracks)

## ----summarise rms results, echo=TRUE-----------------------------------------
# Summarise RMS results
summary(rms.result[["rmsResults"]])

## ----calculate hurst exponent, echo=TRUE--------------------------------------
# Summarise linear model results and identify the Hurst exponent 
RMSlinearModel <- rms.result[["lm"]]
print(RMSlinearModel)
# Determine the Hurst exponent 
RMSlinearModel$estimate[2]

## ----calc disp, echo=TRUE-----------------------------------------------------
# Calculate displacements from the tracks dataset with default parameters
disp.all <- CalcDisp(tracks)

## ----sum disp from 1st time window, echo=TRUE---------------------------------
# Summarise displacements calculated over the first time window (24 ± 6 hours)
summary(unlist(disp.all[[1]]))

## ----plot all disp (not norm), echo=TRUE, error=FALSE, message=FALSE, out.width="60%"----
# Create a probability density function (pdf) plot of raw (i.e., not normalized) displacements
plot.data.norm <- PlotDispPDF(disp.all, normalised=FALSE)

## ----plot all norm disp, echo=TRUE, error=FALSE, message=FALSE, fig.width = 7, fig.height = 5, out.width="60%"----
# Create a probability density function (pdf) plot of normalized displacements
plot.data <- PlotDispPDF(disp.all, normalised=TRUE)

## ----calc disp over 24 hours, echo=TRUE---------------------------------------
# Calculate displacements over 24 ± 6 hours
disp <- CalcDisp(tracks, max_hr=24)
# Summarise displacements
summary(unlist(disp))

## ----plot 24 hr disp, echo=TRUE, error=FALSE, message=FALSE, out.width="60%"----
# Plot displacements (as displacements were only calculated over one time window they do not need to be normalised)
plot.data.pdf <- PlotDispPDF(disp, normalised=FALSE)

## ----fit full dist, echo=TRUE-------------------------------------------------
# Fit all distributions to the full range of displacement data 
distResults <- FitDist(disp, full=TRUE, normalise=FALSE) 
distResults[["distResults"]]

## ----plot full dist, echo=TRUE, error=FALSE, message=FALSE, fig.width = 7, fig.height = 5, out.width="60%"----
# Create a ccdf plot of displacements with fit lines illustrating distributions fit to the full range of displacements
plot.data.all.pdf <- PlotDist(disp, distResults)

## ----comp dist fits, echo=TRUE------------------------------------------------
# Identify the best-fit distribution for the full range of displacement data
compResults <- CompDist(disp, distResults)
compResults

## ----find best-fit d_min for each dist , echo=TRUE----------------------------
# Fit all distributions and identify the best-fit dmin for each distribution
distResults.trunc <- FitDist(disp, full=FALSE, normalise=FALSE)
print(distResults.trunc[["distResults"]])

## ----plot trunc dist, echo=TRUE, message=FALSE, out.width="60%"---------------
# Create a ccdf plot of displacements with fit lines illustrating distributions
# fit to the best-fit xmin for each distribution
plot.data.all.trunc <- PlotDist(disp, distResults.trunc)

## ----fit dist with pl, echo=TRUE, message=FALSE-------------------------------
# Fit all distributions using the d_min value for the power-law distribution
dmin <- distResults.trunc[["distResults"]][1,2]
distResultsPl <- FitDist(disp, set_dmin=dmin, normalise=FALSE)
print(distResultsPl)

## ----fit dist with exp, echo=TRUE, message=FALSE------------------------------
# Fit all distributions using the d_min value for the exponential distribution
dmin <- distResults.trunc[["distResults"]][2,2]
distResultsExp <- FitDist(disp, set_dmin=dmin, normalise=FALSE)
print(distResultsExp[["distResults"]])

## ----fit dist with lnorm, echo=TRUE, message=FALSE----------------------------
# Fit all distributions using the d_min value for the lognormal distribution
dmin <- distResults.trunc[["distResults"]][3,2]
distResultsLnorm <- FitDist(disp, set_dmin=dmin, normalise=FALSE)
print(distResultsLnorm[["distResults"]])


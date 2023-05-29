## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(dpi=300) # set all figures to 300 dpi to avoid pixelated images
knitr::opts_chunk$set(fig.align='center', fig.width = 7, fig.height = 5)
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>")

## ----load physmove, echo=TRUE, highlight=TRUE, clipboard=TRUE-----------------
# Load PhysMove
library(PhysMove)

## ----preview tracks, echo=TRUE------------------------------------------------
# Preview the first 6 rows of the tracks dataset
head(tracks)

## ----tracks structure, echo=TRUE----------------------------------------------
# Determine the structure of the tracks dataset
str(tracks)

## ----plot tracks, out.width="60%"---------------------------------------------
PlotTracks(tracks)

## ----calcualte rms, echo=TRUE, message=FALSE, out.width="60%"-----------------
# Calculate RMS values with default parameters
# rms.result <- RMS(tracks)

## ----summarise rms results, echo=TRUE-----------------------------------------
# Summarise RMS results
# summary(rms.result)

## ----calculate hurst exponent, echo=TRUE--------------------------------------
# Summarise the linear model results and identify the Hurst exponent 
# summary(RMSlinearModel)

## ----view hurst exp, echo=TRUE------------------------------------------------
# Determine the Hurst exponent without displaying the full linear model summary 
# RMSlinearModel$coefficients[2]

## ----calc disp, echo=TRUE-----------------------------------------------------
# Calculate displacements from the tracks dataset with default parameters
disp.all <- CalcDisp(tracks,max_hr=24)

## ----sum disp from 1st time window, echo=TRUE---------------------------------
# Summarise displacements calculated over the first time window (24 ± 6 hours)
summary(unlist(disp.all[[1]]))

## ----plot all norm disp, echo=TRUE, error=FALSE, message=FALSE, fig.width = 7, fig.height = 5, out.width="60%"----
plot.data <- PlotDispPDF(disp.all)

## ----plot all disp (not norm), echo=TRUE, error=FALSE, message=FALSE, out.width="60%"----
plot.data.norm <- PlotDispPDF(disp.all, normalised=FALSE)

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
distResults

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
distResults.trunc

## ----plot trunc dist, echo=TRUE, message=FALSE, out.width="60%"---------------
# Create a ccdf plot of displacements with fit lines illustrating distributions fit to the best-fit xmin for each distribution
plot.data.all.trunc <- PlotDist(disp, distResults.trunc)

## ----fit dist with pl, echo=TRUE, message=FALSE-------------------------------
# Fit all distributions using the d_min value for the power-law distribution
distResultsPl <- FitDist(disp, set_dmin=distResults.trunc$dmin[1], normalise=FALSE)
distResultsPl

## ----fit dist with exp, echo=TRUE, message=FALSE------------------------------
# Fit all distributions using the d_min value for the exponential distribution
distResultsExp <- FitDist(disp, set_dmin=distResults.trunc$dmin[2], normalise=FALSE)
distResultsExp

## ----fit dist with lnorm, echo=TRUE, message=FALSE----------------------------
# Fit all distributions using the d_min value for the lognormal distribution
distResultsLnorm <- FitDist(disp, set_dmin=distResults.trunc$dmin[3], normalise=FALSE)
distResultsLnorm

## ----compdist pl, echo=TRUE, message=FALSE------------------------------------
# Compare distribution fits based on the best-fit d_min value for the power-law distribution
compResultsPl <- CompDist(disp, distResultsPl)
compResultsPl

## ----compdist exp, echo=TRUE, message=FALSE-----------------------------------
# Compare distribution fits based on the best-fit d_min value for the exponential distribution
compResultsExp <- CompDist(disp, distResultsExp)
compResultsExp

## ----compdist lnorm, echo=TRUE, message=FALSE---------------------------------
# Compare distribution fits based on the best-fit d_min value for the lognormal distribution
compResultsLnorm <- CompDist(disp, distResultsLnorm)
compResultsLnorm

## ----randomise tracks, echo=FALSE, error=FALSE, message=FALSE, out.width="60%"----
# Randomise() involves random number selection, so setting a seed enables the replication of results
set.seed(1)
# Randomise tracks from the tracks dataset with default parameters
randomise.result <- Randomise(tracks)

## ----view random results, echo=TRUE-------------------------------------------
# Summarise RMS results
summary(randomise.result)

## ----lm of randomised results, echo=TRUE--------------------------------------
# Determine the slope of the linear model
summary(RandomiselinearModel)

## ----slope of randomised results, echo=TRUE-----------------------------------
# Determine the slope without displaying the full linear model summary
RandomiselinearModel$coefficients[2]


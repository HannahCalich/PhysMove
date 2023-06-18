## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(dpi = 600, 
                      fig.align = 'center', 
                      fig.width = 4, 
                      fig.height = 4,
                      echo = TRUE,
                      collapse = TRUE,
                      comment = "#>") 

## ----load physmove movement vignette, echo=FALSE------------------------------
# Load PhysMove
library(PhysMove)

## ----calcualte rms, eval=FALSE------------------------------------------------
#  # Calculate RMS values with default parameters
#  rms.result <- RMS(tracks)

## ----aspect ratio rms, eval=TRUE, echo=FALSE----------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/calcualte_rms-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----occ_map_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"-----------
knitr::include_graphics("../vignettes/word_formatted/images/calcualte_rms-1.png")

## ----summarise rms results, eval=FALSE----------------------------------------
#  # Summarise RMS results
#  summary(rms.result[["rmsResults"]])

## ----summary_rms_load_fig, echo=FALSE, fig.align='left', out.width='100%'-----
knitr::include_graphics("../vignettes/word_formatted/tables/summary_rms.png")

## ----calculate hurst exponent, eval=FALSE-------------------------------------
#  # Summarise linear model results and identify the Hurst exponent
#  RMSlinearModel <- rms.result[["lm"]]
#  print(RMSlinearModel)

## ----rms_lm_load_fig, echo=FALSE, fig.align='left', out.width='100%'----------
knitr::include_graphics("../vignettes/word_formatted/tables/rms_lm.png")

## ----hurst, eval=FALSE--------------------------------------------------------
#  # Determine the Hurst exponent
#  RMSlinearModel$estimate[2]

## ----hurst_load_fig, echo=FALSE, fig.align='left', out.width='100%'-----------
knitr::include_graphics("../vignettes/word_formatted/tables/hurst.png")

## ----calc disp, eval=FALSE----------------------------------------------------
#  # Calculate displacements with default parameters
#  disp.all <- CalcDisp(tracks)
#  
#  # "15598 displacements in 24 +/- 6 hour(s)"
#  # "15573 displacements in 48 +/- 6 hour(s)"
#  # "15548 displacements in 72 +/- 6 hour(s)"
#  # "15523 displacements in 96 +/- 6 hour(s)"
#  # "15498 displacements in 120 +/- 6 hour(s)"
#  # "15473 displacements in 144 +/- 6 hour(s)"
#  # "15448 displacements in 168 +/- 6 hour(s)"
#  # "15423 displacements in 192 +/- 6 hour(s)"
#  # "15398 displacements in 216 +/- 6 hour(s)"
#  # "15373 displacements in 240 +/- 6 hour(s)"

## ----sum disp from 1st time window, eval=FALSE--------------------------------
#  # Summarise displacements calculated over the first time window (24 ± 6 hours)
#  summary(unlist(disp.all[[1]]))

## ----summary_disp_load_fig, echo=FALSE, fig.align='left', out.width='100%'----
knitr::include_graphics("../vignettes/word_formatted/tables/summary_disp_all_24.png")

## ----plot all norm disp, eval=FALSE-------------------------------------------
#  # Create a probability density function (pdf) plot of normalized displacements
#  plot.data <- PlotDispPDF(disp.all)

## ----aspect ratio all norm, eval=TRUE, echo=FALSE-----------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/plot_all_norm_disp-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----all_norm_disp_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"-----
knitr::include_graphics("../vignettes/word_formatted/images/plot_all_norm_disp-1.png")

## ----plot all disp (not norm), eval=FALSE-------------------------------------
#  # Create a probability density function (pdf) plot of raw (i.e., not normalized) displacements
#  plot.data.norm <- PlotDispPDF(disp.all, normalised=FALSE)

## ----aspect ratio all disp, eval=TRUE, echo=FALSE-----------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/plot_all_disp_not_norm-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----all_disp_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"----------
knitr::include_graphics("../vignettes/word_formatted/images/plot_all_disp_not_norm-1.png")

## ----calc disp over 24 hours, eval=FALSE--------------------------------------
#  # Calculate displacements over 24 ± 6 hours
#  disp <- CalcDisp(tracks, max_hr=24)
#  
#  # "15598 displacements in 24 +/- 6 hour(s)"

## ----summarise disp, eval=FALSE-----------------------------------------------
#  # Summarise displacements
#  summary(unlist(disp))

## ----summary_disp24_load_fig, echo=FALSE, echo=FALSE, fig.align='left', out.width='100%'----
knitr::include_graphics("../vignettes/word_formatted/tables/summary_disp24.png")

## ----plot 24 hr disp, eval=FALSE----------------------------------------------
#  # Plot displacements (as displacements were only calculated over one time window they do not need to be normalised)
#  plot.data.pdf <- PlotDispPDF(disp, normalised=FALSE)

## ----aspect ratio disp24, eval=TRUE, echo=FALSE-------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/plot_24hr_disp-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----disp_24_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"-----------
knitr::include_graphics("../vignettes/word_formatted/images/plot_24hr_disp-1.png")

## ----fit full dist, eval=FALSE------------------------------------------------
#  # Fit all distributions to the full range of displacement data
#  distResults <- FitDist(disp, full=TRUE, normalise=FALSE)
#  distResults[["distResults"]]

## ----dist_full_load_fig, echo=FALSE, echo=FALSE, fig.align='left', out.width='100%'----
knitr::include_graphics("../vignettes/word_formatted/tables/dist_full.png")

## ----plot full dist, eval=FALSE-----------------------------------------------
#  # Create a ccdf plot of displacements with fit lines illustrating distributions fit to the full range of displacements
#  plot.data.all.pdf <- PlotDist(disp, distResults)

## ----aspect ratio full dist, eval=TRUE, echo=FALSE----------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/plot_full_dist-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----all_dist_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"----------
knitr::include_graphics("../vignettes/word_formatted/images/plot_full_dist-1.png")

## ----comp dist fits, eval=FALSE-----------------------------------------------
#  # Identify the best-fit distribution for the full range of displacement data
#  compResults <- CompDist(disp, distResults)
#  compResults

## ----dist_full_comp_load, echo=FALSE, echo=FALSE, fig.align='left', out.width='100%'----
knitr::include_graphics("../vignettes/word_formatted/tables/dist_full_comp.png")

## ----find best-fit dmin for each dist, eval=FALSE-----------------------------
#  # Fit all distributions and identify the best-fit dmin for each distribution
#  distResults.trunc <- FitDist(disp, full=FALSE, normalise=FALSE)
#  print(distResults.trunc[["distResults"]])

## ----dist_trunc_load, echo=FALSE, echo=FALSE, fig.align='left', out.width='100%'----
knitr::include_graphics("../vignettes/word_formatted/tables/dist_trunc.png")

## ----plot trunc dist, eval=FALSE----------------------------------------------
#  # Create a ccdf plot of displacements with fit lines illustrating distributions
#  # fit to the best-fit dmin for each distribution
#  plot.data.all.trunc <- PlotDist(disp, distResults.trunc)

## ----aspect ratio trunc dist, eval=TRUE, echo=FALSE---------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/plot_trunc_dist-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----all_trunc_dist_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"----
knitr::include_graphics("../vignettes/word_formatted/images/plot_trunc_dist-1.png")

## ----fit dist with pl, eval=FALSE---------------------------------------------
#  # Fit all distributions using the dmin value for the power-law distribution
#  dmin <- distResults.trunc[["distResults"]][1,2]
#  distResultsPl <- FitDist(disp, set_dmin=dmin, normalise=FALSE)

## ----fit dist with exp, eval=FALSE--------------------------------------------
#  # Fit all distributions using the dmin value for the exponential distribution
#  dmin <- distResults.trunc[["distResults"]][2,2]
#  distResultsExp <- FitDist(disp, set_dmin=dmin, normalise=FALSE)

## ----fit dist with lnorm, eval=FALSE------------------------------------------
#  # Fit all distributions using the dmin value for the lognormal distribution
#  dmin <- distResults.trunc[["distResults"]][3,2]
#  distResultsLnorm <- FitDist(disp, set_dmin=dmin, normalise=FALSE)

## ----comp pl, eval=FALSE------------------------------------------------------
#  # Compare distribution fits based on the best-fit dmin value for the power-law distribution
#  compResultsPl <- CompDist(disp, distResultsPl)
#  compResultsPl

## ----pl_trunc_dist_load, echo=FALSE, fig.align='left', out.width='100%'-------
knitr::include_graphics("../vignettes/word_formatted/tables/dist_pl_comp.png")

## ----comp exp, eval=FALSE-----------------------------------------------------
#  # Compare distribution fits based on the best-fit dmin value for the exponential distribution
#  compResultsExp <- CompDist(disp, distResultsExp)
#  compResultsExp

## ----exp_trunc_dist_load, echo=FALSE, fig.align='left', out.width='100%'------
knitr::include_graphics("../vignettes/word_formatted/tables/dist_exp_comp.png")

## ----comp lnorm, eval=FALSE---------------------------------------------------
#  # Compare distribution fits based on the best-fit dmin value for the lognormal distribution
#  compResultsLnorm <- CompDist(disp, distResultsLnorm)
#  compResultsLnorm

## ----lnorm_trunc_dist_load, echo=FALSE, fig.align='left', out.width='100%'----
knitr::include_graphics("../vignettes/word_formatted/tables/dist_lnorm_comp.png")

## ----randomise tracks, eval=FALSE---------------------------------------------
#  # Randomise() involves random number selection, so setting a seed enables the replication of results
#  set.seed(1)
#  # Randomise tracks from the tracks dataset with default parameters
#  randomise.result <- Randomise(tracks)

## ----aspect ratio randomise tracks, eval=TRUE, echo=FALSE---------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/randomise_tracks-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----randomise_tracks_load, echo=FALSE, fig.asp=img.asp, out.width="60%"------
knitr::include_graphics("../vignettes/word_formatted/images/randomise_tracks-1.png")

## ----view random results, eval=FALSE------------------------------------------
#  # Summarise RMS results
#  summary(randomise.result[["resultsDF"]])

## ----summarise_random_load, echo=FALSE, fig.align='left', out.width='100%'----
knitr::include_graphics("../vignettes/word_formatted/tables/summary_randomise.png")

## ----lm of randomised results, echo=TRUE, eval=FALSE--------------------------
#  # Determine the slope of the linear model
#  RandomiselinearModel <- randomise.result[["lm"]]
#  print(RandomiselinearModel)

## ----summarise_random_lm_load, echo=FALSE, fig.align='left', out.width='100%'----
knitr::include_graphics("../vignettes/word_formatted/tables/randomise_lm.png")

## ----slope of randomised results, echo=TRUE, eval=FALSE-----------------------
#  # Determine the slope without displaying the full linear model summary
#  RandomiselinearModel$estimate[2]

## ----summarise_random_lm_exp_load, echo=FALSE, fig.align='left', out.width='100%'----
knitr::include_graphics("../vignettes/word_formatted/tables/randomise_exp.png")

## ----plot random tracks, eval=FALSE-------------------------------------------
#  # Plot random tracks for tracks dataset reference id 1
#  plot.data.random.tracks <- PlotRandomTracks(tracks, ref=1, randomise.result)

## ----aspect ratio random tracks, eval=TRUE, echo=FALSE------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/plot_random_tracks-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----random_tracks_load, echo=FALSE, fig.asp=img.asp, out.width="60%"---------
knitr::include_graphics("../vignettes/word_formatted/images/plot_random_tracks-1.png")

## ----calc turn angles, eval=FALSE---------------------------------------------
#  # Calculate turning angles in the tracks dataset using default parameters
#  angle.results <- TurningAngles(tracks)
#  
#  # "15573 angles in 24 +/- 6 hour(s)"
#  # "15523 angles in 48 +/- 6 hour(s)"
#  # "15473 angles in 72 +/- 6 hour(s)"
#  # "15423 angles in 96 +/- 6 hour(s)"
#  # "15373 angles in 120 +/- 6 hour(s)"
#  # "15323 angles in 144 +/- 6 hour(s)"
#  # "15273 angles in 168 +/- 6 hour(s)"
#  # "15223 angles in 192 +/- 6 hour(s)"
#  # "15173 angles in 216 +/- 6 hour(s)"
#  # "15123 angles in 240 +/- 6 hour(s)"

## ----aspect ratio angles, eval=TRUE, echo=FALSE-------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/turn_angles-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----turn_angles_load, echo=FALSE, fig.asp=img.asp, out.width="60%"-----------
knitr::include_graphics("../vignettes/word_formatted/images/turn_angles-1.png")

## ----summarise turning angles, eval=FALSE-------------------------------------
#  # Summarise turning angles calculated over the first time window
#  summary(angle.results[[1]])

## ----summary_angles_load, echo=FALSE, fig.align='left', out.width='100%'------
knitr::include_graphics("../vignettes/word_formatted/tables/summary_angles.png")

## ----plot angles with a circle plot, eval=FALSE-------------------------------
#  # Plot angles with a circle plot
#  plot.data.angles <- PlotAngles(angle.results)

## ----aspect ratio angles circle, eval=TRUE, echo=FALSE------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/plot_angles_circle_plot-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----angles_circle__load, echo=FALSE, fig.asp=img.asp, out.width="60%"--------
knitr::include_graphics("../vignettes/word_formatted/images/plot_angles_circle_plot-1.png")


## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(dpi = 600, 
                      fig.align = 'center', 
                      fig.width = 4, 
                      fig.height = 4,
                      echo = TRUE,
                      collapse = TRUE,
                      comment = "#>") 

## ----installation, eval=FALSE-------------------------------------------------
#  # Install the devtools package from CRAN (if required)
#  install.packages("devtools")
#  
#  # Download the development version from GitHub:
#  devtools::install_github("HannahCalich/PhysMove", auth_token = "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", build_vignettes = TRUE)

## ----load physmove------------------------------------------------------------
# Load PhysMove
library(PhysMove)

## ----check tracks, eval=FALSE-------------------------------------------------
#  # Check your data are formatted correctly
#  CheckTracks(data) # replace "data" with your data frame

## ----head tracks, eval=FALSE--------------------------------------------------
#  # Preview the first 6 rows of the tracks dataset
#  head(tracks)

## ----tracks_head_load_fig, echo=FALSE, fig.align='left', out.width='100%'-----
knitr::include_graphics("../vignettes/word_formatted/tables/tracks_head.png")

## ----structure tracks, eval=FALSE---------------------------------------------
#  # Determine the structure of the tracks dataset
#  str(tracks)

## ----tracks_str_load_fig, echo=FALSE, fig.align='left', out.width='100%'------
knitr::include_graphics("../vignettes/word_formatted/tables/tracks_str.png")

## ----plot tracks, eval=FALSE, message=FALSE-----------------------------------
#  PlotTracks(tracks)

## ----aspect ratio, eval=TRUE, echo=FALSE--------------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/plot_tracks-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----plot_tracks_load_fig, echo=FALSE, fig.asp=img.asp, out.width="70%"-------
knitr::include_graphics("../vignettes/word_formatted/images/plot_tracks-1.png")


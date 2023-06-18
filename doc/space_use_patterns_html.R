## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(dpi = 600, 
                      fig.align = 'center', 
                      fig.width = 4, 
                      fig.height = 4,
                      echo = TRUE,
                      collapse = TRUE,
                      comment = "#>") 

## ----load physmove space-use vignette, echo=FALSE-----------------------------
# Load PhysMove
library(PhysMove)

## ----calc occ, eval=FALSE, echo=TRUE, message=FALSE---------------------------
#  # Create an occupancy map based on the tracks dataset
#  Occ <- Occupancy(tracks)

## ----aspect ratio, eval=TRUE, echo=FALSE--------------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/occ_map-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----occ_map_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"-----------
knitr::include_graphics("../vignettes/word_formatted/images/occ_map-1.png")

## ----summarise occ, echo=TRUE, eval=FALSE-------------------------------------
#  # Summarize occupancy results
#  summary(Occ)

## ----summary_occ_load_fig, echo=FALSE, fig.align='left', out.width='100%'-----
knitr::include_graphics("../vignettes/word_formatted/tables/summary_occ.png")

## ----pdf occ, echo=TRUE, eval=FALSE-------------------------------------------
#  # Create a pdf plot of occupancy values
#  pdf.occ  <- PlotPDF(Occ$Occupancy, desc="Occupancy")

## ----aspect ratio2, eval=TRUE, echo=FALSE-------------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/occ_pdf-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----occ_pdf_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"-----------
knitr::include_graphics("../vignettes/word_formatted/images/occ_pdf-1.png")

## ----load infomap, eval=FALSE, message=FALSE, warning=FALSE-------------------
#  # Identify community-wide movements
#  setwd("~/2023/PhysMove") # Update your working directory to the folder containing the infomap file
#  library(infomapecology)
#  infomapResult <- InfomapCommunities(tracks)

## ----structure of infomap, echo=TRUE, eval=FALSE------------------------------
#  # View the Infomap monolayer object structure
#  str(infomapResult[["infomap_object"]])

## ----infomap_str_load_fig, echo=FALSE, fig.align='left', out.width='100%'-----
knitr::include_graphics("../vignettes/word_formatted/tables/infomap_str.png")

## ----infomap map, echo=TRUE, message=FALSE, eval=FALSE------------------------
#  # Create a map of the Infomap communities
#  CommunityMap(infomapResult)

## ----aspect ratio3, eval=TRUE, echo=FALSE-------------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/infomap_map-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----incomap_map_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"-------
knitr::include_graphics("../vignettes/word_formatted/images/infomap_map-1.png")


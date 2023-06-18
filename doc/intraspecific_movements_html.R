## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(dpi = 600, 
                      fig.align = 'center', 
                      fig.width = 4, 
                      fig.height = 4,
                      echo = TRUE,
                      collapse = TRUE,
                      comment = "#>") 

## ----load physmove intraspecific vignette, echo=FALSE-------------------------
# Load PhysMove
library(PhysMove)

## ----gyrad, eval=FALSE, echo=TRUE, message=FALSE------------------------------
#  # Calculate the dispersion of each track in the tracks dataset
#  GR <- GyrationRad(tracks)

## ----aspect ratio, eval=TRUE, echo=FALSE--------------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/gyrad-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----gryad_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"-------------
knitr::include_graphics("../vignettes/word_formatted/images/gyrad-1.png")

## ----preview gyrad, echo=TRUE, eval=FALSE-------------------------------------
#  # Summarize gyration radius results
#  summary(GR)

## ----summary_gyrad_load_fig, echo=FALSE, fig.align='left', out.width='100%'----
knitr::include_graphics("../vignettes/word_formatted/tables/summary_gyrad.png")

## ----gyrad pdf, eval=FALSE, echo=TRUE-----------------------------------------
#  # Create a pdf plot of gyration radius values
#  pdf.gr <- PlotPDF((GR$`rG (km)`), desc="GyrationRad")

## ----aspect ratio2, eval=TRUE, echo=FALSE-------------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/gyrad_pdf-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----gyrad_pdf_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"---------
knitr::include_graphics("../vignettes/word_formatted/images/gyrad_pdf-1.png")

## ----ent, echo=TRUE, eval=FALSE-----------------------------------------------
#  # Calculate track entropy using default parameters
#  Ent <- Entropy(tracks)

## ----aspect ratio3, eval=TRUE, echo=FALSE-------------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/ent-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----ent_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"---------------
knitr::include_graphics("../vignettes/word_formatted/images/ent-1.png")

## ----ent head, echo=TRUE, eval=FALSE------------------------------------------
#  # Summarise Entropy results
#  summary(Ent)

## ----summary_ent_load_fig, echo=FALSE, fig.align='left', out.width='100%'-----
knitr::include_graphics("../vignettes/word_formatted/tables/summary_ent.png")

## ----ent pdf, echo=TRUE, eval=FALSE-------------------------------------------
#  # Create a pdf plot of the entropy scores
#  pdf.ent <- PlotPDF(Ent$normalisedEntropy, "Entropy")

## ----aspect ratio4, eval=TRUE, echo=FALSE-------------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/ent_pdf-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----ent_pdf_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"-----------
knitr::include_graphics("../vignettes/word_formatted/images/ent_pdf-1.png")

## ----predict, echo=TRUE, eval=FALSE-------------------------------------------
#  # Track predictability using Predictability() default parameters and the output from Entropy()
#  Pred <- Predictability(tracks, Ent)

## ----aspect ratio5, eval=TRUE, echo=FALSE-------------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/predict-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----predict_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"-----------
knitr::include_graphics("../vignettes/word_formatted/images/predict-1.png")

## ----predict head, echo=TRUE, eval=FALSE--------------------------------------
#  # Summarize Predictability scores
#  summary(Pred)

## ----summary_predict_load_fig, echo=FALSE, fig.align='left', out.width='100%'----
knitr::include_graphics("../vignettes/word_formatted/tables/summary_predict.png")

## ----predict pdf, echo=TRUE, eval=FALSE---------------------------------------
#  # Create a pdf plot of the predictability scores
#  pdf.pred <- PlotPDF(Pred$Predictability, desc="Predictability")

## ----aspect ratio6, eval=TRUE, echo=FALSE-------------------------------------
# First chunk to fetch the image size and calculate its aspect ratio
img <- magick::image_read("../vignettes/word_formatted/images/predict_pdf-1.png") # read the image using the magic library
img.asp <- magick::image_info(img)$height / magick::image_info(img)$width # calculate the figures aspect ratio

## ----predict_pdf_load_fig, echo=FALSE, fig.asp=img.asp, out.width="60%"-------
knitr::include_graphics("../vignettes/word_formatted/images/predict_pdf-1.png")


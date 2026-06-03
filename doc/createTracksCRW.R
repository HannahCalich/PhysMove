###########################################################################################
###########################################################################################
#####                                                                                 #####
##### Create sample data for the PhysMove R Package                                   #####
##### Last updated: April 2026                                                        #####
#####                                                                                 #####
###########################################################################################
###########################################################################################

# Create simulated tracking data that follows a correlated random
# walk (crw) movement model. Simulated tracks created with the
# aniMotum package developed by Ian Jonsen et al., 2023. Tracks
# were iteratively simulated until 25 tracks that did not intersect
# land were created.

# Reference:
# Ian Jonsen, W James Grecian, Lachlan Phillips, Gemma Carroll,
# Clive R. McMahon, Robert G. Harcourt, Mark A. Hindell, and
# Toby A. Patterson (2023) aniMotum, an R package for animal
# movement data: Rapid quality control, behavioural estimation
# and simulation. Methods in Ecology and Evolution
# <doi:10.1111/2041-210X.14060>

library(aniMotum)
library(sf)
library(rnaturalearth)

# For consistency with existing tracks dataset, trackCRW has
# 25 tracks each with between 200 and 1000 location estimates
n.tracks <- 25
set.seed(1)
samplesize <- floor(stats::runif(n.tracks, 200, 1000))

# CRW correlation parameter
crw_D <- 0.5

# load land polygons
land <- ne_countries(scale = "medium", returnclass = "sf")
land <- st_make_valid(land)

# loop through sim() to create tracks
tracksCRW_list <- vector("list", n.tracks)
for (s in seq_len(n.tracks)) {
  valid <- FALSE
  while (!valid) {

    # simulate CRW
    tr <- sim(
      N = samplesize[s],
      model = "crw",
      D = crw_D
    )

    # convert to sf points
    tr_sf <- st_as_sf(
      tr,
      coords = c("lon", "lat"),
      crs = 4326,
      remove = FALSE
    )

    # check for land intersections
    intersects <- st_intersects(tr_sf, land, sparse = FALSE)

    # if the track does not intersect, add it to the dataset
    if (!any(intersects)) {
      valid <- TRUE
      tr$track_id <- s
      tracksCRW_list[[s]] <- tr
    }
  }
}
# combine tracks
tracksCRW <- do.call(rbind, tracksCRW_list)

# clean up
rm(tracksCRW_list)
rm(tr)

# format for PhysMove
tracksCRW <- tracksCRW[, c("track_id", "lon", "lat", "date")]
names(tracksCRW)[1] <- "ref"
names(tracksCRW)[4] <- "day"

tracksCRW <- as.data.frame(tracksCRW)

# save output
# setwd()
# save(tracksCRW, file = "tracksCRW.RData")

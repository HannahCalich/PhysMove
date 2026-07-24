## create example data

setwd("~/2025/PhysMove/data")

devtools::install_github("HannahCalich/PhysMove", build_vignettes = TRUE, force = TRUE)

library(PhysMove)

angleList <- turningAngles(tracks, max_hr = 24)
save(angleList, file ="angleList.RData", compress = "xz")

angleListAll <- turningAngles(tracks)
save(angleListAll, file ="angleListAll.RData", compress = "xz")

disp <- calcDisp(tracks, max_hr=24)
save(disp, file="disp.RData", compress = "xz")

dispAll <- calcDisp(tracks)
save(dispAll, file="dispAll.RData", compress = "xz")

distResultsAll <- fitDist(disp, full=TRUE, normalise=FALSE) 
save(distResultsAll, file="distResultsAll.RData", compress = "xz")

distResultsTrunc <- fitDist(disp, full=FALSE, normalise=FALSE) # find best-fit dmin for all
save(distResultsTrunc, file="distResultsTrunc.RData", compress = "xz")

distResultsExp <- fitDist(disp, set_dmin = distResultsTrunc[[1]][2,2], normalise=FALSE) # fit all dist to best fit dmin for exp
save(distResultsExp, file="distResultsExp.RData", compress = "xz")

entropyResults <- entropy(tracks)
save(entropyResults, file="entropyResults.RData", compress = "xz")

setwd("~/2025/PhysMove")
library(infomapecology)
infomapResult <- infomapCommunities(tracks)
setwd("~/2025/PhysMove/PhysMove_Git/PhysMove/data")
save(infomapResult, file="infomapResult.RData", compress = "xz")

occupancyResults <- occupancy(tracks)
save(occupancyResults, file="occupancyResults.RData", compress = "xz")

set.seed(1)
randomResults <- randomise(tracks)
save(randomResults, file="randomResults.RData", compress = "xz")


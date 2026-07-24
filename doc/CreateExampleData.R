## create example data

setwd("~/2025/PhysMove/data")

devtools::install_github("HannahCalich/PhysMove", build_vignettes = TRUE, force = TRUE)

library(PhysMove)

angleList <- turningAngles(tracks, max_hr = 24)
save(angleList, file ="angleList.RData")

angleListAll <- turningAngles(tracks)
save(angleListAll, file ="angleListAll.RData")

disp <- calcDisp(tracks, max_hr=24)
save(disp, file="disp.RData")

dispAll <- calcDisp(tracks)
save(dispAll, file="dispAll.RData")

distResultsAll <- fitDist(disp, full=TRUE, normalise=FALSE) 
save(distResultsAll, file="distResultsAll.RData")

distResultsTrunc <- fitDist(disp, full=FALSE, normalise=FALSE) # find best-fit dmin for all
save(distResultsTrunc, file="distResultsTrunc.RData")

distResultsExp <- fitDist(disp, set_dmin = distResultsTrunc[[1]][2,2], normalise = distResultsTrunc[2][[1]]) # fit all dist to best fit dmin for exp
save(distResultsExp, file="distResultsExp.RData")

entropyResults <- entropy(tracks)
save(entropyResults, file="entropyResults.RData")

setwd("~/2025/PhysMove")
library(infomapecology)
infomapResult <- infomapCommunities(tracks)
setwd("~/2025/PhysMove/PhysMove_Git/PhysMove/data")
save(infomapResult, file="infomapResult.RData")

occupancyResults <- occupancy(tracks)
save(occupancyResults, file="occupancyResults.RData")

randomResults <- randomise(tracks, randTrack = 1)
save(randomResults, file="randomResults.RData")


## create example data

setwd("~/2023/PhysMove/PhysMove_Git/PhysMove/data")

library(PhysMove)

angleList <- TurningAngles(tracks, max_hr = 24)
save(angleList, file ="angleList.RData")

disp <- CalcDisp(tracks, max_hr=24)
save(disp, file="disp.RData")

distResultsAll <- FitDist(disp, full=TRUE)
save(distResultsAll, file="distResultsAll.RData")

distResultsExp <- FitDist(disp, set_dmin = 1.649160)
save(distResultsExp, file="distResultsExp.RData")

entropyResults <- Entropy(tracks)
save(entropyResults, file="entropyResults.RData")

setwd("~/2023/PhysMove")
library(infomapecology)
infomapResult <- InfomapCommunities(tracks, tpm=TRUE)
setwd("~/2023/PhysMove/PhysMove_Git/PhysMove/data")
save(infomapResult, file="infomapResult.RData")

occupancyResults <- Occupancy(tracks)
save(occupancyResults, file="occupancyResults.RData")

randomResults <- Randomise(tracks, randTrack = 1)
save(randomResults, file="randomResults.RData")


#Calculate random location data where displacement lengths follow a power law distribution
set.seed(1)
samplesize <- 1000
distmin <- 2.5e-1 #in km
alpha <- 2.5 #scaling parameter typically lies between 2-3
r <- runif(samplesize,0,1) #uniform random number between 0-1
pl_x <- distmin*(1-r)^(-1/(alpha-1)) #Displacement values in km where x >= xmin
angle <- runif(samplesize,0,360) #random angles between 0-360
plSample <- data.frame("ref"=c(1),"lon"=c(rep(0,samplesize)),"lat"=c(rep(0,samplesize)),"day"=as.POSIXct("2015-01-01 09:30:31")) # 1 track
Radius <- 6371 #Earth Radius in km (disp are in km)
rad <- 3.141592653589793/180
for (i in 1:(samplesize-1)){
  d <- pl_x[i]
  a <- angle[i]*(rad)
  lat1 <- plSample$lat[i]*(rad) # convert to radians
  long1 <- plSample$lon[i]*(rad) # convert to radians
  lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a)) # Calculate new lat in radians based on angle and distance
  long2 <- long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2)) # Calculate new long in radians based on angle and distance
  plSample$lon[i+1] <- long2/rad # convert to degrees and save
  plSample$lat[i+1] <- lat2/rad # convert to degrees and save
  plSample$day[i+1] <- plSample$day[i]+(24*60*60) # previous day plus number of hours it would take the animal to travel that distance
}
# plot(plSample$lon,plSample$lat,type="l", lty=1)
rm(list=ls()[! ls() %in% c("plSample")])
# save(plSample, file = "plSample.RData")

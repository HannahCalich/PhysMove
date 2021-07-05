#Calculate random location data where displacement lengths follow an exponential distribution
set.seed(1)
samplesize <- 1000
lambda <- 0.125
distmin <- 2.5e-1 #in km
r <- runif(samplesize,0,1) #uniform random number between 0-1
exp_x <- distmin-(1/lambda)*log(1-r) #Mean of exponential dist should equal 1/lambda when xmin isn't a factor. e.g., 1/mean(-(1/lambda)*log(1-r)) = lambda
angle <- runif(samplesize,0,360) #random angles between 0-360
expSample<-data.frame("ref"=c(1),"lon"=c(rep(0,samplesize)),"lat"=c(rep(0,samplesize)),"day"=as.POSIXct("2015-01-01 09:30:31")) # 1 track
Radius <- 6371 #Earth Radius in km (disp are in km)
rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
for (i in 1:(samplesize-1)){
  d <- exp_x[i]
  a <- angle[i]*(rad)
  lat1 <- expSample$lat[i]*(rad) # convert to radians
  long1 <- expSample$lon[i]*(rad) # convert to radians
  lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a)) # Calculate new lat in radians based on angle and distance
  long2 <- long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2)) # Calculate new long in radians based on angle and distance
  expSample$lon[i+1] <- long2/rad # convert to degrees and save
  expSample$lat[i+1] <- lat2/rad # convert to degrees and save
  expSample$day[i+1] <- expSample$day[i]+(24*60*60) # previous day plus number of hours it would take the animal to travel that distance
}
# plot(expSample$lon,expSample$lat,type="l", lty=1)
rm(list=ls()[! ls() %in% c("expSample")])
# save(expSample, file = "expSample.RData")

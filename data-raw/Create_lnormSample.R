#Calculate random location data where displacement lengths follow a log-normal distribution
set.seed(1)
samplesize <- 1000
distmin <- 2.5e-1 #in km
distmax <- 50 #to keep on same scale as PL and Exp data
mu <- 0.3
sigma <- 2
lnorm_x <- EnvStats::rlnormTrunc(samplesize, mu, sigma, min = distmin, max = distmax)
lnormSample <- data.frame("ref"=c(1),"lon"=c(rep(0,samplesize)),"lat"=c(rep(0,samplesize)),"day"=as.POSIXct("2015-01-01 09:30:31")) # 1 track
Radius <- 6371 #Earth Radius in km (disp are in km)
rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
for (i in 1:(samplesize-1)){
  d <- lnorm_x[i]
  a <- angle[i]*(rad)
  lat1 <- lnormSample$lat[i]*(rad) # convert to radians
  long1 <- lnormSample$lon[i]*(rad) # convert to radians
  lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a)) # Calculate new lat in radians based on angle and distance
  long2 <- long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2)) # Calculate new long in radians based on angle and distance
  lnormSample$lon[i+1] <- long2/rad # convert to degrees and save
  lnormSample$lat[i+1] <- lat2/rad # convert to degrees and save
  lnormSample$day[i+1] <- lnormSample$day[i]+(24*60*60) # previous day plus number of hours it would take the animal to travel that distance
}
# plot(lnormSample$lon,lnormSample$lat,type="l", lty=1)
rm(list=ls()[! ls() %in% c("lnormSample")])
# save(lnormSample, file = "lnormSample.RData")
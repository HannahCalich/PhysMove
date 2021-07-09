#Calculate random location data where displacement lengths follow an exponential distribution
set.seed(1)

tracks <- 2 #how many tracks do you want to make?
lambda <- 0.125 #for all tracks
distmin <- 2.5e-1 #in km

directedforward <- 0.3 #percent of angles showing forward directed movement
directedbackward <- 0.3 #percent of angles showing backward directed movement
random <- 0.4 #percent of angles showing random movement

dates <- sample(seq(as.POSIXct('2015-01-01 12:00:00'), as.POSIXct('2020-01-01 24:00:00'), by="day"), tracks-1) #random tagging dates
expSample <- data.frame("ref"=c(1),"lon"=c(rep(0,samplesize)),"lat"=c(rep(0,samplesize)),"day"=dates[1]) # 1 track

for (r in 1:tracks){
  samplesize <- floor(runif(1,200,1000)) #how many locations is the track going to have (min 200 max 1000)
  r <- runif(samplesize,0,1) #uniform random number between 0-1
  exp_x <- distmin-(1/lambda)*log(1-r) #Mean of exponential dist should equal 1/lambda when xmin isn't a factor. e.g., 1/mean(-(1/lambda)*log(1-r)) = lambda
  directedForwardAngle1 <- round(runif((ceiling((directedforward/2)*samplesize)),330,360),2)
  directedForwardAngle2 <- round(runif((ceiling((directedforward/2)*samplesize)),0,30),2)
  directedBackwardAngle <- round(runif((ceiling((directedbackward)*samplesize)),150,210),2)
  randomAngle <- runif(ceiling((random*samplesize)),0,360)
  angles <- c(directedForwardAngle1,directedForwardAngle2,directedBackwardAngle,randomAngle) #put the angles together
  angles <- sample(angles,samplesize) #randomize the order of the angles
  Radius <- 6371 #Earth Radius in km (disp are in km)
  rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
  expSample <- data.frame("ref"=c(1),"lon"=c(rep(0,samplesize)),"lat"=c(rep(0,samplesize)),"day"=as.POSIXct("2015-01-01 09:30:31")) # 1 track
  for (i in 1:(samplesize[r]-1)){
    d <- exp_x[i]
    a <- angle[i]*(rad)
    lat1 <- expSample$lat[i]*(rad) # convert to radians
    long1 <- expSample$lon[i]*(rad) # convert to radians
    lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a)) # calculate new lat in radians based on angle and distance
    long2 <- long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2)) # calculate new long in radians based on angle and distance
    expSample$lon[i+1] <- long2/rad # convert to degrees and save
    expSample$lat[i+1] <- lat2/rad # convert to degrees and save
    expSample$day[i+1] <- expSample$day[i]+(24*60*60) # previous day plus number of hours it would take the animal to travel that distance
  }
  expSample$ref <- r
}
# plot(expSample$lon,expSample$lat,type="l", lty=1)
rm(list=ls()[! ls() %in% c("expSample")])
# save(expSample, file = "expSample.RData")

#Calculate random location data where displacement lengths follow an exponential distribution

tracks <- 25 #how many tracks do you want to make?
lambda <- 0.125 #for all tracks
distmin <- 2.5e-1 #in km

directedforward <- 0.3 #percent of angles showing forward directed movement - this ends up being split into 330-360 and 0-30 degree angles below
directedbackward <- 0.3 #percent of angles showing backward directed movement
random <- 0.4 #percent of angles showing random movement

set.seed(1)
startLatRange <- runif(tracks,0,2) #random tagging location between 0-2 deg latitude
set.seed(1)
startLonRange <- runif(tracks,0,2) #random tagging location between 0-2 deg longitude

set.seed(1)
dates <- sample(seq(as.POSIXct('2015-01-01 12:00:00'), as.POSIXct('2020-01-01 24:00:00'), by="day"), tracks) #random tagging dates

speciesA <- data.frame("ref"=0,"lon"=startLonRange[1],"lat"=startLatRange[1],"day"=dates[1]) # start loop, delete row at end of loop

set.seed(1)
samplesize <- floor(runif(tracks,200,1000)) #how many locations is the track going to have (min 200 max 1000)

i=1
for (i in 1:tracks){
  set.seed(1)
  r <- runif(samplesize[i]-1,0,1) #uniform random number between 0-1
  exp_x <- distmin-(1/lambda)*log(1-r) #Mean of exponential dist should equal 1/lambda when xmin isn't a factor. e.g., 1/mean(-(1/lambda)*log(1-r)) = lambda
  set.seed(1)
  directedForwardAngle1 <- round(runif((ceiling((directedforward/2)*samplesize[i])),330,360),2)
  set.seed(1)
  directedForwardAngle2 <- round(runif((ceiling((directedforward/2)*samplesize[i])),0,30),2)
  set.seed(1)
  directedBackwardAngle <- round(runif((ceiling((directedbackward)*samplesize[i])),150,210),2)
  set.seed(1)
  randomAngle <- runif(ceiling((random*samplesize[i])),0,360)
  angles <- c(directedForwardAngle1,directedForwardAngle2,directedBackwardAngle,randomAngle) #put the angles together
  set.seed(1)
  angles <- sample(angles,samplesize[i]) #randomize the order of the angles, if the rounding earlier caused too many angles this will bring it back to the designated sample size
  Radius <- 6371 #Earth Radius in km (disp are in km)
  rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
  speciesA<-rbind(speciesA,c(data.frame("ref"=rep(i,samplesize[i]),"lon"=rep(startLonRange[i],samplesize[i]),"lat"=rep(startLatRange[i],samplesize[i]),
                                          "day"=rep(dates[i],samplesize[i]))))
  if (i==1){
    startRow <- 1
  } else {
    startRow <- sum(samplesize[1:i-1])+1
  }
  j=1
  for (j in 1:(samplesize[i]-1)){ #for each new location in each track  1-411
    d <- exp_x[j] #random disp from exp dist
    a <- angles[j]*(rad) #random angle from those created previously
    lat1 <- speciesA$lat[startRow+j]*(rad) # convert to radians - current lat
    long1 <- speciesA$lon[startRow+j]*(rad) # convert to radians - current long
    lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a)) # calculate new lat in radians based on angle and distance
    long2 <- long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2)) # calculate new long in radians based on angle and distance
    speciesA$lon[startRow+j+1] <- long2/rad # convert to degrees and save
    speciesA$lat[startRow+j+1] <- lat2/rad # convert to degrees and save
    speciesA$day[startRow+j+1] <- speciesA$day[startRow+j]+(24*60*60) # previous day plus number of hours it would take the animal to travel that distance
  }
}
speciesA<-speciesA[-1,]
cols <- data.frame(
  col = rainbow(length(unique(speciesA$ref))),
  id = unique(speciesA$ref)
)
with(speciesA, {
  plot(lon,lat,pch = 16, col = cols$col[match(ref, cols$id)], type = "p",
       ylim=c(min(lat)-0.25,max(lat)+0.25),xlim=c(min(lon)-0.25,max(lon)+0.25),cex=0.5,
       ylab="Latitude", xlab="Longitude")
  for (l in unique(ref)) {
    # print(cols$col[match(l, cols$id)])
    lines(lon[ref == l], lat[ref == l], type = "l", col = cols$col[match(l, cols$id)])
  }
})
# print(tracks)
# print(summary(samplesize))
# print(summary(dates))
rm(list=ls()[! ls() %in% c("speciesA")])

##################################
## Set working directory and save
##################################
# setwd()
# save(speciesA, file = "speciesA.RData")


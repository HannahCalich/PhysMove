###########################################################################################
###########################################################################################
#####                                                                                 #####
##### Create sample data for the PhysMove R Package                                   #####
##### Package Authors: Hannah Calich & Ana Sequeira                                   #####
##### Contact: hannah.calich@gmail.com                                                #####
##### Last updated: January 10, 2022                                                  #####
#####                                                                                 #####
###########################################################################################
###########################################################################################

# Calculate random telemetry data where displacement lengths follow an exponential distribution,
# evidence of directed motion, and high intraspecific variation

# Set track parameters
n.tracks <- 25 # how many tracks do you want to make?
lambda <- 0.125 # exponential exponent for all tracks
distmin <- 2.5e-1 # minimum distance in km

# Turning angles (forward, backwards, and random angles must sum to 1)
directedforward <- 0.3 # percent of angles showing forward directed movement - this ends up being
# split into 330-360 and 0-30 degree angles below
directedbackward <- 0.3 # percent of angles showing backward directed movement
random <- 0.4 # percent of angles showing random movement

# Tagging location
# set.seed(1)
# startLatRange <- runif(n.tracks,0,0.1) # random tagging location between 0-2 deg latitude
# set.seed(2) # stop lat and long from having identical values
# startLonRange <- runif(n.tracks,0,0.1) # random tagging location between 0-2 deg longitude

startLonRange <- startLatRange <- rep (0, n.tracks)

# Tagging dates
set.seed(1)
dates <- sample(seq(as.POSIXct('2015-01-01 12:00:00'), as.POSIXct('2020-01-01 24:00:00'),
                    by="day"), n.tracks)

# Create dataframe
tracks <- data.frame("ref"=0,"lon"=startLonRange[1],"lat"=startLatRange[1],"day"=dates[1])
# start loop, delete row at end of loop

# Number of locations per track
set.seed(1)
samplesize <- floor(runif(n.tracks,200,1000)) # how many locations is the track going to have
# default is: min 200 and max 1000

# Parameters needed to create location data
Radius <- 6371 # Earth Radius in km (displacements are in km)
rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"

# Create tracks with parameters set above
i=1
for (i in 1:n.tracks){ # for each track
  set.seed(1)
  r <- runif(samplesize[i]-1,0,1) # create i displacements based on a uniform distribution between 0-1
  # exp_x <- distmin-(1/lambda)*log(1-r) # convert displacements to an exponential distribution with an xmin
  exp_x <- r
  # calculate angles
  set.seed(1)
  directedForwardAngle1 <- round(runif((ceiling((directedforward/2)*samplesize[i])),330,360),2)
  set.seed(1)
  directedForwardAngle2 <- round(runif((ceiling((directedforward/2)*samplesize[i])),0,30),2)
  set.seed(1)
  directedBackwardAngle <- round(runif((ceiling((directedbackward)*samplesize[i])),150,210),2)
  set.seed(1)
  randomAngle <- runif(ceiling((random*samplesize[i])),0,360)
  # combine angles in vector
  angles <- c(directedForwardAngle1,directedForwardAngle2,directedBackwardAngle,randomAngle)
  set.seed(1)
  # randomize the order of the angles, if  rounding during "calculate angles" caused too many angles
  # this will correct it
  angles <- sample(angles,samplesize[i])
  tracks <- rbind(tracks,c(data.frame("ref"=rep(i,samplesize[i]),"lon"=rep(startLonRange[i],samplesize[i]),"lat"=rep(startLatRange[i],samplesize[i]),
                                          "day"=rep(dates[i],samplesize[i]))))
  if (i==1){ # start position for dataframe
    startRow <- 1
  } else {
    startRow <- sum(samplesize[1:i-1])+1
  }

  j=1
  for (j in 1:(samplesize[i]-1)){ # for each new location in each track
    d <- exp_x[j] # random displacement from exp dist
    a <- angles[j]*(rad) # random angle from those created previously
    lat1 <- tracks$lat[startRow+j]*(rad) # convert current lat location to radians
    long1 <- tracks$lon[startRow+j]*(rad) # convert current long location to radians
    # calculate new lat in radians based on angle and distance
    lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a))
    # calculate new long in radians based on angle and distance
    long2 <- long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2))
    tracks$lon[startRow+j+1] <- long2/rad # convert to degrees and save in dataframe
    tracks$lat[startRow+j+1] <- lat2/rad # convert to degrees and save in dataframe
    # Advance time by 1 day
    tracks$day[startRow+j+1] <- tracks$day[startRow+j]+(24*60*60)
  }
}
tracks <- tracks[-1,] #remove starting location
row.names(tracks) <- 1:nrow(tracks) # rename rows 1:n

# Plot tracks
cols <- data.frame(
  col = rainbow(length(unique(tracks$ref))),
  id = unique(tracks$ref)
)
with(tracks, {
  plot(lon,lat,pch = 16, col = cols$col[match(ref, cols$id)], type = "p",
       ylim=c(min(lat)-0.25,max(lat)+0.25),xlim=c(min(lon)-0.25,max(lon)+0.25),cex=0.5,
       ylab="Latitude", xlab="Longitude")
  for (l in unique(ref)) {
    # print(cols$col[match(l, cols$id)])
    lines(lon[ref == l], lat[ref == l], type = "l", col = cols$col[match(l, cols$id)])
  }
})


rm(list=ls()[! ls() %in% c("tracks")]) # remove temporary vectors etc

##################################
## Set working directory and save
##################################
# setwd()
save(tracks, file = "tracks.RData")


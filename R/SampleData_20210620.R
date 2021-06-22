############################################################################################################################################################
############################################################################################################################################################

#Calculate random displacement lengths following a powerlaw dist
{
set.seed(1)
samplesize <- 1000
distmin <- 2.5e-1 #in km
alpha <- 3
r <- runif(samplesize,0,1) #uniform random number between 0-1
pl_x <- distmin*(1-r)^(-1/(alpha-1)) #Displacement values in km where x >= xmin
summary(pl_x)

angle <- runif(samplesize,0,360) #random angles between 0-360
PLsample<-data.frame("ref"=c(1),"lon"=c(rep(0,samplesize)),"lat"=c(rep(0,samplesize)),"day"=as.POSIXct("2015-01-01 09:30:31")) # 1 track
Radius <- 6371 #Earth Radius in km (disp are in km)
rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
for (i in 1:(samplesize-1)){
  d <- pl_x[i]
  a <- angle[i]*(rad)
  lat1 <- PLsample$lat[i]*(rad) # convert to radians
  long1 <- PLsample$lon[i]*(rad) # convert to radians
  lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a)) # Calculate new lat in radians based on angle and distance
  long2 <- long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2)) # Calculate new long in radians based on angle and distance
  PLsample$lon[i+1] <- long2/rad # convert to degrees and save
  PLsample$lat[i+1] <- lat2/rad # convert to degrees and save
  PLsample$day[i+1] <- PLsample$day[i]+(24*60*60) # previous day plus number of hours it would take the animal to travel that distance
}
plot(PLsample$lon,PLsample$lat,type="l", lty=1)
}
# head(PLsample)
summary(PLsample)
############################################################################################################################################################
############################################################################################################################################################

#Calculate random displacement lengths following an exp dist
{
set.seed(1)
samplesize <- 1000
lambda <- 2
distmin <- 2.5e-1 #in km
r <- runif(samplesize,0,1) #uniform random number between 0-1
exp_x <- distmin-(1/lambda)*log(1-r) ##Mean of exponential dist should equal 1/lambda when xmin isn't a factor. e.g., 1/mean(-(1/lambda)*log(1-r)) = lambda
summary(exp_x)
# min(exp_x)
# 1/mean(exp_x) #expected lambda after dist fit
# summary(-(1/lambda)*log(1-r)) #Clauset formula
# summary(rexp(samplesize,lambda)) #base r formula
angle <- runif(samplesize,0,360) #random angles between 0-360
Expsample<-data.frame("ref"=c(1),"lon"=c(rep(0,samplesize)),"lat"=c(rep(0,samplesize)),"day"=as.POSIXct("2015-01-01 09:30:31")) # 1 track

i<-1
Radius <- 6371 #Earth Radius in km (disp are in km)
rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
for (i in 1:(samplesize-1)){
  d <- exp_x[i]
  a <- angle[i]*(rad)
  lat1 <- Expsample$lat[i]*(rad) # convert to radians
  long1 <- Expsample$lon[i]*(rad) # convert to radians
  lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a)) # Calculate new lat in radians based on angle and distance
  long2 <- long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2)) # Calculate new long in radians based on angle and distance
  Expsample$lon[i+1] <- long2/rad # convert to degrees and save
  Expsample$lat[i+1] <- lat2/rad # convert to degrees and save
  Expsample$day[i+1] <- Expsample$day[i]+(24*60*60) # previous day plus number of hours it would take the animal to travel that distance
}
plot(Expsample$lon,Expsample$lat,type="l", lty=1)

}
############################################################################################################################################################
############################################################################################################################################################

#Calculate random displacement lengths following a lnorm dist
{
# https://msalganik.wordpress.com/2017/01/21/making-sense-of-the-rlnorm-function-in-r/

# endsample <-1000 ## the number of locations you want to end up with
# samplesize <- endsample+(endsample*0.5) ## calculate extra values so you can remove those <distmin

# max speed2m/s = 172.8

set.seed(1)
samplesize<-1000
sigma <- 2
r <- runif(samplesize,0,1) #uniform random number between 0-1
p = c()
for (n in 1:ceiling(samplesize/2)){
  p[n] = sqrt(-2*sigma^2*log(1-r[n]))
}

theta = c()
for (n in (ceiling(samplesize/2)+1):samplesize){
  theta[n] = 2*pi*r[n]
}
theta <-theta[!is.na(theta)]

lnorm_x <- c()
for (n in 1:ceiling(samplesize/2)){
  x1 = exp(p[n]*sin(theta[n]))
  x2 = exp(p[n]*cos(theta[n]))
  lnorm_x = c(lnorm_x, x1, x2)
}

# lnorm_x<-head(lnorm_x[lnorm_x>distmin],n=endsample) # to truncate at distmin -- causes "Error in if (dist > distmin) { : missing value where TRUE/FALSE needed"
length(lnorm_x)
min(lnorm_x)
summary(lnorm_x)

angle <- runif(samplesize,0,360) #random angles between 0-360
LNsample<-data.frame("ref"=c(1),"lon"=c(rep(0,samplesize)),"lat"=c(rep(0,samplesize)),"day"=as.POSIXct("2015-01-01 09:30:31")) # 1 track
Radius <- 6371 #Earth Radius in km (disp are in km)
rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
for (i in 1:(samplesize-1)){
  d <- lnorm_x[i]
  a <- angle[i]*(rad)
  lat1 <- LNsample$lat[i]*(rad) # convert to radians
  long1 <- LNsample$lon[i]*(rad) # convert to radians
  lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a)) # Calculate new lat in radians based on angle and distance
  long2 <- long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2)) # Calculate new long in radians based on angle and distance
  LNsample$lon[i+1] <- long2/rad # convert to degrees and save
  LNsample$lat[i+1] <- lat2/rad # convert to degrees and save
  LNsample$day[i+1] <- LNsample$day[i]+(24*60*60) # previous day plus number of hours it would take the animal to travel that distance
}
plot(LNsample$lon,LNsample$lat,type="l", lty=1)
} ##Clauset methods

{
set.seed(1)
samplesize <- 10000
distmin <- 2.5e-1 #in km
distmax <- 30 ##max of PL - to make these trio of datasets comparable it wont make sense is LN is traveling faster than PL
mu <- -0.81
sigma <- 1.23
lnorm_x <- EnvStats::rlnormTrunc(samplesize, mu, sigma, min = distmin, max = distmax)
summary(lnorm_x)

# m <- -0.81
# s <- 1.23
# location <- log(m^2 / sqrt(s^2 + m^2))
# shape <- sqrt(log(1 + (s^2 / m^2)))
# lnorm_x <- rlnorm(n=samplesize, location, shape)
# mean(lnorm_x)
# sd(lnorm_x)
# lnorm_x <- EnvStats::rlnormTrunc(samplesize, location, shape, min = distmin, max = Inf)

angle <- runif(samplesize,0,360) #random angles between 0-360
LNsample<-data.frame("ref"=c(1),"lon"=c(rep(0,samplesize)),"lat"=c(rep(0,samplesize)),"day"=as.POSIXct("2015-01-01 09:30:31")) # 1 track
i<-1
Radius <- 6371 #Earth Radius in km (disp are in km)
rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
for (i in 1:(samplesize-1)){
  d <- lnorm_x[i]
  a <- angle[i]*(rad)
  lat1 <- LNsample$lat[i]*(rad) # convert to radians
  long1 <- LNsample$lon[i]*(rad) # convert to radians
  lat2 <- asin(sin(lat1)*cos(d/Radius) + cos(lat1)*sin(d/Radius)*cos(a)) # Calculate new lat in radians based on angle and distance
  long2 <- long1 + atan2(sin(a)*sin(d/Radius)*cos(lat1),cos(d/Radius)-sin(lat1)*sin(lat2)) # Calculate new long in radians based on angle and distance
  LNsample$lon[i+1] <- long2/rad # convert to degrees and save
  LNsample$lat[i+1] <- lat2/rad # convert to degrees and save
  LNsample$day[i+1] <- LNsample$day[i]+(24*60*60) # previous day plus number of hours it would take the animal to travel that distance
}
plot(LNsample$lon,LNsample$lat,type="l", lty=1)
} ## rlnormTrunc and rlnorm methods

############################################################################################################################################################
############################################################################################################################################################

##TEST PHYSMOVE
CalculateDisplacements(LNsample, max_hr = 24)
summary(unlist(Displacements)) ## 2 m/s is 173 km/d
min(unlist(Displacements))
FitDist(Displacements, dist="lnorm", Normalize = FALSE)

# CompDist(Displacements)
# PlotDist(Displacements)
# CompDist(DistResults_AIC)

library(poweRlaw)
blackouts = read.table("blackouts.txt")$V1
x<-blackouts

##COMPARE TO POWERLAW PKG
library(poweRlaw)
x <- list()
for (d in 1:length(TimeWindows)){
  disp <- unlist(Displacements[d])
  x[[d]] <- disp/mean(disp)
}

# disp_pl<-conpl$new(unlist(x)) #create continuous  power law object
disp_pl<-conpl$new(x)
pl_xmin<-estimate_xmin(disp_pl)
disp_pl$setXmin(pl_xmin)
pl<-disp_pl
pl

disp_exp<-conexp$new(unlist(x)) #create continuous  power law object
exp_xmin<-estimate_xmin(disp_exp)
disp_exp$setXmin(exp_xmin)
e<-disp_exp

disp_ln<-conlnorm$new(unlist(x)) #create continuous  power law object
ln_xmin<-estimate_xmin(disp_ln)
disp_ln$setXmin(ln_xmin)
ln<-disp_ln

pl
e
ln

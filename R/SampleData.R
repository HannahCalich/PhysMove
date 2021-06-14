# Random number generator
# powerlaw
# x = xmin (1-r)^-1/(a-1)
# Exp
# x = xmin - (1/a)ln(1-r)


#Calculate random displacement lengths following a powerlaw
set.seed(1)
samplesize <- 1000
distmin <- 2.5e-1 #in km
alpha <- 3
r <- runif(samplesize,0,1) #uniform random number between 0-1
pl_x <- distmin*(1-r)^(-1/(alpha-1)) #Displacement values in km where x >= xmin
angle <- runif(samplesize,0,360) #random angles between 0-360
PLsample<-data.frame("ref"=c(1),"lon"=c(rep(0,samplesize)),"lat"=c(rep(0,samplesize)),"day"=as.POSIXct("2015-01-01 09:30:31")) # 1 track

i<-1
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

rm(samplesize)
rm(distmin)
rm(alpha)
rm(r)
rm(pl_x)
rm(angle)
rm(i)
rm(Radius)
rm(rad)
rm(a)
rm(d)
rm(lat1)
rm(lat2)
rm(long1)
rm(long2)

##TEST PHYSMOVE
CalculateDisplacements(PLsample, max_hr = 24)
FitDist(Displacements)
CompDist(DistResults_AIC)

##COMPARE TO POWERLAW PKG
library(poweRlaw)
x <- list()
for (d in 1:length(TimeWindows)){
  disp <- unlist(Displacements[d])
  x[[d]] <- disp/mean(disp)
}

disp_pl<-conpl$new(unlist(x)) #create continuous  power law object
pl_xmin<-estimate_xmin(disp_pl)
disp_pl$setXmin(pl_xmin)
disp_pars_pl<-estimate_pars(disp_pl)
disp_pl$setPars(disp_pars_pl)
m<-disp_pl
m

disp_exp<-conexp$new(unlist(x)) #create continuous  power law object
exp_xmin<-estimate_xmin(disp_exp)
disp_exp$setXmin(exp_xmin)
disp_pars_exp<-estimate_pars(disp_exp)
disp_exp$setPars(disp_pars_exp)
e<-disp_exp
e


m1 = conpl$new(test)
m1$setPars(estimate_pars(m1))
m1


# exp_x = distmin - (1/lambda)*log(1-r)
# plot(exp_x, log = "xy")



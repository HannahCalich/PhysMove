# Random number generator
# powerlaw
# x = xmin (1-r)^-1/(a-1)
# Exp
# x = xmin - (1/a)ln(1-r)


#Calculate random displacement lengths following a powerlaw
set.seed(1)
samplesize <- 100
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
CalculateDisplacements(sharksBull)
FitDist(Displacements, dist="exp")
FitDist(Displacements, dist="exp", set_xmin = 2.883285)
FitDist(Displacements, dist="exp", Full=TRUE)


CompDist(DistResults_AIC)

########################################################################
########################################################################
CalculateDisplacements(PLsample, max_hr=24)
FitDist(Displacements, dist="lnorm")

x <- list()
for (d in 1:length(TimeWindows)){
  disp <- unlist(Displacements[d])
  x[[d]] <- disp/mean(disp)
}

x<-unlist(x)
xmins <- sort(unique(x))
dat <- numeric(length(xmins))
x <- sort(x)
N <- length(x)

      pars.list = c()
      for (i in 1:(length(xmins)-1)){
        xmin <- xmins[i]
        xi <- x[x>xmin]
        n <- length(xi)
        theta_0 <- mean(xi)
        negloglike = function(par) {
          r = -conexp_tail_ll(xi, par, xmin)
          if (!is.finite(r))
            r = 1e+12
          r
        }
        conexp_tail_ll = function (x, rate, xmin){
          n = length(x)
          joint_prob = colSums(matrix(sapply(rate, function(i) dexp(x, i, log = TRUE)), nrow = length(x)))
          prob_over = sapply(rate, function(i) pexp(xmin, i, lower.tail = FALSE, log.p = TRUE))
          return(joint_prob - n * prob_over)
        }
        mle = stats4::mle(minuslogl = negloglike, start=theta_0, method = "L-BFGS-B", lower = 0)
        pars.list[i] = as.numeric(mle@coef[1])
        n <- length(x[x>=xmin])
        xi = x[(N-n+1):N] #identify truncated values to use with cdf. This includes xmin while earlier xi did not.
        fx = 1-exp(-pars.list[i]*(xi-xmin))
        fx[xi<xmin] = 0
        sx <- ((0:(n - 1))/n)[1:length(fx)]
        dat[i] <- max(abs(sx-fx), na.rm=TRUE) # max difference between fitted and empirical cdfs (KS test)
      }
      D <- min(dat[dat>0],na.rm=TRUE)
      Exp_xmin <- xmins[which(dat==D)]
      Exp_lambda <- pars.list[which(dat==D)]
      n <- length(x[x>Exp_xmin]) #length of truncated dataset
      D
      Exp_xmin
      Exp_lambda

##########################################################################################################################################################
##########################################################################################################################################################
### POWERLAW CODE:
library(poweRlaw)
  CalculateDisplacements(PLsample,max_hr=24)
  # CalculateDisplacements(sharksBull,max_hr=24)
  FitDist(Displacements)

  x <- list()
  for (d in 1:length(TimeWindows)){
    disp <- unlist(Displacements[d])
    x[[d]] <- disp/mean(disp)
  }

  disp_exp<-conexp$new(unlist(x)) #EXP
  exp_xmin<-estimate_xmin(disp_exp)
  disp_exp$setXmin(exp_xmin)
  disp_pars_exp<-estimate_pars(disp_exp)
  disp_exp$setPars(disp_pars_exp)
  e<-disp_exp
  # e
  print(paste("KS stat for xmin:",exp_xmin$gof))

  disp_ln<-conlnorm$new(unlist(x))#LNORM
  ln_xmin<-estimate_xmin(disp_ln)
  disp_ln$setXmin(ln_xmin)
  disp_pars_ln<-estimate_pars(disp_ln)
  disp_ln$setPars(disp_pars_ln)
  ln<-disp_ln
  # ln

  disp_pl<-conpl$new(unlist(x)) #PL
  pl_xmin<-estimate_xmin(disp_pl)
  disp_pl$setXmin(pl_xmin)
  disp_pars_pl<-estimate_pars(disp_pl)
  disp_pl$setPars(disp_pars_pl)
  pl<-disp_pl
  # pl

## Test if results match
  pl$xmin==DistResults$xmin[1]
  round(pl$pars,7)==round(DistResults$Parameter1[1],7) #different number of digits in results. PhysMove has more digits - this is fine.
  e$xmin==DistResults$xmin[2]
  round(e$pars,7)==round(DistResults$Parameter1[2],7)
  ln$xmin==DistResults$xmin[3]
  round(ln$pars[1],7)==round(DistResults$Parameter1[3],7)
  round(ln$pars[2],7)==round(DistResults$Parameter2[3],7)

  print(paste("poweRlaw xmin",ln$xmin))
  print(paste("PhysMove xmin",DistResults$xmin[3]))

  print(paste("poweRlaw par1",ln$pars[1]))
  print(paste("PhysMove par1",DistResults$Parameter1[3]))

  print(paste("poweRlaw par2",ln$pars[2]))
  print(paste("PhysMove par2",DistResults$Parameter2[3]))

  print(paste("poweRlaw ntail",ln$internal$n))
  print(paste("PhysMove ntail",DistResults$N_Tail[3]))

  print(paste("KS stat for xmin:",ln_xmin$gof))
##########################################################################################################################################################
##########################################################################################################################################################

################################## LOGNORMAL TROUBLESHOOTING ##################################

### To estimate xmin you need to make fitted and data cdfs and get the KS statistic.
### Once you've run poweRlaw pkg and saved dist_ln as m, run below to check the ccdfs match
### One impt note is that rounding messes everything up, so when instead of manually running PhysMove with xmin=###, run with xmin=xmins[#], by using position of the
### xmin in xmins and not the value itself you eliminate the infuriating rounding issues.
    m <- e
    q = m$dat
    n = m$internal[["n"]]
    N = length(q)
    q = q[(N - n + 1):N]
    length(q)
    # q = q[q <= xmax]
    fit_cdf = dist_cdf(m, q)
    # Note that the dist_cdf code is:
    # p = plnorm(q, pars[1], pars[2], lower.tail = lower_tail)
    # C = plnorm(xmin, pars[1], pars[2], lower.tail = FALSE)
    # pdf = (p/C - 1/C + 1)
    # cdf[q < xmin] = 0
    # all.equal(cdf, fx) # to make sure fit_cdf is same as fx from PhysMove
    all.equal(fit_cdf,fx) ## fx is from PhysMove
    data_cdf = ((0:(n - 1))/n)[seq_along(fit_cdf)]
    all.equal(data_cdf,sx) #sx is from PhysMove
    gof = max(abs(data_cdf - fit_cdf))
    max(abs(fit_cdf-data_cdf), na.rm = TRUE) #should match dat value from PhysMove - this is GOF score for KS test.
    all.equal(dat[i],max(abs(fit_cdf-data_cdf), na.rm = TRUE)) #GOF scores match

##########################################################################################################################################################
##########################################################################################################################################################

################################## EXPONENTIAL TROUBLESHOOTING ##################################

### To estimate xmin you need to make fitted and data cdfs and get the KS statistic.
### Once you've run poweRlaw pkg and saved dist_exp as m, run below to check the ccdfs match
### One impt note is that rounding messes everything up, so when instead of manually running PhysMove with xmin=###, run with xmin=xmins[#], by using position of the
### xmin in xmins and not the value itself you eliminate the infuriating rounding issues.
    m<-e
    q = m$dat
    n = m$internal[["n"]]
    N = length(q)
    q = q[(N - n + 1):N]
    q = q[q <= xmax]
    fit_cdf = dist_cdf(m, q)
    # Note that the dist_cdf code is:
    # p = pexp(q, pars, lower.tail = TRUE)  ## very minor difference between cum_prob and p is rounding of pars value (diff =  2.273868e-08)
    # C = pexp(xmin, pars, lower.tail = FALSE)
    # cdf = (p/C - 1/C + 1)
    # cdf[q < xmin] = 0
    # all.equal(cdf, fx) # to make sure fit_cdf is same as fx from PhysMove
    all.equal(fit_cdf,fx) ## fx is from PhysMove
    data_cdf = ((0:(n - 1))/n)[seq_along(fit_cdf)]
    all.equal(data_cdf,sx) #sx is from PhysMove
    max(abs(fit_cdf-data_cdf), na.rm = TRUE) #should match dat value from PhysMove - this is GOF score for KS test.

### To calculate lambda based on each xmin
### I had been using:
### pars.list[i] = n*(sum(xi-xmin))^-1 (which is the lambda MLE equation)
### but it was slightly off at the ~6th decimal place, which was impacting the CDFS and the KS tests so I updated to the below
    ### One impt note is that rounding messes everything up, so when instead of manually running PhysMove with xmin=###, run with xmin=xmins[#], by using position of the
    ### xmin in xmins and not the value itself you eliminate the infuriating rounding issues (e.g., xmin <- xmins[321]).
    xi <- x[x>xmin]
    n <- length(xi)
    theta_0 = mean(xi)

    negloglike = function(par) {
      r = -conexp_tail_ll(xi, par, xmin)
      if (!is.finite(r))
        r = 1e+12
      r
    }
    conexp_tail_ll = function (x, rate, xmin){
      n = length(x)
      joint_prob = colSums(matrix(sapply(rate, function(i) dexp(x, i, log = TRUE)), nrow = length(x)))
      prob_over = sapply(rate, function(i) pexp(xmin, i, lower.tail = FALSE, log.p = TRUE))
      return(joint_prob - n * prob_over)
    }
    result = stats4::mle(minuslogl = negloglike, start=theta_0, method = "L-BFGS-B", lower = 0)
    result@coef[1]

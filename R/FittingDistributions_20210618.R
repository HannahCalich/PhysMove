#' Fitting Distributions to CDFs of Displacements
#'
#' This function allows you to identify the distribution that best-fits the pdf of the normalized displacements calculated using \code{\link{CalculateDisplacements}}.To begin, this function
#' normalizes all of the displacements by dividing each displacement by the mean displacement from it's corresponding time period and combines the results into a vector of Normalized values.
#' @param Displacements Input the output from \code{\link{CalculateDisplacements}} function. To fit distribution to all displacements input Displacements as is, else you can subset for temporal periods (e.g., Displacements[1]).
#' @param dist Continuous distributions to be fit. Possible values are power-law ("pl"), exponential ("exp), or lognormal ("lnorm") continuous distributions. Default is all three distributions (i.e., dist=c("pl","exp","lnorm")).
#' @param set_xmin To limit the fitted distribution to values above a specified xmin. Default set_xmin=NULL.
#' @param Full To fit the fitted distribution to the full range of displacement data. Default Full=FALSE.Fit
#' @param AIC Calculate the weighted AIC values of the distribution fits to identify the best-fit distribution for your data. Note that AIC values are only meaningful if you are comparing model fits over the same distribution of data. Default AIC=FALSE
#' @param Normalize Normalizes the displacement distances by dividing each displacement by the average displacement for that time period. Required if working with displacements calculated over multiple time windows.
#' @param Plot Plot the Complementary Cumulative Distribution Function for the displacements and add fit lines for each calculated distribution, if desired. Default is c(TRUE, TRUE).
#'
#' @return Dataframe with summary statistics for each distribution fit. N_Tail is the number of data points greater than or equal to current value of xmin
#' @examples CalculateDisplacements(species_df, dist=c("pl","exp"))
#' @examples CalculateDisplacements(species_df, dist=c("pl","exp","lnorm"), set_xmin=NULL, Full=FALSE, AIC=TRUE, Normalize=TRUE)
#' @export

FitDist <- function (Displacements, dist=c("pl","exp","lnorm"), set_xmin=NULL, Full=FALSE, Normalize=TRUE) {

  if ((!is.null(set_xmin)) && (Full==TRUE)){
    stop("To fit all data to Exponential (exp) or Lognormal (lnorm) distributions use Full=TRUE (this does not require an xmin), either supply an xmin for truncated disributions, or use Full=TRUE to fit the full distribution. Please also note that Powerlaw distributions can only be fit using the truncated procedures.")
  }

  if (("pl" %in% dist) && (Full==TRUE)){
    stop("Powerlaws can only be fit to truncated distributions, either remove pl from the distribution list, or make Full=FALSE")
  }

  if ((length(Displacements)>1) && (Normalize==FALSE)){
    stop("Data must be normalized for displacements from multiple time periods to be collated into 1 dataset")
  }

  if (exists("TimeWindows")==FALSE){
    stop("Please Calculate Displacements using the CalcDisp function prior to executing FitDist")
  }

  if (("pl" %in% dist|"exp" %in% dist|"lnorm" %in% dist)!=TRUE){
    stop("Distributions can only be fit to 'pl','exp', or 'lnorm' distributions")
  }

  if (Normalize){
    x <- list()
    for (d in 1:length(TimeWindows)){
      disp <- unlist(Displacements[d])
      x[[d]] <- disp/mean(disp)
    }
  } else {
    x <- unlist(Displacements)
  }

  x <- unlist(x)
  xmins <- sort(unique(x)) #possible xmin values
  dat <- numeric(length(xmins)) #blank vectors for D values
  x <- sort(x)
  N <- length(x)
  DistResults<-data.frame("Distribution"=dist, "xmin"= c(NA), "Parameter1"=c(NA), "Parameter2"=c(NA), "N_Tail"= c(NA)) #make sure dist= is loaded
  AIC_Scores<-c()

  assign("dist",dist, envir = .GlobalEnv)
  assign("Normalize", Normalize, envir = .GlobalEnv)

  if ("pl" %in% dist){
    if (is.null(set_xmin)){
      for (i in 1:length(xmins)){
        xmin <- xmins[i]
        xi <- x[x>=(xmin)]
        n <- length(xi) #size of truncated data set
        a <- 1+n*((sum(log(xi/xmin)))^-1) #estimate alpha using direct MLE
        fx <- 1-((xi/xmin)^(-a+1)) #construct CCDF for fitted data
        fx[xi<round(xmin)] <- 0
        sx <- ((0:(n - 1))/n)[1:length(fx)] #CCDF for empirical data
        dat[i] <- max(abs(fx-sx))
      }
      D <- min(dat[dat>0], na.rm=TRUE) #find smallest D value
      PL_xmin <- xmins[which.max(dat==D)] #find corresponding xmin value such that PLxmin is the D value that minimizes the distance between sx and fx
    }
    if (!is.null(set_xmin)){
      PL_xmin <- set_xmin # If xmin is supplied, assign it as the PL_xmin
    }
    xi <- x[x>=(PL_xmin)] # Use PL_xmin to calculate final datasets and parameters
    n <- length(xi)
    PL_alpha <- 1+n*((sum(log(xi/PL_xmin)))^-1) # calculate alpha using direct MLE based on PL_xmin
    DistResults[which(DistResults$Distribution =="pl"),which(names(DistResults)=="xmin")]<-PL_xmin
    DistResults[which(DistResults$Distribution =="pl"),which(names(DistResults)=="Parameter1")]<-PL_alpha
    DistResults[which(DistResults$Distribution =="pl"),which(names(DistResults)=="N_Tail")]<-n
  }

  if ("exp" %in% dist){
    dat <- numeric(length(xmins)) #blank vectors for D values
    create_nll <- function(x){
      xi <- x[x>xmin]
      n <- length(xi)
      function(lambda){
        nll <- -(sum(dexp(xi, rate=lambda, log = TRUE)) - n*pexp(xmin, rate=lambda, log.p = TRUE, lower.tail = FALSE))
        if (!is.finite(nll)){
          nll <- 1e+12
        }
        nll
      }
    }
    if (Full==FALSE){
      if (is.null(set_xmin)){
        init <-c()
        xmin <- min(x)
        xi <- x[x>xmin] # truncate dataset at xmin
        n <- length(xi) #size of truncated data set
        pars = c(mean(xi)) # Initialize create_nll function with mean and sd of log xi
        my_nll <- create_nll(xi) # Calculate negative log likelihood
        mle = stats4::mle(minuslogl = my_nll, start=list(lambda=pars), method = "L-BFGS-B", lower = 0)
        init = c(as.numeric(mle@coef[1])) # Records parameters of fit
        pars.list = c()
        for (i in 1:(length(xmins)-length(pars)-1)){
          xmin <- xmins[i]
          xi <- x[x>xmin]
          n <- length(xi)
          my_nll <- create_nll(xi)
          mle = stats4::mle(minuslogl = my_nll, start=list(lambda=init), method = "L-BFGS-B", lower = 0)
          pars.list[i] = as.numeric(mle@coef[1])
          n <- length(x[x>=xmin])
          xi = x[(N-n+1):N] #identify truncated values to use with cdf. This includes xmin while earlier xi did not.
          fx = 1-exp(-pars.list[i]*(xi-xmin))
          fx[xi<xmin] = 0
          sx <- ((0:(n - 1))/n)[1:length(fx)]
          dat[i] <- max(abs(sx-fx), na.rm=TRUE) # max difference between fitted and empirical cdfs (KS test)
        }
        D <- min(dat[dat>0],na.rm=TRUE)
        row <- which.max(dat==D)
        Exp_xmin <- xmins[row]
        Exp_lambda <- pars.list[row]
        n <- length(x[x>=Exp_xmin]) #length of truncated dataset
      }
      if (!is.null(set_xmin)){
        xmin <- set_xmin
        xi <- x[x>xmin] # truncate dataset at xmin
        n <- length(x[x>=xmin]) #size of truncated data set
        my_nll <- create_nll(xi)
        mle = stats4::mle(minuslogl = my_nll, start=list(lambda=mean(xi)), method = "L-BFGS-B", lower = 0)
        Exp_lambda = as.numeric(mle@coef[1])
        Exp_xmin <- xmin
      }
    }
    if (Full==TRUE){
      xmin <- min(x)
      xi <- x
      n <- length(xi)
      my_nll <- create_nll(xi)
      mle = stats4::mle(minuslogl = my_nll, start=list(lambda=mean(xi)), method = "L-BFGS-B", lower = 0)
      Exp_lambda = as.numeric(mle@coef[1])
      Exp_xmin <- xmin
    }
    DistResults[which(DistResults$Distribution =="exp"),which(names(DistResults)=="xmin")]<-Exp_xmin
    DistResults[which(DistResults$Distribution =="exp"),which(names(DistResults)=="Parameter1")]<-Exp_lambda
    DistResults[which(DistResults$Distribution =="exp"),which(names(DistResults)=="N_Tail")]<-n
  }

  if ("lnorm" %in% dist){
    dat <- numeric(length(xmins)) #blank vectors for D values{
    create_nll <- function(x){
      xi <- x[x>xmin]
      n <- length(xi)
      function(mu, sigma) {
        nll <- -(sum(dlnorm(xi, mean=mu, sd=sigma, log=TRUE)) - n*plnorm(xmin, mean=mu, sd=sigma, log.p=TRUE, lower.tail=FALSE))
        if (!is.finite(nll)){
          nll <- 1e+12
        }
        nll
      }
    }
    if (Full==FALSE){
      if (is.null(set_xmin)){
        init <- matrix(ncol=2, nrow=1)# Initializing values with min of data
        xmin <- min(x)
        xi <- x[x>xmin] # truncate dataset at xmin
        n <- length(xi) #size of truncated data set
        pars = c(mean(log(xi)), sd(log(xi))) # Initialize create_nll function with mean and sd of log xi
        my_nll <- create_nll(xi) # Calculate negative log likelihood
        mle = stats4::mle(minuslogl = my_nll, start=list(mu=pars[1],sigma=pars[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
        init = c(as.numeric(mle@coef[1]), as.numeric(mle@coef[2])) # Records parameters of fit
        pars.mat <- matrix(ncol=2, nrow=(length(xmins)-1))
        for (i in 1:(length(xmins)-length(pars)-1)){ #-2 needed here because if xi <- x[x>xmin] results in an xi of 1 val you can't calc sd(log(xi)), and the NA throws errors. Also, a fit to the last two values would be meaningless. #poweRlaw code does this through "max_data_pt_needed"
          xmin <- xmins[i]
          xi <- x[x>xmin] # truncate dataset at xmin
          n <- length(xi) #size of truncated data set
          my_nll <- create_nll(xi) # Calculate negative log likelihood
          mle = stats4::mle(minuslogl = my_nll, start=list(mu=init[1],sigma=init[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
          pars.mat[i,] = c(as.numeric(mle@coef[1]), as.numeric(mle@coef[2])) # Records parameters of fit
          n <- length(x[x>=xmin]) #create cdf including xmin
          xi = x[(N-n+1):N] #identify truncated values to use with cdf
          fx = (plnorm(xi, pars.mat[i,1], pars.mat[i,2], lower.tail = TRUE)/plnorm(xmin, pars.mat[i,1], pars.mat[i,2], lower.tail = FALSE))-(1/plnorm(xmin, pars.mat[i,1], pars.mat[i,2], lower.tail = FALSE))+1
          fx[xi<xmin] = 0
          sx <- ((0:(n - 1))/n)[1:length(fx)] #CDF for empirical data
          dat[i] <- max(abs(sx-fx)) # max difference between fitted and empirical cdfs
        }
        D <- min(dat[dat>0], na.rm=TRUE) # find smallest D value
        row <- which.max(dat==D) #which.max is needed for cases where identical D values are calculated. In this case pick the highest row as it containes the most data.
        LN_xmin <- xmins[row] # find corresponding xmin value such that LN_xmin is the D value that minimizes the distance between sx and fx
        LN_mu <-  pars.mat[row,1] # determine parameters that correspond with LN_xmin
        LN_sigma <- pars.mat[row,2]
        n <- length(x[x>=LN_xmin]) # truncate dataset at xmin
      }
      if (!is.null(set_xmin)){
        xmin <- set_xmin
        xi <- x[x>xmin] # truncate dataset at xmin
        n <- length(xi) #size of truncated data set
        pars = c(mean(log(xi)), sd(log(xi))) # Initialize create_nll function with mean and sd of log xi
        my_nll <- create_nll(xi) # Calculate negative log likelihood
        mle = stats4::mle(minuslogl = my_nll, start=list(mu=pars[1],sigma=pars[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
        LN_xmin <- xmin
        LN_mu <- as.numeric(mle@coef[1])
        LN_sigma <-as.numeric(mle@coef[2])
        n <- length(x[x>=LN_xmin]) # truncate dataset at xmin
      }
    }
    if (Full==TRUE){
      xmin <- min(x)
      xi <- x[x>xmin] # truncate dataset at xmin
      n <- length(xi) #size of truncated data set
      pars = c(mean(log(xi)), sd(log(xi))) # Initialize create_nll function with mean and sd of log xi
      my_nll <- create_nll(xi) # Calculate negative log likelihood
      mle = stats4::mle(minuslogl = my_nll, start=list(mu=pars[1],sigma=pars[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
      LN_xmin <- xmin
      LN_mu <- as.numeric(mle@coef[1])
      LN_sigma <-as.numeric(mle@coef[2])
      n <- length(x[x>=LN_xmin]) # truncate dataset at xmin
    }
    DistResults[which(DistResults$Distribution =="lnorm"),which(names(DistResults)=="xmin")]<-LN_xmin
    DistResults[which(DistResults$Distribution =="lnorm"),which(names(DistResults)=="Parameter1")]<-LN_mu
    DistResults[which(DistResults$Distribution =="lnorm"),which(names(DistResults)=="Parameter2")]<-LN_sigma
    DistResults[which(DistResults$Distribution =="lnorm"),which(names(DistResults)=="N_Tail")]<-n
  }
  assign("DistResults",DistResults, envir = .GlobalEnv)
}

#' Fit distributions to displacements
#'
#' This function allows you to fit power law, exponential, or log-normal distributions to the displacements calculated with
#' the \code{\link{CalcDisp}} function. If displacements were calculated over multiple time windows this function will normalize the
#' displacements by dividing each displacement by the mean displacement of the corresponding time window
#' @param displacements List of displacements output from the \code{\link{CalcDisp}} function.
#' @param dist Continuous distributions that will be fit to the displacements. Possible values are power law ("pl"), exponential ("exp"), or log-normal ("lnorm")
#' continuous distributions. Default is dist=c("pl","exp","lnorm").
#' @param set_xmin To limit the fitted distribution to values above a specified value. Keep in mind that if your data were normalized
#' this value will have to be a normalized value as well. Default is NULL.
#' @param full To fit the distributions to the full range of displacement data. Default is FALSE.
#' @param normalize Normalizes the displacement distances by dividing each displacement by the average displacement for that time window
#' normalize=TRUE is required if working with displacements calculated over multiple time windows.
#' @return A data frame that contains the summary statistics for each distribution fit including the distribution name,
#' xmin (the x value used to fit the distribution), parameter 1 (alpha, lambda, mu) and parameter 2 (NA, NA, sigma) for pl, exp, and lnorm
#' distributions respectively, and nTail (the number of data points greater than or equal to xmin). A vector stating if the data were normalized or not,
#' ('normalize') is automatically assigned to the global environment as this information is needed for the \code{\link{CompDist}} and \code{\link{PlotDist}}
#'functions.
#' @examples FitDist(displacements)
#' @examples FitDist(displacements, dist=c("exp","lnorm"), full=TRUE)
#' @examples FitDist(displacements, dist=c("pl","exp","lnorm"), set_xmin=NULL, full=FALSE, normalize=TRUE)
#' @export

FitDist <- function (displacements, dist=c("pl","exp","lnorm"), set_xmin=NULL, full=FALSE, normalize=TRUE) {

  if (class(displacements)!="list"){
   stop("Distributions can only be fit to the output from the CalcDisp function.")
  }

  if ((!is.null(set_xmin)) & (full==TRUE)){
    stop("To fit distributions to the full range of data use full=TRUE and leave set_xmin as default (NULL).")
  }

  if ((length(displacements)>1) & (normalize==FALSE)){
    stop("Data must be normalized for displacements from multiple time windows to be collated into 1 dataset.")
  }

  if (("pl" %in% dist|"exp" %in% dist|"lnorm" %in% dist)!=TRUE){
    stop("Distributions can only be fit to 'pl','exp', or 'lnorm' distributions.")
  }

  if (normalize){
    x <- list()
    for (d in 1:length(displacements)){
      disp <- unlist(displacements[d])
      x[[d]] <- disp/mean(disp)
    }
  } else {
    x <- unlist(displacements)
  }

  x <- unlist(x)
  xmins <- sort(unique(x)) #possible xmin values
  dat <- numeric(length(xmins)) #blank vectors for D values
  x <- sort(x)
  N <- length(x)
  distResults <- data.frame("distribution"=dist, "xmin"= c(NA), "parameter1"=c(NA), "parameter2"=c(NA), "nTail"= c(NA)) #make sure dist= is loaded

  # assign("dist",dist, envir = .GlobalEnv)
  assign("normalize", normalize, envir = .GlobalEnv)

  if ("pl" %in% dist){
    message("Fitting a power law distribution")
    if (full==FALSE){
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
    }
    if (full==TRUE){
      PL_xmin <- min(x)
    }
    xi <- x[x>=(PL_xmin)] # Use PL_xmin to calculate final datasets and parameters
    n <- length(xi)
    PL_alpha <- 1+n*((sum(log(xi/PL_xmin)))^-1) # calculate alpha using direct MLE based on PL_xmin
    distResults[which(distResults$distribution =="pl"),which(names(distResults)=="xmin")] <- PL_xmin
    distResults[which(distResults$distribution =="pl"),which(names(distResults)=="parameter1")] <- PL_alpha
    distResults[which(distResults$distribution =="pl"),which(names(distResults)=="nTail")] <- n
  }

  if ("exp" %in% dist){
    message("Fitting an exponential distribution")
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
    if (full==FALSE){
      if (is.null(set_xmin)){
        init <- c()
        xmin <- min(x)
        xi <- x[x>xmin] # truncate dataset at xmin
        n <- length(xi) #size of truncated data set
        pars <- c(mean(xi)) # Initialize create_nll function with mean and sd of log xi
        my_nll <- create_nll(xi) # Calculate negative log likelihood
        mle <- stats4::mle(minuslogl = my_nll, start=list(lambda=pars), method = "L-BFGS-B", lower = 0)
        init <- c(as.numeric(mle@coef[1])) # Records parameters of fit
        pars.list <- c()
        for (i in 1:(length(xmins)-length(pars)-1)){
          xmin <- xmins[i]
          xi <- x[x>xmin]
          n <- length(xi)
          my_nll <- create_nll(xi)
          mle <- stats4::mle(minuslogl = my_nll, start=list(lambda=init), method = "L-BFGS-B", lower = 0)
          pars.list[i] <- as.numeric(mle@coef[1])
          n <- length(x[x>=xmin])
          xi <- x[(N-n+1):N] #identify truncated values to use with cdf. This includes xmin while earlier xi did not.
          fx <- 1-exp(-pars.list[i]*(xi-xmin))
          fx[xi<xmin] <- 0
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
        mle <- stats4::mle(minuslogl = my_nll, start=list(lambda=mean(xi)), method = "L-BFGS-B", lower = 0)
        Exp_lambda <- as.numeric(mle@coef[1])
        Exp_xmin <- xmin
      }
    }
    if (full==TRUE){
      xmin <- min(x)
      xi <- x
      n <- length(xi)
      my_nll <- create_nll(xi)
      mle <- stats4::mle(minuslogl = my_nll, start=list(lambda=mean(xi)), method = "L-BFGS-B", lower = 0)
      Exp_lambda <- as.numeric(mle@coef[1])
      Exp_xmin <- xmin
    }
    distResults[which(distResults$distribution =="exp"),which(names(distResults)=="xmin")] <- Exp_xmin
    distResults[which(distResults$distribution =="exp"),which(names(distResults)=="parameter1")] <- Exp_lambda
    distResults[which(distResults$distribution =="exp"),which(names(distResults)=="nTail")] <- n
  }

  if ("lnorm" %in% dist){
    message("Fitting a log-normal distribution")
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
    if (full==FALSE){
      if (is.null(set_xmin)){
        init <- matrix(ncol=2, nrow=1)# Initializing values with min of data
        xmin <- min(x)
        xi <- x[x>xmin] # truncate dataset at xmin
        n <- length(xi) #size of truncated data set
        pars <- c(mean(log(xi)), sd(log(xi))) # Initialize create_nll function with mean and sd of log xi
        my_nll <- create_nll(xi) # Calculate negative log likelihood
        mle <- stats4::mle(minuslogl = my_nll, start=list(mu=pars[1],sigma=pars[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
        init <- c(as.numeric(mle@coef[1]), as.numeric(mle@coef[2])) # Records parameters of fit
        pars.mat <- matrix(ncol=2, nrow=(length(xmins)-1))
        for (i in 1:(length(xmins)-length(pars)-1)){ #-2 needed here because if xi <- x[x>xmin] results in an xi of 1 val you can't calc sd(log(xi)), and the NA throws errors. Also, a fit to the last two values would be meaningless. #poweRlaw code does this through "max_data_pt_needed"
          xmin <- xmins[i]
          xi <- x[x>xmin] # truncate dataset at xmin
          n <- length(xi) #size of truncated data set
          my_nll <- create_nll(xi) # Calculate negative log likelihood
          mle <- stats4::mle(minuslogl = my_nll, start=list(mu=init[1],sigma=init[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
          pars.mat[i,] = c(as.numeric(mle@coef[1]), as.numeric(mle@coef[2])) # Records parameters of fit
          n <- length(x[x>=xmin]) #create cdf including xmin
          xi <- x[(N-n+1):N] #identify truncated values to use with cdf
          fx <- (plnorm(xi, pars.mat[i,1], pars.mat[i,2], lower.tail = TRUE)/plnorm(xmin, pars.mat[i,1], pars.mat[i,2], lower.tail = FALSE))-(1/plnorm(xmin, pars.mat[i,1], pars.mat[i,2], lower.tail = FALSE))+1
          fx[xi<xmin] = 0
          sx <- ((0:(n - 1))/n)[1:length(fx)] #CDF for empirical data
          dat[i] <- max(abs(sx-fx)) # max difference between fitted and empirical cdfs
        }
        D <- min(dat[dat>0], na.rm=TRUE) # find smallest D value
        row <- which.max(dat==D) #which.max is needed for cases where identical D values are calculated. In this case pick the highest row as it containes the most data.
        LN_xmin <- xmins[row] # find corresponding xmin value such that LN_xmin is the D value that minimizes the distance between sx and fx
        LN_mu <- pars.mat[row,1] # determine parameters that correspond with LN_xmin
        LN_sigma <- pars.mat[row,2]
        n <- length(x[x>=LN_xmin]) # truncate dataset at xmin
      }
      if (!is.null(set_xmin)){
        xmin <- set_xmin
        xi <- x[x>xmin] # truncate dataset at xmin
        n <- length(xi) #size of truncated data set
        pars <- c(mean(log(xi)), sd(log(xi))) # Initialize create_nll function with mean and sd of log xi
        my_nll <- create_nll(xi) # Calculate negative log likelihood
        mle <- stats4::mle(minuslogl = my_nll, start=list(mu=pars[1],sigma=pars[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
        LN_xmin <- xmin
        LN_mu <- as.numeric(mle@coef[1])
        LN_sigma <- as.numeric(mle@coef[2])
        n <- length(x[x>=LN_xmin]) # truncate dataset at xmin
      }
    }
    if (full==TRUE){
      xmin <- min(x)
      xi <- x[x>xmin] # truncate dataset at xmin
      n <- length(xi) #size of truncated data set
      pars <- c(mean(log(xi)), sd(log(xi))) # Initialize create_nll function with mean and sd of log xi
      my_nll <- create_nll(xi) # Calculate negative log likelihood
      mle <- stats4::mle(minuslogl = my_nll, start=list(mu=pars[1],sigma=pars[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
      LN_xmin <- xmin
      LN_mu <- as.numeric(mle@coef[1])
      LN_sigma <-as.numeric(mle@coef[2])
      n <- length(x[x>=LN_xmin]) # truncate dataset at xmin
    }
    distResults[which(distResults$distribution =="lnorm"),which(names(distResults)=="xmin")] <- LN_xmin
    distResults[which(distResults$distribution =="lnorm"),which(names(distResults)=="parameter1")] <- LN_mu
    distResults[which(distResults$distribution =="lnorm"),which(names(distResults)=="parameter2")] <- LN_sigma
    distResults[which(distResults$distribution =="lnorm"),which(names(distResults)=="nTail")] <- n
  }
  return(distResults)
}

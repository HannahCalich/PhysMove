#' Fit distributions to displacements
#'
#' This function allows you to fit power law, exponential, or lognormal distributions to the displacements calculated with
#' the \code{\link{CalcDisp}} function.
#'
#' Examples:
#' FitDist(displacements)
#' FitDist(displacements, dist=c("pl","exp","lnorm"), set_dmin=NULL, full=FALSE, normalise=TRUE)
#'
#' @param displacements List of displacements output from the \code{\link{CalcDisp}} function.
#' @param dist Continuous distributions that will be fit to the displacements. Possible values are power law ("pl"), exponential ("exp"), or log-normal ("lnorm")
#' continuous distributions. Default is dist=c("pl","exp","lnorm").
#' @param set_dmin To limit the fitted distribution to values above a specified value. If your displacements are going to be normalised
#' this value will have to be a normalised value as well. Default is NULL.
#' @param full To fit the distributions to the full range of displacement data. Default is FALSE.
#' @param normalise Normalises the displacement distances by dividing each displacement by the average displacement for that time window;
#' normalise=TRUE is required if working with displacements calculated over multiple time windows.
#' @return A list including a dataframe of summary statistics for each distribution fit (1st list element). Results dataframe includes the
#' distribution name, dmin (d values used to fit each distribution), parameter 1 (alpha, lambda, mu) and parameter 2 (NA, NA, sigma) for pl, exp, and lnorm
#' distributions respectively, and nTail (the number of data points greater than or equal to dmin). A logical argument indicating if
#' data were normalised is exported as the 2nd list element because this information is needed for the \code{\link{CompDist}} and \code{\link{PlotDist}}
#' functions.
#' @importFrom stats dlnorm plnorm dexp pexp
#' @export

FitDist <- function (displacements, dist=c("pl","exp","lnorm"), set_dmin=NULL, full=FALSE, normalise=TRUE) {

  if (!("list" %in% is(displacements))){
   stop("Distributions can only be fit to the output from the CalcDisp function in list format.")
  }

  if ((!is.null(set_dmin)) & (full==TRUE)){
    stop("To fit distributions to the full range of data use full=TRUE and leave set_dmin as default (NULL).")
  }

  if ((length(displacements)>1) & (normalise==FALSE)){
    stop("Data must be normalised for displacements from multiple time windows to be collated into 1 dataset.")
  }

  if (("pl" %in% dist|"exp" %in% dist|"lnorm" %in% dist)!=TRUE){
    stop("Distributions can only be fit to 'pl','exp', or 'lnorm' distributions.")
  }

  if (normalise){
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
  distResults <- data.frame("distribution"=dist, "dmin"= c(NA), "parameter1"=c(NA), "parameter2"=c(NA), "nTail"= c(NA)) #make sure dist= is loaded

  if ("pl" %in% dist){
    message("Fitting a power law distribution")
    if (full==FALSE){
      if (is.null(set_dmin)){
        for (i in 1:length(xmins)){
          xmin <- xmins[i]
          xi <- x[x>=(xmin)]
          n <- length(xi) #size of truncated data set
          a <- 1+n*((sum(log(xi/xmin)))^-1) #estimate alpha using direct MLE
          fx <- 1-((xi/xmin)^(-a+1)) #construct CCDF for fitted data
          fx[xi<round(xmin)] <- 0
          sx <- ((0:(n - 1))/n)[1:length(fx)] #complementary empirical CDF
          dat[i] <- max(abs(fx-sx))
        }
        D <- min(dat[dat>0], na.rm=TRUE) #find smallest D value
        PL_xmin <- xmins[which.max(dat==D)] #find corresponding xmin value such that PLxmin is the D value that minimizes the distance between sx and fx
      }
      if (!is.null(set_dmin)){
        PL_xmin <- set_dmin # If xmin is supplied, assign it as the PL_xmin
      }
    }
    if (full==TRUE){
      PL_xmin <- min(x)
    }
    xi <- x[x>=(PL_xmin)] # Use PL_xmin to calculate final datasets and parameters
    n <- length(xi)
    PL_alpha <- 1+n*((sum(log(xi/PL_xmin)))^-1) # calculate alpha using direct MLE based on PL_xmin
    distResults[which(distResults$distribution =="pl"),which(names(distResults)=="dmin")] <- PL_xmin
    distResults[which(distResults$distribution =="pl"),which(names(distResults)=="parameter1")] <- PL_alpha
    distResults[which(distResults$distribution =="pl"),which(names(distResults)=="nTail")] <- n
  }

  if ("exp" %in% dist){
    message("Fitting an exponential distribution")
    dat <- numeric(length(xmins)) #blank vectors for D values

    create_nll <- function(x){
      n <- length(x)
      function(lambda){
        nll <- -(sum(dexp(x, rate=lambda, log=TRUE)) - n*pexp(xmin, rate=lambda, log.p=TRUE, lower.tail=FALSE))
        if (!is.finite(nll)){
          nll <- 1e+12
        }
        nll
      }
    }

    if (full==FALSE){
      if (is.null(set_dmin)){

        xmin <- min(x)
        xi <- x[x>xmin] # truncate dataset at xmin
        n <- length(xi)
        pars <- c(mean(xi)) # Initialize create_nll function with mean and sd of log xi

        # my_nll <- create_nll(xi) # Calculate negative log likelihood
        # mle = suppressWarnings(optim(par = pars, fn = my_nll, method = "L-BFGS-B", lower = 0))
        # init <- mle$par

        rev.index <- rev(seq_along(x))
        pars.list <- c()
        for (i in 1:(length(xmins)-length(pars)-1)){ # need at least number of pars + 1 to fit
          xmin <- xmins[i]
          xi <- x[x>xmin]
          n <- length(xi)

          # my_nll <- create_nll(xi)
          # mle = suppressWarnings(optim(par = init, fn = my_nll, method = "L-BFGS-B", lower = 0))
          # pars.list[i] <- mle$par

          pars.list[i] <- n*(sum(xi-xmin)^-1) # from doi: 10.1038/nature09116

          selection = min(which(x >= (xmin - .Machine$double.eps ^ 0.5))) ## to account for decimal place issue with selection
          n <- rev.index[selection]
          xi <- x[(N-n+1):N]

          # # same as fx calc below
          # p = pexp(xi, pars.list[i], lower.tail = TRUE)
          # C = pexp(xmin, pars.list[i], lower.tail = FALSE)
          # fx1 = (p/C - 1/C + 1)
          # fx1[xi<xmin] <- 0

          fx <- 1-exp(-pars.list[i]*(xi-xmin)) ## original
          fx[xi<xmin] <- 0 ## original

          sx <- ((0:(n - 1))/n)[1:length(fx)] #complementary empirical CDF ## original
          dat[i] <- max(abs(sx-fx), na.rm=TRUE) # max difference between fitted and empirical cdfs (KS test) ## original
        }
        D <- min(dat[dat>0],na.rm=TRUE)
        row <- which.max(dat==D)
        Exp_xmin <- xmins[row]
        Exp_lambda <- pars.list[row]
        n <- length(x[x>=Exp_xmin]) #length of truncated dataset
      }

      if (!is.null(set_dmin)){
        xmin <- set_dmin
        xi <- x[x>xmin] # truncate dataset at xmin
        n <- length(xi)

        # my_nll <- create_nll(xi)
        # mle = suppressWarnings(optim(par = mean(xi), fn = my_nll, method = "L-BFGS-B", lower = 0))
        # Exp_lambda <- mle$par

        Exp_lambda <- n*(sum(xi-xmin)^-1) # from doi: 10.1038/nature09116
        Exp_xmin <- xmin
      }
    }
    if (full==TRUE){
      xmin <- min(x)
      xi <- x
      n <- length(xi)

      my_nll <- create_nll(xi)
      mle = suppressWarnings(optim(par = mean(xi), fn = my_nll, method = "L-BFGS-B", lower = 0))
      Exp_lambda <- mle$par

      # Exp_lambda <- n*(sum(xi-xmin)^-1) # from doi: 10.1038/nature09116

      Exp_xmin <- xmin
    }
    distResults[which(distResults$distribution =="exp"),which(names(distResults)=="dmin")] <- Exp_xmin
    distResults[which(distResults$distribution =="exp"),which(names(distResults)=="parameter1")] <- Exp_lambda
    distResults[which(distResults$distribution =="exp"),which(names(distResults)=="nTail")] <- n
  }

  if ("lnorm" %in% dist){
    message("Fitting a lognormal distribution")
    dat <- numeric(length(xmins)) #blank vectors for D values

    create_nll <- function(x){
      n <- length(x)
      function(param) {
        nll <- -(sum(dlnorm(x, meanlog=param[1], sdlog=param[2], log=TRUE)) - n*plnorm(xmin, meanlog=param[1], sdlog=param[2], log.p=TRUE, lower.tail=FALSE))
        if (!is.finite(nll)){
          nll <- 1e+12
        }
        nll
      }
    } # updated

    if (full==FALSE){
      if (is.null(set_dmin)){
        init <- matrix(ncol=2, nrow=1)# Initializing values with min of data

        xmin <- min(x)
        xi <- x[x>xmin] # truncate dataset at xmin
        n <- length(xi) #size of truncated data set

        pars <- c(mean(log(xi)), stats::sd(log(xi))) # Initialize create_nll function with mean and sd of log xi
        my_nll <- create_nll(xi) # Calculate negative log likelihood

        # mle <- stats4::mle(minuslogl = my_nll, start=list(mu=pars[1],sigma=pars[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
        # init <- c(as.numeric(mle@coef[1]), as.numeric(mle@coef[2])) # Records parameters of fit

        mle <- suppressWarnings(optim(par=pars, fn=my_nll, method="L-BFGS-B", lower=c(-Inf, .Machine$double.eps)))
        init <- c(mle$par[1],mle$par[2])

        rev.index <- rev(seq_along(x))
        pars.mat <- matrix(ncol=2, nrow=(length(xmins)-1))
        for (i in 1:(length(xmins)-length(pars)-1)){ # need at least number of pars + 1 to fit
          xmin <- xmins[i]
          xi <- x[x>xmin] # truncate dataset at xmin
          n <- length(xi) #size of truncated data set

          my_nll <- create_nll(xi) # Calculate negative log likelihood
          # mle <- stats4::mle(minuslogl = my_nll, start=list(mu=init[1],sigma=init[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
          # pars.mat[i,] = c(as.numeric(mle@coef[1]), as.numeric(mle@coef[2])) # Records parameters of fit

          mle <- suppressWarnings(optim(par=init, fn=my_nll, method="L-BFGS-B", lower=c(-Inf, .Machine$double.eps)))
          pars.mat[i,] <- c(mle$par[1],mle$par[2])

          selection = min(which(x >= (xmin - .Machine$double.eps ^ 0.5))) ## to account for decimal place issue with selection
          n <- rev.index[selection]
          xi <- x[(N-n+1):N]

          lnormCDF <- plnorm(xmin, pars.mat[i,1], pars.mat[i,2], lower.tail = FALSE)
          fx <- (plnorm(xi, pars.mat[i,1], pars.mat[i,2], lower.tail = TRUE)/lnormCDF)-(1/lnormCDF)+1
          fx[xi<xmin] = 0

          sx <- ((0:(n - 1))/n)[1:length(fx)] #complementary empirical CDF
          dat[i] <- max(abs(sx-fx)) # max difference between fitted and empirical cdfs
        }
        D <- min(dat[dat>0], na.rm=TRUE) # find smallest D value
        row <- which.max(dat==D) #which.max is needed for cases where identical D values are calculated. In this case pick the highest row as it containes the most data.
        LN_xmin <- xmins[row] # find corresponding xmin value such that LN_xmin is the D value that minimizes the distance between sx and fx
        LN_mu <- pars.mat[row,1] # determine parameters that correspond with LN_xmin
        LN_sigma <- pars.mat[row,2]
        n <- length(x[x>=LN_xmin]) # truncate dataset at xmin
      }

      if (!is.null(set_dmin)){
        xmin <- set_dmin
        xi <- x[x>xmin] # truncate dataset at xmin
        n <- length(xi) #size of truncated data set
        pars <- c(mean(log(xi)), stats::sd(log(xi))) # Initialize create_nll function with mean and sd of log xi

        my_nll <- create_nll(xi) # Calculate negative log likelihood
        mle <- suppressWarnings(optim(par=pars, fn=my_nll, method="L-BFGS-B", lower=c(-Inf, .Machine$double.eps)))
        # mle <- stats4::mle(minuslogl = my_nll, start=list(mu=pars[1],sigma=pars[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
        # LN_mu <- as.numeric(mle@coef[1])
        # LN_sigma <- as.numeric(mle@coef[2])

        LN_mu <- mle$par[1]
        LN_sigma <- mle$par[2]
        LN_xmin <- xmin
        n <- length(x[x>=LN_xmin]) # truncate dataset at xmin
      }
    }
    if (full==TRUE){
      xmin <- min(x)
      xi <- x[x>xmin] # truncate dataset at xmin
      n <- length(xi) #size of truncated data set
      pars <- c(mean(log(xi)), stats::sd(log(xi))) # Initialize create_nll function with mean and sd of log xi
      my_nll <- create_nll(xi) # Calculate negative log likelihood
      mle <- suppressWarnings(optim(par=pars, fn=my_nll, method="L-BFGS-B", lower=c(-Inf, .Machine$double.eps)))
      # mle <- stats4::mle(minuslogl = my_nll, start=list(mu=pars[1],sigma=pars[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))
      # LN_mu <- as.numeric(mle@coef[1])
      # LN_sigma <- as.numeric(mle@coef[2])
      LN_mu <- mle$par[1]
      LN_sigma <- mle$par[2]
      LN_xmin <- xmin
      n <- length(x[x>=LN_xmin]) # truncate dataset at xmin
    }
    distResults[which(distResults$distribution =="lnorm"),which(names(distResults)=="dmin")] <- LN_xmin
    distResults[which(distResults$distribution =="lnorm"),which(names(distResults)=="parameter1")] <- LN_mu
    distResults[which(distResults$distribution =="lnorm"),which(names(distResults)=="parameter2")] <- LN_sigma
    distResults[which(distResults$distribution =="lnorm"),which(names(distResults)=="nTail")] <- n
  }
  distResults <- list(distResults, normalise)
  names(distResults) <- c("distResults", "normalise")
  return(distResults)
}

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
#' @return Dataframe with summary statistics for each distribution fit.
#' @examples CalculateDisplacements(species_df, dist=c("pl","exp"))
#' @examples CalculateDisplacements(species_df, dist=c("pl","exp","lnorm"), set_xmin=NULL, Full=FALSE, AIC=TRUE, Normalize=TRUE)
#' @export

FitDist <- function (Displacements, dist=c("pl","exp","lnorm"), set_xmin=NULL, Full=FALSE, Normalize=TRUE) {

  if ((!is.null(set_xmin)) && (Full==TRUE)){
    stop("To fit all data to Exponential (exp) or Lognormal (lnorm) distributions use Full=TRUE (this does not require an xmin), either supply an xmin for truncated disributions, or use Full=TRUE to fit the full distribution. Please also note that Powerlaw distributions can only be fit using the truncated procedures.")
  }

  # if (("pl" %in% dist) && (Full==TRUE)){
    # stop("Powerlaws can only be fit to truncated distributions, either remove pl from the distribution list, or make Full=FALSE")
  # }

  if ((length(Displacements)>1) && (Normalize==FALSE)){
    stop("Data must be normalized for displacements from multiple time periods to be collated into 1 dataset")
  }

  if (exists("TimeWindows")==FALSE){
    stop("Please Calculate Displacements using the CalcDisp function prior to executing FitDist")
  }

  if (Normalize){
    x <- list()
    for (d in 1:length(TimeWindows)){
      disp <- unlist(Displacements[d])
      x[[d]] <- disp/mean(disp)
    }
  }
  else {
  x <- unlist(Displacements)
  }

  x <- round(unlist(x), digits=8) # To limit discrepancies with floating numbers
  xmins <- sort(unique(x)) #possible xmin values
  dat <- numeric(length(xmins)) #blank vectors for D values
  x <- sort(x)
  N <- length(x)
  DistResults<-data.frame("Distribution"=dist, "xmin"= c(NA), "Parameter1"=c(NA), "Parameter2"=c(NA), "N_Tail"= c(NA))
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
      PL_xmin <- xmins[which(dat==D)] #find corresponding xmin value such that PLxmin is the D value that minimizes the distance between sx and fx
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
    if (Full==FALSE){
      if (missing(set_xmin)){
        pars.list = c()
        for (i in 1:length(xmins)){
          xmin <- xmins[i]
          xi = x[x>xmin] #xi <- x[x>=(xmin)] #truncate full data set at xmin ##might need to do xi = x[x>xmin]
          n <- length(xi) #size of truncated data set
          pars.list[i] = n*(sum(xi-xmin))^-1
          n <- length(x[x>=xmin]) #create cdf including xmin
          xi = x[(N-n+1):N] #identify truncated values to use with cdf
          fx = 1-exp(-pars.list[i]*(x-xmin)) #CCDF for xmin only
          fx[x<xmin] = 0
          fx=fx[fx>0]
          sx <- ((0:(n - 1))/n)[1:length(fx[fx>0])]
          dat[i] <- max(abs(sx-fx)) # max difference between fitted and empirical cdfs
        }
        D <- min(dat[dat>0],na.rm=TRUE) #not exact same D (gof) as poweRlaw code but overall results are same. CDF in powerlaw code is finding slightly different values and I don't know why
        Exp_xmin <- xmins[which(dat==D)]
        Exp_lambda <- pars.list[which(dat==D)]
        n <- length(x[x>Exp_xmin]) # length of truncated dataset
      }
      if (!missing(set_xmin)){
        Exp_xmin <- set_xmin # If xmin is supplied, assign it as the LN_xmin
        xi <- x[x>Exp_xmin] # truncate dataset at xmin
        n <- length(xi) #size of truncated data set
        Exp_lambda <- n*(sum(xi-Exp_xmin))^-1
      }
    }
    if (Full==TRUE){
      n <- length(x)
      Exp_xmin <- min(x)
      Exp_lambda <- n*(sum(x-Exp_xmin))^-1
    }
    DistResults[which(DistResults$Distribution =="exp"),which(names(DistResults)=="xmin")]<-Exp_xmin
    DistResults[which(DistResults$Distribution =="exp"),which(names(DistResults)=="Parameter1")]<-Exp_lambda
    DistResults[which(DistResults$Distribution =="exp"),which(names(DistResults)=="N_Tail")]<-n
  }

  if ("lnorm" %in% dist){
    create_nll <- function(x){
        xi <- x[x>xmin]
        n <- length(xi)
        function(mu, sigma) {
          ll <- sum(dnorm(log(xi), mean=mu, sd=sigma, log=TRUE)) - n*pnorm(log(xmin), mean=mu, sd=sigma, log=TRUE, lower.tail=FALSE)
          -ll
        }
      }
    norm.cdf <- function(x){
        phi = 1/2*(1+VGAM::erf(((x-pars.mat[i,1])/(pars.mat[i,2]))/sqrt(2)))
        phi
      }
    if (Full==FALSE){
      if (missing(set_xmin)){
        pars.mat <- matrix(ncol=2, nrow=(length(xmins)-1))
        core_xmins = length(xmins)-floor(length(xmins)*0.1) #not processing on last 10% of xmins because sd of the xi values get too small which results in sd = NA which throws errors
        for (i in 1:core_xmins){
          xmin <- xmins[i]
          xi <- x[x>xmin] # truncate dataset at xmin
          n <- length(xi) #size of truncated data set
          my_nll <- create_nll(xi) # Calculate negative log likelihood
          pars = c(mean(log(xi)), sd(log(xi))) # Initialize create_nll function with mean and sd of log xi
          mle = stats4::mle(minuslogl = my_nll, start=list(mu=pars[1],sigma=pars[2]), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))#, upper = c(Inf, Inf)) #poweRlaw uses lower = c(-Inf, .Machine$double.eps), but we're using 0 for now
          pars.mat[i,] = c(as.numeric(mle@coef[1]), as.numeric(mle@coef[2])) # Records parameters of fit
          n <- length(x[x>=xmin]) #create cdf including xmin
          xi= x[(N-n+1):N] #identify truncated values to use with cdf
          fx = (norm.cdf(log(xi))-norm.cdf(log(xmin)))/(1-norm.cdf(log(xmin))) #lognormal CDF of fitted data
          fx[xi<xmin] = 0
          sx <- ((0:(n - 1))/n)[1:length(fx)] #CDF for empirical data
          dat[i] <- max(abs(sx-fx)) # max difference between fitted and empirical cdfs
        }
        D <- min(dat[dat>0], na.rm=TRUE) # find smallest D value
        LN_xmin <- xmins[which(dat==D)] # find corresponding xmin value such that LN_xmin is the D value that minimizes the distance between sx and fx
        n <- length(x[x>LN_xmin]) # truncate dataset at xmin
        LN_mu <-  pars.mat[which(dat==D),1] # determine parameters that correspond with LN_xmin
        LN_sigma <- pars.mat[which(dat==D),2]
      }
      if (!missing(set_xmin)){
        xmin <- set_xmin # If xmin is supplied, assign it as the LN_xmin
        xi <- x[x>=xmin] # truncate dataset at xmin
        n <- length(xi) # size of truncated data set
        my_nll <- create_nll(xi)
        mle = stats4::mle(minuslogl = my_nll, start=list(mu=mean(log(xi)),sigma=sd(log(xi))), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))#, upper = c(Inf, Inf)) #poweRlaw uses lower = c(-Inf, .Machine$double.eps), but we're using 0 for now
        LN_xmin <- xmin
        LN_mu <- as.numeric(mle@coef[1])
        LN_sigma <-as.numeric(mle@coef[2])
      }
    }
    if (Full==TRUE){
      n <- length(x)
      xmin <- min(x) # size of truncated data set
      my_nll <- create_nll(x)
      mle <- stats4::mle(minuslogl = my_nll, start=list(mu=mean(log(x)),sigma=sd(log(x))), method = "L-BFGS-B", lower = c(-Inf,.Machine$double.eps))#, upper = c(Inf, Inf)) #poweRlaw uses lower = c(-Inf, .Machine$double.eps), but we're using 0 for now
      LN_xmin <- xmin
      LN_mu <- as.numeric(mle@coef[1])
      LN_sigma <-as.numeric(mle@coef[2])
    }
    DistResults[which(DistResults$Distribution =="lnorm"),which(names(DistResults)=="xmin")]<-LN_xmin
    DistResults[which(DistResults$Distribution =="lnorm"),which(names(DistResults)=="Parameter1")]<-LN_mu
    DistResults[which(DistResults$Distribution =="lnorm"),which(names(DistResults)=="Parameter2")]<-LN_sigma
    DistResults[which(DistResults$Distribution =="lnorm"),which(names(DistResults)=="N_Tail")]<-n
  }
  assign("DistResults",DistResults, envir = .GlobalEnv)
}

#' Identify the best-fit distribution for displacement data
#'
#' This function allows you to determine if a power law, exponential, or log-normal distribution best-fits a probability density function
#' of the displacements using weighted Akaike Information Criterion (AIC). These fits use displacement data, xmin and parameter values that were
#' previously calculated with the \code{\link{CalcDisp}} and \code{\link{FitDist}} functions. By default, this function will calculate
#' AICc scores if n/K is <= 40 for the largest value of K, where n = sample size (nTail) and K = number of parameters in the model
#' (see Burnham and Anderson (2004) for further details, DOI: 10.1177/0049124104268644). However, if force_AICc = TRUE AICc scores will be
#' calculated regardless of n/K.
#' @param displacements List of displacements that was output from the \code{\link{CalcDisp}} function.
#' @param force_AICc Force function to calculate AICc scores instead of AIC scores when n/K is > 40. Default is FALSE.
#' @return An Updated "distResults" data frame with AICc (or AIC) scores and weighted AICc (or AIC) scores for each distribution fit.
#' @examples CompDist(displacements)
#' @examples CompDist(displacements, force_AICc=FALSE)
#' @export

CompDist <- function (displacements, force_AICc=FALSE){

  if (exists("displacements")==FALSE){
    stop("Please calculate displacements using the CalcDisp function and fit distriubtions using the FitDisp function prior to executing CompDist")
  }

  if (exists("distResults")==FALSE){
    stop("Please fit distributions using the FitDist function prior to executing CompDist")
  }

  if (normalize){
    x <- list()
    for (d in 1:length(timeWindows)){
      disp <- unlist(displacements[d])
      x[[d]] <- disp/mean(disp)
    }
    x <- unlist(x)
  } else {
  x <- unlist(displacements)
  }

  xmins <- sort(unique(x))
  x <- sort(x)
  n_all <- c()
  K_all <- c()

  if ("pl" %in% dist){
    MyPowerLaw <- function(parameters, displacements){
      pl_PDF = ((parameters[1]-1)/parameters[2])*((displacements/parameters[2])^(-parameters[1]))
      return(pl_PDF)
    }
    pl_xmin <- distResults[which(distResults$distribution=="pl"),"xmin"]
    pl_alpha <- distResults[which(distResults$distribution=="pl"),"parameter1"]
    n_all <- c(n_all,distResults[which(distResults$distribution=="pl"),"nTail"])
    pl_pdf <- MyPowerLaw(c(pl_alpha, pl_xmin), x)
    pl_pdf[x<pl_xmin] <- 0
    pl_logLik <- sum(log(pl_pdf[pl_pdf>0]))
    K_all <- c(K_all, 1)
  }

  if ("exp" %in% dist){
    MyexponentialTrunc <- function(parameters, displacements){
        exp_PDF = parameters[1]*exp(-parameters[1]*(displacements-parameters[2]))
        return(exp_PDF)
    }
    exp_xmin <- distResults[which(distResults$distribution=="exp"),"xmin"]
    exp_lambda <- distResults[which(distResults$distribution=="exp"),"parameter1"]
    n_all <- c(n_all,distResults[which(distResults$distribution=="exp"),"nTail"])
    exp_pdf <- MyexponentialTrunc(c(exp_lambda, exp_xmin), x)
    exp_pdf[x<exp_xmin] <- 0
    exp_logLik <- sum(log(exp_pdf[exp_pdf>0]))
    K_all <- c(K_all, 1)
  }

  if ("lnorm" %in% dist){
    MyLogNormalTrunc <- function(parameters, displacements){ # 1=mu, 2= sigma
      lnorm_PDF = exp(dlnorm(displacements, parameters[1], parameters[2], log = TRUE) - plnorm(parameters[3],parameters[1], parameters[2], lower.tail = FALSE, log.p = TRUE))
      return(lnorm_PDF)
    }
    lnorm_xmin <- distResults[which(distResults$distribution=="lnorm"),"xmin"]
    lnorm_mu <- distResults[which(distResults$distribution=="lnorm"),"parameter1"]
    lnorm_sigma <- distResults[which(distResults$distribution=="lnorm"),"parameter2"]
    n_all <- c(n_all,distResults[which(distResults$distribution=="lnorm"),"nTail"])
    lnorm_pdf <- MyLogNormalTrunc(c(lnorm_mu, lnorm_sigma, lnorm_xmin), x)
    lnorm_pdf[x<lnorm_xmin] <- 0
    lnorm_logLik <- sum(log(lnorm_pdf[lnorm_pdf>0]))
    K_all <- c(K_all,2)
  }
  if (n_all[which.max(K_all)]/max(K_all)>40 & force_AICc==FALSE){ # use AIC according to Burnham and Anderson (2004)
    if (length(unique(n_all))!=1){
      stop("The n/K ratio is > 40 and AIC values can be calculated, however, AIC values can only be compared
           over equal data ranges. Please re-run FitDist using the set_xmin parameter to fit each distribution to the same data range")
    }
    AIC_Scores <- c()
    distResults <- cbind(distResults,"AIC"=c(NA), "AICw"=c(NA))
    if ("pl" %in% dist){
      K<-K_all[which(dist=="pl")]
      pl_AIC <- -2*pl_logLik + 2*K
      AIC_Scores <- c(AIC_Scores,pl_AIC)
      distResults[which(distResults$distribution =="pl"),"AIC"] <- pl_AIC
    }
    if ("exp" %in% dist){
      K<-K_all[which(dist=="exp")]
      exp_AIC <- -2*exp_logLik + 2*K
      AIC_Scores <- c(AIC_Scores,exp_AIC)
      distResults[which(distResults$distribution =="exp"),"AIC"] <- exp_AIC
    }
    if ("lnorm" %in% dist){
      K<-K_all[which(dist=="lnorm")]
      lnorm_AIC <- -2*lnorm_logLik + 2*K
      AIC_Scores <- c(AIC_Scores,lnorm_AIC)
      distResults[which(distResults$distribution =="lnorm"),"AIC"] <- lnorm_AIC
    }
    rel_like <- exp(-1/2*((distResults$AIC)-min(distResults$AIC)))
    distResults$AICw <- rel_like/sum(rel_like)
  }
  else { # use AICc according to Burnham and Anderson (2004)
    AICc_Scores < -c()
    distResults <- cbind(distResults,"AICc"=c(NA), "AICcw"=c(NA))
    if ("pl" %in% dist){
      K <- K_all[which(dist=="pl")]
      n <- n_all[which(dist=="pl")]
      pl_AICc <- -2*pl_logLik + 2*K + ((2*K*(K+1))/(n-K-1))
      AICc_Scores <- c(AICc_Scores,pl_AICc)
      distResults[which(distResults$distribution =="pl"),"AICc"] <- pl_AICc
    }
    if ("exp" %in% dist){
      K <- K_all[which(dist=="exp")]
      n <- n_all[which(dist=="exp")]
      exp_AICc <- -2*exp_logLik + 2*K + ((2*K*(K+1))/(n-K-1))
      AICc_Scores <- c(AICc_Scores,exp_AICc)
      distResults[which(distResults$distribution =="exp"),"AICc"] <- exp_AICc
    }
    if ("lnorm" %in% dist){
      K <- K_all[which(dist=="lnorm")]
      n <- n_all[which(dist=="lnorm")]
      lnorm_AICc <- -2*lnorm_logLik + 2*K + ((2*K*(K+1))/(n-K-1))
      AICc_Scores <- c(AICc_Scores,lnorm_AICc)
      distResults[which(distResults$distribution =="lnorm"),"AICc"] <- lnorm_AICc
    }
    rel_like <- exp(-1/2*((distResults$AICc)-min(distResults$AICc)))
    distResults$AICcw <- rel_like/sum(rel_like)
    }
  assign("distResults", distResults, envir = .GlobalEnv)
}

#' Identify the best-fit distribution for displacement data using weighted Akaike information criterion
#'
#' This function allows you to identify the distribution that best-fits the pdf of the displacements, best-fit x-min, and parameters calculated using the \code{\link{CalculateDisplacements}} and \code{\link{FitDist}} functions. This function will automatically calculate AICc scores if n/K is <= 40 for the largest value of K, where n = sample size (N_Tail) and K = number of parameters in the model. See Burnham and Anderson (2004) for further details (DOI: 10.1177/0049124104268644).
#' @param Displacements Input the output from the \code{\link{CalculateDisplacements}} function. This function also requires outputs from the \code{\link{FitDist}} function
#' @param Force_AICc Force function to calculate AICc scores instead of AIC scores when n/K is > 40. Default is FALSE.
#' @return Updated "DistResults" data frame with summary statistics, AICc (or AIC) scores, and weighted AIC scores for each distribution fit.
#' @examples CompDist(Displacements)
#' @export

CompDist <- function (Displacements, force_AICc=FALSE){

  if (exists("Displacements")==FALSE){
    stop("Please calculate displacements using the CalcDisp function and fit distriubtions using the FitDisp function prior to executing CompDist")
  }

  if (exists("DistResults")==FALSE){
    stop("Please fit distributions using the FitDist function prior to executing CompDist")
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

  xmins <- sort(unique(x)) #possible xmin values
  x <- sort(x)
  n_all<-c()
  K_all<-c()

  if ("pl" %in% dist){
    MyPowerLaw <- function(parameters, Displacements){
      PL_PDF = ((parameters[1]-1)/parameters[2])*((Displacements/parameters[2])^(-parameters[1]))
      return(PL_PDF)
    }
    PL_xmin <- DistResults[which(DistResults$Distribution=="pl"),"xmin"]
    PL_alpha <- DistResults[which(DistResults$Distribution=="pl"),"Parameter1"]
    n_all <- c(n_all,DistResults[which(DistResults$Distribution=="pl"),"N_Tail"])
    PL_pdf <- MyPowerLaw(c(PL_alpha, PL_xmin), x)
    PL_pdf[x<PL_xmin] = 0
    PL_logLik<-sum(log(PL_pdf[PL_pdf>0]))
    K_all <- c(K_all, 1)
  }

  if ("exp" %in% dist){
    MyExponentialTrunc <- function(parameters, Displacements){
        Exp_PDF = parameters[1]*exp(-parameters[1]*(Displacements-parameters[2]))
        return(Exp_PDF)
    }
    Exp_xmin <- DistResults[which(DistResults$Distribution=="exp"),"xmin"]
    Exp_lambda <- DistResults[which(DistResults$Distribution=="exp"),"Parameter1"]
    n_all <- c(n_all,DistResults[which(DistResults$Distribution=="exp"),"N_Tail"])
    Exp_pdf <- MyExponentialTrunc(c(Exp_lambda, Exp_xmin), x)
    Exp_pdf[x<Exp_xmin] = 0
    Exp_logLik<-sum(log(Exp_pdf[Exp_pdf>0]))
    K_all <- c(K_all, 1)
  }

  if ("lnorm" %in% dist){
    MyLogNormalTrunc <- function(parameters, Displacements){ # 1=mu, 2= sigma
      LN_PDF = exp(dlnorm(Displacements, parameters[1], parameters[2], log = TRUE) - plnorm(parameters[3],parameters[1], parameters[2], lower.tail = FALSE, log.p = TRUE))
      return(LN_PDF)
    }
    LN_xmin <- DistResults[which(DistResults$Distribution=="lnorm"),"xmin"]
    LN_mu <- DistResults[which(DistResults$Distribution=="lnorm"),"Parameter1"]
    LN_sigma <- DistResults[which(DistResults$Distribution=="lnorm"),"Parameter2"]
    n_all <- c(n_all,DistResults[which(DistResults$Distribution=="lnorm"),"N_Tail"])
    LN_pdf <- MyLogNormalTrunc(c(LN_mu, LN_sigma, LN_xmin), x)
    LN_pdf[x<LN_xmin] = 0
    LN_logLik<-sum(log(LN_pdf[LN_pdf>0]))
    K_all <- c(K_all,2)
  }
  if (n_all[which.max(K_all)]/max(K_all)>40 & force_AICc==FALSE){ #use AIC according to Burnham and Anderson (2004)
    if (length(unique(n_all))!=1){
      stop("AIC values can only be compared over equal data ranges. Please re-run FitDist using the set xmin function to fit each distribution to the same data range")
    }
    AIC_Scores<-c()
    DistResults<-cbind(DistResults,"AIC"=c(NA), "AICw"=c(NA))
    if ("pl" %in% dist){
      K<-K_all[which(dist=="pl")]
      PL_AIC <- -2*PL_logLik + 2*K
      AIC_Scores<-c(AIC_Scores,PL_AIC)
      DistResults[which(DistResults$Distribution =="pl"),"AIC"]<-PL_AIC
    }
    if ("exp" %in% dist){
      K<-K_all[which(dist=="exp")]
      Exp_AIC <- -2*Exp_logLik + 2*K
      AIC_Scores<-c(AIC_Scores,Exp_AIC)
      DistResults[which(DistResults$Distribution =="exp"),"AIC"]<-Exp_AIC
    }
    if ("lnorm" %in% dist){
      K<-K_all[which(dist=="lnorm")]
      LN_AIC <- -2*LN_logLik + 2*K
      AIC_Scores<-c(AIC_Scores,LN_AIC)
      DistResults[which(DistResults$Distribution =="lnorm"),"AIC"]<-LN_AIC
    }
    rel_like <- exp(-1/2*((DistResults$AIC)-min(DistResults$AIC)))
  } else { #use AICc according to Burnham and Anderson (2004)
    AICc_Scores<-c()
    DistResults<-cbind(DistResults,"AICc"=c(NA), "AICw"=c(NA))
    if ("pl" %in% dist){
      K<-K_all[which(dist=="pl")]
      n<-n_all[which(dist=="pl")]
      PL_AICc <- -2*PL_logLik + 2*K + ((2*K*(K+1))/(n-K-1))
      AICc_Scores<-c(AICc_Scores,PL_AICc)
      DistResults[which(DistResults$Distribution =="pl"),"AICc"]<-PL_AICc
    }
    if ("exp" %in% dist){
      K<-K_all[which(dist=="exp")]
      n<-n_all[which(dist=="exp")]
      Exp_AICc <- -2*Exp_logLik + 2*K + ((2*K*(K+1))/(n-K-1))
      AICc_Scores<-c(AICc_Scores,Exp_AICc)
      DistResults[which(DistResults$Distribution =="exp"),"AICc"]<-Exp_AICc
    }
    if ("lnorm" %in% dist){
      K<-K_all[which(dist=="lnorm")]
      n<-n_all[which(dist=="lnorm")]
      LN_AICc <- -2*LN_logLik + 2*K + ((2*K*(K+1))/(n-K-1))
      AICc_Scores<-c(AICc_Scores,LN_AICc)
      DistResults[which(DistResults$Distribution =="lnorm"),"AICc"]<-LN_AICc
    }
    rel_like <- exp(-1/2*((DistResults$AICc)-min(DistResults$AICc)))
    }
  DistResults$AICw <- rel_like/sum(rel_like)
  assign("DistResults", DistResults, envir = .GlobalEnv)
}

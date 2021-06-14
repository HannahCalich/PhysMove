#' Identify the best-fit distribution for displacement data using weighted Akaike information criterion
#'
#' This function allows you to identify the distribution that best-fits the pdf of the normalized displacements calculated using \code{\link{CalculateDisplacements}}.
#' @param Displacements Input the output from \code{\link{CalculateDisplacements}} function. To fit distribution to all displacements input MyDisp as is, else you can subset for temporal periods (e.g., MyDisp[1]).
#' @return "DistResults_AIC" Dataframe with summary statistics and AIC scores for each distribution fit.
#' @examples CompDist(Displacements)
#' @export

CompDist <- function (Displacements){

  if (exists("Displacements")==FALSE){
    stop("Please calculate displacements using the CalcDisp function and fit distriubtions using the FitDisp function prior to executing PlotDist")
  }

  if (exists("DistResults")==FALSE){
    stop("Please fit distributions using the FitDist function prior to executing PlotDist")
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

  x <- round(unlist(x), digits=8) # To limit discrepancies with floating numbers
  # xmins <- sort(unique(x)) #possible xmin values
  # dat <- numeric(length(xmins)) #blank vectors for D values
  x <- sort(x)
  # N <- length(x)
  DistResults<-cbind(DistResults,"AIC"=c(NA), "AICw"=c(NA))
  AIC_Scores<-c()

  if ("pl" %in% dist){
    MyPowerLaw <- function(parameters, Displacements){
      PL_PDF = ((parameters[1]-1)/parameters[2])*((Displacements/parameters[2])^(-parameters[1]))
      return(PL_PDF)
    }
    PL_xmin <- DistResults[which(DistResults$Distribution=="pl"),"xmin"]
    PL_alpha <- DistResults[which(DistResults$Distribution=="pl"),"Parameter1"]
    PL_pdf <- MyPowerLaw(c(PL_alpha, PL_xmin), x)
    PL_pdf[x<PL_xmin] = 0
    logLik<-sum(log(PL_pdf[PL_pdf>0]))
    PL_AIC <- -2*logLik + 2*1
    AIC_Scores<-c(AIC_Scores,PL_AIC)
    DistResults[which(DistResults$Distribution =="pl"),"AIC"]<-PL_AIC
  }

  if ("exp" %in% dist){
    MyExponentialTrunc <- function(parameters, Displacements){
        Exp_PDF = parameters[1]*exp(-parameters[1]*(Displacements-parameters[2]))
        return(Exp_PDF)
    }
    Exp_xmin <- DistResults[which(DistResults$Distribution=="exp"),"xmin"]
    Exp_lambda <- DistResults[which(DistResults$Distribution=="exp"),"Parameter1"]
    Exp_pdf <- MyExponentialTrunc(c(Exp_lambda, Exp_xmin), x)
    Exp_pdf[x<Exp_xmin] = 0
    logLik<-sum(log(Exp_pdf[Exp_pdf>0]))
    Exp_AIC <- -2*logLik + 2*1
    AIC_Scores<-c(AIC_Scores,Exp_AIC)
    DistResults[which(DistResults$Distribution =="exp"),"AIC"]<-Exp_AIC
  }

  if ("lnorm" %in% dist){
    MyLogNormalTrunc <- function(parameters, Displacements){ # 1=mu, 2= sigma
      LN_PDF = exp(dlnorm(Displacements, parameters[1], parameters[2], log = TRUE) - plnorm(parameters[3],parameters[1], parameters[2], lower.tail = FALSE, log.p = TRUE))
      return(LN_PDF)
    }
    LN_xmin <- DistResults[which(DistResults$Distribution=="lnorm"),"xmin"]
    LN_mu <- DistResults[which(DistResults$Distribution=="lnorm"),"Parameter1"]
    LN_sigma <- DistResults[which(DistResults$Distribution=="lnorm"),"Parameter2"]
    LN_pdf <- MyLogNormalTrunc(c(LN_mu, LN_sigma, LN_xmin), x)
    LN_pdf[x<LN_xmin] = 0
    logLik<-sum(log(LN_pdf[LN_pdf>0]))
    LN_AIC <- -2*logLik + 2*1
    AIC_Scores<-c(AIC_Scores,LN_AIC)
    DistResults[which(DistResults$Distribution =="lnorm"),"AIC"]<-LN_AIC
    }

  rel_like <- exp(-1/2*((DistResults$AIC)-min(DistResults$AIC)))
  sum.rel.lik <- sum(rel_like)
  AICw <- rel_like/sum.rel.lik
  DistResults$AICw<-AICw
  assign("DistResults_AIC", DistResults, envir = .GlobalEnv)
}

#' Predictability of trajectories
#'
#' This function allows you to calculate the limit of predictability for each trajectory based on each individual's entropy. This function requires
#' 'indivEntropy', 'cellsVisited', and 'normalisedEntropy' from the \code{\link{Entropy}} function.
#' A pdf plot of the predictability values can be created with the \code{\link{PlotPDF}} function.
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{tracks}}.
#' @param entropyResults Data frame of results output from the \code{\link{Entropy}} function.
#' @param startVal Starting value used to find a root value for the limit of predictability equation. Function will loop through values
#' starting at startVal and decrease by 0.01 at each iteration until an acceptable root value is identified. Default is 0.99
#' @param histPlot Plot a histogram of the limit of predictability scores. Default is TRUE.
#' @return Limit of predictability values for each trajectory. If histPlot=TRUE a histogram of the limit of predictability scores is created.
#' @examples
#' Predictability(tracks, entropyResults)
#' Predictability(tracks, entropyResults, startVal=0.99, histPlot=TRUE)
#' @export

Predictability<-function(species_df, entropyResults, startVal=0.99, histPlot=TRUE){

  if (nrow(entropyResults)!=length(unique(species_df$ref))){
    stop("The number of individuals in the species_df does not match the number of normalised entropy values, check to ensure the right data has \n  been entered.")
  }

  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  Pred <- c()

  for (i in 1:length(species_index) ){
    if (entropyResults$cellsVisited[i]==1){
      warning(paste("Ref",unique(species_df$ref)[i],"only visited 1 cell so Pred scores cannot be calculated and NA is produced. NA values will be excluded from histPlot"), immediate. = TRUE)
      Pred[i] <- NA
      next
    }
    model <- function(x) c(F1 = x*log(x) + (1-x)*log(1-x) - (1-x)*log(entropyResults$cellsVisited[i]-1) + entropyResults$indivEntropy[i])

    if (startVal==0.99){
      ss <- suppressWarnings(rootSolve::multiroot(f = model, start = 0.99))#(1.01-entropyResults$normalisedEntropy[i]))) # +0.01 added to deal with cases where NormEnt = 1
      if (ss$root > 0 & ss$root < 1){
        Pred[i] <- ss$root
    } else {
        start_value_default <- 0.99
        repeat{
          ss <- suppressWarnings(rootSolve::multiroot(f = model, start = start_value_default))
          if (ss$root > 0 & ss$root < 1){
            break
          }
          else {
            startVal<-startVal-0.01
          }
        }
        Pred[i] <- ss$root
      }
    } else if (startVal!=0.99) {
      ss <- rootSolve::multiroot(f = model, start = startVal)
      Pred[i] <- ss$root
    }
  }

  predictabilityResults <- as.data.frame(cbind("ref"=unique(species_df$ref),"Predictability"=Pred))

  if (histPlot==TRUE){
    Predictability.plot <- as.data.frame(predictabilityResults[!is.na(predictabilityResults$Predictability),])
    h <- graphics::hist(Predictability.plot$Predictability, breaks=seq(0, 1, length.out = 21), plot=FALSE) # Determine hist values so you can automate plot better
    xlab <- c(0,"",0.2,"",0.4,"",0.6,"",0.8,"",1)
    hist_plot <- ggplot2::ggplot(Predictability.plot, ggplot2::aes(Predictability))+
      ggplot2::geom_histogram(breaks=h$breaks, color="black", fill="darkgrey")+
      ggplot2::scale_y_continuous(breaks=function(x) seq(ceiling(x[1]), floor(x[2]), by = 2))+
      ggplot2::scale_x_continuous("Limit of Predictability", breaks=seq(0,1,0.1), labels=c("0.0", "", "0.2", "", "0.4", "", "0.6", "", "0.8", "", "1.0"))+
      ggplot2::labs(y = "Frequency")+
      ggplot2::theme_classic(base_size = 18)#+
    # ggplot2::geom_vline(ggplot2::aes(xintercept=0.5, color="0.5"), linetype="dashed", size=1) +
    # ggplot2::scale_color_manual(name = "", values = c("0.5" = "red"))
    plot(hist_plot)
  }
  return(predictabilityResults)
}

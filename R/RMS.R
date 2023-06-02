#' Root-Mean-Square of Displacements
#'
#' This function allows you to calculate root-mean-square displacements and plot them scaled with time
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{tracks}}.
#' @param wBins Bin width refers to the size of the time bins used to calculate how frequently displacements occurred. Default is 1.1
#' @param timeUnit Unit used to calculate time between locations (e.g., "secs", "mins", "hours", "days", "weeks"). Default is "days".
#' @param plot Plot the root-mean-square of displacements versus the mean displacements against their corresponding time periods. Default is TRUE.
#' @param lm Calculate a linear regression to examine the relationship between the root-mean-square displacement values (target variable)
#' and time (predictor variable) and add fit line to the plot (if plot=TRUE). The slope of this relationship, which is also known as the Hurst or
#' scaling exponent) is the RMS statistic and it can be determined by typing 'RMSlinearModel$coefficients[2]'. Default is TRUE.
#' @return List containing a dataframe of results (list element 1) and if lm = TRUE, the results of the linear model are also output (list element 2).
#' The results dataframe includes the 'timeWindows' in log-sized bins along with their corresponding 'meanDisplacements', and root-mean-square
#' displacement 'rmsDisplacements' values. If plot = TRUE, a plot of the mean displacement values and the root-mean-square displacement values
#' against their corresponding time period is created, and if lm = TRUE, a fit line and reference line are added to the plot.
#' @examples
#' RMS(tracks)
#' RMS(tracks, timeUnit="days", wBins=1.1, plot=TRUE, lm=TRUE)
#' @export

RMS <- function (species_df, timeUnit="days", wBins=1.1, plot=TRUE, lm=TRUE){

  MydistHaversine <- function(lon1, lat1, lon2, lat2) {
    radlat1 = rad * lat1
    radlat2 = rad * lat2
    dlat = radlat2 - radlat1
    dlon = rad * (lon2 - lon1)
    a = (sin(dlat/2)^2) + cos(radlat1)*cos(radlat2)*(sin(dlon/2)^2)
    a = 2*asin(sqrt(a))
    return(a*Radius)
  }

  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x}) #Convert to Ana terms
  Radius <- 6371 #Earth Radius in km (disp are in km)
  rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
  bins <- seq(1,400,1) #400 time windows
  tmin <- 1/(60*60*24) # 1 second is min time
  sumDist2 <- sumDist <- Timefreq <- rep(0, length(bins))
  statusMessages <- c("25% complete", "50% complete", "75% complete")
  percent <- c(round(dim(species_df)[1]*0.25),round(dim(species_df)[1]*0.5),round(dim(species_df)[1]*0.75))
  p <- 1 #for status messages

  for(j in 1:dim(species_df)[1]){ # for each row
    if (j %in% percent){
      message(statusMessages[p])
      p <- p+1
    }
    n <- species_index[[paste(species_df[j,1])]][length(species_index[[paste(species_df[j,1])]])] #row where each individual ends
    for(k in (j+1):n){ #for each shark, calculate the distance between all locations (k)
      if (j+1 <= n){
        myTime <- as.numeric(difftime(species_df[k,4],species_df[j,4],units=timeUnit))
        b <- floor(log(myTime/tmin)/log(wBins) + 0.5) #log scale time bin
        Timefreq[b] <- Timefreq[b] + 1 #Cumulative count of displacements between each location within each log time bin
        Dist <- MydistHaversine(species_df[k,2], species_df[k,3], species_df[j,2], species_df[j,3]) #Calculates distance from point 1 to all successive points
        sumDist[b] <- sumDist[b] + Dist
        sumDist2[b] <- sumDist2[b] + Dist^2
      }
    }
  }
  message ("Calculations complete")

  mybins <- rep(0, length(bins)) # for the x axis of the plot
  for(b in 1: length(bins)){
    mybins[b] <- tmin*wBins^(b)
  }
  RMS_Result <- as.data.frame(cbind("timeBin_log"=mybins, "Count"=Timefreq, "sumDist"=sumDist, "sumDist2"=sumDist2))
  RMS_Result <- RMS_Result[(RMS_Result[,1]!= 0) & (RMS_Result[,3]!= 0) & (RMS_Result[,4]!= 0),]
  RMS_Result$MeanDisp_per_tb <- RMS_Result[,3]/RMS_Result[,2]
  RMS_Result$Sqrt_dRMS_per_tb <- sqrt(RMS_Result[,4]/RMS_Result[,2])
  plot.df <- cbind(RMS_Result[,c(1,5,6)])
  names(plot.df) <- c("timeWindow", "meanDisplacements", "rmsDisplacements")

  if (plot==TRUE){ # Plot RMS of displacements, and mean displacements on log-log scale
    if (nrow(plot.df)>1){
      ylabel <- expression('<'*d^q*'>'^(1/q)* (km)) #generic expression = ('<'*d^q*'>'^(1/q)* (km)), q=1 is mean disp, and q=2 is RMS
      xlabel <- paste('T(',timeUnit,')',sep="")
      a <- ggplot2::ggplot(plot.df, ggplot2::aes(plot.df[,1],plot.df[,3]))
      if (lm==TRUE){
      a <- a +
        ggplot2::stat_smooth(formula = y ~ x, method="lm", col="red")
      }
      a <- a +
        ggplot2::geom_point(ggplot2::aes(y = plot.df[,2], colour = "Mean disp.")) +
        ggplot2::geom_point(ggplot2::aes(y = plot.df[,3], colour = "RMS disp."))+
        ggplot2::scale_colour_manual(values = c("dark grey", "black"), guide = ggplot2::guide_legend(reverse = TRUE))+
        ggplot2::scale_x_log10(
          breaks = function(x) {
            brks <- scales::extended_breaks(Q = c(1, 5))(log10(x))
            10^(brks[brks %% 1 == 0])
          },
          labels = scales::math_format(format = log10)
        ) +
        ggplot2::scale_y_log10(
          breaks = function(x) {
            brks <- scales::extended_breaks(Q = c(1, 5))(log10(x))
            10^(brks[brks %% 1 == 0])
          },
          labels = scales::math_format(format = log10)
        ) +
        ggplot2::theme_bw(base_size = 18)+
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                            panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                                            axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), colour = "black"),
                                            axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), colour = "black"),
                                            legend.position = c(0, 1), legend.justification = c(0, 1), legend.direction = 'vertical',
                                            legend.background =ggplot2::element_blank(), legend.title = ggplot2::element_blank())+
        ggplot2::annotation_logticks(short=grid::unit(-0.1, "cm"), mid=grid::unit(-0.1, "cm"), long=grid::unit(-0.3,"cm")) +
        ggplot2::coord_cartesian(clip="off")+
        ggplot2::xlab(xlabel)+
        ggplot2::ylab(ylabel)
      plot(a)
    } else {
      warning("Not enough data to create plot")
    }
  }

  plot.df <- list(plot.df)
  names(plot.df) <- "rmsResults"

  if (lm==TRUE){
    if (nrow(RMS_Result)>1){
      fit <- lm(log(RMS_Result$Sqrt_dRMS_per_tb) ~ log(RMS_Result$timeBin_log), data = RMS_Result)
      plot.df[[2]] <- as.data.frame(broom::tidy(fit))
      names(plot.df) <- c("rmsResults", "lm")
      rm(fit)
    } else {
      warning("Not enough data fit linear model")
    }
  }

  return(plot.df)
}

#' Calculate Turning Angles from Trajectories
#'
#' To calculate turning angles between sets of three consecutive location estimates separated by set time period(s).
#' @param species_df A data frame containing location data (rows) and columns with the following headers: "ref", "lon", "lat", "day". "ref" is the unique id number for each animal
#'      (e.g., their satellite tag number; integer format). "lon" and "lat" are the longitude and latitide of each position estimate in decimal degrees (numeric format). "day"
#'      is the datetime stamp for each location estimate (POSIXct format following yyyy-mm-dd hh:mm:ss). See XXX data frame for an example.
#' @param min_hr Minimum number of hours to consider for displacement calculations. Default is 24 hours.
#' @param max_hr Maximum number of hours to consider for displacement calculations (default is 240 hours)
#' @param interval_hr Time interval (in hours) used to identify time period intervals between min_hr and max_hr (default is 24 hours)
#' @param range_hr Range (in hours) applied to interval_hr. This value helps the algorithm identify location estimates that are close to, but not exactly separated by the interval_hr. If multiple location estimates fall within this range the location estimate closest to the interval_hr will be used for calculations.
#' @param histplot Plot a histogram showing the frequency of turning angles from either all time periods combined or one specific time period. For example, histplot=c(TRUE,1) to plot only the 1st time period. Default histplot=c(TRUE, "all").
#' @param spiderplot Plot a spider plot showing the frequency of turning angles from either all time periods combined or one specific time period. For example, spiderplot=c(TRUE,1) to plot only the 1st time period. Default spiderplot=c(TRUE, "all").
#' @param colours Colour(s) for bars in histplot and lines in spiderplot. Default is "Navy".
#' @return List of Turning Angles for each time period and a histogram and/or spiderplot of results, if desired
#' @examples TurningAngles(species_df, max_hr=24)
#' TurningAngles(species_df,min_hr=24,max_hr=240,interval_hr=24,range_hr=6, plot=TRUE)
#' @export

TurningAngles<-function(species_df,min_hr=24,max_hr=240,interval_hr=24,range_hr=6, spiderplot=c(TRUE, "all"), histplot=c(FALSE, "all"), colours="Navy"){

  min_hr<-min_hr*(60*60) #convert hours (input) to seconds
  max_hr<-max_hr*(60*60) #convert hours (input) to seconds
  interval_hr<-interval_hr*(60*60) #convert hours (input) to seconds
  range_hr<-range_hr*(60*60) #convert hours (input) to seconds

  MydistHaversine <- function(lon1, lat1, lon2, lat2) {
    radlat1 = rad * lat1
    radlat2 = rad * lat2
    dlat = radlat2 - radlat1
    dlon = rad * (lon2 - lon1)
    a = (sin(dlat/2)^2) + cos(radlat1)*cos(radlat2)*(sin(dlon/2)^2)
    a = 2*asin(sqrt(a))
    return(a*Radius)
  }

  Radius <- 6371 #Earth Radius in km (disp are in km)
  rad <- 3.141592653589793/180 #Python has more digits of pi than R, so value pasted here instead of "pi"
  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  MyTime <- c(seq(min_hr,max_hr,interval_hr))

  AngleList <- list()
  bins <- 360 / 45
  Days <- MyTime/(24*60*60)
  j<-i<-d<-1
  for (d in 1:length(MyTime)){ ###added
    AngleList[[d]] <- 0 ### dummy value to initialize the list of angles for each time period
    for (i in 1:length(species_index)){ # for each individual
      for(j in 1:length((species_index[[i]]))){ # for each tracked location
        # Find locations separated by MyTime[d]
        Jumpj <-  which(species_df[species_index[[i]],4] >= species_df[species_index[[i]][j],4] + MyTime[d] - range_hr & species_df[species_index[[i]],4] <= species_df[species_index[[i]][j],4] + MyTime[d] + range_hr)
        # If only one jump is found, calculate distance
        if(length(Jumpj) > 0 ){
          if(length(Jumpj) == 1){
            J1 <- Jumpj
            #print(paste("ifjumpj=1", J1))
          } else {
            checkJump <- c()
            for (r in 1:length(Jumpj)){
              checkJump[r] <- abs(as.numeric(species_df[species_index[[i]][Jumpj[r]],4]) - as.numeric(species_df[species_index[[i]][j],4]) - MyTime[d])
            }
            mymin <- which(checkJump == min(checkJump))
            J1 <- Jumpj[mymin[1]] ### mymin[1] to account for cases where there are two jumps where the time difference is symmetrical (i.e., time differences are equal)
            #print(paste("ifjumpj>1", J1))
          }
          Jumpj <-  which(species_df[species_index[[i]],4] >= species_df[species_index[[i]][J1],4] + MyTime[d] - range_hr & species_df[species_index[[i]],4] <= species_df[species_index[[i]][J1],4] + MyTime[d] + range_hr)
          # If only one jump is found, calculate distance
          if(length(Jumpj) > 0 ){
            if(length(Jumpj) == 1){
              J2 <- Jumpj
            } else {
              checkJump <- c()
              for (r in 1:length(Jumpj)){
                checkJump[r] <- abs(as.numeric(species_df[species_index[[i]][Jumpj[r]],4]) - as.numeric(species_df[species_index[[i]][j],4]) - MyTime[d])
              }
              mymin <- which(checkJump == min(checkJump))
              J2 <- Jumpj[mymin[1]] ### mymin[1] to account for cases where there are two jumps where the time difference is simmetrical (i.e., time differences are equal)
            }

            ### Now we need to calculate the angles at J1 (i.e., between J and J2)
            # First we calculate the scalar for each component of the vectors between J and J1 (let's call it 'ax' and 'ay'), and J1 and J2 (let's call it 'bx' and 'by')
            # Load MydistHaversine from RMS code
            ax <- MydistHaversine(species_df[species_index[[i]][j],2], species_df[species_index[[i]][j],3], species_df[species_index[[i]][J1],2], species_df[species_index[[i]][j],3])
            ay <- MydistHaversine(species_df[species_index[[i]][j],2], species_df[species_index[[i]][j],3], species_df[species_index[[i]][j],2], species_df[species_index[[i]][J1],3])
            bx <- MydistHaversine(species_df[species_index[[i]][J1],2], species_df[species_index[[i]][J1],3], species_df[species_index[[i]][J2],2], species_df[species_index[[i]][J1],3])
            by <- MydistHaversine(species_df[species_index[[i]][J1],2], species_df[species_index[[i]][J1],3], species_df[species_index[[i]][J1],2], species_df[species_index[[i]][J2],3])

            # Remember to correct direction in longitude and latitude (i.e., W-E, N-S) # but account for potential movement between longitudes -180 and 180 (e.g., going from -180 to -182, which is represented as 178)
            if(species_df[species_index[[i]][J1],2] - species_df[species_index[[i]][j],2] > 180) { ax = -ax }
            if(species_df[species_index[[i]][J1],2] - species_df[species_index[[i]][j],2] < 0) { ax = -ax }
            if(species_df[species_index[[i]][J1],3] - species_df[species_index[[i]][j],3] < 0) { ay = -ay }
            if(species_df[species_index[[i]][J2],2] - species_df[species_index[[i]][J1],2] < 0 & species_df[species_index[[i]][J2],2] - species_df[species_index[[i]][J1],2] > -180){ bx = -bx }
            if(species_df[species_index[[i]][J2],3] - species_df[species_index[[i]][J1],3] < 0) { by = -by }

            # Now compute the scalar product and vector product, to get the angle
            if((ax*bx + ay*by) == 0) { ### prevent the case where tangent is infinite (when cosinus is zero) i.e., the individual did not move in one of the components (J = J1 or J1 = J2)
              if((ax*by - ay*bx) > 0){
                angle <- pi/2
                AngleList[[d]] <- append(AngleList[[d]], angle)
              } else if ((ax*by - ay*bx) < 0){
                angle <- -pi/2
                AngleList[[d]] <- append(AngleList[[d]], angle)
              }
            } else {
              angle <- atan((ax*by - ay*bx) / (ax*bx + ay*by)) ### in radians
              if(angle > 0){
                if((ax*bx + ay*by) < 0) {
                  angle <- angle - pi
                }
              } else if(angle <= 0){
                if((ax*bx + ay*by) < 0) {
                  angle <- angle + pi
                }
              }
              AngleList[[d]] <- append(AngleList[[d]], angle)
            }
          }
        }
      }
    }
    AngleList[[d]] <- AngleList[[d]][-1] # Remove dummy value from start of list for each individual
    AngleList[[d]] <- AngleList[[d]] / rad # Now transform all the angles from radians to degrees

    h <- hist(unlist(AngleList[[d]]), plot = FALSE, breaks = seq(-180, 180, bins)) #Anglelist is all angels for a time period from all individuals
    AngleProb <- h$counts/length(unlist(AngleList[[d]]))
    AngleProb<- c(AngleProb[1:23],AngleProb[23],AngleProb[24:45]) #Duplicated angle at 0 since 360=0 and 360 is needed for plot
    Cols<-h$mids
    Pos.Angles<-c(Cols[c(1:22)]+360,360,Cols[c(23:45)]) #Added 360 to list for plot. Angles at 360 are the same as at 0 (#Pos.Angles<-c(spider[c(1:22),1]+360,spider[c(23:45),1]) #instead of -180 to + 180, convert to 0 to 365)

    if (d == 1){
      spider<-as.data.frame(cbind(Pos.Angles,AngleProb,Days=c(rep(Days[d],length(AngleProb)))))
    }
    if (d > 1){
      spider_temp<-as.data.frame(cbind(Pos.Angles,AngleProb,Days=c(rep(Days[d],length(AngleProb)))))
      spider<-rbind(spider,spider_temp)
    }
  }
  assign("AngleList",AngleList, envir = .GlobalEnv)

  if (spiderplot[1]==TRUE){
    if (spiderplot[2]!="all"){
      spider <- spider[which(spider$Days==spiderplot[2]),]
    }
    colourpal<-rep(colours,length(Days))
    spider_plot<-ggplot2::ggplot(spider, ggplot2::aes(x = Pos.Angles, y = AngleProb, group=as.factor(Days),colour=as.factor(Days)))+
      ggplot2::coord_polar()+
      ggplot2::geom_hline(yintercept = c(0, max(spider$AngleProb)+0.01), colour = "black", size = 0.25) +
      ggplot2::geom_vline(xintercept = seq(0, 360, by = 90), colour = "black", size = 0.25) +
      ggplot2::geom_point(size=0.4) +
      ggplot2::geom_line(size=1)+
      ggplot2::scale_colour_manual(values=colourpal)+
      ggplot2::scale_x_continuous(limits = c(0, 360), breaks = c(0,90,180,270), labels=c("0°","90°","180°","270°"))+
      ggplot2::xlab("") +
      ggplot2::ylab("")+
      ggplot2::labs(title="")+
      ggplot2::theme_bw()+
      ggplot2::theme(
        plot.title = ggplot2::element_text(size=15, face="bold",hjust = 0.5),
        axis.text.x = ggplot2::element_text(size=12, margin=grid::unit(c(0, 0, 0, 0), "cm"), face="bold"),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        legend.position = "none",
        panel.border = ggplot2::element_blank(),
        panel.grid  = ggplot2::element_blank(),
        plot.margin = grid::unit(c(0, 0, 0, 0), "points"))
      plot(spider_plot)
  }

  if (histplot[1]==TRUE){ #histogram of all angles combined
    if (histplot[2]=="all"){
    h <- hist(unlist(AngleList), plot = FALSE, breaks = seq(-180, 180, bins)) # Anglelist is all angels for all time periods from all individuals
    } else {
    h <- hist(unlist(AngleList[[histplot[2]]]), plot = FALSE, breaks = seq(-180, 180, bins)) # Anglelist is all angels for all time periods from all individuals
    }
    plot(h, main="", xlab="Turning Angles", xaxt="n",col=colours)
    axis(1, at=seq(-180,180,by=20), labels=seq(-180,180,by=20))
  }
}

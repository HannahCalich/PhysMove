#' Identify Infomap communities and create a transition probability matrix
#'
#' This function uses the network community detection Infomap to identify Infomap communities based on a transition probability matrix (tmp), which summarizes
#' the probability of individuals moving from one grid cell to another. This function assumes directed movement, allows for self-links (where an individual
#' stays in the same cell over time), and uses a tpm in link list format to create an Infomap 'monolayer_object'.
#' Please note: to run this function you must first download the infomapecology package from gitub and install the infomap.exe.
#' For details please see: https://ecological-complexity-lab.github.io/infomap_ecology_package/installation
#' To learn more about Infomap please visit: https://www.mapequation.org/
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format),
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{plSample}}, \code{\link{expSample}}, or \code{\link{lnormSample}}.
#' @param gridCell Grid cell size in degrees. Default is 0.25.
#' @param hours Number of hours to consider movements for. Default is 24.
#' @param range_hr Range (in hours) converts the hours parameter into a time window (hours +/-  range_hr) so the
#' code can identify location estimates that are close to, but not exactly separated by a set number of hours.
#' If multiple location estimates fall within this time window the location estimate closest to the set hours input value
#' will be used for calculations. For example, if hours = 24 and range = 6, the algorithm will search for
#' locations spaced 18 to 32 hours apart. Default is 6.
#' @param infomap Identify Infomap communities. Default is TRUE.
#' @param tpm Export the transition probability matrix in link list format. Default is FALSE.
#' @return 'infomap_object' that summarizes the hierarchical structure of the Infomap communities (regions where individuals are likely
#' to stay for longer periods of time).If tpm=TRUE the transition probability matrix used to create 'infomap_object' is exported.
#' @examples
#' InfomapCommunities(expSample)
#' InfomapCommunities(expSample, gridCell=0.25, hours=24, range_hr=6, infomap=TRUE, tpm=FALSE)
#' @export
#'

InfomapCommunities <- function(species_df, gridCell=0.25, hours=24, range_hr=6, infomap=TRUE, tpm=FALSE){

  outerror <- tryCatch({
    if (infomap==TRUE){
        if(infomapecology::check_infomap()!=TRUE){
          warning()
          break()
        }
      }

    species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
    longmin <- -180
    latmin <- -90
    longmax <- 180
    latmax <- 90
    grid <- 1/gridCell
    coordlong <- floor(grid * (as.numeric(species_df[,2]) - longmin))
    coordlat <- floor(grid * (as.numeric(species_df[,3]) - latmin))
    species_df$cellnum <- coordlong + grid * (longmax - longmin) * coordlat
    MyTime <- hours*60*60
    range_hr <- range_hr*60*60
    totalcells <- (grid * (longmax - longmin)) * (grid * (latmax - latmin))
    DestinationCells <- list()
    DestinationCells[[totalcells + 1 ]] <- 0

    for (i in 1:length(species_index)){
      for(j in 1:length((species_index[[i]]))){
        Jumpj<-which(species_df[species_index[[i]],4] >= species_df[species_index[[i]][j],4] + MyTime - range_hr & species_df[species_index[[i]],4] <= species_df[species_index[[i]][j],4] + MyTime + range_hr)
          if(length(Jumpj) == 1){
            DestinationCells[[as.numeric(species_df[species_index[[i]][j],5])]] <- append(DestinationCells[[as.numeric(species_df[species_index[[i]][j],5])]], as.numeric(species_df[species_index[[i]][Jumpj],5]))
          } else if(length(Jumpj) > 1){
              checkJump <- c()
              for (r in 1:length(Jumpj)){
                checkJump[r] <- abs(as.numeric(species_df[species_index[[i]][Jumpj[r]],4]) - as.numeric(species_df[species_index[[i]][j],4]) - MyTime)
              }
              if(length(which(checkJump == 1)) == 1){
                mymin <- which(checkJump == 1)
              } else {
                mymin <- which(checkJump == min(checkJump))
              }
              DestinationCells[[as.numeric(species_df[species_index[[i]][j],5])]] <- append(DestinationCells[[as.numeric(species_df[species_index[[i]][j],5])]], as.numeric(species_df[species_index[[i]][Jumpj[mymin]],5]))
          }
      }
    }
    names(DestinationCells) <- seq_along(DestinationCells) # to keep origin cells as names
    DestinationCells <- Filter(Negate(is.null), DestinationCells) # remove cells that were not visited within time window
    DestinationCells <- DestinationCells[-length(DestinationCells)] # Remove dummy value from tail
    Probability <- c()
    Probability.Total <- c()

    for(i in 1:length(DestinationCells)){ # total number of origin cells
      MyP <- as.numeric(table(DestinationCells[[i]]))/length(DestinationCells[[i]]) # Calculate the probability of each destination cell being visited
      Probability<-data.frame("OriginCell"=c(as.numeric(paste(names(DestinationCells[i])))),
                              "DestinationCell"=c(as.numeric(names(table(DestinationCells[[i]])))), "Probability"=c(MyP))
      Probability.Total<-rbind(Probability.Total, Probability)
    }
    Probability.Total$OriginCell<-Probability.Total$OriginCell+1
    Probability.Total$DestinationCell<-Probability.Total$DestinationCell+1
    empty.vector<-c(rep(0,times=totalcells)) # make vector for each cell in world (n=1036800 cells for 0.25 deg resolution)
    Visited.Cells<-unique(as.vector(t(cbind(Probability.Total$OriginCell,Probability.Total$DestinationCell)))) # convert transition probability matrix to consecutive vector of origin then destination cells, maintaining movement order
    CellCoords<-data.frame("Cell"=Visited.Cells)

    for(i in 1:nrow(CellCoords)){ # Find center point of cell for plotting
      coordlat <- floor(CellCoords$Cell[i]/(grid*(longmax-longmin)))
      coordlong <- CellCoords$Cell[i] - (grid*(longmax-longmin)) * coordlat
      if (coordlong==0) {
        CellCoords$long[i]  <- (longmin + (360 / grid)) - 0.5*gridCell
        CellCoords$lat[i] <- (latmin + (coordlat / grid)) - 0.5*gridCell
      } else {
        CellCoords$long[i]  <- (longmin + (coordlong/grid)) - 0.5*gridCell
        CellCoords$lat[i] <- (latmin + (coordlat / grid)) + 0.5*gridCell
      }
    }

    visited.order<-replace(empty.vector, Visited.Cells, seq(1,length(Visited.Cells),1)) # re-numbers vector of visited cells (all cells in world)
    order.df<-data.frame("OriginCell"=as.numeric(c(Probability.Total$OriginCell)), "OriginNode"=as.numeric(c("0")),
                         "DestinationCell"=as.numeric(c(Probability.Total$DestinationCell)), "DestinationNode"=as.numeric(c("0")),
                         "Probability"=as.numeric(c(Probability.Total$Probability)))

    for (c in 1:length(visited.order)){ # for all cells in the world, go through all the visited cells, and renumber the order that they were visited starting at 1
      order.df$OriginNode[which(c==order.df[,1])]<-as.numeric(paste(visited.order[c]))
      order.df$DestinationNode[which(c==order.df[,3])]<-as.numeric(paste(visited.order[c]))
    }

    order.df<-merge(order.df, CellCoords, by.x="OriginCell", by.y="Cell", all.x=TRUE)
    names(order.df)[6:7]<-c("OriginLong","OriginLat")
    order.df<-merge(order.df, CellCoords, by.x="DestinationCell", by.y="Cell", all.x=TRUE)
    names(order.df)[8:9]<-c("DestinationLong","DestinationLat")
    order.df<-order.df[,c(3,2,6,7,4,1,8,9,5)]

    LinkList<-order.df[,c(1,5,9)] # remove cell numbers (infomap requires order of cell visits only)
    colnames(LinkList)<-c("from", "to", "weight") # rename columns following infomap requirements
    LinkList$from<-sub("^","Node",LinkList$from)
    LinkList$to<-sub("^","Node",LinkList$to)

    names(order.df) <-c(rep(c("Node", "Cell", "Long", "Lat"),2),"Probability")
    nodenames<-rbind(order.df[,c(1:4)],order.df[,c(5:8)])
    nodenames<-unique(nodenames[order(nodenames$Node),])
    names(nodenames)<-c("node_name","cell", "long", "lat")
    nodenames$node_name<-sub("^","Node",nodenames$node_name)

    if (infomap==TRUE){
      monolayer_object<-infomapecology::create_monolayer_object(LinkList, directed = T, bipartite = F,node_metadata = nodenames)
      infomap_object<-infomapecology::run_infomap_monolayer(monolayer_object, infomap_executable='infomap', flow_model='directed', silent=T, verbose=F, two_level=F, ...="-k")
      assign("infomap_object",infomap_object, envir = .GlobalEnv)
    }

    if (tpm==TRUE){
      names(order.df)<-c("OriginNode", "OriginCell", "OriginLong", "OriginLat","DestinationNode", "DestinationCell", "DestinationLong", "DestinationLat","Probability")
      assign("TransitionProbabilityMatrix", order.df, envir = .GlobalEnv)
    }
  },
  error=function(cond){
    message(paste("Trouble installing or locating infomapecology package. \nTo resolve: load the package directly via library(infomapecology), or reinstall the infomapecology package in R. \nFor more information visit https://ecological-complexity-lab.github.io/infomap_ecology_package/installation"))
  },
  warning=function(cond){
    message(paste("Cannot find infomap.exe, please set working directory to folder containing infomap.exe file. \nFor more information visit https://ecological-complexity-lab.github.io/infomap_ecology_package/installation"))
  }
  )
  return(invisible(outerror))
}

#' Create Link List for Infomap Analysis
#'
#' This function allows you to create a link list for the Network community detection Infomap (https://www.mapequation.org/infomap/#Install)
#' When function is finished please export LinkList as .txt file following "write.table(LinkListOutput, "LinkList2.txt", sep="/t", row.names = FALSE, col.names = FALSE)" and upload .txt file at https://www.mapequation.org/infomap/#Install
#' Important settings to consider when configuring analysis for Infomap are: '-i link-list -k --tree -d'
#' '-i link-list' identifies the input at a link list, '-k' enables self-links, '--tree' ensures the output is in .tree format, and '-d' assumed directed links."

#' @param species_df A data frame containing location data (rows) and columns with the following headers: "ref", "lon", "lat", "day". "ref" is the unique id number for each animal
#'      (e.g., their satellite tag number; integer format). "lon" and "lat" are the longitude and latitide of each position estimate in decimal degrees (numeric format). "day"
#'      is the datetime stamp for each location estimate (POSIXct format following yyyy-mm-dd hh:mm:ss). See XXX data frame for an example.
#' @param grid grid used to split 1-degree by 1-degree cells. A grid of 4 results in 0.25-degree by 0.25 degree grid cells, while a grid of 0.25 results in 4-degrees by 4-degree grid cells. Default is 4.
#' @param hours Number of hours to consider movements for. Default is 24.
#' @param range_hr Range (in hours) applied to hours This value helps the algorithm identify location estimates that are close to, but not exactly separated by the interval_hr. If multiple location estimates fall within this range_hr the location estimate closest to the interval_hr will be used for calculations.
#' @return A matrix containing the link list needed to calculate Infomap communities using https://www.mapequation.org/infomap/#Install, and a data frame of the LinkListCellNumbers that is used with the \code{\link{InfomapCoords}} function to interpret the output from Infomap.
#' @examples
#' LinkList(species_df)
#' LinkList(species_df, grid=4, hours=24, range_hr=6)
#' @export

LinkList <- function(species_df, grid=4, hours=24, range_hr=6, infomap=TRUE, tpm=FALSE){

  if (infomap==TRUE){
    if(infomapecology::check_infomap()!=TRUE){
      message("Cannot find infomap.exe, please set working directory to folder containing infomap.exe file. See https://ecological-complexity-lab.github.io/infomap_ecology_package/installation for more information")
      opt <- options(show.error.messages = FALSE)
      on.exit(options(opt))
      stop()
    }
  }

  outerror <- tryCatch({
  species_index <- tapply(1:nrow(species_df), species_df[,1], function(x){x})
  longmin <- -180
  latmin <- -90
  longmax <- 180
  latmax <- 90
  coordlong <- floor(grid * (as.numeric(species_df[,2]) - longmin)) # convert longitude from (-180 to +180) to (0-360)
  coordlat <- floor(grid * (as.numeric(species_df[,3]) - latmin)) # convert latitude from (-90 to +90) to (0-180)
  species_df$cellnum <- coordlong + grid * (longmax - longmin) * coordlat # assign cell number to each lat/long
  MyTime <- hours*60*60 # 1 day time periods (in seconds)# There are 3600 seconds in 1 hour
  range_hr <- range_hr*60*60
  totalcells <- (grid * (longmax - longmin)) * (grid * (latmax - latmin))   # Vector to store origin and destination cells, plus counts# 1,036,800 cells of 0.25 deg in world

  DestinationCells <- list()
  DestinationCells[[totalcells + 1 ]] <- 0 ### Dummy value at the last element + 1 possible to make sure the list has at least one element for each possible origin

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
          } else {mymin <- which(checkJump == min(checkJump))
          }
          DestinationCells[[as.numeric(species_df[species_index[[i]][j],5])]] <- append(DestinationCells[[as.numeric(species_df[species_index[[i]][j],5])]], as.numeric(species_df[species_index[[i]][Jumpj[mymin]],5]))
        }
    }
  }
  names(DestinationCells) <- seq_along(DestinationCells) # to keep origin cells as names
  DestinationCells <- Filter(Negate(is.null), DestinationCells) # remove cells that were not visited within time window
  DestinationCells <- (DestinationCells[-length(DestinationCells)]) # Remove dummy value from tail
  Probability<-c()
  Probability.Total<-c()
  for(i in 1:length(DestinationCells)){ # total number of origin cells
    MyP <- as.numeric(table(DestinationCells[[i]]))/length(DestinationCells[[i]]) # Calculate the probability of each destination cell being visited
    Probability<-data.frame("OriginCell"=c(as.numeric(paste(names(DestinationCells[i])))),
                            "DestinationCell"=c(as.numeric(names(table(DestinationCells[[i]])))), "Probability"=c(MyP))
    Probability.Total<-rbind(Probability.Total, Probability)
  }
  empty.vector<-c(rep(0,times=totalcells)) # make vector for each cell in world (n=1036800 cells for 0.25 deg resolution)
  Visited.Cells<-as.vector(t(cbind(Probability.Total$OriginCell,Probability.Total$DestinationCell))) # convert transition probability matrix to consecutive vector of origin then destination cells, maintaining movement order
  visited.order<-replace(empty.vector, unique(Visited.Cells), seq(1,length(unique(Visited.Cells)),1)) # re-numbers vector of visited cells (all cells in world)
  order.df<-data.frame("OriginCell"=as.numeric(c(Probability.Total$OriginCell)), "OriginOrder"=as.numeric(c("0")),
                       "DestinationCell"=as.numeric(c(Probability.Total$DestinationCell)), "DestinationOrder"=as.numeric(c("0")),
                       "Probability"=as.numeric(c(Probability.Total$Probability)))
  for (c in 1:length(visited.order)){ # for all cells in the world, go through all the visited cells, and renumber the order that they were visited starting at 1
    order.df$OriginOrder[which(c==order.df[,1])]<-as.numeric(paste(visited.order[c]))
    order.df$DestinationOrder[which(c==order.df[,3])]<-as.numeric(paste(visited.order[c]))
  }
  LinkList<-order.df[,c(2,4,5)] # remove cell numbers (infomap requires order of cell visits only)
  colnames(LinkList)<-c("from", "to", "weight") # rename columns following infomap requirements
  LinkList$from<-sub("^","Node",LinkList$from)
  LinkList$to<-sub("^","Node",LinkList$to)
  names(order.df)<-c("Cell", "Node", "Cell", "Node")
  nodenames<-rbind(order.df[,c(1:2)],order.df[,c(3:4)])
  nodenames<-unique(nodenames[order(nodenames$Node),])
  names(nodenames)<-c("cell", "node_name")
  nodenames<-nodenames[,2:1]
  nodenames$node_name<-sub("^","Node",nodenames$node_name)

  if (tpm==TRUE){
  assign("TransitionProbabilityMatrix", order.df, envir = .GlobalEnv)
  }

  if (infomap==TRUE){
    monolayer_object<-infomapecology::create_monolayer_object(LinkList, directed = T, bipartite = F,node_metadata = nodenames)
    infomap_object<-infomapecology::run_infomap_monolayer(monolayer_object, infomap_executable='infomap', flow_model='directed', silent=T, verbose=F, two_level=F, ...="-k")
    infomap_modules<-as.data.frame(infomap_object$modules)
    for(i in 1:nrow(infomap_modules)){
      coordlat <- floor(infomap_modules$cell[i]/(grid*(longmax-longmin)))
      coordlong <- infomap_modules$cell[i] - (grid*(longmax-longmin)) * coordlat
      infomap_modules$long[i] <- longmin + (coordlong / grid)
      infomap_modules$lat[i] <- latmin + (coordlat / grid)
    }
    assign("Infomap_Results",infomap_modules, envir = .GlobalEnv)
  }
  },
  error=function(cond){
    message(paste("Trouble locating infomapecology package. To resolve: load the package directly via library(infomapecology)"))
    # return(NA)
  }
  )
  return(invisible(outerror))
}

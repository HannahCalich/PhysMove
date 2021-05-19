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

LinkList <- function(species_df, grid=4, hours=24, range_hr=6){

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

  # DestinationCells is list of all cells that were visited within time window, i.e., each list number is an origin cell and
  # All of the numbers in the list are the destination cells visited within the time window.
    DestinationCells <- list()
    DestinationCells[[totalcells + 1 ]] <- 0 ### Dummy value at the last element + 1 possible to make sure the list has at least one element for each possible origin

  # Create list of origin cells and all cells visited from origin cell (destination cells) within time period (set earlier).If >1 cell is visited within time period, the cell that was visited closest to the time period is closen as the destination cell.
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

  # Calculate the probability that each origin cell was visited by each destination cell. Creates transition probability matrix of movements.
    Probability<-c()
    Probability.Total<-c()
    for(i in 1:length(DestinationCells)){ # total number of origin cells
      MyP <- as.numeric(table(DestinationCells[[i]]))/length(DestinationCells[[i]]) # Calculate the probability of each destination cell being visited
      Probability<-data.frame("OriginCell"=c(as.numeric(paste(names(DestinationCells[i])))),
                              "DestinationCell"=c(as.numeric(names(table(DestinationCells[[i]])))), "Probability"=c(MyP))
      Probability.Total<-rbind(Probability.Total, Probability)
    }

  # Format transition probability matrix for infomap by re-numbering visited cells by order of visit
    empty.vector<-c(rep(0,times=totalcells)) # make vector for each cell in world (n=1036800 cells for 0.25 deg resolution)
    Visited.Cells<-as.vector(t(cbind(Probability.Total$OriginCell,Probability.Total$DestinationCell))) # convert transition probability matrix to consecutive vector of origin then destination cells, maintaining movement order
    visited.order<-replace(empty.vector, unique(Visited.Cells), seq(1,length(unique(Visited.Cells)),1)) # re-numbers vector of visited cells (all cells in world)

  # Create new dataframe with all destination cells re-numbered in order of visit
    order.df<-data.frame("OriginCell"=as.numeric(c(Probability.Total$OriginCell)), "OriginOrder"=as.numeric(c("0")),
                         "DestinationCell"=as.numeric(c(Probability.Total$DestinationCell)), "DestinationOrder"=as.numeric(c("0")),
                         "Probability"=as.numeric(c(Probability.Total$Probability)))
    for (c in 1:length(visited.order)){ # for all cells in the world, go through all the visited cells, and renumber the order that they were visited starting at 1
      order.df$OriginOrder[which(c==order.df[,1])]<-as.numeric(paste(visited.order[c]))
      order.df$DestinationOrder[which(c==order.df[,3])]<-as.numeric(paste(visited.order[c]))
    }
    assign("LinkListCellNumbers", order.df, envir = .GlobalEnv)

    # Format results for infomap
    ordered.weights<-as.matrix(order.df[,c(2,4,5)]) # remove cell numbers (infomap requires order of cell visits only)
    colnames(ordered.weights)<-c("from", "to", "weight") # rename columns following infomap requirements
    names(order.df)<-c("Cell", "Node", "Cell", "Node")
    nodenames<-rbind(order.df[,c(1:2)],order.df[,c(3:4)])
    nodenames<-unique(nodenames[order(nodenames$Node),])
    assign("nodenames", nodenames, envir = .GlobalEnv)
    assign("LinkList", ordered.weights, envir = .GlobalEnv)
    assign("grid",grid, envir = .GlobalEnv)
}

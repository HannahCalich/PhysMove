#' Convert Infomap Nodes to Coordinates

#' Assigns coordinates to the formatted output from Infomap using the LinkListCellNumber data frame and grid outputs from the \code{\link{LinkList}} function.
#' To format the Infomap output:
#' 1) Download .tree output from Infomap website (https://www.mapequation.org/infomap/#Install):
#' 1) Open file in excel
#' 2) Delete summary text before results (cell A1 should contain path data in #:#:# format)
#' 3) Using "text to columns" tool, set "space" as delimiter, and format column with path information as "text" (default is "General")
#' 4) Save as "Network.csv"
#' 5) Import to R as data.frame and add headers "path", "flow", "name", and "node" to columns 1 to 4, respectively
#' @param Network Formatted output from https://www.mapequation.org/infomap/#Install.
#' @return InfomapCoords dataframe containing
#' @examples
#' InfomapCoords(Network)
#' @export

InfomapCoords<- function(Network){

    results.sorted<-results[order(results[,4]),] # Sort the nodes so they line up with the original order.df from earlier

    # Convert node numbers back to cell numbers
    for (r in 1:nrow(results.sorted)){ # for all rows in the results df
      for (n in 1:nrow(LinkListCellNumbers)){ # for each row in the order df(from previous code, where nodes and matching cell numbers are)
        if (r==LinkListCellNumbers[n,2]){ #if the cell number 1 is found in the origin order column
          results.sorted[r,5]<-LinkListCellNumbers[n,1]
        }
        else if (r==LinkListCellNumbers[n,4]){
          results.sorted[r,5]<-LinkListCellNumbers[n,3]
        }
      }
    }
    names(results.sorted)[names(results.sorted)== 'V5'] <-'cellnum'
    results.sorted$cellnum<-results.sorted$cellnum+1 #shift by 1 as per occupancy code

    # Convert cell numbers back to lat/long so they can be plotted on map
    lat<-long<-c()
    longmin <- -180
    latmin <- -90
    longmax <- 180
    latmax <- 90

    for(i in 1:nrow(results.sorted)){
      coordlat <- floor(results.sorted[i,5]/(grid*(longmax-longmin))) #Take origin cell number and divide by number of cells in longitude of grid.
      #Floor used because here we're identifying the latitude row this origin cell came from
      coordlong <- results.sorted[i,5] - (grid*(longmax-longmin)) * coordlat
      long[i] <- longmin + (coordlong / grid)
      lat[i] <- latmin + (coordlat / grid)
    }

    results.sorted.loc <- cbind(results.sorted, "lat" = lat, "long" = long)
    results.sorted.loc<-cbind(stringr::str_split_fixed(results.sorted.loc$path, ':',n=6),results.sorted.loc)
    colnames(results.sorted.loc)[1:6]<-c("Level1","Level2","Level3","Level4", "Level5","Level6")
    results.sorted.loc[,1:6][results.sorted.loc[,1:6]==""]<-NA
    assign("InfomapCoords",results.sorted.loc, envir = .GlobalEnv)

    if (map==TRUE){
      xyz <- results.sorted.loc[,c(12,13,1)]
      xyz$Level1<-as.numeric(xyz$Level1)
      mapWorld <- ggplot2::borders("world", colour="gray50", fill="gray50") # create a layer of borders
      z <- ggplot2::ggplot() + mapWorld
      z <- z +
        ggplot2::geom_tile(data=xyz, ggplot2::aes(x=long, y=lat, fill=Level1))+
        ggplot2::coord_sf(xlim = c(min(xyz$long), max(xyz$long)), ylim = c(min(xyz$lat), max(xyz$lat)))+
        ggplot2::theme_minimal()+
        ggplot2::scale_fill_gradientn(colours = c("blue", "light blue","red"))
      z
    }
}

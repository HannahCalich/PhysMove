#' Map Infomap Communities
#'
#' This function allows you to create a map of infomap communities calculated using the \code{\link{InfomapCommuities}} function
#' @param infomap_object output from the \code{\link{InfomapCommuities}} function
#' @param core_communities Vector of communities to be plotted in map. Default is NULL
#' @return A map illustrating Infomap communities
#' @examples
#' CommunityMap(infomap_object)
#' CommunityMap(infomap_object, core_communities)
#' @export

CommunityMap <- function(infomap_object, core_communities){

  if (exists("infomap_object")==FALSE){
    stop("Please calculate infomap communities using the InfomapCommunities function prior to executing CommunityMap")
  }

  infomap_modules<-as.data.frame(infomap_object$modules)

  if (!missing(core_communities)){
    infomap_modules<-infomap_modules[which(infomap_modules$module_level1 %in% core_communities),]
  }

    xyz <- infomap_modules[,c("module_level1", "long", "lat")]
    z <- ggplot2::ggplot() +
      ggplot2::geom_tile(data=xyz, ggplot2::aes(x=long, y=lat, fill=as.factor(module_level1)))+
      # geom_point(data = sharksBull, aes(x = lon, y = lat),col="black", size=2)+
      ggplot2::labs(x = "Longitude",y = "Latitude", fill = "Community")+
      ggplot2::coord_sf(xlim = c(min(xyz$long), max(xyz$long)), ylim = c(min(xyz$lat), max(xyz$lat)))+
      ggplot2::theme_minimal()+
      ggplot2::scale_fill_brewer(palette="Dark2")+
      ggplot2::borders("world", colour="gray50", fill="gray50", xlim = c(min(xyz$long), max(xyz$long)), ylim = c(min(xyz$lat), max(xyz$lat))) # create a layer of borders
    plot(z)
}

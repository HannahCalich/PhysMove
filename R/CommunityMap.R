#' Map Infomap communities
#'
#' This function allows you to create a map of the level 1 Infomap communities calculated using the \code{\link{InfomapCommunities}} function.
#' To map only a subset of the communities, save the relevant level 1 community numbers to a vector and input as subset_communities.
#' @param infomap_object Monolayer object that was output from the \code{\link{InfomapCommunities}} function.
#' @param subset_communities Vector of communities to be plotted in map. Default is NULL.
#' @param colPalette  Colour palette for map (applied to ggplot2::scale_fill_brewer). Default is "Dark2".
#' @param land Add a land polygon to the map. Note that if there is no land in your region you'll receive a "nothing to draw" error.
#' This can be fixed by using land=FALSE. Default is TRUE.
#' @return A map illustrating level 1 Infomap communities.
#' @examples
#' CommunityMap(infomap_object)
#' CommunityMap(infomap_object, subset_communities)
#' CommunityMap(infomap_object, c(1,2,3))
#' @export

CommunityMap <- function(infomap_object, subset_communities, colPalette="Dark2", land=TRUE){

  if (exists("infomap_object")==FALSE){
    stop("Please calculate Infomap communities using the InfomapCommunities function prior to executing CommunityMap")
  }

  infomap_modules<-as.data.frame(infomap_object$modules)

  if (!missing(subset_communities)){
    infomap_modules<-infomap_modules[which(infomap_modules$module_level1 %in% subset_communities),]
  }

    xyz <- infomap_modules[,c("module_level1", "long", "lat")]
    z <- ggplot2::ggplot() +
      ggplot2::geom_tile(data=xyz, ggplot2::aes(x=long, y=lat, fill=as.factor(module_level1)))+
      ggplot2::labs(x = "Longitude",y = "Latitude", fill = "Community")+
      ggplot2::coord_sf(xlim = c(min(xyz$long), max(xyz$long)), ylim = c(min(xyz$lat), max(xyz$lat)))+
      ggplot2::theme_minimal()+
      ggplot2::scale_fill_brewer(palette=colPalette)
    if (land==TRUE){
      z <- z + ggplot2::borders("world", colour="gray50", fill="gray50", xlim = c(min(xyz$long), max(xyz$long)),
                                ylim = c(min(xyz$lat), max(xyz$lat))) # create a layer of borders
    }
    plot(z)
}

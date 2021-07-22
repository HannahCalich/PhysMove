#' Map Infomap communities
#'
#' This function allows you to create a map of the level 1 Infomap communities calculated using the \code{\link{InfomapCommunities}} function.
#' To map only a subset of the communities, save the relevant level 1 community numbers to a vector and input as subset_communities.
#' @param infomap_object Monolayer object that was output from the \code{\link{InfomapCommunities}} function.
#' @param subset_communities Vector of communities to be plotted in map. Default is NULL.
#' @param colPalette  ggplot2 colour palette for map. Default is "Dark2".
#' @return A map illustrating level 1 Infomap communities.
#' @examples
#' CommunityMap(infomap_object)
#' CommunityMap(infomap_object, subset_communities)
#' CommunityMap(infomap_object, c(1,2,3))
#' @export

CommunityMap <- function(infomap_object, subset_communities, colPalette="Dark2"){

  if (exists("infomap_object")==FALSE){
    stop("Please calculate Infomap communities using the InfomapCommunities function prior to executing CommunityMap")
  }

  infomap_modules <- as.data.frame(infomap_object$modules)

  if (!missing(subset_communities)){
    infomap_modules<-infomap_modules[which(infomap_modules$module_level1 %in% subset_communities),]
  }

  if (length(unique(infomap_modules$module_level1)) > 8) { # ggplot pallets only have 8 unique colours but colorRampPalette can be used to extend the pallet
    myColoursPal <- colorRampPalette(RColorBrewer::brewer.pal(8, colPalette))(length(unique(infomap_modules$module_level1)))
  } else {
    myColoursRaw <- colPalette
  }

  xyz <- infomap_modules[,c("module_level1", "long", "lat")]
  z <- ggplot2::ggplot() +
    ggplot2::geom_tile(data=xyz, ggplot2::aes(x=long, y=lat, fill=as.factor(module_level1)))+
    ggplot2::labs(x = "Longitude",y = "Latitude", fill = "Community")+
    ggplot2::coord_sf(xlim = c(min(xyz$long), max(xyz$long)), ylim = c(min(xyz$lat), max(xyz$lat)))+
    ggplot2::theme_minimal()

  if (exists("myColoursPal")==TRUE){ # If a new colour pallet was created
    z <- z + ggplot2::scale_fill_manual(values = myColoursPal)
  } else {
    z <- z + ggplot2::scale_fill_brewer(palette = myColoursRaw) # If a standard ggplot2 colour pallet was used
  }
  tryCatch({
    z <- z +
      ggplot2::borders("world", colour="gray50", fill="gray50", xlim = c(min(xyz$long), max(xyz$long)), ylim = c(min(xyz$lat), max(xyz$lat)))
  }, error = function(e){message('World polygon does not overlap with occupancy data')})
  plot(z)
}

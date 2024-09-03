#' Map Infomap communities
#'
#' This function allows you to create a map of the level 1 Infomap communities calculated using the \code{\link{infomapCommunities}} function.
#' To map only a selection of the communities use the subset_communities parameter.
#' @param infomap_output Infomap monolayer object output from the \code{\link{infomapCommunities}} function.
#' @param subset_communities Concatenated vector of level 1 communities to be mapped. For example, subset_communities=c(1,2,3) will plot level 1 communities
#' 1, 2, and 3. This parameter is particularly useful if Infomap has identified many communities and they are difficult to distinguish in the map
#' All communities are included by default.
#' @param colours Colour(s) for each community in the map. Valid input options include: base R (grDevices) color pallets (e.g., colours=rainbow), RColorBrewer
#' palettes (e.g., colours="Dark2"), and colour names or hex numbers (e.g.,colours=c("darkred", "#4682B4", "#00008B", "darkgreen")). Note that grDevices color
#' pallets do not use quotations. If the palette does not have enough distinct colours to match the communities being plotted the function will automatically
#' create a continuous pallet with the colours provided. Default is "Dark2".
#' @return A map illustrating level 1 Infomap communities.
#' @importFrom rlang .data
#' @examples communityMap(infomapResult)
#' @export

communityMap <- function(infomap_output, subset_communities, colours="Dark2"){

  if (!("infomap_monolayer" %in% is(infomap_output[["infomap_object"]]))){
    stop("This function requires the Infomap monolayer object that is output from the InfomapCommunities function. \n  Please run the InfomapCommunities function prior to executing CommunityMap.")
  }

  infomap_output <- infomap_output[["infomap_object"]]

  infomap_modules <- as.data.frame(infomap_output$modules)

  if (!missing(subset_communities)){
    infomap_modules<-infomap_modules[which(infomap_modules$module_level1 %in% subset_communities),]
  }

  if ("function" %in% is(colours)){ # If a grDevices colour pallet is used
    myColoursPal <- colours(length(unique(infomap_modules$module_level1)))
  } else if (colours[1] %in% rownames(RColorBrewer::brewer.pal.info)){ # If a RColourBrewer pallet is used
    myColoursPal <- grDevices::colorRampPalette(RColorBrewer::brewer.pal(RColorBrewer::brewer.pal.info[colours,1], colours))(length(unique(infomap_modules$module_level1))) # Use the submitted colour palette and extend if to the number of colours needed
  } else {
    myPal <- grDevices::colorRampPalette(colours) # If hex codes or colour names are used
    myColoursPal <- myPal(length(unique(infomap_modules$module_level1)))
  }

  xyz <- infomap_modules[,c("module_level1", "long", "lat")]
  z <- ggplot2::ggplot() +
    ggplot2::geom_tile(data=xyz, ggplot2::aes(x=.data$long, y=.data$lat, fill=as.factor(.data$module_level1)))+
    ggplot2::labs(x = "Longitude",y = "Latitude", fill = "Community")+
    ggplot2::coord_sf(xlim = c(min(xyz$long), max(xyz$long)), ylim = c(min(xyz$lat), max(xyz$lat)))+
    ggplot2::theme_minimal(base_size = 12)+
    ggplot2::scale_fill_manual(values = myColoursPal) #input values for colour palette as hex codes

  tryCatch({ # This prevents the plot from crashing if the mapped area does not overlap with the world polygon (e.g., for pelagic species)
    z <- z +
      ggplot2::borders("world", colour="gray50", fill="gray50", xlim = c(min(xyz$long), max(xyz$long)), ylim = c(min(xyz$lat), max(xyz$lat)))
  }, error = function(e){message('World polygon does not overlap with Infomap communities')})
  plot(z)
}

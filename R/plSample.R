#' Random location data where displacement lengths follow a power-law distribution
#'
#' A data frame containing location data from a sample trajectory that follows a power-law distribution.
#'
#' @format A data frame with 1000 rows and 4 variables:
#' \describe{
#'   \item{ref}{trajectory's unique id number as an integer}
#'   \item{lon}{longitude of each position estimate in decimal degrees}
#'   \item{lat}{latitide of each position estimate in decimal degrees}
#'   \item{day}{datetime stamp for each location estimate in POSIXct format following: yyyy-mm-dd hh:mm:ss}
#'   ...
#' }
#' @source \url{https://github.com/HannahCalich/PhysMove/tree/master/data-raw}
"plSample"

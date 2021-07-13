#' Random location data where displacement lengths follow an exponential distribution
#'
#' A data frame containing location data from 5 sample trajectories that follows an exponential distribution.
#'
#' @format A data frame with 2854 rows and 4 variables:
#' \describe{
#'   \item{ref}{trajectory's unique id number as an integer}
#'   \item{lon}{longitude of each position estimate in decimal degrees}
#'   \item{lat}{latitide of each position estimate in decimal degrees}
#'   \item{day}{datetime stamp for each location estimate in POSIXct format following: yyyy-mm-dd hh:mm:ss}
#'   ...
#' }
#' @source \url{https://github.com/HannahCalich/PhysMove/tree/master/data-raw}
"expSample_v2"

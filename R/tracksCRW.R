#' Sample location data to demonstrate PhysMove functions
#'
#' A data frame containing location data from 25 sample trajectories
#' that were developed to follow a correlated random walk model using
#' the aniMotum package.
#'
#' Reference: https://ianjonsen.r-universe.dev/aniMotum
#'
#' @format A data frame with 15623 rows and 4 variables:
#' \describe{
#'   \item{ref}{trajectory's unique id number as an integer}
#'   \item{lon}{longitude of each position estimate in decimal degrees}
#'   \item{lat}{latitude of each position estimate in decimal degrees}
#'   \item{day}{datetime stamp for each location estimate in POSIXct format following: yyyy-mm-dd hh:mm:ss}
#'   ...
#' }
"tracksCRW"

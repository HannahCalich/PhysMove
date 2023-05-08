#' Fit distributions to displacements
#'
#' This function allows you to fit power law, exponential, or log-normal distributions to the displacements calculated with
#' the \code{\link{CalcDisp}} function. If displacements were calculated over multiple time windows this function will normalise the
#' displacements by dividing each displacement by the mean displacement of the corresponding time window
#' @param displacements List of displacements output from the \code{\link{CalcDisp}} function.
#' @param dist Continuous distributions that will be fit to the displacements. Possible values are power law ("pl"), exponential ("exp"), or log-normal ("lnorm")
#' continuous distributions. Default is dist=c("pl","exp","lnorm").
#' @param set_dmin To limit the fitted distribution to values above a specified value. Keep in mind that if your data were normalised
#' this value will have to be a normalised value as well. Default is NULL.
#' @param full To fit the distributions to the full range of displacement data. Default is FALSE.
#' @param normalise Normalises the displacement distances by dividing each displacement by the average displacement for that time window
#' normalise=TRUE is required if working with displacements calculated over multiple time windows.
#' @return A data frame that contains the summary statistics for each distribution fit including the distribution name,
#' dmin (the d value used to fit the distribution), parameter 1 (alpha, lambda, mu) and parameter 2 (NA, NA, sigma) for pl, exp, and lnorm
#' distributions respectively, and nTail (the number of data points greater than or equal to dmin). A vector stating if the data were normalised or not,
#' ('normalise') is automatically assigned to the global environment as this information is needed for the \code{\link{CompDist}} and \code{\link{PlotDist}}
#'functions.
#' @examples FitDist(displacements)
#' @examples FitDist(displacements, dist=c("exp","lnorm"), full=TRUE)
#' @examples FitDist(displacements, dist=c("pl","exp","lnorm"), set_dmin=NULL, full=FALSE, normalise=TRUE)
#' @export

check_track <- function (
  
  displacements, dist=c("pl","exp","lnorm"), set_dmin=NULL, full=FALSE, normalise=TRUE) {

  head(tracks)
  
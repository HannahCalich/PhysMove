#' Fit distributions to data
#'
#' This function allows you to fit power law, exponential, or lognormal distributions to a list of values (e.g., displacement data).
#'
#' @param input List of values used to fit the specified distributions; values are combined (and optionally normalised) prior to fitting.
#' @param dist Continuous distributions that will be fit to the data. Possible values are power law ("pl"), exponential ("exp"), or lognormal ("lnorm"). Default is dist=c("pl","exp","lnorm").
#' @param set_dmin To limit the fitted distribution to values above a specified value. If your data are going to be normalised
#' this value will have to be a normalised value as well. Default is NULL.
#' @param full To fit the distributions to the full range of data. Default is FALSE.
#' @param normalise Normalises the input values by dividing each input value by the mean of its corresponding time window;
#' normalise = TRUE is required if working with data calculated over multiple time windows.
#' @return A list including a data frame of summary statistics for each distribution fit (first list element). Results data frame includes the
#' distribution name, dmin (minimum value used to fit each distribution), parameter 1 (alpha, lambda, mu) and parameter 2 (NA, NA, sigma) for pl, exp, and lnorm
#' distributions respectively, and nTail (the number of data points greater than or equal to dmin). A logical argument indicating if
#' data were normalised is exported as the second list element because this information is needed for the \code{\link{compDist}} and \code{\link{plotDist}}
#' functions.
#' @importFrom stats dlnorm plnorm optim sd
#' @examples fitDist(disp, dist=c("pl","exp","lnorm"), full=TRUE)
#' @export

fitDist <- function(input, dist=c("pl","exp","lnorm"), set_dmin=NULL, full=FALSE, normalise=TRUE) {

  if (!(inherits(input, "list"))){ #
    # if the data are in data frame format AND from the occupancy function they can automatically be converted to a list
    if (inherits(input, "data.frame") &
        all(colnames(input)==c("Latitude", "Longitude", "Area", "Counts", "Occupancy"))){
        input <- list(input$Occupancy)
      message("Occupancy data automatically converted to list format")
    } else {
   stop("Distributions can only be fit to data in list format")
    }
  }

  if ((!is.null(set_dmin)) & (full==TRUE)){
    stop("To fit distributions to the full range of data use full=TRUE and leave set_dmin as default (NULL)")
  }

  if ((length(input)>1) & (normalise==FALSE)){
    stop("Data must be normalised for data from multiple time windows to be collated into 1 dataset")
  }

  if (("pl" %in% dist|"exp" %in% dist|"lnorm" %in% dist)!=TRUE){
    stop("Distributions can only be fit to 'pl','exp', or 'lnorm' distributions")
  }

  if (normalise){
    x <- unlist(lapply(input, function(vals) vals / mean(vals)))
  } else {
    x <- unlist(input)
  }

  x <- sort(x)

  if (length(input) > 1) {
    warning(
      "Fitting distributions across multiple time windows can be computationally intensive because all values are combined into a single dataset. Consider fitting distributions to a single time window to reduce run time.",
      call. = FALSE
    )
  }

  distResults <- data.frame("distribution"=dist, "dmin"= c(NA), "parameter1"=c(NA), "parameter2"=c(NA), "nTail"= c(NA)) #make sure dist= is loaded
  rev.index <- rev(seq_along(x))
  eps <- .Machine$double.eps^0.5

  if ("pl" %in% dist){
    message("Fitting a power law distribution")
    PL <- NULL
    if (full==FALSE){
      if (is.null(set_dmin)){
        con_pl <- poweRlaw::conpl$new(x)
        PL <- poweRlaw::estimate_xmin(con_pl)
        PL_dmin <- PL$xmin
        PL_alpha <- PL$pars
        n <- PL$ntail
      }
      if (!is.null(set_dmin)){
        PL_dmin <- set_dmin # If dmin is supplied, assign it as the PL_dmin
      }
    }
    if (full==TRUE){
      PL_dmin <- min(x) #for full, min val = dmin
    }

    if (is.null(PL)){ ## if parameters haven't been calculated already, calc alpha and ntail
      con_pl <- poweRlaw::conpl$new(x) ## create continuous pl object
      con_pl$setXmin(PL_dmin) ## set dmin as defined above
      PL <- poweRlaw::estimate_pars(con_pl) ## estimate alpha
      PL_alpha <- PL$pars
      selection <- which(x >= (PL_dmin - eps))[1]
      n <- rev.index[selection] # number of values of x >= dmin
    }

    distResults$dmin[distResults$distribution == "pl"] <- PL_dmin
    distResults$parameter1[distResults$distribution == "pl"] <- PL_alpha
    distResults$nTail[distResults$distribution == "pl"] <- n
  }

  if ("exp" %in% dist){
    message("Fitting an exponential distribution")
    EX <- NULL
    if (full==FALSE){
      if (is.null(set_dmin)){
        con_exp <- poweRlaw::conexp$new(x)
        EX <- poweRlaw::estimate_xmin(con_exp)
        Exp_dmin <- EX$xmin
        Exp_lambda <- EX$pars
        n <- EX$ntail
      }
      if (!is.null(set_dmin)){
        Exp_dmin <- set_dmin # If dmin is supplied, assign it as the PL_dmin
      }
    }
    if (full==TRUE){
      Exp_dmin <- min(x) #for full, min val = dmin
    }

    if (is.null(EX)){ ## if parameters haven't been calculated already, calc alpha and ntail
      con_exp <- poweRlaw::conexp$new(x) ## create continuous pl object
      con_exp$setXmin(Exp_dmin) ## set dmin as defined above
      EX <- poweRlaw::estimate_pars(con_exp) ## estimate alpha
      Exp_lambda <- EX$pars
      selection <- which(x >= (Exp_dmin - eps))[1]
      n <- rev.index[selection] # number of values of x >= dmin
    }
    distResults$dmin[distResults$distribution == "exp"] <- Exp_dmin
    distResults$parameter1[distResults$distribution == "exp"] <- Exp_lambda
    distResults$nTail[distResults$distribution == "exp"] <- n
  }

  if ("lnorm" %in% dist){
    message("Fitting a lognormal distribution")
    LN <- NULL
    if (full==FALSE){
      if (is.null(set_dmin)){
        con_ln <- poweRlaw::conlnorm$new(x)
        LN <- poweRlaw::estimate_xmin(con_ln)
        LN_dmin <- LN$xmin
        lnorm_pars <- LN$pars
        n <- LN$ntail
      }
      if (!is.null(set_dmin)){
        LN_dmin <- set_dmin # If dmin is supplied, assign it as the PL_dmin
      }
      }
      if (full==TRUE){
        LN_dmin <- min(x) #for full, min val = dmin
      }

      if (is.null(LN)){ ## if parameters haven't been calculated already, calc alpha and ntail
        con_ln <- poweRlaw::conlnorm$new(x) ## create continuous pl object
        con_ln$setXmin(LN_dmin) ## set dmin as defined above
        LN <- poweRlaw::estimate_pars(con_ln) ## estimate alpha
        lnorm_pars <- LN$pars
        selection <- which(x >= (LN_dmin - eps))[1]
        n <- rev.index[selection] # number of values of x >= dmin
      }
    distResults$dmin[distResults$distribution == "lnorm"] <- LN_dmin
    distResults$parameter1[distResults$distribution == "lnorm"] <- lnorm_pars[1]
    distResults$parameter2[distResults$distribution == "lnorm"] <- lnorm_pars[2]
    distResults$nTail[distResults$distribution == "lnorm"] <- n
  }

  distResults <- list(distResults, normalise)
  names(distResults) <- c("distResults", "normalise")
  return(distResults)
}

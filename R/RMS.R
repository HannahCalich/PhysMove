#' Root-Mean-Square of Displacements
#'
#' This function allows you to calculate root-mean-square displacements and plot them scaled with time
#' @param species_df A data frame containing location data in rows. Columns have the following headers: "ref", "lon", "lat", "day".
#' "ref" is the unique id number for each animal (e.g., their satellite tag number formatted as an integer),
#' "lon" and "lat" are the longitude and latitude of each position estimate in decimal degrees in numeric format,
#' "day" is the datetime stamp for each location estimate in POSIXct format following yyyy-mm-dd hh:mm:ss.
#' See attached sample data \code{\link{tracks}}.
#' @param timeUnit Unit used to calculate time between locations (e.g., "secs", "mins", "hours", "days", "weeks"). Default is "days".
#' @param wBins Bin width refers to the size of the time bins used to calculate how frequently displacements occurred. Default is 1.1
#' @param plot Plot the root-mean-square and mean displacements against their corresponding time periods. Default is TRUE.
#' @param lm Calculate a linear regression to examine the relationship between the root-mean-square displacement values (target variable)
#' and time (predictor variable) and add fit line to the plot (if plot=TRUE). Default is TRUE.
#' @param strict If TRUE, abort with a detailed error when invalid time/displacement pairs are found; if FALSE, warn and
#' proceed using only valid pairs (up to 25 examples shown). Default is TRUE.
#' @return List containing a dataframe of results (list element 1) and the results of the linear model (if lm = TRUE, list element 2).
#' The results dataframe includes the 'timeWindows' in log-sized bins along with their corresponding 'meanDisplacements' and 'rmsDisplacements'
#' (root-mean-square displacements). If plot = TRUE, a plot of the mean displacement values and the root-mean-square displacement values
#' against their corresponding time period is created, and if lm = TRUE, a fit line and reference line are added to the plot.
#' @examples
#' \dontrun{
#'
#' rms(tracks, timeUnit="days", wBins=1.1, plot=TRUE, lm=TRUE)
#'
#' }
#' @export

rms <- function (species_df, timeUnit="days", wBins=1.1, plot=TRUE, lm=TRUE, strict=TRUE){

  Radius <- 6371 # Earth Radius in km (disp are in km)
  rad <- 3.141592653589793/180 # Python has more digits of pi than R, so value pasted here instead of "pi" for consistency with python versions of code
  bins <- seq(1,400,1)
  nbins <- length(bins) # avoid multiple calls to length(bins)
  tmin <- 1/(60*60*24) # 1 second in days
  max_examples <- 25

  # Identify end points for each track
  ids_chr <- as.character(species_df[[1]])
  runs <- rle(ids_chr) # length of each track
  end_idx <- cumsum(runs$lengths) # last loc per track
  nvec <- rep(end_idx, times = runs$lengths) # index of where each individual's last row ends

  # Compute numeric cols
  lon <- as.numeric(species_df[[2]])
  lat <- as.numeric(species_df[[3]])
  time <- species_df[[4]]
  radlat <- rad * lat
  cos_radlat <- cos(radlat)

  # Vars that accumulate in loop below
  sumDist2 <- sumDist <- Timefreq <- rep(0, nbins)

  # Status messages
  statusMessages <- c("25% complete", "50% complete", "75% complete")
  percent <- c(round(dim(species_df)[1]*0.25),round(dim(species_df)[1]*0.5),round(dim(species_df)[1]*0.75))
  p <- j <- 1

  # Create list to store errors
  invalid_list <- vector("list", length = 0L)

  # Calc disp and time bins
  for (j in seq_len(nrow(species_df))) {
    if (p <= length(percent) && j == percent[p]) { # if we've reached the 25%, 50%, or 75% position, and we haven't sent all 3 messages yet, send msg
      message(statusMessages[p])
      p <- p + 1
    }

    n <- nvec[j]  # last row for the individual
    if (j + 1 <= n) { # for all rows that are not the last row
      idx <- (j + 1):n # index the rows excluding row j and prior rows
      myTime <- as.numeric(difftime(time[idx], time[j], units = timeUnit)) # Vector of all time differences relative to j
      bvec <- floor(log(myTime / tmin) / log(wBins) + 0.5) # log scale time bin in vector

      # Review data to skip invalid data
      reason_nonpos_time  <- !is.na(myTime) & (myTime <= 0)
      reason_nonfinite_bin <- !is.finite(bvec)
      reason_oob_bin       <- is.finite(bvec) & (bvec < 1 | bvec > nbins)

      any_invalid <- any(reason_nonpos_time | reason_nonfinite_bin | reason_oob_bin)
      if (any_invalid) {
        bad <- which(reason_nonpos_time | reason_nonfinite_bin | reason_oob_bin)
        inv <- data.frame(
          ref = ids_chr[j],
          loc_a = j,
          loc_b = idx[bad],
          time = myTime[bad],
          reason = ifelse(reason_nonpos_time[bad], "non_positive_time",
                          ifelse(reason_nonfinite_bin[bad], "non_finite_bin",
                                 ifelse(reason_oob_bin[bad], "bin_out_of_range", "unknown")))
        )
        invalid_list[[length(invalid_list) + 1L]] <- inv
      }

      # Proceed with valid data
      valid <- !(reason_nonpos_time | reason_nonfinite_bin | reason_oob_bin)
      if (any(valid)) {
        idxv <- idx[valid]
        bvec_ok <- bvec[valid]

        # Vectorized Haversine from j to all idxv --
        dlat <- radlat[idxv] - radlat[j]
        dlon <- rad * (lon[idxv] - lon[j])
        a <- (sin(dlat / 2)^2) + cos_radlat[j] * cos_radlat[idxv] * (sin(dlon / 2)^2)
        # clip for numerical safety (avoids tiny >1 due to floating point):
        a <- pmax(0, a)
        sqrt_a <- sqrt(a)
        # asin input must be <= 1; guard against minimal overshoot
        angle <- 2 * asin(pmin(1, sqrt_a))
        dist <- angle * Radius
        ## NEED TO CHECK ABOVE compared to:
        # Dist <- MydistHaversine(species_df[k,2], species_df[k,3], species_df[j,2], species_df[j,3]) # Calculates distance from point 1 to all successive points

        Timefreq <- Timefreq + tabulate(bvec_ok, nbins) # Cumulative count of displacements between each location within each log time bin
        s  <- tapply(dist,   bvec_ok, sum)
        s2 <- tapply(dist^2, bvec_ok, sum)
        buniq <- as.integer(names(s))
        if (length(buniq)) {
          sumDist[buniq]  <- sumDist[buniq]  + as.numeric(s)
          sumDist2[buniq] <- sumDist2[buniq] + as.numeric(s2)
        }
      }
    }
  }

  if (length(invalid_list)) { ## If invalid vals are found
    invalids <- do.call(rbind, invalid_list)
    invalids <- invalids[order(invalids$ref, invalids$loc_a, invalids$loc_b), ]
    n_invalid <- nrow(invalids)

    summary_counts <- aggregate(loc_b ~ reason, invalids, length)
    summary_text <- paste0(
      "Invalid pair(s) detected: ", n_invalid, " total\n",
      paste(sprintf(" - %s: %d", summary_counts$reason, summary_counts$k), collapse = "\n")
    )

    head_examples <- utils::head(invalids, max_examples)
    example_text <- paste(
      capture.output(print(head_examples, row.names = FALSE)),
      collapse = "\n"
    )
    tail_note <- if (n_invalid > max_examples)
      sprintf("\n… plus %d more invalid pair(s) not shown.", n_invalid - max_examples) else ""

    msg <- paste0(
      summary_text, "\n\nExamples (ref, location a, location b, time difference, reason):\n",
      example_text, tail_note, "\n\n",
      "Tip: Review the locations identified above to ensure there are no duplicated rows and all
      values are valid"
    )

    if (strict) {
      stop(msg, call. = FALSE)
    } else {
      warning(msg, call. = FALSE)
    }
  }

  message ("Calculations complete")

  mybins <- rep(0, nbins) # for the x axis of the plot
  for(b in 1: nbins){
    mybins[b] <- tmin*wBins^(b)
  }
  RMS_Result <- as.data.frame(cbind("timeBin_log"=mybins, "Count"=Timefreq, "sumDist"=sumDist, "sumDist2"=sumDist2))
  RMS_Result <- RMS_Result[(RMS_Result[,1]!= 0) & (RMS_Result[,3]!= 0) & (RMS_Result[,4]!= 0),]
  RMS_Result$MeanDisp_per_tb <- RMS_Result[,3]/RMS_Result[,2]
  RMS_Result$Sqrt_dRMS_per_tb <- sqrt(RMS_Result[,4]/RMS_Result[,2])
  plot.df <- cbind(RMS_Result[,c(1,5,6)])
  names(plot.df) <- c("timeWindow", "meanDisplacements", "rmsDisplacements")

  if (plot==TRUE){
    if (nrow(plot.df)>1){
      ylabel <- expression('<'*d^q*'>'^(1/q)* (km)) #generic expression = ('<'*d^q*'>'^(1/q)* (km)), q=1 is mean disp, and q=2 is RMS
      xlabel <- paste('T(',timeUnit,')',sep="")
      a <- ggplot2::ggplot(plot.df, ggplot2::aes(plot.df[,1],plot.df[,3]))
      if (lm==TRUE){
      a <- a +
        ggplot2::stat_smooth(formula = y ~ x, method="lm", col="red")
      }
      a <- a +
        ggplot2::geom_point(ggplot2::aes(y = plot.df[,2], colour = "Mean disp.")) +
        ggplot2::geom_point(ggplot2::aes(y = plot.df[,3], colour = "RMS disp."))+
        ggplot2::scale_colour_manual(values = c("dark grey", "black"), guide = ggplot2::guide_legend(reverse = TRUE))+
        ggplot2::scale_x_log10(
          breaks = function(x) {
            brks <- scales::extended_breaks(Q = c(1, 5))(log10(x))
            10^(brks[brks %% 1 == 0])
          },
          labels = scales::math_format(format = log10)
        ) +
        ggplot2::scale_y_log10(
          breaks = function(x) {
            brks <- scales::extended_breaks(Q = c(1, 5))(log10(x))
            10^(brks[brks %% 1 == 0])
          },
          labels = scales::math_format(format = log10)
        ) +
        ggplot2::theme_bw(base_size = 12)+
        ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                            panel.grid.minor = ggplot2::element_blank(), axis.line = ggplot2::element_line(colour = "black"),
                                            axis.text.x = ggplot2::element_text(margin = ggplot2::margin(t = 10), colour = "black"),
                                            axis.text.y = ggplot2::element_text(margin = ggplot2::margin(r = 10), colour = "black"),
                                            legend.position = "inside", legend.position.inside = c(0, 1), legend.justification = c(0, 1), legend.direction = 'vertical',
                                            legend.background =ggplot2::element_blank(), legend.title = ggplot2::element_blank())+
        ggplot2::annotation_logticks(short=grid::unit(-0.1, "cm"), mid=grid::unit(-0.1, "cm"), long=grid::unit(-0.3,"cm")) +
        ggplot2::coord_cartesian(clip="off")+
        ggplot2::xlab(xlabel)+
        ggplot2::ylab(ylabel)
      plot(a)
    } else {
      warning("Not enough data to create plot")
    }
  }

  plot.df <- list(plot.df)
  names(plot.df) <- "rmsResults"

  if (lm==TRUE){
    if (nrow(RMS_Result)>1){
      fit <- lm(log(RMS_Result$Sqrt_dRMS_per_tb) ~ log(RMS_Result$timeBin_log), data = RMS_Result)
      plot.df[[2]] <- as.data.frame(broom::tidy(fit))
      names(plot.df) <- c("rmsResults", "lm")
      message("Hurst exponent = ", round(fit$coefficients[[2]],4))
      rm(fit)
    } else {
      warning("Not enough data fit linear model")
    }
  }

  return(plot.df)
}

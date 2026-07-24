#' Check data format
#' This function checks the format of telemetry data prior to running PhysMove 
#' metrics.
#' @param species_df A data frame containing telemetry data with columns named 
#' ref, lon, lat, and day.
#' @param verbose Logical. If TRUE feedback is provided on your dataset. Default 
#' is TRUE.   
#' @details
#' The columns must be formatted as follows:
#' ref: numeric ID for each individual.
#' lon and lat: numeric longitude and latitude in decimal degrees.
#' day: POSIXct datetime values.
#' Datetime format: %Y-%m-%d %H:%M:%S
#' @return Invisibly returns an integer error count (0 if no issues found), in 
#' addition to printing diagnostic messages/warnings.
#' @importFrom methods is
#' @examples
#' checkTracks(tracks)
#' @export

checkTracks <- function(species_df, verbose=TRUE){

  error_count <- 0

  if (!(methods::is(species_df, "data.frame"))) {
    if (verbose) warning("Input data must be a data frame")
    if (verbose) message("Please review formatting requirements")
    return(invisible(1)) # stop the function here, no point in proceeding
  }

  # Prevent non-dataframe subclasses
  if (!identical(class(species_df), "data.frame")) {
    if (verbose) warning("Input data must be a base data.frame. 
              Tibbles or data.frame subclasses are not supported.
              Use as.data.frame() before proceeding.")
      error_count <- error_count + 1
  }

  # Col names must match exactly
  expected_cols <- c("ref", "lon", "lat", "day")
  if (!identical(colnames(species_df), expected_cols)) {
    if (verbose) warning("Column names are either incorrect or in the wrong order. ",
    "Column names must be: ref, lon, lat, day (in that order)")
    error_count <- error_count+1
    if (verbose) message("Please review formatting requirements")
    return(invisible(error_count)) # stop the function here, no point in proceeding
  }

  # Check ref col
  if (!is.numeric(species_df$ref)) {
    if (verbose) warning("ref column must be numeric format")
    error_count <- error_count+1
  } else if (anyNA(species_df$ref)) {
    if (verbose) warning("ref column contains missing (NA) values")
    error_count <- error_count + 1
  }

  # Check lon col
  if (!is.numeric(species_df$lon)) {
    if (verbose) warning("lon column must be numeric format")
    error_count <- error_count+1
  } else {
    if (anyNA(species_df$lon)) {
      if (verbose) warning("lon column contains missing (NA) values")
      error_count <- error_count + 1
    }
    if (any(species_df$lon > 180, na.rm = TRUE)) {
      if (verbose) warning("longitude value greater than 180")
      error_count <- error_count + 1
    }
    if (any(species_df$lon < -180, na.rm = TRUE)) {
      if (verbose) warning("longitude value less than -180")
      error_count <- error_count + 1
    }
  }

  # Check lat col
  if (!is.numeric(species_df$lat)) {
    if (verbose) warning("lat column must be numeric format")
    error_count <- error_count+1
  } else {
    if (anyNA(species_df$lat)) {
      if (verbose) warning("lat column contains missing (NA) values")
      error_count <- error_count + 1
    }
    if (any(species_df$lat > 90, na.rm = TRUE)) {
      if (verbose) warning("latitude value greater than 90")
      error_count <- error_count + 1
    }
    if (any(species_df$lat < -90, na.rm = TRUE)) {
      if (verbose) warning("latitude value less than -90")
      error_count <- error_count + 1
    }
  }

  # Check day col
  if (!methods::is(species_df$day, "POSIXct")) {
    if (verbose) warning("day column must be POSIXct format (e.g., convert with ",
            "as.POSIXct(x, format = '%Y-%m-%d %H:%M:%S'))")
    error_count <- error_count + 1
  } else if (anyNA(species_df$day)) {
    if (verbose) warning("day column contains missing (NA) values")
    error_count <- error_count + 1
  }

  # Check sort order (to ensure rows are sorted by ref, then by day within each ref)
  if (is.numeric(species_df$ref) && !anyNA(species_df$ref) &&
      methods::is(species_df$day, "POSIXct") && !anyNA(species_df$day)) {
    
    # each ref should appear as one unbroken run of rows
    ref_contiguous <- identical(rle(species_df$ref)$values, unique(species_df$ref))
    
    # increasing day within each ref's existing row order
    day_increasing <- all(unlist(tapply(species_df$day, species_df$ref,
                                        function(x) all(diff(x) > 0))))
    
    if (!ref_contiguous || !day_increasing) {
      if (verbose) warning("Data are not sorted by ref then day. Rows for the same individual (ref) ",
              "should be grouped together and in increasing chronological order (day). ",
              "Unsorted data may produce incorrect or misleading results. Sort with: ",
              "species_df <- species_df[order(species_df$ref, species_df$day), ]")
      error_count <- error_count + 1
    }
  }
  
  # Check for duplicated timestamps within a ref
  if (is.numeric(species_df$ref) && !anyNA(species_df$ref) &&
      methods::is(species_df$day, "POSIXct") && !anyNA(species_df$day)) {
    has_dup <- tapply(species_df$day, species_df$ref, function(x) anyDuplicated(x) > 0)
    if (any(has_dup)) {
      if (verbose) warning("One or more individuals (ref) have duplicate timestamps in the day column. ",
              "This will cause errors or unreliable results in downstream PhysMove functions.")
      error_count <- error_count + 1
    }
  }
  
  if (error_count == 0) {
    if (verbose) message("Your data are formatted correctly")
  } else {
    if (verbose) message("Please review formatting requirements")
  }

  invisible(error_count)
}

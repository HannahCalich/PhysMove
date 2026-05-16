#' Check data format
#' This function checks the format of telemetry data prior to running PhysMove metrics.
#' @param species_df A data frame containing telemetry data with columns named
#'   ref, lon, lat, and day.
#' @details
#' The columns must be formatted as follows:
#' ref: numeric ID for each individual.
#' lon and lat: numeric longitude and latitude in decimal degrees.
#' day: POSIXct datetime values.
#' Datetime format: %Y-%m-%d %H:%M:%S
#' @return Messages indicating whether the data are correctly formatted; warnings describe any issues.
#' @importFrom methods is
#' @examples
#' checkTracks(tracks)
#' @export

checkTracks <- function(species_df){

  error_count <- 0

  if (!(methods::is(species_df, "data.frame"))) {
    warning("Input data must be a data frame")
    error_count <- error_count+1
  }

  # Prevent non-data frame subclasses
  if (!identical(class(species_df), "data.frame")) {
      warning("Input data must be a base data.frame. Tibbles or
              data.frame subclasses are not supported.
              Use as.data.frame() before proceeding.")
      error_count <- error_count + 1
  }


  if (any(colnames(species_df)!=c("ref","lon","lat","day"))) {
    warning("Column names are either incorrect or in the wrong order. Column names must be: ",
            "ref, ", "lon, ", "lat, " ,"day", " (in that order)")
    error_count <- error_count+1
  }

  if (!(is(species_df$ref, "numeric"))) {
    warning("ref column must be numeric format")
    error_count <- error_count+1
  }

  if (!(is(species_df$lon, "numeric"))) {
    warning("lon column must be numeric format")
    error_count <- error_count+1
  }

  if (max(species_df$lon)>180) {
    warning("longitude value greater than 180")
    error_count <- error_count+1
  }

  if (min(species_df$lon)< -180) {
    warning("longitude value less than -180")
    error_count <- error_count+1
  }

  if (!(is(species_df$lat, "numeric"))) {
    warning("lat column must be numeric format")
    error_count <- error_count+1
  }

  if (max(species_df$lat)>90) {
    warning("latitude value greater than 90")
    error_count <- error_count+1
  }

  if (min(species_df$lat)< -90) {
    warning("latitude value less than -90")
    error_count <- error_count+1
  }

  if (!(is(species_df$day,"POSIXct"))) {
    warning("day column must be POSIXct format")
    error_count <- error_count+1
  }

  out <- tryCatch({
      test <- as.POSIXct(species_df$day, format='%Y-%m-%d %H:%M:%S')
    },
    error=function(cond) {
      warning("day column is not formatted correctly, format must = '%Y-%m-%d %H:%M:%S'")
    })

  if (exists("test") && all(test != species_df$day)) {
      error_count <- error_count+1
      return(out)
    }

  if (error_count == 0)
    message("Your data are formatted correctly")

  if (error_count > 0){
    message(paste0("Please review formatting requirements: "))
  }
}

#' Import BirdNET data into R
#'
#' @param files Files to be imported
#' @param format Character. The file format; either "csv" or "txt".
#' @param conf Character. Filter by confidence level. Defaults to 0.5 (50%).
#'
#' @returns
#' @import warbleR dplyr tidyr lubridate
#' @export
#'
#' @examples
import_birdnet <- function(files,
                           format = "csv",
                           conf = 0.5) {

  dfs <- lapply(files, function(file) {


    if (format == 'txt') {
      df <- read.delim(file, header = TRUE, sep = "\t", dec = ".")
    } else {
      df <- read.csv(file, header = TRUE)
    }

    # Only add the filename column if the dataframe is not empty
    if(nrow(df) > 0) {
      df$filename <- substr(basename(file), 1, 24)

      df <- df |>
        # Use extract to split the filename column
        tidyr::extract(filename, into = c("sensor.id", "date", "time"),
                       regex = "^(.*)_([0-9]{8})_([0-9]{6})$", remove = FALSE) |>
        mutate(
          # Convert date and time into ymd_hms format
          datetime = ymd_hms(paste(date, substr(time, 1, 2), ":",
                                   substr(time, 3, 4), ":",
                                   substr(time, 5, 6),
                                   sep = " "))
        ) |>
        select(filename, sensor.id, date, time, datetime, everything())  # Rearrange columns
    }
    return(df)
  })

  # Filter out any empty data frames before binding rows
  dfs <- Filter(function(df) nrow(df) > 0, dfs)

  common_cols <- Reduce(intersect, lapply(dfs, names))
  merged_df <- do.call(rbind, lapply(dfs, function(x) x[common_cols]))
  # merged_df <- do.call(rbind,dfs)

  merged_df$date <- as_date(merged_df$date)

  # Apply filter by confidence
  merged_df2 <- merged_df |>
    filter(Confidence >= conf)

  return(as_tibble(merged_df2))
}

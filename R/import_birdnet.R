#' Import BirdNET data into R
#'
#' @param files Files to be imported
#' @param format Character. The file format; either "csv" or "txt".
#' @param conf Character. Filter by confidence level. Defaults to 0.5 (50%).
#' @param combined Logical. Whether to analyze a single combined CSV. Default is
#' TRUE.
#'
#' @returns a tibble.
#' @import dplyr tidyr lubridate stringr
#' @export
#'
#' @examples
#'  \dontrun{
#'  csvs <- list_csvs()
#'  import_birdnet(csvs)
#'  }
import_birdnet <- function(files,
                           format = "csv",
                           conf = 0.5,
                           combined = TRUE) {
  if (combined) {
    # Handle multiple CSV files and combine them
    dfs <- lapply(files, function(file) {
      if (format == 'txt') {
        df <- read.delim(file, header = TRUE, sep = "\t", dec = ".")
      } else {
        df <- read.csv(file, header = TRUE)
      }
      if (nrow(df) > 0) {
        # Check if File column exists (for pre-combined files) or create filename from file path
        if ("File" %in% names(df)) {
          # Extract filename from File column and process
          df <- df |>
            mutate(
              filename = str_remove(basename(File), "\\.wav$"),
              sensor.id = str_extract(filename, "^[^_]+"),
              date = str_extract(filename, "(?<=_)[0-9]{8}(?=_)"),  # 8 digits between "_"
              time = str_extract(filename, "(?<=_)[0-9]{6}(?=\\.wav|$)"),
              datetime = ymd_hms(paste(date, substr(time, 1, 2), ":",
                                       substr(time, 3, 4), ":",
                                       substr(time, 5, 6),
                                       sep = " "))
            ) |>
            select(filename, sensor.id, date, time, datetime, everything())
        } else {
          # Use individual file processing approach
          df$filename <- substr(basename(file), 1, 24)
          df <- df |>
            tidyr::extract(filename, into = c("sensor.id", "date", "time"),
                           regex = "^(.*)_([0-9]{8})_([0-9]{6})$", remove = FALSE) |>
            mutate(
              datetime = ymd_hms(paste(date, substr(time, 1, 2), ":",
                                       substr(time, 3, 4), ":",
                                       substr(time, 5, 6),
                                       sep = " "))
            ) |>
            select(filename, sensor.id, date, time, datetime, everything())
        }
      }
      return(df)
    })

    # Filter out empty dataframes
    dfs <- Filter(function(df) nrow(df) > 0, dfs)

    # Find common columns and combine
    if (length(dfs) > 0) {
      common_cols <- Reduce(intersect, lapply(dfs, names))
      merged_df <- do.call(rbind, lapply(dfs, function(x) x[common_cols]))
    } else {
      # Return empty tibble with expected structure if no data
      merged_df <- tibble()
    }

  } else {
    # Original processing for multiple files
    dfs <- lapply(files, function(file) {
      if (format == 'txt') {
        df <- read.delim(file, header = TRUE, sep = "\t", dec = ".")
      } else {
        df <- read.csv(file, header = TRUE)
      }
      if(nrow(df) > 0) {
        df$filename <- substr(basename(file), 1, 24)
        df <- df |>
          tidyr::extract(filename, into = c("sensor.id", "date", "time"),
                         regex = "^(.*)_([0-9]{8})_([0-9]{6})$", remove = FALSE) |>
          mutate(
            datetime = ymd_hms(paste(date, substr(time, 1, 2), ":",
                                     substr(time, 3, 4), ":",
                                     substr(time, 5, 6),
                                     sep = " "))
          ) |>
          select(filename, sensor.id, date, time, datetime, everything())
      }
      return(df)
    })
    dfs <- Filter(function(df) nrow(df) > 0, dfs)
    common_cols <- Reduce(intersect, lapply(dfs, names))
    merged_df <- do.call(rbind, lapply(dfs, function(x) x[common_cols]))
  }

  # Only proceed with filtering if we have data
  if (nrow(merged_df) > 0) {
    merged_df$date <- as_date(merged_df$date)
    merged_df2 <- merged_df |>
      filter(Confidence >= conf)
    return(as_tibble(merged_df2))
  } else {
    return(as_tibble(merged_df))
  }
}

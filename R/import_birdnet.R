#' Import BirdNET data into R
#'
#' @param files Files to be imported
#' @param format Character. The file format; either "csv" or "txt".
#' @param conf Character. Filter by confidence level. Defaults to 0.5 (50%).
#' @param combined Logical. Whether to analyze a single combined CSV. Default is
#' TRUE.
#'
#' @returns a tibble.
#' @import warbleR dplyr tidyr lubridate stringr
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
    # Handle combined CSV case
    if (length(files) != 1) {
      warning("For combined=TRUE, only one file should be provided. Using the first file.")
      files <- files[1]
    }

    if (format == 'txt') {
      df <- read.delim(files, header = TRUE, sep = "\t", dec = ".")
    } else {
      df <- read.csv(files, header = TRUE)
    }

    if (nrow(df) > 0) {
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
    }

    merged_df <- df
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

  merged_df$date <- as_date(merged_df$date)

  merged_df2 <- merged_df |>
    filter(Confidence >= conf)

  return(as_tibble(merged_df2))
}

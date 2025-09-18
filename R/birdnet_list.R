#' List of species detected by BirdNET
#'
#' @param df A data frame with the detections
#' @param sort Character. Indicates how the list should be sorted out. Use
#' "n.days" for number of days detected, "n.calls" for number of calls detected,
#' or "call.rate" for call rate.
#'
#' @returns A data frame.
#' @import dplyr lubridate
#' @export
#'
#' @examples
#' \dontrun{
#' birdListSundarban <- birdnetList(BirdnetDetections)
#' }
birdnet_list <- function(df, sort = "n.days") {

  # Calculate the total calls per week for each species
  calls_per_week <- df |>
    group_by(Scientific.name, Common.name,
             week = floor_date(date, "week")) |>
    summarize(
      weekly_calls = n(), # Total number of detections per week
      .groups = 'drop'
    )

  # Identify the week with the maximum calls for each species
  peak_week <- calls_per_week |>
    group_by(Scientific.name, Common.name) |>
    slice_max(order_by = weekly_calls, n = 1) |>
    select(-weekly_calls) |>
    rename(peak.week = week) |>
    distinct(Scientific.name, Common.name, .keep_all = TRUE)

  # Call activity summary
  data <- df |>
    group_by(Scientific.name, Common.name) |>
    summarize(
      n.days = n_distinct(date), # Number of unique days
      n.calls = n(), # Total number of detections
      .groups = 'drop'
    ) |>
    mutate(call.rate = round(n.calls / n.days))

  # Join the original data with the peak week information
  data <- data |>
    inner_join(peak_week, by = c("Scientific.name", "Common.name"))

  # Calculate the maximum calls per day
  max_calls_per_day <- df |>
    group_by(Scientific.name, Common.name, date) |>
    summarize(
      daily_calls = n(), # Total number of detections per day
      .groups = 'drop'
    ) |>
    group_by(Scientific.name, Common.name) |>
    summarize(
      max.calls.day = max(daily_calls), # Maximum calls in a single day
      peak.day = date[which.max(daily_calls)], # Date of maximum calls
      .groups = 'drop'
    ) |>
    distinct(Scientific.name, Common.name, .keep_all = TRUE)  # Ensure unique rows

  # Join the data with max_calls_per_day
  data <- data |>
    inner_join(max_calls_per_day, by = c("Scientific.name", "Common.name"))

  # Sort the data based on the specified sort parameter
  if (sort == "n.days") {
    data <- data |> arrange(desc(n.days), desc(n.calls))
  } else if (sort == "n.calls") {
    data <- data |> arrange(desc(n.calls), desc(n.days))
  } else if (sort == "call.rate") {
    data <- data |> arrange(desc(call.rate), desc(n.calls), desc(n.days))
  }

  return(data)
}


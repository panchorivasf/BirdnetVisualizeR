#' Create a heatmap of bird vocal activity by month and hour
#'
#' This function generates a heatmap visualization showing patterns of bird
#' vocal activity across different hours of the day and months of the year.
#' The heatmap uses color intensity to represent the frequency of detected
#' vocalizations.
#'
#' @param df A data frame containing bird detection data. Must include a
#' 'datetime' column with POSIXct dates, and either 'Scientific.name' or
#' 'Common.name' columns if filtering by species.
#' @paramspeciesOptional character string specifying the species to plot. Can be
#' either the scientific name or common name (must match values in the data).
#' If NULL (default), includes all species in the data.
#'
#' @return A ggplot object displaying the vocal activity heatmap. The x-axis
#' shows hours of the day (0-23), the y-axis shows months, and tile color
#' represents the count of vocalizations.
#'
#' @details The function:
#' \itemize{
#'   \item Filters data for the specified species (if provided)
#'   \item Aggregates detections by month and hour
#'   \item Creates a complete grid of all month-hour combinations
#'   \item Generates a heatmap using viridis color scale
#' }
#'
#' @examples
#' \dontrun{
#' # Plot vocal activity for a specific species
#' vocal_activity_heatmap(df = BirdnetDetections, species= "Anthus hodgsoni")
#'
#' # Plot vocal activity for all species in the data
#' vocal_activity_heatmap(df = BirdnetDetections)
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise left_join
#' @importFrom ggplot2 ggplot aes geom_tile scale_x_continuous scale_fill_viridis
#' @importFrom ggplot2 theme_minimal theme element_text labs
#' @importFrom lubridate month
#' @importFrom viridis scale_fill_viridis
#' @importFrom tidyr replace_na
#' @importFrom stats complete.cases
#' @export
vocal_activity_heatmap  <- function(df, species = NULL) {

  if(!is.null(sp)){
  # Filter data for the specified species
  df <- df |>
    filter(Scientific.name == species | Common.name == sp)

  }

  # Create a complete grid of months and hours
  months <- factor(month.abb, levels = month.abb)
  hours <- 0:23
  complete_grid <- expand.grid(month = months, hour = hours)

  # Extract month and hour from datetime and count occurrences
  df_vocal_activity <- df  |>
    mutate(month = factor(format(datetime, "%b"), levels = month.abb),
           hour = as.numeric(format(datetime, "%H"))) |>
    group_by(month, hour) |>
    summarise(count = n(), .groups = 'drop')

  # Join the complete grid with the vocal activity data
  vocal_activity_complete <- complete_grid |>
    left_join(df_vocal_activity, by = c("month", "hour")) |>
    replace_na(list(count = 0))  # Replace NA with zero

  # Create the heatmap plot
  plot <- ggplot(vocal_activity_complete, aes(x = hour, y = month,
                                              fill = count)) +
    geom_tile() +
    scale_x_continuous(breaks = 0:23, labels = 0:23) +
    scale_fill_viridis(name = "Call Count", option = "C") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
          plot.title = element_text(size = 14, face = "bold"))

  if(!is.null(sp)){
  plot <- plot +
    labs(x = "Hour of the Day", y = "Month",
         title = paste(sp, "vocal activity"))

  } else {
    plot <- plot +
    labs(x = "Hour of the Day", y = "Month",
         title = "Bird Vocal Activity")

  }

  print(plot)
}


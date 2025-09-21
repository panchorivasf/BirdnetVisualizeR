#' Create a heatmap of bird vocal activity by month and hour of the day
#'
#' This function generates a heatmap visualization showing patterns of bird
#' vocal activity across different hours of the day and months of the year.
#' The heatmap uses color intensity to represent the frequency of detected
#' vocalizations.
#'
#' @param df A data frame containing bird detection data. Must include a
#' 'datetime' column with POSIXct dates, and either 'Scientific.name' or
#' 'Common.name' columns if filtering by species.
#' @param species Optional character string specifying the species to plot. Can be
#' either the scientific name or common name (must match values in the data).
#' If NULL (default), includes all species in the data.
#' @param background_color Character string specifying the color for tiles with
#' no detections (count = 0). Default is "transparent".
#' @param save_png Logical. Whether to save plot as png.
#' @param prefix Characer. Prefix for file name.
#' @param height Numeric. Height in inches.
#' @param width Numeric. Width in inches.
#'
#' @return A ggplot object displaying the vocal activity heatmap. The x-axis
#' shows hours of the day (0-23), the y-axis shows months, and tile color
#' represents the count of vocalizations. Months without any data are shown
#' in light grey.
#'
#' @details The function:
#' \itemize{
#'   \item Filters data for the specified species (if provided)
#'   \item Aggregates detections by month and hour
#'   \item Creates a complete grid of all month-hour combinations
#'   \item Generates a heatmap using viridis color scale
#'   \item Shows months without data in light grey
#'   \item Uses user-specified background color for no detections
#' }
#'
#' @examples
#' \dontrun{
#' # Plot vocal activity for a specific species
#' calendar_heatmap2(df = BirdnetDetections, species= "Anthus hodgsoni")
#'
#' # Plot vocal activity for all species with white background
#' calendar_heatmap2(df = BirdnetDetections, background_color = "white")
#'
#' # Plot with light blue background for zero detections
#' calendar_heatmap2(df = BirdnetDetections, background_color = "lightblue")
#' }
#'
#' @importFrom dplyr filter mutate group_by summarise left_join
#' @importFrom ggplot2 ggplot aes geom_tile scale_x_continuous
#' @importFrom ggplot2 theme_minimal theme element_text labs scale_fill_gradient
#' @importFrom lubridate month
#' @importFrom viridis scale_fill_viridis
#' @importFrom tidyr replace_na
#' @importFrom stats complete.cases
#' @export
calendar_heatmap2 <- function(df, species = NULL,
                              background_color = "transparent",
                              save_png = TRUE,
                              prefix = "",
                              height = 8,
                              width = 15) {
  if(!is.null(species)){
    # Filter data for the specified species
    df <- df |>
      filter(Scientific.name == species | Common.name == species)
  }

  # Create a complete grid of months and hours
  months <- factor(month.abb, levels = month.abb)
  hours <- 0:23
  complete_grid <- expand.grid(month = months, hour = hours)

  # Extract month and hour from datetime and count occurrences
  df_vocal_activity <- df |>
    mutate(month = factor(format(datetime, "%b"), levels = month.abb),
           hour = as.numeric(format(datetime, "%H"))) |>
    group_by(month, hour) |>
    summarise(count = n(), .groups = 'drop')

  # Identify months that have data
  months_with_data <- unique(df_vocal_activity$month)

  # Join the complete grid with the vocal activity data
  vocal_activity_complete <- complete_grid |>
    left_join(df_vocal_activity, by = c("month", "hour")) |>
    mutate(
      count = replace_na(count, 0),  # Replace NA with zero
      # Create a flag for months without any data
      month_has_data = month %in% months_with_data,
      # Create display count: -1 for months without data, actual count otherwise
      display_count = ifelse(month_has_data, count, -1)
    )

  # Determine the maximum count for color scaling
  max_count <- max(vocal_activity_complete$count, na.rm = TRUE)

  # Create the heatmap plot
  plot <- ggplot(vocal_activity_complete, aes(x = hour, y = month, fill = display_count)) +
    geom_tile() +
    scale_x_continuous(breaks = 0:23, labels = 0:23) +
    scale_fill_gradientn(
      name = "Call Count",
      colors = c("lightgrey", background_color, viridis::viridis(100)),
      values = c(-1, 0, seq(0.001, max_count, length.out = 100)) / max(c(max_count, 1)),
      na.value = background_color,
      guide = "colorbar",
      breaks = function(x) {
        # Only show breaks for non-negative values
        all_breaks <- pretty(c(0, max_count))
        all_breaks[all_breaks >= 0]
      },
      labels = function(x) {
        # Only label non-negative values
        ifelse(x >= 0, as.character(round(x)), "")
      }
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),
          plot.title = element_text(size = 14, face = "bold"))

  if(!is.null(species)){
    plot <- plot +
      labs(x = "Hour of the Day", y = "Month",
           title = species,
           subtitle = "Vocal activity")
  } else {
    plot <- plot +
      labs(x = "Hour of the Day", y = "Month",
           title = "All birds",
           subtitle = "Vocal Activity")
  }

  if(save_png) {
    ggsave(filename = paste0(prefix, "_calendar.png"),
           plot = plot, bg = "white",
           width = width, height = height, units = "in",
           dpi = 300)
    cat("Treemap saved as:", paste0(prefix, "_calendar.png"), "\n")
  }

  print(plot)
}

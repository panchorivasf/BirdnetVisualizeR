#' Phenological Chart - Single Species
#'
#' @param df A data frame
#' @param sp Character. The species' binomial (scientific name).
#' @param aggregate_by Character. Level of data aggregataion. Otions are "day"
#' and "week".
#' @param add_sensor_id Logical. Whether to add the sensor ID to the plot.
#'
#' @returns A heatmap  plot.
#' @import ggplot2 dplyr lubridate viridis
#' @export
#'
#' @examples
#' \dontrun{
#' phenochart_singls(data, "Troglodytes aedon",
#'                   aggregate_by = "week",
#'                   add_sensor_id = TRUE)
#' }
phenochart_single <- function(df,
                              sp,
                              aggregate_by = "week",
                              add_sensor_id= FALSE) {

  sensor.id <- unique(df$sensor.id)
  start.date <- min(df$date)
  end.date <- max(df$date)
  # Filter data for the specified species
  df_filtered <- df |>
    filter(Scientific.name == sp | Common.name == sp)

  common.name <- unique(df_filtered$Common.name)[1]
  scientific.name <- unique(df_filtered$Scientific.name)[1]
  unique_years <- unique(format(df_filtered$date, "%Y"))
  sorted_years <- sort(unique_years)
  years_str <- paste(sorted_years, collapse = "-")
  plot.title <- bquote(.(common.name) ~ "(" * italic(.(scientific.name)) * ")" ~ .(years_str))

  # Aggregate data by day or week
  df_aggregated <- df_filtered |>
    mutate(aggregation_date = case_when(
      aggregate_by == "day" ~ as.Date(date),
      aggregate_by == "week" ~ as.Date(floor_date(datetime, "week"))
    )) |>
    group_by(aggregation_date) |>
    summarise(total_calls = n(), .groups = 'drop')

  # Adjust bar widths based on aggregation
  bar_width <- if (aggregate_by == "day") .5 else 3

  # Adjusting data to center bars around zero
  df_aggregated <- df_aggregated |>
    mutate(ymin = -total_calls / 2, ymax = total_calls / 2, xmax = aggregation_date + bar_width, xmin = aggregation_date - bar_width)

  # Create the plot
  plot <- ggplot(df_aggregated) +
    geom_hline(yintercept = 0, color = "grey", size = 0.5, linetype = "dashed") +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = total_calls)) +
    scale_fill_viridis()+
    # scale_fill_gradientn(colors = c(heat.colors(n = 3, rev = TRUE))) +
    theme_light() +
    theme(axis.text.y = element_blank(),
          axis.ticks.y = element_blank(),
          axis.title.y = element_blank(),
          axis.text.x = element_text(angle = 0, hjust = 1),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank()) +
    labs(x = NULL, y = NULL, title = plot.title,
         # subtitle = paste("Total calls by", aggregate_by),
         fill = paste("Calls/", aggregate_by)) +
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 limits = c(start.date, end.date), expand = c(0.01, 0.01))

  if(add_sensor_id== TRUE){
    plot <- plot +
      annotate("text", x = structure(max(df_aggregated$date), class = "Date"), y = 1.5,
               label = sensor.id, hjust = 1, vjust = 1, size = 5, colour = "black")
  }

  print(plot)
}

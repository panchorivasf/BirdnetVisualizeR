#' Interactive Map of Bird Detections
#'
#' @param data A data frame containing birdnet data, sensor.id, lat, and
#' long columns.
#' @param title Character. A title for the legend
#'
#' @returns A Leaflet interactive map with bird detections by sensor.id
#' @export
#'
bird_map <- function(data,
                     title = "Bird Detections"){

  data_n <- data |>
    group_by(sensor.id, lat, long) |>
    tally(sort = TRUE)

  pal_custom <- colorNumeric(
    palette = viridis::viridis(256),
    domain = data_n$n
  )

  # Map it
  leaflet(data_n) |>
    addProviderTiles(providers$Esri.WorldImagery) |>
    addCircleMarkers(
      lng = ~long,
      lat = ~lat,
      radius = 6,
      color = ~pal_custom(n),
      stroke = FALSE,
      fillOpacity = 1,
      popup = ~paste(
        "<b>Sensor ID:</b>", sensor.id, "<br>",
        "<b>N Detections:</b>", n, "<br>"
      ),
      label = ~as.character(paste("Detections:", n))
    ) |>
    addLegendNumeric(
      pal = pal_custom,
      values = data_n$n,
      title = title,
      position = "bottomright",
      orientation = "horizontal",
      width = 180,
      height = 10
    )

}

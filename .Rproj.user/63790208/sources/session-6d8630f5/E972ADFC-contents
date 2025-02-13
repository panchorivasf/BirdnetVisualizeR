#' Top 10 species
#'
#' @description
#' Get a stacked bar plot with top 10 species (i.e., the most detected)
#'
#' @param birdnet_list a data frame obtained with the "birdnet_list" function.
#' @param site_id A string (text) with an identifier for the site, sensor, or
#' plot (use quotations)
#'
#' @import dplyr
#' @import ggplot2
#' @import patchwork
#'
#' @returns a bar plot depicting the 10 most detected species in a site.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' top10statsPlot(S4A17644_list, " - sensor S4A17644")
#' }
topten_plot <- function(birdnet_list, site_id){

  # Create a unified list of top 10 species across all categories
  top_species <- birdnet_list |>
    arrange(desc(n.calls)) |>
    top_n(10, n.calls) |>
    bind_rows(
      birdnet_list |>
        arrange(desc(n.days)) |>
        top_n(10, n.days),
      birdnet_list |>
        arrange(desc(call.rate)) |>
        top_n(10, call.rate)
    ) |>
    distinct(Common.name) |>
    pull(Common.name)

  # Generate a consistent color palette
  species_colors <- setNames(hcl.colors(length(top_species)), top_species)

  # Function to create each plot with the same color palette
  create_plot <- function(data, metric, title, xlab, ylab) {
    ggplot(data, aes(x = reorder(Common.name, !!sym(metric)), y = !!sym(metric), fill = Common.name)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      theme_minimal() +
      labs(title = title, x = xlab, y = ylab) +
      scale_fill_manual(values = species_colors) +
      theme(legend.position = "none")
  }

  # Make the plots
  top10days.plot <- create_plot(birdnet_list |>
                                  arrange(desc(n.days)) |>
                                  top_n(10, n.days),
                                "n.days",
                                paste("Top 10 species ", site_id, sep = "-"),
                                "Species", "Number of Days Detected")

  top10calls.plot <- create_plot(birdnet_list |>
                                   arrange(desc(n.calls)) |>
                                   top_n(10, n.calls),
                                 "n.calls",
                                  "",
                                 "Species", "Number of Calls")

  top10rate.plot <- create_plot(birdnet_list |>
                                  arrange(desc(call.rate)) |>
                                  top_n(10, call.rate),
                                "call.rate",
                                "",
                                "Species", "Call rate (N calls / N days detected)")

  # Combine the plots
  finalPlot <- top10days.plot / top10calls.plot / top10rate.plot

  return(finalPlot)
}



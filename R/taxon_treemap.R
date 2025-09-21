#' Treemap of bird call detections organized by order and species
#'
#' @param data a data frame containing BirdNET detections and taxonomic
#' information, including "order" and "species" columns.
#' @param save_png Logical. Whether to save plot as png.
#' @param prefix Characer. Prefix for file name.
#'
#' @returns a treemap plot
#' @export
#'
taxon_treemap <- function(data, save_png = TRUE, prefix = "") {

  birds_summary <- data |>
    group_by(order, species) |>
    tally()

  # Create ggplot treemap
  tree_plot <- ggplot(birds_summary, aes(area = n, fill = order,
                                         label = species,
                                         subgroup = order)) +
    geom_treemap() +
    geom_treemap_subgroup_border(color = "white", size = 3) +
    geom_treemap_text(aes(label = species),
                      color = "black",
                      size = 9,
                      family = "Arial",
                      fontface = "plain",
                      place = "bottomleft") +
    geom_treemap_subgroup_text(aes(label = order),
                               color = "white",
                               size = 12,
                               family = "Arial",
                               fontface = "bold",
                               place = "center") +
    theme_void() +
    theme(legend.position = "none",
          plot.margin = unit(c(10, 10, 10, 10), "pt"),
          plot.title = element_text(
            hjust = 0.5,
            margin = margin(b = 5, unit = "pt"),
            size = 14,
            face = "bold")) +
    labs(title = "Distribution of detections by Order")

  print(tree_plot)

  if(save_png) {
    ggsave(filename = paste0(prefix, "_treemap.png"),
           plot = tree_plot, bg = "white",
           width = 12, height = 8, units = "in",
           dpi = 300)
    cat("Treemap saved as:", paste0(prefix, "_treemap.png"), "\n")
  }

  return(tree_plot)
}

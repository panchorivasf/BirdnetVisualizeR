#' Create Faceted Barplots for Taxonomic Summary Results
#'
#' This function generates faceted barplots for each table in the taxonomic
#' summary results list, with options to limit the number of bars displayed.
#'
#' @param summary_list A list of taxonomic summary tables (output from taxa_summary functions)
#' @param n_top Number of top items to display in each plot. Use NULL to show all.
#'              Default is 15.
#' @param fill_color Color for the bars. Default is "steelblue".
#' @param base_size Base font size for the plots. Default is 12.
#'
#' @return A list of ggplot objects, one for each summary table
#'
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap labs theme_minimal theme
#'                     element_text
#' @importFrom dplyr slice_max
#' @importFrom purrr map
#' @importFrom rlang .data
#' @export
taxa_plots <- function(summary_list, n_top = 15, fill_color = "steelblue",
                      base_size = 12) {

  # Define plotting function for each type of summary
  create_plot <- function(data, title, x_var, y_var, n_top) {

    # Limit to top n items if requested
    if (!is.null(n_top) && nrow(data) > n_top) {
      data <- data |>
        dplyr::slice_max(order_by = .data[[y_var]], n = n_top)
    }

    # Create the plot
    p <- ggplot2::ggplot(data, ggplot2::aes(x = reorder(.data[[x_var]], -.data[[y_var]]),
                                            y = .data[[y_var]])) +
      ggplot2::geom_col(fill = fill_color, alpha = 0.8) +
      ggplot2::labs(title = title,
                    x = NULL,
                    y = y_var) +
      ggplot2::theme_minimal(base_size = base_size) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold")
      )

    return(p)
  }

  # Create plots for each summary table
  plots <- list()

  # 1. Order Richness
  if ("order_richness" %in% names(summary_list)) {
    plots$order_richness <- create_plot(
      summary_list$order_richness,
      "Species Richness by Order",
      "order",
      "n_species",
      n_top
    )
  }

  # 2. Family Richness
  if ("family_richness" %in% names(summary_list)) {
    plots$family_richness <- create_plot(
      summary_list$family_richness,
      "Species Richness by Family",
      "family",
      "n_species",
      n_top
    ) +
      ggplot2::facet_wrap(~order, scales = "free", ncol = 3)
  }

  # 3. Order Calls
  if ("order_calls" %in% names(summary_list)) {
    plots$order_calls <- create_plot(
      summary_list$order_calls,
      "Number of Calls by Order",
      "order",
      "n_calls",
      n_top
    )
  }

  # 4. Family Calls
  if ("family_calls" %in% names(summary_list)) {
    plots$family_calls <- create_plot(
      summary_list$family_calls,
      "Number of Calls by Family",
      "family",
      "n_calls",
      n_top
    ) +
      ggplot2::facet_wrap(~order, scales = "free", ncol = 3)
  }

  # 5. Order Days
  if ("order_days" %in% names(summary_list)) {
    plots$order_days <- create_plot(
      summary_list$order_days,
      "Number of Detection Days by Order",
      "order",
      "n_days",
      n_top
    )
  }

  # 6. Family Days
  if ("family_days" %in% names(summary_list)) {
    plots$family_days <- create_plot(
      summary_list$family_days,
      "Number of Detection Days by Family",
      "family",
      "n_days",
      n_top
    ) +
      ggplot2::facet_wrap(~order, scales = "free", ncol = 3)
  }

  # 7. Order Sensors
  if ("order_sensors" %in% names(summary_list)) {
    plots$order_sensors <- create_plot(
      summary_list$order_sensors,
      "Number of Sensors by Order",
      "order",
      "n_sensors",
      n_top
    )
  }

  # 8. Family Sensors
  if ("family_sensors" %in% names(summary_list)) {
    plots$family_sensors <- create_plot(
      summary_list$family_sensors,
      "Number of Sensors by Family",
      "family",
      "n_sensors",
      n_top
    ) +
      ggplot2::facet_wrap(~order, scales = "free", ncol = 3)
  }

  return(plots)
}

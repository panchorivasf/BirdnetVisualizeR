#' Create a histogram of species observation counts
#'
#' @param data Data frame containing species observation data
#' @param species_col Character. Name of column containing species names (default: "species")
#' @param top_n Integer. Number of top species to label (0 for no labels, default: 5)
#' @param plot_type Character. Plot type: "base", "ggplot2", or "plotly" (default: "plotly")
#' @param bins Integer. Number of bins for histogram (default: 30)
#' @param title Character. Plot title (default: "Species Count Distribution")
#' @param x_label Character. X-axis label (default: "Number of Observations")
#' @param y_label Character. Y-axis label (default: "Frequency")
#'
#' @return A histogram plot showing distribution of species observation counts.
#'         Returns ggplot object for "ggplot2", plotly object for "plotly",
#'         or base R plot for "base".
#'
#' @details Creates a histogram of species observation counts with optional
#'          labeling of top-n most observed species. The function automatically
#'          loads required packages based on the selected plot type.
#'
#' @examples
#' \dontrun{
#' # Using default parameters
#' species_histogram(observation_data)
#'
#' # With ggplot2 and custom labels
#' species_histogram(observation_data, plot_type = "ggplot2", top_n = 3)
#' }
#'
#' @importFrom ggplot2 ggplot aes geom_histogram labs theme_minimal
#' @importFrom plotly ggplotly
#' @importFrom stats hist
#' @importFrom grDevices rainbow
#' @importFrom dplyr group_by tally
#' @export
sp_histogram <- function(data,
                              species_col = "species",
                              top_n = 5,
                              plot_type = "plotly",
                              bins = 30,
                              title = "Species Count Distribution",
                              x_label = "Number of Observations",
                              y_label = "Frequency") {

  # Load required packages based on plot type
  if (plot_type == "ggplot2") {
    if (!require(ggplot2, quietly = TRUE)) {
      stop("ggplot2 package is required for this plot type")
    }
  } else if (plot_type == "plotly") {
    if (!require(plotly, quietly = TRUE)) {
      stop("plotly package is required for this plot type")
    }
    if (!require(ggplot2, quietly = TRUE)) {
      stop("ggplot2 package is required for plotly")
    }
  }

  data1 <- data |>
    group_by(species) |>
    tally(sort = TRUE)

  # Extract count values and sort data by count
  counts <- data1$n
  data_sorted <- data[order(data1$n, decreasing = TRUE), ]

  # Get top species for labeling
  if (top_n > 0) {
    top_species <- data_sorted[1:min(top_n, nrow(data_sorted)), ]
    label_colors <- rainbow(nrow(top_species))
  }

  # Base R plotting
  if (plot_type == "base") {
    # Create histogram
    hist_data <- hist(counts, breaks = bins, plot = FALSE)
    hist(counts, breaks = bins,
         main = title,
         xlab = x_label,
         ylab = y_label,
         col = "lightblue",
         border = "white")

    # Add labels for top species
    if (top_n > 0) {
      for (i in 1:nrow(top_species)) {
        species_count <- top_species[[count_col]][i]
        species_name <- top_species[[species_col]][i]

        # Find which bin this species falls into
        bin_idx <- findInterval(species_count, hist_data$breaks)
        if (bin_idx > 0 && bin_idx <= length(hist_data$counts)) {
          bin_height <- hist_data$counts[bin_idx]

          # Draw line from x-axis to top of bar
          lines(c(species_count, species_count), c(0, bin_height),
                col = label_colors[i], lwd = 2)

          # Add label
          text(species_count, bin_height + max(hist_data$counts) * 0.05,
               species_name,
               col = label_colors[i],
               srt = 45,
               adj = 0,
               cex = 0.8)
        }
      }
    }

    # ggplot2 plotting
  } else if (plot_type == "ggplot2") {
    p <- ggplot(data, aes_string(x = count_col)) +
      geom_histogram(bins = bins, fill = "lightblue", color = "white", alpha = 0.7) +
      labs(title = title, x = x_label, y = y_label) +
      theme_minimal()

    # Add labels for top species
    if (top_n > 0) {
      # Get histogram data to find bar heights
      hist_build <- ggplot_build(p)
      hist_data <- hist_build$data[[1]]

      for (i in 1:nrow(top_species)) {
        species_count <- top_species[[count_col]][i]
        species_name <- top_species[[species_col]][i]

        # Find the corresponding bin
        bin_info <- hist_data[hist_data$xmin <= species_count & hist_data$xmax > species_count, ]
        if (nrow(bin_info) > 0) {
          bin_height <- bin_info$count[1]

          # Add vertical line
          p <- p + geom_segment(aes(x = species_count, y = 0,
                                    xend = species_count, yend = bin_height),
                                color = label_colors[i], size = 1)

          # Add label
          p <- p + annotate("text", x = species_count,
                            y = bin_height + max(hist_data$count) * 0.05,
                            label = species_name,
                            color = label_colors[i],
                            angle = 45,
                            hjust = 0,
                            size = 3)
        }
      }
    }

    return(p)

    # Plotly plotting
  } else if (plot_type == "plotly") {
    # Create base ggplot
    p <- ggplot(data, aes_string(x = count_col)) +
      geom_histogram(bins = bins, fill = "lightblue", color = "white", alpha = 0.7) +
      labs(title = title, x = x_label, y = y_label) +
      theme_minimal()

    # Add labels for top species (similar to ggplot2)
    if (top_n > 0) {
      hist_build <- ggplot_build(p)
      hist_data <- hist_build$data[[1]]

      for (i in 1:nrow(top_species)) {
        species_count <- top_species[[count_col]][i]
        species_name <- top_species[[species_col]][i]

        bin_info <- hist_data[hist_data$xmin <= species_count & hist_data$xmax > species_count, ]
        if (nrow(bin_info) > 0) {
          bin_height <- bin_info$count[1]

          p <- p + geom_segment(aes(x = species_count, y = 0,
                                    xend = species_count, yend = bin_height),
                                color = label_colors[i], size = 1)

          p <- p + annotate("text", x = species_count,
                            y = bin_height + max(hist_data$count) * 0.05,
                            label = species_name,
                            color = label_colors[i],
                            angle = 45,
                            hjust = 0,
                            size = 3)
        }
      }
    }

    # Convert to plotly
    p_plotly <- ggplotly(p, tooltip = c("x", "count"))
    return(p_plotly)

  } else {
    stop("plot_type must be one of: 'base', 'ggplot2', or 'plotly'")
  }
}

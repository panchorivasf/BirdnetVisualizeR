#' Create a histogram of species observation counts
#'
#' @param data Data frame containing species observation data
#' @param y_var Character. Name of column containing taxa name (default: "species")
#' @param top_n Integer. Number of top species to label (0 for no labels, default: 5)
#' @param bins Integer. Number of bins for histogram (default: 30)
#' @param title Character. Plot title (default: "Species Count Distribution")
#' @param x_label Character. X-axis label (default: "Number of Observations")
#' @param y_label Character. Y-axis label (default: "auto")
#' @param x_exp_factor Numeric. Expansion factor for the right side of the
#' x-axis, used to accommodate species labels.
#' @param save_png  Logical. Whether to save plot as png.
#' @param prefix Character. A prefix for the file name.
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
#' @importFrom graphics hist
#' @importFrom grDevices rainbow
#' @importFrom dplyr group_by tally
#' @export
#' @examples
#' \dontrun{
#' birdnet_histogram(all_data_geotax_clean,
#'                  y_var = "family",
#'                  top_n = 5,
#'                  exp_x_factor = 1.2)
#' }
birdnet_histogram <- function(data,
                              y_var = "species",
                              top_n = 5,
                              bins = 100,
                              title = "",
                              x_label = "Number of Detections",
                              y_label = "auto",
                              exp_x_factor = 1.3,
                              save_png = TRUE,
                              width = 15,
                              height = 8,
                              prefix = "") {


  if (y_label == "auto"){
    if (y_var == "family") {
      y_label <- "Number of families"
    } else if (y_var == "order"){
      y_label <- "Number of orders"
    } else if (y_var == "species"){
      y_label <- "Number of species"
    }


  }


  if (!require(ggplot2, quietly = TRUE)) stop("ggplot2 package is required")
  if (!require(dplyr, quietly = TRUE)) stop("dplyr package is required")
  if (!require(rlang, quietly = TRUE)) stop("rlang package is required")

  # Calculate species counts
  data1 <- data |>
    dplyr::group_by(!!rlang::sym(y_var)) |>
    dplyr::tally(sort = TRUE)

  # Color palette
  nice_colors <- c("#E31A1C", "#1F78B4", "#33A02C", "#FF7F00", "#6A3D9A",
                   "#FB9A99", "#A6CEE3", "#B2DF8A", "#FDBF6F", "#CAB2D6")
  y_varors <- rep(nice_colors, length.out = min(top_n, nrow(data1)))

  # Define explicit breaks
  bin_breaks <- pretty(range(data1$n), n = bins)
  binwidth <- diff(bin_breaks)[1]

  data1$bin <- cut(data1$n, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)

  hist_data <- data.frame(
    bin = levels(data1$bin),
    xmin = bin_breaks[-length(bin_breaks)],
    xmax = bin_breaks[-1]
  ) |>
    left_join(
      data1 |> count(bin),
      by = "bin"
    ) |>
    mutate(count = ifelse(is.na(n), 0, n)) |>
    select(-n)


  # Base histogram
  p <- ggplot(data1, aes(x = n)) +
    geom_histogram(breaks = bin_breaks, fill = "#B0B0B0",
                   color = "white", alpha = 0.9, linewidth = 0.3) +
    labs(title = title, x = x_label, y = y_label) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
      panel.grid.major.y = element_line(color = "gray90", linewidth = 0.3)
    )

  # Highlight top species
  if (top_n > 0 && nrow(data1) > 0) {
    top_species <- data1[1:min(top_n, nrow(data1)), ]

    highlight_list <- lapply(1:nrow(top_species), function(i) {
      species_count <- top_species$n[i]
      color <- y_varors[i]

      bin_info <- hist_data[hist_data$xmin <= species_count & hist_data$xmax > species_count, ]
      if (nrow(bin_info) > 0) {
        return(data.frame(
          xmin = bin_info$xmin[1],
          xmax = bin_info$xmax[1],
          ymin = 0,
          ymax = bin_info$count[1],
          color = color
        ))
      } else {
        return(NULL)
      }
    })

    highlight_data <- do.call(rbind, highlight_list)

    if (!is.null(highlight_data)) {
      p <- p +
        geom_rect(
          data = highlight_data,
          aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = color),
          color = "white", linewidth = 0.3, alpha = 0.8,
          inherit.aes = FALSE
        ) +
        scale_fill_identity()
    }

    # Labels
    max_y <- max(hist_data$count, na.rm = TRUE)
    max_x <- max(data1$n)
    min_x <- min(data1$n)
    x_range <- max_x - min_x

    label_list <- lapply(1:nrow(top_species), function(i) {
      species_count <- top_species$n[i]
      species_name <- top_species[[y_var]][i]
      color <- y_varors[i]

      bin_info <- hist_data[hist_data$xmin <= species_count & hist_data$xmax > species_count, ]
      if (nrow(bin_info) > 0) {
        bin_mid <- (bin_info$xmin + bin_info$xmax) / 2
        bin_height <- bin_info$count[1]
        return(data.frame(
          species_count = species_count,
          species_name = species_name,
          bin_mid = bin_mid,
          bin_height = bin_height,
          label_x = bin_mid,
          label_y = bin_height + max_y * 0.08,
          color = color,
          stringsAsFactors = FALSE
        ))
      } else {
        return(NULL)
      }
    })

    label_data <- do.call(rbind, label_list)

    if (!is.null(label_data)) {
      # Adjust overlapping labels
      label_data <- label_data[order(label_data$bin_mid, label_data$label_y), ]
      min_spacing <- max_y * 0.1
      for (j in 2:nrow(label_data)) {
        if (label_data$label_y[j] - label_data$label_y[j-1] < min_spacing) {
          label_data$label_y[j] <- label_data$label_y[j-1] + min_spacing
        }
      }

      p <- p +
        geom_segment(
          data = label_data,
          aes(x = bin_mid, y = bin_height, xend = bin_mid, yend = label_y, color = color),
          linewidth = 0.5, linetype = "dashed", inherit.aes = FALSE
        ) +
        geom_label(
          data = label_data,
          aes(x = label_x, y = label_y,
              label = paste0(species_name, "\n(n=", species_count, ")"),
              fill = color),
          color = "white",
          hjust = 0, vjust = 0.5,
          size = 3, fontface = "bold",
          label.padding = unit(0.4, "lines"),
          label.r = unit(0.2, "lines"),
          inherit.aes = FALSE
        ) +
        scale_fill_identity() +
        scale_color_identity()

      final_max_y <- max(label_data$label_y) + max_y * 0.1
      p <- p + coord_cartesian(
        xlim = c(min_x, max_x * exp_x_factor),
        ylim = c(0, final_max_y),
        expand = FALSE
      )
    }
  }

  if(save_png) {
    ggsave(filename = paste0(prefix, "_birdnet_hist.png"),
           plot = p, bg = "white",
           width = width, height = height, units = "in",
           dpi = 300)
    cat("Histogram saved as:", paste0(prefix, "_birdnet_hist.png"), "\n")
  }

  return(p)
}


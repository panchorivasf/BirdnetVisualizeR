#' Phenological charts
#'
#' @description  This function creates a "phenochart" with all the species
#' detected with BirdNET. The chart is a stacked tile plot with a heatmap
#' indicating the number of calls per species per day, and a separate tile plot
#' at the bottom indicating the total number of species detected each day.
#' Francisco Rivas, April 2024
#'
#' @param df A data frame obtained with "import_birdnet" function.
#' @param plot_title Character. A title for the plot (use quotation marks).
#' @param min_days Numeric. Minimum number of days of detection for a species
#' to be included in the plot.
#' @param sort Character. Specifies how to order the species list vertically.
#' Options:
#' "start" (default) sorts by the first day of detection;
#' "n.days" sorts by the total number of days that the species was detected;
#' "n.calls" sorts by the total number of calls detected by species;
#' "call.rate" sorts by the calling rate of each species (n.calls/n.days)
#' "common.name" sorts by the common name of the species, in English.
#' "scientific.name" sorts by the binomial scientific name of the species.
#' @param desc  Logical. Whether to sort in descending order.
#'
#' @returns a Plotly object.
#' @export
#'
#' @examples
#' \dontrun{
#' pheno_chart(BirdnetDetections,
#'              plot_title = "BirdNET - Sundarban Mangrove Forest S4A17644",
#'              sort = "start", min_days = 15)
#' # filter some species
#'selectedsP <- BirdnetDetections |>
#'  filter(Common.name %in% c("Mangrove Pitta", "Indian White-eye",
#'                            "Mangrove Whistler","Common Tailorbird"))
#'pheno_chart(selectedsP,  plot_title = "BirdNET - Sundarban Mangrove Forest S4A17644")
#'
#'
#'selectedsP <- BirdnetDetections |>
#'  filter(Common.name %in% c("Large-tailed Nightjar","Oriental Scops-Owl" ))
#'
#'pheno_chart(selectedsP,  plot_title = "BirdNET - Sundarban Mangrove Forest S4A17644")
#'
#'
#' }
pheno_chart <- function(df, plot_title = "", min_days = 5,
                        sort = "start", desc = TRUE) {

  if(sort=="start"){
    sorting <- "the first date of detection"
  } else if (sort == "n.days"){
    sorting <- "the total number of days detected"
  } else if (sort == "n.calls"){
    sorting <- "the total number of calls/songs detected"
  } else if (sort == "call.rate") {
    sorting <- "calling rate (total N of calls / total N of days detected)"
  } else if (sort == "common.name") {
    sorting <- "common name"
  } else if (sort == "scientific.name") {
    sorting <- "scientific name"
  }

  # Calculate detection metrics for each species
  species_info <- df |>
    group_by(Scientific.name, Common.name) |>
    summarize(
      first_detected = min(date), # Earliest date of detection
      n.days = n_distinct(date), # Number of unique detection days
      n.calls = n(), # Total number of detections
      .groups = 'drop'
    ) |>
    mutate(call.rate = round(n.calls / n.days))

  # Filter based on min_days
  species_info <- species_info |>
    filter(n.days >= min_days)

  # Order based on the specified criterion

  # Order based on the specified criterion
  if (sort == "start") {
    if (desc) {
      species_info <- species_info |>
        arrange(desc(first_detected))
    } else {
      species_info <- species_info |>
        arrange(first_detected)
    }
  } else if (sort == "n.days") {
    if (desc) {
      species_info <- species_info |>
        arrange(desc(n.days))
    } else {
      species_info <- species_info |>
        arrange(n.days)
    }
  } else if (sort == "n.calls") {
    if (desc) {
      species_info <- species_info |>
        arrange(desc(n.calls))
    } else {
      species_info <- species_info |>
        arrange(n.calls)
    }
  } else if (sort == "call.rate") {
    if (desc) {
      species_info <- species_info |>
        arrange(desc(call.rate))
    } else {
      species_info <- species_info |>
        arrange(call.rate)
    }
  } else if (sort == "common.name") {
    if (desc) {
      species_info <- species_info |>
        arrange(desc(Common.name))
    } else {
      species_info <- species_info |>
        arrange(Common.name)
    }
  } else if (sort == "scientific.name") {
    if (desc) {
      species_info <- species_info |>
        arrange(desc(Scientific.name))
    } else {
      species_info <- species_info |>
        arrange(Scientific.name)
    }
  } else {
    stop("Invalid order parameter. Choose among 'start', 'n.days', 'n.calls', 'call.rate',
         'common.name', and 'scientific.name'")
  }


  if (sort == "scientific.name"){
    filtered_species <- species_info$Scientific.name

    # Filter the original dataframe to include only the species meeting the min_days requirement
    df <- df |>
      filter(Scientific.name %in% filtered_species)
    # Proceed with summarizing and plotting as before, now using the filtered and ordered dataset
    df_summarized <- df |>
      group_by(date, Scientific.name) |>
      summarise(n = n(), .groups = "drop") |>
      mutate(Scientific.name = factor(Scientific.name, levels = filtered_species))

    # Main plot
    main_plot <- ggplot(df_summarized, aes(x = date, y = Scientific.name, fill = n)) +
      geom_tile() +
      scale_fill_gradientn(colors = c("#ffeda0", "orange", "red2")) +
      labs(title = plot_title, x = "", y = "", fill = "Calls/sp/day",
           subtitle = paste0("Species list sorted by ", sorting)) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = c(0.2,20))+
      scale_x_date(date_breaks = "1 week", date_labels = "%b %d",
                   expand = c(0.005,0.005))

  } else {
    filtered_species <- species_info$Common.name

    # Filter the original dataframe to include only the species meeting the min_days requirement
    df <- df |>
      filter(Common.name %in% filtered_species)

    # Proceed with summarizing and plotting as before, now using the filtered and ordered dataset
    df_summarized <- df |>
      group_by(date, Common.name) |>
      summarise(n = n(), .groups = "drop") |>
      mutate(Common.name = factor(Common.name, levels = filtered_species))

    # Main plot
    main_plot <- ggplot(df_summarized, aes(x = date, y = Common.name, fill = n)) +
      geom_tile() +
      scale_fill_gradientn(colors = c("#ffeda0", "orange", "red2")) +
      labs(title = plot_title, x = "", y = "", fill = "Calls/sp/day",
           subtitle = paste0("Species list sorted by ", sorting)) +
      theme_minimal() +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            legend.position = c(0.2,20))+
      scale_x_date(date_breaks = "1 week", date_labels = "%b %d",
                   expand = c(0.005,0.005))
  }



  # Species richness plot, now based on the filtered data
  species_richness <- df |>
    group_by(date) |>
    summarise(Total_Species = n_distinct(Common.name), .groups = "drop")

  richness_plot <- ggplot(species_richness,
                          aes(x = date, y = 1, fill = Total_Species)) +
    geom_tile() +
    scale_fill_gradientn(colors = c("#deebf7", "#9ecae1", "blue3")) +
    labs(x = "Date", y = "", fill = "Species/day") +
    theme(panel.grid = element_blank(),
          legend.position = c(0.1,20),
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())+
    scale_x_date(date_breaks = "1 month", date_labels = "%b",
                 expand = c(0.005,0.005))

  # Combine the plots with patchwork, adjusting the layout
  combined_plot <- main_plot / richness_plot +
    plot_layout(heights = c(10, .5), guides = 'collect')+
    theme(plot.margin = unit(c(1, 1, 1, 1), "pt"))  # Adjust margins as needed

  return(combined_plot)
}

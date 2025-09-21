#' Generate Vocal Hyperdominance Analysis Report
#'
#' This function creates a comprehensive Word document report analyzing vocal
#' hyperdominance patterns in acoustic detection data. The report includes
#' both console output and a formatted Word document with tables, statistics,
#' and interpretations.
#'
#' @param data A data frame containing acoustic detection data with species information.
#' @param group Optional character string specifying the grouping variable for analysis.
#'             If NULL (default), performs ungrouped overall analysis only.
#' @param data_name Optional character string specifying the name of the dataset for reporting.
#'                 If NULL (default), uses the name of the data frame object.
#' @param prefix Optional character string prefix for the output filename.
#'              If NULL (default), uses the data_name or "overall" if data_name is also NULL.
#'
#' @return Invisibly returns the path to the generated Word document. The function
#'         primarily creates a .docx file and prints summary information to the console.
#'
#' @details
#' The function performs the following analyses:
#' \itemize{
#'   \item Calculates overall vocal hyperdominance patterns
#'   \item If grouped, analyzes patterns within each group and provides cross-group comparisons
#'   \item Generates formatted tables with binomial species names italicized
#'   \item Provides statistical summaries and ecological interpretations
#'   \item Creates both console output and a comprehensive Word document report
#' }
#'
#' @note
#' Requires the following packages: `officer`, `flextable`, `dplyr`,
#' `purrr`, and `rlang`.
#'
#' @examples
#' \dontrun{
#' # Ungrouped analysis
#' vhd_report(bird_detections)
#'
#' # Grouped analysis by sensor
#' vhd_report(bird_detections, group = "sensor.id")
#'
#' # Custom data name and prefix
#' vhd_report(bird_detections, group = "location",
#'           data_name = "Spring Survey 2023", prefix = "spring_analysis")
#' }
#'
#' @seealso
#' \code{\link{vocal_hyperdominance}} for the underlying analysis function
#'
#' @importFrom officer read_docx body_add_par body_add_fpar
#' @importFrom flextable flextable set_caption theme_zebra autofit italic body_add_flextable
#' @importFrom dplyr group_by select
#' @importFrom purrr map_chr
#' @importFrom rlang sym
#' @export
vhd_report <- function(data,
                       group = NULL,
                       data_name = NULL,
                       prefix = NULL) {

  # Helper function to check if a name is binomial
  is_binomial <- function(species_name) {
    grepl("^[A-Z][a-z]+ [a-z]+$", species_name)
  }

  # Get the data frame name
  if (is.null(data_name)) {
    data_name <- deparse(substitute(data))
  }

  if (is.null(prefix)){
    prefix <- ifelse(is.null(data_name), "overall", as.character(data_name))
  }

  # Overall vocal hyperdominance
  overall_vhd <- vocal_hyperdominance(data, verbose = FALSE)

  # Print overall results to console
  cat("=== OVERALL DOMINANCE PATTERNS ===\n")
  cat(sprintf("Total detections: %d\n", sum(overall_vhd$detections)))
  cat(sprintf("Number of dominant species: %d\n", nrow(overall_vhd)))
  cat(sprintf("Top species: %s (%.1f%%)\n\n",
              overall_vhd$species[1], overall_vhd$percentage[1]))

  print(overall_vhd)
  cat("\n\n")

  output_file = paste0(prefix, "_vhd_report.docx")

  # Create new Word document
  doc <- read_docx()


  if (!is.null(group)) {
    # GROUPED ANALYSIS
    # Convert group to symbol for tidy evaluation
    group_sym <- rlang::sym(group)

    # Grouped vocal hyperdominance - preserving group identifiers
    grouped_vhd <- data |>
      group_by(!!group_sym) |>
      group_map(~ {
        result <- vocal_hyperdominance(.x)
        # Add group identifier to the result
        result$group_id <- .y[[1]]
        result
      }, .keep = TRUE)

    # Extract group names for reporting
    group_names <- purrr::map_chr(grouped_vhd, ~ unique(.x$group_id))

    # Add title and introduction for grouped analysis
    doc <- doc %>%
      body_add_par("VOCAL HYPERDOMINANCE ANALYSIS REPORT", style = "heading 1") %>%
      body_add_par("") %>%
      body_add_par(sprintf("Report generated on: %s", Sys.Date()), style = "Normal") %>%
      body_add_par("") %>%
      body_add_par(sprintf("This report presents vocal hyperdominance analysis for %s dataset.",
                           data_name), style = "Normal") %>%
      body_add_par("") %>%
      body_add_par(sprintf("These data were grouped by %s.", group), style = "Normal") %>%
      body_add_par("")

    # OVERALL DOMINANCE SECTION
    doc <- doc %>%
      body_add_par("OVERALL DOMINANCE PATTERNS", style = "heading 2") %>%
      body_add_par("") %>%
      body_add_par("Analysis across all data (ungrouped):", style = "heading 3") %>%
      body_add_par("")

    # Overall summary statistics
    total_detections_overall <- sum(overall_vhd$detections)
    dominant_species_overall <- overall_vhd$species[1]
    dominant_percentage_overall <- overall_vhd$percentage[1]

    doc <- doc %>%
      body_add_par(sprintf("Total detections across all groups: %d", total_detections_overall), style = "Normal") %>%
      body_add_par(sprintf("Number of dominant species: %d", nrow(overall_vhd)), style = "Normal") %>%
      body_add_fpar(
        fpar(
          ftext("Top species overall: ", prop = fp_text()),
          ftext(dominant_species_overall, prop = fp_text(italic = is_binomial(dominant_species_overall))),
          ftext(sprintf(" (%.1f%%)", dominant_percentage_overall), prop = fp_text())
        )
      ) %>%
      body_add_par("")

    # Add overall table with italicized species names
    doc <- doc %>%
      body_add_par("Overall Hyperdominance Table:", style = "heading 3") %>%
      body_add_flextable(
        overall_vhd %>%
          flextable() %>%
          set_caption("Overall Species Dominance") %>%
          theme_zebra() %>%
          autofit() %>%
          italic(j = "species",
                 i = ~ grepl("^[A-Z][a-z]+ [a-z]+$", species),
                 part = "body")
      ) %>%
      body_add_par("") %>%
      body_add_par("")  # Add spacing

    # GROUP-SPECIFIC ANALYSIS
    doc <- doc %>%
      body_add_par("GROUP-SPECIFIC ANALYSIS", style = "heading 2") %>%
      body_add_par("")

    # Process each group's data
    for (i in seq_along(grouped_vhd)) {
      group_data <- grouped_vhd[[i]]
      group_id <- group_names[i]

      # Print group results to console
      cat(sprintf("=== GROUP: %s = %s ===\n", group, group_id))
      cat(sprintf("Total detections: %d\n", sum(group_data$detections)))
      cat(sprintf("Number of dominant species: %d\n", nrow(group_data)))
      cat(sprintf("Top species: %s (%.1f%%)\n",
                  group_data$species[1], group_data$percentage[1]))
      print(group_data %>% select(-group_id))
      cat("\n\n")

      # Add group header with actual identifier to Word doc
      doc <- doc %>%
        body_add_par(sprintf("GROUP ANALYSIS: %s = %s", group, group_id), style = "heading 2") %>%
        body_add_par("")

      # Add summary statistics
      total_detections <- sum(group_data$detections)
      dominant_species <- group_data$species[1]
      dominant_percentage <- group_data$percentage[1]

      doc <- doc %>%
        body_add_par("Summary:", style = "heading 3") %>%
        body_add_par(sprintf("Total detections: %d", total_detections), style = "Normal") %>%
        body_add_par(sprintf("Number of dominant species: %d", nrow(group_data)), style = "Normal") %>%
        body_add_fpar(
          fpar(
            ftext("Top species: ", prop = fp_text()),
            ftext(dominant_species, prop = fp_text(italic = is_binomial(dominant_species))),
            ftext(sprintf(" (%.1f%%)", dominant_percentage), prop = fp_text())
          )
        ) %>%
        body_add_par("")

      # Add the table with italicized species names
      doc <- doc %>%
        body_add_par("Hyperdominance Table:", style = "heading 3") %>%
        body_add_flextable(
          group_data %>%
            select(-group_id) %>%  # Remove group_id column for cleaner table
            flextable() %>%
            set_caption(sprintf("%s: %s - Species Dominance", group, group_id)) %>%
            theme_zebra() %>%
            autofit() %>%
            italic(j = "species",
                   i = ~ grepl("^[A-Z][a-z]+ [a-z]+$", species),
                   part = "body")
        ) %>%
        body_add_par("")

      # Add interpretation with italicized species names
      doc <- doc %>%
        body_add_par("Interpretation:", style = "heading 3")

      if (nrow(group_data) >= 2) {
        second_species <- group_data$species[2]
        second_percentage <- group_data$percentage[2]

        # Create formatted text with italics for binomial names
        doc <- doc %>%
          body_add_fpar(
            fpar(
              ftext("The acoustic environment is dominated by ", prop = fp_text()),
              ftext(dominant_species, prop = fp_text(italic = is_binomial(dominant_species))),
              ftext(sprintf(" (%.1f%%) followed by ", dominant_percentage), prop = fp_text()),
              ftext(second_species, prop = fp_text(italic = is_binomial(second_species))),
              ftext(sprintf(" (%.1f%%). The top %d species account for %.1f%% of all detections.",
                            second_percentage, nrow(group_data), max(group_data$cumulative)),
                    prop = fp_text())
            )
          )
      } else {
        doc <- doc %>%
          body_add_fpar(
            fpar(
              ftext("The acoustic environment is overwhelmingly dominated by ", prop = fp_text()),
              ftext(dominant_species, prop = fp_text(italic = is_binomial(dominant_species))),
              ftext(sprintf(" (%.1f%%), indicating very low species diversity in detections.",
                            dominant_percentage), prop = fp_text())
            )
          )
      }

      doc <- doc %>%
        body_add_par("") %>%
        body_add_par("")  # Add spacing between groups
    }

    # Add cross-group summary
    doc <- doc %>%
      body_add_par("CROSS-GROUP SUMMARY", style = "heading 2") %>%
      body_add_par("") %>%
      body_add_par(sprintf("Total groups analyzed: %d", length(grouped_vhd)), style = "Normal")

    # Calculate cross-group statistics
    all_dominant_species <- purrr::map_chr(grouped_vhd, ~ .x$species[1])
    species_frequency <- table(all_dominant_species)
    most_common_dominant <- names(which.max(species_frequency))
    unique_dominant_species <- unique(all_dominant_species)

    # Print cross-group summary to console
    cat("=== CROSS-GROUP SUMMARY ===\n")
    cat(sprintf("Total groups analyzed: %d\n", length(grouped_vhd)))
    cat(sprintf("Most frequently dominant species: %s\n", most_common_dominant))
    cat(sprintf("Number of unique dominant species: %d\n", length(unique_dominant_species)))
    cat("Unique dominant species across groups:\n")
    for (species in sort(unique_dominant_species)) {
      count <- sum(all_dominant_species == species)
      cat(sprintf("  - %s (appears in %d groups)\n", species, count))
    }
    cat("\n")

    # Add formatted text for cross-group summary
    doc <- doc %>%
      body_add_fpar(
        fpar(
          ftext("Most frequently dominant species across groups: ", prop = fp_text()),
          ftext(most_common_dominant, prop = fp_text(italic = is_binomial(most_common_dominant)))
        )
      ) %>%
      body_add_par(sprintf("Number of unique dominant species across groups: %d",
                           length(unique_dominant_species)), style = "Normal") %>%
      body_add_par("Unique dominant species across groups:", style = "Normal")

    # Add list of unique dominant species with italics
    for (species in sort(unique_dominant_species)) {
      count <- sum(all_dominant_species == species)
      doc <- doc %>%
        body_add_fpar(
          fpar(
            ftext("  . ", prop = fp_text()),
            ftext(species, prop = fp_text(italic = is_binomial(species))),
            ftext(sprintf(" (appears in %d groups)", count), prop = fp_text())
          )
        )
    }

    doc <- doc %>% body_add_par("")

    # Compare group dominance with overall patterns
    doc <- doc %>%
      body_add_par("COMPARISON WITH OVERALL PATTERNS", style = "heading 3") %>%
      body_add_par("")

    if (dominant_species_overall %in% all_dominant_species) {
      doc <- doc %>%
        body_add_fpar(
          fpar(
            ftext("The overall dominant species (", prop = fp_text()),
            ftext(dominant_species_overall, prop = fp_text(italic = is_binomial(dominant_species_overall))),
            ftext(sprintf(") also appears as a dominant species in %d out of %d groups.",
                          sum(all_dominant_species == dominant_species_overall),
                          length(grouped_vhd)), prop = fp_text())
          )
        )
    } else {
      doc <- doc %>%
        body_add_fpar(
          fpar(
            ftext("The overall dominant species (", prop = fp_text()),
            ftext(dominant_species_overall, prop = fp_text(italic = is_binomial(dominant_species_overall))),
            ftext(") does not appear as the top species in any individual group, indicating different local dominance patterns.",
                  prop = fp_text())
          )
        )
    }

  } else {
    # UNGROUPED ANALYSIS (only overall)
    doc <- doc %>%
      body_add_par("VOCAL HYPERDOMINANCE ANALYSIS REPORT", style = "heading 1") %>%
      body_add_par("") %>%
      body_add_par(sprintf("Report generated on: %s", Sys.Date()), style = "Normal") %>%
      body_add_par("") #%>%
    #
    #       body_add_par("This report presents overall vocal hyperdominance analysis (ungrouped).", style = "Normal") %>%
    #       body_add_par("")
    #
    # Overall summary statistics
    total_detections_overall <- sum(overall_vhd$detections)
    dominant_species_overall <- overall_vhd$species[1]
    dominant_percentage_overall <- overall_vhd$percentage[1]

    doc <- doc %>%
      body_add_par("SUMMARY", style = "heading 2") %>%
      body_add_par("") %>%
      body_add_par(sprintf("Total detections: %d", total_detections_overall), style = "Normal") %>%
      body_add_par(sprintf("Number of dominant species: %d", nrow(overall_vhd)), style = "Normal") %>%
      body_add_fpar(
        fpar(
          ftext("Top species: ", prop = fp_text()),
          ftext(dominant_species_overall, prop = fp_text(italic = is_binomial(dominant_species_overall))),
          ftext(sprintf(" (%.1f%%)", dominant_percentage_overall), prop = fp_text())
        )
      ) %>%
      body_add_par("")

    # Add overall table with italicized species names
    doc <- doc %>%
      body_add_par("Hyperdominance Table:", style = "heading 3") %>%
      body_add_flextable(
        overall_vhd %>%
          flextable() %>%
          set_caption("Overall Species Dominance") %>%
          theme_zebra() %>%
          autofit() %>%
          italic(j = "species",
                 i = ~ grepl("^[A-Z][a-z]+ [a-z]+$", species),
                 part = "body")
      ) %>%
      body_add_par("")

    # Add interpretation
    doc <- doc %>%
      body_add_par("INTERPRETATION", style = "heading 3")

    if (nrow(overall_vhd) >= 2) {
      second_species <- overall_vhd$species[2]
      second_percentage <- overall_vhd$percentage[2]

      doc <- doc %>%
        body_add_fpar(
          fpar(
            ftext("The acoustic environment is dominated by ", prop = fp_text()),
            ftext(dominant_species_overall, prop = fp_text(italic = is_binomial(dominant_species_overall))),
            ftext(sprintf(" (%.1f%%) followed by ", dominant_percentage_overall), prop = fp_text()),
            ftext(second_species, prop = fp_text(italic = is_binomial(second_species))),
            ftext(sprintf(" (%.1f%%). The top %d species account for %.1f%% of all detections.",
                          second_percentage, nrow(overall_vhd), max(overall_vhd$cumulative)),
                  prop = fp_text())
          )
        )
    } else {
      doc <- doc %>%
        body_add_fpar(
          fpar(
            ftext("The acoustic environment is overwhelmingly dominated by ", prop = fp_text()),
            ftext(dominant_species_overall, prop = fp_text(italic = is_binomial(dominant_species_overall))),
            ftext(sprintf(" (%.1f%%), indicating very low species diversity in detections.",
                          dominant_percentage_overall), prop = fp_text())
          )
        )
    }
  }

  # Save the document
  print(doc, target = output_file)
  message(sprintf("\nReport successfully created: %s", output_file))

  return(invisible(output_file))
}

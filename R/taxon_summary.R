#' Taxonomic summary
#'
#' Calculates the number of species, families, and orders of birds
#' represented in a data frame
#'
#' @param data A data frame
#'
#' @returns A tibble with number of detections(count),
#' number of species, families, and orders represented.
#' @export
#'
#' @examples
#' \dontrun{
#' taxon_summary(my_data)
#' }
taxon_summary <- function(data, data.name = "birds",
                          report = TRUE) {
  # Create the summary tibble
  taxon_summ <- tibble(
    n.det = nrow(data),
    n.sp = n_distinct(data$species),
    n.fam = n_distinct(data$family),
    n.ord = n_distinct(data$order)
  )

  # Create the paragraph text report
  if (report) {
    text_report <- paste0(
      "The ", data.name, " dataset consisted of ", taxon_summ$n.det, " detections ",
      " from ", taxon_summ$n.sp, " species representing ", taxon_summ$n.fam,
      " families and ", taxon_summ$n.ord, " orders."
    )

    # # Print the paragraph to console
    # cat(text_report, "\n")
  } else {
    text_report <- NULL
  }

  # Return both as a list
  return(list(summary = taxon_summ, report = text_report))
}


#' Retrieve GBIF Taxonomy and Join to Original Data
#'
#' This function takes a dataframe containing species names, queries the GBIF
#' backbone taxonomy for each unique species, and joins the taxonomic information
#' back to the original dataframe.
#'
#' @param df A dataframe containing a column with species names.
#' @param species_col The name of the column in `df` that contains the species names.
#'                   Default is "Scientific.name".
#'
#' @return The original dataframe with additional taxonomic columns joined from GBIF:
#' \describe{
#'   \item{status}{Taxonomic status (e.g., "ACCEPTED", "SYNONYM")}
#'   \item{order}{Taxonomic order}
#'   \item{family}{Taxonomic family}
#'   \item{genus}{Genus name}
#'   \item{species}{Species epithet}
#' }
#'
#' @details
#' The function extracts unique species names from the specified column, queries
#' the GBIF backbone taxonomy using `rgbif::name_backbone()`, and joins the resulting
#' taxonomic information back to the original dataframe using the species name as key.
#'
#' @examples
#' \dontrun{
#' # Create a sample dataframe
#' species_df <- data.frame(
#'   Scientific.name = c("Vulpes vulpes", "Panthera leo", "Bubo bubo"),
#'   Count = c(5, 2, 8)
#' )
#'
#' # Get taxonomy information joined to original data
#' result_df <- add_taxonomy(species_df)
#'
#' # View results
#' print(result_df)
#' }
#'
#' @importFrom rgbif name_backbone
#' @importFrom dplyr select distinct left_join
#' @importFrom purrr map_dfr
#' @export
add_taxonomy <- function(df, species_col = "Scientific.name") {

  # Input validation
  if (!species_col %in% names(df)) {
    stop("Column '", species_col, "' not found in the input dataframe")
  }

  species_list <- unique(df[[species_col]])

  # Use map_dfr to map over the list and row-bind the results
  taxonomy_df <- purrr::map_dfr(species_list, ~{
    result <- rgbif::name_backbone(name = .x)
    result$search_name <- .x
    return(result)
  })

  taxonomy_df <- taxonomy_df |>
    dplyr::select(status, order, family, genus, species,
                  search_name)

  # Join taxonomic information back to original dataframe
  result_df <- df |>
    dplyr::left_join(taxonomy_df,
                     by = stats::setNames("search_name", species_col))

  return(result_df)
}

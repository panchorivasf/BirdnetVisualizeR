#' Retrieve GBIF Taxonomy for a List of Species
#'
#' This function takes a dataframe containing species names and returns taxonomic
#' information from the GBIF backbone taxonomy for each unique species.
#'
#' @param df A dataframe containing a column with species names.
#' @param species_col The name of the column in `df` that contains the species names.
#'                   Default is "Scientific.name".
#'
#' @return A dataframe with taxonomic information from GBIF. The returned dataframe includes:
#' \describe{
#'   \item{status}{Taxonomic status (e.g., "ACCEPTED", "SYNONYM")}
#'   \item{order}{Taxonomic order}
#'   \item{family}{Taxonomic family}
#'   \item{genus}{Genus name}
#'   \item{species}{Species epithet}
#'   \item{search_name}{Original species name used for the query}
#' }
#'
#' @details
#' The function extracts unique species names from the specified column and queries
#' the GBIF backbone taxonomy using `rgbif::name_backbone()`. It returns a dataframe
#' with key taxonomic information for each species.
#'
#' @examples
#' \dontrun{
#' # Create a sample dataframe
#' species_df <- data.frame(
#'   Scientific.name = c("Vulpes vulpes", "Panthera leo", "Bubo bubo"),
#'   Count = c(5, 2, 8)
#' )
#'
#' # Get taxonomy information
#' taxonomy_data <- get_taxonomy(species_df)
#'
#' # View results
#' print(taxonomy_data)
#' }
#'
#' @importFrom rgbif name_backbone
#' @importFrom dplyr select
#' @importFrom purrr map_dfr
#' @export
get_taxonomy <- function(df, species_col = "Scientific.name") {

  # Input validation
  if (!species_col %in% names(df)) {
    stop("Column '", species_col, "' not found in the input dataframe")
  }

  species_list <- unique(df[[species_col]])

  # Use map_dfr to map over the list and row-bind the results
  taxonomy_df <- map_dfr(species_list, ~{
    result <- rgbif::name_backbone(name = .x)
    result$search_name <- .x
    return(result)
  })

  taxonomy_df <- taxonomy_df |>
    dplyr::select(status, order, family, genus, species,
                  search_name)

  return(taxonomy_df)

}

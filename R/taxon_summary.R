#' Taxonomic summary
#'
#' Calculates the number of species, families, and orders of birds
#' represented in a data frame
#'
#' @param data A data frame
#'
#' @returns A tibble with number of species, families, and orders.
#' @export
#'
#' @examples
#' \dontrun{
#' taxon_summary(my_data)
#' }
taxon_summary <- function(data){
  taxon_summ <- tibble(
    sp = n_distinct(all_data_geotax$species),
    fam = n_distinct(all_data_geotax$family),
    ord = n_distinct(all_data_geotax$order)
  )
  return(taxon_summ)
}

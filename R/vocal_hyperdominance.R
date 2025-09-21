#' Vocal hyperdominance per grouping variable
#'
#' @param data A data frame with BirdNET outputs and, optionally,
#' taxonomic data.
#' @param group_var Character. The name of the column to be used as
#' grouping variable (e.g., species, Common.name, order, family)
#' @param past_half Logical. Whether to show the species that pushes
#' past 50%
#'
#' @returns A tibble
#' @export
#'
vocal_hyperdominance <- function(data, group_var = species,
                                 past_half = TRUE,
                                 verbose = TRUE){

  group_var <- enquo(group_var)

  dominant_sp <- data |>
    group_by(!!group_var) |>
    tally(sort = TRUE) |>
    mutate(
      percentage = round((n / sum(n) * 100), digits = 1),
      cumulative = round(cumsum(percentage), digits = 1)
    )

  dominant_sp <- dominant_sp |>
    rename(detections = n)

  if (past_half){

    dominant_sp <- dominant_sp |>
      filter(row_number() <= which(cumulative >= 50)[1])

  } else {

    dominant_sp <- dominant_sp |>
      filter(cumulative <= 50)
  }

  if (verbose){
    print(knitr::kable(dominant_sp))
  }


  invisible(dominant_sp)

}



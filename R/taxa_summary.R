#' Create Taxonomic Summaries with Flexible Grouping
#'
#' This function generates taxonomic summary tables with optional grouping by
#' one or more variables (e.g., treatment, site, habitat).
#'
#' @param df A dataframe containing taxonomic and detection data
#' @param group_by A character vector of column names to group by.
#'                 Use NULL for no grouping. Default is NULL.
#'
#' @return If grouped: A named list where each element contains the taxonomic summaries
#'         for that group level. If ungrouped: A list of summary tibbles.
#'
#' @importFrom dplyr group_by groups group_split group_keys
#' @importFrom purrr map set_names
#' @importFrom rlang syms
#' @export
taxa_summary <- function(df, group_by = NULL) {

  taxa_summ <- function(df) {

    # Input validation
    required_cols <- c("order", "family", "Scientific.name", "date")
    if (!all(required_cols %in% names(df))) {
      missing_cols <- setdiff(required_cols, names(df))
      stop("Missing required columns: ", paste(missing_cols, collapse = ", "))
    }


    # Create summary list
    summary_list <- list()

    # 1. Species Richness by Order
    summary_list$order_richness <- df |>
      dplyr::group_by(order) |>
      dplyr::summarise(n_species = dplyr::n_distinct(Scientific.name),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(n_species))

    # 2. Species Richness by Family
    summary_list$family_richness <- df |>
      dplyr::group_by(order, family) |>
      dplyr::summarise(n_species = dplyr::n_distinct(Scientific.name),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(n_species))

    # 3. Number of calls/detections (if count column provided)
    # By Order
    summary_list$order_calls <- df |>
      dplyr::group_by(order) |>
      dplyr::summarise(n_calls = dplyr::n(),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(n_calls))

    # By Family
    summary_list$family_calls <- df |>
      dplyr::group_by(order, family) |>
      dplyr::summarise(n_calls = dplyr::n(),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(n_calls))


    # 4. Number of unique detection days
    summary_list$order_days <- df |>
      dplyr::group_by(order) |>
      dplyr::summarise(n_days = dplyr::n_distinct(date),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(n_days))

    summary_list$family_days <- df |>
      dplyr::group_by(order, family) |>
      dplyr::summarise(n_days = dplyr::n_distinct(date),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(n_days))

    # 5. Number of unique sensors (if sensor.id column exists)
    if ("sensor.id" %in% names(df)) {
      summary_list$order_sensors <- df |>
        dplyr::group_by(order) |>
        dplyr::summarise(n_sensors = dplyr::n_distinct(sensor.id),
                         .groups = "drop") |>
        dplyr::arrange(dplyr::desc(n_sensors))

      summary_list$family_sensors <- df |>
        dplyr::group_by(order, family) |>
        dplyr::summarise(n_sensors = dplyr::n_distinct(sensor.id),
                         .groups = "drop") |>
        dplyr::arrange(dplyr::desc(n_sensors))
    }


    return(summary_list)
  }

  # Group the data if group_by is specified
  if (!is.null(group_by)) {

    # Validate group_by columns
    missing_cols <- setdiff(group_by, names(df))
    if (length(missing_cols) > 0) {
      stop("Grouping columns not found in dataframe: ",
           paste(missing_cols, collapse = ", "))
    }

    grouped_df <- df |>
      dplyr::group_by(!!!rlang::syms(group_by))

    # Get group values for naming
    group_keys <- dplyr::group_keys(grouped_df)
    group_names <- apply(group_keys, 1, function(x) paste(x, collapse = "_"))

    # Create summaries for each group
    result <- grouped_df |>
      dplyr::group_split() |>
      purrr::map(function(group_data) {
        taxa_summ(group_data)
      }) |>
      purrr::set_names(group_names)

    return(result)

  } else {
    # If no grouping, proceed normally
    return(taxa_summ(df))
  }
}

call_diversity <- function(data) {

  library(vegan)
  library(tibble)

  data <- birdnet_list(data)

  # Shannon's diversity for n.days (number of days each species was detected)
  shannon_days <- diversity(data$n.days, index = "shannon")

  # Shannon's diversity for n.calls (number of calls for each species)
  shannon_calls <- diversity(data$n.calls, index = "shannon")

  # Calculate other common diversity metrics for comparison
  simpson_days <- diversity(data$n.days, index = "simpson")
  simpson_calls <- diversity(data$n.calls, index = "simpson")

  # Species richness (number of species)
  species_richness <- nrow(data)

  # Evenness (Pielou's evenness index)
  evenness_days <- shannon_days / log(species_richness)
  evenness_calls <- shannon_calls / log(species_richness)

  # Create wide format tibble
  diversity_metrics <- tibble(
    shannon_days = round(shannon_days, digits = 2),
    shannon_calls = round(shannon_calls, digits = 2),
    simpson_days = round(simpson_days, digits = 2),
    simpson_calls = round(simpson_calls, digits = 2),
    sp_richness = round(species_richness, digits = 2),
    evenness_days = round(evenness_days, digits = 2),
    evenness_calls = round(evenness_calls, digits = 2)
  )

  return(diversity_metrics)
}

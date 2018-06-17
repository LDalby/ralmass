#' Calculate the fit of habitat use frequencies
#'
#' Calculate the fit between habitat use scored in the field
#' and habitat use based on the simulations.
#'
#' @param obs tibble The table with the field data
#' @param sim tibble The table with simulated data
#' @return tibble The fit per species
#' @export
calc_habuse_fit <- function(sim, obs) {
  if(missing(sim)) stop("Argument sim not specified")
  if(missing(obs)) stop("Argument obs not specified")
  sim %>%
    ungroup() %>%
    rename_all(tolower) %>%
    mutate_if(is.character, tolower) -> sim
  obs %>%
    ungroup() %>%
    rename_all(tolower) %>%
    mutate_if(is.character, tolower) -> obs
  left_join(obs, sim,
            by = c("species", "month", "habitat")) %>%
    replace_na(list(use.y = 0.0)) %>%
    group_by(species, month) %>%
    summarise(fit = sum((use.x - use.y)^2)) %>%
    group_by(species) %>%
    summarise(fit = sum(fit)/n_distinct(month)) %>%
    mutate(species = stringr::str_to_title(species)) %>%
    return()
}

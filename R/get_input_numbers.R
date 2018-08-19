#' Get total number of geese entering the simulation
#'
#' The actual number of geese in the simulation is a function of multiple
#' parameters. This function reads these from the config file and computes
#' the numbers.
#'
#' @param config character The config variable as read in by readLines
#' @return tibble A table with the calculated numbers
#' @export
get_input_numbers <- function(config) {
  if(missing(config)) stop("Input parameter config is missing")
  # Set up table with the variables we need to extract from the config:
  tribble(
    ~species,   ~numbers_cfg,        ~young_prop_cfg,             ~fall_mig_nos_cfg,
    "pinkfoot", "GOOSE_PF_STARTNOS", "GOOSE_PF_YOUNG_PROPORTION", "GOOSE_PF_SPRING_MIG_NOS",
    "greylag",  "GOOSE_GL_STARTNOS", "GOOSE_GL_YOUNG_PROPORTION", "GOOSE_GL_SPRING_MIG_NOS",
    "barnacle", "GOOSE_BN_STARTNOS", "GOOSE_BN_YOUNG_PROPORTION", "GOOSE_BN_SPRING_MIG_NOS"
  ) %>%
    group_by(species) %>%
    mutate(numbers = GetParamValue(numbers_cfg, config),
           spring_number = GetParamValue(fall_mig_nos_cfg, config),
           young_prop = GetParamValue(young_prop_cfg, config),
           number_scaler = GetParamValue("GOOSE_STARTNO_SCALER", config)) %>%
    summarise(total = calc_total_numbers(numbers,
                                         spring_number,
                                         young_prop,
                                         number_scaler)) -> input_goose_numbers
  return(input_goose_numbers)
}

# Helper function to calculate input numbers
calc_total_numbers <- function(start_number, spring_number, young_prop, number_scaler) {
  n_start <- round(start_number * number_scaler)
  n_young <- n_start * young_prop
  n_fam <- n_young/4
  n_nb <- n_start - (n_fam - n_young)
  n_spring <- round(spring_number * number_scaler)
  return(as.integer(n_fam + n_nb + n_spring))
}

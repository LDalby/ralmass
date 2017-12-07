#' Calculate the fit of the simulated flock size distribution to the observed
#' flock size distribution
#'
#' Calculate fit between the simulated flock size distribution to the observed
#' flock size distribution based as 1 minus the least squares difference.
#'
#' The fit is measured on the proportions of the flock sizes falling into
#' each bin of 10% quantiles of the observed range of flock sizes.
#' The fit is in both cases measured over the entire season.
#'
#' @param sim tibble A vector of flock sizes from the simulation
#' @param obs tibble A tibble with a columns identifying the species
#' and a column with each flock observation from the simulation.
#' @param var character Name of the varible to compare. Must be identical
#' in the both sim and obs.
#' @param measture character Either LS (least sq.) or KS (Kolmogorov-Smirnov)
#' @return numeric The calculated fit.
#' @export
CalcFlockSizeFit =  function(sim, obs, var, measure) {
	if (missing(sim))
	  {
	  	stop('sim should be the data from the simulation')
	}
  if (missing(obs))
  {
    stop('obs should be the field data')
  }
  if (missing(var))
  {
    stop('var should specify the name of the variable to compare')
  }
  if (missing(measure))
  {
    stop('measure should specify type of fit')
  }

  if (measure == "LS") {
  obs %>%
    pull(var) -> tmp
	vec <- quantile(tmp, probs = seq(.1,.9,.1))
	flockobs <- findInterval(tmp, vec = vec)

	sim %>%
	  pull(var) %>%
	  findInterval(vec = vec) -> flocksim

	obs <- table(flockobs)/(sum(table(flockobs)))
	sim <- table(flocksim)/(sum(table(flocksim)))

	tabdefault <- rep(0,length(obs))
	names(tabdefault) <- names(obs)  # Get the same names as field data
	tabdefault[match(names(sim), names(tabdefault))] <- sim

	result <- 1 - sum((as.numeric(tabdefault) - as.numeric(obs))^2)
  }
  if (measure == "KS") {
    ks <- ks.test(obs[,var], sim[, var])
    result <- ks[["statistic"]]
  }
	assertthat::assert_that(dplyr::between(result, 0, 1),
	                        msg = "Fit not between 0 and 1")
	return(result)
}

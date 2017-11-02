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
#' @return numeric The calculated fit.
#' @export
CalcFlockSizeFit =  function(sim = NULL, obs = NULL, var = NULL) {
	if (any(is.null(sim), is.null(obs), is.null(var)))
	{
		stop('Input parameter missing')
	}
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

	if (result > 1 | result < 0)
		{
			stop('0 > least squares fit 1 <')  # Should be between 0 and 1
		}
	return(result)
}

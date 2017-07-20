#' Calculate the fit of the simulated forage distance distribution to the
#'  observed forage distance distribution
#'
#' Calculate fit between the simulated forage distance distribution to the observed
#' forage distance distribution based on the sum of standardized squared
#' errors (SSSE, Frank & Baret 2013) or 1 minus the least squares difference.
#'
#' The fit is based on the proportions of the forage distances falling
#' into each bin of 10% quantiles of the observed range of forage distances.
#'  The fit is measured over the entire season.
#'
#' @param Sim data.table A data.table distances calculated by CalcDistToRoosts
#' @param Obs data.table Same as Sim but field observations.
#' @param species character Either Barnacle, Pinkfoot or Greylag.
#' @param measure character Either LS or SSSE
#' @return numeric The calculated fit.
#' @references Frank, B. M. and P. V. Baret (2013). "Simulating brown trout
#'  demogenetics in a river/nursery brook system: The individual-based
#'  model DemGenTrout." Ecological Modelling 248: 184-202.
#' @export
CalcForageDistFit = function(Sim = NULL, Obs = NULL, species = NULL,
							 measure = NULL) {
	if (any(is.null(Sim), is.null(Obs), is.null(species), is.null(measure)))
	{
		stop('Input parameter missing')
	}
	if (!is.data.table(Obs)) {
	  Obs = data.table::as.data.table(Obs)
	}
  tmp = Obs[Species == species,Shortest]
	vec = quantile(tmp, probs = seq(.1,.9,.1))
	distobs = findInterval(tmp, vec = vec)
	if (!is.data.table(Sim)) {
	  Sim = data.table::as.data.table(Sim)
	}
	tmpsim = Sim[Species == species,Shortest]
	distsim = findInterval(tmpsim, vec = vec)

	obs = table(distobs)/(sum(table(distobs)))
	sim = table(distsim)/(sum(table(distsim)))
	tabdefault = rep(0,length(obs))
	names(tabdefault) = names(obs)  # Get the same names as field data
	tabdefault[match(names(sim), names(tabdefault))] = sim
	foragedists = data.table('Sim' = as.numeric(tabdefault), 'Obs' = as.numeric(obs))
	if (measure == 'SSSE')
	{
		result = foragedists[,sum((Sim - Obs)^2/Obs)]
	}
	if (measure == 'LS')
	{
		result = foragedists[,1 - sum((Sim - Obs)^2)]
		if (result > 1 | result < 0)
		{
			stop('0 > least squares fit 1 <')  # Should be between 0 and 1
		}
	}
	return(result)
}

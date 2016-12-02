#' Calculate the fit of the simulated flock size distribution to the observed
#' flock size distribution
#' 
#' Calculate fit between the simulated flock size distribution to the observed
#' flock size distribution based on the sum of standardized squared 
#' errors (SSSE, Frank & Baret 2013). The SSSE is here based on the proportions
#' of the flock sizes falling into each bin of roughly 1/10 of the observed
#' range of flock sizes. The upper bins were truncated to avoid empty bins.
#' The fit is measured over the entire season.
#' 
#' @param Sim data.table A data.table with a columns identifying the species 
#' and a column with each flock observation from the simulation.
#' @param Obs data.table Same as Sim but field observations.
#' @param species character Either Barnacle, Pinkfoot or Greylag.
#' @references Frank, B. M. and P. V. Baret (2013). "Simulating brown trout
#'  demogenetics in a river/nursery brook system: The individual-based
#'  model DemGenTrout." Ecological Modelling 248: 184-202.
#' @export
CalcFlockSizeFit = function(Sim = NULL, Obs = NULL, species = NULL) {
	if(any(is.null(Sim), is.null(Obs), is.null(species)))	 
	{
		stop('Input parameter missing')
	}
	tmp = Obs[Species == species,Numbers]
	vec = quantile(tmp, probs = seq(.1,.9,.1))
	flockobs = findInterval(tmp, vec = vec)
	tmpsim = Sim[Species == species,Numbers]
	flocksim = findInterval(tmpsim, vec = vec)

	obs = table(flockobs)/(sum(table(flockobs)))
	sim = table(flocksim)/(sum(table(flocksim)))
	tabdefault = rep(0,length(obs))
	names(tabdefault) = names(obs)  # Get the same names as field data
	tabdefault[match(names(sim), names(tabdefault))] = sim
	flocksizes = data.table('Sim' = as.numeric(sim), 'Obs' = as.numeric(obs))
	return(flocksizes[,sum((Sim-Obs)^2/Obs)])
}

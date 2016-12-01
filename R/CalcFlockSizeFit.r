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
	if(species == 'Greylag') {
		vec = c(52,104,156,208,260,312,364)
		flocksim = findInterval(Sim[Species == 'GreylagTimed', Numbers], vec = vec)
		flockobs = findInterval(Obs[Species == 'Greylag', Numbers], vec = vec)
	}
	if(species == 'Pinkfoot') {
		vec = c(235,470,705,940,1175,1410,1645,1880)
		flocksim = findInterval(Sim[Species == 'PinkfootTimed', Numbers], vec = vec)
		flockobs = findInterval(Obs[Species == 'Pinkfoot', Numbers], vec = vec)
	}
	if(species == 'Barnacle') {
		vec = c(440,880,1320,1760,2200,2640,3080,3520)
		flocksim = findInterval(Sim[Species == 'BarnacleTimed', Numbers], vec = vec)
		flockobs = findInterval(Obs[Species == 'Barnacle', Numbers], vec = vec)
	}
	obs = table(flockobs)/(sum(table(flockobs)))
	sim = table(flocksim)/(sum(table(flocksim)))
	tabdefault = rep(0,length(obs))
	names(tabdefault) = names(obs)  # Get the same names as field data
	tabdefault[match(names(sim), names(tabdefault))] = sim
	flocksizes = data.table('Sim' = as.numeric(sim), 'Obs' = as.numeric(obs))
	return(flocksizes[,sum((Sim-Obs)^2/Obs)])
}

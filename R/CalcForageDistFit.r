#' Calculate the fit of the simulated forage distance distribution to the
#'  observed forage distance distribution
#' 
#' Calculate fit between the simulated forage distance distribution to the observed
#' forage distance distribution based on the sum of standardized squared 
#' errors (SSSE, Frank & Baret 2013). The SSSE is here based on the proportions
#' of the forage distances falling into each bin of 10% quantiles of the observed
#' range of forage distances. The fit is measured over the entire season.
#' 
#' @param Sim data.table A data.table distances calculated by CalcDistToRoosts
#' @param Obs data.table Same as Sim but field observations.
#' @param species character Either Barnacle, Pinkfoot or Greylag.
#' @references Frank, B. M. and P. V. Baret (2013). "Simulating brown trout
#'  demogenetics in a river/nursery brook system: The individual-based
#'  model DemGenTrout." Ecological Modelling 248: 184-202.
#' @export
CalcForageDistFit = function(Sim = NULL, Obs = NULL, species = NULL) {
	if(any(is.null(Sim), is.null(Obs), is.null(species)))	 
	{
		stop('Input parameter missing')
	}
	tmp = Obs[Species == species,Shortest]
	vec = quantile(tmp, probs = seq(.1,.9,.1))
	distobs = findInterval(tmp, vec = vec)
	tmpsim = Sim[Species == species,Shortest]
	distsim = findInterval(tmpsim, vec = vec)
	
	obs = table(distobs)/(sum(table(distobs)))
	sim = table(distsim)/(sum(table(distsim)))
	tabdefault = rep(0,length(obs))
	names(tabdefault) = names(obs)  # Get the same names as field data
	tabdefault[match(names(sim), names(tabdefault))] = sim
	foragedists = data.table('Sim' = as.numeric(sim), 'Obs' = as.numeric(obs))
	return(foragedists[,sum((Sim-Obs)^2/Obs)])
}

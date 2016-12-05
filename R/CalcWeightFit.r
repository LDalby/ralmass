#' Calculate the fit of the simulated weights to the weights observed
#'
#' Calculate fit between field weights and simulated weights using either least 
#' squares or the sum of standardized squared errors (SSSE, Frank & Baret 2013)
#' 
#' @param Sim data.table The file GooseEnergeticsData.txt
#' @param Field data.table The observed weights
#' @param measure character Fit measure to use. Either LS or SSSE
#' @return numeric The calculated fit.
#' @references Frank, B. M. and P. V. Baret (2013). "Simulating brown trout
#'  demogenetics in a river/nursery brook system: The individual-based
#'  model DemGenTrout." Ecological Modelling 248: 184-202.
#' @export
CalcWeightFit = function(Sim = NULL, Field = NULL, measure = NULL) {
	if(any(is.null(measure), is.null(Sim), is.null(Field)))	 
	{
		stop('Input parameter missing')
	}
	Sim[, Date:=as.Date(Day-365, origin = '2012-01-01')]
	Sim = Sim[data.table::month(Date) %in% c(9:12,1:3)]
	Sim[, Week:=data.table::week(as.Date(DayInYear, origin = '2012-01-01'))]

	Field = Field[Date > lubridate::ymd('2010-08-01') & 
	Date < lubridate::ymd('2015-05-31'),]
	Field[, Week:=data.table::week(Date)]
	Field[, MeanWeightField:=mean(Weight), by = Week]
	Field = unique(Field[,.(Week, MeanWeightField)])
	data.table::setkey(Field, Week)
	# Calculate least squares
	seasons = unique(Sim[, Season])
	fits = rep(NA, length(seasons))
	for (i in seq_along(seasons)) {
		full = merge(Field, Sim[Season == seasons[i], .(Week, MeanWeight)])
		if(measure == 'LS') {
			themin = min(full[,.(MeanWeight, MeanWeightField)])
			themax = max(full[,.(MeanWeight, MeanWeightField)])
			denum = themax-themin
			full[, MeanWeight:=(MeanWeight-themin)/denum, by = Week]
			full[, MeanWeightField:=(MeanWeightField-themin)/denum, by = Week]
			fits[i] = full[, 1-sum((MeanWeight-MeanWeightField)^2)/length(unique(Week))]
		}
		if(measure == 'SSSE') {
			fits[i] = full[, sum((MeanWeight-MeanWeightField)^2/MeanWeightField)]
		}
	}
	names(fits) = paste0('Season', seasons)
	if(any(fits < 0)) {
		stop('Fit less than 0')
	}
	return(fits)
}
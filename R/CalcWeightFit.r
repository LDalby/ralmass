#' Calculate the fit of the simulated weights to the weights observed
#'
#' Calculate fit between field weights as least squares difference
#'
#' @param sim data.frame The file GooseEnergeticsData.txt
#' @param field data.frame The observed weights
#' @return numeric The calculated fit.
#' @export
CalcWeightFit = function(sim = NULL, field = NULL) {
	if(any(is.null(sim), is.null(field)))
	{
		stop('Input parameter missing')
	}
  # Calculate least squares
  themin <- min(sim$MeanWeight, field$MeanWeightField)
  themax <- max(sim$MeanWeight, field$MeanWeightField)
  denum <- themax-themin
  inner_join(sim, field, by = "week") %>%
    mutate(MeanWeight = (MeanWeight-themin)/denum,
           MeanWeightField = (MeanWeightField-themin)/denum) %>%
    summarise(fit = 1-sum((MeanWeight-MeanWeightField)^2)/n_distinct(week)) %>%
    pull(fit) -> fit

  assertthat::assert_that(fit > 0,
                          msg = "Fit less than 0")
	return(fit)
}

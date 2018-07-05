#' Calculate the fit of the simulated weights to the weights observed
#'
#' Calculate fit between field weights as least squares difference
#'
#' @param sim data.frame The file GooseEnergeticsData.txt
#' @param field data.frame The observed weights
#' @return numeric The calculated fit.
#' @export
calc_weight_fit = function(sim = NULL, field = NULL) {
	if(any(is.null(sim), is.null(field)))
	{
		stop('Input parameter missing')
	}
  # Calculate least squares
  themin <- min(sim$mean_weight, field$mean_weight_field)
  themax <- max(sim$mean_weight, field$mean_weight_field)
  denum <- themax-themin
  inner_join(sim, field, by = "week") %>%
    mutate(mean_weight = (mean_weight-themin)/denum,
           mean_weight_field = (mean_weight_field-themin)/denum) %>%
    summarise(fit = 1-sum((mean_weight-mean_weight_field)^2)/n_distinct(week)) %>%
    pull(fit) -> fit

  assertthat::assert_that(fit > 0,
                          msg = "Fit less than 0")
	return(fit)
}

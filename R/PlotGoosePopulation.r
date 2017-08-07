#' Plot goose population data
#'
#' Plot the number of geese per species and type
#'
#' @param data tibble The ALMaSS data file GoosePopulationData.txt
#' @return A nice plot
#' @export
PlotGoosePopulation = function(data){
	# Remodel:
  molten <- tidyr::gather_(data, gather_cols = c("Day", "Season"),
		key_col = "GooseType", value_col = "Numbers") %>%
	  dplyr::filter(Numbers > 0) %>%
    dplyr::mutate(Date = as.Date(Day, origin = '2010-01-01')) %>%
	# Plot:
	  ggplot2::ggplot(ggplot2::aes(Date, Numbers, group = GooseType)) +
		ggplot2::geom_line(ggplot2::aes(colour = GooseType)) +
		ggplot2::geom_point(ggplot2::aes(colour = GooseType)) +
		ggplot2::ylab('Numbers') +
	  ggplot2::theme_bw() +
	  ggplot2::scale_x_date(date_breaks = "1 month", date_labels = "%b") +
	  ggplot2::xlab('Month') +
	  ggplot2::scale_color_brewer(palette = 'Paired')
	return(p)
}



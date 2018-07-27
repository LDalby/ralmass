#' Plot goose population data
#'
#' Plot the number of geese per species and type
#'
#' @param data tibble The ALMaSS data file GoosePopulationData.txt
#' @param dates logical Should we plot day numbers as dates?
#' @return A nice plot
#' @export
PlotGoosePopulation = function (data, dates = FALSE)
{
  cols <- c("pf_families", "pf_non_breeders", "bn_families", "bn_non_breeders", "gl_families", "gl_non_breeders")
  tidyr::gather_(data,
          gather_cols = cols,
          key_col = "GooseType",
          value_col = "numbers") %>%
    dplyr::filter(numbers > 0) -> gathered

  if (dates) {
    gathered %>%
      dplyr::mutate(day = as.Date(day, origin = "2010-01-01")) -> gathered
  }
  p <- ggplot2::ggplot(gathered, ggplot2::aes(day, numbers, group = GooseType)) +
    ggplot2::geom_line(ggplot2::aes(colour = GooseType)) +
    ggplot2::geom_point(ggplot2::aes(colour = GooseType)) +
    ggplot2::ylab("Numbers") + ggplot2::theme_bw()
  if (dates) {
    p <- p + ggplot2::scale_x_date(date_breaks = "1 month",
                                   date_labels = "%b") + ggplot2::xlab("Month")
  }
  return(p + scale_color_brewer(palette = "Paired"))
}



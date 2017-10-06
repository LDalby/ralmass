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
  cols <- c("PFFamilies", "PFNonBreeders", "BNFamilies", "BNNonBreeders", "GLFamilies", "GLNonBreeders")
  tidyr::gather_(data,
          gather_cols = cols,
          key_col = "GooseType",
          value_col = "Numbers") %>%
    dplyr::filter(Numbers > 0) -> gathered

  if (dates) {
    gathered %>%
      dplyr::mutate(Day = as.Date(Day, origin = "2010-01-01")) -> gathered
  }
  p <- ggplot2::ggplot(gathered, ggplot2::aes(Day, Numbers, group = GooseType)) +
    ggplot2::geom_line(ggplot2::aes(colour = GooseType)) +
    ggplot2::geom_point(ggplot2::aes(colour = GooseType)) +
    ggplot2::ylab("Numbers") + ggplot2::theme_bw()
  if (dates) {
    p <- p + ggplot2::scale_x_date(date_breaks = "1 month",
                                   date_labels = "%b") + ggplot2::xlab("Month")
  }
  return(p + scale_color_brewer(palette = "Paired"))
}



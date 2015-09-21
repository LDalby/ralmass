#' Plot forage available to geese
#'
#' Plot the forage available as grazing and grain available to geese in ALMaSS
#' 
#' @param data data.frame or data.table The ALMaSS data file GooseFieldForageData.txt
#' @param species character One of the goose species ('Pinkfoot', 'Barnacle', 'Greylag') defaults to 'Pinkfoot'
#' @param type character What forage should be plotted ('Grass', 'Grain' or 'Combined'). No default.
#' @param plottype character The plot type. Either 'line' or 'points' (default)
#' @param dates logical Use dates?
#' @return A nice plot
#' @export

PlotGooseForage = function(data, species = 'Pinkfoot', type = NULL, plottype = 'points', dates = TRUE){
	types = c('Grass', 'grass', 'Grain', 'grain', 'combined', 'Combined')
	if(is.null(type) | !is.character(type) | !type %in% types) 
	{
		stop('Please specify type as either: Grass, Grain or Combined.\n')
	}
	
	ggplot2::theme_update(
		panel.background = ggplot2::element_blank(),
		axis.text.x = ggplot2::element_text(colour = "black", face = "plain", size = 10),
		axis.text.y = ggplot2::element_text(colour = "black", face = "plain", size = 10),
		axis.title.x = ggplot2::element_text(colour = "black", face = "plain", size = 12),
		axis.title.y = ggplot2::element_text(colour = "black", face = "plain", angle = 90, size = 12),
		panel.grid.minor = ggplot2::element_blank(),
		panel.grid.major = ggplot2::element_blank(),
		panel.background = ggplot2::element_blank(),
		axis.line = ggplot2::element_line(size = 0.5),
		axis.ticks = ggplot2::element_line(colour = "black", size = 0.5)
		)
	col = rgb(0,0,0, alpha = 0.2)
	column = paste('Grass', species, sep = '')
	if(dates) 
	{
		data[,Day:=as.Date(Day, origin = as.Date(paste(Year + 1989, "-01-01", sep = '')))]
	}	

	if(tolower(plottype) == 'line') 
	{
		grassBarnacle = ggplot2::ggplot(data[Barnacle > 0,], ggplot2::aes(Day, GrassBarnacle, group = Polyref)) + 
		ggplot2::geom_line(colour = col) + ggplot2::ylab('Grazing forage available') + 
		ggplot2::scale_x_continuous(breaks = seq(0, 365, by = 25)) + ggtitle('Grazing barnacle')
		grainBarnacle = ggplot2::ggplot(data[Barnacle > 0,], ggplot2::aes(Day, Grain, group = Polyref)) + 
		ggplot2::geom_line(colour = col) + ggplot2::ylab('Grain forage available') +
		ggplot2::scale_x_continuous(breaks = seq(0, 365, by = 25)) + ggtitle('Grain barnacle')

		grassPinkfoot = ggplot2::ggplot(data[Pinkfoot > 0,], ggplot2::aes(Day, GrassPinkfoot, group = Polyref)) + 
		ggplot2::geom_line(colour = col) + ggplot2::ylab('Grazing forage available') + 
		ggplot2::scale_x_continuous(breaks = seq(0, 365, by = 25))+ ggtitle('Grazing pinkfoot')
		grainPinkfoot = ggplot2::ggplot(data[Pinkfoot > 0,], ggplot2::aes(Day, Grain, group = Polyref)) + 
		ggplot2::geom_line(colour = col) + ggplot2::ylab('Grain forage available') +
		ggplot2::scale_x_continuous(breaks = seq(0, 365, by = 25))+ ggtitle('Grain pinkfoot')

		grassGreylag = ggplot2::ggplot(data[Greylag > 0,], ggplot2::aes(Day, GrassGreylag, group = Polyref)) + 
		ggplot2::geom_line(colour = col) + ggplot2::ylab('Grazing forage available') + 
		ggplot2::scale_x_continuous(breaks = seq(0, 365, by = 25))+ ggtitle('Grazing Greylag')
		grainGreylag = ggplot2::ggplot(data[Greylag > 0,], ggplot2::aes(Day, Grain, group = Polyref)) + 
		ggplot2::geom_line(colour = col) + ggplot2::ylab('Grain forage available') +
		ggplot2::scale_x_continuous(breaks = seq(0, 365, by = 25))+ ggtitle('Grain Greylag')
	}

	if(tolower(plottype) == 'point') 
	{
		grassBarnacle = ggplot2::ggplot(data[Barnacle > 0 & Grain < 99,], ggplot2::aes(Day, GrassBarnacle, group = Polyref)) + 
		ggplot2::geom_point(colour = col, aes(size = log10(Barnacle))) + ggplot2::ylab('Grazing forage available') + ggtitle('Grazing barnacle')
		grainBarnacle = ggplot2::ggplot(data[Barnacle > 0  & Grain > 98,], ggplot2::aes(Day, Grain, group = Polyref)) + 
		ggplot2::geom_point(colour = col, aes(size = log10(Barnacle))) + ggplot2::ylab('Grain forage available') +
		 ggtitle('Grain barnacle')

		grassPinkfoot = ggplot2::ggplot(data[Pinkfoot > 0 & Grain < 135,], ggplot2::aes(Day, GrassPinkfoot, group = Polyref)) + 
		ggplot2::geom_point(colour = col, aes(size = log10(Pinkfoot))) + ggplot2::ylab('Grazing forage available') + 
		 ggtitle('Grazing pinkfoot')
		grainPinkfoot = ggplot2::ggplot(data[Pinkfoot > 0 & Grain > 134,], ggplot2::aes(Day, Grain, group = Polyref)) + 
		ggplot2::geom_point(colour = col, aes(size = log10(Pinkfoot))) + ggplot2::ylab('Grain forage available') +
		 ggtitle('Grain pinkfoot')

		grassGreylag = ggplot2::ggplot(data[Greylag > 0 & Grain < 242,], ggplot2::aes(Day, GrassGreylag, group = Polyref)) + 
		ggplot2::geom_point(colour = col, aes(size = log10(Greylag))) + ggplot2::ylab('Grazing forage available') + 
		 ggtitle('Grazing Greylag')
		grainGreylag = ggplot2::ggplot(data[Greylag > 0 & Grain > 241,], ggplot2::aes(Day, Grain, group = Polyref)) + 
		ggplot2::geom_point(colour = col, aes(size = log10(Greylag))) + ggplot2::ylab('Grain forage available') +
		 ggtitle('Grain Greylag')
	}

	if(tolower(type) == 'combined') 
	{
		gridExtra::grid.arrange(grassBarnacle, grainBarnacle, grassPinkfoot, grainPinkfoot, grassGreylag, grainGreylag, nrow = 3, ncol = 2)
	}
	if(type == 'grain' | type == 'Grain') 
	{
		if(species %in% c('pinkfoot', 'Pinkfoot'))
		{
			return(grainPinkfoot)
		}
		if(species %in% c('barnacle', 'Barnacle'))
		{
			return(grainBarnacle)
		}
		if(species %in% c('greylag', 'Greylag'))
		{
			return(grainGreylag)
		} 
		else (return(gridExtra::grid.arrange(grainPinkfoot, grassGreylag, grainGreylag, nrow = 3, ncol = 1)))
	}
	if(tolower(type) == 'grass') 
	{
		return(grass)
	}
}

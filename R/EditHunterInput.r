#' Edit input file for the Hunter model
#'
#' Some of the input parameters for the hunter model are given as individual
#' values to each object. This is controlled via the
#'  Hunter_Hunting_Locations.txt file and this \code{EditHunterInput} will
#' apply changes to specified inputs.
#' 
#' \code{change} gives the proportion of hunters which will have a 1 in 
#' WeekdayHunterChance or GooseLookChance. When \code{column} is HuntingDays
#' \code{change} will increase (when \code{change} > 1) or decrease 
#' (when \code{change} < 1) the already existing distribution of HuntingDays. 
#' When \code{column} is Efficiency, \code{change} will be the actual value
#' given to all hunters.
#' 
#' \code{weekbehav} controls the weekly hunting behaviour. 0 is weekend hunting
#' only (default), 1 is not weekend only and 2 means leave a refraction period
#'  in between hunting events. The refraction period is controlled by a config
#'  variable in ALMaSS. 
#' 
#' @param hhlpath character Path to an existing Hunter_Hunting_Locations.txt file
#' @param column character Name of column to apply change to
#' @param change numeric The change to apply. See details
#' @param weekbehav numeric The weekly hunting behaviour. See details
#' @return A tab separated text file formatted as an ALMaSS input file.
#' @export
EditHunterInput = function(hhlpath = NULL, column = NULL, change = NULL, weekbehav = 0) {
	if(any(is.null(hhlpath), is.null(column), is.null(change))){
		stop('Input parameter missing')
	}
	hhl = data.table::fread(hhlpath, skip = 1)
	if(column == 'HuntingDays'){
		hhl[, HuntingDays:=round(HuntingDays*change)]
	}
	if(column == 'WeekdayHunterChance'){
		if(change > 1.0 | change < 0.0 ){
			stop('Invalid proportion of weekday hunters')
		}
		hunters = nrow(hhl)
		weekdayhunters = round(hunters*change)
		weekendhunters = hunters-weekdayhunters
		thehunters = c(rep(1, weekdayhunters), rep(weekbehav, weekendhunters))
		final = sample(x = thehunters, replace = FALSE)
		hhl[, WeekdayHunterChance:=final]
	}
	if(column == 'GooseLookChance'){
		if(change > 1.0 | change < 0.0 ){
			stop('Invalid proportion of checkers hunters')
		}
		hunters = nrow(hhl)
		checkers = round(hunters*change)
		noncheckers = hunters-checkers
		thehunters = c(rep(1, checkers), rep(0, noncheckers))
		final = sample(x = thehunters, replace = FALSE)
		hhl[, GooseLookChance:=final]
	}
	if(column == 'Efficiency'){
		hhl[, Efficiency:=change]
	}
	WriteAlmassInput(table = hhl, pathtofile = hhlpath) 
}
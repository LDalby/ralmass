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
#' @param hhl data.table An existing Hunter_Hunting_Locations.txt file
#' @param column character Name of column to apply change to
#' @param change numeric The change to apply. See details
#' @return A tab separated text file formatted as an ALMaSS input file.
#' @export
EditHunterInput = function(hhl = NULL, column = NULL, change = NULL) {
	if(any(is.null(hhl), is.null(column), is.null(change))){
		stop('Input parameter missing')
	}
	if(column == 'HuntingDays'){
		hhl[, HuntingDays:=round(HuntingDays*change)]
	}
	if(column == 'WeekdayHunterChance'){
		hunters = nrow(hhl)
		weekdayhunters = round(hunters*change)
		weekendhunters = hunters-weekdayhunters
		thehunters = c(rep(1, weekdayhunters), rep(0, weekendhunters))
		final = sample(x = thehunters, replace = FALSE)
		hhl[, WeekdayHunterChance:=final]
	}
	if(column == 'GooseLookChance'){
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
	WriteAlmassInput(table = hhl, pathtofile = file.path(getwd(), 'Hunter_Hunting_Locations.txt')) 
}
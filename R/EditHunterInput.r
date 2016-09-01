#' Edit input file for the Hunter model
#'
#' Some of the input parameters for the hunter model are given as individual
#' values to each object. This is controlled via the
#'  Hunter_Hunting_Locations.txt file and \code{EditHunterInput} will
#' apply changes to specified inputs.
#' 
#' \code{change} gives the proportion of hunters which will have a 1 in 
#' WeekdayHunterChance or GooseLookChance. When \code{parameter} is HuntingDays
#' \code{change} will increase (when \code{change} > 1) or decrease 
#' (when \code{change} < 1) the already existing distribution of HuntingDays. 
#' When \code{parameter} is Efficiency, \code{change} will be the actual value
#' given to all hunters.
#' 
#' \code{weekbehav} controls the weekly hunting behaviour. 0 is weekend hunting
#' only (default), 1 is not weekend only and 2 means leave a refraction period
#'  in between hunting events. The refraction period is controlled by a config
#'  variable in ALMaSS. 
#' 
#' \code{file} is the orginal Hunter_Hunting_Locations.txt file to modify and 
#' \code{hhlpath} gives the path to where the modified 
#' Hunter_Hunting_Locations.txt is written to.
#' 
#' \code{huntersubset} Defines the subset of hunters to apply change to. 
#' By default we only modify WeekdayHunterChance and GooseLookChance for
#' the 343 expert goose hunters. If the change should be applied across
#' all hunters, set \code{huntersubset = 'all'}. The 403 occasional hunters
#' can be modified by using \code{huntersubset = 'occasional'} and the expert
#' group by \code{huntersubset = 'experts'}
#' 
#' @param file character Path to the existing Hunter_Hunting_Locations.txt file
#' @param hhlpath character Path to an existing Hunter_Hunting_Locations.txt file
#' @param parameter character Name of parameter to apply change to
#' @param change numeric The change to apply. See details
#' @param weekbehav numeric The weekly hunting behaviour. See details
#' @param huntersubset character Should the behaviour of all
#'  hunters be modified? See details
#' @return A tab separated text file formatted as an ALMaSS input file.
#' @export
EditHunterInput = function(file = NULL, hhlpath = NULL, parameter = NULL,
 change = NULL, weekbehav = 0, huntersubset = 'all') {
	if(any(is.null(hhlpath), is.null(parameter), is.null(change), is.null(file))) {
		stop('Input parameter missing')
	}
	if(!tolower(huntersubset) %in% c('all', 'experts', 'occasional')) {
		stop('Invalid huntersubset')
	}
	hhl = data.table::fread(file, skip = 1)
	if(parameter == 'HuntingDays'){
		hhl[, HuntingDays:=round(HuntingDays*change)]
	}
	if(parameter == 'WeekdayHunterChance'){
		if(change > 1.0 | change < 0.0 ){
			stop('Invalid proportion of weekday hunters')
		}
		# 
		if(tolower(huntersubset) == 'all') {
			hunters = nrow(hhl)
		}
		if(tolower(huntersubset) == 'experts') { 
			start = 1 
			stop = 343
			hunters = length(start:stop)
		}
		if(tolower(huntersubset) == 'occasional') { 
			start = 344 
			stop = 746
			hunters = length(start:stop)
		}
		weekdayhunters = round(hunters*change)
		weekendhunters = hunters-weekdayhunters
		thehunters = c(rep(1, weekdayhunters), rep(weekbehav, weekendhunters))
		final = sample(x = thehunters, replace = FALSE)
		if(tolower(huntersubset) == 'all') {
			hhl[, WeekdayHunterChance:=final]
		}
		if(tolower(huntersubset) != 'all') {
			hhl[start:stop, WeekdayHunterChance:=final]
		}
	}
	if(parameter == 'GooseLookChance'){
		if(change > 1.0 | change < 0.0 ){
			stop('Invalid proportion of checkers hunters')
		}
		if(tolower(huntersubset) == 'all') {
			hunters = nrow(hhl)
		}
		if(tolower(huntersubset) == 'experts') { 
			start = 1 
			stop = 343
			hunters = length(start:stop)
		}
		if(tolower(huntersubset) == 'occasional') { 
			start = 344 
			stop = 746
			hunters = length(start:stop)
		}
		checkers = round(hunters*change)
		noncheckers = hunters-checkers
		thehunters = c(rep(1, checkers), rep(0, noncheckers))
		final = sample(x = thehunters, replace = FALSE)
		if(tolower(huntersubset) == 'all') {
			hhl[, GooseLookChance:=final]
		}
		if(tolower(huntersubset) != 'all') {
			hhl[start:stop, GooseLookChance:=final]
		}
	}
	if(parameter == 'Efficiency'){
		hhl[, Efficiency:=change]
	}
	if(parameter == 'NumberOfHunters') 
	{
		nohunters = nrow(hhl)
		newnohunters = round(nohunters*change)
		if(change > 1) 
		{
			rest = newnohunters %% nohunters
			if(rest != 0) 
			{
				resthunters = sample(1:nohunters, rest, replace = FALSE)
				multiple = floor(change)
				tmp = hhl
				for (i in 1:multiple) {
					tmp = rbind(tmp, hhl)
				}
				hhl = rbind(tmp, hhl[resthunters,])
			}
			if(rest == 0)
			{
				tmp = hhl
				for (i in 1:(change-1)) {
					tmp = rbind(tmp, hhl)
				}
				hhl = tmp
			}
		}
		if(change < 1){
			newnohunters = round(nohunters*change)
			chosenones = sample(1:nohunters, newnohunters, replace = FALSE)
			hhl = hhl[chosenones,]
		}
	}
	WriteAlmassInput(table = hhl, pathtofile = hhlpath) 
}
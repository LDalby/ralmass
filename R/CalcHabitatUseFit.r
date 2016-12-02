#' Calculate the fit of habitat use frequencies
#'
#' Calculate the fit between habitat use scored in the field 
#' and habitat use based on the simulations. The timing of the counts in 
#' the simulation is controlled by the config variable GOOSE_TIMEDCOUNTS
#'
#' @param FieldData data.table The table with the field data
#' @param SimData data.table The field forage output file from ALMaSS
#' @param measure character The method to use for estimating the fit. Either
#' 'SSSE' or 'LS'
#' @return data.table The fit per species and month
#' @export
CalcHabitatUseFit = function(FieldData, SimData, measure = NULL) {
	if(is.null(measure)) 
	{
		stop('Input parameter missing')
	}
# Field data:
	fielddata = FieldData[, .(Month, HabitatUse, N, Species)]
	h = unique(fielddata[, HabitatUse])
	sp = unique(fielddata[, Species])
	mths = unique(fielddata[, Month])
	fieldcombs = expand.grid(N = 0, HabitatUse = h, Month = mths, Species =  sp,
		stringsAsFactors = FALSE)
	fieldcombs = data.table::as.data.table(fieldcombs)
	fielddata = rbind(fielddata, fieldcombs)
	cols = c('Species', 'HabitatUse', 'Month')
	fielddata = fielddata[, lapply(.SD, sum, na.rm = TRUE), by = cols]
	fielddata[, PropField:=N/sum(N), by=.(Month, Species)]
	fielddata[, N:=NULL]
	data.table::setkeyv(fielddata, cols)
# field data end
	
# Sim data:
	SimData = SimData[Month %in% mths,]
	newnames = c('HabitatUse', 'N')
# Pinkfoot
	hbpf = SimData[, .(PinkfootTimed, HabitatUsePF, Month)]
	hbpf[, Species:='Pinkfoot']
	data.table::setnames(hbpf, old = c('HabitatUsePF', 'PinkfootTimed'), new = newnames)
# Greylag
	hbgl = SimData[, .(GreylagTimed, HabitatUseGL, Month)]
	hbgl[, Species:='Greylag']
	data.table::setnames(hbgl, old = c('HabitatUseGL', 'GreylagTimed'), new = newnames)
# Barnacle
	hbbn = SimData[, .(BarnacleTimed, HabitatUseBN, Month)]
	hbbn[, Species:='Barnacle']
	data.table::setnames(hbbn, old = c('HabitatUseBN', 'BarnacleTimed'), new = newnames)
# Full data
	hb = rbind(hbpf, hbbn, hbgl)
	hb = hb[complete.cases(hb),]
	hbpfMonths = hb[Species == 'Pinkfoot', unique(Month)]
	hbglMonths = hb[Species == 'Greylag', unique(Month)]
	hbbnMonths = hb[Species == 'Barnacle', unique(Month)]
# Fill in zeros in all habitat classes
	totalh = unique(c(hb[,HabitatUse], h))
	combs = expand.grid(N = 0, HabitatUse = totalh, Month = mths,
		Species = sp, stringsAsFactors = FALSE) 
	combs = data.table::as.data.table(combs)

	temp = rbind(hb, combs)
	temp[,PropSim:=N/sum(N), by=.(Month, Species)]
	temp = temp[, lapply(.SD, sum, na.rm = TRUE), by = cols]
	temp = unique(temp[, .(HabitatUse, Month, Species, PropSim)][is.na(PropSim), PropSim:=0])
	data.table::setkeyv(temp, cols)

	HabFit = merge(temp, fielddata)
	if(measure == 'SSSE') {
		HabFit = HabFit[PropField > 0, SSE:=(PropSim-PropField)^2/PropField, by = cols]
		# Sum to get the SSSE
		HabFit = HabFit[, sum(SSE, na.rm = TRUE), by = Species]
		setnames(HabFit, c('Species', 'SSSE'))
		return(HabFit)
		}
		if(measure == 'LS') {
			HabFit = HabFit[, Fit:=1-sum(abs(PropSim-PropField)), by = cols]
			# Knock out cases where popn in sim went extinct:
			HabFit[Species == 'Barnacle' & !Month %in% hbbnMonths, Fit:=NA]
			HabFit[Species == 'Greylag' & !Month %in% hbglMonths, Fit:=NA]
			HabFit[Species == 'Pinkfoot' & !Month %in% hbpfMonths, Fit:=NA]
			# Calculate the sesonal fit as the mean of the monthly fits:
			HabFit[, MonthlyFit:=sum(Fit, na.rm = TRUE)/length(h), by = c('Species', 'Month')]
			HabFit = unique(HabFit[!is.na(Fit), .(Species, MonthlyFit)])
			HabFit[,Fit:=sum(MonthlyFit)/.N, by = 'Species']
			HabFit = unique(HabFit[, .(Species, Fit)])
			return(HabFit)
		}
	}

#' Calculate the fit of habitat use frequencies
#'
#' Calculate the fit between habitat use scored in the field 
#' and habitat use based on the simulations
#'
#' @param FieldData data.table The table with the field data
#' @param SimData data.table The field forage output file from ALMaSS
#' @return data.table The fit per species and month
#' @export
CalcHabitatUseFit = function(FieldData, SimData) {
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
	fielddata[, NMTotal:=sum(N), by=.(Month, Species)]
	fielddata[, PropField:=N/NMTotal]
	fielddata[, c('N', 'NMTotal'):=NULL]
	data.table::setkeyv(fielddata, cols)
# field data end
	
# Sim data:
	SimData = SimData[Month %in% mths,]
	newnames = c('HabitatUse', 'N')
# Pinkfoot
	hbpf = SimData[, .(Pinkfoot, HabitatUsePF, Month)]
	hbpf[, Species:='Pinkfoot']
	data.table::setnames(hbpf, old = c('HabitatUsePF', 'Pinkfoot'), new = newnames)
# Greylag
	hbgl = SimData[, .(Greylag, HabitatUseGL, Month)]
	hbgl[, Species:='Greylag']
	data.table::setnames(hbgl, old = c('HabitatUseGL', 'Greylag'), new = newnames)
# Barnacle
	hbbn = SimData[, .(Barnacle, HabitatUseBN, Month)]
	hbbn[, Species:='Barnacle']
	data.table::setnames(hbbn, old = c('HabitatUseBN', 'Barnacle'), new = newnames)
# Full data
	hb = rbind(hbpf, hbbn, hbgl)
	hb = hb[complete.cases(hb),]
	hbpfMonths = unique(hb[Species == 'Pinkfoot', Month])
	hbglMonths = unique(hb[Species == 'Greylag', Month])
	hbbnMonths = unique(hb[Species == 'Barnacle', Month])
# Fill in zeros in all habitat classes
	h = unique(c(hb[,HabitatUse], h))
	combs = expand.grid(N = 0, HabitatUse = h, Month = mths,
		Species = sp, stringsAsFactors = FALSE) 
	combs = data.table::as.data.table(combs)

	temp = rbind(hb, combs)
	temp[,NMTotal:=sum(N), by=.(Month, Species)]
	temp[, PropSim:=N/NMTotal]
	temp = temp[, lapply(.SD, sum, na.rm = TRUE), by = cols]
	temp = temp[, .(HabitatUse, Month, Species, PropSim)][is.na(PropSim), PropSim:=0] %>% unique
	data.table::setkeyv(temp, cols)

	HabFit = merge(temp, fielddata)
	cols = c('Species', 'Month')
	HabFit = HabFit[, Fit:=1-sum((PropSim-PropField)^2), by = cols]
	HabFit = HabFit[, .(Species, Month, Fit)] %>% unique
	# Knock out cases where popn in sim went extinct:
	HabFit[Species == 'Barnacle' & !Month %in% hbbnMonths, Fit:=NA]
	HabFit[Species == 'Greylag' & !Month %in% hbglMonths, Fit:=NA]
	HabFit[Species == 'Pinkfoot' & !Month %in% hbpfMonths, Fit:=NA]
	setkeyv(HabFit, cols)
	return(HabFit)
}
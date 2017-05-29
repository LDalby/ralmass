#' Calculate the distance from fields to goose roosts
#'
#' Calculate the distance from fields to goose roosts (in this case)
#' Using the centroids from the polyref file distances can be calculated 
#' outside of ALMaSS. 
#' 
#' @param roost data.table The GooseRoosts.txt file
#' @param fields data.table If fieldobs = FALSE then the GooseFieldForageData.txt
#'  file otherwise a data.table with field observations converted to ALMaSS 
#' coordinates using the UtmToAlmass function.
#' @param fieldobs logical If TRUE then input is fieldobservations otherwise
#' output from ALMaSS simulations
#' @param polyref data.table The object with the polyref file (only if
#' fieldobs = FALSE)
#' @param species character A vector of species names for which the distances should be 
#' calculated for. Either 'Barnacle', 'Pinkfoot' or 'Greylag'
#' @return data.table A data.table with the polyrefnumber or if fieldobs then
#'  the PolyID (renamed to PolyRefNum), distances for each roost and a column with
#'  the shortest distance.
#' @export
CalcDistToRoosts = function(roost = NULL, fields = NULL, fieldobs = NULL, polyref = NULL, species = NULL) {
	if(any(is.null(roost),is.null(fields),is.null(fieldobs),is.null(species))){
		stop('Required input argument missing')
	}
	if(!fieldobs && is.null(polyref)) {
		stop('Provide polyref file when fieldobs = FALSE')
	}
	if(!data.table::is.data.table(roost)) 
	{
		roost = data.table::as.data.table(roost)
	}
	if(!data.table::is.data.table(fields)) 
	{
		fields = data.table::as.data.table(fields)
	}
	if(!data.table::is.data.table(polyref)) 
	{
		polyref = data.table::as.data.table(polyref)
	}
	setnames(roost, c('Type', 'CentroidX', 'CentroidY'))
	if(!all(roost[, Type] %in% species)) {
		roost[, Type:=sapply(Type, FUN = SwapType)]
	}
	if(fieldobs & any((names(fields) %in% c('ALong', 'ALat')))) {
			setnames(fields, old = c('ALong', 'ALat'), new = c('CentroidX', 'CentroidY'))
	}
	TheList = vector('list', length(species))
	for (i in seq_along(species)) {
		Roost = roost[Type == species[i],]  # Get the roost for the species
		if(fieldobs) {
			Fields = fields[Species == species[i], .(CentroidX, CentroidY)]
		}
		if(!fieldobs) {
			spobs = eval(as.name(species[i]), fields)
			ForagePolys = unique(fields[spobs != 0,Polyref])
			Fields = polyref[PolyRefNum %in% ForagePolys, .(CentroidX, CentroidY)]
			DT = polyref[PolyRefNum %in% ForagePolys, .(PolyRefNum)]
		}
		for(j in 1:nrow(Roost))
		{
			TheDistances = dist(rbind(Roost[j,.(CentroidX, CentroidY)], Fields))[1:nrow(Fields)]
			newcolname = paste0('Roost', j)
			DT[,(newcolname):=TheDistances]
		}
		DT[,Shortest:=apply(DT[,2:ncol(DT)], FUN = min, MARGIN = 1)]
		DT[,Species:=species[i]]
		nullcols = grep('Roost', names(DT))
		DT[, (nullcols):=NULL]
		TheList[[i]] = DT
	}
	result = data.table::rbindlist(TheList)
	if(fieldobs) 
	{
		result[, Type:='Fieldobs']
	}
	if(!fieldobs) 
	{
		result[, Type:='Simulated']
	}
	return(tibble::as_tibble(result))
}

# Helper function
SwapType = function(Type) {
	switch(EXPR = as.character(Type),
		'0' = 'Pinkfoot',
		'1' = 'Barnacle',
		'2' = 'Greylag',
		'Undefined species')
}

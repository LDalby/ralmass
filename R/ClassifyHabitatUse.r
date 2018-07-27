#' Classify animal habitat use
#'
#' Classify animal habitat use from what ALMaSS uses into categories
#' matching the ones that are scored in the field
#'
#' @param data data.table A data.table with the habitat use from ALMaSS
#' @param species character String giving the species (or animal model)
#' @param timed logical Should we use timed counts (TRUE, default) or daily
#' maximum (FALSE)?
#' @return A data.table with the classifed habitat use.
#' @export
ClassifyHabitatUse = function(data, species = NULL, timed = TRUE) {
	if(is.null(species))
	{
		stop('Input parameter species missing')
	}
	if(tolower(species) != 'goose')
	{
		stop('Currently only implemented for geese')
	}
	data = data.table::as.data.table(data)
	if(tolower(species) == 'goose')
	{
		data[,c('HabitatUsePF', 'HabitatUseGL', 'HabitatUseBN'):='foo']
		if(!timed)
		{
	# Remove extra white spaces and make combined variable:
			data[, last_sown_veg:=stringr::str_trim(last_sown_veg, side = 'right')]
			data[, VegTypeCombo:=paste(last_sown_veg, veg_phase, sep = '-')]
			data[pinkfoot > 0, HabitatUsePF:=sapply(VegTypeCombo, ClassifyVegType)]
			data[greylag > 0, HabitatUseGL:=sapply(VegTypeCombo, ClassifyVegType)]
			data[barnacle > 0, HabitatUseBN:=sapply(VegTypeCombo, ClassifyVegType)]
		# Fix for a rare bug:
			data[pinkfoot > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUsePF:='Stubble']
			data[greylag > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUseGL:='Stubble']
			data[barnacle > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUseBN:='Stubble']
		# Fix natural grass types:
			data[pinkfoot > 0 & VegTypeChr == 'NaturalGrass', HabitatUsePF:='Grass']
			data[greylag > 0 & VegTypeChr == 'NaturalGrass', HabitatUseGL:='Grass']
			data[barnacle > 0 & VegTypeChr == 'NaturalGrass', HabitatUseBN:='Grass']
		# Fix undersown spring barley - CGG1:
			data[pinkfoot > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[greylag > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[barnacle > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
			data[pinkfoot > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[greylag > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[barnacle > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
		# SG1
			data[pinkfoot > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUsePF:='Stubble']
			data[greylag > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUseGL:='Stubble']
			data[barnacle > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUseBN:='Stubble']
			data[pinkfoot > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUsePF:='Stubble']
			data[greylag > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUseGL:='Stubble']
			data[barnacle > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUseBN:='Stubble']
		# Spring barley that is not undersown
			data[pinkfoot > 0 & previous_crop == 'SpringBarley' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[greylag > 0 & previous_crop == 'SpringBarley' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[barnacle > 0 & previous_crop == 'SpringBarley' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
		}
		if(timed)
		{
		# Remove extra white spaces and make combined variable:
			data[, last_sown_veg:=stringr::str_trim(last_sown_veg, side = 'right')]
			data[, VegTypeCombo:=paste(last_sown_veg, veg_phase, sep = '-')]
			data[pinkfoot_timed > 0, HabitatUsePF:=sapply(VegTypeCombo, ClassifyVegType)]
			data[greylag_timed > 0, HabitatUseGL:=sapply(VegTypeCombo, ClassifyVegType)]
			data[barnacle_timed > 0, HabitatUseBN:=sapply(VegTypeCombo, ClassifyVegType)]
		# Fix for a rare bug:
			data[pinkfoot_timed > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUsePF:='Stubble']
			data[greylag_timed > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUseGL:='Stubble']
			data[barnacle_timed > 0 & VegTypeCombo == 'OFieldPeas-3', HabitatUseBN:='Stubble']
		# Fix natural grass types:
			data[pinkfoot_timed > 0 & VegTypeChr == 'NaturalGrass', HabitatUsePF:='Grass']
			data[greylag_timed > 0 & VegTypeChr == 'NaturalGrass', HabitatUseGL:='Grass']
			data[barnacle_timed > 0 & VegTypeChr == 'NaturalGrass', HabitatUseBN:='Grass']
		# Fix undersown spring barley - CGG1:
			data[pinkfoot_timed > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[greylag_timed > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[barnacle_timed > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
			data[pinkfoot_timed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[greylag_timed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[barnacle_timed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
		# SG1
			data[pinkfoot_timed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUsePF:='Stubble']
			data[greylag_timed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUseGL:='Stubble']
			data[barnacle_timed > 0 & VegTypeChr == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUseBN:='Stubble']
			data[pinkfoot_timed > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUsePF:='Stubble']
			data[greylag_timed > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUseGL:='Stubble']
			data[barnacle_timed > 0 & previous_crop == 'SprBarleyCloverGrass' &
			last_sown_veg == 'SeedGrass1', HabitatUseBN:='Stubble']
		# Spring barley that is not undersown
			data[pinkfoot_timed > 0 & previous_crop == 'SpringBarley' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUsePF:='Stubble']
			data[greylag_timed > 0 & previous_crop == 'SpringBarley' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseGL:='Stubble']
			data[barnacle_timed > 0 & previous_crop == 'SpringBarley' &
			last_sown_veg == 'CloverGrassGrazed1', HabitatUseBN:='Stubble']
		}
	data[HabitatUseBN == 'foo', HabitatUseBN:=NA]
	data[HabitatUsePF == 'foo', HabitatUsePF:=NA]
	data[HabitatUseGL == 'foo', HabitatUseGL:=NA]
	return(tibble::as_tibble(data))
	}
}

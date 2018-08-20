#' Convert ALMaSS config parameter to a meaningful character string
#'
#' Convert ALMaSS config parameter to a meaningful character string. This
#' can be useful when doing plots and generating labels and titles
#' automatically.
#'
#' @param param character The input parameter
#' @return character String with more meaningful explanation
#' @export
ConvertParam <- function(param = NULL) {
	if(is.null(param)) {stop("Input argument missing \n")}
	param = stringr::str_trim(param, side = "both")
	param = stringr::word(param)
	switch(EXPR = param,
  	# Names of the paramters:
		"GOOSE_MINFORAGEOPENNESS" = "Minimum openness",
		"GOOSE_AFTERDARKTIME" = "Past sunset active time",
		"GOOSE_FOLLOWINGLIKELYHOOD_BN" = "Following likelyhood - Barnacle",
		"GOOSE_FOLLOWINGLIKELYHOOD_PF" = "Following likelyhood - Pinkfoot",
		"GOOSE_FOLLOWINGLIKELYHOOD_GL" = "Following likelyhood - Greylag",
		"GOOSE_MAXAPPETITESCALER" = "Max appetite scaler",
		"GOOSE_MAXENERGYRESERVEPROPORTION" = "Max energy reserve proportion",
		"GOOSE_INITIALENERGYRESERVEPROPORTION" = "Initial energy reserve proportion",
		"GOOSE_LEAVINGTHRESHOLD" = "Leaving threshold",
		"GOOSE_FORAGEDIST_BN" = "Max foraging distance from roost - Barnacle",
		"GOOSE_FORAGEDIST_PF" = "Max foraging distance from roost- Pinkfoot",
		"GOOSE_FORAGEDIST_GL" = "Max foraging distance from roost - Greylag",
		"GOOSE_MINFORAGEDECAYRATE" = "Min accepted forage rate decay",
		"GOOSE_FEEDINGTIME" = "Feeding time",
		"GOOSE_ROOSTLEAVINGLIKELYHOOD" = "Roost leaving likelyhood",
		"GOOSE_MEM_MINMEMVALUE" = "Memory duration",
		"GOOSE_GRAINDECAYRATE" = "Grain decay rate",
		"GOOSE_GRAINDECAYRATEWINTER" = "Grain decay rate winter",
		"GOOSE_GRAINDECAYRATESPRING" = "Grain decay rate spring",
		"GOOSE_ROOSTLEAVEDISTSD" = "SD of roost leave time distribution",
		"GOOSE_ROOSTLEAVEDISTMEAN" = "Mean of roost leave time distribution",
		"GOOSE_MEM_EXPECTEDFORAGINGTIME" = "Expected foraging time",
		"GOOSE_FLIGHTCOST_PF" = "Flight cost - Pinkfoot",
		"GOOSE_FLIGHTCOST_BN" = "Flight cost - Barnacle",
		"GOOSE_FLIGHTCOST_GL" = "Flight cost - Greylag",
		"GOOSE_FLIGHTCOST" = "Flight cost",
		"GOOSE_BARNACLEWEIGHT" = "Base weight - Barnacle",
		"GOOSE_PINKFOOTWEIGHT" = "Base weight - Pinkfoot",
		"GOOSE_GREYLAGWEIGHT" = "Base weight - Greylag",
		"GOOSE_BMRCONSTANTA" = "BMR constant A",
		"GOOSE_BMRCONSTANTB" = "BMR constant B",
		"GOOSE_BN_YOUNG_PROPORTION" = "Proportion of young - Barnacle",
		"GOOSE_PF_YOUNG_PROPORTION" = "Proportion of young - Pinkfoot",
		"GOOSE_GL_YOUNG_PROPORTION" = "Proportion of young - Greylag",
		"GOOSE_ENERGYCONTENTOFFAT" = "Energy content of fat",
		"GOOSE_FIELDFORAGEDIST_BN" = "Max foraging distance from forage location - Barnacle",
		"GOOSE_FIELDFORAGEDIST_PF" = "Max foraging distance from forage location - Pinkfoot",
		"GOOSE_FIELDFORAGEDIST_GL" = "Max foraging distance from forage location - Greylag",
		"GOOSE_METABOLICCONVCOSTS" = "Metabolic conversion cost",
		"GOOSE_THERMALCONSTANTA_BN" = "Lower critical temperature - Barnacle",
		"GOOSE_THERMALCONSTANTA_PF" = "Lower critical temperature - Pinkfoot",
		"GOOSE_THERMALCONSTANTA_GL" = "Lower critical temperature - Greylag",
		"GOOSE_TIMEDCOUNTS" = "Hour for timed counts (from sunrise)",
		"GOOSE_DAYTIMEBMRMULTIPLIER" = "Multiple of BMR spent during daytime (DEB calculations)",
		"GOOSE_NIGHTTIMEBMRMULTIPLIER" = "Multiple of BMR spent during nighttime (DEB calculations)",
		"GOOSE_THERMALCONSTANTB" = "Thermal regulation cost constant B",
		"GOOSE_ROOSTCHANGECHANCE_PF" = "Chance of changing roost - Pinkfoot",
		"GOOSE_ROOSTCHANGECHANCE_GL" = "Chance of changing roost - Greylag",
		"GOOSE_ROOSTCHANGECHANCE_BN" = "Chance of changing roost - Barnacle",
		"GOOSE_FLIGHTCOST" = "Flight cost",
		"GOOSE_DIST_WEIGHTING_POWER" = "Decay constant. Probability as a function of distance",
		"GOOSE_ROOSTCHANGECHANCE" = "Chance of changing roost",
		"GOOSE_SNOW_SCALER" = "Intake rate decrease constant if snow cover",
		"GOOSE_GRASS_TO_WINTER_CEREAL_SCALER" = "Scaler for energy content of winter cereal compared to grass",
		"PETTIFOR_G_BN" = "Cut-off on density functional response - Barnacle"
		)
}

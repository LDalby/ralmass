#' Add type to input parameter string
#'
#' Add the input parameter type to the string of just the parameter name.
#' 
#' @param param character The input parameter
#' @return character param plus the type added in parantheses
#' @export
GetParamType <- function(param = NULL) {
	if(is.null(param)) {stop('Input argument missing \n')}
    param = stringr::str_trim(param, side = "both")
    switch(EXPR = param,
  	# Names of the paramters:
  		'GOOSE_MINFORAGEOPENNESS' = 'GOOSE_MINFORAGEOPENNESS (float)',
  		'GOOSE_AFTERDARKTIME' = 'GOOSE_AFTERDARKTIME (int)',
        'BGOOSE_FOLLOWINGLIKELYHOOD' = 'BGOOSE_FOLLOWINGLIKELYHOOD (int)',
		'PFGOOSE_FOLLOWINGLIKELYHOOD' = 'PFGOOSE_FOLLOWINGLIKELYHOOD (int)',
		'GLGOOSE_FOLLOWINGLIKELYHOOD' = 'GLGOOSE_FOLLOWINGLIKELYHOOD (int)',
		'GOOSE_MAXAPPETITESCALER' = 'GOOSE_MAXAPPETITESCALER (float)',
		'GOOSE_MAXENERGYRESERVEPROPORTION' = 'GOOSE_MAXENERGYRESERVEPROPORTION (float)',
		'GOOSE_LEAVINGTHRESHOLD' = 'GOOSE_LEAVINGTHRESHOLD (float)',
		'GOOSE_FORAGEDIST_BN' = 'GOOSE_FORAGEDIST_BN (float)',
		'GOOSE_FORAGEDIST_PF' = 'GOOSE_FORAGEDIST_PF (float)',
		'GOOSE_FORAGEDIST_GL' = 'GOOSE_FORAGEDIST_GL (float)',
		'GOOSE_MINFORAGEDECAYRATE' = 'GOOSE_MINFORAGEDECAYRATE (float)',
		'GOOSE_ENERGYCALIBRATION' = 'GOOSE_ENERGYCALIBRATION (float)',
		'GOOSE_FEEDINGTIME' = 'GOOSE_FEEDINGTIME (float)',
		'GOOSE_ROOSTLEAVINGLIKELYHOOD' = 'GOOSE_ROOSTLEAVINGLIKELYHOOD (int)',
		'GOOSE_MEM_DISTPENALTY' = 'GOOSE_MEM_DISTPENALTY (float)',
		'GOOSE_MEM_MINMEMVALUE' = 'GOOSE_MEM_MINMEMVALUE (int)',
		'GOOSE_GRAINDECAYRATE' = 'GOOSE_GRAINDECAYRATE (float)',
		'HUNTERS_MAXDENSITY' = 'HUNTERS_MAXDENSITY (float)',
        'CLOSESTFARMPROBPARAMONE' = 'CLOSESTFARMPROBPARAMONE (float)',
        'HUNTERS_DISTRIBUTE_RULESET' = 'HUNTERS_DISTRIBUTE_RULESET (int)',
        'GOOSE_ROOSTLEAVEDISTSD' = 'GOOSE_ROOSTLEAVEDISTSD (int)',
        'GOOSE_MEM_EXPECTEDFORAGINGTIME' = 'GOOSE_MEM_EXPECTEDFORAGINGTIME (int)',
        'GOOSE_MODELEXITDAY' = 'GOOSE_MODELEXITDAY (int)',
        'MAP_WEATHER_FILE' = 'MAP_WEATHER_FILE (string)',
        'FARMSIZEPROBPARAMONE' = 'FARMSIZEPROBPARAMONE (float)',
        'ROOSTDISTPROBPARAMONE' = 'ROOSTDISTPROBPARAMONE (float)',
        'HUNTERS_DISTRIBUTE' = 'HUNTERS_DISTRIBUTE (bool)',
        'HUNTERS_RECORDBAG' = 'HUNTERS_RECORDBAG (bool)',
        'GOOSE_BN_FAMILIES_STARTNOS' = 'GOOSE_BN_FAMILIES_STARTNOS (int)',
        'GOOSE_BN_NONBREEDERS_STARTNOS' = 'GOOSE_BN_NONBREEDERS_STARTNOS (int)',
        'GOOSE_PF_FAMILIES_STARTNOS' = 'GOOSE_PF_FAMILIES_STARTNOS (int)',
        'GOOSE_PF_NONBREEDERS_STARTNOS' = 'GOOSE_PF_NONBREEDERS_STARTNOS (int)',
        'GOOSE_GL_FAMILIES_STARTNOS' = 'GOOSE_GL_FAMILIES_STARTNOS (int)',
        'GOOSE_GL_NONBREEDERS_STARTNOS' = 'GOOSE_GL_NONBREEDERS_STARTNOS (int)',
        'GOOSE_OPENSEASON_END' = 'GOOSE_OPENSEASON_END (int)',
        'HUNTER_REFRACTIONPERIOD' = 'HUNTER_REFRACTIONPERIOD (int)',
        'HUNTER_MAGAZINECAPACITY' = 'HUNTER_MAGAZINECAPACITY (int)',
        # These are a special case for the hunter model and are supposed to
        # stay unchanged. This a hack so GenerateParams still works.
        'HuntingDays' = 'HuntingDays',
        'WeekdayHunterChance' = 'WeekdayHunterChance',
        'GooseLookChance' = 'GooseLookChance',
        'Efficiency' = 'Efficiency',
        # Default:
        'MissingParameter'
        )
}

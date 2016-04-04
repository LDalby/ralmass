#' Add type to input parameter string
#'
#' Add the input parameter type to the string of just the parameter name.
#' 
#'  
#' @param param character The input paramter
#' @return character param plus the type added in parantheses
#' @export
GetParamType <- function(param = NULL) {
	if(is.null(param)) {stop('Input argument missing \n')}
    param = str_trim(param, side = "both")
    switch(EXPR = param,
  	# Names of the paramters:
  		'GOOSE_MINFORAGEOPENNESS' = 'GOOSE_MINFORAGEOPENNESS (float)',
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
        'CLOSESTFARMPROBPARAMONE' = 'CLOSESTFARMPROBPARAMONE (float)'
        )
}

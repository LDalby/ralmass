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
	if(is.null(param)) {stop('Input argument missing \n')}
    param = stringr::str_trim(param, side = "both")
    switch(EXPR = param,
  	# Names of the paramters:
  		'GOOSE_MINFORAGEOPENNESS (float)' = 'Minimum openness',
  		'GOOSE_MINFORAGEOPENNESS' = 'Minimum openness',
        'BGOOSE_FOLLOWINGLIKELYHOOD (int)' = 'Following likelyhood',
        'BGOOSE_FOLLOWINGLIKELYHOOD' = 'Following likelyhood',
		'PFGOOSE_FOLLOWINGLIKELYHOOD (int)' = 'Following likelyhood',
		'PFGOOSE_FOLLOWINGLIKELYHOOD' = 'Following likelyhood',
		'GLGOOSE_FOLLOWINGLIKELYHOOD (int)' = 'Following likelyhood',
		'GLGOOSE_FOLLOWINGLIKELYHOOD' = 'Following likelyhood',
		'GOOSE_MAXAPPETITESCALER (float)' = 'Max appetite scaler',
		'GOOSE_MAXAPPETITESCALER' = 'Max appetite scaler',
		'GOOSE_MAXENERGYRESERVEPROPORTION (float)' = 'Max energy reserve proportion',
		'GOOSE_MAXENERGYRESERVEPROPORTION' = 'Max energy reserve proportion',
		'GOOSE_LEAVINGTHRESHOLD (float)' = 'Leaving threshold',
		'GOOSE_LEAVINGTHRESHOLD' = 'Leaving threshold',
		'GOOSE_FORAGEDIST_BN (float)' = 'Max foraging distance',
		'GOOSE_FORAGEDIST_BN' = 'Max foraging distance',
		'GOOSE_FORAGEDIST_PF (float)' = 'Max foraging distance',
		'GOOSE_FORAGEDIST_PF' = 'Max foraging distance',
		'GOOSE_FORAGEDIST_GL (float)' = 'Max foraging distance',
		'GOOSE_FORAGEDIST_GL' = 'Max foraging distance',
		'GOOSE_MINFORAGEDECAYRATE (float)' = 'Min accepted forage rate decay',
		'GOOSE_MINFORAGEDECAYRATE' = 'Min accepted forage rate decay',
		'GOOSE_FEEDINGTIME (float)' = 'Feeding time',
		'GOOSE_FEEDINGTIME' = 'Feeding time',
		'GOOSE_ROOSTLEAVINGLIKELYHOOD (int)' = 'Roost leaving likelyhood',
		'GOOSE_ROOSTLEAVINGLIKELYHOOD' = 'Roost leaving likelyhood',
		'GOOSE_MEM_MINMEMVALUE (int)' = 'Memory duration',
		'GOOSE_MEM_MINMEMVALUE' = 'Memory duration',
		'GOOSE_GRAINDECAYRATE (float)' = 'Grain decay rate',
		'GOOSE_GRAINDECAYRATE' = 'Grain decay rate',
        'GOOSE_MEM_EXPECTEDFORAGINGTIME (int)' = 'Expected foraging time',
        'GOOSE_MEM_EXPECTEDFORAGINGTIME' = 'Expected foraging time'
        )
}

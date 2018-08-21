#' Get the value of a config variable
#'
#' Return the value of a config variable in a specified ALMaSS config file.
#'
#' @param config character Path to the file to read start and leave date from
#' @param param character The name of the config variables
#' @return The value of the config parameter
#' @export
	GetParamValue = function(param, config = NULL) {
		if(missing(param)){
			stop("Input parameter param missing")
		}
		if(!is.null(config))
		{
			param <- config[grep(param, config)]
			param <- param[length(param)]  # Get the last mention of param
		}
	valuestring <- stringr::str_split(param[1], '=')
	value <- as.numeric(stringr::str_trim(valuestring[[1]][2]))
	if(is.na(value)) stop("Config variable not found")
	return(value)
}

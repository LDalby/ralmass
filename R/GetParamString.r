#' Get the string of a config variable
#'
#' Return the character string of a config variable in a specified
#'  ALMaSS config file.
#' 
#' @param config character String from which to extract the config parameter
#' string.
#' @return The character string of the config parameter
#' @export 
GetParamString = function(config = NULL) {
	if(is.null(config)){
		stop('Input parameter config missing')
	}
	if(length(grep('\\(', config)) > 0) 
	{
		valuestring = stringr::str_split(config[1], '\\(')
	}
	if(length(grep('\\(', config)) == 0) 
	{
		valuestring = stringr::str_split(config[1], '=')
	} 
	value = stringr::str_trim(valuestring[[1]][1])
	return(value)
}

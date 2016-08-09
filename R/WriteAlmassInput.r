#' Write input file to ALMaSS
#'
#' Write an input file ready for loading in ALMaSS. This has the format of line
#' 1 being the number of rows, line 2 the headers (if needed) and then the 
#' actual content of the colummns. The file is a tab delimeted txt file.
#' 
#' A few of the older ALMaSS input files do not contain headers. This can be 
#' switched on or of using the headers argument.
#'
#' @param table data.table The table with the input
#' @param pathtofile character The path to the where the file is 
#' written (including the name and extension of the file, usually .txt)
#' @param headers logical Should the output file include headers?
#' @return A tab separated text file formatted as an ALMaSS input file.
#' @export
WriteAlmassIput = function(table = NULL, pathtofile = NULL, headers = TRUE){
	if(any(is.null(table), is.null(pathtofile))) 
	{
		stop('Input parameter missing \n')
	}
	filecon = file(pathtofile, open = 'wt')
	cat(paste(nrow(table)), '\n',  file = filecon)
	ScipenDefault = getOption('scipen')
	options(scipen = 99)  # To avoid scientific notation in the resulting file
	utils::write.table(table, file = filecon, sep = '\t', append = TRUE, 
		row.names = FALSE, col.names = headers, quote = FALSE)
	close(filecon)
	options(scipen = ScipenDefault)  # Reset scipen option to default
}
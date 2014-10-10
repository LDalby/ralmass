#' Write polyref file
#'
#' Write a polyref file to a text file ready for loading in ALMaSS.
#'
#' @param Table data.frame An attribute table
#' @param PathToFile The path to the where the file is written (including the name and extension of the file, usually .txt)
#' @return A tab separated text file formatted according to the requirements for a polygon reference file for ALMaSS.
#' @export

WritePolyref = function(Table, PathToFile){
	filecon = file(PathToFile, open = 'wt')
	cat(paste(nrow(Table)), '\n',  file = filecon)
	write.table(Table, file = filecon, sep = '\t', append = TRUE, row.names = FALSE, col.names = FALSE, quote = FALSE)
	close(filecon)
}
#' Append work directory paths
#'
#' Append work directory paths to already excisting scripts
#'
#' @param wd character Path to the work directory with the ALMaSS program
#' @param in_script character Path to the base script into which the WD is appended
#' @param out_script character File name for the modified base script. This will
#' usually be the same name as the base script.
#' @export

append_wd <- function(wd = NULL, in_script = NULL, out_script = NULL) {
	if(missing(wd)) {
	  stop("argument wd is missing")
	}

  if(missing(in_script)) {
    stop("argument in_script is missing")
  }

  if(missing(out_script)) {
    stop("argument out_script is missing")
  }

  script <- readLines(in_script)
	output_script <- fs::path(wd, out_script)
	the_wd <- paste0('setwd(','\'', wd, '\'', ')')
	script <- append(script, the_wd, grep("# Setup work directory", script))
	filecon <- file(output_script, open = "wt")
	write(script, file = filecon)
	close(filecon)
}

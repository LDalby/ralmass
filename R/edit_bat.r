#' Edit bat file or shell script
#'
#' Edit a shell script to reflect the correct number of runs
#' in a scenario. This function requires the ParameterValues.txt file
#' to be present in work_dir. The function reads the number of runs
#' from that file.
#'
#' @param work_dir character Path to the work directory with the ALMaSS exe file
#' @param sh_file character File name for the shell file doing the loop
#' @export
edit_bat = function(work_dir, sh_file) {
	if(missing(work_dir))
	{
		stop("Input parameter work_dir is missing")
	}
  if(missing(sh_file))
  {
    stop("Input parameter sh_file is missing")
  }
	# Get the number of runs:
	param_file <- fs::path(work_dir, "ParameterValues.txt")
	if(!fs::file_exists(param_file)) {
		stop(glue::glue("ParameterValues.txt missing from {work_dir}"))
	}
	param_vals <- data.table::fread(param_file)
	n_params <- length(param_vals[, unique(V1)])
	runs <- nrow(param_vals)/n_params

	sh_file_path <- fs::path(work_dir, sh_file)
	if(!fs::file_exists(sh_file_path)) {
	  stop(glue::glue("{sh_file_path} missing"))
	}
	sh <- readLines(sh_file_path)
	if(length(sh) == 0) {
		stop(glue::glue("Error reading {sh_file_path}"))
	}

  for_line <- glue::glue("for i in {{01..{runs}}")
  index <- grep("for i in ", sh)
  if(length(index) == 0) {
    stop(glue::glue("Cannot find the for loop in {sh_file}"))
  }
	sh[index] <- for_line
	filecon <- file(sh_file_path, open = "wt")
	write(sh, file = filecon)
	close(filecon)
}

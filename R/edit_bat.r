#' Edit bat file or shell script
#'
#' Edit a bat file or shell script to reflect the correct number of runs
#' in a scenario. This function requires the ParameterValues.txt file
#' to be present in work_dir. The function reads the number of runs
#' from that file.
#'
#' @param work_dir character Path to the work directory with the ALMaSS exe file
#' @param bat_file character File name for the shell file doing the loop
#' @export
edit_bat = function(work_dir, bat_file) {
	if(missing(work_dir))
	{
		stop("Input parameter work_dir is missing")
	}
  if(missing(bat_file))
  {
    stop("Input parameter bat_file is missing")
  }
	# Get the number of runs:
	param_file <- fs::path(work_dir, "ParameterValues.txt")
	if(!fs::file_exists(param_file)) {
		stop(glue::glue("ParameterValues.txt missing from {work_dir}"))
	}
	paramvals <- data.table::fread(param_file)
	n_params <- length(paramvals[, unique(V1)])
	runs <- nrow(paramvals)/n_params

	bat_path <- fs::path(work_dir, bat_file)
	if(!fs::file_exists(bat_path)) {
	  stop(glue::glue("{bat_path} missing"))
	}
	bat <- readLines(bat_file)
	if(length(bat) == 0) {
		stop("Error reading bat file")
	}

 for_line <- glue::glue("for i in {{01..{runs}}")
 index <- grep("for i in ", bat)
 if(length(index) == 0) {
    stop("Cannot find the for loop in the bat file")
  }
	bat[index] <- for_line
	filecon <- file(bat_path, open = "wt")
	write(bat, file = filecon)
	close(filecon)
}

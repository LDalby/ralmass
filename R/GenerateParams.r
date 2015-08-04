#' Generate a range combinations of paramter values
#'
#' Generate all possible combinations of values in the input vectors and 
#' (optionally) write them to a file. Typically used when making scenarios for 
#' fitting parameters.
#' 
#' This function is a modification of expand.grid from base.
#' 
#' @param ... numeric One or more vectors (usually made with seq and the
#' length.out argument)
#' @param write logical Should a file with the table be written to disk?
#' @param splits numeric The number of files to split the scenarios into. 
#' (Optional). If not NULL the files gets written to disk.
#' @return data.frame One column data.frame where each line is a text string
#' with the config and and its value
#' @export
#' @examples
#' val = seq(1, 5, length.out = 5)
#' val2 = seq(10, 50, length.out = 5)
#' GenerateParams('FOO (int)' = val, 'BAR (int)' = val2, write = FALSE)
GenerateParams = function (..., write = FALSE, splits = NULL) 
{
	# Code from the base expand.grid with the KEEP.ATTR.OUT and StringAsFactor
	# bits removed.
	nargs <- length(args <- list(...))
	if (!nargs) 
		return(as.data.frame(list()))
	if (nargs == 1L && is.list(a1 <- args[[1L]])) 
		nargs <- length(args <- a1)
	if (nargs == 0L) 
		return(as.data.frame(list()))
	cargs <- vector("list", nargs)
	iArgs <- seq_len(nargs)
	nmc <- paste0("Var", iArgs)
	nm <- names(args)
	if (is.null(nm)) 
		nm <- nmc
	else if (any(ng0 <- nzchar(nm))) 
		nmc[ng0] <- nm[ng0]
	names(cargs) <- nmc
	rep.fac <- 1L
	d <- lengths(args)

	orep <- prod(d)
	if (orep == 0L) {
		for (i in iArgs) cargs[[i]] <- args[[i]][FALSE]
	}
	else {
		for (i in iArgs) {
			x <- args[[i]]
			nx <- length(x)
			orep <- orep/nx
			x <- x[rep.int(rep.int(seq_len(nx), rep.int(rep.fac, nx)), orep)]
			cargs[[i]] <- x
			rep.fac <- rep.fac * nx
		}
	}

	rn <- .set_row_names(as.integer(prod(d)))
    # The ralmass modification
	exgrid = structure(cargs, class = "data.frame", row.names = rn)
	values = as.vector(t(exgrid))
	confignames = rep(names(exgrid), nrow(exgrid))
	df = data.frame('Params' = paste(confignames, values, sep = ' = '))
	if(!is.null(splits)) 
	{
		starts = seq(1, nrow(df), by = nrow(df)/splits)
		ends = seq(nrow(df)/splits, nrow(df), by = nrow(df)/splits)
		for (i in 1:splits) 
		{
			temp = df[starts[i]:ends[i],]
			fname = paste0('ParameterValues', i, '.txt')
			write.table(temp, file = fname, sep = '\t', quote = FALSE,
			row.names = FALSE, col.names = FALSE)
		}
		cat('Wrote', splits, 'versions of ParameterValues.txt to',
			getwd(), '\n')
		cat('The full list of scenarios is printed here:\n')
	}
	if(write) 
	{
		write.table(df, file = 'ParameterValues.txt', sep = '\t', quote = FALSE,
			row.names = FALSE, col.names = FALSE)
		cat('Printed following to ParameterValues.txt:\n')
	}
	return(df)
}



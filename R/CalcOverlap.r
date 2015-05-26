#' Calculate the overlap of kernel densities of flock sizes
#'
#' Calculate the overlap of kernel densities of flock sizes calculated from
#' field observations and from the simulated flock sizes
#' 
#' 
#' @param data data.table An object containing both the simulated and observed 
#' flock sizes
#' @param species character The species for which the calculated for.
#' Either 'Barnacle', 'Pinkfoot' or 'Greylag'
#' @return numeric The overlap
#' @export

CalcOverlab = function(data, species = NULL) 
{
	if(is.null(data)) {stop('data argument empty \n')}
	if(is.null(species)) {stop('species argument empty \n')}
    # The capwords function from the examples in ?tolower
	capwords = function(s, strict = FALSE) {
		cap = function(s) paste(toupper(substring(s, 1, 1)),
			{s <- substring(s, 2); if(strict) tolower(s) else s},
			sep = "", collapse = " " )
		sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
	}

	species = capwords(species)
	data = data[Species == species,][, Numbers:=log10(Numbers)]

  # Set limits of a common grid, ensuring that tails aren't cut off
	min = min(data[, Numbers]) - 1
	max = max(data[, Numbers]) + 1

	simdens = density(data[Type == 'Simulated', Numbers], from=min, to=max)
	obsdens = density(data[Type == 'Fieldobs', Numbers], from=min, to=max)
	d = data.frame(x=simdens$x, a=simdens$y, b=obsdens$y)

  # calculate intersection densities
	d$w = pmin(d$a, d$b)

  # Integrate the area under the curves
	total = integrate.xy(d$x, d$a) + integrate.xy(d$x, d$b)
	intersection = integrate.xy(d$x, d$w)

	return(2 * intersection / total)
}


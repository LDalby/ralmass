#' get_func_resp
#' Return value from functional reponse
#'
#' The geese have different functional responses to grain density.
#' This function return the repsonse given a grain density.
#'
#' @param dens double The grain density
#' @param resp data.table The curve points written from ALMaSS
#' @param species character Either Pinkfoot, Barnacle or Greylag
#'
#' @return double The response.
#' @export
get_func_resp <- function(dens = NULL, resp = NULL, species = NULL) {
  if (any(is.null(dens), is.null(resp), is.null(species))) {
    stop("Input parameter missing")
  }
  # Make sure we have a keyed data.table with column names x 6 y:
  stopifnot(species %in% c("Pinkfoot", "Greylag", "Barnacle"))
  stopifnot(data.table::is.data.table(resp), data.table::haskey(resp))
  stopifnot(all.equal(names(resp), c("x", "y")))
  # Okay, let's calculate the response:
  the_y <- resp[J(dens), roll = "nearest"][,y]
  grain <- the_y * 17.67 * 0.695 * 60
  if (species == "Greylag") {
    grain <- grain * 1.16  # up scale for greylags
  }
  if (species == "Barnacle") {
    grain <- grain * 0.79  # down scale for barnies
  }
  return(grain)
}

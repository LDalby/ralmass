#' Tidy the standard AOR output
#'
#'The standard AOR output has a format thats not easy
#'to program with. This function transforms it into
#'a tidy format.
#' @param aor tibble The AOR table from ALMaSS
#' @param species character Pinkfoot, Barnacle or Greylag
#'
#' @return a tibble
#' @export
#'
tidy_aor <- function(aor, species = NULL) {
  if (is.null(species)) {
    stop("Input parameter species is missing")
  }
  n_years <- length(aor$Year)
  tibble::tibble(year = rep(aor$Year, 4),
                 day = rep(aor$Day, 4),
                 total_no = rep(aor$Total_no, 4),
                 dim = rep(c(50, 100, 200, 400), each = n_years),
                 prop_occupied = c(aor$Occupied50/aor$Cells50,
                                   aor$Occupied100/aor$Cells100,
                                   aor$Occupied200/aor$Cells200,
                                   aor$Occupied400/aor$Cells400),
                 species = rep(species, n_years*4))
}

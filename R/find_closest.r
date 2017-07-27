#' find_closest finds the nearest roost
#'
#'The find_closest function returns the distance to the nearst roost for
#'each species. Designed to be used with purr::pmap
#'
#' @param x int The x-coordinate of the forage location
#' @param y int The y-coordinate of the forage location
#' @param species character The species name
#'
#' @return double The distance to the nearest roost in meters
#' @export
#'
find_closest <- function(x, y, species) {
  if (species %in% c("Pinkfoot", "Barnacle")) {
    x1 <- c(9954, 21167)
    y1 <- c(16132, 12947)
  }

  if (species == "Greylag") {
    x1 <- c(9954, 21167, 17861, 17472)
    y1 <- c(16132, 12947, 9394, 6117)
  }

  X <- c(x, x1)
  Y <- c(y, y1)
  m <- matrix(data = c(X, Y), ncol = 2, byrow = FALSE) %>%
    dist() %>%
    as.matrix()

  m[2:nrow(m), 1] %>%
    min() %>%
    return()
}

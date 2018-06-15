#' Tidy the standard AOR output
#'
#'The standard AOR output has a format thats not easy
#'to program with. This function transforms it into
#'a tidy format.
#' @param data tibble The AOR table from ALMaSS
#'
#' @return a tibble
#' @export
#'
tidy_aor <- function(data) {
  if (missing(data)) {
    stop("No data supplied")
  }
  data %>%
    select(-starts_with("Cells")) %>%
    gather(key = cell_size,
           value = occupied,
           -Year, -Day, -Total_no) %>%
    mutate(cell_size = str_extract(cell_size, "[\\d]+")) -> occupied
  data %>%
    select(-starts_with("Occupied")) %>%
    gather(key = cell_size,
           value = cells,
           -Year, -Day, -Total_no) %>%
    mutate(cell_size = str_extract(cell_size, "[\\d]+")) -> cell_numbers
  left_join(occupied,
            cell_numbers,
            c("Year", "Day", "Total_no", "cell_size")) %>%
    rename(year = Year,
           day = Day,
           birds = Total_no) %>%
    mutate(prop_occupied = occupied/cells) -> tidy_aor_table

  col_names <- c("species", "year", "day", "birds", "cell_size", "occupied", "cells", "prop_occupied")
  if (!identical(names(tidy_aor_table), col_names)) {
    stop("names does not match expectation")
  }

  return(tidy_aor_table)
}

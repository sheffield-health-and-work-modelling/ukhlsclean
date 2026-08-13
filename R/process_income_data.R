#' Clean Unearned-Income File
#'
#' Perform basic cleaning on the income data file to process benefit income
#' data and reshape into a single row per individual.
#'
#' @param data.income Data table. Understanding Society income file.
#'
#' @export
process_income_data <- function(data.income = NULL) {

  ### retain identifiers, income source code, and amount per month, and amount
  ### per month imputed (to use as a flag for receipt)

  data_subset <- data.income[, c("pidp","ficode","frmnth_dv","frmnthimp_dv")]

  # recode 43-45 - Scottish equivalents of PIP. Combine with PIP in 41
  data_subset[ficode %in% 43:45, ficode := 41]

  # aggregate within person/ficode
  data_subset <- data_subset[, .(ben_income = sum(frmnth_dv)), by = c("pidp","ficode")]

  ### create a grid of pidp and all relevant ficodes
  grid <- expand.grid(pidp = unique(data_subset$pidp),
                      ficode = c(8:16, 18:23, 33, 40, 41) )
  setDT(grid)

  ### keep only relevant ficodes for the benefit income
  #data_subset <- data_subset[ficode %in% c(8:16, 18:23, 33, 40, 41), ]

  ### generate a flag to differentiate missingness from non-receipt once merged to the grid.
  data_subset[, ben_receipt := 1]

  ### merge the data onto the grid
  data_subset <- merge(grid, data_subset, by = c("pidp","ficode"), sort = F, all.x = TRUE, all.y = FALSE)

  ### reshape wide
  data_wide <- dcast(data_subset, pidp ~ ficode, value.var = list("ben_receipt","ben_income"))

  ### set benefit receipt values = 0 if missing
  cols <- paste0("ben_receipt_", c(8:16, 18:23, 33, 40, 41))
  data_wide[, (cols) := lapply(.SD, function(x) fifelse(is.na(x), 0, x)), .SDcols = cols]

  return(data_wide)
}

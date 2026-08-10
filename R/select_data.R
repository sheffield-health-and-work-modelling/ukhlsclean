#' Select variables and apply filters
#'
#' Selects the variables required for analysis and selects only the rows without missing data
#' for specified variables.
#'
#' @param data Data table - the cleaned Understanding Society dataset.
#' @param ages Integer vector - the ages in single years to retain (defaults to NULL - all ages).
#' @param country Character - country to produce data for. One of c("england","wales","scotland","northern_ireland"). Defaults to NULL which includes all UK.
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to NULL - keep all observations).
#' @param calendar_year Logical - TRUE when the code is processing calendar year data (defaults to FALSE).
#' @importFrom data.table :=
#' @return Returns a reduced version of data
#' @export
#'
#' @examples
#'
#' \dontrun{
#'
#'
#' }
#'
select_data <- function(
  data,
  ages = NULL,
  country = NULL,
  complete_vars = NULL,
  calendar_year = FALSE
) {

  ### apply age filter if specified

  if (!is.null(ages)) {

    data <- data[d_age %in% ages]

  }

  ### apply country filter if specified

  if(!is.null(country)){

  if (country %in% c("england","England")) {

    data <- data[d_country == "england",]
  }
  if (country %in% c("scotland","Scotland")) {

    data <- data[d_country == "scotland",]
  }
  if (country %in% c("wales","Wales")) {

    data <- data[d_country == "wales",]
  }
  if (country %in% c("northern_ireland","Northern_Ireland")) {

    data <- data[d_country == "northern_ireland",]
  }
}
  ## keep only complete cases of variables named in complete_vars

  for(cv in complete_vars) {

    data <- data[!is.na(get(cv))]

  }

  return(data)
}

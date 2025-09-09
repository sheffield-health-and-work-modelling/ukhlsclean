#' Clean gambling Variables
#'
#' Clean all gambling related variables.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_gambling <- function(data = NULL) {

  #cat(crayon::green("\tCleaning gambling variables\n"))

  #### wave 13 variables
  if("privbet" %in% colnames(data)) {

    gamble_vars      <- Hmisc::CS(m_privbet, m_onlbet, m_natlot, m_olott, m_scratchc, m_footpool, m_spreadbet, m_betex, m_onlgam, m_othgam)

    ## recode as dummies
    data[privbet   == 2, privbet := 0]
    data[onlbet    == 2, onlbet := 0]
    data[natlot    == 2, natlot := 0]
    data[olott     == 2, olott := 0]
    data[scatchc   == 2, scatchc := 0]
    data[footpool  == 2, footpool := 0]
    data[spreadbet == 2, spreadbet := 0]
    data[betex     == 2, betex := 0]
    data[onlgam    == 2, onlgam := 0]
    data[othgam    == 2, othgam := 0]


  } else {

    data[, privbet := NA]
    data[, onlbet := NA]
    data[, natlot := NA]
    data[, olott := NA]
    data[, scatchc := NA]
    data[, footpool := NA]
    data[, spreadbet := NA]
    data[, betex := NA]
    data[, onlgam := NA]
    data[, othgam := NA]

  }

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "id", "hidp", "wave_no",
                         "privbet", "onlbet", "spreadbet",
                         "natlot", "olott", "scatchc",
                         "footpool","betex","onlgam","othgam")]

  var_names <- c("privbet", "onlbet", "spreadbet",
                 "natlot", "olott", "scatchc",
                 "footpool","betex","onlgam","othgam")

  setnames(final_data, var_names, paste0("g_", var_names))

  return(final_data)

}

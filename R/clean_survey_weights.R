#' Clean Survey Weights
#'
#' Produce consistent variables containing cross-sectional weights for the purpose of
#' producing representative descriptive statistics by wave.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
clean_survey_weights <- function(data = NULL) {




  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "hidp", "wave_no")]

  return(final_data)
}

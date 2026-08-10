#' Clean Survey Weights
#'
#' Produce consistent variables containing cross-sectional weights for the purpose of
#' producing representative descriptive statistics by wave.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
clean_survey_weights <- function(data = NULL) {

  wave <- as.integer(unique(data[ , wave_no][1]))

  # =========================================================================
  # 1. SETUP & CONFIGURATION FLAGS
  # =========================================================================

  is_wave1      <- wave == 1      ## Main UK + ethnic minority boots
  is_wave2to5   <- wave %in% 2:5  ## Combined UK + BHPS sample
  is_wave6to13  <- wave %in% 6:13 ## Combined UK + BHPS + Immigrant Boost
  is_wave14plus <- wave >= 14     ## Combined sample + general population boost 2 (GPS2)

  # =========================================================================
  # 2. CROSS-SECTIONAL WEIGHTS - FOR WITHIN-WAVE REPRESENTATIVENESS
  # =========================================================================

  if (isTRUE(is_wave1)){

  data[, weight_xw := indinus_xw ]

  }
  if (isTRUE(is_wave2to5)){

    data[, weight_xw := indinub_xw ]

  }
  if (isTRUE(is_wave6to13)){

    data[, weight_xw := indinui_xw ]

  }
  if (isTRUE(is_wave14to15)){

    data[, weight_xw := inding2_xw ]

  }

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "hidp", "wave_no",
                         "weight_xw")]

  return(final_data)
}

#' Clean Smoking Variables
#'
#' Clean all variables related to smoking behaviour - current smoking status, smoking history, and
#' level of cigarette consumption for current smokers.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
clean_smoke <- function(data = NULL) {

  wave <- as.integer(unique(data[ , wave_no][1]))

  # =========================================================================
  # 1. SETUP & CONFIGURATION FLAGS
  # =========================================================================

  is_wave_2_5 <- wave %in% c(2,5)
  is_wave_6_14 <- wave %in% 6:14

  data <- data[order(pidp, wave_no),]

  # =========================================================================
  # 2. WAVES 2 & 5
  # =========================================================================

  ### ---------- SMOKING STATUS

  if (isTRUE(is_wave_2_5)){

  ## current smoker status
  data[smever == 1 & smnow == 1, current_smoker := "smoker"]
  data[smever == 1 & smnow == 2, current_smoker := "non_smoker"]
  data[smever == 2, current_smoker := "non_smoker"]

  ## ever smoked
  data[smever == 2, ever_smoked := "never_smoked"]
  data[smever == 1, ever_smoked := "smoked"]

  data[, ever_smoked  := as.factor(ever_smoked)]
  data[, current_smoker := as.factor(current_smoker)]

  } else {

  data[, ever_smoked := NA]
  }

  ### ---------- AGE STARTED SMOKING

  if (isTRUE(is_wave_2_5)){

  data[smagbg == 0, smagbg := NA]
  data[, smk_age_start := smagbg]

  } else {

  data[, smk_age_start := NA]
  }

  # =========================================================================
  # 3. WAVES 6 to 14 SMOKING STATUS
  # =========================================================================

  ### ---------- SMOKING STATUS

  if (isTRUE(is_wave_6_14)){

  data[smoker == 1, current_smoker := "smoker"]
  data[smoker == 2, current_smoker := "non_smoker"]

  data[, current_smoker := as.factor(current_smoker)]

  }

  if (!("current_smoker" %in% colnames(data)) ){

  data[, current_smoker := NA]
  }

  # =========================================================================
  # 4. OTHER SMOKERS IN HOUSEHOLD (DERIVED FROM CURRENT SMOKER)
  # =========================================================================

  if (isTRUE(is_wave_2_5) | isTRUE(is_wave_6_14)) {

  # indicator for any other household smokers
  data[current_smoker == "smoker", smoke := 1]
  data[current_smoker == "non_smoker", smoke := 0]
  data[, num_smoker_hhold := sum(smoke, na.rm=TRUE), by = c("wave_no","hidp")]
      # number of other smokers = number of smokers - respondent
  data[, num_othersmoker_hhold := num_smoker_hhold - smoke]
      # create a binary indicator
  data[num_othersmoker_hhold > 0, othersmoker_hhold := "yes"]
  data[num_othersmoker_hhold == 0, othersmoker_hhold := "no"]

  data[, othersmoker_hhold := as.factor(othersmoker_hhold)]

  }

  if (!("othersmoker_hhold" %in% colnames(data)) ){

  data[, othersmoker_hhold := NA]
  }

  # =========================================================================
  # 5. NUMBER OF CIGARETTES SMOKED BY SMOKERS
  # =========================================================================

  if (!("ncigs" %in% colnames(data)) ){

    data[, ncigs := NA]
  }

  #######################################################
  #### e-cigarette use (WAVES 7 - 12)

  #if ("ecigs" %in% colnames(data)){
  #  data[ecigs %in% 1, ecig := "never_used_ecig"]
  #  data[ecigs %in% 2:3, ecig := "not_currently_using_ecig"]
  #  data[ecigs %in% 4:5, ecig := "infrequent_ecig_use"]
  #  data[ecigs %in% 6, ecig := "frequent_ecig_use"]

  #} else {

  #  data[, ecig := NA]
  #}



  #if ("ecigs" %in% colnames(data)){
  #  data[ecigs %in% 1:3, ecig_current := "non_ecig_user"]
  #  data[ecigs %in% 4:6, ecig_current := "ecig_user"]

  #} else {

  #  data[, ecig_current := NA]
  #}


  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "hidp", "wave_no",
                         "current_smoker", "ncigs", "ever_smoked", "smk_age_start")]

  var_names <- c("current_smoker", "ncigs", "ever_smoked", "smk_age_start")

  setnames(final_data, var_names, paste0("s_", var_names))


  return(final_data)
}



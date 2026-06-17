#' Clean Health and Wellbeing Variables
#'
#' Produce clean versions of variables measuring health outcomes and healthcare utilisation. Also apply the
#' algorithm developed by Gray A, Rivero-Arias O, Clarke P (2006) to estimate EQ-5D utility values from SF-12 responses.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_health <- function(data = NULL) {

  wave <- as.integer(unique(data[ , wave_no][1]))

  # =========================================================================
  # 1. SETUP & CONFIGURATION FLAGS
  # =========================================================================

  is_swemwbs   <- wave %in% c(1,4,7,10,13)

  is_wave7to15 <- wave %in% 7:15

  # =========================================================================
  # 2. VARIABLE CLEANING
  # =========================================================================

  ### individual is a carer for someone in their household

  data[caring == 1, care_hhold := "carer"]
  data[caring != 1 | is.na(caring), care_hhold := "not_carer"]

  data[, care_hhold := as.factor(care_hhold)]

  ### individual has a long-standing illness/disability

  data[, disability := factor(lt_sick,
                              levels = 1:2,
                              labels = c("disability","no_disability"))]

  ### satisfaction with health

  data[, satisfaction_health := factor(health_satisf,
                                       levels = 1:7,
                                       labels = c("completely_disatisfied", "mostly_disatisfied", "somewhat_disatisfied",
                                                  "neutral", "somewhat_satisfied", "mostly_satisfied",
                                                  "completely_satisfied"))]

  ### satisfaction with life in general

  data[, satisfaction_life := factor(life_satisf,
                                     levels = 1:7,
                                     labels = c("completely_disatisfied", "mostly_disatisfied", "somewhat_disatisfied",
                                                "neutral", "somewhat_satisfied", "mostly_satisfied",
                                                "completely_satisfied"))]

  ### SWEMWBS

  if (is_swemwbs){

    ## Feel optimistic about the future
    data[, scwemwba := factor(scwemwba, levels = 1:5, labels = c("none_of_the_time", "rarely", "some_of_the_time","often", "all_of_the_time"))]
    ## Feel useful
    data[, scwemwbb := factor(scwemwbb, levels = 1:5, labels = c("none_of_the_time", "rarely", "some_of_the_time","often", "all_of_the_time"))]
    ## Feel relaxed
    data[, scwemwbc := factor(scwemwbc, levels = 1:5, labels = c("none_of_the_time", "rarely", "some_of_the_time","often", "all_of_the_time"))]
    ## Dealing with problems well
    data[, scwemwbd := factor(scwemwbd, levels = 1:5, labels = c("none_of_the_time", "rarely", "some_of_the_time","often", "all_of_the_time"))]
    ## Thinking clearly
    data[, scwemwbe := factor(scwemwbe, levels = 1:5, labels = c("none_of_the_time", "rarely", "some_of_the_time","often", "all_of_the_time"))]
    ## Feeling close to others
    data[, scwemwbf := factor(scwemwbf, levels = 1:5, labels = c("none_of_the_time", "rarely", "some_of_the_time","often", "all_of_the_time"))]
    ## Able to make up own mind
    data[, scwemwbg := factor(scwemwbg, levels = 1:5, labels = c("none_of_the_time", "rarely", "some_of_the_time","often", "all_of_the_time"))]


  } else {

    data[, scwemwba := NA]
    data[, scwemwbb := NA]
    data[, scwemwbc := NA]
    data[, scwemwbd := NA]
    data[, scwemwbe := NA]
    data[, scwemwbf := NA]
    data[, scwemwbg := NA]

    data[, swemwbs_dv := NA]


  }

  ##########################
  ### currently pregnant ###

  if("pregout1" %in% colnames(data)) {

  data[pregout1 == 4 | pregout2 == 4 , pregnant := 1]
  data[is.na(pregnant) , pregnant := 0]

  data[, c("pregout1","pregout2") := NULL]

  } else {

  data[, pregnant := NA]
  }

  if("pregout3" %in% colnames(data)) {

    data[pregout3 == 4, pregnant := 1]
    data[is.na(pregnant) , pregnant := 0]

    data[, c("pregout3") := NULL]

  }


  if("pregout4" %in% colnames(data)) {

    data[pregout4 == 4, pregnant := 1]
    data[is.na(pregnant) , pregnant := 0]

    data[, c("pregout4") := NULL]

  }
  if("pregout5" %in% colnames(data)) {

    data[pregout5 == 4, pregnant := 1]
    data[is.na(pregnant) , pregnant := 0]

    data[, c("pregout5") := NULL]

  }

  data[, pregnant := factor(pregnant, levels = 0:1, labels = c("not_pregnant","pregnant"))]

  ###################################
  ##### GHQ-12 ######################

  ###################################
  ##### EQ-5D mapped from SF-12 #####

  ## Use the algorithm from :
  ## https://www.herc.ox.ac.uk/downloads/downloads-supporting-material-1/sf-12-responses-and-eq-5d-utility-values
  ##
  ## Gray A, Rivero-Arias O, Clarke P. Estimating the association between SF-12 responses and EQ-5D utility values
  ## by response mapping. Medical Decision Making 2006; 26(1):18-29.

  ## Rename the SF-12 Variables

  setnames(data,
           c("sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7"),
           c("sfstat","sfmode","sfstaira","sfless","sflimit","sflesse","sfcarful","sfpainb","sfcalm","sfener","sflow","sfvisit"))

  eq5d_data <- ukhlsclean::MapEQ5D(data = data,
                                   matrix = ukhlsclean::CoefficientMatrix,
                                   seed = 0)

  merged_data <- merge(data, eq5d_data, by = c("pidp","id","wave_no"), sort = F, all.x = TRUE)

  ###############################################
  ##### Healthcare utilisation (W7 onwards) #####

  if (is_wave7to15){

    merged_data[, gp_use := factor(hl2gp,
                                   levels = 0:4,
                                   labels = c("0","1-2","3-5","6-10","More than 10"))]

    merged_data[, hosp_out := factor(hl2hop,
                                     levels = 0:4,
                                     labels = c("0","1-2","3-5","6-10","More than 10"))]

    merged_data[hosp == 2, hosp_in_days := 0]
    merged_data[hosp == 1, hosp_in_days := hospd]

  } else {

    merged_data[, hosp_in_days := NA]
    merged_data[, hosp_out := NA]
    merged_data[, gp_use := NA]

  }

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- merged_data[, c("pidp", "id", "hidp", "wave_no",
                                "scwemwba","scwemwbb","scwemwbc","scwemwbd","scwemwbe","scwemwbf","scwemwbg","swemwbs_dv",
                                "eq5d_score", "eqmob_est","eqcare_est","equact_est","eqpain_est","eqanx_est", 
                                "sfstat", "sfmode", "sfstaira", "sfless", "sflimit", "sflesse", "sfcarful", "sfpainb", "sfcalm", "sfener", "sflow", "sfvisit",
                                "sf12_pcs","sf12_mcs",
                                "pregnant", "disability", "care_hhold",
                                "satisfaction_health", "satisfaction_life",
                                "gp_use", "hosp_out", "hosp_in_days")]

  var_names <- c("scwemwba","scwemwbb","scwemwbc","scwemwbd","scwemwbe","scwemwbf","scwemwbg","swemwbs_dv",
                 "eq5d_score", "eqmob_est","eqcare_est","equact_est","eqpain_est","eqanx_est", 
                 "sfstat", "sfmode", "sfstaira", "sfless", "sflimit", "sflesse", "sfcarful", "sfpainb", "sfcalm", "sfener", "sflow", "sfvisit",
                 "sf12_pcs","sf12_mcs",
                 "pregnant", "disability", "care_hhold",
                 "satisfaction_health", "satisfaction_life",
                 "gp_use", "hosp_out", "hosp_in_days")

  setnames(final_data, var_names, paste0("h_", var_names))


  return(final_data)

}

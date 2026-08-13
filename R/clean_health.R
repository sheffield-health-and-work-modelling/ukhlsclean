#' Clean Health and Wellbeing Variables
#'
#' Produce clean versions of variables measuring health outcomes and healthcare utilisation. Also apply the
#' algorithm developed by Gray A, Rivero-Arias O, Clarke P (2006) to estimate EQ-5D utility values from SF-12 responses.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
clean_health <- function(data = NULL) {

  wave <- as.integer(unique(data[ , wave_no][1]))

  # =========================================================================
  # 1. SETUP & CONFIGURATION FLAGS
  # =========================================================================

  is_swemwbs   <- wave %in% c(1,4,7,10,13)
  is_wave7to15 <- wave %in% 7:15
  is_wave2to15 <- wave %in% 2:15


  if(isTRUE(is_wave2to15)){

    setnames(data,
  c("scsf1", "scsf2a", "scsf2b", "scsf3a", "scsf3b", "scsf4a", "scsf4b", "scsf5", "scsf6a", "scsf6b", "scsf6c", "scsf7"),
  c("sf1", "sf2a", "sf2b", "sf3a", "sf3b", "sf4a", "sf4b", "sf5", "sf6a", "sf6b", "sf6c", "sf7"))
  }

  # =========================================================================
  # 2. VARIABLE CLEANING
  # =========================================================================

  ### individual is a carer for someone in their household

  data[aidhh == 1, care_hhold := "carer"]
  data[aidhh == 2 | (is.na(aidhh) & hh_size == 1), care_hhold := "not_carer"]

  data[, care_hhold := as.factor(care_hhold)]

  ### individual has a long-standing illness/disability

  data[, lt_sick_ill_disab := factor(health,
                                     levels = 1:2,
                                     labels = c("yes","no"))]

  ### satisfaction with health

  data[, health_sat := factor(sclfsat1,
                                       levels = 1:7,
                                       labels = c("completely_disatisfied", "mostly_disatisfied", "somewhat_disatisfied",
                                                  "neutral", "somewhat_satisfied", "mostly_satisfied",
                                                  "completely_satisfied"))]

  ### satisfaction with life in general

  data[, life_sat := factor(sclfsato,
                                     levels = 1:7,
                                     labels = c("completely_disatisfied", "mostly_disatisfied", "somewhat_disatisfied",
                                                "neutral", "somewhat_satisfied", "mostly_satisfied",
                                                "completely_satisfied"))]
  ### SF-12

  setnames(data, c("sf12pcs_dv","sf12mcs_dv"), c("sf12_pcs","sf12_mcs"))

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

  ### GHQ-12

  ## ghq1_dv - converts all GHQ12 responses from 1-4 to 0-3, then sums up (-0-36)
  ## ghq2_dv - converts all GHQ12 responses 1/2 to 0, 3/4 to 1, then sums up (0-12)

  setnames(data, c("scghq1_dv", "scghq2_dv"), c("ghq12_0_to_36", "ghq12_0_to_12"))

  ### EQ-5D mapped from SF-12

  ## Use the algorithm from :
  ## https://www.herc.ox.ac.uk/downloads/downloads-supporting-material-1/sf-12-responses-and-eq-5d-utility-values
  ##
  ## Gray A, Rivero-Arias O, Clarke P. Estimating the association between SF-12 responses and EQ-5D utility values
  ## by response mapping. Medical Decision Making 2006; 26(1):18-29.

  ## Rename the SF-12 Variables

  setnames(data,
           c("sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7"),
           c("sfstat","sfmode","sfstaira","sfless","sflimit","sflesse","sfcarful","sfpainb","sfcalm","sfener","sflow","sfvisit"))

  eq5d_data <- ukhlsclean::Map_SF12_EQ5D3L(data = data,
                                           matrix = ukhlsclean::CoefficientMatrix,
                                           seed = 0)

  merged_data <- merge(data, eq5d_data, by = c("pidp","wave_no"), sort = F, all.x = TRUE)

  ### Healthcare utilisation (W7 onwards)

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

  final_data <- merged_data[, c("pidp", "hidp", "wave_no",
                                "scwemwba","scwemwbb","scwemwbc","scwemwbd","scwemwbe","scwemwbf","scwemwbg","swemwbs_dv",
                                "eq5d_score", "eqmob_est","eqcare_est","equact_est","eqpain_est","eqanx_est",
                                "sfstat", "sfmode", "sfstaira", "sfless", "sflimit", "sflesse", "sfcarful", "sfpainb", "sfcalm", "sfener", "sflow", "sfvisit",
                                "sf12_pcs","sf12_mcs",
                                "ghq12_0_to_36", "ghq12_0_to_12",
                                "lt_sick_ill_disab", "care_hhold",
                                "life_sat", "health_sat",
                                "gp_use", "hosp_out", "hosp_in_days")]

  var_names <- c("scwemwba","scwemwbb","scwemwbc","scwemwbd","scwemwbe","scwemwbf","scwemwbg","swemwbs_dv",
                 "eq5d_score", "eqmob_est","eqcare_est","equact_est","eqpain_est","eqanx_est",
                 "sfstat", "sfmode", "sfstaira", "sfless", "sflimit", "sflesse", "sfcarful", "sfpainb", "sfcalm", "sfener", "sflow", "sfvisit",
                 "sf12_pcs","sf12_mcs",
                 "ghq12_0_to_36", "ghq12_0_to_12",
                 "lt_sick_ill_disab", "care_hhold",
                 "life_sat", "health_sat",
                 "gp_use", "hosp_out", "hosp_in_days")

  setnames(final_data, var_names, paste0("h_", var_names))


  return(final_data)

}

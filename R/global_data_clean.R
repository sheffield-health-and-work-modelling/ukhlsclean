#' Clean UKHLS data
#'
#' A wrapper function for applying all of the cleaning functions, selecting the
#' desired variables/observations for the analysis, and specifying complete case
#' restrictions.
#'
#' @param data Data table - the combined Understanding Society dataset for one wave.
#' @param ages Integer vector - the ages in single years to retain (defaults to NULL - all ages).
#' @param country Character - country to produce data for. One of c("england","wales","scotland","northern_ireland"). Defaults to NULL which includes all UK.
#' @param keep_vars Character vector - the names of the variables to keep (defaults NULL - keep all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to NULL - keep all observations).
#' @param calendar_year Logical - TRUE when the code is processing calendar year data (defaults to FALSE).
#' @param inflation Data table. Inflation data input for real-terms adjustments. Defaults to CPIH.
#'
#' @return Returns a new set of variables
#' @export
global_data_clean <- function(data,
                              ages = 16:89,
                              country = NULL,
                              complete_vars = NULL,
                              calendar_year = FALSE,
                              inflation = ukhlsclean::cpih
) {

  #######################################################################
  #### Save out a dataset containing the key identifiers and weights ####

  main_data <- data[, c("pidp", "hidp", "wave_no", "bhps_sample", "strata", "psu",
                        "istrtdaty", "istrtdatm", "istrtdatd")]

  ### survey weights
  print("survey weights")
  weights <- ukhlsclean::clean_survey_weights(data = data)

  ### demographics
  print("demographics")
  demographics <- ukhlsclean::clean_demographic(data = data)

  ### labour market
  print("labour market")
  lmkt <- ukhlsclean::clean_labour_market(data = data,
                                          inflation = inflation)

  ### work
  print("work")
  work <- ukhlsclean::clean_work(data = data, calendar_year = calendar_year)

  ### benefits
  print("benefits")
  #benefit <- ukhlsclean::clean_benefits(data = data)

  ### income
  print("income")
  income <- ukhlsclean::clean_income(data = data)

  ### health and well-being
  print("health and wellbeing")
  health <- ukhlsclean::clean_health(data = data)

  ### health conditions
  print("health conditions")
  health_conditions <- ukhlsclean::clean_health_conditions(data = data)

  ### alcohol
  print("alcohol")
  alcohol <- ukhlsclean::clean_alcohol(data = data)

  ### smoking
  print("smoking")
  smoke <- ukhlsclean::clean_smoke(data = data)

  ### household
  print("household")
  hhold <- ukhlsclean::clean_hhold(data = data, calendar_year = calendar_year, inflation = inflation)

  ######################
  ### Merge datasets ###
  print("merge datasets")

  merged_data <- merge(main_data, weights,             by = c("pidp", "hidp", "wave_no"))
  merged_data <- merge(merged_data, demographics,      by = c("pidp", "hidp", "wave_no"))
  merged_data <- merge(merged_data, lmkt,              by = c("pidp", "hidp", "wave_no"))
  merged_data <- merge(merged_data, work,              by = c("pidp", "hidp", "wave_no"))
  #merged_data <- merge(merged_data, benefit,           by = c("pidp", "hidp", "wave_no"))
  merged_data <- merge(merged_data, income,            by = c("pidp", "hidp", "wave_no"))
  merged_data <- merge(merged_data, health,            by = c("pidp", "hidp", "wave_no"))
  merged_data <- merge(merged_data, health_conditions, by = c("pidp", "hidp", "wave_no"))
  merged_data <- merge(merged_data, alcohol,           by = c("pidp", "hidp", "wave_no"))
  merged_data <- merge(merged_data, smoke,             by = c("pidp", "hidp", "wave_no"))
  merged_data <- merge(merged_data, hhold,             by = c("pidp", "hidp", "wave_no"))

  ############################
  ### Apply data filtering ###

  final_data <- ukhlsclean::select_data(
    data = merged_data,
    ages = ages,
    country = country,
    complete_vars = complete_vars,
    calendar_year = calendar_year
  )


  return(final_data)
}

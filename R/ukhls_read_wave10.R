#' Read Understanding Society Wave 10
#'
#' Reads and performs basic cleaning operations on the UKHLS tenth wave. Missing values as detailed below are all set to NA.
#'
#' MISSING VALUES
#'
#' \itemize{
#' \item -1 Don't know. When the respondent does not know the answer to a question.
#' \item -2 Refused: When the respondent refuses to answer a question.
#' \item -7 Proxy: A question not included in the subset of questions asked of proxy respondents.
#' obtained or not attempted.
#' \item -8 Not applicable: Used to signify that a particular variable did not apply to a given respondent
#' usually because of internal routing. For example, men in women only questions.
#' \item -9 Missing by error or implausible answer.
#' }
#'
#' @source University of Essex, Institute for Social and Economic Research. (2022). Understanding Society: Waves 1-12, 2009-2021
#' and Harmonised BHPS: Waves 1-18, 1991-2009. [data collection]. 17th Edition. UK Data Service. SN: 6614,
#' \href{https://beta.ukdataservice.ac.uk/datacatalogue/studies/study?id=6614}{DOI: 10.5255/UKDA-SN-6614-18}
#'
#' @param root Character - the root directory.
#' @param file Character - the file path and name.
#' @param full Logical - TRUE if restricting the sample to full interviews only (excluding proxies)
#' @importFrom data.table :=
#' @return Returns a data table. Note that:
#' \itemize{
#' \item Missing data ("NA", "", "-1", "-2", "-6", "-7", "-9", "-90", "-90.0", "N/A") is replace with NA,
#' except -8 ("don't know") as this is data.
#' \item All variable names are converted to lower case.
#' \item Each data point is assigned a weight of 1 as there is no weight variable supplied.
#' \item A single sampling cluster is assigned.
#' \item The probabilistic sampling unit have the year appended to them.
#' }
#' @export
ukhls_read_wave10 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 10 datasets")))

  # ==========================================
  # 1. Main Individual Response File
  # ==========================================

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/j_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[j_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, j_hidp, j_pno, j_psu, j_strata, j_istrtdaty, j_istrtdatm, j_istrtdatd)
  demographic_vars <- Hmisc::Cs(j_sex, j_dvage, j_birthy, j_gor_dv, j_urban_dv, j_mlstat, j_marstat, j_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(j_notempchk, j_empchk)
  econ_stat_vars   <- Hmisc::Cs(j_jbstat, j_jbhas, j_jboff, j_jboffy, j_jbterm1, j_jbterm2, j_jbsemp, j_jbpen, j_jbpenm)
  income_vars      <- Hmisc::Cs(j_fimnnet_dv, j_fimngrs_dv,
                                j_fimnlabnet_dv, j_fimnlabgrs_dv, j_fimnmisc_dv, j_fimnprben_dv, j_fimninvnet_dv, j_fimnpen_dv, j_fimnsben_dv)
  work_vars        <- Hmisc::Cs(j_paygu_dv, j_payg_dv, j_jbhrs, j_seearngrs_dv, j_jbsic07_cc, j_jbot, j_jbotpd, j_jshrs,
                                j_jbnssec_dv, j_jbnssec3_dv, j_jbnssec5_dv, j_jbnssec8_dv, j_jbsize)
  benefits_vars    <- Hmisc::Cs(j_benbase1, j_benbase2, j_benbase3, j_benbase4, j_benbase96,
                                j_benctc)
  hhfinance_vars   <- Hmisc::Cs(j_fiyrdia, j_fiyrdb1, j_fiyrdb2, j_fiyrdb3, j_fiyrdb4, j_fiyrdb5, j_fiyrdb6, j_finnow, j_finfut)
  health_vars      <- Hmisc::Cs(j_health, j_aidhh, j_sclfsat1, j_sclfsato, j_sf12pcs_dv, j_sf12mcs_dv,
                                j_scsf1, j_scsf2a, j_scsf2b, j_scsf3a, j_scsf3b, j_scsf4a, j_scsf4b, j_scsf5, j_scsf6a, j_scsf6b, j_scsf6c, j_scsf7,
                                j_scghq1_dv,j_scghq2_dv,
                                j_scwemwba, j_scwemwbb, j_scwemwbc, j_scwemwbd, j_scwemwbe, j_scwemwbf, j_scwemwbg, j_swemwbs_dv)
  health_care_vars <- Hmisc::Cs(j_hl2gp, j_hl2hop, j_hosp, j_hospd)
  health_cond_vars <- Hmisc::Cs(j_hcond1, j_hcond2, j_hcond3, j_hcond4, j_hcond5, j_hcond6, j_hcond7, j_hcond8, j_hcond10, j_hcond11, j_hcond12, j_hcond13, j_hcond14, j_hcond15, j_hcond16, j_hcond21,

                                j_hcondever1, j_hcondever2, j_hcondever3, j_hcondever4, j_hcondever5, j_hcondever6, j_hcondever7, j_hcondever8, j_hcondever10, j_hcondever11, j_hcondever12, j_hcondever13, j_hcondever14, j_hcondever15, j_hcondever16, j_hcondever21,

                                j_hconds01, j_hconds03, j_hconds04, j_hconds05, j_hconds08, j_hconds10, j_hconds11, j_hconds12, j_hconds15, j_hconds16, j_hconds21,
                                j_hconds23, j_hconds24, j_hconds26, j_hconds27, j_hconds28, j_hconds29, j_hconds30, j_hconds31, j_hconds33, j_hconds34, j_hconds35
  )
  smoke_vars       <- Hmisc::Cs(j_smoker, j_ncigs, j_giveup, j_gvupreas1, j_gvupreas2, j_gvupreas3, j_gvupreas4, j_gvupreas5, j_gvupreas6, j_gvupreas7, j_gvupreas8, j_gvupreas9, j_ecigs1)
  alc_vars         <- Hmisc::Cs(j_dklm, j_drnk4w, j_evralc, j_fivealcdr)
  weight_vars      <- Hmisc::Cs(j_indinus_lw, j_indinui_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^j_", "", colnames(data))

  data[, wave_no := 10]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  # ==========================================
  # 2. Income File
  # ==========================================

  cat(crayon::green("\tIncome.."))

  data.income <- data.table::fread(
    paste0(path, "/j_income.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  colnames(data.income) <- sub("^j_", "", colnames(data.income))

  data.income <- process_income_data(data.income = data.income)

  ### for individuals not in the income file set their benefit receipt variables equal to:
  ### - 0 if a full interview (ivfio = 1)
  ### - NA if a proxy interview (ivfio = 2) as proxy respondents routed away from income questions

  income_merged <- merge(x = data, y = data.income, by="pidp", all.x=TRUE)

  cols <- paste0("ben_receipt_", c(8:16, 18:23, 33, 40, 41))
  income_merged[, (cols) := lapply(.SD, function(x) fifelse(is.na(x) & ivfio == 1, 0, x)) , .SDcols = cols]
  income_merged[, (cols) := lapply(.SD, function(x) fifelse(is.na(x) & ivfio == 2, NA_real_ , x)), .SDcols = cols]

  # ==========================================
  # 3. Household File
  # ==========================================

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/j_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(j_hidp, j_tenure_dv, j_nkids_dv, j_hhsize, j_hhtype_dv,
                                   j_nch02_dv, j_nch34_dv, j_nch511_dv, j_nch1215_dv,
                                   j_fihhmngrs1_dv, j_fihhmnlabgrs_dv,
                                   j_fihhmnnet1_dv, j_fihhmnlabnet_dv, j_fihhmnsben_dv, j_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("j_hidp","j_tenure_dv","j_nkids_dv","j_hhsize","j_hhtype_dv",
                         "j_nch02_dv","j_nch34_dv","j_nch511_dv","j_nch1215_dv",
                         "j_fihhmngrs1_dv", "j_fihhmnlabgrs_dv",
                         "j_fihhmnnet1_dv", "j_fihhmnlabnet_dv", "j_fihhmnsben_dv","j_ieqmoecd_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numchild","hh_size","hh_type",
                         "hh_numchild02","hh_numchild34","hh_numchild511","hh_numchild1215",
                         "hh_fihhmngrs1_dv", "hh_fihhmnlabgrs_dv",
                         "hh_fihhmnnet1_dv", "hh_fihhmnlabnet_dv", "hh_fihhmnsben_dv","ieqmoecd_dv"))

  hhold_merged <- merge(x = income_merged,
                        y = data.hhold,
                        by="hidp",
                        all.x=TRUE,
                        all.y=FALSE)

  #########################################
  ######## ADD IN CROSS-WAVE DATA #########

  cat(crayon::green("\tCross-Wave..."))

  data.xwave <- data.table::fread(
    paste0(path, "/xwavedat.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.xwave, names(data.xwave), tolower(names(data.xwave)))

  xwave_vars  <- colnames(data.xwave[ , c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv")])

  data.xwave <- data.xwave[ , xwave_vars, with = F]
  data.table::setnames(data.xwave,
                       # old names
                       c("pidp","ethn_dv","dcsedfl_dv","dcsedw_dv"),
                       # new names
                       c("pidp","ethnicity_raw","deceased","deceased_when"))

  ####### Combine - keep all observations in the main data and drop excess xwave observations

  xwave_merged <- merge(x = hhold_merged,
                        y = data.xwave,
                        by="pidp",
                        all.x=TRUE,
                        all.y=FALSE)

  ####################################################
  #### ADD IN THE INDALL DATA ########################

  cat(crayon::green("\tIndall..."))

  data.indall <- data.table::fread(
    paste0(path, "/j_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","j_hidp",
                                            "j_imd2019qe_dv","j_imd2017qni_dv",
                                            "j_imd2020qs_dv","j_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","j_hidp",
                         "j_imd2019qe_dv","j_imd2017qni_dv",
                         "j_imd2020qs_dv","j_imd2019qw_dv"),
                       # new names
                       c("pidp","hidp",
                         "imdq_e","imdq_ni",
                         "imdq_s","imdq_w"))

  ## Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = xwave_merged,
                       y = data.indall,
                       by = c("pidp","hidp"),
                       all.x = TRUE,
                       all.y = FALSE)

  ##########################################################################

  cat(crayon::blue(crayon::bold("\tdone\n")))

  return(data_merged)
}

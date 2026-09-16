#' Read Understanding Society Wave 9
#'
#' Reads and performs basic cleaning operations on the UKHLS ninth wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave9 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 9 datasets")))

  # ==========================================
  # 1. Main Individual Response File
  # ==========================================

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/i_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[i_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, i_hidp, i_pno, i_psu, i_strata, i_istrtdaty, i_istrtdatm, i_istrtdatd, i_ivfio)
  demographic_vars <- Hmisc::Cs(i_sex, i_dvage, i_birthy, i_gor_dv, i_urban_dv, i_mlstat, i_marstat, i_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(i_notempchk, i_empchk)
  econ_stat_vars   <- Hmisc::Cs(i_jbstat, i_jbhas, i_jboff, i_jboffy, i_jbterm1, i_jbterm2, i_jbsemp)
  income_vars      <- Hmisc::Cs(i_fimnnet_dv, i_fimngrs_dv,
                                i_fimnlabnet_dv, i_fimnlabgrs_dv, i_fimnmisc_dv, i_fimnprben_dv, i_fimninvnet_dv, i_fimnpen_dv, i_fimnsben_dv)
  work_vars        <- Hmisc::Cs(i_paygu_dv, i_payg_dv, i_jbhrs, i_seearngrs_dv, i_jbsic07_cc, i_jbot, i_jbotpd, i_jshrs,
                                i_jbnssec_dv, i_jbnssec3_dv, i_jbnssec5_dv, i_jbnssec8_dv, i_jbsize)
  benefits_vars    <- Hmisc::Cs(i_benbase1, i_benbase2, i_benbase3, i_benbase4, i_benbase96,
                                i_benctc)
  hhfinance_vars   <- Hmisc::Cs(i_fiyrdia, i_fiyrdb1, i_fiyrdb2, i_fiyrdb3, i_fiyrdb4, i_fiyrdb5, i_fiyrdb6, i_finnow, i_finfut)
  health_vars      <- Hmisc::Cs(i_health, i_aidhh, i_sclfsat1, i_sclfsato, i_sf12pcs_dv, i_sf12mcs_dv,
                                i_scsf1, i_scsf2a, i_scsf2b, i_scsf3a, i_scsf3b, i_scsf4a, i_scsf4b, i_scsf5, i_scsf6a, i_scsf6b, i_scsf6c, i_scsf7,
                                i_scghq1_dv,i_scghq2_dv)
  health_cond_vars <- Hmisc::Cs(i_hcond1, i_hcond2, i_hcond3, i_hcond4, i_hcond5, i_hcond6, i_hcond7, i_hcond8, i_hcond9, i_hcond10, i_hcond11, i_hcond12, i_hcond13, i_hcond14, i_hcond15, i_hcond16, i_hcond17,

                                i_hcondn1, i_hcondn2, i_hcondn3, i_hcondn4, i_hcondn5, i_hcondn6, i_hcondn7, i_hcondn8, i_hcondn9, i_hcondn10, i_hcondn11, i_hcondn12, i_hcondn13, i_hcondn14, i_hcondn15, i_hcondn16, i_hcondn17,

                                i_hconds01, i_hconds02, i_hconds03, i_hconds04, i_hconds05, i_hconds08, i_hconds09, i_hconds10, i_hconds11, i_hconds12, i_hconds13, i_hconds14, i_hconds15, i_hconds16, i_hconds17
  )
  health_care_vars <- Hmisc::Cs(i_hl2gp, i_hl2hop, i_hosp, i_hospd)
  smoke_vars       <- Hmisc::Cs(i_smoker, i_ncigs, i_giveup, i_gvupreas1, i_gvupreas2, i_gvupreas3, i_gvupreas4, i_gvupreas5, i_gvupreas6, i_gvupreas7, i_gvupreas8, i_gvupreas9, i_ecigs1)
  alc_vars         <- Hmisc::Cs(i_auditc1, i_auditc2, i_auditc3, i_auditc4, i_auditc5)
  weight_vars      <- Hmisc::Cs(i_indinus_lw, i_indinui_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^i_", "", colnames(data))

  data[, wave_no := 9]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  # ==========================================
  # 2. Income File
  # ==========================================

  cat(crayon::green("\tIncome.."))

  data.income <- data.table::fread(
    paste0(path, "/i_income.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  colnames(data.income) <- sub("^i_", "", colnames(data.income))

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
    paste0(path, "/i_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(i_hidp, i_tenure_dv, i_nkids_dv, i_hhsize, i_hhtype_dv,
                                   i_nch02_dv, i_nch34_dv, i_nch511_dv, i_nch1215_dv,
                                   i_fihhmngrs1_dv, i_fihhmnlabgrs_dv,
                                   i_fihhmnnet1_dv, i_fihhmnlabnet_dv, i_fihhmnsben_dv, i_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("i_hidp","i_tenure_dv","i_nkids_dv","i_hhsize","i_hhtype_dv",
                         "i_nch02_dv","i_nch34_dv","i_nch511_dv","i_nch1215_dv",
                         "i_fihhmngrs1_dv", "i_fihhmnlabgrs_dv",
                         "i_fihhmnnet1_dv", "i_fihhmnlabnet_dv", "i_fihhmnsben_dv","i_ieqmoecd_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numchild","hh_size","hh_type",
                         "hh_numchild02","hh_numchild34","hh_numchild511","hh_numchild1215",
                         "hh_fihhmngrs1_dv", "hh_fihhmnlabgrs_dv",
                         "hh_fihhmnnet1_dv", "hh_fihhmnlabnet_dv", "hh_fihhmnsben_dv","ieqmoecd_dv"))

  hhold_merged <- merge(x = income_merged,
                        y = data.hhold,
                        by = "hidp",
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
    paste0(path, "/i_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","i_hidp",
                                            "i_imd2019qe_dv","i_imd2017qni_dv",
                                            "i_imd2020qs_dv","i_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","i_hidp",
                         "i_imd2019qe_dv","i_imd2017qni_dv",
                         "i_imd2020qs_dv","i_imd2019qw_dv"),
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

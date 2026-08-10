#' Read Understanding Society Wave 6
#'
#' Reads and performs basic cleaning operations on the UKHLS sixth wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave6 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 6 datasets")))

  # ==========================================
  # 1. Main Individual Response File
  # ==========================================

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/f_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[f_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, f_hidp,f_pno, f_psu, f_strata, f_istrtdaty, f_istrtdatm, f_istrtdatd)
  demographic_vars <- Hmisc::Cs(f_sex, f_dvage, f_birthy, f_gor_dv, f_urban_dv, f_mlstat, f_marstat, f_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(f_notempchk, f_empchk)
  econ_stat_vars   <- Hmisc::Cs(f_jbstat, f_jbhas, f_jboff, f_jboffy, f_jbterm1, f_jbterm2, f_jbsemp, f_jbpen, f_jbpenm)
  income_vars      <- Hmisc::Cs(f_fimnnet_dv, f_fimngrs_dv,
                                f_fimnlabnet_dv, f_fimnlabgrs_dv, f_fimnmisc_dv, f_fimnprben_dv, f_fimninvnet_dv, f_fimnpen_dv, f_fimnsben_dv)
  work_vars        <- Hmisc::Cs(f_paygu_dv, f_payg_dv, f_jbhrs, f_seearngrs_dv, f_jbsic07_cc, f_jbot, f_jbotpd, f_jshrs,
                                f_jbnssec_dv, f_jbnssec3_dv, f_jbnssec5_dv, f_jbnssec8_dv, f_jbsize)
  benefits_vars    <- Hmisc::Cs(f_benbase1, f_benbase2, f_benbase3, f_benbase4, f_benbase96,
                                f_benctc)
  hhfinance_vars   <- Hmisc::Cs(f_fiyrdia, f_fiyrdb1, f_fiyrdb2, f_fiyrdb3, f_fiyrdb4, f_fiyrdb5, f_fiyrdb6, f_finnow, f_finfut)
  health_care_vars  <- NULL
  health_vars      <- Hmisc::Cs(f_health, f_aidhh, f_sclfsat1, f_sclfsato, f_sf12pcs_dv, f_sf12mcs_dv,
                                f_scsf1, f_scsf2a, f_scsf2b, f_scsf3a, f_scsf3b, f_scsf4a, f_scsf4b, f_scsf5, f_scsf6a, f_scsf6b, f_scsf6c, f_scsf7,
                                f_scghq1_dv,f_scghq2_dv)
  health_cond_vars <- Hmisc::Cs(f_hcond1, f_hcond2, f_hcond3, f_hcond4, f_hcond5, f_hcond6, f_hcond7, f_hcond8, f_hcond9, f_hcond10, f_hcond11, f_hcond12, f_hcond13, f_hcond14, f_hcond15, f_hcond16, f_hcond17,

                                f_hcondn1, f_hcondn2, f_hcondn3, f_hcondn4, f_hcondn5, f_hcondn6, f_hcondn7, f_hcondn8, f_hcondn9, f_hcondn10, f_hcondn11, f_hcondn12, f_hcondn13, f_hcondn14, f_hcondn15, f_hcondn16, f_hcondn17,

                                f_hconds01, f_hconds02, f_hconds03, f_hconds04, f_hconds05, f_hconds08, f_hconds09, f_hconds10, f_hconds11, f_hconds12, f_hconds13, f_hconds14, f_hconds15, f_hconds16, f_hconds17
                                )
  smoke_vars       <- Hmisc::Cs(f_smoker, f_ncigs)
  alc_vars         <- Hmisc::Cs(f_dklm, f_drnk4w,f_evralc, f_fivealcdr)
  weight_vars      <- Hmisc::Cs(f_indinus_lw, f_indinui_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^f_", "", colnames(data))

  data[, wave_no := 6]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  # ==========================================
  # 2. Income File
  # ==========================================

  cat(crayon::green("\tIncome.."))

  data.income <- data.table::fread(
    paste0(path, "/f_income.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  colnames(data.income) <- sub("^f_", "", colnames(data.income))

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
    paste0(path, "/f_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(f_hidp, f_tenure_dv, f_nkids_dv, f_hhsize, f_hhtype_dv,
                                   f_nch02_dv, f_nch34_dv, f_nch511_dv, f_nch1215_dv,
                                   f_fihhmngrs1_dv, f_fihhmnlabgrs_dv,
                                   f_fihhmnnet1_dv, f_fihhmnlabnet_dv, f_fihhmnsben_dv, f_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("f_hidp","f_tenure_dv","f_nkids_dv","f_hhsize","f_hhtype_dv",
                         "f_nch02_dv","f_nch34_dv","f_nch511_dv","f_nch1215_dv",
                         "f_fihhmngrs1_dv", "f_fihhmnlabgrs_dv",
                         "f_fihhmnnet1_dv", "f_fihhmnlabnet_dv", "f_fihhmnsben_dv","f_ieqmoecd_dv"),
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
    paste0(path, "/f_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","f_hidp",
                                            "f_imd2019qe_dv","f_imd2017qni_dv",
                                            "f_imd2020qs_dv","f_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","f_hidp",
                         "f_imd2019qe_dv","f_imd2017qni_dv",
                         "f_imd2020qs_dv","f_imd2019qw_dv"),
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

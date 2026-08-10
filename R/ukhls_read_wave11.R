#' Read Understanding Society Wave 11
#'
#' Reads and performs basic cleaning operations on the UKHLS eleventh wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave11 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 11 datasets")))

  # ==========================================
  # 1. Main Individual Response File
  # ==========================================

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/k_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[k_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, k_hidp, k_pno, k_psu, k_strata, k_istrtdaty, k_istrtdatm, k_istrtdatd, k_ivfio)
  demographic_vars <- Hmisc::Cs(k_sex, k_dvage, k_birthy, k_gor_dv, k_urban_dv, k_mlstat, k_marstat, k_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(k_notempchk, k_empchk)
  econ_stat_vars   <- Hmisc::Cs(k_jbstat, k_jbhas, k_jboff, k_jboffy, k_jbterm1, k_jbterm2, k_jbsemp)
  income_vars      <- Hmisc::Cs(k_fimnnet_dv, k_fimngrs_dv,
                                k_fimnlabnet_dv, k_fimnlabgrs_dv, k_fimnmisc_dv, k_fimnprben_dv, k_fimninvnet_dv, k_fimnpen_dv, k_fimnsben_dv)
  work_vars        <- Hmisc::Cs(k_paygu_dv, k_payg_dv, k_jbhrs, k_seearngrs_dv, k_jbsic07_cc, k_jbot, k_jbotpd, k_jshrs,
                                k_jbnssec_dv, k_jbnssec3_dv, k_jbnssec5_dv, k_jbnssec8_dv, k_jbsize)
  benefits_vars    <- Hmisc::Cs(k_benbase1, k_benbase2, k_benbase3, k_benbase4, k_benbase96,
                                k_benctc)
  hhfinance_vars   <- Hmisc::Cs(k_fiyrdia, k_fiyrdb1, k_fiyrdb2, k_fiyrdb3, k_fiyrdb4, k_fiyrdb5, k_fiyrdb6, k_finnow, k_finfut)
  health_vars      <- Hmisc::Cs(k_health, k_aidhh, k_sclfsat1, k_sclfsato, k_sf12pcs_dv, k_sf12mcs_dv,
                                k_scsf1, k_scsf2a, k_scsf2b, k_scsf3a, k_scsf3b, k_scsf4a, k_scsf4b, k_scsf5, k_scsf6a,
                                k_scsf6b, k_scsf6c, k_scsf7,
                                k_scghq1_dv,k_scghq2_dv)
  health_care_vars <- Hmisc::Cs(k_hl2gp, k_hl2hop, k_hosp, k_hospd)
  health_cond_vars <- Hmisc::Cs(k_hcond1, k_hcond2, k_hcond3, k_hcond4, k_hcond5, k_hcond6, k_hcond7, k_hcond8, k_hcond10, k_hcond11, k_hcond12, k_hcond13, k_hcond14, k_hcond15, k_hcond16, k_hcond21,

                                k_hcondnew1, k_hcondnew2, k_hcondnew3, k_hcondnew4, k_hcondnew5, k_hcondnew6, k_hcondnew7, k_hcondnew8, k_hcondnew10, k_hcondnew11, k_hcondnew12, k_hcondnew13, k_hcondnew14, k_hcondnew15, k_hcondnew16, k_hcondnew21,

                                k_hconds01, k_hconds03, k_hconds04, k_hconds05, k_hconds08, k_hconds10, k_hconds11, k_hconds12, k_hconds15, k_hconds16, k_hconds21,
                                k_hconds23, k_hconds24, k_hconds26, k_hconds27, k_hconds28, k_hconds29, k_hconds30, k_hconds31, k_hconds33, k_hconds34, k_hconds35
  )
  smoke_vars       <- Hmisc::Cs(k_smoker, k_ncigs, k_giveup, k_gvupreas1, k_gvupreas2, k_gvupreas3, k_gvupreas4, k_gvupreas5, k_gvupreas6, k_gvupreas7, k_gvupreas8, k_gvupreas9, k_ecigs1)
  alc_vars         <- Hmisc::Cs(k_auditc1, k_auditc2, k_auditc3, k_auditc4, k_auditc5)
  weight_vars      <- Hmisc::Cs(k_indinus_lw, k_indinui_xw)

  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^k_", "", colnames(data))

  data[, wave_no := 11]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  # ==========================================
  # 2. Income File
  # ==========================================

  cat(crayon::green("\tIncome.."))

  data.income <- data.table::fread(
    paste0(path, "/k_income.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  colnames(data.income) <- sub("^k_", "", colnames(data.income))

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
    paste0(path, "/k_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(k_hidp, k_tenure_dv, k_nkids_dv, k_hhsize, k_hhtype_dv,
                                   k_nch02_dv, k_nch34_dv, k_nch511_dv, k_nch1215_dv,
                                   k_fihhmngrs1_dv, k_fihhmnlabgrs_dv,
                                   k_fihhmnnet1_dv, k_fihhmnlabnet_dv, k_fihhmnsben_dv, k_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("k_hidp", "k_tenure_dv", "k_nkids_dv", "k_hhsize","k_hhtype_dv",
                         "k_nch02_dv", "k_nch34_dv", "k_nch511_dv", "k_nch1215_dv",
                         "k_fihhmngrs1_dv", "k_fihhmnlabgrs_dv",
                         "k_fihhmnnet1_dv", "k_fihhmnlabnet_dv", "k_fihhmnsben_dv","k_ieqmoecd_dv"),
                       # new names
                       c("hidp", "hh_tenure", "hh_numchild", "hh_size", "hh_type",
                         "hh_numchild02", "hh_numchild34", "hh_numchild511", "hh_numchild1215",
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
    paste0(path, "/k_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","k_hidp",
                                            "k_imd2019qe_dv","k_imd2017qni_dv",
                                            "k_imd2020qs_dv","k_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","k_hidp",
                         "k_imd2019qe_dv","k_imd2017qni_dv",
                         "k_imd2020qs_dv","k_imd2019qw_dv"),
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

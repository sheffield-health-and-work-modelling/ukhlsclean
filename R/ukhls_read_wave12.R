#' Read Understanding Society Wave 12
#'
#' Reads and performs basic cleaning operations on the UKHLS twelfth wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave12 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 12 datasets")))

  # ==========================================
  # 1. Main Individual Response File
  # ==========================================

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/l_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[l_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, l_hidp, l_pno, l_psu, l_strata, l_istrtdaty, l_istrtdatm, l_istrtdatd)
  demographic_vars <- Hmisc::Cs(l_sex, l_dvage, l_birthy, l_gor_dv, l_urban_dv, l_mlstat, l_marstat, l_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(l_notempchk, l_empchk)
  econ_stat_vars   <- Hmisc::Cs(l_jbstat, l_jbhas, l_jboff, l_jboffy, l_jbterm1, l_jbterm2, l_jbsemp, l_jbpen, l_jbpenm)
  income_vars      <- Hmisc::Cs(l_fimnnet_dv,l_fimngrs_dv,
                                l_fimnlabnet_dv, l_fimnlabgrs_dv, l_fimnmisc_dv, l_fimnprben_dv, l_fimninvnet_dv, l_fimnpen_dv, l_fimnsben_dv)
  work_vars        <- Hmisc::Cs(l_paygu_dv, l_payg_dv, l_jbhrs, l_seearngrs_dv, l_jbsic07_cc, l_jbot, l_jbotpd, l_jshrs,
                                l_jbnssec_dv, l_jbnssec3_dv, l_jbnssec5_dv, l_jbnssec8_dv, l_jbsize)
  benefits_vars    <- Hmisc::Cs(l_benbase1, l_benbase2, l_benbase3, l_benbase4, l_benbase96,
                                l_benctc)
  hhfinance_vars   <- Hmisc::Cs(l_fiyrdia, l_fiyrdb1, l_fiyrdb2, l_fiyrdb3, l_fiyrdb4, l_fiyrdb5, l_fiyrdb6, l_finnow, l_finfut)
  health_vars      <- Hmisc::Cs(l_health, l_aidhh, l_sclfsat1, l_sclfsato, l_sf12pcs_dv, l_sf12mcs_dv,
                                l_scsf1, l_scsf2a, l_scsf2b, l_scsf3a, l_scsf3b, l_scsf4a, l_scsf4b, l_scsf5, l_scsf6a,
                                l_scsf6b, l_scsf6c, l_scsf7,
                                l_scghq1_dv,l_scghq2_dv)
  health_cond_vars <- Hmisc::Cs(l_hcond1, l_hcond2, l_hcond3, l_hcond4, l_hcond5, l_hcond6, l_hcond7, l_hcond8, l_hcond10, l_hcond11, l_hcond12, l_hcond13, l_hcond14, l_hcond15, l_hcond16, l_hcond21,

                                l_hcondnew1, l_hcondnew2, l_hcondnew3, l_hcondnew4, l_hcondnew5, l_hcondnew6, l_hcondnew7, l_hcondnew8, l_hcondnew10, l_hcondnew11, l_hcondnew12, l_hcondnew13, l_hcondnew14, l_hcondnew15, l_hcondnew16, l_hcondnew21,

                                l_hconds01, l_hconds03, l_hconds04, l_hconds05, l_hconds08, l_hconds10, l_hconds11, l_hconds12, l_hconds15, l_hconds16, l_hconds21,
                                l_hconds23, l_hconds24, l_hconds26, l_hconds27, l_hconds28, l_hconds29, l_hconds30, l_hconds31, l_hconds33, l_hconds34, l_hconds35
  )
  health_care_vars <- Hmisc::Cs(l_hl2gp, l_hl2hop, l_hosp, l_hospd)
  smoke_vars       <- Hmisc::Cs(l_smoker, l_ncigs, l_giveup, l_gvupreas1, l_gvupreas2, l_gvupreas3, l_gvupreas4, l_gvupreas5, l_gvupreas6, l_gvupreas7, l_gvupreas8, l_gvupreas9, l_ecigs1)
  alc_vars         <- Hmisc::Cs(l_dklm, l_drnk4w, l_evralc, l_fivealcdr, l_auditc1, l_auditc2, l_auditc3, l_auditc4, l_auditc5)
  weight_vars      <- Hmisc::Cs(l_indinus_lw, l_indinui_xw)

  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^l_", "", colnames(data))

  data[, wave_no := 12]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  # ==========================================
  # 2. Income File
  # ==========================================

  cat(crayon::green("\tIncome.."))

  data.income <- data.table::fread(
    paste0(path, "/l_income.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  colnames(data.income) <- sub("^l_", "", colnames(data.income))

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
    paste0(path, "/l_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(l_hidp, l_tenure_dv, l_nkids_dv, l_hhsize, l_hhtype_dv,
                                   l_nch02_dv, l_nch34_dv, l_nch511_dv, l_nch1215_dv,
                                   l_fihhmngrs1_dv, l_fihhmnlabgrs_dv,
                                   l_fihhmnnet1_dv, l_fihhmnlabnet_dv, l_fihhmnsben_dv, l_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("l_hidp", "l_tenure_dv", "l_nkids_dv", "l_hhsize","l_hhtype_dv",
                         "l_nch02_dv", "l_nch34_dv", "l_nch511_dv", "l_nch1215_dv",
                         "l_fihhmngrs1_dv", "l_fihhmnlabgrs_dv",
                         "l_fihhmnnet1_dv", "l_fihhmnlabnet_dv", "l_fihhmnsben_dv","l_ieqmoecd_dv"),
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
    paste0(path, "/l_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","l_hidp",
                                           "l_imd2019qe_dv","l_imd2017qni_dv",
                                           "l_imd2020qs_dv","l_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","l_hidp",
                         "l_imd2019qe_dv","l_imd2017qni_dv",
                         "l_imd2020qs_dv","l_imd2019qw_dv"),
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

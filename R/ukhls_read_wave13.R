#' Read Understanding Society Wave 13
#'
#' Reads and performs basic cleaning operations on the UKHLS 13th wave (Jan 2021 - May 2023). Missing values as detailed below are all set to NA.
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
ukhls_read_wave13 <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
    full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 13 datasets")))

  # ==========================================
  # 1. Main Individual Response File
  # ==========================================

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/m_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[m_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, m_hidp, m_pno, m_psu, m_strata, m_istrtdaty, m_istrtdatm, m_istrtdatd)
  demographic_vars <- Hmisc::Cs(m_sex, m_dvage, m_birthy, m_gor_dv, m_urban_dv, m_mlstat, m_marstat, m_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(m_notempchk, m_empchk)
  econ_stat_vars   <- Hmisc::Cs(m_jbstat, m_jbhas, m_jboff, m_jboffy, m_jbterm1, m_jbterm2, m_jbsemp)
  income_vars      <- Hmisc::Cs(m_fimnnet_dv, m_fimngrs_dv,
                                m_fimnlabnet_dv, m_fimnlabgrs_dv, m_fimnmisc_dv, m_fimnprben_dv, m_fimninvnet_dv, m_fimnpen_dv, m_fimnsben_dv)
  work_vars        <- Hmisc::Cs(m_paygu_dv, m_payg_dv, m_jbhrs, m_seearngrs_dv, m_jbsic07_cc, m_jbot, m_jbotpd, m_jshrs,
                                m_jbnssec_dv, m_jbnssec3_dv, m_jbnssec5_dv, m_jbnssec8_dv, m_jbsize)
  benefits_vars    <- Hmisc::Cs(m_benbase1, m_benbase2, m_benbase3, m_benbase4, m_benbase96,
                                m_benctc)
  hhfinance_vars   <- Hmisc::Cs(m_fiyrdia, m_fiyrdb1, m_fiyrdb2, m_fiyrdb3, m_fiyrdb4, m_fiyrdb5, m_fiyrdb6, m_finnow, m_finfut)
  health_vars      <- Hmisc::Cs(m_health, m_aidhh, m_sclfsat1, m_sclfsato, m_sf12pcs_dv, m_sf12mcs_dv,
                                m_scsf1, m_scsf2a, m_scsf2b, m_scsf3a, m_scsf3b, m_scsf4a, m_scsf4b, m_scsf5, m_scsf6a,
                                m_scsf6b, m_scsf6c, m_scsf7,
                                m_scghq1_dv,m_scghq2_dv,
                                m_scwemwba, m_scwemwbb, m_scwemwbc, m_scwemwbd, m_scwemwbe, m_scwemwbf, m_scwemwbg, m_swemwbs_dv)
  health_cond_vars <- Hmisc::Cs(m_hcond1, m_hcond2, m_hcond3, m_hcond4, m_hcond5, m_hcond6, m_hcond7, m_hcond8, m_hcond10, m_hcond11, m_hcond12, m_hcond13, m_hcond14, m_hcond15, m_hcond16, m_hcond21,

                                m_hcondnew1, m_hcondnew2, m_hcondnew3, m_hcondnew4, m_hcondnew5, m_hcondnew6, m_hcondnew7, m_hcondnew8, m_hcondnew10, m_hcondnew11, m_hcondnew12, m_hcondnew13, m_hcondnew14, m_hcondnew15, m_hcondnew16, m_hcondnew21,

                                m_hconds01, m_hconds03, m_hconds04, m_hconds05, m_hconds08, m_hconds10, m_hconds11, m_hconds12, m_hconds15, m_hconds16, m_hconds21,
                                m_hconds23, m_hconds24, m_hconds26, m_hconds27, m_hconds28, m_hconds29, m_hconds30, m_hconds31, m_hconds33, m_hconds34, m_hconds35
  )
  health_care_vars <- Hmisc::Cs(m_hl2gp, m_hl2hop, m_hosp, m_hospd)
  smoke_vars       <- Hmisc::Cs(m_smoker, m_ncigs, m_giveup, m_gvupreas1, m_gvupreas2, m_gvupreas3, m_gvupreas4, m_gvupreas5, m_gvupreas6, m_gvupreas7, m_gvupreas8, m_gvupreas9, m_ecigs1)
  alc_vars         <- Hmisc::Cs(m_auditc1, m_auditc2, m_auditc3, m_auditc4, m_auditc5)
  weight_vars      <- Hmisc::Cs(m_indinus_lw, m_indinui_xw)

  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^m_", "", colnames(data))

  data[, wave_no := 13]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  # ==========================================
  # 2. Income File
  # ==========================================

  cat(crayon::green("\tIncome.."))

  data.income <- data.table::fread(
    paste0(path, "/m_income.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  colnames(data.income) <- sub("^m_", "", colnames(data.income))

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
    paste0(path, "/m_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(m_hidp, m_tenure_dv, m_nkids_dv, m_hhsize, m_hhtype_dv,
                                   m_nch02_dv, m_nch34_dv, m_nch511_dv, m_nch1215_dv,
                                   m_fihhmngrs1_dv, m_fihhmnlabgrs_dv,
                                   m_fihhmnnet1_dv, m_fihhmnlabnet_dv, m_fihhmnsben_dv, m_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("m_hidp", "m_tenure_dv", "m_nkids_dv", "m_hhsize","m_hhtype_dv",
                         "m_nch02_dv", "m_nch34_dv", "m_nch511_dv", "m_nch1215_dv",
                         "m_fihhmngrs1_dv", "m_fihhmnlabgrs_dv",
                         "m_fihhmnnet1_dv", "m_fihhmnlabnet_dv", "m_fihhmnsben_dv","m_ieqmoecd_dv"),
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
    paste0(path, "/m_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indalm_vars  <- colnames(data.indall[ , c("pidp","m_hidp",
                                            "m_imd2019qe_dv","m_imd2017qni_dv",
                                            "m_imd2020qs_dv","m_imd2019qw_dv")])

  data.indall <- data.indall[ , indalm_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","m_hidp",
                         "m_imd2019qe_dv","m_imd2017qni_dv",
                         "m_imd2020qs_dv","m_imd2019qw_dv"),
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

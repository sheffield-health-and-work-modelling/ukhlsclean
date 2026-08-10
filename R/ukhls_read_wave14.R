#' Read Understanding Society Wave 14
#'
#' Reads and performs basic cleaning operations on the UKHLS 14th wave (Jan 2022 - May 2024). Missing values as detailed below are all set to NA.
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
ukhls_read_wave14 <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
    full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 14 datasets")))

  # ==========================================
  # 1. Main Individual Response File
  # ==========================================

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/n_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[n_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, n_hidp, n_pno, n_psu, n_strata, n_istrtdaty, n_istrtdatm, n_istrtdatd)
  demographic_vars <- Hmisc::Cs(n_sex, n_dvage, n_birthy, n_gor_dv, n_urban_dv, n_mlstat, n_marstat, n_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(n_notempchk, n_empchk)
  econ_stat_vars   <- Hmisc::Cs(n_jbstat, n_jbhas, n_jboff, n_jboffy, n_jbterm1, n_jbterm2, n_jbsemp)
  income_vars      <- Hmisc::Cs(n_fimnnet_dv, n_fimngrs_dv,
                                n_fimnlabnet_dv, n_fimnlabgrs_dv, n_fimnmisc_dv, n_fimnprben_dv, n_fimninvnet_dv, n_fimnpen_dv, n_fimnsben_dv)
  work_vars        <- Hmisc::Cs(n_paygu_dv, n_payg_dv, n_jbhrs, n_seearngrs_dv, n_jbsic07_cc, n_jbot, n_jbotpd, n_jshrs,
                                n_jbnssec_dv, n_jbnssec3_dv, n_jbnssec5_dv, n_jbnssec8_dv, n_jbsize)
  benefits_vars    <- Hmisc::Cs(n_benbase1, n_benbase2, n_benbase3, n_benbase4, n_benbase96,
                                n_benctc)
  hhfinance_vars   <- Hmisc::Cs(n_fiyrdia, n_fiyrdb1, n_fiyrdb2, n_fiyrdb3, n_fiyrdb4, n_fiyrdb5, n_fiyrdb6, n_finnow, n_finfut)
  health_vars      <- Hmisc::Cs(n_health, n_aidhh, n_sclfsat1, n_sclfsato, n_sf12pcs_dv, n_sf12mcs_dv,
                                n_scsf1, n_scsf2a, n_scsf2b, n_scsf3a, n_scsf3b, n_scsf4a, n_scsf4b, n_scsf5, n_scsf6a,
                                n_scsf6b, n_scsf6c, n_scsf7,
                                n_scghq1_dv,n_scghq2_dv)
  health_cond_vars <- Hmisc::Cs(n_hcond1, n_hcond2, n_hcond3, n_hcond4, n_hcond5, n_hcond6, n_hcond7, n_hcond8, n_hcond10, n_hcond11, n_hcond12, n_hcond13, n_hcond14, n_hcond15, n_hcond16, n_hcond21,

                                n_hcondnew1, n_hcondnew2, n_hcondnew3, n_hcondnew4, n_hcondnew5, n_hcondnew6, n_hcondnew7, n_hcondnew8, n_hcondnew10, n_hcondnew11, n_hcondnew12, n_hcondnew13, n_hcondnew14, n_hcondnew15, n_hcondnew16, n_hcondnew21,

                                n_hconds01, n_hconds03, n_hconds04, n_hconds05, n_hconds08, n_hconds11, n_hconds12, n_hconds15, n_hconds16, n_hconds21
  )
  health_care_vars <- Hmisc::Cs(n_hl2gp, n_hl2hop, n_hosp, n_hospd)
  smoke_vars       <- Hmisc::Cs(n_smoker, n_ncigs, n_giveup, n_gvupreas1, n_gvupreas2, n_gvupreas3, n_gvupreas4, n_gvupreas5, n_gvupreas6, n_gvupreas7, n_gvupreas8, n_gvupreas9, n_ecigs1)
  alc_vars         <- NULL #Hmisc::Cs(n_auditc1, n_auditc2, n_auditc3, n_auditc4, n_auditc5)
  weight_vars      <- Hmisc::Cs(n_indinus_lw, n_inding2_xw)

  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^n_", "", colnames(data))

  data[, wave_no := 14]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  # ==========================================
  # 2. Income File
  # ==========================================

  cat(crayon::green("\tIncome.."))

  data.income <- data.table::fread(
    paste0(path, "/n_income.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  colnames(data.income) <- sub("^n_", "", colnames(data.income))

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
    paste0(path, "/n_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(n_hidp, n_tenure_dv, n_nkids_dv, n_hhsize, n_hhtype_dv,
                                   n_nch02_dv, n_nch34_dv, n_nch511_dv, n_nch1215_dv,
                                   n_fihhmngrs1_dv, n_fihhmnlabgrs_dv,
                                   n_fihhmnnet1_dv, n_fihhmnlabnet_dv, n_fihhmnsben_dv, n_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("n_hidp", "n_tenure_dv", "n_nkids_dv", "n_hhsize","n_hhtype_dv",
                         "n_nch02_dv", "n_nch34_dv", "n_nch511_dv", "n_nch1215_dv",
                         "n_fihhmngrs1_dv", "n_fihhmnlabgrs_dv",
                         "n_fihhmnnet1_dv", "n_fihhmnlabnet_dv", "n_fihhmnsben_dv","n_ieqmoecd_dv"),
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
    paste0(path, "/n_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indalm_vars  <- colnames(data.indall[ , c("pidp","n_hidp",
                                            "n_imd2019qe_dv","n_imd2017qni_dv",
                                            "n_imd2020qs_dv","n_imd2019qw_dv")])

  data.indall <- data.indall[ , indalm_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","n_hidp",
                         "n_imd2019qe_dv","n_imd2017qni_dv",
                         "n_imd2020qs_dv","n_imd2019qw_dv"),
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

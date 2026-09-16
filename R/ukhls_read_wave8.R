#' Read Understanding Society Wave 8
#'
#' Reads and performs basic cleaning operations on the UKHLS eighth wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave8 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 8 datasets")))

  # ==========================================
  # 1. Main Individual Response File
  # ==========================================

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/h_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
  # retain full interviews only
  data <- data[h_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp,pid, h_hidp, h_pno, h_psu, h_strata, h_istrtdaty, h_istrtdatm, h_istrtdatd, h_ivfio)
  demographic_vars <- Hmisc::Cs(h_sex, h_dvage, h_birthy, h_gor_dv, h_urban_dv, h_mlstat, h_marstat, h_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(h_notempchk, h_empchk)
  econ_stat_vars   <- Hmisc::Cs(h_jbstat, h_jbhas, h_jboff, h_jboffy, h_jbterm1, h_jbterm2, h_jbsemp, h_jbpen, h_jbpenm)
  income_vars      <- Hmisc::Cs(h_fimnnet_dv, h_fimngrs_dv,
                                h_fimnlabnet_dv, h_fimnlabgrs_dv, h_fimnmisc_dv, h_fimnprben_dv, h_fimninvnet_dv, h_fimnpen_dv, h_fimnsben_dv)
  work_vars        <- Hmisc::Cs(h_paygu_dv, h_payg_dv, h_jbhrs, h_seearngrs_dv, h_jbsic07_cc, h_jbot, h_jbotpd, h_jshrs,
                                h_jbnssec_dv, h_jbnssec3_dv, h_jbnssec5_dv, h_jbnssec8_dv, h_jbsize)
  benefits_vars    <- Hmisc::Cs(h_benbase1, h_benbase2, h_benbase3, h_benbase4, h_benbase96,
                                h_benctc)
  hhfinance_vars   <- Hmisc::Cs(h_fiyrdia, h_fiyrdb1, h_fiyrdb2, h_fiyrdb3, h_fiyrdb4, h_fiyrdb5, h_fiyrdb6, h_finnow, h_finfut)
  health_vars      <- Hmisc::Cs(h_health, h_aidhh, h_sclfsat1, h_sclfsato, h_sf12pcs_dv, h_sf12mcs_dv,
                                h_scsf1, h_scsf2a, h_scsf2b, h_scsf3a, h_scsf3b, h_scsf4a, h_scsf4b, h_scsf5, h_scsf6a, h_scsf6b,h_scsf6c, h_scsf7,
                                h_scghq1_dv,h_scghq2_dv)
  health_cond_vars <- Hmisc::Cs(h_hcond1, h_hcond2, h_hcond3, h_hcond4, h_hcond5, h_hcond6, h_hcond7, h_hcond8, h_hcond9, h_hcond10, h_hcond11, h_hcond12, h_hcond13, h_hcond14, h_hcond15, h_hcond16, h_hcond17,

                                h_hcondn1, h_hcondn2, h_hcondn3, h_hcondn4, h_hcondn5, h_hcondn6, h_hcondn7, h_hcondn8, h_hcondn9, h_hcondn10, h_hcondn11, h_hcondn12, h_hcondn13, h_hcondn14, h_hcondn15, h_hcondn16, h_hcondn17,

                                h_hconds01, h_hconds02, h_hconds03, h_hconds04, h_hconds05, h_hconds08, h_hconds09, h_hconds10, h_hconds11, h_hconds12, h_hconds13, h_hconds14, h_hconds15, h_hconds16, h_hconds17
  )
  health_care_vars <- Hmisc::Cs(h_hl2gp, h_hl2hop, h_hosp, h_hospd)
  smoke_vars       <- Hmisc::Cs(h_smoker, h_ncigs, h_ecigs)
  alc_vars         <- Hmisc::Cs(h_dklm, h_drnk4w, h_evralc, h_fivealcdr)
  weight_vars      <- Hmisc::Cs(h_indinus_lw, h_indinui_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^h_", "", colnames(data))

  data[, wave_no := 8]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  # ==========================================
  # 2. Income File
  # ==========================================

  cat(crayon::green("\tIncome.."))

  data.income <- data.table::fread(
    paste0(path, "/h_income.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  colnames(data.income) <- sub("^h_", "", colnames(data.income))

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
    paste0(path, "/h_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(h_hidp, h_tenure_dv, h_nkids_dv, h_hhsize, h_hhtype_dv,
                                   h_nch02_dv, h_nch34_dv, h_nch511_dv, h_nch1215_dv,
                                   h_fihhmngrs1_dv, h_fihhmnlabgrs_dv,
                                   h_fihhmnnet1_dv, h_fihhmnlabnet_dv, h_fihhmnsben_dv, h_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("h_hidp","h_tenure_dv","h_nkids_dv","h_hhsize","h_hhtype_dv",
                         "h_nch02_dv","h_nch34_dv","h_nch511_dv","h_nch1215_dv",
                         "h_fihhmngrs1_dv", "h_fihhmnlabgrs_dv",
                         "h_fihhmnnet1_dv", "h_fihhmnlabnet_dv", "h_fihhmnsben_dv","h_ieqmoecd_dv"),
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
    paste0(path, "/h_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","h_hidp",
                                            "h_imd2019qe_dv","h_imd2017qni_dv",
                                            "h_imd2020qs_dv","h_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","h_hidp",
                         "h_imd2019qe_dv","h_imd2017qni_dv",
                         "h_imd2020qs_dv","h_imd2019qw_dv"),
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

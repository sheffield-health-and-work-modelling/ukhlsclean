#' Read Understanding Society Wave 3
#'
#' Reads and performs basic cleaning operations on the UKHLS third wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave3 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {
  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 3 datasets")))

  # ==========================================
  # 1. Main Individual Response File
  # ==========================================

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/c_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[c_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, c_hidp, c_pno, c_psu, c_strata, c_istrtdaty, c_istrtdatm, c_istrtdatd)
  demographic_vars <- Hmisc::Cs(c_sex, c_dvage, c_birthy, c_gor_dv, c_urban_dv, c_mlstat, c_marstat, c_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(c_notempchk, c_empchk)
  econ_stat_vars   <- Hmisc::Cs(c_jbstat, c_jbhas, c_jboff, c_jboffy, c_jbterm1, c_jbterm2, c_jbsemp)
  income_vars      <- Hmisc::Cs(c_fimnnet_dv, c_fimngrs_dv,
                                c_fimnlabnet_dv, c_fimnlabgrs_dv, c_fimnmisc_dv, c_fimnprben_dv, c_fimninvnet_dv, c_fimnpen_dv, c_fimnsben_dv)
  work_vars        <- Hmisc::Cs(c_paygu_dv, c_payg_dv, c_jbhrs, c_seearngrs_dv, c_jbsic07_cc, c_jbot, c_jbotpd, c_jshrs,
                                c_jbnssec_dv, c_jbnssec3_dv, c_jbnssec5_dv, c_jbnssec8_dv, c_jbsize)
  benefits_vars    <- Hmisc::Cs(c_btype1, c_btype2, c_btype3, c_btype4, c_btype5, c_btype6, c_btype7, c_btype8, c_btype9, c_btype96,
                                c_benunemp1, c_benunemp2, c_benunemp3, c_benunemp96, c_bendis1, c_bendis2, c_bendis3, c_bendis4, c_bendis5, c_bendis6, c_bendis7,
                                c_bendis8, c_bendis9, c_bendis10, c_bendis12, c_bendis96, c_bendis97)
  hhfinance_vars   <- Hmisc::Cs(c_fiyrdia, c_fiyrdb1, c_fiyrdb2, c_fiyrdb3, c_fiyrdb4, c_fiyrdb5, c_fiyrdb6, c_finnow, c_finfut)
  health_care_vars  <- NULL
  health_vars      <- Hmisc::Cs(c_health, c_aidhh, c_sclfsat1, c_sclfsato, c_sf12pcs_dv, c_sf12mcs_dv,
                                c_scsf1, c_scsf2a, c_scsf2b, c_scsf3a, c_scsf3b, c_scsf4a, c_scsf4b, c_scsf5, c_scsf6a, c_scsf6b, c_scsf6c, c_scsf7,
                                c_scghq1_dv,c_scghq2_dv)
  health_cond_vars <- Hmisc::Cs(c_hcond1, c_hcond2, c_hcond3, c_hcond4, c_hcond5, c_hcond6, c_hcond7, c_hcond8, c_hcond9, c_hcond10, c_hcond11, c_hcond12, c_hcond13, c_hcond14, c_hcond15, c_hcond16, c_hcond17,

                                c_hcondn1, c_hcondn2, c_hcondn3, c_hcondn4, c_hcondn5, c_hcondn6, c_hcondn7, c_hcondn8, c_hcondn9, c_hcondn10, c_hcondn11, c_hcondn12, c_hcondn13, c_hcondn14, c_hcondn15, c_hcondn16, c_hcondn17,

                                c_hconds01, c_hconds02, c_hconds03, c_hconds04, c_hconds05, c_hconds08, c_hconds09, c_hconds10, c_hconds11, c_hconds12, c_hconds13, c_hconds14, c_hconds15, c_hconds16, c_hconds17
                                )
  smoke_vars       <- Hmisc::Cs(c_evrsmo, c_smofrq)
  alc_vars         <- Hmisc::Cs(c_dklm, c_drnk4w, c_evralc, c_fivealcdr)
  weight_vars      <- Hmisc::Cs(c_indinus_lw, c_indinub_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^c_", "", colnames(data))

  data[, wave_no := 3]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  # ==========================================
  # 2. Income File
  # ==========================================

  cat(crayon::green("\tIncome.."))

  data.income <- data.table::fread(
    paste0(path, "/c_income.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  colnames(data.income) <- sub("^c_", "", colnames(data.income))

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
    paste0(path, "/c_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(c_hidp, c_tenure_dv, c_numadult, c_nkids015, c_hhsize, c_hhtype_dv,
                                   c_nch02_dv, c_nch34_dv, c_nch511_dv, c_nch1215_dv,
                                   c_fihhmngrs1_dv, c_fihhmnlabgrs_dv,
                                   c_fihhmnnet1_dv, c_fihhmnlabnet_dv, c_fihhmnsben_dv, c_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("c_hidp","c_tenure_dv","c_numadult","c_nkids015","c_hhsize","c_hhtype_dv",
                         "c_nch02_dv","c_nch34_dv","c_nch511_dv","c_nch1215_dv",
                         "c_fihhmngrs1_dv", "c_fihhmnlabgrs_dv",
                         "c_fihhmnnet1_dv", "c_fihhmnlabnet_dv", "c_fihhmnsben_dv","c_ieqmoecd_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numadult","hh_numchild","hh_size","hh_type",
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
    paste0(path, "/c_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","c_hidp",
                                            "c_imd2019qe_dv","c_imd2017qni_dv",
                                            "c_imd2020qs_dv","c_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","c_hidp",
                         "c_imd2019qe_dv","c_imd2017qni_dv",
                         "c_imd2020qs_dv","c_imd2019qw_dv"),
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

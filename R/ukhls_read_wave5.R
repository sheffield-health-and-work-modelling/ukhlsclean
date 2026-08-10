#' Read Understanding Society Wave 5
#'
#' Reads and performs basic cleaning operations on the UKHLS fifth wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave5 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 5 datasets")))

  # ==========================================
  # 1. Main Individual Response File
  # ==========================================

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/e_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[e_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, e_hidp, e_pno, e_psu, e_strata, e_istrtdaty, e_istrtdatm, e_istrtdatd, e_ivfio)
  demographic_vars <- Hmisc::Cs(e_sex, e_dvage, e_birthy, e_gor_dv, e_urban_dv, e_mlstat, e_marstat, e_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(e_notempchk, e_empchk)
  econ_stat_vars   <- Hmisc::Cs(e_jbstat, e_jbhas, e_jboff, e_jboffy, e_jbterm1, e_jbterm2, e_jbsemp)
  income_vars      <- Hmisc::Cs(e_fimnnet_dv, e_fimngrs_dv,
                                e_fimnlabnet_dv, e_fimnlabgrs_dv, e_fimnmisc_dv, e_fimnprben_dv, e_fimninvnet_dv, e_fimnpen_dv, e_fimnsben_dv)
  work_vars        <- Hmisc::Cs(e_paygu_dv, e_payg_dv, e_jbhrs, e_seearngrs_dv, e_jbsic07_cc, e_jbot, e_jbotpd, e_jshrs,
                                e_jbnssec_dv, e_jbnssec3_dv, e_jbnssec5_dv, e_jbnssec8_dv, e_jbsize)
  benefits_vars    <- Hmisc::Cs(e_btype1, e_btype2, e_btype3, e_btype4, e_btype5, e_btype6, e_btype7, e_btype8, e_btype9, e_btype96,
                                e_benunemp1, e_benunemp2, e_benunemp3, e_benunemp96, e_bendis1, e_bendis11, e_bendis2, e_bendis3, e_bendis4, e_bendis5, e_bendis12,
                                e_bendis6, e_bendis7, e_bendis8, e_bendis9, e_bendis10, e_bendis96, e_bendis97)
  hhfinance_vars   <- Hmisc::Cs(e_fiyrdia, e_fiyrdb1, e_fiyrdb2, e_fiyrdb3, e_fiyrdb4, e_fiyrdb5, e_fiyrdb6, e_finnow, e_finfut)
  health_care_vars <- NULL
  health_vars      <- Hmisc::Cs(e_health, e_aidhh, e_sclfsat1, e_sclfsato, e_sf12pcs_dv, e_sf12mcs_dv,
                                e_scsf1, e_scsf2a, e_scsf2b, e_scsf3a, e_scsf3b, e_scsf4a, e_scsf4b, e_scsf5, e_scsf6a, e_scsf6b, e_scsf6c, e_scsf7,
                                e_scghq1_dv,e_scghq2_dv)
  health_cond_vars <- Hmisc::Cs(e_hcond1, e_hcond2, e_hcond3, e_hcond4, e_hcond5, e_hcond6, e_hcond7, e_hcond8, e_hcond9, e_hcond10, e_hcond11, e_hcond12, e_hcond13, e_hcond14, e_hcond15, e_hcond16, e_hcond17,

                                e_hcondn1, e_hcondn2, e_hcondn3, e_hcondn4, e_hcondn5, e_hcondn6, e_hcondn7, e_hcondn8, e_hcondn9, e_hcondn10, e_hcondn11, e_hcondn12, e_hcondn13, e_hcondn14, e_hcondn15, e_hcondn16, e_hcondn17,

                                e_hconds01, e_hconds02, e_hconds03, e_hconds04, e_hconds05, e_hconds08, e_hconds09, e_hconds10, e_hconds11, e_hconds12, e_hconds13, e_hconds14, e_hconds15, e_hconds16, e_hconds17
                                )
  smoke_vars       <- Hmisc::Cs(e_smever, e_smnow, e_ncigs, e_smcigs, e_smncigs, e_aglquit, e_smagbg)
  alc_vars         <- Hmisc::Cs(e_sceverdrnk, e_scfalcdrnk, e_scalcl7d, e_scnalcl7d, e_scnalcpint, e_scnalcshot, e_scnalcwine, e_scnalcpops)
  weight_vars      <- Hmisc::Cs(e_indinus_lw, e_indinub_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^e_", "", colnames(data))

  data[, wave_no := 5]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  # ==========================================
  # 2. Income File
  # ==========================================

  cat(crayon::green("\tIncome.."))

  data.income <- data.table::fread(
    paste0(path, "/e_income.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  colnames(data.income) <- sub("^e_", "", colnames(data.income))

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
    paste0(path, "/e_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(e_hidp, e_tenure_dv, e_nkids_dv, e_hhsize, e_hhtype_dv,
                                   e_nch02_dv, e_nch34_dv, e_nch511_dv, e_nch1215_dv,
                                   e_fihhmngrs1_dv, e_fihhmnlabgrs_dv,
                                   e_fihhmnnet1_dv, e_fihhmnlabnet_dv, e_fihhmnsben_dv, e_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("e_hidp","e_tenure_dv","e_nkids_dv","e_hhsize","e_hhtype_dv",
                         "e_nch02_dv","e_nch34_dv","e_nch511_dv","e_nch1215_dv",
                         "e_fihhmngrs1_dv", "e_fihhmnlabgrs_dv",
                         "e_fihhmnnet1_dv", "e_fihhmnlabnet_dv", "e_fihhmnsben_dv","e_ieqmoecd_dv"),
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
    paste0(path, "/e_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","e_hidp",
                                            "e_imd2019qe_dv","e_imd2017qni_dv",
                                            "e_imd2020qs_dv","e_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","e_hidp",
                         "e_imd2019qe_dv","e_imd2017qni_dv",
                         "e_imd2020qs_dv","e_imd2019qw_dv"),
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

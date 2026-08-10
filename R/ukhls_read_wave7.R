#' Read Understanding Society Wave 7
#'
#' Reads and performs basic cleaning operations on the UKHLS seventh wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave7 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 7 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/g_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[g_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, g_hidp, g_pno, g_psu, g_strata, g_istrtdaty, g_istrtdatm, g_istrtdatd)
  demographic_vars <- Hmisc::Cs(g_sex, g_dvage, g_birthy, g_gor_dv, g_urban_dv, g_mlstat, g_marstat, g_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(g_notempchk, g_empchk)
  econ_stat_vars   <- Hmisc::Cs(g_jbstat, g_jbhas, g_jboff, g_jboffy, g_jbterm1, g_jbterm2, g_jbsemp)
  income_vars      <- Hmisc::Cs(g_fimnnet_dv, g_fimngrs_dv,
                                g_fimnlabnet_dv, g_fimnlabgrs_dv, g_fimnmisc_dv, g_fimnprben_dv, g_fimninvnet_dv, g_fimnpen_dv, g_fimnsben_dv)
  work_vars        <- Hmisc::Cs(g_paygu_dv, g_payg_dv, g_jbhrs, g_seearngrs_dv, g_jbsic07_cc, g_jbot, g_jbotpd,
                                g_jbnssec_dv, g_jbnssec3_dv, g_jbnssec5_dv, g_jbnssec8_dv, g_jbsize)
  benefits_vars    <- Hmisc::Cs(g_benbase1, g_benbase2, g_benbase3, g_benbase4, g_benbase96,
                                g_benctc)
  hhfinance_vars   <- Hmisc::Cs(g_fiyrdia, g_fiyrdb1, g_fiyrdb2, g_fiyrdb3, g_fiyrdb4, g_fiyrdb5, g_fiyrdb6, g_finnow, g_finfut)
  education_vars   <- Hmisc::Cs(g_hiqual_dv)
  health_vars      <- Hmisc::Cs(g_health, g_aidhh, g_sclfsat1, g_sclfsato, g_sf12pcs_dv, g_sf12mcs_dv,
                                g_scsf1, g_scsf2a, g_scsf2b, g_scsf3a, g_scsf3b, g_scsf4a, g_scsf4b, g_scsf5, g_scsf6a, g_scsf6b, g_scsf6c, g_scsf7,
                                g_scghq1_dv,g_scghq2_dv,
                                g_scwemwba, g_scwemwbb, g_scwemwbc, g_scwemwbd, g_scwemwbe, g_scwemwbf, g_scwemwbg, g_swemwbs_dv)
  health_cond_vars <- Hmisc::Cs(g_hcond1, g_hcond2, g_hcond3, g_hcond4, g_hcond5, g_hcond6, g_hcond7, g_hcond8, g_hcond9, g_hcond10, g_hcond11, g_hcond12, g_hcond13, g_hcond14, g_hcond15, g_hcond16, g_hcond17,

                                g_hcondn1, g_hcondn2, g_hcondn3, g_hcondn4, g_hcondn5, g_hcondn6, g_hcondn7, g_hcondn8, g_hcondn9, g_hcondn10, g_hcondn11, g_hcondn12, g_hcondn13, g_hcondn14, g_hcondn15, g_hcondn16, g_hcondn17,

                                g_hconds01, g_hconds02, g_hconds03, g_hconds04, g_hconds05, g_hconds08, g_hconds09, g_hconds10, g_hconds11, g_hconds12, g_hconds13, g_hconds14, g_hconds15, g_hconds16, g_hconds17
  )
  health_care_vars <- Hmisc::Cs(g_hl2gp, g_hl2hop, g_hosp, g_hospd)
  smoke_vars       <- Hmisc::Cs(g_smoker, g_ncigs, g_ecigs)
  alc_vars         <- Hmisc::Cs(g_auditc1, g_auditc2, g_auditc3, g_auditc4, g_auditc5)
  weight_vars      <- Hmisc::Cs(g_indinus_lw, g_indinui_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^g_", "", colnames(data))

  data[, wave_no := 7]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/g_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(g_hidp, g_tenure_dv, g_nkids_dv, g_hhsize, g_hhtype_dv,
                                   g_nch02_dv, g_nch34_dv, g_nch511_dv, g_nch1215_dv,
                                   g_fihhmngrs1_dv, g_fihhmnlabgrs_dv,
                                   g_fihhmnnet1_dv, g_fihhmnlabnet_dv, g_fihhmnsben_dv, g_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("g_hidp","g_tenure_dv","g_nkids_dv","g_hhsize","g_hhtype_dv",
                         "g_nch02_dv","g_nch34_dv","g_nch511_dv","g_nch1215_dv",
                         "g_fihhmngrs1_dv", "g_fihhmnlabgrs_dv",
                         "g_fihhmnnet1_dv", "g_fihhmnlabnet_dv", "g_fihhmnsben_dv","g_ieqmoecd_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numchild","hh_size","hh_type",
                         "hh_numchild02","hh_numchild34","hh_numchild511","hh_numchild1215",
                         "hh_fihhmngrs1_dv", "hh_fihhmnlabgrs_dv",
                         "hh_fihhmnnet1_dv", "hh_fihhmnlabnet_dv", "hh_fihhmnsben_dv","ieqmoecd_dv"))

  hhold_merged <- merge(x = data,
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
    paste0(path, "/g_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","g_hidp",
                                            "g_imd2019qe_dv","g_imd2017qni_dv",
                                            "g_imd2020qs_dv","g_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","g_hidp",
                         "g_imd2019qe_dv","g_imd2017qni_dv",
                         "g_imd2020qs_dv","g_imd2019qw_dv"),
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

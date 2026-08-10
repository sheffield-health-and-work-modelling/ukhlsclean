#' Read Understanding Society Wave 15
#'
#' Reads and performs basic cleaning operations on the UKHLS 14th wave (Jan 2023 - May 2025). Missing values as detailed below are all set to NA.
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
ukhls_read_wave15 <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/USoc/Data/SN6614_2026_02_20/tab/ukhls/",
    full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 15 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/o_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[o_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, o_hidp, o_pno, o_psu, o_strata, o_istrtdaty, o_istrtdatm, o_istrtdatd)
  demographic_vars <- Hmisc::Cs(o_sex, o_dvage, o_birthy, o_gor_dv, o_urban_dv, o_mlstat, o_marstat, o_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(o_notempchk, o_empchk)
  econ_stat_vars   <- Hmisc::Cs(o_jbstat, o_jbhas, o_jboff, o_jboffy, o_jbterm1, o_jbterm2, o_jbsemp)
  income_vars      <- Hmisc::Cs(o_fimnnet_dv, o_fimngrs_dv,
                                o_fimnlabnet_dv, o_fimnlabgrs_dv, o_fimnmisc_dv, o_fimnprben_dv, o_fimninvnet_dv, o_fimnpen_dv, o_fimnsben_dv)
  work_vars        <- Hmisc::Cs(o_paygu_dv, o_payg_dv, o_jbhrs, o_seearngrs_dv, o_jbsic07_cc, o_jbot, o_jbotpd, o_jshrs,
                                o_jbnssec_dv, o_jbnssec3_dv, o_jbnssec5_dv, o_jbnssec8_dv, o_jbsize)
  benefits_vars    <- Hmisc::Cs(o_benbase1, o_benbase2, o_benbase3, o_benbase4, o_benbase96,
                                o_benctc)
  hhfinance_vars   <- Hmisc::Cs(o_fiyrdia, o_fiyrdb1, o_fiyrdb2, o_fiyrdb3, o_fiyrdb4, o_fiyrdb5, o_fiyrdb6, o_finnow, o_finfut)
  health_vars      <- Hmisc::Cs(o_health, o_aidhh, o_sclfsat1, o_sclfsato, o_sf12pcs_dv, o_sf12mcs_dv,
                                o_scsf1, o_scsf2a, o_scsf2b, o_scsf3a, o_scsf3b, o_scsf4a, o_scsf4b, o_scsf5, o_scsf6a,
                                o_scsf6b, o_scsf6c, o_scsf7,
                                o_scghq1_dv,o_scghq2_dv)
  health_cond_vars <- Hmisc::Cs(o_hcond1, o_hcond2, o_hcond3, o_hcond4, o_hcond5, o_hcond6, o_hcond7, o_hcond8, o_hcond10, o_hcond11, o_hcond12, o_hcond13, o_hcond14, o_hcond15, o_hcond16, o_hcond21,

                                o_hcondnew1, o_hcondnew2, o_hcondnew3, o_hcondnew4, o_hcondnew5, o_hcondnew6, o_hcondnew7, o_hcondnew8, o_hcondnew10, o_hcondnew11, o_hcondnew12, o_hcondnew13, o_hcondnew14, o_hcondnew15, o_hcondnew16, o_hcondnew21,

                                o_hconds01, o_hconds03, o_hconds04, o_hconds05, o_hconds08, o_hconds11, o_hconds12, o_hconds15, o_hconds16, o_hconds21
  )
  health_care_vars <- Hmisc::Cs(o_hl2gp, o_hl2hop, o_hosp, o_hospd)
  smoke_vars       <- NULL #Hmisc::Cs(o_smoker, o_ncigs, o_giveup, o_gvupreas1, o_gvupreas2, o_gvupreas3, o_gvupreas4, o_gvupreas5, o_gvupreas6, o_gvupreas7, o_gvupreas8, o_gvupreas9, o_ecigs1)
  alc_vars         <- NULL #Hmisc::Cs(o_auditc1, o_auditc2, o_auditc3, o_auditc4, o_auditc5)
  weight_vars      <- Hmisc::Cs(o_indinus_lw, o_inding2_xw)

  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^o_", "", colnames(data))

  data[, wave_no := 15]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/o_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(o_hidp, o_tenure_dv, o_nkids_dv, o_hhsize, o_hhtype_dv,
                                   o_nch02_dv, o_nch34_dv, o_nch511_dv, o_nch1215_dv,
                                   o_fihhmngrs1_dv, o_fihhmnlabgrs_dv,
                                   o_fihhmnnet1_dv, o_fihhmnlabnet_dv, o_fihhmnsben_dv, o_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("o_hidp", "o_tenure_dv", "o_nkids_dv", "o_hhsize","o_hhtype_dv",
                         "o_nch02_dv", "o_nch34_dv", "o_nch511_dv", "o_nch1215_dv",
                         "o_fihhmngrs1_dv", "o_fihhmnlabgrs_dv",
                         "o_fihhmnnet1_dv", "o_fihhmnlabnet_dv", "o_fihhmnsben_dv","o_ieqmoecd_dv"),
                       # new names
                       c("hidp", "hh_tenure", "hh_numchild", "hh_size", "hh_type",
                         "hh_numchild02", "hh_numchild34", "hh_numchild511", "hh_numchild1215",
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
    paste0(path, "/o_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indalm_vars  <- colnames(data.indall[ , c("pidp","o_hidp",
                                            "o_imd2019qe_dv","o_imd2017qni_dv",
                                            "o_imd2020qs_dv","o_imd2019qw_dv")])

  data.indall <- data.indall[ , indalm_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","o_hidp",
                         "o_imd2019qe_dv","o_imd2017qni_dv",
                         "o_imd2020qs_dv","o_imd2019qw_dv"),
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

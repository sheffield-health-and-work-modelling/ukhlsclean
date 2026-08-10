#' Read Understanding Society Wave 2
#'
#' Reads and performs basic cleaning operations on the UKHLS second wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave2 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 2 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/b_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[b_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, b_hidp, b_pno, b_psu, b_strata, b_istrtdaty, b_istrtdatm, b_istrtdatd)
  demographic_vars <- Hmisc::Cs(b_sex, b_dvage, b_birthy, b_gor_dv, b_urban_dv, b_mlstat, b_marstat, b_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(b_notempchk, b_empchk)
  econ_stat_vars   <- Hmisc::Cs(b_jbstat, b_jbhas, b_jboff, b_jboffy, b_jbterm1, b_jbterm2, b_jbsemp, b_jbpen, b_jbpenm)
  income_vars      <- Hmisc::Cs(b_fimnnet_dv, b_fimngrs_dv,
                                b_fimnlabnet_dv, b_fimnlabgrs_dv, b_fimnmisc_dv, b_fimnprben_dv, b_fimninvnet_dv, b_fimnpen_dv, b_fimnsben_dv)
  work_vars        <- Hmisc::Cs(b_paygu_dv, b_payg_dv, b_jbhrs, b_seearngrs_dv, b_jbsic07_cc, b_jbot, b_jbotpd, b_jshrs,
                                b_jbnssec_dv, b_jbnssec3_dv, b_jbnssec5_dv, b_jbnssec8_dv, b_jbsize)
  benefits_vars    <- Hmisc::Cs(b_btype1, b_btype2, b_btype3, b_btype4, b_btype5, b_btype6, b_btype7, b_btype8, b_btype9, b_btype96,
                                b_benunemp1, b_benunemp2, b_benunemp96, b_bendis1, b_bendis2, b_bendis3, b_bendis4, b_bendis5, b_bendis6, b_bendis7,
                                b_bendis8, b_bendis9, b_bendis10, b_bendis11, b_bendis96)
  hhfinance_vars   <- NULL
  health_care_vars <- NULL
  health_cond_vars <- NULL
  health_vars      <- Hmisc::Cs(b_health, b_aidhh, b_sclfsat1, b_sclfsato, b_sf12pcs_dv, b_sf12mcs_dv,
                                b_scsf1, b_scsf2a, b_scsf2b, b_scsf3a, b_scsf3b, b_scsf4a, b_scsf4b, b_scsf5, b_scsf6a, b_scsf6b, b_scsf6c, b_scsf7,
                                b_scghq1_dv,b_scghq2_dv)
  smoke_vars       <- Hmisc::Cs(b_smever, b_smnow, b_ncigs, b_smcigs, b_smncigs, b_aglquit, b_smagbg)
  alc_vars         <- Hmisc::Cs(b_sceverdrnk, b_scfalcdrnk, b_scalcl7d, b_scnalcl7d, b_scnalcpint, b_scnalcshot, b_scnalcwine, b_scnalcpops)
  weight_vars      <- Hmisc::Cs(b_indinus_lw, b_indinub_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^b_", "", colnames(data))

  data[, wave_no := 2]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/b_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(b_hidp, b_tenure_dv, b_numadult, b_nkids015, b_hhsize, b_hhtype_dv,
                                   b_nch02_dv, b_nch34_dv, b_nch511_dv, b_nch1215_dv,
                                   b_fihhmngrs1_dv, b_fihhmnlabgrs_dv,
                                   b_fihhmnnet1_dv, b_fihhmnlabnet_dv, b_fihhmnsben_dv, b_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("b_hidp","b_tenure_dv","b_numadult","b_nkids015","b_hhsize","b_hhtype_dv",
                         "b_nch02_dv","b_nch34_dv","b_nch511_dv","b_nch1215_dv",
                         "b_fihhmngrs1_dv", "b_fihhmnlabgrs_dv",
                         "b_fihhmnnet1_dv", "b_fihhmnlabnet_dv", "b_fihhmnsben_dv","b_ieqmoecd_dv"),
                       # new names
                       c("hidp","hh_tenure","hh_numadult","hh_numchild","hh_size","hh_type",
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
    paste0(path, "/b_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","b_hidp",
                                            "b_imd2019qe_dv","b_imd2017qni_dv",
                                            "b_imd2020qs_dv","b_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","b_hidp",
                         "b_imd2019qe_dv","b_imd2017qni_dv",
                         "b_imd2020qs_dv","b_imd2019qw_dv"),
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

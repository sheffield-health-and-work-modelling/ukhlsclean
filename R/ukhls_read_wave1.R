#' Read Understanding Society Wave 1
#'
#' Reads and performs basic cleaning operations on the UKHLS first wave. Missing values as detailed below are all set to NA.
#'
#' A sample of the population living in private households. All persons living in the household, including those
#' under 2 years were eligible for inclusion. At addresses where there were more than two children under 16,
#' two children were selected at random. Information was obtained directly from persons aged 13 and
#' over. Information about children aged 0-12 was obtained from a parent, with the child present.
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
ukhls_read_wave1 <- function(
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 1 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/a_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[a_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, a_hidp, a_pno, a_psu, a_strata, a_istrtdaty, a_istrtdatm, a_istrtdatd)
  demographic_vars <- Hmisc::Cs(a_sex, a_dvage, a_birthy, a_gor_dv, a_urban_dv, a_mlstat, a_marstat, a_hiqual_dv)
  prev_wave_vars   <- NULL
  econ_stat_vars   <- Hmisc::Cs(a_jbstat,a_jbhas,a_jboff,a_jboffy, a_jbterm1, a_jbterm2, a_jbsemp, a_jbpen, a_jbpenm)
  income_vars      <- Hmisc::Cs(a_fimnnet_dv, a_fimngrs_dv,
                                a_fimnlabnet_dv, a_fimnlabgrs_dv, a_fimnmisc_dv, a_fimnprben_dv, a_fimninvnet_dv, a_fimnpen_dv, a_fimnsben_dv)
  work_vars        <- Hmisc::Cs(a_paygu_dv, a_payg_dv, a_jbhrs, a_seearngrs_dv, a_jbsic07_cc, a_jbot, a_jbotpd, a_jshrs,
                                a_jbnssec_dv, a_jbnssec3_dv, a_jbnssec5_dv, a_jbnssec8_dv, a_jbsize)
  benefits_vars    <- Hmisc::Cs(a_btype1, a_btype2, a_btype3, a_btype4, a_btype5, a_btype6, a_btype7, a_btype8, a_btype9, a_btype96,
                                a_benunemp1, a_benunemp2, a_benunemp96, a_bendis1, a_bendis2, a_bendis3, a_bendis4, a_bendis5, a_bendis6, a_bendis7,
                                a_bendis8, a_bendis9, a_bendis10, a_bendis11, a_bendis96)
  hhfinance_vars   <- NULL
  health_care_vars <- NULL
  health_vars      <- Hmisc::Cs(a_health, a_aidhh, a_sclfsat1, a_sclfsato, a_sf12pcs_dv, a_sf12mcs_dv,
                                a_sf1, a_sf2a, a_sf2b, a_sf3a, a_sf3b, a_sf4a, a_sf4b, a_sf5, a_sf6a, a_sf6b, a_sf6c, a_sf7,
                                a_scghq1_dv, a_scghq2_dv,
                                a_scwemwba, a_scwemwbb, a_scwemwbc, a_scwemwbd, a_scwemwbe, a_scwemwbf, a_scwemwbg, a_swemwbs_dv)
  health_cond_vars <- Hmisc::Cs(a_hcond1, a_hcond2, a_hcond3, a_hcond4, a_hcond5, a_hcond6, a_hcond7, a_hcond8, a_hcond9, a_hcond10, a_hcond11, a_hcond12, a_hcond13, a_hcond14, a_hcond15, a_hcond16, a_hcond17,

                                a_hconds01, a_hconds02, a_hconds03, a_hconds04, a_hconds05, a_hconds06, a_hconds07, a_hconds08, a_hconds09, a_hconds10, a_hconds11, a_hconds12, a_hconds13, a_hconds14, a_hconds15, a_hconds16, a_hconds17
                                )
  smoke_vars       <- NULL
  alc_vars         <- NULL
  weight_vars      <- Hmisc::Cs(a_indinus_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^a_", "", colnames(data))


  data[, wave_no := 1]
  data[, bhps_sample := FALSE]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/a_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(a_hidp, a_tenure_dv, a_numadult, a_numchild, a_hhsize, a_hhtype_dv,
                                   a_nch02_dv, a_nch34_dv, a_nch511_dv, a_nch1215_dv,
                                   a_fihhmngrs1_dv, a_fihhmnlabgrs_dv,
                                   a_fihhmnnet1_dv, a_fihhmnlabnet_dv, a_fihhmnsben_dv, a_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("a_hidp","a_tenure_dv","a_numadult","a_numchild","a_hhsize","a_hhtype_dv",
                         "a_nch02_dv","a_nch34_dv","a_nch511_dv","a_nch1215_dv",
                         "a_fihhmngrs1_dv", "a_fihhmnlabgrs_dv",
                         "a_fihhmnnet1_dv", "a_fihhmnlabnet_dv", "a_fihhmnsben_dv","a_ieqmoecd_dv"),
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

  ## Combine - keep all observations in the main data and drop excess xwave observations

  xwave_merged <- merge(x = hhold_merged,
                        y = data.xwave,
                                   by="pidp",
                                   all.x=TRUE,
                                   all.y=FALSE)

  ####################################################
  #### ADD IN THE INDALL DATA ########################

  cat(crayon::green("\tIndall..."))

  data.indall <- data.table::fread(
    paste0(path, "/a_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","a_hidp",
                                           "a_imd2019qe_dv","a_imd2017qni_dv",
                                           "a_imd2020qs_dv","a_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","a_hidp",
                         "a_imd2019qe_dv","a_imd2017qni_dv",
                         "a_imd2020qs_dv","a_imd2019qw_dv"),
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

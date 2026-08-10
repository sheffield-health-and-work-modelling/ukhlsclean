#' Read Understanding Society Wave 4
#'
#' Reads and performs basic cleaning operations on the UKHLS fourth wave. Missing values as detailed below are all set to NA.
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
ukhls_read_wave4 <- function(
    #root = c("C:/"),
    #file = "Users/damon/OneDrive/Documents/USoc/Datasets/Main data/tab/ukhls"
  root = c("X:/"),
  file = "HAR_PR/PR/USoc/Data/SN6614_2024_12_02/tab/ukhls/",
  full = TRUE
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Wave 4 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/d_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[d_ivfio==1,]
  }

  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, d_hidp, d_pno, d_psu, d_strata, d_istrtdaty, d_istrtdatm, d_istrtdatd)
  demographic_vars <- Hmisc::Cs(d_sex, d_dvage, d_birthy, d_gor_dv, d_urban_dv, d_mlstat,  d_marstat, d_hiqual_dv)
  prev_wave_vars   <- Hmisc::Cs(d_notempchk, d_empchk)
  econ_stat_vars   <- Hmisc::Cs(d_jbstat, d_jbhas, d_jboff, d_jboffy, d_jbterm1, d_jbterm2, d_jbsemp, d_jbpen, d_jbpenm)
  income_vars      <- Hmisc::Cs(d_fimnnet_dv, d_fimngrs_dv,
                                d_fimnlabnet_dv, d_fimnlabgrs_dv, d_fimnmisc_dv, d_fimnprben_dv, d_fimninvnet_dv, d_fimnpen_dv, d_fimnsben_dv)
  work_vars        <- Hmisc::Cs(d_paygu_dv, d_payg_dv, d_jbhrs, d_seearngrs_dv, d_jbsic07_cc, d_jbot, d_jbotpd,
                                d_jbnssec_dv, d_jbnssec3_dv, d_jbnssec5_dv, d_jbnssec8_dv, d_jbsize)
  benefits_vars    <- Hmisc::Cs(d_btype1, d_btype2, d_btype3, d_btype4, d_btype5, d_btype6, d_btype7, d_btype8, d_btype9, d_btype96,
                                d_benunemp1, d_benunemp2, d_benunemp3, d_benunemp96, d_bendis1, d_bendis11, d_bendis2, d_bendis3, d_bendis4, d_bendis5, d_bendis12,
                                d_bendis6, d_bendis7, d_bendis8, d_bendis9, d_bendis10, d_bendis96, d_bendis97)
  hhfinance_vars   <- Hmisc::Cs(d_fiyrdia, d_fiyrdb1, d_fiyrdb2, d_fiyrdb3, d_fiyrdb4, d_fiyrdb5, d_fiyrdb6, d_finnow, d_finfut)
  health_care_vars  <- NULL
  health_vars      <- Hmisc::Cs(d_health, d_aidhh, d_sclfsat1, d_sclfsato, d_sf12pcs_dv, d_sf12mcs_dv,
                                d_scsf1, d_scsf2a, d_scsf2b, d_scsf3a, d_scsf3b, d_scsf4a, d_scsf4b, d_scsf5, d_scsf6a, d_scsf6b, d_scsf6c, d_scsf7,
                                d_scghq1_dv,d_scghq2_dv,
                                d_scwemwba, d_scwemwbb, d_scwemwbc, d_scwemwbd, d_scwemwbe, d_scwemwbf, d_scwemwbg, d_swemwbs_dv)
  health_cond_vars <- Hmisc::Cs(d_hcond1, d_hcond2, d_hcond3, d_hcond4, d_hcond5, d_hcond6, d_hcond7, d_hcond8, d_hcond9, d_hcond10, d_hcond11, d_hcond12, d_hcond13, d_hcond14, d_hcond15, d_hcond16, d_hcond17,

                                d_hcondn1, d_hcondn2, d_hcondn3, d_hcondn4, d_hcondn5, d_hcondn6, d_hcondn7, d_hcondn8, d_hcondn9, d_hcondn10, d_hcondn11, d_hcondn12, d_hcondn13, d_hcondn14, d_hcondn15, d_hcondn16, d_hcondn17,

                                d_hconds01, d_hconds02, d_hconds03, d_hconds04, d_hconds05, d_hconds08, d_hconds09, d_hconds10, d_hconds11, d_hconds12, d_hconds13, d_hconds14, d_hconds15, d_hconds16, d_hconds17
                                )
  smoke_vars       <- Hmisc::Cs(d_evrsmo, d_smofrq)
  alc_vars         <- Hmisc::Cs(d_dklm, d_drnk4w, d_evralc, d_fivealcdr)
  weight_vars      <- Hmisc::Cs(d_indinus_lw, d_indinub_xw)


  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars,
             income_vars, work_vars, benefits_vars, hhfinance_vars,
             health_vars, health_care_vars, health_cond_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  ## remove wave-specific prefix
  colnames(data) <- sub("^d_", "", colnames(data))

  data[, wave_no := 4]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/d_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(d_hidp, d_tenure_dv, d_numadult, d_nkids015, d_hhsize, d_hhtype_dv,
                                   d_nch02_dv, d_nch34_dv, d_nch511_dv, d_nch1215_dv,
                                   d_fihhmngrs1_dv, d_fihhmnlabgrs_dv,
                                   d_fihhmnnet1_dv, d_fihhmnlabnet_dv, d_fihhmnsben_dv, d_ieqmoecd_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("d_hidp","d_tenure_dv","d_numadult","d_nkids015","d_hhsize","d_hhtype_dv",
                         "d_nch02_dv","d_nch34_dv","d_nch511_dv","d_nch1215_dv",
                         "d_fihhmngrs1_dv", "d_fihhmnlabgrs_dv",
                         "d_fihhmnnet1_dv", "d_fihhmnlabnet_dv", "d_fihhmnsben_dv","d_ieqmoecd_dv"),
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
    paste0(path, "/d_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","d_hidp",
                                            "d_imd2019qe_dv","d_imd2017qni_dv",
                                            "d_imd2020qs_dv","d_imd2019qw_dv")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","d_hidp",
                         "d_imd2019qe_dv","d_imd2017qni_dv",
                         "d_imd2020qs_dv","d_imd2019qw_dv"),
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

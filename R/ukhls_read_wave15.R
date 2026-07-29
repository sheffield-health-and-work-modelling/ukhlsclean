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
  demographic_vars <- Hmisc::Cs(o_sex, o_dvage, o_birthy, o_gor_dv, o_urban_dv, o_mlstat, o_marstat)
  prev_wave_vars   <- Hmisc::Cs(o_notempchk, o_empchk)
  econ_stat_vars   <- Hmisc::Cs(o_jbstat, o_jbhas, o_jboff, o_jboffy, o_jbterm1, o_jbterm2, o_jbsemp)
  income_vars      <- Hmisc::Cs(o_fimnnet_dv, o_fimngrs_dv,
                                o_fimnlabnet_dv, o_fimnlabgrs_dv, o_fimnmisc_dv, o_fimnprben_dv, o_fimninvnet_dv, o_fimnpen_dv, o_fimnsben_dv)
  work_vars        <- Hmisc::Cs(o_paygu_dv, o_payg_dv, o_jbhrs, o_fimnlabgrs_dv, o_seearngrs_dv, o_jbsic07_cc, o_jbot, o_jbotpd,
                                o_jbnssec_dv, o_jbnssec3_dv, o_jbnssec5_dv, o_jbnssec8_dv, o_jbsize)
  employees_vars   <- Hmisc::Cs(o_paygl, o_paynl, o_payu, o_payug, o_paytyp, o_ovtpay, o_pvtpyset, o_extrate, o_basnset, o_basrate, o_ovtnset, o_ovtrate)
  s.emp_vars       <- Hmisc::Cs(o_jshrs, o_jspayu, o_jspytx, o_jspyni)
  non.emp_vars     <- Hmisc::Cs(o_jbhad)
  job2_vars        <- NULL #Hmisc::Cs(o_j2has, o_j2semp, o_j2hrs, o_j2pay)
  benefits_vars    <- Hmisc::Cs(o_benbase1, o_benbase2, o_benbase3, o_benbase4, o_benbase96,
                                o_benctc)
  pension_vars     <- Hmisc::Cs(o_benpen1, o_benpen2, o_benpen3, o_benpen4, o_benpen5, o_benpen6, o_benpen7, o_benpen8, o_benpen96)
  bendis_vars      <- Hmisc::Cs(o_bendis1, o_bendis2, o_bendis3, o_bendis4, o_bendis5, o_bendis12,
                                o_bendis7, o_bendis8, o_bendis10, o_bendis97, o_bendis96)
  otherben_vars    <- Hmisc::Cs(o_benesa,
                                o_othben1, o_othben2, o_othben5, o_othben6, o_othben7, o_othben8, o_othben9, o_othben97, o_othben96)
  benincome_vars   <- Hmisc::Cs(o_bensta2, o_bensta3, o_bensta4, o_bensta5, o_bensta6, o_bensta7, o_bensta97, o_bensta96)
  hhfinance_vars   <- Hmisc::Cs(o_fiyrdia, o_fiyrdb1, o_fiyrdb2, o_fiyrdb3, o_fiyrdb4, o_fiyrdb5, o_fiyrdb6, o_finnow, o_finfut)
  education_vars   <- Hmisc::Cs(o_hiqual_dv)
  health_vars      <- Hmisc::Cs(o_health, o_aidhh, o_sclfsat1, o_sclfsato, o_sf12pcs_dv, o_sf12mcs_dv,
                                o_scsf1, o_scsf2a, o_scsf2b, o_scsf3a, o_scsf3b, o_scsf4a, o_scsf4b, o_scsf5, o_scsf6a,
                                o_scsf6b, o_scsf6c, o_scsf7,
                                o_scghq1_dv,o_scghq2_dv)
  health_cond_vars <- Hmisc::Cs(o_hcond1, o_hcond2, o_hcond3, o_hcond4, o_hcond5, o_hcond6, o_hcond7, o_hcond8, o_hcond10, o_hcond11, o_hcond12, o_hcond13, o_hcond14, o_hcond15, o_hcond16, o_hcond21,

                                o_hcondnew1, o_hcondnew2, o_hcondnew3, o_hcondnew4, o_hcondnew5, o_hcondnew6, o_hcondnew7, o_hcondnew8, o_hcondnew10, o_hcondnew11, o_hcondnew12, o_hcondnew13, o_hcondnew14, o_hcondnew15, o_hcondnew16, o_hcondnew21,

                                o_hconds01, o_hconds03, o_hconds04, o_hconds05, o_hconds08, o_hconds11, o_hconds12, o_hconds15, o_hconds16, o_hconds21
  )
  health_care_vars <- Hmisc::Cs(o_hl2gp, o_hl2hop, o_hosp, o_hospd)
  preg_vars        <- Hmisc::Cs(o_preg,
                                o_pregout1, o_pregend1, o_pregsmoke1, o_smkmnth11, o_smkmnth21, o_smkmnth31, o_pregsmk11, o_pregsmk21, o_pregsmk31, o_aedrof1, o_aepuwk1, o_aepuda1, o_lchmulti1,
                                o_pregout2, o_pregend2, o_pregsmoke2, o_smkmnth12, o_smkmnth22, o_smkmnth32, o_pregsmk12, o_pregsmk22, o_pregsmk32, o_aedrof2, o_aepuwk2, o_aepuda2, o_lchmulti2,
                                o_pregout3, o_pregend3, o_pregsmoke3, o_smkmnth13, o_smkmnth23, o_smkmnth33, o_pregsmk13, o_pregsmk23, o_pregsmk33, o_aedrof3, o_aepuwk3, o_aepuda3, o_lchmulti3)
  smoke_vars       <- NULL #Hmisc::Cs(o_smoker, o_ncigs, o_giveup, o_gvupreas1, o_gvupreas2, o_gvupreas3, o_gvupreas4, o_gvupreas5, o_gvupreas6, o_gvupreas7, o_gvupreas8, o_gvupreas9, o_ecigs1)
  alc_vars         <- NULL #Hmisc::Cs(o_auditc1, o_auditc2, o_auditc3, o_auditc4, o_auditc5)
  weight_vars      <- Hmisc::Cs(o_indinus_lw, o_inding2_xw)

  names <- c(id_vars, demographic_vars, prev_wave_vars, econ_stat_vars, income_vars, work_vars, employees_vars,
             s.emp_vars, non.emp_vars, job2_vars, benefits_vars, pension_vars, bendis_vars, otherben_vars,
             benincome_vars, hhfinance_vars, education_vars, health_vars, health_care_vars, health_cond_vars, preg_vars, smoke_vars, alc_vars, weight_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","o_hidp","o_pno","o_psu","o_strata","o_istrtdaty","o_istrtdatm","o_istrtdatd",
                         ## demographic
                         "o_sex","o_dvage","o_birthy","o_gor_dv","o_urban_dv","o_mlstat","o_marstat",
                         ## previous wave variables
                         "o_notempchk","o_empchk",
                         ## economic status
                         "o_jbstat","o_jbhas","o_jboff","o_jboffy","o_jbterm1","o_jbterm2","o_jbsemp",
                         ## income variables
                         "o_fimnnet_dv", "o_fimngrs_dv",
                         "o_fimnlabnet_dv", "o_fimnlabgrs_dv", "o_fimnmisc_dv", "o_fimnprben_dv", "o_fimninvnet_dv", "o_fimnpen_dv", "o_fimnsben_dv",
                         ## work variables
                         "o_paygu_dv","o_payg_dv","o_jbhrs","o_fimnlabgrs_dv","o_seearngrs_dv","o_jbsic07_cc","o_jbot","o_jbotpd",
                         "o_jbnssec_dv","o_jbnssec3_dv","o_jbnssec5_dv","o_jbnssec8_dv", "o_jbsize",
                         ## employees
                         "o_paygl","o_paynl","o_payu","o_payug","o_paytyp","o_ovtpay","o_pvtpyset","o_extrate","o_basnset","o_basrate",
                         "o_ovtnset","o_ovtrate",
                         ## self-employed
                         "o_jshrs","o_jspayu","o_jspytx","o_jspyni",
                         ## non-employed
                         "o_jbhad",
                         ## second job

                         ## benefits
                         "o_benbase1","o_benbase2","o_benbase3","o_benbase4","o_benbase96",
                         "o_benctc",
                         ## pensions
                         "o_benpen1","o_benpen2","o_benpen3","o_benpen4","o_benpen5","o_benpen6","o_benpen7","o_benpen8","o_benpen96",
                         ## disability benefits
                         "o_bendis1","o_bendis2","o_bendis3","o_bendis4","o_bendis5","o_bendis12",
                         "o_bendis7","o_bendis8","o_bendis10","o_bendis97","o_bendis96",
                         ## other benefits
                         "o_benesa",
                         "o_othben1","o_othben2","o_othben5","o_othben6","o_othben7","o_othben8","o_othben9","o_othben97","o_othben96",
                         ## benefit income variables (formerly receivables)
                         "o_bensta2","o_bensta3","o_bensta4","o_bensta5","o_bensta6","o_bensta7","o_bensta97","o_bensta96",
                         ## household finance variables (interest and dividends)
                         "o_fiyrdia","o_fiyrdb1","o_fiyrdb2","o_fiyrdb3","o_fiyrdb4","o_fiyrdb5","o_fiyrdb6","o_finnow","o_finfut",
                         ## education variables
                         "o_hiqual_dv",
                         ## health variables
                         "o_health","o_aidhh","o_sclfsat1","o_sclfsato","o_sf12pcs_dv","o_sf12mcs_dv",
                         "o_scsf1","o_scsf2a","o_scsf2b","o_scsf3a","o_scsf3b","o_scsf4a","o_scsf4b","o_scsf5","o_scsf6a","o_scsf6b","o_scsf6c","o_scsf7",
                         "o_scghq1_dv","o_scghq2_dv",
                         ## health condition variables
                         "o_hcond1", "o_hcond2", "o_hcond3", "o_hcond4", "o_hcond5", "o_hcond6", "o_hcond7", "o_hcond8", "o_hcond10", "o_hcond11", "o_hcond12", "o_hcond13", "o_hcond14", "o_hcond15", "o_hcond16", "o_hcond21",
                         "o_hcondnew1", "o_hcondnew2", "o_hcondnew3", "o_hcondnew4", "o_hcondnew5", "o_hcondnew6", "o_hcondnew7", "o_hcondnew8", "o_hcondnew10", "o_hcondnew11", "o_hcondnew12", "o_hcondnew13", "o_hcondnew14", "o_hcondnew15", "o_hcondnew16", "o_hcondnew21",
                         "o_hconds01", "o_hconds03", "o_hconds04", "o_hconds05", "o_hconds08", "o_hconds11", "o_hconds12", "o_hconds15", "o_hconds16", "o_hconds21",
                         ## health care utilisation variables
                         "o_hl2gp", "o_hl2hop", "o_hosp", "o_hospd",
                         ### health conditions

                         ## pregnancy variables
                         "o_preg",
                         "o_pregout1","o_pregend1","o_pregsmoke1","o_smkmnth11","o_smkmnth21","o_smkmnth31","o_pregsmk11","o_pregsmk21","o_pregsmk31","o_aedrof1","o_aepuwk1","o_aepuda1","o_lchmulti1",
                         "o_pregout2","o_pregend2","o_pregsmoke2","o_smkmnth12","o_smkmnth22","o_smkmnth32","o_pregsmk12","o_pregsmk22","o_pregsmk32","o_aedrof2","o_aepuwk2","o_aepuda2","o_lchmulti2",
                         "o_pregout3","o_pregend3","o_pregsmoke3","o_smkmnth13","o_smkmnth23","o_smkmnth33","o_pregsmk13","o_pregsmk23","o_pregsmk33","o_aedrof3","o_aepuwk3","o_aepuda3","o_lchmulti3",
                         ## smoke variables

                         ## alcohol variables

                         ## weight
                         "o_indinus_lw","o_inding2_xw"),

                       c("pidp","pid","hidp","persoo_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## previous wave variables
                         "notempchk","empchk",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy","jbterm1","jbterm2","jbsemp",
                         ## income variables
                         "fimnnet_dv", "fimngrs_dv",
                         "fimnlabnet_dv", "fimnlabgrs_dv", "fimnmisc_dv", "fimnprben_dv", "fimninvnet_dv", "fimnpen_dv", "fimnsben_dv",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07","ovthours_pw","ovthours_paid",
                         "nssec","nssec_3cat","nssec_5cat","nssec_8cat", "jbsize",
                         ## employees
                         "last_gross_pay","last_net_pay","usuam_pay","payug","pay_type","ovtpay","additional.pay_set","extrate","basic.pay_set","baspay_rate",
                         "ovt.pay_set","ovtpay_rate",
                         ## self-employed
                         "s.emp_hours","s.emp_pay","s.emp_pay_pretax","s.emp_pay_preNI",
                         ## non-employed
                         "jbhad",
                         ## second job

                         ## benefits
                         "benbase1","benbase2","benbase3","benbase4","benbase96",
                         "benctc",
                         ## pensions
                         "NI.state_pen","employer_pen","spouse.emp_pen","pencred_pen","prvt_pen","widow_pen","parent_pen","benpen8","noo_benpen",
                         ## disability benefits
                         "bendis1","bendis2","bendis3","bendis4","bendis5","bendis12",
                         "bendis7","bendis8","bendis10","bendis97","bendis96",
                         ## other benefits
                         "benesa","othben1","othben2","othben5","othben6",
                         "othben7","othben8","othben9","othben97","othben96",
                         ## benefit income variables (formerly receivables)
                         "bensta_edugrant","bensta_tupay","bensta_alimony","bensta_fampay","bensta_rentlodge","bensta_rentother","bensta_other","noo_bensta",
                         ## household finance variables
                         "fiyrdia","fiyrdb1","fiyrdb2","fiyrdb3","fiyrdb4","fiyrdb5","fiyrdb6","finnow","finfut",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         "ghq1","ghq2",
                         ## health condition variables
                         "hcond1", "hcond2", "hcond3", "hcond4", "hcond5", "hcond6", "hcond7", "hcond8", "hcond10", "hcond11", "hcond12", "hcond13", "hcond14", "hcond15", "hcond16", "hcond21",
                         "hcondnew1", "hcondnew2", "hcondnew3", "hcondnew4", "hcondnew5", "hcondnew6", "hcondnew7", "hcondnew8", "hcondnew10", "hcondnew11", "hcondnew12", "hcondnew13", "hcondnew14", "hcondnew15", "hcondnew16", "hcondnew21",
                         "hconds01", "hconds03", "hconds04", "hconds05", "hconds08", "hconds11", "hconds12", "hconds15", "hconds16", "hconds21",
                         ## health care utilisation variables
                         "hl2gp", "hl2hop", "hosp", "hospd",
                         ## pregnancy variables
                         "preg",
                         "pregout1","pregend1","pregsmoke1","smkmnth11","smkmnth21","smkmnth31","pregsmk_ncigs11","pregsmk_ncigs21","pregsmk_ncigs31","pregdrnk_freq1","pregdrnk_unitpw1","pregdrnk_unit1","lchmulti1",
                         "pregout2","pregend2","pregsmoke2","smkmnth12","smkmnth22","smkmnth32","pregsmk_ncigs12","pregsmk_ncigs22","pregsmk_ncigs32","pregdrnk_freq2","pregdrnk_unitpw2","pregdrnk_unit2","lchmulti2",
                         "pregout3","pregend3","pregsmoke3","smkmnth13","smkmnth23","smkmnth33","pregsmk_ncigs13","pregsmk_ncigs23","pregsmk_ncigs33","pregdrnk_freq3","pregdrnk_unitpw3","pregdrnk_unit3","lchmulti3",
                         ## smoke variables

                         ## alcohol variables

                         ## weight
                         "weight_lw","weight_xw"))

  data[, wave := "UKHLS Wave 15"]
  data[, wave_no := 15]
  data[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data[, dataset := "UKHLS"]
  data[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

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

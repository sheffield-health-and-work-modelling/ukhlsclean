#' Read Understanding Society 2023
#'
#' Reads and cleans the Understanding Society calendar year data for 2023 These data are derived from the main
#' Understanding Society survey but designed to be a representative cross-section for the year 2023 For this data,
#' the function applies the full reading and cleaning process.
#'
#' UKDS Study Number: \href{https://datacatalogue.ukdataservice.ac.uk/studies/study/9471}{SN9471 Understanding Society: Calendar Year Dataset, 2023}
#'
#' @source University of Essex, Institute for Social and Economic Research. (2024).
#' Understanding Society: Calendar Year Dataset, 2024 [data collection]. UK Data Service. SN: 9471, \href{https://doi.org/10.5255/UKDA-SN-9471-1}{DOI: 10.5255/UKDA-SN-9471-1}
#'
#'
#' @param root Character - the root directory.
#' @param file Character - the file path and name.
#' @param full Logical - TRUE if restricting the sample to full interviews only (excluding proxies)
#' @param ages Integer vector - the ages in single years to retain (defaults to 16 to 89 years).
#' @param country Character - country to produce data for. One of c("UK","england","wales","scotland","northern_ireland"). Defaults to all UK.
#' @param keep_vars Character vector - the names of the variables to keep (defaults to NULL - retaining all variables).
#' @param complete_vars Character vector - the names of the variables on which the selection of complete cases will be based (defaults to year, age and sex).
#' @param inflation_index Character - one of c("cpih","rpi"). Default option is CPIH.
#'
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
ukhlsclean_2023 <- function(
    root = c("X:/"),
    file = "HAR_PR/PR/USoc/Data/Calendar Year Datasets/2023/SN9471_2025_11_06/tab",
    #root = c("C:/"),
    #file = "Users/damon/OneDrive/Documents/USoc/Datasets/Calendar year data/2022/tab",
    full = FALSE,
    ages = 16:89,
    country = "UK",
    keep_vars = NULL,
    complete_vars = NULL,
    inflation_index = "cpih"
) {

  cat(crayon::blue(crayon::underline("\tReading UKHLS Calendar Year 2023 datasets")))

  cat(crayon::green("\tIndividual..."))

  path <- here::here(paste0(root, file))

  data <- data.table::fread(
    paste0(path, "/mno_indresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-10", "-90", "-90.0", "N/A")
  )
  if (full == TRUE) {
    # retain full interviews only
    data <- data[mno_ivfio==1,]
  }


  data.table::setnames(data, names(data), tolower(names(data)))

  id_vars          <- Hmisc::Cs(pidp, pid, mno_hidp, mno_pno, mno_psu, mno_strata, mno_istrtdaty, mno_istrtdatm, mno_istrtdatd)
  demographic_vars <- Hmisc::Cs(mno_sex, mno_dvage, mno_birthy, mno_gor_dv, mno_urban_dv, mno_mlstat, mno_marstat)
  econ_stat_vars   <- Hmisc::Cs(mno_jbstat, mno_jbhas, mno_jboff, mno_jboffy)
  work_vars        <- Hmisc::Cs(mno_paygu_dv, mno_payg_dv, mno_jbhrs, mno_fimnlabgrs_dv, mno_seearngrs_dv, mno_jbsic07_cc)
  education_vars   <- Hmisc::Cs(mno_hiqual_dv)
  health_vars      <- Hmisc::Cs(mno_health, mno_aidhh, mno_sclfsat1, mno_sclfsato, mno_sf12pcs_dv, mno_sf12mcs_dv,
                                mno_scsf1, mno_scsf2a, mno_scsf2b, mno_scsf3a, mno_scsf3b, mno_scsf4a, mno_scsf4b, mno_scsf5, mno_scsf6a,
                                mno_scsf6b, mno_scsf6c, mno_scsf7)
  health_cond_vars <- Hmisc::Cs(mno_hconds01, mno_hconds03, mno_hconds04, mno_hconds05, mno_hconds08,
                                              mno_hconds11, mno_hconds12, mno_hconds15, mno_hconds16,
                                mno_hconds21,                                           mno_hconds26, mno_hconds27, mno_hconds28, mno_hconds29,
                                mno_hconds30, mno_hconds31, mno_hconds32, mno_hconds33, mno_hconds34, mno_hconds35,

                                mno_hcondns1, mno_hcondns3, mno_hcondns4, mno_hcondns5, mno_hcondns6, mno_hcondns7, mno_hcondns8, mno_hcondns10)
  preg_vars        <- Hmisc::Cs(mno_pregout1, mno_pregout2)
  smoke_vars       <- Hmisc::Cs(mno_smoker, mno_ncigs)
  benefits_vars    <- Hmisc::Cs(mno_benbase1, mno_benbase2, mno_benbase3, mno_benbase4, mno_benbase96,
                                mno_benctc)
  pension_vars     <- Hmisc::Cs(mno_benpen1, mno_benpen2, mno_benpen3, mno_benpen4, mno_benpen5, mno_benpen6, mno_benpen7, mno_benpen8, mno_benpen96)
  bendis_vars      <- Hmisc::Cs(mno_bendis1, mno_bendis2, mno_bendis3, mno_bendis4, mno_bendis5, mno_bendis12,
                                mno_bendis7, mno_bendis8, mno_bendis10, mno_bendis97, mno_bendis96)
  otherben_vars    <- Hmisc::Cs(mno_benesa,
                                mno_othben1, mno_othben2,                           mno_othben5, mno_othben6, mno_othben7, mno_othben8, mno_othben9, mno_othben97, mno_othben96)
  benincome_vars   <- Hmisc::Cs(mno_bensta2, mno_bensta3, mno_bensta4, mno_bensta5, mno_bensta6, mno_bensta7, mno_bensta97, mno_bensta96)
  weight_vars      <- Hmisc::Cs(mno_inding2_xw)

  s2020_vars       <- Hmisc::Cs(mno_ethn_dv)

  names <- c(id_vars, demographic_vars, econ_stat_vars, work_vars, education_vars,
             health_vars, preg_vars, smoke_vars,
             benefits_vars, pension_vars, bendis_vars, otherben_vars, benincome_vars,
             weight_vars, s2020_vars)
  names <- tolower(names)

  data <- data[ , names, with = F]

  data.table::setnames(data,

                       c("pidp","pid","mno_hidp","mno_pno","mno_psu","mno_strata","mno_istrtdaty","mno_istrtdatm","mno_istrtdatd",
                         ## demographic
                         "mno_sex","mno_dvage","mno_birthy","mno_gor_dv","mno_urban_dv","mno_mlstat","mno_marstat",
                         ## economic status
                         "mno_jbstat","mno_jbhas","mno_jboff","mno_jboffy",
                         ## work variables
                         "mno_paygu_dv","mno_payg_dv","mno_jbhrs","mno_fimnlabgrs_dv","mno_seearngrs_dv","mno_jbsic07_cc",
                         ## education variables
                         "mno_hiqual_dv",
                         ## health variables
                         "mno_health","mno_aidhh","mno_sclfsat1","mno_sclfsato","mno_sf12pcs_dv","mno_sf12mcs_dv",
                         "mno_scsf1","mno_scsf2a","mno_scsf2b","mno_scsf3a","mno_scsf3b","mno_scsf4a","mno_scsf4b","mno_scsf5","mno_scsf6a","mno_scsf6b","mno_scsf6c","mno_scsf7",
                         ## health conditions
                         #"mno_hconds01", "mno_hconds03", "mno_hconds04", "mno_hconds05", "mno_hconds08",
                         #                "mno_hconds11", "mno_hconds12", "mno_hconds15", "mno_hconds16",
                         #"mno_hconds21"                                                , "mno_hconds26", "mno_hconds27", "mno_hconds28", "mno_hconds29",
                         #"mno_hconds30", "mno_hconds31", "mno_hconds32", "mno_hconds33", "mno_hconds34", "mno_hconds35",

                         #"mno_hcondns1", "mno_hcondns3", "mno_hcondns4", "mno_hcondns5", "mno_hcondns6", "mno_hcondns7", "mno_hcondns8",
                         #"mno_hcondns10",
                         ## pregnancy variables
                         "mno_pregout1","mno_pregout2",
                         ## smoke variables
                         "mno_smoker", "mno_ncigs",
                         ## benefits
                         "mno_benbase1","mno_benbase2","mno_benbase3","mno_benbase4","mno_benbase96",
                         "mno_benctc",
                         ## pensions
                         "mno_benpen1","mno_benpen2","mno_benpen3","mno_benpen4","mno_benpen5","mno_benpen6","mno_benpen7","mno_benpen8","mno_benpen96",
                         ## disability benefits
                         "mno_bendis1","mno_bendis2","mno_bendis3","mno_bendis4","mno_bendis5","mno_bendis12",
                         "mno_bendis7","mno_bendis8","mno_bendis10","mno_bendis97","mno_bendis96",
                         ## other benefits
                         "mno_benesa",
                         "mno_othben1","mno_othben2"                            ,"mno_othben5","mno_othben6","mno_othben7","mno_othben8","mno_othben9","mno_othben97","mno_othben96",
                         ## benefit income variables (formerly receivables)
                         "mno_bensta2","mno_bensta3","mno_bensta4","mno_bensta5","mno_bensta6","mno_bensta7","mno_bensta97","mno_bensta96",
                         ## weight
                         "mno_inding2_xw",
                         ## 2020 specific variables
                         "mno_ethn_dv"),


                       c("pidp","pid","hidp","person_number","psu","strata","year","month","day",
                         ## demographic
                         "sex","age","birth_year","region","urban","mlstat","marstat",
                         ## economic status
                         "econ_stat","jbhas","jboff","jboffy",
                         ## work variables
                         "grss_pay_usual","grss_pay_last","hours","grss_lab_inc","grss_semp","sic07",
                         ## education variables
                         "highest_qual",
                         ## health variables
                         "lt_sick","caring","health_satisf","life_satisf","sf12_pcs","sf12_mcs",
                         "sf1","sf2a","sf2b","sf3a","sf3b","sf4a","sf4b","sf5","sf6a","sf6b","sf6c","sf7",
                         ## health condition variables
                         #"hconds01", "hconds03", "hconds04", "hconds05", "hconds08",
                         #            "hconds11", "hconds12", "hconds15", "hconds16",
                         #"hconds21",                                     "hconds26", "hconds27", "hconds28", "hconds29",
                         #"hconds30", "hconds31", "hconds32", "hconds33", "hconds34", "hconds35",

                         #"hcondns1", "hcondns3", "hcondns4", "hcondns5", "hcondns6", "hcondns7", "hcondns8", "hcondns10",
                         ## pregnancy variables
                         "pregout1","pregout2",
                         ## smoke variables
                         "smoker", "ncigs",
                         ## benefits
                         "benbase1","benbase2","benbase3","benbase4","benbase96",
                         "benctc",
                         ## pensions
                         "NI.state_pen","employer_pen","spouse.emp_pen","pencred_pen","prvt_pen","widow_pen","parent_pen","benpen8","non_benpen",
                         ## disability benefits
                         "bendis1","bendis2","bendis3","bendis4","bendis5","bendis12",
                         "bendis7","bendis8","bendis10","bendis97","bendis96",
                         ## other benefits
                         "benesa","othben1","othben2","othben5","othben6",
                         "othben7","othben8","othben9","othben97","othben96",
                         ## benefit income variables (formerly receivables)
                         "bensta_edugrant","bensta_tupay","bensta_alimony","bensta_fampay","bensta_rentlodge","bensta_rentother","bensta_other","non_bensta",
                         ## weight
                         "weight_xw",
                         ## 2020 specific variables
                         "ethn_dv"))

  ########################################
  ######## ADD IN HOUSEHOLD DATA #########

  cat(crayon::green("\tHousehold..."))

  data.hhold <- data.table::fread(
    paste0(path, "/mno_hhresp.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.hhold, names(data.hhold), tolower(names(data.hhold)))

  hhold_vars          <- Hmisc::Cs(mno_hidp, mno_tenure_dv, mno_nkids_dv, mno_hhsize, mno_hhtype_dv,
                                   mno_nch02_dv, mno_nch34_dv, mno_nch511_dv, mno_nch1215_dv)

  data.hhold <- data.hhold[ , hhold_vars, with = F]
  data.table::setnames(data.hhold,
                       # old names
                       c("mno_hidp", "mno_tenure_dv", "mno_nkids_dv", "mno_hhsize","mno_hhtype_dv",
                         "mno_nch02_dv", "mno_nch34_dv", "mno_nch511_dv", "mno_nch1215_dv"),
                       # new names
                       c("hidp", "hh_tenure", "hh_numchild", "hh_size", "hh_type",
                         "hh_numchild02", "hh_numchild34", "hh_numchild511", "hh_numchild1215"))

  hhold_merged <- merge(x = data,
                        y = data.hhold,
                        by = "hidp",
                        all.x = TRUE,
                        all.y = FALSE)

  ####################################################
  #### ADD IN THE INDALL DATA ########################

  cat(crayon::green("\tIndall..."))

  data.indall <- data.table::fread(
    paste0(path, "/mno_indall.tab"),
    showProgress = FALSE,
    na.strings = c("NA", "", "-1", "-2", "-6", "-7", "-8", "-9", "-90", "-90.0", "N/A")
  )
  data.table::setnames(data.indall, names(data.indall), tolower(names(data.indall)))

  indall_vars  <- colnames(data.indall[ , c("pidp","mno_hidp")])

  data.indall <- data.indall[ , indall_vars, with = F]
  data.table::setnames(data.indall,
                       # old names
                       c("pidp","mno_hidp"),
                       # new names
                       c("pidp","hidp"))

  ## Combine - keep all observations in the main data and drop excess xwave observations

  data_merged <- merge(x = hhold_merged,
                       y = data.indall,
                       by = c("pidp","hidp"),
                       all.x = TRUE,
                       all.y = FALSE)

  ##########################################################################

  rm(data, data.hhold); gc()

  cat(crayon::blue(crayon::bold("\tdone\n")))

  ####################################
  ### Apply all cleaning functions ###

  data_merged[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merged[, wave_no := NA]
  data_merged[, wave := " "]
  data_merged[, bhps_sample := ifelse(!is.na(pid),TRUE,FALSE)]
  data_merged[, id := ifelse(bhps_sample==FALSE, pidp, pid)]

  ## drop small number (643) of 2021 observations
  data_merged <- data_merged[year == 2023,]

  if (inflation_index == "cpih"){
    inflation <- ukhlsclean::cpih
  }
  if (inflation_index == "rpi"){
    inflation <- ukhlsclean::rpi
  }

  cleaned <- ukhls_clean_global(data = data_merged,
                                ages = ages,
                                country = country,
                                complete_vars = complete_vars,
                                calendar_year = TRUE,
                                inflation = inflation)
  return(cleaned)
}

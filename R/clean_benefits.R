#' Clean Benefit Variables
#'
#' Clean all variables related to social security benefit receipt and monthly
#' income received from social security benefits.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
clean_benefits <- function(data = NULL) {

  # =========================================================================
  # 1. BENEFIT RECEIPT VARIABLES
  # =========================================================================

  # (8) Severe Disablement Allowance
  # (9) Industrial Injury Disablement Benefit
  # (10) Disability Living Allowance
  # (11) Attendance Allowance
  # (12) Carer's Allowance
  # (13) War Disablement Pension
  # (14) Incapacity Benefit
  # (15) Income Support
  # (16) Jobseekers' Allowance
  # (18) Child Benefit
  # (19) Child Tax Credit
  # (20) Working Tax Credit
  # (21) Maternity Allowance
  # (22) Housing Benefit
  # (23) Council Tax Benefit
  # (33) Employment and Support Allowance
  # (40) Universal Credit
  # (41) Personal Independence Payments

  ### ------- Industrial Injuries Disablement Benefit

  data[, b_IIDB := ben_receipt_9]

  ### ------- PIP
  # (Disability / Care needs)
  # Disability Living Allowance - legacy benefit for extra costs associated with disability
  # Attendance Allowance - the equivalent of DLA/PIP for individuals over SPA
  # Personal Independence Payment - Introduced to replace DLA for working-age adults

  data[, b_PIP_broad := ben_receipt_10 + ben_receipt_11 + ben_receipt_41]
  data[b_PIP_broad >= 1, b_PIP_broad := 1]

  ### ------- Employment & Support Allowance
  # (Sickness / Incapacity for Work)
  # Incapacity Benefit - those unable to work due to illness
  # Severe Disablement Allowance - Older legacy benefit, closed to new entrants since 2001
  # Employment and Support Allowance - Introduced to replace IB and SDA

  data[, b_ESA_broad := ben_receipt_8 + ben_receipt_14 + ben_receipt_33]
  data[b_ESA_broad >= 1, b_ESA_broad := 1]

  ### ------- Income Support

  data[, b_IncomeSupport := ben_receipt_15]

  ### ------- Jobseekers' Allowance

  data[, b_JSA := ben_receipt_16]

  ### ------- Housing Benefit

  data[, b_HousingBenefit := ben_receipt_22]

  ### ------- Working Tax Credit

  data[, b_WTC := ben_receipt_20]

  ### ------- Child Tax Credit

  data[, b_CTC := ben_receipt_19]

  ### ------- Child Benefit

  data[, b_ChildBenefit := ben_receipt_18]

  ### ------- Child Benefit

  data[, b_CarersAllowance := ben_receipt_12]

  ### ------- Universal Credit

  data[, b_UniCred := ben_receipt_40]

  # UC with legacy benefits included
  data[, b_UniCred_legacy := ben_receipt_40 + b_WTC + b_HousingBenefit + b_JSA + b_ESA_broad + b_IncomeSupport]
  data[b_UniCred_legacy >= 1, b_UniCred_legacy := 1]

  # =========================================================================
  # 3. AGGREGATED BENEFIT TYPES
  # =========================================================================

  ### Unemployment benefits
  data[, b_UnempBenefit := b_JSA]
  data[b_UniCred == 1 & jbstat == 3, b_UnempBenefit := b_UnempBenefit + b_UniCred]
  data[b_UnempBenefit >= 1, b_UnempBenefit := 1]

  ### Sickness/Illness/Incapacity benefits
  data[, b_SickDisabBenefit := b_ESA_broad + b_PIP_broad + b_IIDB]
  data[b_UniCred == 1 & jbstat == 8, b_SickDisabBenefit := b_SickDisabBenefit + b_UniCred]
  data[b_SickDisabBenefit >= 1, b_SickDisabBenefit := 1]

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "hidp", "wave_no",
                         "b_UnempBenefit", "b_SickDisabBenefit", "b_UniCred", "b_UniCred_legacy",
                         "b_PIP_broad", "b_IIDB", "b_ESA_broad", "b_CarersAllowance",
                         "b_ChildBenefit", "b_WTC", "b_CTC", "b_HousingBenefit",
                         "b_JSA", "b_IncomeSupport")]

  return(final_data)
}



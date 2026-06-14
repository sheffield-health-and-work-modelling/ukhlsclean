#' Clean Health and Caring Variables
#'
#' Produce clean versions of indicator variables for health conditions.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#'
#' @export
ukhls_clean_health_conditions <- function(data = NULL) {

  wave <- as.integer(unique(data[ , wave_no][1]))

  # =========================================================================
  # 1. SETUP & CONFIGURATION FLAGS
  # =========================================================================

  is_wave1      <- wave == 1
  is_wave2      <- wave == 2
  is_wave3to9   <- wave %in% 3:9
  is_wave10     <- wave == 10
  is_wave11to13 <- wave %in% 11:13
  is_wave14to15 <- wave %in% 14:15

  # =========================================================================
  # 2. INIITAL HEALTH CONDITIONS REPORTED IN WAVE 1 AND MISSINGS IN WAVE 2
  # =========================================================================

  if (is_wave1){
    print("Wave 1")

    ## ========= RESPIRATORY CONDITIONS ================================== ##

    #### ---------- (1) Asthma --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond1 >= 0 | hconds01 > 0, cond1_asthma := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond1 == 1 & hconds01 == 1, cond1_asthma := 1]

    # 3. Apply Missingness Rule
    data[hcond1 == 1 & (hconds01 < 0 | is.na(hconds01)), cond1_asthma := NA]

    # 4. Construct Lifetime History (ever_asthma)
    data[hcond1 >= 0, cond1_asthma_ever := 0]
    data[hcond1 == 1, cond1_asthma_ever := 1]

    #### ---------- (8) Emphysema ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond8 >= 0 | hconds08 > 0, cond8_emphysema := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond8 == 1 & hconds08 == 1, cond8_emphysema := 1]

    # 3. Apply Missingness Rule
    data[hcond8 == 1 & (hconds08 < 0 | is.na(hconds08)), cond8_emphysema := NA]

    # 4. Construct Lifetime History (ever_asthma)
    data[hcond8 >= 0, cond8_emphysema_ever := 0]
    data[hcond8 == 1, cond8_emphysema_ever := 1]

    #### ---------- (11) Bronchitis ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond11 >= 0 | hconds11 > 0, cond11_chronic_bronchitis := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond11 == 1 & hconds11 == 1, cond11_chronic_bronchitis := 1]

    # 3. Apply Missingness Rule
    data[hcond11 == 1 & (hconds11 < 0 | is.na(hconds11)), cond11_chronic_bronchitis := NA]

    # 4. Construct Lifetime History (ever_asthma)
    data[hcond11 >= 0, cond11_chronic_bronchitis_ever := 0]
    data[hcond11 == 1, cond11_chronic_bronchitis_ever := 1]

    ### ------------ (21) COPD ----------- ####

    data[, cond21_copd := NA]
    data[, cond21_copd_ever := NA]

    ## ========= CARDIOVASCULAR CONDITIONS ================================== ##

    #### ---------- (3) Congestive heart failure ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond3 >= 0 | hconds03 > 0, cond3_congestive_heart_failure := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond3 == 1 & hconds03 == 1, cond3_congestive_heart_failure := 1]

    # 3. Apply Missingness Rule
    data[hcond3 == 1 & (hconds03 < 0 | is.na(hconds03)), cond3_congestive_heart_failure := NA]

    # 4. Construct Lifetime History
    data[hcond3 >= 0, cond3_congestive_heart_failure_ever := 0]
    data[hcond3 == 1, cond3_congestive_heart_failure_ever := 1]

    #### ---------- (4) Coronary heart disease ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond4 >= 0 | hconds04 > 0, cond4_coronary_heart_disease := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond4 == 1 & hconds04 == 1, cond4_coronary_heart_disease := 1]

    # 3. Apply Missingness Rule
    data[hcond4 == 1 & (hconds04 < 0 | is.na(hconds04)), cond4_coronary_heart_disease := NA]

    # 4. Construct Lifetime History
    data[hcond4 >= 0, cond4_coronary_heart_disease_ever := 0]
    data[hcond4 == 1, cond4_coronary_heart_disease_ever := 1]

    #### ---------- (5) Angina ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond5 >= 0 | hconds05 > 0, cond5_angina := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond5 == 1 & hconds05 == 1, cond5_angina := 1]

    # 3. Apply Missingness Rule
    data[hcond5 == 1 & (hconds05 < 0 | is.na(hconds05)), cond5_angina := NA]

    # 4. Construct Lifetime History
    data[hcond5 >= 0, cond5_angina_ever := 0]
    data[hcond5 == 1, cond5_angina_ever := 1]

    #### ---------- (16) High blood pressure ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond16 >= 0 | hconds16 > 0, cond16_high_bp := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond16 == 1 & hconds16 == 1, cond16_high_bp := 1]

    # 3. Apply Missingness Rule
    data[hcond16 == 1 & (hconds16 < 0 | is.na(hconds16)), cond16_high_bp := NA]

    # 4. Construct Lifetime History
    data[hcond16 >= 0, cond16_high_bp_ever := 0]
    data[hcond16 == 1, cond16_high_bp_ever := 1]

    ## ========= METABOLIC AND ENDOCRINE CONDITIONS ========================= ##

    #### ---------- (10) Hypothyroidism ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond10 >= 0 | hconds10 > 0, cond10_hypothyroidism := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond10 == 1 & hconds10 == 1, cond10_hypothyroidism := 1]

    # 3. Apply Missingness Rule
    data[hcond10 == 1 & (hconds10 < 0 | is.na(hconds10)), cond10_hypothyroidism := NA]

    # 4. Construct Lifetime History (ever_asthma)
    data[hcond10 >= 0, cond10_hypothyroidism_ever := 0]
    data[hcond10 == 1, cond10_hypothyroidism_ever := 1]

    #### ---------- (9) Hyperthyroidism ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond9 >= 0 | hconds09 > 0, cond9_hyperthyroidism := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond9 == 1 & hconds09 == 1, cond9_hyperthyroidism := 1]

    # 3. Apply Missingness Rule
    data[hcond9 == 1 & (hconds09 < 0 | is.na(hconds09)), cond9_hyperthyroidism := NA]

    # 4. Construct Lifetime History (ever_asthma)
    data[hcond9 >= 0, cond9_hyperthyroidism_ever := 0]
    data[hcond9 == 1, cond9_hyperthyroidism_ever := 1]

    #### ---------- (14) Diabetes ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond14 >= 0 | hconds14 > 0, cond14_diabetes := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond14 == 1 & hconds14 == 1, cond14_diabetes := 1]

    # 3. Apply Missingness Rule
    data[hcond14 == 1 & (hconds14 < 0 | is.na(hconds14)), cond14_diabetes := NA]

    # 4. Construct Lifetime History (ever_asthma)
    data[hcond14 >= 0, cond14_diabetes_ever := 0]
    data[hcond14 == 1, cond14_diabetes_ever := 1]


    ## ========= MUSCULOSKELETAL CONDITIONS ================================= ##

    #### ---------- (2) Arthritis --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond2 >= 0 | hconds02 > 0, cond2_arthritis := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond2 == 1 & hconds02 == 1, cond2_arthritis := 1]

    # 3. Apply Missingness Rule
    data[hcond2 == 1 & (hconds02 < 0 | is.na(hconds02)), cond2_arthritis := NA]

    # 4. Construct Lifetime History (ever_asthma)
    data[hcond2 >= 0, cond2_arthritis_ever := 0]
    data[hcond2 == 1, cond2_arthritis_ever := 1]

    ## ========= NEUROLOGICAL CONDITIONS ==================================== ##

    #### ---------- (15) Epilepsy --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond15 >= 0 | hconds15 > 0, cond15_epilepsy := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond15 == 1 & hconds15 == 1, cond15_epilepsy := 1]

    # 3. Apply Missingness Rule
    data[hcond15 == 1 & (hconds15 < 0 | is.na(hconds15)), cond15_epilepsy := NA]

    # 4. Construct Lifetime History (ever_asthma)
    data[hcond15 >= 0, cond15_epilepsy_ever := 0]
    data[hcond15 == 1, cond15_epilepsy_ever := 1]

    ## ========= OTHER CONDITIONS ================================= ##

    #### ---------- (13) Cancer --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond13 >= 0 | hconds13 > 0, cond13_cancer := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond13 == 1 & hconds13 == 1, cond13_cancer := 1]

    # 3. Apply Missingness Rule
    data[hcond13 == 1 & (hconds13 < 0 | is.na(hconds13)), cond13_cancer := NA]

    # 4. Construct Lifetime History (ever_asthma)
    data[hcond13 >= 0, cond13_cancer_ever := 0]
    data[hcond13 == 1, cond13_cancer_ever := 1]

    #### ---------- (12) Liver conditions --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond12 >= 0 | hconds12 > 0, cond12_liver_conditions := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond12 == 1 & hconds12 == 1, cond12_liver_conditions := 1]

    # 3. Apply Missingness Rule
    data[hcond12 == 1 & (hconds12 < 0 | is.na(hconds12)), cond12_liver_conditions := NA]

    # 4. Construct Lifetime History (ever_asthma)
    data[hcond12 >= 0, cond12_liver_conditions_ever := 0]
    data[hcond12 == 1, cond12_liver_conditions_ever := 1]

  } ## end wave 1 processing

  if (is_wave2){
    print("Wave 2")

    data[, cond1_asthma := NA]
    data[, cond8_emphysema := NA]
    data[, cond11_chronic_bronchitis := NA]
    data[, cond10_hypothyroidism := NA]
    data[, cond9_hyperthyroidism := NA]
    data[, cond14_diabetes := NA]
    data[, cond2_arthritis := NA]
    data[, cond13_cancer := NA]
    data[, cond21_copd := NA]
    data[, cond3_congestive_heart_failure := NA]
    data[, cond4_coronary_heart_disease := NA]
    data[, cond5_angina := NA]
    data[, cond16_high_bp := NA]
    data[, cond15_epilepsy := NA]
    data[, cond12_liver_conditions := NA]

    data[, cond1_asthma_ever := NA]
    data[, cond8_emphysema_ever := NA]
    data[, cond11_chronic_bronchitis_ever := NA]
    data[, cond10_hypothyroidism_ever := NA]
    data[, cond9_hyperthyroidism_ever := NA]
    data[, cond14_diabetes_ever := NA]
    data[, cond2_asthma_ever := NA]
    data[, cond13_cancer_ever := NA]
    data[, cond21_copd_ever := NA]
    data[, cond3_congestive_heart_failure_ever := NA]
    data[, cond4_coronary_heart_disease_ever := NA]
    data[, cond5_angina_ever := NA]
    data[, cond16_high_bp_ever := NA]
    data[, cond15_epilepsy_ever := NA]
    data[, cond12_liver_conditions_ever := NA]

  } ## end wave 2 processing

  # =========================================================================
  # 3. UPDATING HEALTH CONDITIONS THROUGH WAVES 3-9 (not asked in wave 2)
  # =========================================================================

  if (is_wave3to9) {
    print("Wave 3-9")

    ## ========= RESPIRATORY CONDITIONS ================================== ##

    #### ---------- (1) Asthma --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond1 >= 0 | hconds01 > 0 | hcondn1 >= 0, cond1_asthma := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond1 == 1 | hconds01 == 1 | hcondn1 == 1, cond1_asthma := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond1 == 1 & hconds01 == 2, cond1_asthma := 0]

    # 4. Apply Missingness Rule
    data[hcond1 == 1 & (hconds01 < 0 | is.na(hconds01)), cond1_asthma := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond1 >= 0 | hcondn1 >= 0 | hconds01 > 0, cond1_asthma_ever := 0]
    data[hcond1 == 1 | hcondn1 == 1 | hconds01 %in% c(1,2), cond1_asthma_ever := 1]

    #### ---------- (8) Emphysema ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond8 >= 0 | hconds08 > 0 | hcondn8 >= 0, cond8_emphysema := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond8 == 1 | hconds08 == 1 | hcondn8 == 1, cond8_emphysema := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond8 == 1 & hconds08 == 2, cond8_emphysema := 0]

    # 4. Apply Missingness Rule
    data[hcond8 == 1 & (hconds08 < 0 | is.na(hconds08)), cond8_emphysema := NA]

    # 5. Construct Lifetime History (ever_emphysema)
    data[hcond8 >= 0 | hcondn8 >= 0 | hconds08 > 0, cond8_emphysema_ever := 0]
    data[hcond8 == 1 | hcondn8 == 1 | hconds08 %in% c(1,2), cond8_emphysema_ever := 1]

    #### ---------- (11) Bronchitis ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond11 >= 0 | hconds11 > 0 | hcondn11 >= 0, cond11_chronic_bronchitis := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond11 == 1 | hconds11 == 1 | hcondn11 == 1, cond11_chronic_bronchitis := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond11 == 1 & hconds11 == 2, cond11_chronic_bronchitis := 0]

    # 4. Apply Missingness Rule
    data[hcond11 == 1 & (hconds11 < 0 | is.na(hconds11)), cond11_chronic_bronchitis := NA]

    # 5. Construct Lifetime History (ever_bronchitis)
    data[hcond11 >= 0 | hcondn11 >= 0 | hconds11 > 0, cond11_chronic_bronchitis_ever := 0]
    data[hcond11 == 1 | hcondn11 == 1 | hconds11 %in% c(1,2), cond11_chronic_bronchitis_ever := 1]

    ### ------------ (21) COPD ----------- ####

    data[, cond21_copd := NA]
    data[, cond21_copd_ever := NA]

    ## ========= CARDIOVASCULAR CONDITIONS ================================== ##

    #### ---------- (3) Congestive heart failure --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond3 >= 0 | hconds03 > 0 | hcond3 >= 0, cond3_congestive_heart_failure := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond3 == 1 | hconds03 == 1 | hcond3 == 1, cond3_congestive_heart_failure := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond3 == 1 & hconds03 == 2, cond3_congestive_heart_failure := 0]

    # 4. Apply Missingness Rule
    data[hcond3 == 1 & (hconds03 < 0 | is.na(hconds03)), cond3_congestive_heart_failure := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond3 >= 0 | hcondn3 >= 0 | hconds03 > 0, cond3_congestive_heart_failure_ever := 0]
    data[hcond3 == 1 | hcondn3 == 1 | hconds03 %in% c(1,2), cond3_congestive_heart_failure_ever := 1]

    #### ---------- (4) Coronary heart disease --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond4 >= 0 | hconds04 > 0 | hcondn4 >= 0, cond4_coronary_heart_disease := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond4 == 1 | hconds04 == 1 | hcondn4 == 1, cond4_coronary_heart_disease := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond4 == 1 & hconds04 == 2, cond4_coronary_heart_disease := 0]

    # 4. Apply Missingness Rule
    data[hcond4 == 1 & (hconds04 < 0 | is.na(hconds04)), cond4_coronary_heart_disease := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond4 >= 0 | hcondn4 >= 0 | hconds04 > 0, cond4_coronary_heart_disease_ever := 0]
    data[hcond4 == 1 | hcondn4 == 1 | hconds04 %in% c(1,2), cond4_coronary_heart_disease_ever := 1]

    #### ---------- (5) Angina --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond5 >= 0 | hconds05 > 0 | hcondn5 >= 0, cond5_angina := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond5 == 1 | hconds05 == 1 | hcondn5 == 1, cond5_angina := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond5 == 1 & hconds05 == 2, cond5_angina := 0]

    # 4. Apply Missingness Rule
    data[hcond5 == 1 & (hconds05 < 0 | is.na(hconds05)), cond5_angina := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond5 >= 0 | hcondn5 >= 0 | hconds05 > 0, cond5_angina_ever := 0]
    data[hcond5 == 1 | hcondn5 == 1 | hconds05 %in% c(1,2), cond5_angina_ever := 1]

    #### ---------- (16) High blood pressure --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond16 >= 0 | hconds16 > 0 | hcondn16 >= 0, cond16_high_bp := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond16 == 1 | hconds16 == 1 | hcondn16 == 16, cond16_high_bp := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond16 == 1 & hconds16 == 2, cond16_high_bp := 0]

    # 4. Apply Missingness Rule
    data[hcond16 == 1 & (hconds16 < 0 | is.na(hconds16)), cond16_high_bp := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond16 >= 0 | hcondn16 >= 0 | hconds16 > 0, cond16_high_bp_ever := 0]
    data[hcond16 == 1 | hcondn16 == 1 | hconds16 %in% c(1,2), cond16_high_bp_ever := 1]

    ## ========= METABOLIC AND ENDOCRINE CONDITIONS ========================= ##

    #### ---------- (10) Hypothyroidism ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond10 >= 0 | hconds10 > 0 | hcondn10 >= 0, cond10_hypothyroidism := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond10 == 1 | hconds10 == 1 | hcondn10 == 1, cond10_hypothyroidism := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond10 == 1 & hconds10 == 2, cond10_hypothyroidism := 0]

    # 4. Apply Missingness Rule
    data[hcond10 == 1 & (hconds10 < 0 | is.na(hconds10)), cond10_hypothyroidism := NA]

    # 5. Construct Lifetime History (ever_hypothyroidism)
    data[hcond10 >= 0 | hcondn10 >= 0 | hconds10 > 0, cond10_hypothyroidism_ever := 0]
    data[hcond10 == 1 | hcondn10 == 1 | hconds10 %in% c(1,2), cond10_hypothyroidism_ever := 1]

    #### ---------- (9) Hyperthyroidism ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond9 >= 0 | hconds09 > 0 | hcondn9 >= 0, cond9_hyperthyroidism := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond9 == 1 | hconds09 == 1 | hcondn9 == 1, cond9_hyperthyroidism := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond9 == 1 & hconds09 == 2, cond9_hyperthyroidism := 0]

    # 4. Apply Missingness Rule
    data[hcond9 == 1 & (hconds09 < 0 | is.na(hconds09)), cond9_hyperthyroidism := NA]

    # 5. Construct Lifetime History (ever_hypothyroidism)
    data[hcond9 >= 0 | hcondn9 >= 0 | hconds09 > 0, cond9_hyperthyroidism_ever := 0]
    data[hcond9 == 1 | hcondn9 == 1 | hconds09 %in% c(1,2), cond9_hyperthyroidism_ever := 1]

    #### ---------- (14) Diabetes ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond14 >= 0 | hconds14 > 0 | hcondn14 >= 0, cond14_diabetes := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond14 == 1 | hconds14 == 1 | hcondn14 == 1, cond14_diabetes := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond14 == 1 & hconds14 == 2, cond14_diabetes := 0]

    # 4. Apply Missingness Rule
    data[hcond14 == 1 & (hconds14 < 0 | is.na(hconds14)), cond14_diabetes := NA]

    # 5. Construct Lifetime History (ever_diabetes)
    data[hcond14 >= 0 | hcondn14 >= 0 | hconds14 > 0, cond14_diabetes_ever := 0]
    data[hcond14 == 1 | hcondn14 == 1 | hconds14 %in% c(1,2), cond14_diabetes_ever := 1]

    ## ========= MUSCULOSKELETAL CONDITIONS ================================= ##

    #### ---------- (2) Arthritis --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond2 >= 0 | hconds02 > 0 | hcondn2 >= 0, cond2_arthritis := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond2 == 1 | hconds02 == 1 | hcondn2 == 1, cond2_arthritis := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond2 == 1 & hconds02 == 2, cond2_arthritis := 0]

    # 4. Apply Missingness Rule
    data[hcond2 == 1 & (hconds02 < 0 | is.na(hconds02)), cond2_arthritis := NA]

    # 5. Construct Lifetime History
    data[hcond2 >= 0 | hcondn2 >= 0 | hconds02 == 0, cond2_arthritis_ever := 0]
    data[hcond2 == 1 | hcondn2 == 1 | hconds02 %in% c(1,2), cond2_arthritis_ever := 1]

    ## ========= NEUROLOGICAL CONDITIONS ================================= ##

    #### ---------- (15) Epilepsy --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond15 >= 0 | hconds15 > 0 | hcondn15 >= 0, cond15_epilepsy := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond15 == 1 | hconds15 == 1 | hcondn15 == 1, cond15_epilepsy := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond15 == 1 & hconds15 == 2, cond15_epilepsy := 0]

    # 4. Apply Missingness Rule
    data[hcond15 == 1 & (hconds15 < 0 | is.na(hconds15)), cond15_epilepsy := NA]

    # 5. Construct Lifetime History
    data[hcond15 >= 0 | hcondn15 >= 0 | hconds15 == 0, cond15_epilepsy_ever := 0]
    data[hcond15 == 1 | hcondn15 == 1 | hconds15 %in% c(1,2), cond15_epilepsy_ever := 1]

    ## ========= OTHER CONDITIONS ================================= ##

    #### ---------- (13) Cancer --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond13 >= 0 | hconds13 > 0 | hconds13 >= 0, cond13_cancer := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond13 == 1 | hconds13 == 1 | hcondn13 == 1, cond13_cancer := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond13 == 1 & hconds13 == 2, cond13_cancer := 0]

    # 4. Apply Missingness Rule
    data[hcond13 == 1 & (hconds13 < 0 | is.na(hconds13)), cond13_cancer := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond13 >= 0 | hcondn13 >= 0 | hconds13 == 0, cond13_cancer_ever := 0]
    data[hcond13 == 1 | hcondn13 == 1 | hconds13 %in% c(1,2), cond13_cancer_ever := 1]

    #### ---------- (12) Liver conditions --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond12 >= 0 | hconds12 > 0 | hcondn12 >= 0, cond12_liver_conditions := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond12 == 1 | hconds12 == 1 | hcondn12 == 1, cond12_liver_conditions := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond12 == 1 & hconds12 == 2, cond12_liver_conditions := 0]

    # 4. Apply Missingness Rule
    data[hcond12 == 1 & (hconds12 < 0 | is.na(hconds12)), cond12_liver_conditions := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond12 >= 0 | hcondn12 >= 0 | hconds12 == 0, cond12_liver_conditions_ever := 0]
    data[hcond12 == 1 | hcondn12 == 1 | hconds12 %in% c(1,2), cond12_liver_conditions_ever := 1]

  }
  # =========================================================================
  # 4. UPDATING HEALTH CONDITIONS WAVE 10 (02, 13, 14 coded differently)
  # =========================================================================

  ## note the code here is the same as the previous block for all but three
  ## conditions:
  ## (2) Arthritis
  ## (13) Cancer / malignancy
  ## (14) Diabetes
  ## The survey changed to regard these as permanent from wave 10 and didn't
  ## ask returning survey participants if they still had the condition. Therefore
  ## active prevalence and lifetime prevalence calculated in this wave need to be
  ## carried forward from waves 1-9 for these conditions

  ## Also, this wave introduced COPD as a condition and "re-baselined" everyone by
  ## asking if 'ever diagnosed' - `hcondeverXX` to everyone instead of just to new
  ## survey entrants (`hcond`) - treat both hcondever and hcond as a combined baselining
  ## variable

  if (is_wave10){
    print("Wave 10")

    ## ========= RESPIRATORY CONDITIONS ================================== ##

    #### ---------- (1) Asthma --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond1 >= 0 | hcondever1 >= 0 | hconds01 > 0, cond1_asthma := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond1 == 1 | hcondever1 == 1 | hconds01 == 1, cond1_asthma := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond1 == 1 | hcondever1 == 1) & hconds01 == 2, cond1_asthma := 0]

    # 4. Apply Missingness Rule
    data[(hcond1 == 1 | hcondever1 == 1) & (hconds01 < 0 | is.na(hconds01)), cond1_asthma := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond1 >= 0 | hcondever1 >= 0 | hconds01 > 0, cond1_asthma_ever := 0]
    data[hcond1 == 1 | hcondever1 == 1 | hconds01 %in% c(1,2), cond1_asthma_ever := 1]

    #### ---------- (8) Emphysema ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond8 >= 0 | hcondever8 >= 0 | hconds08 > 0, cond8_emphysema := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond8 == 1 | hcondever8 == 1 | hconds08 == 1, cond8_emphysema := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond8 == 1 | hcondever8 == 1) & hconds08 == 2, cond8_emphysema := 0]

    # 4. Apply Missingness Rule
    data[(hcond8 == 1 | hcondever8 == 1) & (hconds08 < 0 | is.na(hconds08)), cond8_emphysema := NA]

    # 5. Construct Lifetime History (ever_emphysema)
    data[hcond8 >= 0 | hcondever8 >= 0 | hconds08 > 0, cond8_emphysema_ever := 0]
    data[hcond8 == 1 | hcondever8 == 1 | hconds08 %in% c(1,2), cond8_emphysema_ever := 1]

    #### ---------- (11) Bronchitis ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond11 >= 0 | hcondever11 >= 0 | hconds11 > 0, cond11_chronic_bronchitis := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond11 == 1 | hcondever11 == 1 | hconds11 == 1, cond11_chronic_bronchitis := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond11 == 1 | hcondever11 == 1) & hconds11 == 2, cond11_chronic_bronchitis := 0]

    # 4. Apply Missingness Rule
    data[(hcond11 == 1 | hcondever11 == 1) & (hconds11 < 0 | is.na(hconds11)), cond11_chronic_bronchitis := NA]

    # 5. Construct Lifetime History (ever_chronic_bronchitis)
    data[hcond11 >= 0 | hcondever11 >= 0 | hconds11 > 0, cond11_chronic_bronchitis_ever := 0]
    data[hcond11 == 1 | hcondever11 == 1 | hconds11 %in% c(1,2), cond11_chronic_bronchitis_ever := 1]

    #### ---------- (21) COPD ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond21 >= 0 | hcondever21 >= 0 | hconds21 > 0, cond21_copd := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond21 == 1 | hcondever21 == 1 | hconds21 == 1, cond21_copd := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond21 == 1 | hcondever21 == 1) & hconds21 == 2, cond21_copd := 0]

    # 4. Apply Missingness Rule
    data[(hcond21 == 1 | hcondever21 == 1) & (hconds21 < 0 | is.na(hconds21)), cond21_copd := NA]

    # 5. Construct Lifetime History (ever_chronic_bronchitis)
    data[hcond21 >= 0 | hcondever21 >= 0 | hconds21 > 0, cond21_copd_ever := 0]
    data[hcond21 == 1 | hcondever21 == 1 | hconds21 %in% c(1,2), cond21_copd_ever := 1]

    ## ========= CARDIOVASCULAR CONDITIONS ================================== ##

    #### ---------- (3) Congestive heart failure --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond3 >= 0 | hcondever3 >= 0 | hconds03 > 0, cond3_congestive_heart_failure := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond3 == 1 | hcondever3 == 1 | hconds03 == 1, cond3_congestive_heart_failure := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond3 == 1 | hcondever3 == 1) & hconds03 == 2, cond3_congestive_heart_failure := 0]

    # 4. Apply Missingness Rule
    data[(hcond3 == 1 | hcondever3 == 1) & (hconds03 < 0 | is.na(hconds03)), cond3_congestive_heart_failure := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond3 >= 0 | hcondever3 >= 0 | hconds03 > 0, cond3_congestive_heart_failure_ever := 0]
    data[hcond3 == 1 | hcondever3 == 1 | hconds03 %in% c(1,2), cond3_congestive_heart_failure_ever := 1]

    #### ---------- (4) Coronary heart disease --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond4 >= 0 | hcondever4 >= 0 | hconds04 > 0, cond4_coronary_heart_disease := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond4 == 1 | hcondever4 == 1 | hconds04 == 1, cond4_coronary_heart_disease := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond4 == 1 | hcondever4 == 1) & hconds04 == 2, cond4_coronary_heart_disease := 0]

    # 4. Apply Missingness Rule
    data[(hcond4 == 1 | hcondever4 == 1) & (hconds04 < 0 | is.na(hconds04)), cond4_coronary_heart_disease := NA]

    # 5. Construct Lifetime History
    data[hcond4 >= 0 | hcondever4 >= 0 | hconds04 > 0, cond4_coronary_heart_disease_ever := 0]
    data[hcond4 == 1 | hcondever4 == 1 | hconds04 %in% c(1,2), cond4_coronary_heart_disease_ever := 1]

    #### ---------- (5) Angina --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond5 >= 0 | hcondever5 >= 0 | hconds05 > 0, cond5_angina := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond5 == 1 | hcondever5 == 1 | hconds05 == 1, cond5_angina := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond5 == 1 | hcondever5 == 1) & hconds05 == 2, cond5_angina := 0]

    # 4. Apply Missingness Rule
    data[(hcond5 == 1 | hcondever5 == 1) & (hconds05 < 0 | is.na(hconds05)), cond5_angina := NA]

    # 5. Construct Lifetime History
    data[hcond5 >= 0 | hcondever5 >= 0 | hconds05 > 0, cond5_angina_ever := 0]
    data[hcond5 == 1 | hcondever5 == 1 | hconds05 %in% c(1,2), cond5_angina_ever := 1]

    #### ---------- (16) High blood pressure --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond16 >= 0 | hcondever16 >= 0 | hconds16 > 0, cond16_high_bp := 0]

    # 2. Assign active prevalence (New/Returning reported OR returning confirmed)
    data[hcond16 == 1 | hcondever16 == 1 | hconds16 == 1, cond16_high_bp := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond16 == 1 | hcondever16 == 1) & hconds16 == 2, cond16_high_bp := 0]

    # 4. Apply Missingness Rule
    data[(hcond16 == 1 | hcondever16 == 1) & (hconds16 < 0 | is.na(hconds16)), cond16_high_bp := NA]

    # 5. Construct Lifetime History
    data[hcond16 >= 0 | hcondever16 >= 0 | hconds16 > 0, cond16_high_bp_ever := 0]
    data[hcond16 == 1 | hcondever16 == 1 | hconds16 %in% c(1,2), cond16_high_bp_ever := 1]

    ## ========= METABOLIC AND ENDOCRINE CONDITIONS ========================= ##

    #### ---------- (10) Hypothyroidism ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond10 >= 0 | hcondever10 >= 0 | hconds10 > 0, cond10_hypothyroidism := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond10 == 1 | hcondever10 == 1 | hconds10 == 1, cond10_hypothyroidism := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond10 == 1 | hcondever10 == 1) & hconds10 == 2, cond10_hypothyroidism := 0]

    # 4. Apply Missingness Rule
    data[(hcond10 == 1 | hcondever10 == 1) & (hconds10 < 0 | is.na(hconds10)), cond10_hypothyroidism := NA]

    # 5. Construct Lifetime History (ever_hypothyroidism)
    data[hcond10 >= 0 | hcondever10 >= 0 | hconds10 > 0, cond10_hypothyroidism_ever := 0]
    data[hcond10 == 1 | hcondever10 == 1 | hconds10 %in% c(1,2), cond10_hypothyroidism_ever := 1]

    #### ---------- (9) Hyperthyroidism ------ ####

    data[, cond9_hyperthyroidism := NA]
    data[, cond9_hyperthyroidism_ever := NA]

    #### ---------- (14) Diabetes ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond14 >= 0 | hcondever14 >= 0 | hconds33 > 0 | hconds34 > 0 | hconds35 > 0, cond14_diabetes := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond14 == 1 | hcondever14 == 1 | hconds33 == 1 | hconds34 == 1 | hconds35 == 1, cond14_diabetes := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond14 == 1 | hcondever14 == 1) & (hconds33 == 2 | hconds34 == 2 | hconds35 == 2), cond14_diabetes := 0]

    # 4. Apply Missingness Rule
    data[(hcond14 == 1 | hcondever14 == 1) &
           (hconds33 < 0 | is.na(hconds33)) &
           (hconds34 < 0 | is.na(hconds34)) &
           (hconds35 < 0 | is.na(hconds35)) , cond14_diabetes := NA]

    # 5. Construct Lifetime History (ever_arthritis)
    data[hcond14 >= 0 | hcondever14 >= 0 | hconds33 > 0 | hconds34 > 0 | hconds35 > 0, cond14_diabetes_ever := 0]
    data[hcond14 == 1 | hcondever14 == 1 | hconds33 %in% c(1, 2) | hconds34 %in% c(1, 2) | hconds35 %in% c(1, 2), cond14_diabetes_ever := 1]

    ## ========= MUSCULOSKELETAL CONDITIONS ================================= ##

    #### ---------- (2) Arthritis --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond2 >= 0 | hcondever2 >= 0 | hconds23 > 0 | hconds24 > 0, cond2_arthritis := 0]

    # 2. Assign active prevalence
    data[hcond2 == 1 | hcondever2 == 1 | hconds23 == 1 | hconds24 == 1, cond2_arthritis := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond2 == 1 | hcondever2 == 1) & (hconds23 == 2 | hconds24 == 2), cond2_arthritis := 0]

    # 4. Apply Missingness Rule
    data[(hcond2 == 1 | hcondever2 == 1)&
           (hconds23 < 0 | is.na(hconds23)) &
           (hconds24 < 0 | is.na(hconds24)), cond2_arthritis := NA]

    # 5. Construct Lifetime History (ever_arthritis)
    data[hcond2 >= 0 | hcondever2 >= 0 | hconds23 > 0 | hconds24 > 0, cond2_arthritis_ever := 0]
    data[hcond2 == 1 | hcondever2 == 1 | hconds23 %in% c(1, 2) | hconds24 %in% c(1, 2), cond2_arthritis_ever := 1]

    ## ========= NEUROLOGICAL CONDITIONS ================================= ##

    #### ---------- (15) Epilepsy --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond15 >= 0 | hcondever15 >= 0 | hconds15 > 0, cond15_epilepsy := 0]

    # 2. Assign active prevalence
    data[hcond15 == 1 | hcondever15 == 1 | hconds15 == 1, cond15_epilepsy := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond15 == 1 | hcondever15 == 1) & hconds15 == 2, cond15_epilepsy := 0]

    # 4. Apply Missingness Rule
    data[(hcond15 == 1 | hcondever15 == 1) & (hconds15 < 0 | is.na(hconds15)), cond15_epilepsy := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond15 >= 0 | hcondever15 >= 0 | hconds15 > 0, cond15_epilepsy_ever := 0]
    data[hcond15 == 1 | hcondever15 == 1 | hconds15 %in% c(1,2), cond15_epilepsy_ever := 1]

    ## ========= OTHER CONDITIONS ================================= ##

    #### ---------- (13) Cancer --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond13 >= 0 | hcondever13 >= 0 | hconds26 > 0 | hconds27 > 0 | hconds28 > 0 | hconds29 > 0 | hconds30 > 0 | hconds31 > 0, cond13_cancer := 0]

    # 2. Assign active prevalence
    data[hcond13 == 1 | hcondever13 == 1 | hconds26 == 1 | hconds27 == 1 | hconds28 == 1 | hconds29 == 1 | hconds30 == 1 | hconds31 == 1, cond13_cancer := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond13 == 1 | hcondever13 == 1) & (hconds26 == 2 | hconds27 == 2 | hconds28 == 2 | hconds29 == 2 | hconds30 == 2 | hconds31 == 2), cond13_cancer := 0]

    # 4. Apply Missingness Rule
    data[(hcond13 == 1 | hcondever13 == 1) &
           (hconds26 < 0 | is.na(hconds26)) &
           (hconds27 < 0 | is.na(hconds27)) &
           (hconds28 < 0 | is.na(hconds28)) &
           (hconds29 < 0 | is.na(hconds29)) &
           (hconds30 < 0 | is.na(hconds30)) &
           (hconds31 < 0 | is.na(hconds31)), cond13_cancer := NA]

    # 5. Construct Lifetime History (ever_cancer)
    data[hcond13 >= 0 | hcondever13 >= 0 | hconds26 > 0 | hconds27 > 0 | hconds28 > 0 | hconds29 > 0 | hconds30 > 0 | hconds31 > 0, cond13_cancer_ever := 0]
    data[hcond13 == 1 | hcondever13 == 1 | hconds26 %in% c(1, 2) | hconds27 %in% c(1, 2) | hconds28 %in% c(1, 2) | hconds29 %in% c(1, 2) | hconds30 %in% c(1, 2) | hconds31 %in% c(1, 2), cond13_cancer_ever := 1]

    #### ---------- (12) Liver conditions --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond12 >= 0 | hcondever12 >= 0 | hconds12 > 0, cond12_liver_conditions := 0]

    # 2. Assign active prevalence
    data[hcond12 == 1 | hcondever12 == 1 | hconds12 == 1, cond12_liver_conditions := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[(hcond12 == 1 | hcondever12 == 1) & hconds12 == 2, cond12_liver_conditions := 0]

    # 4. Apply Missingness Rule
    data[(hcond12 == 1 | hcondever12 == 1) & (hconds12 < 0 | is.na(hconds12)), cond12_liver_conditions := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond12 >= 0 | hcondever12 >= 0 | hconds12 > 0, cond12_liver_conditions_ever := 0]
    data[hcond12 == 1 | hcondever12 == 1 | hconds12 %in% c(1,2), cond12_liver_conditions_ever := 1]


  }

  # =========================================================================
  # 6. UPDATING HEALTH CONDITIONS THROUGH WAVES 11-13
  # =========================================================================

  if (is_wave11to13){

    ## ========= RESPIRATORY CONDITIONS ================================== ##

    #### ---------- (1) Asthma --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond1 >= 0 | hconds01 > 0 | hcondnew1 >= 0, cond1_asthma := 0]

    # 2. Assign active prevalence
    data[hcond1 == 1 | hconds01 == 1 | hcondnew1 == 1, cond1_asthma := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond1 == 1 & hconds01 == 2, cond1_asthma := 0]

    # 4. Apply Missingness Rule
    data[hcond1 == 1 & (hconds01 < 0 | is.na(hconds01)), cond1_asthma := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond1 >= 0 | hcondnew1 >= 0 | hconds01 > 0, cond1_asthma_ever := 0]
    data[hcond1 == 1 | hcondnew1 == 1 | hconds01 %in% c(1, 2), cond1_asthma_ever := 1]

    #### ---------- (8) Emphysema ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond8 >= 0 | hconds08 > 0 | hcondnew8 >= 0, cond8_emphysema := 0]

    # 2. Assign active prevalence
    data[hcond8 == 1 | hconds08 == 1 | hcondnew8 == 1, cond8_emphysema := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond8 == 1 & hconds08 == 2, cond8_emphysema := 0]

    # 4. Apply Missingness Rule
    data[hcond8 == 1 & (hconds08 < 0 | is.na(hconds08)), cond8_emphysema := NA]

    # 5. Construct Lifetime History (ever_emphysema)
    data[hcond8 >= 0 | hcondnew8 >= 0 | hconds08 > 0, cond8_emphysema_ever := 0]
    data[hcond8 == 1 | hcondnew8 == 1 | hconds08 %in% c(1, 2), cond8_emphysema_ever := 1]

    #### ---------- (11) Bronchitis ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond11 >= 0 | hconds11 > 0 | hcondnew11 >= 0, cond11_chronic_bronchitis := 0]

    # 2. Assign active prevalence
    data[hcond11 == 1 | hconds11 == 1 | hcondnew11 == 1, cond11_chronic_bronchitis := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond11 == 1 & hconds11 == 2, cond11_bronchitis := 0]

    # 4. Apply Missingness Rule
    data[hcond11 == 1 & (hconds11 < 0 | is.na(hconds11)), cond11_chronic_bronchitis := NA]

    # 5. Construct Lifetime History (ever_bronchitis)
    data[hcond11 >= 0 | hcondnew11 >= 0 | hconds11 > 0, cond11_chronic_bronchitis_ever := 0]
    data[hcond11 == 1 | hcondnew11 == 1 | hconds11 %in% c(1, 2), cond11_chronic_bronchitis_ever := 1]

    #### ---------- (21) COPD ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond21 >= 0 | hconds21 > 0 | hcondnew21 >= 0, cond21_copd := 0]

    # 2. Assign active prevalence
    data[hcond21 == 1 | hconds21 == 1 | hcondnew21 == 1, cond21_copd := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond21 == 1 & hconds21 == 2, cond21_bronchitis := 0]

    # 4. Apply Missingness Rule
    data[hcond21 == 1 & (hconds21 < 0 | is.na(hconds21)), cond21_copd := NA]

    # 5. Construct Lifetime History (ever_bronchitis)
    data[hcond21 >= 0 | hcondnew21 >= 0 | hconds21 > 0, cond21_copd_ever := 0]
    data[hcond21 == 1 | hcondnew21 == 1 | hconds21 %in% c(1, 2), cond21_copd_ever := 1]

    ## ========= CARDIOVASCULAR CONDITIONS ================================== ##

    #### ---------- (3) Congestive heart failure --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond3 >= 0 | hconds03 > 0 | hcondnew3 >= 0, cond3_congestive_heart_failure := 0]

    # 2. Assign active prevalence
    data[hcond3 == 1 | hconds03 == 1 | hcondnew3 == 1, cond3_congestive_heart_failure := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond3 == 1 & hconds03 == 2, cond3_asthma := 0]

    # 4. Apply Missingness Rule
    data[hcond3 == 1 & (hconds03 < 0 | is.na(hconds03)), cond3_congestive_heart_failure := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond3 >= 0 | hcondnew3 >= 0 | hconds03 > 0, cond3_congestive_heart_failure_ever := 0]
    data[hcond3 == 1 | hcondnew3 == 1 | hconds03 %in% c(1, 2), cond3_congestive_heart_failure_ever := 1]

    #### ---------- (4) Coronary heart disease --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond4 >= 0 | hconds04 > 0 | hcondnew4 >= 0, cond4_coronary_heart_disease := 0]

    # 2. Assign active prevalence
    data[hcond4 == 1 | hconds04 == 1 | hcondnew4 == 1, cond4_coronary_heart_disease := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond4 == 1 & hconds04 == 2, cond4_asthma := 0]

    # 4. Apply Missingness Rule
    data[hcond4 == 1 & (hconds04 < 0 | is.na(hconds04)), cond4_coronary_heart_disease := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond4 >= 0 | hcondnew4 >= 0 | hconds04 > 0, cond4_coronary_heart_disease_ever := 0]
    data[hcond4 == 1 | hcondnew4 == 1 | hconds04 %in% c(1, 2), cond4_coronary_heart_disease_ever := 1]

    #### ---------- (5) Angina --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond5 >= 0 | hconds05 > 0 | hcondnew5 >= 0, cond5_angina := 0]

    # 2. Assign active prevalence
    data[hcond5 == 1 | hconds05 == 1 | hcondnew5 == 1, cond5_angina := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond5 == 1 & hconds05 == 2, cond5_asthma := 0]

    # 4. Apply Missingness Rule
    data[hcond5 == 1 & (hconds05 < 0 | is.na(hconds05)), cond5_angina := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond5 >= 0 | hcondnew5 >= 0 | hconds05 > 0, cond5_angina_ever := 0]
    data[hcond5 == 1 | hcondnew5 == 1 | hconds05 %in% c(1, 2), cond5_angina_ever := 1]

    #### ---------- (16) High blood pressure --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond16 >= 0 | hconds16 > 0 | hcondnew16 >= 0, cond16_high_bp := 0]

    # 2. Assign active prevalence
    data[hcond16 == 1 | hconds16 == 1 | hcondnew16 == 1, cond16_high_bp := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond16 == 1 & hconds16 == 2, cond16_asthma := 0]

    # 4. Apply Missingness Rule
    data[hcond16 == 1 & (hconds16 < 0 | is.na(hconds16)), cond16_high_bp := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond16 >= 0 | hcondnew16 >= 0 | hconds16 > 0, cond16_high_bp_ever := 0]
    data[hcond16 == 1 | hcondnew16 == 1 | hconds16 %in% c(1, 2), cond16_high_bp_ever := 1]

    ## ========= METABOLIC AND ENDOCRINE CONDITIONS ========================= ##

    #### ---------- (10) Hypothyroidism ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond10 >= 0 | hconds10 > 0 | hcondnew10 >= 0, cond10_hypothyroidism := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond10 == 1 & hconds10 == 1| hcondnew10 == 1, cond10_hypothyroidism := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond10 == 1 & hconds10 == 2, cond10_hypothyroidism := 0]

    # 4. Apply Missingness Rule
    data[hcond10 == 1 & (hconds10 < 0 | is.na(hconds10)), cond10_hypothyroidism := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond10 >= 0 | hcondnew10 >= 0, cond10_hypothyroidism_ever := 0]
    data[hcond10 == 1 | hcondnew10 == 1, cond10_hypothyroidism_ever := 1]

    #### ---------- (9) Hyperthyroidism --------- ####

    data[, cond9_hyperthyroidism := NA]
    data[, cond9_hyperthyroidism_ever := NA]

    #### ---------- (14) Diabetes ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond14 >= 0 | hconds33 > 0 | hconds34 > 0 | hconds35 > 0 | hcondnew14 >= 0, cond14_diabetes := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond14 == 1 & hconds33 == 1 | hconds34 == 1 | hconds35 == 1 | hcondnew14 == 1, cond14_diabetes := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond14 == 1 & (hconds33 == 2 | hconds34 == 2 | hconds35 == 2), cond14_diabetes := 0]

    # 4. Apply Missingness Rule
    data[hcond14 == 1 &
           (hconds33 < 0 | is.na(hconds33)) &
           (hconds34 < 0 | is.na(hconds34)) &
           (hconds35 < 0 | is.na(hconds35)) , cond14_diabetes := NA]

    # 5. Construct Lifetime History (ever_arthritis)
    data[hcond14 >= 0 | hconds33 > 0 | hconds34 > 0 | hconds35 > 0, cond14_diabetes_ever := 0]
    data[hcond14 == 1 | hconds33 %in% c(1, 2) | hconds34 %in% c(1, 2) | hconds35 %in% c(1, 2), cond14_diabetes_ever := 1]

    ## ========= MUSCULOSKELETAL CONDITIONS ================================= ##

    #### ---------- (2) Arthritis --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond2 >= 0 | hconds23 > 0 | hconds24 > 0 | hcondnew2 >= 0, cond2_arthritis := 0]

    # 2. Assign active prevalence
    data[hcond2 == 1 | hconds23 == 1 | hconds24 == 1 | hcondnew2 == 1, cond2_arthritis := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond2 == 1 & (hconds23 == 2 | hconds24 == 2), cond2_arthritis := 0]

    # 4. Apply Missingness Rule
    data[hcond2 == 1 &
           (hconds23 < 0 | is.na(hconds23)) &
           (hconds24 < 0 | is.na(hconds24)), cond2_arthritis := NA]

    # 5. Construct Lifetime History (ever_arthritis)
    data[hcond2 >= 0 | hcondnew2 >= 0 | hconds23 > 0 | hconds24 > 0, cond2_arthritis_ever := 0]
    data[hcond2 == 1 | hcondnew2 == 1 | hconds23 %in% c(1, 2) | hconds24 %in% c(1, 2), cond2_arthritis_ever := 1]

    ## ========= NEUROLOGICAL CONDITIONS ========================== ##

    #### ---------- (15) Epilepsy --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond15 >= 0 | hconds15 > 0 | hcondnew15 >= 0, cond15_epilepsy := 0]

    # 2. Assign active prevalence
    data[hcond15 == 1 | hconds15 == 1 | hcondnew15 == 1, cond15_epilepsy := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond15 == 1 & hconds15 == 2, cond15_epilepsy := 0]

    # 4. Apply Missingness Rule
    data[hcond15 == 1 & (hconds15 < 0 | is.na(hconds15)), cond15_epilepsy := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond15 >= 0 | hcondnew15 >= 0 | hconds15 > 0, cond15_epilepsy_ever := 0]
    data[hcond15 == 1 | hcondnew15 == 1 | hconds15 %in% c(1, 2), cond15_epilepsy_ever := 1]

    ## ========= OTHER CONDITIONS ================================= ##

    #### ---------- (13) Cancer --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond13 >= 0 | hconds26 > 0 | hconds27 > 0 | hconds28 > 0 | hconds29 > 0 | hconds30 > 0 | hconds31 > 0 | hcondnew13 >= 0, cond13_cancer := 0]

    # 2. Assign active prevalence
    data[hcond13 == 1 | hconds26 == 1 | hconds27 == 1 | hconds28 == 1 | hconds29 == 1 | hconds30 == 1 | hconds31 == 1 | hcondnew13 == 1, cond13_cancer := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond13 == 1 & (hconds26 == 2 | hconds27 == 2 | hconds28 == 2 | hconds29 == 2 | hconds30 == 2 | hconds31 == 2), cond13_cancer := 0]

    # 4. Apply Missingness Rule
    data[hcond13 == 1 &
           (hconds26 < 0 | is.na(hconds26)) &
           (hconds27 < 0 | is.na(hconds27)) &
           (hconds28 < 0 | is.na(hconds28)) &
           (hconds29 < 0 | is.na(hconds29)) &
           (hconds30 < 0 | is.na(hconds30)) &
           (hconds31 < 0 | is.na(hconds31)), cond13_cancer := NA]

    # 5. Construct Lifetime History (ever_cancer)
    data[hcond13 >= 0 | hcondnew13 >= 0 | hconds26 > 0 | hconds27 > 0 | hconds28 > 0 | hconds29 > 0 | hconds30 > 0 | hconds31 > 0, cond13_cancer_ever := 0]
    data[hcond13 == 1 | hcondnew13 == 1 | hconds26 %in% c(1, 2) | hconds27 %in% c(1, 2) | hconds28 %in% c(1, 2) | hconds29 %in% c(1, 2) | hconds30 %in% c(1, 2) | hconds31 %in% c(1, 2), cond13_cancer_ever := 1]

    #### ---------- (12) Liver conditions --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond12 >= 0 | hconds12 > 0 | hcondnew12 >= 0, cond12_liver_conditions := 0]

    # 2. Assign active prevalence
    data[hcond12 == 1 | hconds12 == 1 | hcondnew12 == 1, cond12_liver_conditions := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond12 == 1 & hconds12 == 2, cond12_liver_conditions := 0]

    # 4. Apply Missingness Rule
    data[hcond12 == 1 & (hconds12 < 0 | is.na(hconds12)), cond12_liver_conditions := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond12 >= 0 | hcondnew12 >= 0 | hconds12 > 0, cond12_liver_conditions_ever := 0]
    data[hcond12 == 1 | hcondnew12 == 1 | hconds12 %in% c(1, 2), cond12_liver_conditions_ever := 1]

  }

  # =========================================================================
  # 7. UPDATING HEALTH CONDITIONS THROUGH WAVES 14-15 (permanence)
  # =========================================================================

  if (is_wave14to15){

    ## ========= RESPIRATORY CONDITIONS ================================== ##

    #### ---------- (1) Asthma --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond1 >= 0 | hconds01 > 0 | hcondnew1 >= 0, cond1_asthma := 0]

    # 2. Assign active prevalence
    data[hcond1 == 1 | hconds01 == 1 | hcondnew1 == 1, cond1_asthma := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond1 == 1 & hconds01 == 2, cond1_asthma := 0]

    # 4. Apply Missingness Rule
    data[hcond1 == 1 & (hconds01 < 0 | is.na(hconds01)), cond1_asthma := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond1 >= 0 | hconds01 > 0 | hcondnew1 >= 0, cond1_asthma_ever := 0]
    data[hcond1 == 1 | hcondnew1 == 1 | hconds01 %in% c(1, 2), cond1_asthma_ever := 1]

    #### ---------- (8) Emphysema ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond8 >= 0 | hconds08 > 0 | hcondnew8 >= 0, cond8_emphysema := 0]

    # 2. Assign active prevalence
    data[hcond8 == 1 | hconds08 == 1 | hcondnew8 == 1, cond8_emphysema := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond8 == 1 & hconds08 == 2, cond8_emphysema := 0]

    # 4. Apply Missingness Rule
    data[hcond8 == 1 & (hconds08 < 0 | is.na(hconds08)), cond8_emphysema := NA]

    # 5. Construct Lifetime History (ever_emphysema)
    data[hcond8 >= 0 | hconds08 > 0 | hcondnew8 >= 0, cond8_emphysema_ever := 0]
    data[hcond8 == 1 | hcondnew8 == 1 | hconds08 %in% c(1, 2), cond8_emphysema_ever := 1]

    #### ---------- (11) Bronchitis ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond11 >= 0 | hconds11 > 0 | hcondnew8 >= 0, cond11_chronic_bronchitis := 0]

    # 2. Assign active prevalence
    data[hcond11 == 1 | hconds11 == 1 | hcondnew8 == 1, cond11_chronic_bronchitis := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond11 == 1 & hconds11 == 2, cond11_bronchitis := 0]

    # 4. Apply Missingness Rule
    data[hcond11 == 1 & (hconds11 < 0 | is.na(hconds11)), cond11_chronic_bronchitis := NA]

    # 5. Construct Lifetime History (ever_bronchitis)
    data[hcond11 >= 0 | hconds11 > 0 | hcondnew8 >= 0, cond11_chronic_bronchitis_ever := 0]
    data[hcond11 == 1 | hcondnew11 == 1 | hconds11 %in% c(1, 2), cond11_chronic_bronchitis_ever := 1]

    #### ---------- (21) COPD ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond21 >= 0 | hconds21 > 0 | hcondnew21 >= 0, cond21_copd := 0]

    # 2. Assign active prevalence
    data[hcond21 == 1 | hconds21 == 1 | hcondnew21 == 1, cond21_copd := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond21 == 1 & hconds21 == 2, cond21_bronchitis := 0]

    # 4. Apply Missingness Rule
    data[hcond21 == 1 & (hconds21 < 0 | is.na(hconds21)), cond21_copd := NA]

    # 5. Construct Lifetime History (ever_bronchitis)
    data[hcond21 >= 0 | hconds21 > 0 | hcondnew21 >= 0, cond21_copd_ever := 0]
    data[hcond21 == 1 | hcondnew21 == 1 | hconds21 %in% c(1, 2), cond21_copd_ever := 1]

    ## ========= CARDIOVASCULAR CONDITIONS ================================== ##

    #### ---------- (3) Congestive heart failure --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond3 >= 0 | hconds03 > 0 | hcondnew3 >= 0, cond3_congestive_heart_failure := 0]

    # 2. Assign active prevalence
    data[hcond3 == 1 | hconds03 == 1 | hcondnew3 == 1, cond3_congestive_heart_failure := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond3 == 1 & hconds03 == 2, cond3_asthma := 0]

    # 4. Apply Missingness Rule
    data[hcond3 == 1 & (hconds03 < 0 | is.na(hconds03)), cond3_congestive_heart_failure := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond3 >= 0 | hcondnew3 >= 0 | hconds03 > 0, cond3_congestive_heart_failure_ever := 0]
    data[hcond3 == 1 | hcondnew3 == 1 | hconds03 %in% c(1, 2), cond3_congestive_heart_failure_ever := 1]

    #### ---------- (4) Coronary heart disease --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond4 >= 0 | hconds04 > 0 | hcondnew4 >= 0, cond4_coronary_heart_disease := 0]

    # 2. Assign active prevalence
    data[hcond4 == 1 | hconds04 == 1 | hcondnew4 == 1, cond4_coronary_heart_disease := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond4 == 1 & hconds04 == 2, cond4_asthma := 0]

    # 4. Apply Missingness Rule
    data[hcond4 == 1 & (hconds04 < 0 | is.na(hconds04)), cond4_coronary_heart_disease := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond4 >= 0 | hcondnew4 >= 0 | hconds04 > 0, cond4_coronary_heart_disease_ever := 0]
    data[hcond4 == 1 | hcondnew4 == 1 | hconds04 %in% c(1, 2), cond4_coronary_heart_disease_ever := 1]

    #### ---------- (5) Angina --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond5 >= 0 | hconds05 > 0 | hcondnew5 >= 0, cond5_angina := 0]

    # 2. Assign active prevalence
    data[hcond5 == 1 | hconds05 == 1 | hcondnew5 == 1, cond5_angina := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond5 == 1 & hconds05 == 2, cond5_asthma := 0]

    # 4. Apply Missingness Rule
    data[hcond5 == 1 & (hconds05 < 0 | is.na(hconds05)), cond5_angina := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond5 >= 0 | hcondnew5 >= 0 | hconds05 > 0, cond5_angina_ever := 0]
    data[hcond5 == 1 | hcondnew5 == 1 | hconds05 %in% c(1, 2), cond5_angina_ever := 1]

    #### ---------- (16) High blood pressure --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond16 >= 0 | hconds16 > 0 | hcondnew16 >= 0, cond16_high_bp := 0]

    # 2. Assign active prevalence
    data[hcond16 == 1 | hconds16 == 1 | hcondnew16 == 1, cond16_high_bp := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond16 == 1 & hconds16 == 2, cond16_asthma := 0]

    # 4. Apply Missingness Rule
    data[hcond16 == 1 & (hconds16 < 0 | is.na(hconds16)), cond16_high_bp := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond16 >= 0 | hcondnew16 >= 0 | hconds16 > 0, cond16_high_bp_ever := 0]
    data[hcond16 == 1 | hcondnew16 == 1 | hconds16 %in% c(1, 2), cond16_high_bp_ever := 1]

    ## ========= METABOLIC AND ENDOCRINE CONDITIONS ========================= ##

    #### ---------- (10) Hypothyroidism ------ ####

    # 1. Initialize valid survey universe to 0
    data[hcond10 >= 0 | hcondnew10 >= 0, cond10_hypothyroidism := 0]

    # 2. Assign active prevalence (Must be ever diagnosed AND still have it)
    data[hcond10 == 1 & hcondnew10 == 1, cond10_hypothyroidism := 1]

    # 5. Construct Lifetime History (ever_hypothyroidism)
    data[hcond10 >= 0 | hcondnew10 >= 0, cond10_hypothyroidism_ever := 0]
    data[hcond10 == 1 | hcondnew10 == 1, cond10_hypothyroidism_ever := 1]

    #### ---------- (14) Diabetes --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond14 >= 0 | hcondnew14 >= 0, cond14_diabetes := 0]

    # 2. Assign active prevalence
    data[hcond14 == 1 | hcondnew14 == 1, cond14_diabetes := 1]

    # 5. Construct Lifetime History (ever_arthritis)
    data[hcond14 >= 0 | hcondnew14 >= 0, cond14_diabetes_ever := 0]
    data[hcond14 == 1 | hcondnew14 == 1, cond14_diabetes_ever := 1]

    ## ========= MUSCULOSKELETAL CONDITIONS ================================= ##

    #### ---------- (2) Arthritis --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond2 >= 0 | hcondnew2 >= 0, cond2_arthritis := 0]

    # 2. Assign active prevalence
    data[hcond2 == 1 | hcondnew2 == 1, cond2_arthritis := 1]

    # 5. Construct Lifetime History (ever_arthritis)
    data[hcond2 >= 0 | hcondnew2 >= 0, cond2_arthritis_ever := 0]
    data[hcond2 == 1 | hcondnew2 == 1, cond2_arthritis_ever := 1]

    ## ========= NEUROLOGICAL CONDITIONS ========================== ##

    #### ---------- (15) Epilepsy --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond15 >= 0 | hconds15 > 0 | hcondnew15 >= 0, cond15_epilepsy := 0]

    # 2. Assign active prevalence
    data[hcond15 == 1 | hconds15 == 1 | hcondnew15 == 1, cond15_epilepsy := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond15 == 1 & hconds15 == 2, cond15_epilepsy := 0]

    # 4. Apply Missingness Rule
    data[hcond15 == 1 & (hconds15 < 0 | is.na(hconds15)), cond15_epilepsy := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond15 >= 0 | hcondnew15 >= 0 | hconds15 > 0, cond15_epilepsy_ever := 0]
    data[hcond15 == 1 | hcondnew15 == 1 | hconds15 %in% c(1, 2), cond15_epilepsy_ever := 1]

    ## ========= OTHER CONDITIONS ================================= ##

    #### ---------- (13) Cancer --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond13 >= 0 | hcondnew13 >= 0, cond13_cancer := 0]

    # 2. Assign active prevalence
    data[hcond13 == 1 | hcondnew13 == 1, cond13_cancer := 1]

    # 5. Construct Lifetime History (ever_arthritis)
    data[hcond13 >= 0 | hcondnew13 >= 0, cond13_cancer_ever := 0]
    data[hcond13 == 1 | hcondnew13 == 1, cond13_cancer_ever := 1]

    #### ---------- (12) Liver conditions --------- ####

    # 1. Initialize valid survey universe to 0
    data[hcond12 >= 0 | hconds12 > 0 | hcondnew12 >= 0, cond12_liver_conditions := 0]

    # 2. Assign active prevalence
    data[hcond12 == 1 | hconds12 == 1 | hcondnew12 == 1, cond12_liver_conditions := 1]

    # 3. Correct the New Entrant Trap (Override past-only conditions)
    data[hcond12 == 1 & hconds12 == 2, cond12_liver_conditions := 0]

    # 4. Apply Missingness Rule
    data[hcond12 == 1 & (hconds12 < 0 | is.na(hconds12)), cond12_liver_conditions := NA]

    # 5. Construct Lifetime History (ever_asthma)
    data[hcond12 >= 0 | hcondnew12 >= 0 | hconds12 > 0, cond12_liver_conditions_ever := 0]
    data[hcond12 == 1 | hcondnew12 == 1 | hconds12 %in% c(1, 2), cond12_liver_conditions_ever := 1]

  }

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "id", "hidp", "wave_no",
                         ## current prevalence
                         "cond1_asthma","cond8_emphysema","cond11_chronic_bronchitis","cond21_copd",
                         "cond3_congestive_heart_failure","cond4_coronary_heart_disease","cond5_angina","cond16_high_bp",
                         "cond10_hypothyroidism","cond14_diabetes",
                         "cond2_arthritis",
                         "cond15_epilepsy",
                         "cond13_cancer","cond12_liver_conditions"
                        )]

  var_names <- c(
    ## current prevalence
    "cond1_asthma","cond8_emphysema","cond11_chronic_bronchitis","cond21_copd",
    "cond3_congestive_heart_failure","cond4_coronary_heart_disease","cond5_angina","cond16_high_bp",
    "cond10_hypothyroidism","cond14_diabetes",
    "cond2_arthritis",
    "cond15_epilepsy",
    "cond13_cancer","cond12_liver_conditions"
  )

  setnames(final_data, var_names, paste0("h_", var_names))


  return(final_data)

}

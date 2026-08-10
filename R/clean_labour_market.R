#' Clean Labour Market
#'
#' Generate three variables indicating economic status at different levels of detail,
#' and earnings variables. Variables with "pay" in the name relate to earnings from employment,
#' "earnings" refers to employed and self employed earnings combined.
#'
#' @param data Data table. Understanding Society data produced using the read functions.
#' @param inflation Data table. CPIH inflation data input for real-terms adjustments.
#'
#' @export
clean_labour_market <- function(data = NULL,
                                 inflation = ukhlsclean::cpih) {

  ### 2 categories - work/not work #####

  data[jbstat %in% 1:2        , econ_stat_2cat := "employed"]
  data[jbstat %in% c(3:14,97) , econ_stat_2cat := "not_employed"]

  data[,econ_stat_2cat := factor(econ_stat_2cat,
                                 levels = c("employed","not_employed"),
                                 labels = c("employed","not_employed"))]

  ### 3 categories - employed/unemployed/inactive

  data[jbstat %in% 1:2        , econ_stat_3cat := "employed"]
  data[jbstat %in% 3          , econ_stat_3cat := "unemployed"]
  data[jbstat %in% c(4:14,97) , econ_stat_3cat := "inactive"]

  data[,econ_stat_3cat := factor(econ_stat_3cat,
                                 levels = c("employed","unemployed","inactive"),
                                 labels = c("employed","unemployed","inactive"))]

  ### 7 categories

  data[jbstat == 1 , econ_stat_7cat := "self_employed"]
  data[jbstat == 2 , econ_stat_7cat := "employed"]
  data[jbstat == 3 , econ_stat_7cat := "unemployed"]
  data[jbstat == 4 , econ_stat_7cat := "retired"]
  data[jbstat %in% 7 , econ_stat_7cat := "education"]
  data[jbstat %in% 8 , econ_stat_7cat := "sick"]
  data[jbstat %in% c(5:6,9:14,97) , econ_stat_7cat := "other"]

  data[,econ_stat_7cat := factor(econ_stat_7cat,
                                 levels = c("employed","self_employed","unemployed","sick","retired","education","other"),
                                 labels = c("employed","self_employed","unemployed","sick","retired","education","other"))]

  ##################
  ## RETAIN THE CLEANED VARIABLES

  final_data <- data[, c("pidp", "hidp", "wave_no",
                          "econ_stat_2cat", "econ_stat_3cat", "econ_stat_7cat")]

  var_names <- c("econ_stat_2cat", "econ_stat_3cat", "econ_stat_7cat")

  setnames(final_data, var_names, paste0("l_", var_names))


  return(final_data)
}

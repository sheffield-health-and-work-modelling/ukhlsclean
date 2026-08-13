#' Combine waves of UKHLS
#'
#' Combines years of data when provided as a list of data tables.
#'
#' @param data_list List of data tables to combine.
#'
#' @export
combine_waves <- function(
  data_list
) {

  # combine all the data tables
  data <- data.table::rbindlist(data_list, use.names = T, fill = T)

  # order rows and columns
  data <- data[order(pidp, wave_no),]

  # create flags for each wave

  data[, wave_1  := ifelse(wave_no == 1,1,0)]
  data[, wave_2  := ifelse(wave_no == 2,1,0)]
  data[, wave_3  := ifelse(wave_no == 3,1,0)]
  data[, wave_4  := ifelse(wave_no == 4,1,0)]
  data[, wave_5  := ifelse(wave_no == 5,1,0)]
  data[, wave_6  := ifelse(wave_no == 6,1,0)]
  data[, wave_7  := ifelse(wave_no == 7,1,0)]
  data[, wave_8  := ifelse(wave_no == 8,1,0)]
  data[, wave_9  := ifelse(wave_no == 9,1,0)]
  data[, wave_10 := ifelse(wave_no == 10,1,0)]
  data[, wave_11 := ifelse(wave_no == 11,1,0)]
  data[, wave_12 := ifelse(wave_no == 12,1,0)]
  data[, wave_13 := ifelse(wave_no == 13,1,0)]
  data[, wave_14 := ifelse(wave_no == 14,1,0)]

  data[, wave_1  := max(wave_1) , by = "pidp"]
  data[, wave_2  := max(wave_2) , by = "pidp"]
  data[, wave_3  := max(wave_3) , by = "pidp"]
  data[, wave_4  := max(wave_4) , by = "pidp"]
  data[, wave_5  := max(wave_5) , by = "pidp"]
  data[, wave_6  := max(wave_6) , by = "pidp"]
  data[, wave_7  := max(wave_7) , by = "pidp"]
  data[, wave_8  := max(wave_8) , by = "pidp"]
  data[, wave_9  := max(wave_9) , by = "pidp"]
  data[, wave_10 := max(wave_10), by = "pidp"]
  data[, wave_11 := max(wave_11), by = "pidp"]
  data[, wave_12 := max(wave_12), by = "pidp"]
  data[, wave_13 := max(wave_13), by = "pidp"]
  data[, wave_14 := max(wave_14), by = "pidp"]

  ## calculate number of waves each individual is in

  data[, nwaves := .N, by = c("pidp")]

  setnames(data,
           c("istrtdaty", "istrtdatm", "istrtdatd"),
           c("year", "month", "day"))

  # order columns
  setcolorder(data, c("pidp","hidp","wave_no","weight_xw","psu","strata","bhps_sample","nwaves",
                      "wave_1","wave_2","wave_3","wave_4","wave_5","wave_6",
                      "wave_7","wave_8","wave_9","wave_10","wave_11","wave_12",
                      "wave_13","wave_14",
                      "year","month","day"))


  return(data)
}

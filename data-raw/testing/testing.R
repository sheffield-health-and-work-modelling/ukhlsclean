library(data.table)
library(ukhlsclean)

root  <- "C:/"
file  <- "Users/cm1djm/Documents/Data/Understanding Society/SN6614_2026_02_20/tab/ukhls"
full  <- FALSE # full interviews (no proxies) only
waves <- 1:15
ages  <- 16:89
country <- "UK"
complete_vars <- NULL

##########################
### Full UKHLS panel data

fulldata <- ukhlsclean(root = root,
                       file = file,
                       full = full,
                       waves = waves,
                       ages = ages,
                       country = country,
                       complete_vars = complete_vars)

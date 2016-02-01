# read in and clean CDEC data for Owens stations

library(dplyr)
library(ggplot2)
library(lubridate)
library(magrittr)
library(reshape2)

ctt_precip_monthly <- loadCDECMonthlyData("ctt", "precip")
hwe_precip_monthly <- loadCDECMonthlyData("hwe", "precip")
ipn_precip_monthly <- loadCDECMonthlyData("ipn", "precip")

cw1_snow_depth_monthly <- loadCDECMonthlyData("cw1", "snow_depth")
cw2_snow_depth_monthly <- loadCDECMonthlyData("cw2", "snow_depth")
trl_snow_depth_monthly <- loadCDECMonthlyData("trl", "snow_depth")

cw1_snow_wc_monthly <- loadCDECMonthlyData("cw1", "snow_wc")
cw2_snow_wc_monthly <- loadCDECMonthlyData("cw2", "snow_wc")
trl_snow_wc_monthly <- loadCDECMonthlyData("trl", "snow_wc")

tnm_storage_monthly <- loadCDECMonthlyData("tnm", "storage")

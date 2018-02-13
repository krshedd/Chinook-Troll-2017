# This script is to compare GAPS 3.0 populations with the UW Coastwide genotype wrangling
# Kyle Shedd Mon Feb 12 12:44:23 2018
date()

setwd("V:/Analysis/1_SEAK/Chinook/Baseline/Coastwide SNP from UW/")

#### DFO ####
# GAPS from V:\Analysis\1_SEAK\Chinook\Baseline\GAPS3.0\ChinookReportingGroups_CTC_NewModel_Jan2018.xlsx tab "GroupbyPop"
GAPS.df <- read.table(file = 'clipboard', sep = "\t", stringsAsFactors = FALSE, header = TRUE, quote = "\"")
str(GAPS.df)

# UW from V:\Analysis\1_SEAK\Chinook\Baseline\Coastwide SNP from UW\TablesForADFG_20180208.xlsx tab "byLocation"
UW.df <- read.table(file = 'clipboard', sep = "\t", stringsAsFactors = FALSE, header = TRUE, quote = "\"")
str(UW.df)


require(tidyverse)

# Only compare DFO collections
UW_DFO.df <- UW.df %>% 
  filter(Agency == "DFO")
str(UW_DFO.df)

GAPS_DFO.df <- GAPS.df %>% 
  filter(Finest.Scale %in% c("Taku", "Stikine", "Nass", "BCCoastHaiGw", "Skeena", "LoStrtGeo", "WCVI", "ECVI", "UpStrtGeo", "FraserL", "FraserE", "LThompson", "SThompson"))

DFO_join.df <- full_join(x = GAPS_DFO.df, UW_DFO.df, by = c("Population" = "Location"))
str(DFO_join.df)

write.csv(x = DFO_join.df, file = "DFO_UW_GAPS_join.csv", row.names = FALSE)
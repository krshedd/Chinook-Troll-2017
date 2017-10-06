#### Extraction List ####
# 2017 TBR (D108 + D111 Gillnet/Sport) + 2016/17 MSF (Mark Select Fishery)
# Kyle Shedd
# Created Tue Aug 08 10:28:57 2017
date()

# Read in WGC data
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
# load("Extraction Lists/K120ExtractionLists.RData")
require(xlsx)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Drift Gillnet D108/111 ####
#~~~~~~~~~~~~~~~~~~
### Only select TBR fish, even if this means cherry picking fish off of cards
# Only large fish (>=660mm, SW 17-29, District 108 and 111)
Drift_ASL.dat <- read.xlsx(file = "Associated Data/D108+111 Gillnet + MSF/Harvest - Detailed ASL Samples 2017 Drift 108-111 SW17-29 Chinook.xlsx",
                           sheetName = "Harvest - Detailed ASL Samples ", header = TRUE)
str(Drift_ASL.dat)

# Verify that "Size" factor is correct
tapply(Drift_ASL.dat$Length.Millimeters, Drift_ASL.dat$Size, function(x) range(x, na.rm = TRUE))

# Remove non-genetics fish
Drift_ASL_Gen.dat <- Drift_ASL.dat[!is.na(Drift_ASL.dat$Dna.Specimen.No), ]
str(Drift_ASL_Gen.dat)

table(Drift_ASL_Gen.dat$District)
# 108 111 
# 359 148 

# Split WGC number and Fish ID number
table(sapply(strsplit(x = as.character(Drift_ASL_Gen.dat$Dna.Specimen.No), split = ""), length))
Drift_ASL_Gen.dat$Dna.Specimen.No[211:240]
Drift_ASL_Gen.dat$Dna.Specimen.No[Drift_ASL_Gen.dat$Dna.Specimen.No == 2206] <- 902206


Drift_ASL_Gen.dat$WGC <- substr(x = Drift_ASL_Gen.dat$Dna.Specimen.No, start = 1, stop = 4)
Drift_ASL_Gen.dat$WGC_FishID <- substr(x = Drift_ASL_Gen.dat$Dna.Specimen.No, start = 5, stop = 6)

# Verify that "Size" factor is correct
tapply(Drift_ASL_Gen.dat$Length.Millimeters, Drift_ASL_Gen.dat$Size, function(x) range(x, na.rm = TRUE))

# What samples do we have?
table(Drift_ASL_Gen.dat$District, Drift_ASL_Gen.dat$Stat.Week, Drift_ASL_Gen.dat$Size)
table(Drift_ASL_Gen.dat$District, Drift_ASL_Gen.dat$Size)
#     LARGE SMALL
# 108   253   106
# 111    55    93

# Subset for appropriate fish (Large, SW 17-29)
Drift_ASL_Gen_D108.dat <- subset(Drift_ASL_Gen.dat, District == 108 & Size == "LARGE" & Stat.Week >= 17 & Stat.Week <= 29)
str(Drift_ASL_Gen_D108.dat)  # 253


Drift_ASL_Gen_D111.dat <- subset(Drift_ASL_Gen.dat, District == 111 & Size == "LARGE" & Stat.Week >= 17 & Stat.Week <= 29)
str(Drift_ASL_Gen_D111.dat)  # 51

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extraction list for D108
write.xlsx(x = Drift_ASL_Gen_D108.dat[, c("WGC", "WGC_FishID")], file = "Extraction Lists/K120 Sport Gillnet TBR Extraction.xlsx", 
           sheetName = "KGILL17D8 Extraction List", append = TRUE, row.names = FALSE)
length(Drift_ASL_Gen_D108.dat$WGC_FishID); length(unique(Drift_ASL_Gen_D108.dat$WGC))

# Extraction list for D111
write.xlsx(x = Drift_ASL_Gen_D111.dat[, c("WGC", "WGC_FishID")], file = "Extraction Lists/K120 Sport Gillnet TBR Extraction.xlsx", 
           sheetName = "KGILL17D11 Extraction List", append = TRUE, row.names = FALSE)
length(Drift_ASL_Gen_D111.dat$WGC_FishID); length(unique(Drift_ASL_Gen_D111.dat$WGC))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Resolving discrepancy between LOKI and ASL regarding D11
Drift_LOKI_Gen_D111 <- readClipboard()
Drift_LOKI_Gen_D111_WGC <- unique(substr(x = Drift_LOKI_Gen_D111, start = 1, stop = 4))

setdiff(Drift_LOKI_Gen_D111_WGC, unique(subset(Drift_ASL_Gen.dat, District == 111)$WGC))
# [1] "9544" "9579", these two cards are missing from my ASL data

c("9544", "9579") %in% Drift_ASL_Gen.dat$WGC

setdiff(Drift_LOKI_Gen_D111, Drift_ASL_Gen.dat$Dna.Specimen.No)
# [1] "954401" "954402" "954403" "954404" "954405" "954406" "954407" "957901" "957902"
# These fish were from SW30, we do not want them




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Sport D108/111 ####
#~~~~~~~~~~~~~~~~~~
### Run all WGCs with correct fish on them, no cherry picking fish off of cards
### These non-TBR fish will be used for Port estimates later
# Only large fish (>=660mm, SW 17-29, District 108 and 111)
Sport_ASL.dat <- read.xlsx(file = "Associated Data/Sport Data Detailed thru SW 31/2017_SSE_SF_Whatman_AWL_07AUG17.xlsx",
                           sheetName = "SSE Sport", header = TRUE, stringsAsFactors = TRUE)
str(Sport_ASL.dat)

# Verify that "Size" factor is correct
tapply(Sport_ASL.dat$Length, Sport_ASL.dat$Size, function(x) range(x, na.rm = TRUE))

# Remove non-genetics fish
Sport_ASL_Gen.dat <- Sport_ASL.dat[!is.na(Sport_ASL.dat$GSI_CARD), ]
str(Sport_ASL_Gen.dat)

table(Sport_ASL_Gen.dat$DISTRICT)
#     101  102  103  104  105  106  107  108  110  111  112  113  114  181  183  325  365 
# 67  851  162   90  708    8  153   24  204   15  225   28 1719   30   18   88   25   10

# What samples do we have?
table(Sport_ASL_Gen.dat$DISTRICT, Sport_ASL_Gen.dat$STATWEEK, Sport_ASL_Gen.dat$Size)
table(Sport_ASL_Gen.dat$DISTRICT, Sport_ASL_Gen.dat$Size)

#     LARGE SMALL
#        45    22
# 101   695   156
# 102   137    25
# 103    88     2
# 104   667    41
# 105     8     0
# 106   114    39
# 107    24     0
# 108   191    13
# 110    14     1
# 111   157    68
# 112    22     6
# 113  1454   265
# 114    28     2
# 181    16     2
# 183    86     2
# 325    16     9
# 365     9     1
# Subset for appropriate fish (Large, SW 17-29)
Sport_ASL_Gen_D108.dat <- subset(Sport_ASL_Gen.dat, DISTRICT == "108" & Size == "LARGE" & STATWEEK >= 17 & STATWEEK <= 29)
str(Sport_ASL_Gen_D108.dat)  # 189


Sport_ASL_Gen_D111.dat <- subset(Sport_ASL_Gen.dat, DISTRICT == "111" & Size == "LARGE" & STATWEEK >= 17 & STATWEEK <= 29)
str(Sport_ASL_Gen_D111.dat)  # 150

# What else is on those cards?
Sport_D108_WGC <- unique(Sport_ASL_Gen_D108.dat$GSI_CARD)
Sport_D111_WGC <- unique(Sport_ASL_Gen_D111.dat$GSI_CARD)

table(Sport_ASL_Gen.dat$GSI_CARD %in% Sport_D108_WGC)
# FALSE  TRUE 
# 4143   282

table(Sport_ASL_Gen.dat$GSI_CARD %in% Sport_D111_WGC)
# FALSE  TRUE 
# 4208   217  


Sport_ASL_Gen_D108_allCards.dat <- subset(Sport_ASL_Gen.dat, GSI_CARD %in% Sport_D108_WGC)
table(Sport_ASL_Gen_D108_allCards.dat$DISTRICT, Sport_ASL_Gen_D108_allCards.dat$Size)
table(Sport_ASL_Gen_D108_allCards.dat$SITE)

Sport_ASL_Gen_D111_allCards.dat <- subset(Sport_ASL_Gen.dat, GSI_CARD %in% Sport_D111_WGC)
table(Sport_ASL_Gen_D111_allCards.dat$DISTRICT, Sport_ASL_Gen_D111_allCards.dat$Size)
table(Sport_ASL_Gen_D111_allCards.dat$SITE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extraction list for D108 and D111 Sport
write.xlsx(x = unique(rbind(Sport_ASL_Gen_D108_allCards.dat[, "GSI_CARD", drop = FALSE], Sport_ASL_Gen_D111_allCards.dat[, "GSI_CARD", drop = FALSE])),
                        file = "Extraction Lists/K120 Sport Gillnet TBR Extraction.xlsx", 
           sheetName = "KSPORT17 Extraction List", append = TRUE, row.names = FALSE)
length(c(Sport_ASL_Gen_D108_allCards.dat$GsiCardRow, Sport_ASL_Gen_D111_allCards.dat$GsiCardRow))
length(unique(c(Sport_ASL_Gen_D108_allCards.dat$GSI_CARD, Sport_ASL_Gen_D111_allCards.dat$GSI_CARD)))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Mark Select Fishery (MSF) 2016/2017 ####
#~~~~~~~~~~~~~~~~~~
### Run all MSF fish from both years
## 2017
# No filters, we just want to know what was caught
MSF_2017_ASL.dat <- read.xlsx(file = "Associated Data/D108+111 Gillnet + MSF/Harvest - Detailed ASL Samples 2017 MSF Chinook.xlsx",
                           sheetName = "Harvest - Detailed ASL Samples ", header = TRUE)
str(MSF_2017_ASL.dat)  # 489

# Verify that "Size" factor is correct
tapply(MSF_2017_ASL.dat$Length.Millimeters, MSF_2017_ASL.dat$Size, function(x) range(x, na.rm = TRUE))

# Remove non-genetics fish
MSF_2017_ASL_Gen.dat <- MSF_2017_ASL.dat[!is.na(MSF_2017_ASL.dat$Dna.Specimen.No), ]
str(MSF_2017_ASL_Gen.dat)

table(MSF_2017_ASL_Gen.dat$District)
# 171 172 173 174 
# 158 224  22  85 

# Split WGC number and Fish ID number
table(sapply(strsplit(x = as.character(MSF_2017_ASL_Gen.dat$Dna.Specimen.No), split = ""), length))

MSF_2017_ASL_Gen.dat$WGC <- substr(x = MSF_2017_ASL_Gen.dat$Dna.Specimen.No, start = 1, stop = 4)
MSF_2017_ASL_Gen.dat$WGC_FishID <- substr(x = MSF_2017_ASL_Gen.dat$Dna.Specimen.No, start = 5, stop = 6)

# Verify that "Size" factor is correct
tapply(MSF_2017_ASL_Gen.dat$Length.Millimeters, MSF_2017_ASL_Gen.dat$Size, function(x) range(x, na.rm = TRUE))

# What samples do we have?
table(MSF_2017_ASL_Gen.dat$District, MSF_2017_ASL_Gen.dat$Stat.Week, MSF_2017_ASL_Gen.dat$Size)
table(MSF_2017_ASL_Gen.dat$District, MSF_2017_ASL_Gen.dat$Size)
#     LARGE SMALL
# 171   118    40
# 172   187    37
# 173    16     6
# 174    62    23


## 2016
# No filters, we just want to know what was caught
MSF_2016_ASL.dat <- read.xlsx(file = "Associated Data/D108+111 Gillnet + MSF/Harvest - Detailed ASL Samples 2016 MSF Chinook.xlsx",
                              sheetName = "Harvest - Detailed ASL Samples ", header = TRUE)
str(MSF_2016_ASL.dat)  # 117

# Verify that "Size" factor is correct
tapply(MSF_2016_ASL.dat$Length.Millimeters, MSF_2016_ASL.dat$Size, function(x) range(x, na.rm = TRUE))

# Remove non-genetics fish
MSF_2016_ASL_Gen.dat <- MSF_2016_ASL.dat[!is.na(MSF_2016_ASL.dat$Dna.Specimen.No), ]
str(MSF_2016_ASL_Gen.dat)

table(MSF_2016_ASL_Gen.dat$District)
# 171 172 173 174 
# 76  34   4   3

# Split WGC number and Fish ID number
table(sapply(strsplit(x = as.character(MSF_2016_ASL_Gen.dat$Dna.Specimen.No), split = ""), length))

MSF_2016_ASL_Gen.dat$WGC <- substr(x = MSF_2016_ASL_Gen.dat$Dna.Specimen.No, start = 1, stop = 4)
MSF_2016_ASL_Gen.dat$WGC_FishID <- substr(x = MSF_2016_ASL_Gen.dat$Dna.Specimen.No, start = 5, stop = 6)

# Verify that "Size" factor is correct
tapply(MSF_2016_ASL_Gen.dat$Length.Millimeters, MSF_2016_ASL_Gen.dat$Size, function(x) range(x, na.rm = TRUE))

# What samples do we have?
table(MSF_2016_ASL_Gen.dat$District, MSF_2016_ASL_Gen.dat$Stat.Week, MSF_2016_ASL_Gen.dat$Size)
table(MSF_2016_ASL_Gen.dat$District, MSF_2016_ASL_Gen.dat$Size)
#     LARGE SMALL
# 171    61    15
# 172    21    13
# 173     2     2
# 174     2     1


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Extraction list for 2017 Troll MSF
write.xlsx(x = unique(MSF_2017_ASL_Gen.dat[, "WGC", drop = FALSE]), file = "Extraction Lists/K120 Sport Gillnet TBR Extraction.xlsx", 
           sheetName = "KTROL17MS Extraction List", append = TRUE, row.names = FALSE)
length(MSF_2017_ASL_Gen.dat$WGC_FishID); length(unique(MSF_2017_ASL_Gen.dat$WGC))

# Extraction list for 2016 Troll MSF
write.xlsx(x = unique(MSF_2016_ASL_Gen.dat[, "WGC", drop = FALSE]), file = "Extraction Lists/K120 Sport Gillnet TBR Extraction.xlsx", 
           sheetName = "KTROL16MS Extraction List", append = TRUE, row.names = FALSE)
length(MSF_2016_ASL_Gen.dat$WGC_FishID); length(unique(MSF_2016_ASL_Gen.dat$WGC))

# save.image("Extraction Lists/K120ExtractionLists.RData")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Verify that all WGC numbers are correct ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(qdap)

K120_TissueTable.dat <- read.csv(file = "Extraction Lists/K120_GEN_SAMPLED_FISH_TISSUE.csv", header = TRUE)
str(K120_TissueTable.dat)

table(K120_TissueTable.dat$ï..Silly.Code)

table(sapply(strsplit(x = as.character(K120_TissueTable.dat$DNA_TRAY_CODE), split = ""), length))

K120_DNA_TRAY_CODE_Right3 <- substr(x = as.character(K120_TissueTable.dat$DNA_TRAY_CODE), 8, 10)
K120_DNA_TRAY_CODE_Right4 <- substr(x = as.character(K120_TissueTable.dat$DNA_TRAY_CODE), 7, 10)
K120_DNA_TRAY_CODE_Right5 <- substr(x = as.character(K120_TissueTable.dat$DNA_TRAY_CODE), 6, 10)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Drift D8
# Missing any trays?
table(unique(Drift_ASL_Gen_D108.dat$WGC) %in% unique(K120_TissueTable.dat$DNA_TRAY_CODE))  # Yes

# Which ones?
WGC_DriftD8_Missing <- unique(Drift_ASL_Gen_D108.dat$WGC)[!unique(Drift_ASL_Gen_D108.dat$WGC) %in% unique(K120_TissueTable.dat$DNA_TRAY_CODE)]
WGC_DriftD8_Missing

# Are they supposed to be 10 digits?
WGC_DriftD8_Missing %in% K120_DNA_TRAY_CODE_Right4
WGC_DriftD8_Replace <- unique(K120_TissueTable.dat$DNA_TRAY_CODE[K120_DNA_TRAY_CODE_Right4 %in% WGC_DriftD8_Missing])
WGC_DriftD8_Replace

# Replace them
Drift_ASL_Gen_D108.dat$WGC <- mgsub(pattern = WGC_DriftD8_Missing, replacement = WGC_DriftD8_Replace, text.var = Drift_ASL_Gen_D108.dat$WGC)

# Make sure all others are 10 digits
Drift_ASL_Gen_D108.dat$WGC <- 
  sapply(Drift_ASL_Gen_D108.dat$WGC, function(WGC) {ifelse(nchar(WGC) == 10, 
                                                           WGC,
                                                           paste0(paste(rep("0", 10 - nchar(WGC)), collapse = ''), WGC))})
# Write new output
write.xlsx(x = Drift_ASL_Gen_D108.dat[, c("WGC", "WGC_FishID")], file = "Extraction Lists/K120 Sport Gillnet TBR Extraction.xlsx", 
           sheetName = "KGILL17D8 Extraction List", append = TRUE, row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Drift D11
# Missing any trays?
table(unique(Drift_ASL_Gen_D111.dat$WGC) %in% unique(K120_TissueTable.dat$DNA_TRAY_CODE))  # Yes

# Which ones?
WGC_DriftD11_Missing <- unique(Drift_ASL_Gen_D111.dat$WGC)[!unique(Drift_ASL_Gen_D111.dat$WGC) %in% unique(K120_TissueTable.dat$DNA_TRAY_CODE)]
WGC_DriftD11_Missing

# Are they supposed to be 10 digits?
WGC_DriftD11_Missing %in% K120_DNA_TRAY_CODE_Right4
WGC_DriftD11_Replace <- unique(K120_TissueTable.dat$DNA_TRAY_CODE[K120_DNA_TRAY_CODE_Right4 %in% WGC_DriftD11_Missing])
WGC_DriftD11_Replace

# Replace them
Drift_ASL_Gen_D111.dat$WGC <- mgsub(pattern = WGC_DriftD11_Missing, replacement = WGC_DriftD11_Replace, text.var = Drift_ASL_Gen_D111.dat$WGC)

# Make sure all others are 10 digits
Drift_ASL_Gen_D111.dat$WGC <- 
  sapply(Drift_ASL_Gen_D111.dat$WGC, function(WGC) {ifelse(nchar(WGC) == 10, 
                                                           WGC,
                                                           paste0(paste(rep("0", 10 - nchar(WGC)), collapse = ''), WGC))})
# Write new output
write.xlsx(x = Drift_ASL_Gen_D111.dat[, c("WGC", "WGC_FishID")], file = "Extraction Lists/K120 Sport Gillnet TBR Extraction.xlsx", 
           sheetName = "KGILL17D11 Extraction List", append = TRUE, row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sport
# Missing any trays?
c(Sport_D108_WGC, Sport_D111_WGC)
table(c(Sport_D108_WGC, Sport_D111_WGC) %in% unique(K120_TissueTable.dat$DNA_TRAY_CODE))  # Nope

# Make sure all others are 10 digits
Sport_TBR_WGC <- 
  sapply(c(Sport_D108_WGC, Sport_D111_WGC), function(WGC) {ifelse(nchar(WGC) == 10, 
                                                           WGC,
                                                           paste0(paste(rep("0", 10 - nchar(WGC)), collapse = ''), WGC))})
# Write new output
write.xlsx(x = Sport_TBR_WGC, file = "Extraction Lists/K120 Sport Gillnet TBR Extraction.xlsx", 
           sheetName = "KSPORT17 Extraction List", append = TRUE, row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MSF 2016
# Missing any trays?
table(unique(MSF_2016_ASL_Gen.dat$WGC) %in% unique(K120_TissueTable.dat$DNA_TRAY_CODE))  # No

# Make sure all others are 10 digits
MSF_2016_ASL_Gen.dat$WGC <- 
  sapply(MSF_2016_ASL_Gen.dat$WGC, function(WGC) {ifelse(nchar(WGC) == 10, 
                                                         WGC,
                                                         paste0(paste(rep("0", 10 - nchar(WGC)), collapse = ''), WGC))})
# Write new output
write.xlsx(x = unique(MSF_2016_ASL_Gen.dat[, "WGC", drop = FALSE]), file = "Extraction Lists/K120 Sport Gillnet TBR Extraction.xlsx", 
           sheetName = "KTROL16MS Extraction List", append = TRUE, row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## MSF 2017
# Missing any trays?
table(unique(MSF_2017_ASL_Gen.dat$WGC) %in% unique(K120_TissueTable.dat$DNA_TRAY_CODE))  # Yes


# Which ones?
WGC_MSF2017_Missing <- unique(MSF_2017_ASL_Gen.dat$WGC)[!unique(MSF_2017_ASL_Gen.dat$WGC) %in% unique(K120_TissueTable.dat$DNA_TRAY_CODE)]
WGC_MSF2017_Missing

# Are they supposed to be 10 digits?
WGC_MSF2017_Missing %in% K120_DNA_TRAY_CODE_Right4
WGC_MSF2017_Replace <- K120_TissueTable.dat$DNA_TRAY_CODE[match(WGC_MSF2017_Missing, K120_DNA_TRAY_CODE_Right4)]
WGC_MSF2017_Replace

# Replace them
MSF_2017_ASL_Gen.dat$WGC <- mgsub(pattern = WGC_MSF2017_Missing, replacement = WGC_MSF2017_Replace, text.var = MSF_2017_ASL_Gen.dat$WGC)

# Make sure all others are 10 digits
MSF_2017_ASL_Gen.dat$WGC <- 
  sapply(MSF_2017_ASL_Gen.dat$WGC, function(WGC) {ifelse(nchar(WGC) == 10, 
                                                         WGC,
                                                         paste0(paste(rep("0", 10 - nchar(WGC)), collapse = ''), WGC))})
# Write new output
write.xlsx(x = unique(MSF_2017_ASL_Gen.dat[, "WGC", drop = FALSE]), file = "Extraction Lists/K120 Sport Gillnet TBR Extraction.xlsx", 
           sheetName = "KTROL17MS Extraction List", append = TRUE, row.names = FALSE)

# save.image("Extraction Lists/K120ExtractionLists.RData")
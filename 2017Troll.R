#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### SEAK Chinook Troll/Sport/Gillnet Mixtures 2017 ####
# Kyle Shedd Thu Sep 21 17:41:08 2017
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
date()

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Introduction ####
# The goal of this script is to analyze Chinook salmon mixtures from the SEAK
# commercial (troll and gillnet) and sport harvests from 2017 using the GAPS3.0
# baseline containing 357 populations in 26 reporting groups characterized by 
# 13 uSATs. All mixtures are to be analyzed with the program BAYES.

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Specific Objectives ####
# This script will:
# 1) Import mixture data
# 2) Add attribute data
# 3) Define spatio-temporal strata
# 4) Perform a data QC on mixtures
# 5) Prepare BAYES input files
# 6) Summarize BAYES results
# 7) Generate plots and tables of results

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Initial Setup ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
source("H:/R Source Scripts/Functions.GCL_KS.R")
source("C:/Users/krshedd/Documents/R/Functions.GCL.R")
username <- "krshedd"
password <- "********"

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pull all data for each silly code and create .gcl objects for each
K119Mixtures <- c("KTROL16EW", "KTROL17LW", "KTROL17SP")
K120Mixtures <- c("KGILL17D8", "KGILL17D11", "KSPORT17", "KTROL16MS", "KTROL17MS")
  
# source("V:\\Analysis\\R files\\Scripts\\PROD\\rGCL\\1 Old_do not delete\\ReadLOKI_GAPS.GCL.r")
# Get read-in mixture data converted to CTC standardized allele calls.    #Select "Machine Data Source", and then "PCFRES".
# ReadLOKI_GAPS.GCL(sillyvec = c(K119Mixtures, K120Mixtures), markersuite = "GAPS_Chinook_uSATs")

LOKI2R_GAPS.GCL(sillyvec = c(K119Mixtures, K120Mixtures), username = username, password = password)

rm(username, password)
objects(pattern = "\\.gcl")

## Save unaltered .gcl's as back-up:
# dir.create("Raw genotypes")
# dir.create("Raw genotypes/OriginalCollections")
invisible(sapply(c(K119Mixtures, K120Mixtures), function(silly) {dput(x = get(paste0(silly, ".gcl")), file = paste0("Raw genotypes/OriginalCollections/" , silly, ".txt"))} )); beep(8)

## Original sample sizes by SILLY
collection.size.original <- sapply(c(K119Mixtures, K120Mixtures), function(silly) get(paste0(silly, ".gcl"))$n)
# KTROL16EW  KTROL17LW  KTROL17SP  KGILL17D8 KGILL17D11   KSPORT17  KTROL16MS  KTROL17MS 
# 371        499       1010        246         48        489        114        479  # This is how many fish there are
# 380        500       1020        253         51        499        116        485  # This is how many fish I asked for
#   9          1         10          7          3         10          2          6  # Discrepancy
# K119 had 20 full failures
# K120 had 28 full failures

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save LocusControl
# dir.create("Objects")
dput(x = LocusControl, file = "Objects/LocusControl.txt")
dput(x = K119Mixtures, file = "Objects/K119Mixtures.txt")
dput(x = K120Mixtures, file = "Objects/K120Mixtures.txt")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Get mixture objects from SEAK16
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK16/Objects")

SEAK16Objects <- list.files()
SEAK16Objects

SEAK16Objects2copy <- c(list.files(pattern = "Group"),
                        list.files(pattern = "GAPS"))
SEAK16Objects2copy

file.copy(from = SEAK16Objects2copy, to = "V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17/Objects")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Clean workspace; dget .gcl objects and Locus Control ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
# This sources all of the new GCL functions to this workspace
source("C:/Users/krshedd/Documents/R/Functions.GCL.R")
source("H:/R Source Scripts/Functions.GCL_KS.R")

## Get objects
SEAKobjects <- list.files(path = "Objects", recursive = FALSE)
SEAKobjects

invisible(sapply(SEAKobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Objects", objct, sep = "/")), pos = 1) })); beep(2)

## Get un-altered mixtures
invisible(sapply(c(K119Mixtures, K120Mixtures), function(silly) {assign(x = paste0(silly, ".gcl"), value = dget(file = paste0("Raw genotypes/OriginalCollections/", silly, ".txt")), pos = 1)} )); beep(2)
objects(pattern = "\\.gcl")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Add District and StatWeek Info ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### K119 - Winter + Spring Troll
K119Mixtures
# "KTROL16EW" "KTROL17LW" "KTROL17SP"

#~~~~~~~~~~~~~~~~~~
## Early Winter Mixtures
# Read in fish data
EWint16.dat <- read.xlsx(file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", 
                         sheetName = "EW Extraction Data")
str(EWint16.dat)
aggregate(X..Tissues ~ Dist.Quad, data = EWint16.dat, sum)
#   Dist.Quad X..Tissues
# 1       171        296
# 2       172         10
# 3       173         34
# 4       174         43
table(nchar(EWint16.dat$Whatman.Card..))  # Checking for number of digits in WGC for matching

# Add to attributues table
KTROL16EW.gcl$n  # 371
EWint16.WGC.match <- match(KTROL16EW.gcl$attributes$DNA_TRAY_CODE, EWint16.dat$Whatman.Card..)
any(is.na(EWint16.WGC.match))  # TRUE, now FALSE
unique(KTROL16EW.gcl$attributes$DNA_TRAY_CODE[which(is.na(EWint16.WGC.match))])  # 6240, was missing from original data, but found in OceanAK report; Added row to "EW Extraction Data", and re-read in

KTROL16EW.gcl$attributes$Quadrant <- EWint16.dat$Dist.Quad[EWint16.WGC.match]
KTROL16EW.gcl$attributes$StatWeek <- EWint16.dat$Week[EWint16.WGC.match]
table(KTROL16EW.gcl$attributes$Quadrant, useNA = "always")
#   171  172  173  174 <NA> 
#   286   10   34   41    0 
sum(table(KTROL16EW.gcl$attributes$Quadrant))  # 371


#~~~~~~~~~~~~~~~~~~
## Late Winter Mixtures
# Read in fish data
LWint17.dat <- read.xlsx(file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", 
                         sheetName = "LW Extraction Data")
str(LWint17.dat)
aggregate(X..Tissues ~ Dist.Quad, data = LWint17.dat, sum)
#   Dist.Quad X..Tissues
# 1       171        302
# 2       172         42
# 3       173         61
# 4       174         95
table(nchar(LWint17.dat$Whatman.Card..))  # Checking for number of digits in WGC for matching
# Standardize WGC digit length
LWint17.dat$Whatman.Card.. <- ifelse(nchar(LWint17.dat$Whatman.Card..) == 4,
                                     paste0("000000", LWint17.dat$Whatman.Card..),
                                     LWint17.dat$Whatman.Card..)

# Add to attributues table
KTROL17LW.gcl$n  # 499
LWint17.WGC.match <- match(KTROL17LW.gcl$attributes$DNA_TRAY_CODE, LWint17.dat$Whatman.Card..)
any(is.na(LWint17.WGC.match))  # FALSE

KTROL17LW.gcl$attributes$Quadrant <- LWint17.dat$Dist.Quad[LWint17.WGC.match]
KTROL17LW.gcl$attributes$StatWeek <- LWint17.dat$Week[LWint17.WGC.match]
table(KTROL17LW.gcl$attributes$Quadrant, useNA = "always")
#   171  172  173  174 <NA> 
#   301   42   61   95    0 
sum(table(KTROL17LW.gcl$attributes$Quadrant))  # 499


#~~~~~~~~~~~~~~~~~~
## Spring Mixtures
# Note, we are analyzing spring 1 and 2 separately, then will stratify for total spring estimates!!!
# Read in fish data
Spring17.dat <- rbind(read.xlsx(file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", sheetName = "SP1 Extraction Data"),
                     read.xlsx(file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", sheetName = "SP2 Extraction Data")
                     )
str(Spring17.dat)
aggregate(X..Tissues ~ Dist.Quad, data = Spring17.dat, sum)
#   Dist.Quad X..Tissues
# 1       171        389
# 2       172        105
# 3       173        152
# 4       174        374
table(nchar(Spring17.dat$Whatman.Card..))
Spring17.dat$Whatman.Card.. <- ifelse(nchar(Spring17.dat$Whatman.Card..) == 4,
                                     paste0("000000", Spring17.dat$Whatman.Card..),
                                     Spring17.dat$Whatman.Card..)

# Add to attributues table
KTROL17SP.gcl$n  # 1010
Spring17.WGC.match <- match(KTROL17SP.gcl$attributes$DNA_TRAY_CODE, Spring17.dat$Whatman.Card..)
any(is.na(Spring17.WGC.match))  # TRUE, now FALSE
unique(KTROL17SP.gcl$attributes$DNA_TRAY_CODE[which(is.na(Spring17.WGC.match))])  # 1000004136, 1000004140, and 1000004730 were missing from original data, but found in OceanAK report; Added row to "EW Extraction Data", and re-read in

KTROL17SP.gcl$attributes$Quadrant <- Spring17.dat$Dist.Quad[Spring17.WGC.match]
KTROL17SP.gcl$attributes$StatWeek <- Spring17.dat$Week[Spring17.WGC.match]
table(KTROL17SP.gcl$attributes$Quadrant, useNA = "always")
#   171  172  173  174 <NA> 
#   387  103  151  369   0 
sum(table(KTROL17SP.gcl$attributes$Quadrant))  # 1010

table(KTROL17SP.gcl$attributes$StatWeek, useNA = "always")
#   18   19   20   21   22   24   25   26 <NA> 
#   17   80  136  268   54   44  290  121    0 
sum(table(KTROL17SP.gcl$attributes$StatWeek, useNA = "always")[1:5])  # 555
sum(table(KTROL17SP.gcl$attributes$StatWeek, useNA = "always")[6:8])  # 455


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### K120 - TBR (D8+11) Gillnet + Sport + MSF (2016/2017)
K120Mixtures
# "KGILL17D8"  "KGILL17D11" "KSPORT17"   "KTROL16MS"  "KTROL17MS" 

#~~~~~~~~~~~~~~~~~~
## D108 Gillnet Mixture
# No extra data needed, this whole silly is the mixture
KGILL17D8.gcl$n  # 246


#~~~~~~~~~~~~~~~~~~
## D111 Gillnet Mixture
# No extra data needed, this whole silly is the mixture
KGILL17D11.gcl$n  # 48


#~~~~~~~~~~~~~~~~~~
## D108/111 Sport Mixtures
# Read in fish data
D8_D11_Sport.dat <- read.xlsx(file = "Associated Data/Sport Data Detailed thru SW 31/2017_SSE_SF_Whatman_AWL_07AUG17.xlsx",
                              sheetName = "SSE Sport", header = TRUE, stringsAsFactors = TRUE)
str(D8_D11_Sport.dat)
table(D8_D11_Sport.dat$Size, D8_D11_Sport.dat$DISTRICT)
#             101  102  103  104  105  106  107  108  110  111  112  113  114  181  183  325  365
# LARGE   45  695  137   88  667    8  114   24  191   14  157   22 1454   28   16   86   16    9
# SMALL   22  156   25    2   41    0   39    0   13    1   68    6  265    2    2    2    9    1
table(nchar(D8_D11_Sport.dat$GSI_CARD))  # Checking for number of digits in WGC for matching
D8_D11_Sport.dat$GSI_CARD <- sapply(D8_D11_Sport.dat$GSI_CARD, function(ind) {paste0(paste(rep(0, 10 - nchar(ind)), collapse = ''), ind)} )

# Add to attributues table
KSPORT17.gcl$n  # 489
D8_D11_Sport.WGC.match <- match(KSPORT17.gcl$attributes$DNA_TRAY_CODE, D8_D11_Sport.dat$GSI_CARD)
any(is.na(D8_D11_Sport.WGC.match))  # FALSE

KSPORT17.gcl$attributes$District <- D8_D11_Sport.dat$DISTRICT[D8_D11_Sport.WGC.match]
KSPORT17.gcl$attributes$StatWeek <- D8_D11_Sport.dat$STATWEEK[D8_D11_Sport.WGC.match]
KSPORT17.gcl$attributes$Size <- D8_D11_Sport.dat$Size[D8_D11_Sport.WGC.match]

table(KSPORT17.gcl$attributes$StatWeek, useNA = "always")  # All fish are from SW 17-29

table(KSPORT17.gcl$attributes$Size, KSPORT17.gcl$attributes$District, useNA = "always")
#             101 102 103 104 105 106 107 108 110 111 112 113 114 181 183 325 365 <NA>
#   LARGE   0   0   0   0   0   0  78   9 154  18 162   0   0   0   0   0   0   0    0
#   SMALL   0   0   0   0   0   0   0   0  15   0  53   0   0   0   0   0   0   0    0
#   <NA>    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0    0
sum(table(KSPORT17.gcl$attributes$District))  # 489


#~~~~~~~~~~~~~~~~~~
## Mark Select Fishery 2016 Mixture
# No extra data needed, this whole silly is the mixture due to low sample size

#~~~~~~~~~~~~~~~~~~
## Mark Select Fishery 2017 Mixture
# Read in fish data
MSF17.dat <- read.xlsx(file = "Associated Data/D108+111 Gillnet + MSF/Harvest - Detailed ASL Samples 2017 MSF Chinook.xlsx",
                       sheetName = "Harvest - Detailed ASL Samples ", header = TRUE)
str(MSF17.dat)  # 489
addmargins(table(MSF17.dat$Size, MSF17.dat$District))
#       171 172 173 174 Sum
# LARGE 118 187  16  62 383
# SMALL  40  37   6  23 106
# Sum   158 224  22  85 489
MSF17.dat$WGC <- substr(x = MSF17.dat$Dna.Specimen.No, start = 1, stop = 4)

# Add to attributues table
KTROL17MS.gcl$n  # 479
MSF17.WGC.match <- match(substr(x = KTROL17MS.gcl$attributes$DNA_TRAY_CODE, start = 7, stop = 10), MSF17.dat$WGC)
any(is.na(MSF17.WGC.match))  # FALSE

KTROL17MS.gcl$attributes$District <- MSF17.dat$District[MSF17.WGC.match]
KTROL17MS.gcl$attributes$StatWeek <- MSF17.dat$Stat.Week[MSF17.WGC.match]
KTROL17MS.gcl$attributes$Size <- MSF17.dat$Size[MSF17.WGC.match]

table(KTROL17MS.gcl$attributes$StatWeek, useNA = "always")  # All fish are from SW 27-30

table(KTROL17MS.gcl$attributes$District, useNA = "always")
#   171  172  173  174 <NA> 
#   152  220   22   85    0 
sum(table(KTROL17MS.gcl$attributes$District))  # 479

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save .gcl's with additional attributes data as back-up:
# dir.create("Raw genotypes/OriginalCollections_Attributes")
invisible(sapply(c(K119Mixtures, K120Mixtures), function(silly) {
  dput(x = get(paste0(silly, ".gcl")), file = paste0("Raw genotypes/OriginalCollections_Attributes/" , silly, ".txt"))
} )); beep(8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Define Strata ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Early Winter Troll
# Create Mixtures for 1) NO, and 2) NI, SI, SO quadrants
table(KTROL16EW.gcl$attributes$Quadrant)
# 171 172 173 174 
# 286  10  34  41 
EWintNO_2017.vials <- setNames(object = list(AttributesToIDs.GCL(silly = "KTROL16EW", attribute = "Quadrant", matching = 171)), nm = "KTROL16EW")
PoolCollections.GCL(collections = "KTROL16EW", loci = GAPSLoci_reordered, IDs = EWintNO_2017.vials, newname = "EWintNO_2017")

EWintNISISO_2017.vials <- setNames(object = list(AttributesToIDs.GCL(silly = "KTROL16EW", attribute = "Quadrant", matching = 172:174)), nm = "KTROL16EW")
PoolCollections.GCL(collections = "KTROL16EW", loci = GAPSLoci_reordered, IDs = EWintNISISO_2017.vials, newname = "EWintNISISO_2017")

sapply(grep(pattern = "EWint", objects(pattern = "\\.gcl"), value = TRUE), function(silly) {get(silly)$n})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Late Winter Troll
# Create Mixtures for 1) NO, and 2) NI, SI, SO quadrants
table(KTROL17LW.gcl$attributes$Quadrant)
# 171 172 173 174 
# 301  42  61  95 
LWintNO_2017.vials <- setNames(object = list(AttributesToIDs.GCL(silly = "KTROL17LW", attribute = "Quadrant", matching = 171)), nm = "KTROL17LW")
PoolCollections.GCL(collections = "KTROL17LW", loci = GAPSLoci_reordered, IDs = LWintNO_2017.vials, newname = "LWintNO_2017")

LWintNISISO_2017.vials <- setNames(object = list(AttributesToIDs.GCL(silly = "KTROL17LW", attribute = "Quadrant", matching = 172:174)), nm = "KTROL17LW")
PoolCollections.GCL(collections = "KTROL17LW", loci = GAPSLoci_reordered, IDs = LWintNISISO_2017.vials, newname = "LWintNISISO_2017")

sapply(grep(pattern = "LWint", objects(pattern = "\\.gcl"), value = TRUE), function(silly) {get(silly)$n})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Spring Troll
# Create Mixtures for each quadrant for each "retention period" (8 total mixtures) 1) NO, 2) NI, 3) SI, and 4) SO quadrants
table(KTROL17SP.gcl$attributes$Quadrant)
# 171 172 173 174 
# 387 103 151 369
table(KTROL17SP.gcl$attributes$StatWeek)
KTROL17SP.gcl$attributes$Period <- ifelse(KTROL17SP.gcl$attributes$StatWeek < 23, 1, 2)
addmargins(table(KTROL17SP.gcl$attributes$Period, KTROL17SP.gcl$attributes$Quadrant))
#      171  172  173  174  Sum
# 1    221   41   89  204  555
# 2    166   62   62  165  455
# Sum  387  103  151  369 1010
SpringRet1NI_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Quadrant", matching = 173),
                                                            AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Period", matching = 1))), nm = "KTROL17SP")
PoolCollections.GCL(collections = "KTROL17SP", loci = GAPSLoci_reordered, IDs = SpringRet1NI_2017.vials, newname = "SpringRet1NI_2017")

SpringRet1NO_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Quadrant", matching = 171),
                                                            AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Period", matching = 1))), nm = "KTROL17SP")
PoolCollections.GCL(collections = "KTROL17SP", loci = GAPSLoci_reordered, IDs = SpringRet1NO_2017.vials, newname = "SpringRet1NO_2017")

SpringRet1SI_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Quadrant", matching = 174),
                                                            AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Period", matching = 1))), nm = "KTROL17SP")
PoolCollections.GCL(collections = "KTROL17SP", loci = GAPSLoci_reordered, IDs = SpringRet1SI_2017.vials, newname = "SpringRet1SI_2017")

SpringRet1SO_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Quadrant", matching = 172),
                                                            AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Period", matching = 1))), nm = "KTROL17SP")
PoolCollections.GCL(collections = "KTROL17SP", loci = GAPSLoci_reordered, IDs = SpringRet1SO_2017.vials, newname = "SpringRet1SO_2017")

SpringRet2NI_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Quadrant", matching = 173),
                                                            AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Period", matching = 2))), nm = "KTROL17SP")
PoolCollections.GCL(collections = "KTROL17SP", loci = GAPSLoci_reordered, IDs = SpringRet2NI_2017.vials, newname = "SpringRet2NI_2017")

SpringRet2NO_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Quadrant", matching = 171),
                                                            AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Period", matching = 2))), nm = "KTROL17SP")
PoolCollections.GCL(collections = "KTROL17SP", loci = GAPSLoci_reordered, IDs = SpringRet2NO_2017.vials, newname = "SpringRet2NO_2017")

SpringRet2SI_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Quadrant", matching = 174),
                                                            AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Period", matching = 2))), nm = "KTROL17SP")
PoolCollections.GCL(collections = "KTROL17SP", loci = GAPSLoci_reordered, IDs = SpringRet2SI_2017.vials, newname = "SpringRet2SI_2017")

SpringRet2SO_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Quadrant", matching = 172),
                                                            AttributesToIDs.GCL(silly = "KTROL17SP", attribute = "Period", matching = 2))), nm = "KTROL17SP")
PoolCollections.GCL(collections = "KTROL17SP", loci = GAPSLoci_reordered, IDs = SpringRet2SO_2017.vials, newname = "SpringRet2SO_2017")

sapply(grep(pattern = "Spring", objects(pattern = "\\.gcl"), value = TRUE), function(silly) {get(silly)$n})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## District 108 Gillnet
# Create Mixtures for 1) D108Gill_2017, (large fish >= 660mm, StatWeek 17-29)
KGILL17D8.gcl$n
# 246
D108Gill_2017.gcl <- KGILL17D8.gcl


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## District 111 Gillnet
# Create Mixtures for 1) D111Gill_2017, (large fish >= 660mm, StatWeek 17-29)
KGILL17D11.gcl$n
# 48
D111Gill_2017.gcl <- KGILL17D11.gcl


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## District 108/111 Sport
# Create Mixtures for 1) D108Sport_2017 and 2) D111Sport_2017 (large fish >= 660mm, StatWeek 17-29)
table(KSPORT17.gcl$attributes$Size, KSPORT17.gcl$attributes$District)
#           101 102 103 104 105 106 107 108 110 111 112 113 114 181 183 325 365
# LARGE   0   0   0   0   0   0  78   9 154  18 162   0   0   0   0   0   0   0
# SMALL   0   0   0   0   0   0   0   0  15   0  53   0   0   0   0   0   0   0
D108Sport_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KSPORT17", attribute = "District", matching = 108),
                                                         AttributesToIDs.GCL(silly = "KSPORT17", attribute = "Size", matching = "LARGE"))), nm = "KSPORT17")
PoolCollections.GCL(collections = "KSPORT17", loci = GAPSLoci_reordered, IDs = D108Sport_2017.vials, newname = "D108Sport_2017")

D111Sport_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KSPORT17", attribute = "District", matching = 111),
                                                         AttributesToIDs.GCL(silly = "KSPORT17", attribute = "Size", matching = "LARGE"))), nm = "KSPORT17")
PoolCollections.GCL(collections = "KSPORT17", loci = GAPSLoci_reordered, IDs = D111Sport_2017.vials, newname = "D111Sport_2017")

sapply(grep(pattern = "Sport", objects(pattern = "\\.gcl"), value = TRUE), function(silly) {get(silly)$n})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mark Select Fishery 2016
# Create Mixtures for 1) all quadrant 2016 MSF
KTROL16MS.gcl$n
# 114
MarkSelect_2016.gcl <- KTROL16MS.gcl


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Mark Select Fishery 2017
# Create Mixtures for 1) all quadrant 2016 MSF
table(KTROL17MS.gcl$attributes$District)
# 171 172 173 174 
# 152 220  22  85 
MarkSelectNO_2017.vials <- setNames(object = list(AttributesToIDs.GCL(silly = "KTROL17MS", attribute = "District", matching = 171)), nm = "KTROL17MS")
PoolCollections.GCL(collections = "KTROL17MS", loci = GAPSLoci_reordered, IDs = MarkSelectNO_2017.vials, newname = "MarkSelectNO_2017")

MarkSelectSO_2017.vials <- setNames(object = list(AttributesToIDs.GCL(silly = "KTROL17MS", attribute = "District", matching = 172)), nm = "KTROL17MS")
PoolCollections.GCL(collections = "KTROL17MS", loci = GAPSLoci_reordered, IDs = MarkSelectSO_2017.vials, newname = "MarkSelectSO_2017")

MarkSelectNISI_2017.vials <- setNames(object = list(AttributesToIDs.GCL(silly = "KTROL17MS", attribute = "District", matching = 173:174)), nm = "KTROL17MS")
PoolCollections.GCL(collections = "KTROL17MS", loci = GAPSLoci_reordered, IDs = MarkSelectNISI_2017.vials, newname = "MarkSelectNISI_2017")

sapply(grep(pattern = "MarkSelect", objects(pattern = "\\.gcl"), value = TRUE), function(silly) {get(silly)$n})


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EWint_Mixtures <- c("EWintNO_2017", "EWintNISISO_2017")
LWint_Mixtures <- c("LWintNO_2017", "LWintNISISO_2017")
SpringRet1_Mixtures <- c("SpringRet1NI_2017", "SpringRet1NO_2017", "SpringRet1SI_2017", "SpringRet1SO_2017")
SpringRet2_Mixtures <- c("SpringRet2NI_2017", "SpringRet2NO_2017", "SpringRet2SI_2017", "SpringRet2SO_2017")
TBR_Mixtures <- c("D108Gill_2017", "D111Gill_2017", "D108Sport_2017", "D111Sport_2017")
MSF_Mixtures <- c("MarkSelect_2016", "MarkSelectNO_2017", "MarkSelectSO_2017", "MarkSelectNISI_2017")

sapply(c(EWint_Mixtures, LWint_Mixtures, SpringRet1_Mixtures, SpringRet2_Mixtures, TBR_Mixtures, MSF_Mixtures), function(mix) {
  get(paste0(mix, ".gcl"))$n
} )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dput all .vials objects
# dir.create("Objects/Vials")
invisible(sapply(objects(pattern = ".vials"), function(obj) {
  dput(x = get(obj), file = paste0("Objects/Vials/", obj, ".txt"))
} ))

invisible(sapply(objects(pattern = "Mixtures"), function(obj) {
  dput(x = get(obj), file = paste0("Objects/", obj, ".txt"))
} ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save .gcl's with additional attributes data as back-up:
# dir.create("Raw genotypes/OriginalCollections_Attributes_Strata")
invisible(sapply(c(EWint_Mixtures, LWint_Mixtures, SpringRet1_Mixtures, SpringRet2_Mixtures, TBR_Mixtures, MSF_Mixtures), function(silly) {
  dput(x = get(paste0(silly, ".gcl")), file = paste0("Raw genotypes/OriginalCollections_Attributes_Strata/" , silly, ".txt"))
} )); beep(8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Clean workspace; dget .gcl objects and Locus Control ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
# This sources all of the new GCL functions to this workspace
source("C:/Users/krshedd/Documents/R/Functions.GCL.R")
source("H:/R Source Scripts/Functions.GCL_KS.R")

## Get objects
SEAKobjects <- list.files(path = "Objects", recursive = FALSE)
SEAKobjects <- SEAKobjects[-which(SEAKobjects == "Vials")]
SEAKobjects

invisible(sapply(SEAKobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Objects", objct, sep = "/")), pos = 1) })); beep(2)

## Get un-altered mixtures
invisible(sapply(c(EWint_Mixtures, LWint_Mixtures, SpringRet1_Mixtures, SpringRet2_Mixtures, TBR_Mixtures, MSF_Mixtures), function(silly) {assign(x = paste0(silly, ".gcl"), value = dget(file = paste0("Raw genotypes/OriginalCollections_Attributes_Strata//", silly, ".txt")), pos = 1)} )); beep(2)
objects(pattern = "\\.gcl")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Data QC/Massage ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(xlsx)

K119_K120_Strata <- c(EWint_Mixtures, LWint_Mixtures, SpringRet1_Mixtures, SpringRet2_Mixtures, TBR_Mixtures, MSF_Mixtures)

K119_K120_Strata_SampleSizes <- matrix(data = NA, nrow = length(K119_K120_Strata), ncol = 4, 
                                       dimnames = list(K119_K120_Strata, c("Genotyped", "Missing", "Duplicate", "Final")))

#### Check loci
## Get sample size by locus
Original_K119_K120_Strata_SampleSizebyLocus <- SampSizeByLocus.GCL(sillyvec = K119_K120_Strata, loci = GAPSLoci_reordered)
min(Original_K119_K120_Strata_SampleSizebyLocus)  ## 40
apply(Original_K119_K120_Strata_SampleSizebyLocus, 1, min) / apply(Original_K119_K120_Strata_SampleSizebyLocus, 1, max)  ## Good, 0.935

Original_K119_K120_Strata_PercentbyLocus <- apply(Original_K119_K120_Strata_SampleSizebyLocus, 1, function(row) {row / max(row)} )
which(apply(Original_K119_K120_Strata_PercentbyLocus, 2, min) < 0.8)  # no re-runs!

require(lattice)
new.colors <- colorRampPalette(c("black", "white"))
levelplot(t(Original_K119_K120_Strata_PercentbyLocus), 
          col.regions = new.colors, 
          at = seq(from = 0, to = 1, length.out = 100), 
          main = "% Genotyped", xlab = "SILLY", ylab = "Locus", 
          scales = list(x = list(rot = 90)), 
          aspect = "fill")  # aspect = "iso" will make squares

#### Check individuals
### Initial
## Get number of individuals per silly before removing missing loci individuals
Original_K119_K120_Strata_ColSize <- sapply(paste0(K119_K120_Strata, ".gcl"), function(x) get(x)$n)
K119_K120_Strata_SampleSizes[, "Genotyped"] <- Original_K119_K120_Strata_ColSize

### Missing
## Remove individuals with >20% missing data
K119_K120_Strata_MissLoci <- RemoveIndMissLoci.GCL(sillyvec = K119_K120_Strata, proportion = 0.8)
dput(x = K119_K120_Strata_MissLoci, file = "Objects/K119_K120_Strata_MissLoci.txt")

## Get number of individuals per silly after removing missing loci individuals
ColSize_K119_K120_Strata_PostMissLoci <- sapply(paste0(K119_K120_Strata, ".gcl"), function(x) get(x)$n)
K119_K120_Strata_SampleSizes[, "Missing"] <- Original_K119_K120_Strata_ColSize - ColSize_K119_K120_Strata_PostMissLoci

### Duplicate
## Check within collections for duplicate individuals.
K119_K120_Strata_DuplicateCheck95MinProportion <- CheckDupWithinSilly.GCL(sillyvec = K119_K120_Strata, loci = GAPSLoci_reordered, quantile = NULL, minproportion = 0.95)
K119_K120_Strata_DuplicateCheckReportSummary <- sapply(K119_K120_Strata, function(x) K119_K120_Strata_DuplicateCheck95MinProportion[[x]]$report)
K119_K120_Strata_DuplicateCheckReportSummary
dput(x = K119_K120_Strata_DuplicateCheckReportSummary, file = "Objects/K119_K120_Strata_DuplicateCheckReportSummary.txt")

## Remove duplicate individuals
K119_K120_Strata_RemovedDups <- RemoveDups.GCL(K119_K120_Strata_DuplicateCheck95MinProportion)

## Get number of individuals per silly after removing duplicate individuals
ColSize_K119_K120_Strata_PostDuplicate <- sapply(paste0(K119_K120_Strata, ".gcl"), function(x) get(x)$n)
K119_K120_Strata_SampleSizes[, "Duplicate"] <- ColSize_K119_K120_Strata_PostMissLoci-ColSize_K119_K120_Strata_PostDuplicate

### Final
K119_K120_Strata_SampleSizes[, "Final"] <- ColSize_K119_K120_Strata_PostDuplicate
K119_K120_Strata_SampleSizes

# dir.create("Output")
write.xlsx(K119_K120_Strata_SampleSizes, file = "Output/K119_K120_Strata_SampleSizes.xlsx")
dput(x = K119_K120_Strata_SampleSizes, file = "Objects/K119_K120_Strata_SampleSizes.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save PostQC .gcl's as back-up:
# dir.create("Raw genotypes/OriginalCollections_Attributes_Strata_PostQC")
invisible(sapply(K119_K120_Strata, function(silly) {
  dput(x = get(paste(silly, ".gcl", sep = '')), file = paste0("Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/" , silly, ".txt"))
} )); beep(8)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Clean workspace; dget .gcl objects and Locus Control ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
# This sources all of the new GCL functions to this workspace
source("C:/Users/krshedd/Documents/R/Functions.GCL.R")
source("H:/R Source Scripts/Functions.GCL_KS.R")

## Get objects
SEAKobjects <- list.files(path = "Objects", recursive = FALSE)
SEAKobjects <- SEAKobjects[-which(SEAKobjects == "Vials")]
SEAKobjects

invisible(sapply(SEAKobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Objects", objct, sep = "/")), pos = 1) })); beep(2)

## Get un-altered mixtures
invisible(sapply(K119_K120_Strata, function(silly) {assign(x = paste0(silly, ".gcl"), value = dget(file = paste0("Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/", silly, ".txt")), pos = 1)} )); beep(2)
objects(pattern = "\\.gcl")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Get/Create MSA Objects ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dir.create(path = "BAYES")
# sapply(c("Control", "Mixture", "Output"), function(folder) {dir.create(path = paste("BAYES", folder, sep = "/"))} )

file.copy(from = "V:/Analysis/1_SEAK/Chinook/Baseline/GAPS3.0/Objects/SEAKPops357.txt", to = "Objects")
file.copy(from = "V:/Analysis/1_SEAK/Chinook/Baseline/GAPS3.0/Objects/bayesfortran_357.txt", to = "Objects")
# dir.create("BAYES/Baseline")
file.copy(from = "V:/Analysis/1_SEAK/Chinook/Baseline/GAPS3.0/BAYES/Baseline/GAPS357pops13loci.bse", to = "BAYES/Baseline")
file.copy(from = "V:/Analysis/4_Westward/Sockeye/KMA Commercial Harvest 2014-2016/Mixtures/Objects/WASSIPSockeyeSeeds.txt", to = "Objects")
GAPS357PopsInits <- MultiChainInits.GCL(npops = 357, nchains = 5, prop = 0.9)
dput(x = GAPS357PopsInits, file = "Objects/GAPS357PopsInits.txt")
GroupNames5 <- c("Taku", "Andrew", "Stikine", "SSEAK", "Other")
dput(x = GroupNames5, file = "Objects/GroupNames5.txt")
GroupVec5RG_357 <- as.numeric(readClipboard())
dput(x = GroupVec5RG_357, file = "Objects/GroupVec5RG_357.txt")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dumping Mixture Files
mixfortran <- CreateMixture.GCL(sillys = "EWintNO_2017", loci = GAPSLoci_reordered, IDs = NULL, mixname = "EWintNO_2017", dir = "BAYES/Mixture", type = "BAYES", PT = FALSE)
dput(x = mixfortran, file = "Objects/mixfortran.txt")

sapply(K119_K120_Strata, function(Mix) {
  CreateMixture.GCL(sillys = Mix, loci = GAPSLoci_reordered, IDs = NULL, mixname = Mix, dir = "BAYES//Mixture", type = "BAYES", PT = FALSE)
} )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create Priors
Winter_Spring_Troll_Strata_Priors <- apply(read.table(file = "Associated Data/2016WinterSpringTroll26RGEstimates.txt", header = TRUE), 2, function(mix) {
  Prior.GCL(groupvec = GroupVec26RG_357, groupweights = mix, minval = 0.01)
} )
Winter_Spring_Troll_Strata_Priors <- cbind(Winter_Spring_Troll_Strata_Priors, Winter_Spring_Troll_Strata_Priors[, 5:8])
colnames(Winter_Spring_Troll_Strata_Priors) <- c(EWint_Mixtures, LWint_Mixtures, SpringRet1_Mixtures, SpringRet2_Mixtures)
dput(x = Winter_Spring_Troll_Strata_Priors, file = "Objects/Winter_Spring_Troll_Strata_Priors.txt")


TBR_Strata_Priors <- apply(read.table(file = "Associated Data/2016D108D111TBR5RGEstimates.txt", header = TRUE), 2, function(mix) {
  Prior.GCL(groupvec = GroupVec26RG_357, groupweights = mix, minval = 0.01)
} )
colnames(TBR_Strata_Priors) <- TBR_Mixtures
dput(x = TBR_Strata_Priors, file = "Objects/TBR_Strata_Priors.txt")


GAPS357Pops26FlatPrior <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = rep(1 / 26, 26), minval = 0.01)
dput(x = GAPS357Pops26FlatPrior, file = "Objects/GAPS357Pops26FlatPrior.txt")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dumping Control files
sapply(c(EWint_Mixtures, LWint_Mixtures, SpringRet1_Mixtures, SpringRet2_Mixtures), function(Mix) {
  CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname = Mix, basename = "GAPS357pops13loci", suffix = "", nreps = 40000, nchains = 5,
                        groupvec = GroupVec26RG_357, priorvec = Winter_Spring_Troll_Strata_Priors[, Mix], initmat = GAPS357PopsInits, dir = "BAYES/Control",
                        seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), mixfortran = mixfortran, basefortran = bayesfortran_357, switches = "F T F T T T F")
} )

sapply(TBR_Mixtures, function(Mix) {
  CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname = Mix, basename = "GAPS357pops13loci", suffix = "", nreps = 40000, nchains = 5,
                        groupvec = GroupVec5RG_357, priorvec = TBR_Strata_Priors[, Mix], initmat = GAPS357PopsInits, dir = "BAYES/Control",
                        seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), mixfortran = mixfortran, basefortran = bayesfortran_357, switches = "F T F T T T F")
} )

sapply(MSF_Mixtures, function(Mix) {
  CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname = Mix, basename = "GAPS357pops13loci", suffix = "", nreps = 40000, nchains = 5,
                        groupvec = GroupVec26RG_357, priorvec = GAPS357Pops26FlatPrior, initmat = GAPS357PopsInits, dir = "BAYES/Control",
                        seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), mixfortran = mixfortran, basefortran = bayesfortran_357, switches = "F T F T T T F")
} )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create output directories
sapply(K119_K120_Strata, function(Mix) {dir.create(paste0("BAYES//Output/", Mix))} )

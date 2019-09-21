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
LOKI2R_GAPS.GCL(sillyvec = "KSPORT17", username = username, password = password)  # re-reading in KSPORT17 because I messed up the metadata matching

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
SEAKobjects <- SEAKobjects[-which(SEAKobjects == "Vials" | SEAKobjects == "OLD_BAD_LOCUSCONTROL")]
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

KSPORT17.gcl$attributes$DNA_FISH_ID <- paste(KSPORT17.gcl$attributes$DNA_TRAY_CODE, KSPORT17.gcl$attributes$DNA_TRAY_WELL_CODE, sep = "_")
D8_D11_Sport.dat$DNA_FISH_ID <- paste(D8_D11_Sport.dat$GSI_CARD, D8_D11_Sport.dat$GsiCardRow, sep = "_")

D8_D11_Sport.WGC.match <- match(KSPORT17.gcl$attributes$DNA_FISH_ID, D8_D11_Sport.dat$DNA_FISH_ID)
any(is.na(D8_D11_Sport.WGC.match))  # FALSE
KSPORT17.gcl$attributes$DNA_FISH_ID[is.na(D8_D11_Sport.WGC.match)]

KSPORT17.gcl$attributes$District <- D8_D11_Sport.dat$DISTRICT[D8_D11_Sport.WGC.match]
KSPORT17.gcl$attributes$StatWeek <- D8_D11_Sport.dat$STATWEEK[D8_D11_Sport.WGC.match]
KSPORT17.gcl$attributes$Size <- D8_D11_Sport.dat$Size[D8_D11_Sport.WGC.match]

table(KSPORT17.gcl$attributes$StatWeek, useNA = "always")  # All fish are from SW 17-29

table(KSPORT17.gcl$attributes$Size, KSPORT17.gcl$attributes$District, useNA = "always")
#           101 102 103 104 105 106 107 108 110 111 112 113 114 181 183 325 365 <NA>
# LARGE   3   0   0   0   0   0  51  11 182  11 148   0   5   1   0   0   0   0    0
# SMALL   1   0   0   0   0   0   7   0  11   1  56   1   0   0   0   0   0   0    0
# <NA>    0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0   0    0
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
dput(x = KSPORT17.gcl, file = "Raw genotypes/OriginalCollections_Attributes/KSPORT17.txt")  # overwrite with correct attributes (by fish not gy GSI_CARD)

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
# EWintNISISO_2017.gcl     EWintNO_2017.gcl 
#                   85                  286 


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
# LWintNISISO_2017.gcl     LWintNO_2017.gcl 
#                  198                  301 


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
# SpringRet1NI_2017.gcl SpringRet1NO_2017.gcl SpringRet1SI_2017.gcl SpringRet1SO_2017.gcl SpringRet2NI_2017.gcl SpringRet2NO_2017.gcl SpringRet2SI_2017.gcl SpringRet2SO_2017.gcl
#                    89                   221                   204                    41                    62                   166                   165                    62


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
# LARGE   3   0   0   0   0   0  51  11 182  11 148   0   5   1   0   0   0   0
# SMALL   1   0   0   0   0   0   7   0  11   1  56   1   0   0   0   0   0   0
D108Sport_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KSPORT17", attribute = "District", matching = 108),
                                                         AttributesToIDs.GCL(silly = "KSPORT17", attribute = "Size", matching = "LARGE"))), nm = "KSPORT17")
PoolCollections.GCL(collections = "KSPORT17", loci = GAPSLoci_reordered, IDs = D108Sport_2017.vials, newname = "D108Sport_2017")

D111Sport_2017.vials <- setNames(object = list(intersect(AttributesToIDs.GCL(silly = "KSPORT17", attribute = "District", matching = 111),
                                                         AttributesToIDs.GCL(silly = "KSPORT17", attribute = "Size", matching = "LARGE"))), nm = "KSPORT17")
PoolCollections.GCL(collections = "KSPORT17", loci = GAPSLoci_reordered, IDs = D111Sport_2017.vials, newname = "D111Sport_2017")

sapply(grep(pattern = "Sport", objects(pattern = "\\.gcl"), value = TRUE), function(silly) {get(silly)$n})
# D108Sport_2017.gcl D111Sport_2017.gcl 
#                182                148 


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
# MarkSelect_2016.gcl MarkSelectNISI_2017.gcl   MarkSelectNO_2017.gcl   MarkSelectSO_2017.gcl 
#                 114                     107                     152                     220 


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

dput(x = D108Sport_2017.vials, file = "Objects/Vials/D108Sport_2017.vials.txt")
dput(x = D111Sport_2017.vials, file = "Objects/Vials/D111Sport_2017.vials.txt")

invisible(sapply(objects(pattern = "Mixtures"), function(obj) {
  dput(x = get(obj), file = paste0("Objects/", obj, ".txt"))
} ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save .gcl's with additional attributes data as back-up:
# dir.create("Raw genotypes/OriginalCollections_Attributes_Strata")
invisible(sapply(c(EWint_Mixtures, LWint_Mixtures, SpringRet1_Mixtures, SpringRet2_Mixtures, TBR_Mixtures, MSF_Mixtures), function(silly) {
  dput(x = get(paste0(silly, ".gcl")), file = paste0("Raw genotypes/OriginalCollections_Attributes_Strata/" , silly, ".txt"))
} )); beep(8)
dput(x = D108Sport_2017.gcl, file = "Raw genotypes/OriginalCollections_Attributes_Strata/D108Sport_2017.txt")
dput(x = D111Sport_2017.gcl, file = "Raw genotypes/OriginalCollections_Attributes_Strata/D111Sport_2017.txt")

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
SEAKobjects <- SEAKobjects[-which(SEAKobjects == "Vials" | SEAKobjects == "OLD_BAD_LOCUSCONTROL")]
SEAKobjects

invisible(sapply(SEAKobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Objects", objct, sep = "/")), pos = 1) })); beep(2)

## Get un-altered mixtures
invisible(sapply(c(EWint_Mixtures, LWint_Mixtures, SpringRet1_Mixtures, SpringRet2_Mixtures, TBR_Mixtures, MSF_Mixtures), function(silly) {assign(x = paste0(silly, ".gcl"), value = dget(file = paste0("Raw genotypes/OriginalCollections_Attributes_Strata/", silly, ".txt")), pos = 1)} )); beep(2)
objects(pattern = "\\.gcl")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Data QC/Massage ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

require(xlsx)

K119_K120_Strata <- c(EWint_Mixtures, LWint_Mixtures, SpringRet1_Mixtures, SpringRet2_Mixtures, TBR_Mixtures, MSF_Mixtures)
dput(x = K119_K120_Strata, file = "Objects/K119_K120_Strata.txt")

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
dput(x = D108Sport_2017.gcl, file = "Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/D108Sport_2017.txt")
dput(x = D111Sport_2017.gcl, file = "Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/D111Sport_2017.txt")


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
SEAKobjects <- SEAKobjects[-which(SEAKobjects == "Vials" | SEAKobjects == "OLD_BAD_LOCUSCONTROL")]
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
GroupNames3 <- c("TakuStikine", "Andrew", "Other")
dput(x = GroupNames3, file = "Objects/GroupNames3.txt")
GroupVec3 <- c(1, 2, 1, 3, 3)
dput(x = GroupVec3, file = "Objects/GroupVec3.txt")
GroupNames2 <- c("TakuStikine", "Other")
dput(x = GroupNames2, file = "Objects/GroupNames2.txt")
GroupVec2 <- c(1, 2, 1, 2, 2)
dput(x = GroupVec2, file = "Objects/GroupVec2.txt")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dumping Mixture Files
mixfortran <- CreateMixture.GCL(sillys = "EWintNO_2017", loci = GAPSLoci_reordered, IDs = NULL, mixname = "EWintNO_2017", dir = "BAYES/Mixture", type = "BAYES", PT = FALSE)
dput(x = mixfortran, file = "Objects/mixfortran.txt")

sapply(K119_K120_Strata, function(Mix) {
  CreateMixture.GCL(sillys = Mix, loci = GAPSLoci_reordered, IDs = NULL, mixname = Mix, dir = "BAYES//Mixture", type = "BAYES", PT = FALSE)
} )

sapply(c("D108Sport_2017", "D111Sport_2017"), function(Mix) {
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
  Prior.GCL(groupvec = GroupVec5RG_357, groupweights = mix, minval = 0.01)
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
sapply(c("D108Sport_2017", "D111Sport_2017"), function(Mix) {dir.create(paste0("BAYES//Output/", Mix))} )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Missing allele in MarkSelectSO_2017 ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Read in baseline
GAPS357pops13loci <- read.table(file = "BAYES/Baseline/GAPS357pops13loci.bse")
str(GAPS357pops13loci)

by(GAPS357pops13loci[, 4:73], GAPS357pops13loci[, 2], colSums)  # stupid format
GAPS357pops13loci.df <- aggregate(x = GAPS357pops13loci[, -c(1:3)], by = list(GAPS357pops13loci$V2), FUN = sum)
str(GAPS357pops13loci.df)
GAPS357pops13loci.df <- GAPS357pops13loci.df[, -1]
rownames(GAPS357pops13loci.df) <- GAPSLoci_reordered
colnames(GAPS357pops13loci.df) <- paste("Allele", 1:70)

str(MarkSelectSO_2017.gcl$counts)
MarkSelectSO_2017.gcl$counts[1, , ]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Do any fish from MarkSelectSO_2017 have an allele that doesn't exist in the baseline?
any(apply(MarkSelectSO_2017.gcl$counts[, , ], 1, function(ind) {
  any(which(ind > 0) %in% which(GAPS357pops13loci.df == 0))
} ))

# Which fish is it?
which(apply(MarkSelectSO_2017.gcl$counts[, , ], 1, function(ind) {
  any(which(ind > 0) %in% which(GAPS357pops13loci.df == 0))
} ))  # Fish "212", 211 index

which(MarkSelectSO_2017.gcl$counts["212", , ] > 0) %in% which(GAPS357pops13loci.df == 0)  # final allele
which(MarkSelectSO_2017.gcl$counts["212", , ] > 0)[which(MarkSelectSO_2017.gcl$counts["212", , ] > 0) %in% which(GAPS357pops13loci.df == 0)]  # 660

MarkSelectSO_2017.gcl$counts["212", , ][660]
k <- arrayInd(ind = 660, .dim = dim(MarkSelectSO_2017.gcl$counts["212", , ]))
GAPSLoci_reordered[k[, 1]]  # Oki100v1

MarkSelectSO_2017.gcl$scores["212", GAPSLoci_reordered[k[, 1]], ]
# Dose_1 Dose_2 
#  "268"  "365" 

LocusControl$alleles[GAPSLoci_reordered[k[, 1]]]

MarkSelectSO_2017.gcl$counts["212", k[, 1], ]
MarkSelectSO_2017.gcl$scores["212", k[, 1], ]
which(LocusControl$alleles[[GAPSLoci_reordered[k[, 1]]]] == "268")
which(LocusControl$alleles[[GAPSLoci_reordered[k[, 1]]]] == "365")

GAPS357pops13loci.df[GAPSLoci_reordered[k[, 1]], ]

# Summary: Lost one fish ("212") from MarkSelectSO_2017 due to the presence of an unseen allele, specifically Allele 51 ("365") for Oki100v1


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Do any fish from MarkSelectSO_2017 have an allele that doesn't exist in the baseline?
any(apply(SpringRet2NO_2017.gcl$counts[, , ], 1, function(ind) {
  any(which(ind > 0) %in% which(GAPS357pops13loci.df == 0))
} ))

# Which fish is it?
which(apply(SpringRet2NO_2017.gcl$counts[, , ], 1, function(ind) {
  any(which(ind > 0) %in% which(GAPS357pops13loci.df == 0))
} ))  # Fish "3", 3 index

which(SpringRet2NO_2017.gcl$counts["3", , ] > 0) %in% which(GAPS357pops13loci.df == 0)  # final allele
which(SpringRet2NO_2017.gcl$counts["3", , ] > 0)[which(SpringRet2NO_2017.gcl$counts["3", , ] > 0) %in% which(GAPS357pops13loci.df == 0)]  # 23

SpringRet2NO_2017.gcl$counts["3", , ][23]
k <- arrayInd(ind = 23, .dim = dim(SpringRet2NO_2017.gcl$counts["3", , ]))
GAPSLoci_reordered[k[, 1]]  # Oki100v1

SpringRet2NO_2017.gcl$scores["3", GAPSLoci_reordered[k[, 1]], ]
# Dose_1 Dose_2 
#  "176"  "248" 

LocusControl$alleles[GAPSLoci_reordered[k[, 1]]]

SpringRet2NO_2017.gcl$counts["3", k[, 1], ]
SpringRet2NO_2017.gcl$scores["3", k[, 1], ]
which(LocusControl$alleles[[GAPSLoci_reordered[k[, 1]]]] == "176")
which(LocusControl$alleles[[GAPSLoci_reordered[k[, 1]]]] == "248")

GAPS357pops13loci.df[GAPSLoci_reordered[k[, 1]], ]

# Summary: Lost one fish ("3") from SpringRet2NO_2017 due to the presence of an unseen allele, specifically Allele 2 ("176") for Oki100v1

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summarize BAYES 26RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EWint_2017_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", mixvec = EWint_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

LWint_2017_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", mixvec = LWint_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

SpringRet1_2017_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", mixvec = SpringRet1_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

SpringRet2_2017_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", mixvec = SpringRet2_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

TBR_2017_5RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:5, groupnames = GroupNames5, maindir = "BAYES/Output", mixvec = TBR_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

MSF_2016_2017_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", mixvec = MSF_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dput EstimatesStats
# dir.create("Estimates objects")
invisible(sapply(objects(pattern = "RG_EstimatesStats"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, ".txt"))
} ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Gelman-Rubin
sapply(objects(pattern = "RG_EstimatesStats"), function(obj) {
  sapply(get(obj), function(Mix) {
    table(Mix[, "GR"] > 1.2)
  } )
} )
# SpringRet1SO has 2 RGs with GR > 1.2

sort(SpringRet1_2017_26RG_EstimatesStats$SpringRet1SO_2017[, "GR"], decreasing = TRUE) # Two RGs with GR > 1.2 are ColumbiaSp and SORCali (both below 1%); not worth running out to 80K
# ColumbiaSp      SORCali         Taku
#   1.241442     1.210065     1.193772

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Stratified Estimates 26RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MSF_2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = MSF_Mixtures[-1], catchvec = c(1957, 466, sum(118, 139)), 
                          newname = "StratifiedMarkSelectFishery2017_90percentCI_26RG", nchains = 5, xlxs = TRUE)

EWint_2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = EWint_Mixtures, catchvec = c(4989, 1599), 
                          newname = "Stratified_EWint_2017_90percentCI_26RG", nchains = 5, xlxs = TRUE)

LWint_2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = LWint_Mixtures, catchvec = c(22509, 14782), 
                          newname = "Stratified_LWint_2017_90percentCI_26RG", nchains = 5, xlxs = TRUE)

SpringRet1_2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = SpringRet1_Mixtures, catchvec = c(807, 2241, 1700, 283), 
                          newname = "Stratified_SpringRet1_2017_90percentCI_26RG", nchains = 5, xlxs = TRUE)

SpringRet2_2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = SpringRet2_Mixtures, catchvec = c(1471, 8507, 1819, 482), 
                          newname = "Stratified_SpringRet2_2017_90percentCI_26RG", nchains = 5, xlxs = TRUE)

Spring_2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures, SpringRet2_Mixtures), catchvec = c(807, 2241, 1700, 283, 1471, 8507, 1819, 482), 
                          newname = "Stratified_Spring_2017_90percentCI_26RG", nchains = 5, xlxs = TRUE)

SpringNO_2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[2], SpringRet2_Mixtures[2]), catchvec = c(2241, 8507), 
                          newname = "Stratified_SpringNO_2017_90percentCI_26RG", nchains = 5, xlxs = TRUE)

SpringNI_2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[1], SpringRet2_Mixtures[1]), catchvec = c(807, 1471), 
                          newname = "Stratified_SpringNI_2017_90percentCI_26RG", nchains = 5, xlxs = TRUE)

SpringSI_2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[3], SpringRet2_Mixtures[3]), catchvec = c(1700, 1819), 
                          newname = "Stratified_SpringSI_2017_90percentCI_26RG", nchains = 5, xlxs = TRUE)



invisible(sapply(objects(pattern = "_2017_26RG_StratifiedEstimates"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, "Stats.txt"))
} ))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summarize BAYES 18RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EWint_2017_18RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", mixvec = EWint_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

LWint_2017_18RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", mixvec = LWint_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

SpringRet1_2017_18RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", mixvec = SpringRet1_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

SpringRet2_2017_18RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", mixvec = SpringRet2_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

TBR_2017_3RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec3, groupnames = GroupNames3, maindir = "BAYES/Output", mixvec = TBR_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

MSF_2016_2017_18RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", mixvec = MSF_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dput EstimatesStats
# dir.create("Estimates objects")
invisible(sapply(objects(pattern = "18RG_EstimatesStats"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, ".txt"))
} ))

invisible(sapply(objects(pattern = "3RG_EstimatesStats"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, ".txt"))
} ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Gelman-Rubin
sapply(objects(pattern = "18RG_EstimatesStats"), function(obj) {
  sapply(get(obj), function(Mix) {
    table(Mix[, "GR"] > 1.2)
  } )
} )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Stratified Estimates 18RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MSF_2017_18RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = MSF_Mixtures[-1], catchvec = c(1957, 466, sum(118, 139)), 
                          newname = "StratifiedMarkSelectFishery2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)

EWint_2017_18RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = EWint_Mixtures, catchvec = c(4989, 1599), 
                          newname = "Stratified_EWint_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)

LWint_2017_18RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = LWint_Mixtures, catchvec = c(22509, 14782), 
                          newname = "Stratified_LWint_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)

SpringRet1_2017_18RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = SpringRet1_Mixtures, catchvec = c(807, 2241, 1700, 283), 
                          newname = "Stratified_SpringRet1_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)

SpringRet2_2017_18RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = SpringRet2_Mixtures, catchvec = c(1471, 8507, 1819, 482), 
                          newname = "Stratified_SpringRet2_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)

Spring_2017_18RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures, SpringRet2_Mixtures), catchvec = c(807, 2241, 1700, 283, 1471, 8507, 1819, 482), 
                          newname = "Stratified_Spring_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)

SpringNO_2017_18RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[2], SpringRet2_Mixtures[2]), catchvec = c(2241, 8507), 
                          newname = "Stratified_SpringNO_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)

SpringNI_2017_18RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[1], SpringRet2_Mixtures[1]), catchvec = c(807, 1471), 
                          newname = "Stratified_SpringNI_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)

SpringSI_2017_18RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[3], SpringRet2_Mixtures[3]), catchvec = c(1700, 1819), 
                          newname = "Stratified_SpringSI_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)


invisible(sapply(objects(pattern = "18RG_StratifiedEstimates"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, "Stats.txt"))
} ))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summarize BAYES 8RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EWint_2017_8RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", mixvec = EWint_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

LWint_2017_8RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", mixvec = LWint_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

SpringRet1_2017_8RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", mixvec = SpringRet1_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

SpringRet2_2017_8RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", mixvec = SpringRet2_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

TBR_2017_2RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec2, groupnames = GroupNames2, maindir = "BAYES/Output", mixvec = TBR_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

MSF_2016_2017_8RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", mixvec = MSF_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dput EstimatesStats
# dir.create("Estimates objects")
invisible(sapply(objects(pattern = "8RG_EstimatesStats"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, ".txt"))
} ))

invisible(sapply(objects(pattern = "2RG_EstimatesStats"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, ".txt"))
} ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Gelman-Rubin
sapply(objects(pattern = "_8RG_EstimatesStats"), function(obj) {
  sapply(get(obj), function(Mix) {
    table(Mix[, "GR"] > 1.2)
  } )
} )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Stratified Estimates 8RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MSF_2017_8RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = MSF_Mixtures[-1], catchvec = c(1957, 466, sum(118, 139)), 
                          newname = "StratifiedMarkSelectFishery2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)

EWint_2017_8RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = EWint_Mixtures, catchvec = c(4989, 1599), 
                          newname = "Stratified_EWint_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)

LWint_2017_8RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = LWint_Mixtures, catchvec = c(22509, 14782), 
                          newname = "Stratified_LWint_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)

SpringRet1_2017_8RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = SpringRet1_Mixtures, catchvec = c(807, 2241, 1700, 283), 
                          newname = "Stratified_SpringRet1_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)

SpringRet2_2017_8RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = SpringRet2_Mixtures, catchvec = c(1471, 8507, 1819, 482), 
                          newname = "Stratified_SpringRet2_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)

Spring_2017_8RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures, SpringRet2_Mixtures), catchvec = c(807, 2241, 1700, 283, 1471, 8507, 1819, 482), 
                          newname = "Stratified_Spring_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)

SpringNO_2017_8RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[2], SpringRet2_Mixtures[2]), catchvec = c(2241, 8507), 
                          newname = "Stratified_SpringNO_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)

SpringNI_2017_8RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[1], SpringRet2_Mixtures[1]), catchvec = c(807, 1471), 
                          newname = "Stratified_SpringNI_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)

SpringSI_2017_8RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[3], SpringRet2_Mixtures[3]), catchvec = c(1700, 1819), 
                          newname = "Stratified_SpringSI_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)


invisible(sapply(objects(pattern = "_8RG_StratifiedEstimates"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, "Stats.txt"))
} ))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summarize BAYES 4RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
EWint_2017_4RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", mixvec = EWint_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

LWint_2017_4RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", mixvec = LWint_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

SpringRet1_2017_4RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", mixvec = SpringRet1_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

SpringRet2_2017_4RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", mixvec = SpringRet2_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

MSF_2016_2017_4RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", mixvec = MSF_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dput EstimatesStats
# dir.create("Estimates objects")
invisible(sapply(objects(pattern = "4RG_EstimatesStats"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, ".txt"))
} ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Gelman-Rubin
sapply(objects(pattern = "_4RG_EstimatesStats"), function(obj) {
  sapply(get(obj), function(Mix) {
    table(Mix[, "GR"] > 1.2)
  } )
} )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Stratified Estimates 4RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
MSF_2017_4RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = MSF_Mixtures[-1], catchvec = c(1957, 466, sum(118, 139)), 
                          newname = "StratifiedMarkSelectFishery2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)

EWint_2017_4RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = EWint_Mixtures, catchvec = c(4989, 1599), 
                          newname = "Stratified_EWint_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)

LWint_2017_4RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = LWint_Mixtures, catchvec = c(22509, 14782), 
                          newname = "Stratified_LWint_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)

SpringRet1_2017_4RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = SpringRet1_Mixtures, catchvec = c(807, 2241, 1700, 283), 
                          newname = "Stratified_SpringRet1_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)

SpringRet2_2017_4RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = SpringRet2_Mixtures, catchvec = c(1471, 8507, 1819, 482), 
                          newname = "Stratified_SpringRet2_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)

Spring_2017_4RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures, SpringRet2_Mixtures), catchvec = c(807, 2241, 1700, 283, 1471, 8507, 1819, 482), 
                          newname = "Stratified_Spring_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)

SpringNO_2017_4RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[2], SpringRet2_Mixtures[2]), catchvec = c(2241, 8507), 
                          newname = "Stratified_SpringNO_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)

SpringNI_2017_4RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[1], SpringRet2_Mixtures[1]), catchvec = c(807, 1471), 
                          newname = "Stratified_SpringNI_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)

SpringSI_2017_4RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[3], SpringRet2_Mixtures[3]), catchvec = c(1700, 1819), 
                          newname = "Stratified_SpringSI_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)

SpringSO_2017_4RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures[4], SpringRet2_Mixtures[4]), catchvec = c(283, 482), 
                          newname = "Stratified_SpringSO_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)

invisible(sapply(objects(pattern = "4RG_StratifiedEstimates"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, "Stats.txt"))
} ))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create 2017 8RG Summary Tables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
K119_K120_Strata_SampleSizes

## Get objects
SEAK17estimatesobjects <- list.files(path = "Estimates objects", recursive = FALSE, pattern = "_8RG")
# SEAK17estimatesobjects <- SEAK17estimatesobjects[-c(grep(pattern = "AllYearTroll", x = SEAK17estimatesobjects), 10)]
SEAK17estimatesobjects

# Dget all estimates stats
invisible(sapply(SEAK17estimatesobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Estimates objects", objct, sep = "/")), pos = 1) })); beep(2)


Troll2017_8RG_StratifiedEstimatesStats <- list("EWintAllQuad_2017" = EWint_2017_8RG_StratifiedEstimatesStats,
                                               "LWintAllQuad_2017" = LWint_2017_8RG_StratifiedEstimatesStats,
                                               "SpringAllQuad_2017" = Spring_2017_8RG_StratifiedEstimatesStats)
dput(x = Troll2017_8RG_StratifiedEstimatesStats, file = "Estimates objects/Troll2017_8RG_StratifiedEstimatesStats.txt")



SEAK17estimatesobjects <- unlist(lapply(SEAK17estimatesobjects, function(objct) {unlist(strsplit(x = objct, split = ".txt"))}))

Troll2017_8RG_EstimatesStats <- list(
  "EWintNO_2017" = EWint_2017_8RG_EstimatesStats[["EWintNO_2017"]],
  "EWintAllQuad_2017" = EWint_2017_8RG_StratifiedEstimatesStats,
  "LWintNO_2017" = LWint_2017_8RG_EstimatesStats[["LWintNO_2017"]],
  "LWintAllQuad_2017" = LWint_2017_8RG_StratifiedEstimatesStats,
  "SpringRet1NO_2017" = SpringRet1_2017_8RG_EstimatesStats[["SpringRet1NO_2017"]],
  "SpringRet1SI_2017" = SpringRet1_2017_8RG_EstimatesStats[["SpringRet1SI_2017"]],
  "SpringRet1AllQuad_2017" = SpringRet1_2017_8RG_StratifiedEstimatesStats,
  "SpringRet2AllQuad_2017" = SpringRet2_2017_8RG_StratifiedEstimatesStats,
  "SpringNO_2017" = SpringNO_2017_8RG_StratifiedEstimatesStats,
  "SpringSI_2017" = SpringSI_2017_8RG_StratifiedEstimatesStats,
  "SpringAllQuad_2017" = Spring_2017_8RG_StratifiedEstimatesStats,
  "MarkSelectSO_2017" = MSF_2016_2017_8RG_EstimatesStats[["MarkSelectSO_2017"]],
  "MarkSelect_2017" = MSF_2017_8RG_StratifiedEstimatesStats
  )
dput(x = Troll2017_8RG_EstimatesStats, file = "Estimates objects/Troll2017_8RG_EstimatesStats.txt")

# Check GR
any(sapply(Troll2017_8RG_EstimatesStats, function(mix) {any(mix[, "GR"] > 1.2)}))


# Reformat estimates stats
Troll2017_8RG_EstimatesStats_Formatted <- sapply(Troll2017_8RG_EstimatesStats, function(yr) {
  matrix(data = yr[, 1:5], nrow = 8, ncol = 5, dimnames = list(GroupNames8Pub, c("Mean", "SD", "Median", "5%", "95%")))
}, simplify = FALSE)
dput(x = Troll2017_8RG_EstimatesStats_Formatted, file = "Estimates objects/Troll2017_8RG_EstimatesStats_Formatted.txt")

Troll2017_8RG_PubNames <- setNames(object = c("Northern Outside Quadrant",
                                              "All Quadrants",
                                              "Northern Outside Quadrant",
                                              "All Quadrants",
                                              "Northern Outside Quadrant",
                                              "Southern Inside Quadrant",
                                              "All Quadrants",
                                              "All Quadrants",
                                              "Northern Outside Quadrant",
                                              "Southern Inside Quadrant",
                                              "All Quadrants",
                                              "Southern Outside Quadrant",
                                              "All Quadrants"), 
                                   nm = names(Troll2017_8RG_EstimatesStats_Formatted))
dput(x = Troll2017_8RG_PubNames, file = "Objects/Troll2017_8RG_PubNames.txt")

SEAK2017Mixtures <- list.files(path = "BAYES/Mixture", pattern = ".mix", full.names = FALSE, recursive = FALSE)
# SEAK2017Mixtures <- SEAK2017Mixtures[-c(grep(pattern = "Done", x = SEAK2017Mixtures), grep(pattern = "OLD_BAD_LOCUSCONTROL", x = SEAK2017Mixtures))]
SEAK2017Mixtures_SampSizes <- sapply(SEAK2017Mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Mixture/", mix)))[1]} )
names(SEAK2017Mixtures_SampSizes) <- sapply(names(SEAK2017Mixtures_SampSizes), function(mix) {unlist(strsplit(x = mix, split = ".mix"))[1]})

Troll2017_8RG_MixNames <- setNames(object = list("EWintNO_2017",
                                                 EWint_Mixtures,
                                                 "LWintNO_2017",
                                                 LWint_Mixtures,
                                                 SpringRet1_Mixtures[2],
                                                 SpringRet1_Mixtures[3],
                                                 SpringRet1_Mixtures,
                                                 SpringRet2_Mixtures,
                                                 c(SpringRet1_Mixtures[2], SpringRet2_Mixtures[2]), 
                                                 c(SpringRet1_Mixtures[3], SpringRet2_Mixtures[3]),
                                                 c(SpringRet1_Mixtures, SpringRet2_Mixtures),
                                                 MSF_Mixtures[3],
                                                 MSF_Mixtures[2:4]),
                                   nm = names(Troll2017_8RG_EstimatesStats_Formatted))
dput(x = Troll2017_8RG_MixNames, file = "Objects/Troll2017_8RG_MixNames.txt")


Troll2017_8RG_SampleSizes <- sapply(Troll2017_8RG_MixNames, function(mix) {sum(SEAK2017Mixtures_SampSizes[mix])} )

# Create fully formatted spreadsheat
EstimatesStats <- Troll2017_8RG_EstimatesStats_Formatted
SampSizes <- Troll2017_8RG_SampleSizes
PubNames <- Troll2017_8RG_PubNames

# dir.create("Estimates tables")

for(mix in names(EstimatesStats)) {
  
  TableX <- matrix(data = "", nrow = 11, ncol = 7)
  TableX[1, 3] <- paste(PubNames[mix], "(n=", SampSizes[mix], ")")
  TableX[2, 6] <- "90% CI"
  TableX[3, 2:7] <- c("Reporting Group", colnames(EstimatesStats[[mix]]))
  TableX[4:11, 1] <- 1:8
  TableX[4:11, 2] <- rownames(EstimatesStats[[mix]])
  TableX[4:11, 3:7] <- formatC(x = EstimatesStats[[mix]], digits = 3, format = "f")
  
  write.xlsx(x = TableX, file = "Estimates tables/Troll2017_8RG_StratifiedEstimatesStats_FormattedPretty.xlsx",
             col.names = FALSE, row.names = FALSE, sheetName = paste(mix, " Troll 8 Driver"), append = TRUE)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create 2017 18RG Summary Tables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
K119_K120_Strata_SampleSizes

## Get objects
SEAK17estimatesobjects <- list.files(path = "Estimates objects", recursive = FALSE, pattern = "_18RG")
# SEAK17estimatesobjects <- SEAK17estimatesobjects[-c(grep(pattern = "AllYearTroll", x = SEAK17estimatesobjects), 10)]
SEAK17estimatesobjects

# Dget all estimates stats
invisible(sapply(SEAK17estimatesobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Estimates objects", objct, sep = "/")), pos = 1) })); beep(2)

SEAK17estimatesobjects <- unlist(lapply(SEAK17estimatesobjects, function(objct) {unlist(strsplit(x = objct, split = ".txt"))}))

Troll2017_18RG_EstimatesStats <- list(
  "EWintNO_2017" = EWint_2017_18RG_EstimatesStats[["EWintNO_2017"]],
  "EWintAllQuad_2017" = EWint_2017_18RG_StratifiedEstimatesStats,
  "LWintNO_2017" = LWint_2017_18RG_EstimatesStats[["LWintNO_2017"]],
  "LWintAllQuad_2017" = LWint_2017_18RG_StratifiedEstimatesStats,
  "SpringRet1NO_2017" = SpringRet1_2017_18RG_EstimatesStats[["SpringRet1NO_2017"]],
  "SpringRet1SI_2017" = SpringRet1_2017_18RG_EstimatesStats[["SpringRet1SI_2017"]],
  "SpringRet1AllQuad_2017" = SpringRet1_2017_18RG_StratifiedEstimatesStats,
  "SpringRet2AllQuad_2017" = SpringRet2_2017_18RG_StratifiedEstimatesStats,
  "SpringNO_2017" = SpringNO_2017_18RG_StratifiedEstimatesStats,
  "SpringSI_2017" = SpringSI_2017_18RG_StratifiedEstimatesStats,
  "SpringAllQuad_2017" = Spring_2017_18RG_StratifiedEstimatesStats,
  "MarkSelectSO_2017" = MSF_2016_2017_18RG_EstimatesStats[["MarkSelectSO_2017"]],
  "MarkSelect_2017" = MSF_2017_18RG_StratifiedEstimatesStats
)
dput(x = Troll2017_18RG_EstimatesStats, file = "Estimates objects/Troll2017_18RG_EstimatesStats.txt")

# Check GR
any(sapply(Troll2017_18RG_EstimatesStats, function(mix) {any(mix[, "GR"] > 1.2)}))


# Reformat estimates stats
Troll2017_18RG_EstimatesStats_Formatted <- sapply(Troll2017_18RG_EstimatesStats, function(yr) {
  matrix(data = yr[, 1:5], nrow = 18, ncol = 5, dimnames = list(GroupNames18Pub, c("Mean", "SD", "Median", "5%", "95%")))
}, simplify = FALSE)
dput(x = Troll2017_18RG_EstimatesStats_Formatted, file = "Estimates objects/Troll2017_18RG_EstimatesStats_Formatted.txt")

Troll2017_18RG_PubNames <- setNames(object = c("Northern Outside Quadrant",
                                              "All Quadrants",
                                              "Northern Outside Quadrant",
                                              "All Quadrants",
                                              "Northern Outside Quadrant",
                                              "Southern Inside Quadrant",
                                              "All Quadrants",
                                              "All Quadrants",
                                              "Northern Outside Quadrant",
                                              "Southern Inside Quadrant",
                                              "All Quadrants",
                                              "Southern Outside Quadrant",
                                              "All Quadrants"), 
                                   nm = names(Troll2017_18RG_EstimatesStats_Formatted))
dput(x = Troll2017_18RG_PubNames, file = "Objects/Troll2017_18RG_PubNames.txt")

SEAK2017Mixtures <- list.files(path = "BAYES/Mixture", full.names = FALSE, recursive = FALSE)
SEAK2017Mixtures <- SEAK2017Mixtures[-c(grep(pattern = "Done", x = SEAK2017Mixtures), grep(pattern = "OLD_BAD_LOCUSCONTROL", x = SEAK2017Mixtures))]
SEAK2017Mixtures_SampSizes <- sapply(SEAK2017Mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Mixture/", mix)))[1]} )
names(SEAK2017Mixtures_SampSizes) <- sapply(names(SEAK2017Mixtures_SampSizes), function(mix) {unlist(strsplit(x = mix, split = ".mix"))[1]})

Troll2017_18RG_MixNames <- setNames(object = list("EWintNO_2017",
                                                 EWint_Mixtures,
                                                 "LWintNO_2017",
                                                 LWint_Mixtures,
                                                 SpringRet1_Mixtures[2],
                                                 SpringRet1_Mixtures[3],
                                                 SpringRet1_Mixtures,
                                                 SpringRet2_Mixtures,
                                                 c(SpringRet1_Mixtures[2], SpringRet2_Mixtures[2]), 
                                                 c(SpringRet1_Mixtures[3], SpringRet2_Mixtures[3]),
                                                 c(SpringRet1_Mixtures, SpringRet2_Mixtures),
                                                 MSF_Mixtures[3],
                                                 MSF_Mixtures[2:4]),
                                   nm = names(Troll2017_18RG_EstimatesStats_Formatted))
dput(x = Troll2017_18RG_MixNames, file = "Objects/Troll2017_18RG_MixNames.txt")


Troll2017_18RG_SampleSizes <- sapply(Troll2017_18RG_MixNames, function(mix) {sum(SEAK2017Mixtures_SampSizes[mix])} )

# Create fully formatted spreadsheat
EstimatesStats <- Troll2017_18RG_EstimatesStats_Formatted
SampSizes <- Troll2017_18RG_SampleSizes
PubNames <- Troll2017_18RG_PubNames

# dir.create("Estimates tables")

for(mix in names(EstimatesStats)) {
  
  TableX <- matrix(data = "", nrow = 21, ncol = 7)
  TableX[1, 3] <- paste(PubNames[mix], "(n=", SampSizes[mix], ")")
  TableX[2, 6] <- "90% CI"
  TableX[3, 2:7] <- c("Reporting Group", colnames(EstimatesStats[[mix]]))
  TableX[4:21, 1] <- 1:18
  TableX[4:21, 2] <- rownames(EstimatesStats[[mix]])
  TableX[4:21, 3:7] <- formatC(x = EstimatesStats[[mix]], digits = 3, format = "f")
  
  write.xlsx(x = TableX, file = "Estimates tables/Troll2017_18RG_StratifiedEstimatesStats_FormattedPretty.xlsx",
             col.names = FALSE, row.names = FALSE, sheetName = paste(mix, " Troll 18RG"), append = TRUE)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create 2017 4RG Summary Tables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
K119_K120_Strata_SampleSizes

## Get objects
SEAK17estimatesobjects <- list.files(path = "Estimates objects", recursive = FALSE, pattern = "_4RG")
# SEAK17estimatesobjects <- SEAK17estimatesobjects[-c(grep(pattern = "AllYearTroll", x = SEAK17estimatesobjects), 10)]
SEAK17estimatesobjects

# Dget all estimates stats
invisible(sapply(SEAK17estimatesobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Estimates objects", objct, sep = "/")), pos = 1) })); beep(2)

SEAK17estimatesobjects <- unlist(lapply(SEAK17estimatesobjects, function(objct) {unlist(strsplit(x = objct, split = ".txt"))}))

Troll2017_4RG_EstimatesStats <- list(
  "EWintNO_2017" = EWint_2017_4RG_EstimatesStats[["EWintNO_2017"]],
  "EWintAllQuad_2017" = EWint_2017_4RG_StratifiedEstimatesStats,
  "LWintNO_2017" = LWint_2017_4RG_EstimatesStats[["LWintNO_2017"]],
  "LWintAllQuad_2017" = LWint_2017_4RG_StratifiedEstimatesStats,
  "SpringRet1NO_2017" = SpringRet1_2017_4RG_EstimatesStats[["SpringRet1NO_2017"]],
  "SpringRet1SI_2017" = SpringRet1_2017_4RG_EstimatesStats[["SpringRet1SI_2017"]],
  "SpringRet1AllQuad_2017" = SpringRet1_2017_4RG_StratifiedEstimatesStats,
  "SpringRet2NO_2017" = SpringRet2_2017_4RG_EstimatesStats[["SpringRet2NO_2017"]],
  "SpringRet2SI_2017" = SpringRet2_2017_4RG_EstimatesStats[["SpringRet2SI_2017"]],
  "SpringRet2AllQuad_2017" = SpringRet2_2017_4RG_StratifiedEstimatesStats,
  "SpringNO_2017" = SpringNO_2017_4RG_StratifiedEstimatesStats,
  "SpringNI_2017" = SpringNI_2017_4RG_StratifiedEstimatesStats,
  "SpringSO_2017" = SpringSO_2017_4RG_StratifiedEstimatesStats,
  "SpringSI_2017" = SpringSI_2017_4RG_StratifiedEstimatesStats,
  "SpringAllQuad_2017" = Spring_2017_4RG_StratifiedEstimatesStats,
  "MarkSelect_2016" = MSF_2016_2017_4RG_EstimatesStats[["MarkSelect_2016"]],
  "MarkSelectNO_2017" = MSF_2016_2017_4RG_EstimatesStats[["MarkSelectNO_2017"]],
  "MarkSelectSO_2017" = MSF_2016_2017_4RG_EstimatesStats[["MarkSelectSO_2017"]],
  "MarkSelectNISI_2017" = MSF_2016_2017_4RG_EstimatesStats[["MarkSelectNISI_2017"]],
  "MarkSelect_2017" = MSF_2017_4RG_StratifiedEstimatesStats
)
dput(x = Troll2017_4RG_EstimatesStats, file = "Estimates objects/Troll2017_4RG_EstimatesStats.txt")

# Check GR
any(sapply(Troll2017_4RG_EstimatesStats, function(mix) {any(mix[, "GR"] > 1.2)}))


# Reformat estimates stats
Troll2017_4RG_EstimatesStats_Formatted <- sapply(Troll2017_4RG_EstimatesStats, function(yr) {
  matrix(data = yr[, 1:5], nrow = 4, ncol = 5, dimnames = list(GroupNames4Pub, c("Mean", "SD", "Median", "5%", "95%")))
}, simplify = FALSE)
dput(x = Troll2017_4RG_EstimatesStats_Formatted, file = "Estimates objects/Troll2017_4RG_EstimatesStats_Formatted.txt")

Troll2017_4RG_PubNames <- setNames(object = c("Northern Outside Quadrant",
                                              "All Quadrants",
                                              "Northern Outside Quadrant",
                                              "All Quadrants",
                                              "Northern Outside Quadrant",
                                              "Southern Inside Quadrant",
                                              "All Quadrants",
                                              "Northern Outside Quadrant",
                                              "Southern Inside Quadrant",
                                              "All Quadrants",
                                              "Northern Outside Quadrant",
                                              "Northern Inside Quadrant",
                                              "Southern Outside Quadrant",
                                              "Southern Inside Quadrant",
                                              "All Quadrants",
                                              "All Quadrants",
                                              "Northern Outside Quadrant",
                                              "Southern Outside Quadrant",
                                              "Inside Quadrants",
                                              "All Quadrants"), 
                                   nm = names(Troll2017_4RG_EstimatesStats_Formatted))
dput(x = Troll2017_4RG_PubNames, file = "Objects/Troll2017_4RG_PubNames.txt")

SEAK2017Mixtures <- list.files(path = "BAYES/Mixture", full.names = FALSE, recursive = FALSE)
SEAK2017Mixtures <- SEAK2017Mixtures[-c(grep(pattern = "Done", x = SEAK2017Mixtures), grep(pattern = "OLD_BAD_LOCUSCONTROL", x = SEAK2017Mixtures))]
SEAK2017Mixtures_SampSizes <- sapply(SEAK2017Mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Mixture/", mix)))[1]} )
names(SEAK2017Mixtures_SampSizes) <- sapply(names(SEAK2017Mixtures_SampSizes), function(mix) {unlist(strsplit(x = mix, split = ".mix"))[1]})

Troll2017_4RG_MixNames <- setNames(object = list("EWintNO_2017",
                                                 EWint_Mixtures,
                                                 "LWintNO_2017",
                                                 LWint_Mixtures,
                                                 SpringRet1_Mixtures[2],
                                                 SpringRet1_Mixtures[3],
                                                 SpringRet1_Mixtures,
                                                 SpringRet2_Mixtures[2],
                                                 SpringRet2_Mixtures[3],
                                                 SpringRet2_Mixtures,
                                                 c(SpringRet1_Mixtures[2], SpringRet2_Mixtures[2]), 
                                                 c(SpringRet1_Mixtures[1], SpringRet2_Mixtures[1]), 
                                                 c(SpringRet1_Mixtures[4], SpringRet2_Mixtures[4]),
                                                 c(SpringRet1_Mixtures[3], SpringRet2_Mixtures[3]),
                                                 c(SpringRet1_Mixtures, SpringRet2_Mixtures),
                                                 MSF_Mixtures[1],
                                                 MSF_Mixtures[2],
                                                 MSF_Mixtures[3],
                                                 MSF_Mixtures[4],
                                                 MSF_Mixtures[2:4]),
                                   nm = names(Troll2017_4RG_EstimatesStats_Formatted))
dput(x = Troll2017_4RG_MixNames, file = "Objects/Troll2017_4RG_MixNames.txt")


Troll2017_4RG_SampleSizes <- sapply(Troll2017_4RG_MixNames, function(mix) {sum(SEAK2017Mixtures_SampSizes[mix])} )

# Create fully formatted spreadsheat
EstimatesStats <- Troll2017_4RG_EstimatesStats_Formatted
SampSizes <- Troll2017_4RG_SampleSizes
PubNames <- Troll2017_4RG_PubNames

# dir.create("Estimates tables")

for(mix in names(EstimatesStats)) {
  
  TableX <- matrix(data = "", nrow = 7, ncol = 7)
  TableX[1, 3] <- paste(PubNames[mix], "(n=", SampSizes[mix], ")")
  TableX[2, 6] <- "90% CI"
  TableX[3, 2:7] <- c("Reporting Group", colnames(EstimatesStats[[mix]]))
  TableX[4:7, 1] <- 1:4
  TableX[4:7, 2] <- rownames(EstimatesStats[[mix]])
  TableX[4:7, 3:7] <- formatC(x = EstimatesStats[[mix]], digits = 3, format = "f")
  
  write.xlsx(x = TableX, file = "Estimates tables/Troll2017_4RG_StratifiedEstimatesStats_FormattedPretty.xlsx",
             col.names = FALSE, row.names = FALSE, sheetName = paste(mix, " Troll 4RG"), append = TRUE)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create 2017 HeatMaps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dir.create("Figures")

# Create layout
layoutmat <- matrix(c(9,1,2,11,
                      9,3,4,11,
                      9,5,6,11,
                      9,7,8,11,
                      12,10,10,13), ncol=4,nrow=5,byrow=T)
SEAKTrollLayout <- layout(layoutmat,widths=c(0.25,1,1,0.25),heights=c(1,1,1,1,0.25))
layout.show(SEAKTrollLayout)

# Set color ramp
library('lattice')
WhiteRedColPalette <- colorRampPalette(colors=c("white","red"))
WhiteRedcol <- level.colors(x=seq(from=0,to=1,by=0.01), at = seq(from=0,to=1,by=0.01), col.regions = WhiteRedColPalette(100))

# Mixture names
EstimatesStats <- Troll2017_8RG_EstimatesStats_Formatted
EstimatesStats <- c(EstimatesStats[1:5],
                    EstimatesStats[7],
                    list("SpringRet2NO_2017" = SpringRet2_2017_8RG_EstimatesStats[["SpringRet2NO_2017"]][, 1:5]),
                    EstimatesStats[8],
                    list("MarkSelectNO_2017" = MSF_2016_2017_8RG_EstimatesStats[["MarkSelectNO_2017"]][, 1:5]),
                    EstimatesStats[13])
dimnames(EstimatesStats[["SpringRet2NO_2017"]]) <- dimnames(EstimatesStats[[1]])
dimnames(EstimatesStats[["MarkSelectNO_2017"]]) <- dimnames(EstimatesStats[[1]])

mixnames <- names(EstimatesStats)

# Create list object with by RG stock comps
HeatmapEstimates <- sapply(GroupNames8Pub, function(RG) {
  matrix(data = sapply(mixnames, function(mix) {EstimatesStats[[mix]][RG, "Mean"] }),
         nrow = 2, ncol = 5, dimnames = list(c("NO", "AllQuad"), c("EWint", "LWint", "SpringRet1", "SpringRet2", "MSF"))
  )
}, simplify = FALSE)
zmax <- max(sapply(HeatmapEstimates, max))
zmax <- 0.6

Testing <- matrix(c(seq(from = 0, to = zmax, length.out = 102), seq(from = 0, to = zmax, length.out = 102)), nrow = 2, ncol = 102, byrow = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot: Can't do a nested layout, writing out as pdf then pasting in other pdf

# pdf("Figures/2017TrollByFisheryQuadrant.pdf", family = "Times", width = 6.5, height = 6.5, title = "2017 Troll By Fishery and Quadrant")
png("Figures/2017TrollByFisheryQuadrant.png", family = "Times", width = 6.5, height = 6.5, units = "in", res = 300)
# x11(width = 6.5, height = 6.5)
par(xaxt = "n", yaxt = "n", omi = rep(0.1, 4), mar = rep(0.1, 4), family = 'serif')
layout(layoutmat,widths=c(0.3,1,1,0.25),heights=c(1,1,1,1,0.4))

## Loop through Reporting Group plots
sapply(GroupNames8Pub, function(RG) {
  image(t(HeatmapEstimates[[RG]])[, c("AllQuad", "NO")], zlim = c(0, zmax), col = WhiteRedcol, xlab = "", ylab = "", breaks = seq(from = 0, to = zmax, length.out = 102), useRaster = TRUE)
  abline(h = 0.5, lwd = 2, col = 'grey')
  abline(v = c(0.135, 0.38, 0.63, 0.875), lwd= 2 , col = 'grey')
  abline(h = c(-0.5, 1.5), v = c(-0.125, 1.125),lwd = 5, col = 'black')
  text(labels = RG, cex = 2, adj = c(0, 0.5), x = -0.1, y = 1)
})

## Plot 10 - Y-axis label
plot.new()
text(labels = "Quadrant", cex = 3, srt = 90, x = 0.3, y = 0.5, adj = c(0.5, 0))
text(labels = "NO", cex = 2, x = 0.99, y = c(0.97, 0.7, 0.43, 0.16), adj = c(1, 0.5))
text(labels = "All", cex = 2, x = 0.99, y = c(0.97, 0.7, 0.43, 0.16) - 0.135, adj = c(1, 0.5))

## Plot 11 - X-axis label
plot.new()
text(labels = "Fishery", cex = 3, adj = c(0.5, 0.5), x = 0.5, y = 0.35)
text(labels = "EW", cex = 2, adj = c(0.5, 0.5), x = c(0.02, 0.56), y = 0.8)
text(labels = "LW", cex = 2, adj = c(0.5, 0.5), x = c(0.02 + 0.115, 0.56 + 0.115), y = 0.8)
text(labels = "SP1", cex = 2, adj = c(0.5, 0.5), x = c(0.02 + 0.22, 0.56 + 0.22), y = 0.8)
text(labels = "SP2", cex = 2, adj = c(0.5, 0.5), x = c(0.02 + 0.33, 0.56 + 0.33), y = 0.8)
text(labels = "MSF", cex = 2, adj = c(0.5, 0.5), x = c(0.02 + 0.43, 0.56 + 0.43), y = 0.8)

## Plot 13 - Legend
image(Testing, col = WhiteRedcol, xlab = "", ylab = "", breaks = seq(from = 0, to = zmax, length.out = 102))
text(labels = "0%", cex = 2.8, adj = c(0.5, 0.5), x = 0.5, y = 0.03)
text(labels = "60%", cex = 2.8, adj = c(0.5, 0.5), x = 0.5, y = 0.98)
abline(h = c(-0.005,  1.005),  v  =  c(-0.5,  1.5), lwd = 5, col = 'black')
dev.off()
dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create 2017 TBR Summary Tables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
K119_K120_Strata_SampleSizes

## Get objects
SEAK17estimatesobjects <- list.files(path = "Estimates objects", recursive = FALSE, pattern = "TBR")
# SEAK17estimatesobjects <- SEAK17estimatesobjects[-c(grep(pattern = "AllYearTroll", x = SEAK17estimatesobjects), 10)]
SEAK17estimatesobjects

# Dget all estimates stats
invisible(sapply(SEAK17estimatesobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Estimates objects", objct, sep = "/")), pos = 1) })); beep(2)

SEAK17estimatesobjects <- unlist(lapply(SEAK17estimatesobjects, function(objct) {unlist(strsplit(x = objct, split = ".txt"))}))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 5RG
# Check GR
any(sapply(TBR_2017_5RG_EstimatesStats, function(mix) {any(mix[, "GR"] > 1.2)}))


# Reformat estimates stats
TBR_2017_5RG_EstimatesStats_Formatted <- sapply(TBR_2017_5RG_EstimatesStats, function(yr) {
  matrix(data = yr[, 1:5], nrow = 5, ncol = 5, dimnames = list(GroupNames5, c("Mean", "SD", "Median", "5%", "95%")))
}, simplify = FALSE)
dput(x = TBR_2017_5RG_EstimatesStats_Formatted, file = "Estimates objects/TBR_2017_5RG_EstimatesStats_Formatted.txt")

#~~~~~~~~~~~~~~~~~~
## 3RG
# Check GR
any(sapply(TBR_2017_3RG_EstimatesStats, function(mix) {any(mix[, "GR"] > 1.2)}))


# Reformat estimates stats
TBR_2017_3RG_EstimatesStats_Formatted <- sapply(TBR_2017_3RG_EstimatesStats, function(yr) {
  matrix(data = yr[, 1:5], nrow = 3, ncol = 5, dimnames = list(GroupNames3, c("Mean", "SD", "Median", "5%", "95%")))
}, simplify = FALSE)
dput(x = TBR_2017_3RG_EstimatesStats_Formatted, file = "Estimates objects/TBR_2017_3RG_EstimatesStats_Formatted.txt")

#~~~~~~~~~~~~~~~~~~
## 2RG
# Check GR
any(sapply(TBR_2017_2RG_EstimatesStats, function(mix) {any(mix[, "GR"] > 1.2)}))

#~~~~~~~~~~~~~~~~~~
# Reformat estimates stats
TBR_2017_2RG_EstimatesStats_Formatted <- sapply(TBR_2017_2RG_EstimatesStats, function(yr) {
  matrix(data = yr[, 1:5], nrow = 2, ncol = 5, dimnames = list(GroupNames2, c("Mean", "SD", "Median", "5%", "95%")))
}, simplify = FALSE)
dput(x = TBR_2017_2RG_EstimatesStats_Formatted, file = "Estimates objects/TBR_2017_2RG_EstimatesStats_Formatted.txt")



TBR2017_5RG_PubNames <- setNames(object = c("District 108 Gillnet",
                                            "District 111 Gillnet",
                                            "District 108 Sport",
                                            "District 111 Sport"), 
                                 nm = names(TBR_2017_5RG_EstimatesStats_Formatted))
dput(x = TBR2017_5RG_PubNames, file = "Objects/TBR2017_5RG_PubNames.txt")

SEAK2017Mixtures <- list.files(path = "BAYES/Mixture", full.names = FALSE, recursive = FALSE)
SEAK2017Mixtures <- SEAK2017Mixtures[-c(grep(pattern = "Done", x = SEAK2017Mixtures), grep(pattern = "OLD_BAD_LOCUSCONTROL", x = SEAK2017Mixtures), grep(pattern = "OLD_BAD_ATTRIBUTES", x = SEAK2017Mixtures))]
SEAK2017Mixtures_SampSizes <- sapply(SEAK2017Mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Mixture/", mix)))[1]} )
names(SEAK2017Mixtures_SampSizes) <- sapply(names(SEAK2017Mixtures_SampSizes), function(mix) {unlist(strsplit(x = mix, split = ".mix"))[1]})

#~~~~~~~~~~~~~~~~~~
# Create fully formatted spreadsheat
SampSizes <- SEAK2017Mixtures_SampSizes
PubNames <- TBR2017_5RG_PubNames

# dir.create("Estimates tables")

for(mix in TBR_Mixtures) {
  
  EstimatesStats <- TBR_2017_5RG_EstimatesStats_Formatted
  nRG <- dim(EstimatesStats[[1]])[1]
  Table1 <- matrix(data = "", nrow = nRG + 7, ncol = 7)
  Table1[1, 1] <- paste(nRG, "reporting groups")
  Table1[2, 1] <- PubNames[mix]
  Table1[3, 1] <- paste0("N = ", SampSizes[mix])
  Table1[3, 3] <- "Relative Contribution"
  Table1[4, 6] <- "90% CI"
  Table1[5, 2:7] <- c("Reporting Group", colnames(EstimatesStats[[mix]]))
  Table1[seq(nRG) + 5, 1] <- seq(nRG)
  Table1[seq(nRG) + 5, 2] <- rownames(EstimatesStats[[mix]])
  Table1[seq(nRG) + 5, 3:7] <- formatC(x = EstimatesStats[[mix]], digits = 3, format = "f")
  
  EstimatesStats <- TBR_2017_3RG_EstimatesStats_Formatted
  nRG <- dim(EstimatesStats[[1]])[1]
  Table2 <- matrix(data = "", nrow = nRG + 7, ncol = 7)
  Table2[1, 1] <- paste(nRG, "reporting groups")
  Table2[2, 1] <- PubNames[mix]
  Table2[3, 1] <- paste0("N = ", SampSizes[mix])
  Table2[3, 3] <- "Relative Contribution"
  Table2[4, 6] <- "90% CI"
  Table2[5, 2:7] <- c("Reporting Group", colnames(EstimatesStats[[mix]]))
  Table2[seq(nRG) + 5, 1] <- seq(nRG)
  Table2[seq(nRG) + 5, 2] <- rownames(EstimatesStats[[mix]])
  Table2[seq(nRG) + 5, 3:7] <- formatC(x = EstimatesStats[[mix]], digits = 3, format = "f")
  
  EstimatesStats <- TBR_2017_2RG_EstimatesStats_Formatted
  nRG <- dim(EstimatesStats[[1]])[1]
  Table3 <- matrix(data = "", nrow = nRG + 7, ncol = 7)
  Table3[1, 1] <- paste(nRG, "reporting groups")
  Table3[2, 1] <- PubNames[mix]
  Table3[3, 1] <- paste0("N = ", SampSizes[mix])
  Table3[3, 3] <- "Relative Contribution"
  Table3[4, 6] <- "90% CI"
  Table3[5, 2:7] <- c("Reporting Group", colnames(EstimatesStats[[mix]]))
  Table3[seq(nRG) + 5, 1] <- seq(nRG)
  Table3[seq(nRG) + 5, 2] <- rownames(EstimatesStats[[mix]])
  Table3[seq(nRG) + 5, 3:7] <- formatC(x = EstimatesStats[[mix]], digits = 3, format = "f")
  
  write.xlsx(x = rbind(Table1, Table2, Table3), file = "Estimates tables/TBR2017_StratifiedEstimatesStats.xlsx",
             col.names = FALSE, row.names = FALSE, sheetName = PubNames[mix], append = TRUE)
  
}


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### TBR Metadata ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# load("Extraction Lists/K120ExtractionLists.RData")

# Create Key
D108Gill_2017.gcl$attributes$DNA_FISH_ID <- sapply(as.character(D108Gill_2017.gcl$attributes$SillySource), function(i) {unlist(strsplit(x = i, split = "_"))[2]} )
D111Gill_2017.gcl$attributes$DNA_FISH_ID <- sapply(as.character(D111Gill_2017.gcl$attributes$SillySource), function(i) {unlist(strsplit(x = i, split = "_"))[2]} )

str(Drift_ASL_Gen_D108.dat)

all(D108Gill_2017.gcl$attributes$DNA_FISH_ID %in% Drift_ASL_Gen_D108.dat$Dna.Specimen.No)
all(D111Gill_2017.gcl$attributes$DNA_FISH_ID %in% Drift_ASL_Gen_D111.dat$Dna.Specimen.No)

# Write metadata with only mixture fish
require(xlsx)
write.xlsx(x = rbind(Drift_ASL_Gen_D108.dat[Drift_ASL_Gen_D108.dat$Dna.Specimen.No %in% D108Gill_2017.gcl$attributes$DNA_FISH_ID, ],
                     Drift_ASL_Gen_D111.dat[Drift_ASL_Gen_D111.dat$Dna.Specimen.No %in% D111Gill_2017.gcl$attributes$DNA_FISH_ID, ]),
           file = "V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17/D8&11 Estimates 2017.xlsx", sheetName = "Gillnet_Metadata", append = TRUE, row.names = FALSE)

# Create Key
D108Sport_2017.gcl$attributes$DNA_FISH_ID <- paste0(as.numeric(D108Sport_2017.gcl$attributes$DNA_TRAY_CODE), D108Sport_2017.gcl$attributes$DNA_TRAY_WELL_CODE)
D111Sport_2017.gcl$attributes$DNA_FISH_ID <- paste0(as.numeric(D111Sport_2017.gcl$attributes$DNA_TRAY_CODE), D111Sport_2017.gcl$attributes$DNA_TRAY_WELL_CODE)

Sport_ASL_Gen_D108_allCards.dat$DNA_FISH_ID <- paste0(Sport_ASL_Gen_D108_allCards.dat$GSI_CARD, Sport_ASL_Gen_D108_allCards.dat$GsiCardRow)
Sport_ASL_Gen_D111_allCards.dat$DNA_FISH_ID <- paste0(Sport_ASL_Gen_D111_allCards.dat$GSI_CARD, Sport_ASL_Gen_D111_allCards.dat$GsiCardRow)

all(D108Sport_2017.gcl$attributes$DNA_FISH_ID %in% Sport_ASL_Gen_D108_allCards.dat$DNA_FISH_ID)
all(D111Sport_2017.gcl$attributes$DNA_FISH_ID %in% Sport_ASL_Gen_D111_allCards.dat$DNA_FISH_ID)

# Write metadata with only mixture fish
write.xlsx(x = rbind(Sport_ASL_Gen_D108_allCards.dat[Sport_ASL_Gen_D108_allCards.dat$DNA_FISH_ID %in% D108Sport_2017.gcl$attributes$DNA_FISH_ID, ],
                     Sport_ASL_Gen_D111_allCards.dat[Sport_ASL_Gen_D111_allCards.dat$DNA_FISH_ID %in% D111Sport_2017.gcl$attributes$DNA_FISH_ID, ]),
           file = "V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17/D8&11 Estimates 2017.xlsx", sheetName = "Sport_Metadata", append = TRUE, row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Add TBR Data to Summary Sheet ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Add data to summary sheet

# Get samples sizes from mixture files
sapply(list.files(path = "BAYES/Mixture", pattern = "D1", full.names = TRUE), function(mix) {dim(read.table(file = mix, header = FALSE))[1]} )


## Get objects
SEAK17estimatesobjects <- list.files(path = "Estimates objects", recursive = FALSE, pattern = "TBR")
# SEAK17estimatesobjects <- SEAK17estimatesobjects[-c(grep(pattern = "AllYearTroll", x = SEAK17estimatesobjects), 10)]
SEAK17estimatesobjects

# Check GR
sapply(SEAK17estimatesobjects[c(1, 3, 5)], function(est) {
  sapply(get(est), function(mix) {
    mix[, "GR"] > 1.2
  })
})

# Dget all estimates stats
invisible(sapply(SEAK17estimatesobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Estimates objects", objct, sep = "/")), pos = 1) }))

SEAK17estimatesobjects <- unlist(lapply(SEAK17estimatesobjects, function(objct) {unlist(strsplit(x = objct, split = ".txt"))}))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Estimates
# D108Gill
write.table(x = cbind(t(TBR_2017_5RG_EstimatesStats$D108Gill_2017[GroupNames5, c("mean", "sd", "5%", "95%")]), "", 
      t(TBR_2017_3RG_EstimatesStats$D108Gill_2017[GroupNames3, c("mean", "sd", "5%", "95%")]), "", 
      t(TBR_2017_2RG_EstimatesStats$D108Gill_2017[GroupNames2, c("mean", "sd", "5%", "95%")])
), file = 'clipboard', row.names = FALSE, col.names = FALSE, sep = "\t")

# D111Gill
write.table(x = cbind(t(TBR_2017_2RG_EstimatesStats$D111Gill_2017[GroupNames2, c("mean", "sd", "5%", "95%")])
), file = 'clipboard', row.names = FALSE, col.names = FALSE, sep = "\t")

# D108Sport
write.table(x = cbind(t(TBR_2017_5RG_EstimatesStats$D108Sport_2017[GroupNames5, c("mean", "sd", "5%", "95%")]), "", 
                      t(TBR_2017_3RG_EstimatesStats$D108Sport_2017[GroupNames3, c("mean", "sd", "5%", "95%")]), "", 
                      t(TBR_2017_2RG_EstimatesStats$D108Sport_2017[GroupNames2, c("mean", "sd", "5%", "95%")])
), file = 'clipboard', row.names = FALSE, col.names = FALSE, sep = "\t")

# D111Sport
write.table(x = cbind(t(TBR_2017_5RG_EstimatesStats$D111Sport_2017[GroupNames5, c("mean", "sd", "5%", "95%")]), "", 
                      t(TBR_2017_3RG_EstimatesStats$D111Sport_2017[GroupNames3, c("mean", "sd", "5%", "95%")]), "", 
                      t(TBR_2017_2RG_EstimatesStats$D111Sport_2017[GroupNames2, c("mean", "sd", "5%", "95%")])
), file = 'clipboard', row.names = FALSE, col.names = FALSE, sep = "\t")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Metadata
# Gillnet
Gillnet.df <- read.table(file = 'clipboard', header = TRUE, stringsAsFactors = FALSE)




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
SEAKobjects <- SEAKobjects[-which(SEAKobjects == "Vials" | SEAKobjects == "OLD_BAD_LOCUSCONTROL")]
SEAKobjects

invisible(sapply(SEAKobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Objects", objct, sep = "/")), pos = 1) })); beep(2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Read in K123 Genotypes: Summer + Sport ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Pull all data for each silly code and create .gcl objects for each
K123Mixtures <- c("KTROL17SU", "KSPORT17")

LOKI2R_GAPS.GCL(sillyvec = K123Mixtures, username = username, password = password)

rm(username, password)
objects(pattern = "\\.gcl")

## Save unaltered .gcl's as back-up:
invisible(sapply(K123Mixtures, function(silly) {dput(x = get(paste0(silly, ".gcl")), file = paste0("Raw genotypes/OriginalCollections/" , silly, ".txt"))} )); beep(8)

## Original sample sizes by SILLY
collection.size.original <- sapply(K123Mixtures, function(silly) get(paste0(silly, ".gcl"))$n)
collection.size.original

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save Objects
dput(x = K123Mixtures, file = "Objects/K123Mixtures.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Add District and StatWeek Info ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(xlsx)
require(dplyr)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Summer 1 Mixtures
# Read in fish data
Summer17.dat <- read.xlsx(file = "Extraction Lists/K123 Summer Troll Sport Extraction.xlsx", 
                          sheetName = "SU1 Extraction Data", stringsAsFactors = FALSE)
str(Summer17.dat)
aggregate(X..Tissues ~ Dist.Quad, data = Summer17.dat, sum)
#   Dist.Quad X..Tissues
# 1       171        380
# 2       172        380
# 3       173        340
# 4       174        182

KTROL17SU.gcl <- dget("Raw genotypes/OriginalCollections/KTROL17SU.txt")
KTROL17SU.gcl$n  # 
str(KTROL17SU.gcl$attributes)

# Merge with attributues table
KTROL17SU.gcl$attributes <- KTROL17SU.gcl$attributes %>% 
  left_join(Summer17.dat, by = c("DNA_TRAY_CODE" = "WGC"))
rownames(KTROL17SU.gcl$attributes) <- KTROL17SU.gcl$attributes$FK_FISH_ID

table(KTROL17SU.gcl$attributes$Dist.Quad, useNA = 'always')
# 171  172  173  174 <NA> 
# 378  379  340  181    0 

# Verify order is correct
all.equal(rownames(KTROL17SU.gcl$counts), as.character(KTROL17SU.gcl$attributes$FK_FISH_ID))


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sport Mixtures
# Read in fish data
Sport17.dat <- read.xlsx(file = "Associated Data/Sport All/_2017_SEAK_SF_Whatman_AWL_25SEP17.xlsx",
                              sheetName = "SEAK Sport", header = TRUE, stringsAsFactors = FALSE)
str(Sport17.dat)
table(Sport17.dat$SITE, Sport17.dat$Biweek)

KSPORT17.gcl <- dget("Raw genotypes/OriginalCollections/KSPORT17.txt")
str(KSPORT17.gcl)

#~~~~~~~~~~~~~~~~~~
# Create data key for merging
KSPORT17.gcl$attributes <- KSPORT17.gcl$attributes %>% 
  mutate(DNA_FISH_ID = paste(as.numeric(DNA_TRAY_CODE), DNA_TRAY_WELL_CODE, sep = "_")) 

Sport17.dat <- Sport17.dat %>% 
  mutate(DNA_FISH_ID = paste(GSI_CARD, GsiCardRow, sep = "_"))

# Verify only on entry per fish
Sport17.dups.log <- duplicated(Sport17.dat$DNA_FISH_ID)
table(Sport17.dups.log)  # 9 duplicates
Sport17.dups.ind <- Sport17.dat$DNA_FISH_ID[Sport17.dups.log]

# Verify that each WGC only has one site
Sport17.dat %>% 
  group_by(GSI_CARD) %>% 
  summarise(nsite = length(unique(SITE))) %>% 
  group_by(nsite) %>% 
  summarise(n())

# Rename GsiCardRow sequentially by the number of fish per WGC
for(ind in Sport17.dups.ind) {
  gsicard <- unique(Sport17.dat$GSI_CARD[Sport17.dat$DNA_FISH_ID %in% ind])
  nind <- sum(Sport17.dat$GSI_CARD %in% gsicard)
  Sport17.dat$GsiCardRow[Sport17.dat$GSI_CARD == gsicard] <- seq(nind)
}

# Redefine DNA_FISH_ID
Sport17.dat <- Sport17.dat %>% 
  mutate(DNA_FISH_ID = paste(GSI_CARD, GsiCardRow, sep = "_"))
str(Sport17.dat)

# Verify only on entry per fish
table(duplicated(Sport17.dat$DNA_FISH_ID))

# Merge with attributes table
KSPORT17.gcl$attributes <- KSPORT17.gcl$attributes %>% 
  left_join(Sport17.dat, by = c("DNA_FISH_ID" = "DNA_FISH_ID"))
rownames(KSPORT17.gcl$attributes) <- KSPORT17.gcl$attributes$FK_FISH_ID

str(KSPORT17.gcl)
addmargins(table(KSPORT17.gcl$attributes$Biweek, KSPORT17.gcl$attributes$SITE, useNA = "always"))
#      CRAIG_KLAWOCK ELFIN_COVE GUSTAVUS JUNEAU KETCHIKAN PETERSBURG SITKA WRANGELL YAKUTAT <NA>  Sum
# 9                0          0        0      0         3          3     0        2       0    0    8
# 10              24          1        0      4        15          4    84       32       6    0  170
# 11              53          7        1     16        24        133   213       31      12    0  490
# 12             142         23        3     34       105         33   305       42      14    0  701
# 13             157         28        3     91        88         21   293       17       4    0  702
# 14             147         13        3     90        85          8   182        5       2    0  535
# 15             110         13        1     16        38          3   151        1       3    0  336
# 16             101         19        3     19        28          0   103        1       2    0  276
# 17               0          0        0      1         0          0     0        0       0    0    1
# <NA>             0          0        0      0         0          0     0        0       0    1    1
# Sum            734        104       14    271       386        205  1331      131      43    1 3220

all.equal(rownames(KSPORT17.gcl$scores), as.character(KSPORT17.gcl$attributes$FK_FISH_ID))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save .gcl's with additional attributes data as back-up:
# dir.create("Raw genotypes/OriginalCollections_Attributes")
invisible(sapply(K123Mixtures, function(silly) {
  dput(x = get(paste0(silly, ".gcl")), file = paste0("Raw genotypes/OriginalCollections_Attributes/" , silly, ".txt"))
} )); beep(2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Define Strata ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Summer Ret 1 Troll
# Create Mixtures for each quadrant for each "retention period" (4 total mixtures since only 1 retention period in 2017)
# 1) NO, 2) NI, 3) SI, and 4) SO quadrants
table(KTROL17SU.gcl$attributes$Dist.Quad)
# 171 172 173 174 
# 378 379 340 181 

SummerRet1NI_2017.vials <- setNames(object = list(AttributesToIDs.GCL(silly = "KTROL17SU", attribute = "Dist.Quad", matching = 173)), nm = "KTROL17SU")
PoolCollections.GCL(collections = "KTROL17SU", loci = GAPSLoci_reordered, IDs = SummerRet1NI_2017.vials, newname = "SummerRet1NI_2017")

SummerRet1NO_2017.vials <- setNames(object = list(AttributesToIDs.GCL(silly = "KTROL17SU", attribute = "Dist.Quad", matching = 171)), nm = "KTROL17SU")
PoolCollections.GCL(collections = "KTROL17SU", loci = GAPSLoci_reordered, IDs = SummerRet1NO_2017.vials, newname = "SummerRet1NO_2017")

SummerRet1SI_2017.vials <- setNames(object = list(AttributesToIDs.GCL(silly = "KTROL17SU", attribute = "Dist.Quad", matching = 174)), nm = "KTROL17SU")
PoolCollections.GCL(collections = "KTROL17SU", loci = GAPSLoci_reordered, IDs = SummerRet1SI_2017.vials, newname = "SummerRet1SI_2017")

SummerRet1SO_2017.vials <- setNames(object = list(AttributesToIDs.GCL(silly = "KTROL17SU", attribute = "Dist.Quad", matching = 172)), nm = "KTROL17SU")
PoolCollections.GCL(collections = "KTROL17SU", loci = GAPSLoci_reordered, IDs = SummerRet1SO_2017.vials, newname = "SummerRet1SO_2017")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sport
# Create Mixtures for 
# 1) Craig (all, already proportional to harvest, to find WCVI for individual ID for origins)
# 2) Sitka (all, already proportional to harvest, to find WCVI for individual ID for origins)
# 3) Ketchikan (all, already proportional to harvest)
# 4) Petersburg/Wrangell (subsample, proportional to harvest)
# 5) Inside (subsample, proportional to harvest)
# 6) Outside Period 1 (thru biweek 13, subsample, proportional to harvest)
# 7) Outside Period 2 (after biweek 13, subsample, proportional to harvest)

addmargins(table(KSPORT17.gcl$attributes$Biweek, KSPORT17.gcl$attributes$SITE, useNA = "always"))
#      CRAIG_KLAWOCK ELFIN_COVE GUSTAVUS JUNEAU KETCHIKAN PETERSBURG SITKA WRANGELL YAKUTAT <NA>  Sum
# 9                0          0        0      0         3          3     0        2       0    0    8
# 10              24          1        0      4        15          4    84       32       6    0  170
# 11              53          7        1     16        24        133   213       31      12    0  490
# 12             142         23        3     34       105         33   305       42      14    0  701
# 13             157         28        3     91        88         21   293       17       4    0  702
# 14             147         13        3     90        85          8   182        5       2    0  535
# 15             110         13        1     16        38          3   151        1       3    0  336
# 16             101         19        3     19        28          0   103        1       2    0  276
# 17               0          0        0      1         0          0     0        0       0    0    1
# <NA>             0          0        0      0         0          0     0        0       0    1    1
# Sum            734        104       14    271       386        205  1331      131      43    1 3220

#~~~~~~~~~~~~~~~~~~
# Craig
CRGSport_2017.vials <- setNames(object = list(na.omit(AttributesToIDs.GCL(silly = "KSPORT17", attribute = "SITE", matching = "CRAIG_KLAWOCK"))), nm = "KSPORT17")
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% CRGSport_2017.vials[[1]]])
PoolCollections.GCL(collections = "KSPORT17", loci = GAPSLoci_reordered, IDs = CRGSport_2017.vials, newname = "CRGSport_2017")
table(CRGSport_2017.gcl$attributes$Biweek, CRGSport_2017.gcl$attributes$SITE)

#~~~~~~~~~~~~~~~~~~
# Sitka
SITSport_2017.vials <- setNames(object = list(na.omit(AttributesToIDs.GCL(silly = "KSPORT17", attribute = "SITE", matching = "SITKA"))), nm = "KSPORT17")
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% SITSport_2017.vials[[1]]])
PoolCollections.GCL(collections = "KSPORT17", loci = GAPSLoci_reordered, IDs = SITSport_2017.vials, newname = "SITSport_2017")
table(SITSport_2017.gcl$attributes$Biweek, SITSport_2017.gcl$attributes$SITE)




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### This is where KS stopped due to P/L ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
save.image("Summer Troll + Sport Mixtures.RData")

# You will need to create mixtures for the other sport
# See V:\Analysis\1_SEAK\Chinook\Mixture\SEAK17\Associated Data\Sport Extractions - Origins.xlsx; sheetname = "Prelim Harvest Detail"
# For the correct proportions of fish to run for the remaining "KTN", "PBGWRN", "Inside", "OutsidePer1", "OutsidePer2" mixtures
# I was intending to create separate silly's for each mixture prior to running Data QC/Massage so that we get accurate sample size info for each mixture
# Once you great all the mixture silly's and perform Data QC/Massage (double check that code before running!), you can create the mixutre files and run BAYES
# You should be good to go from there, but feel free to recycle my code from above if you want to keep the same format that I've been using
# Good luck!!! Thanks so much for your help


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Serena picking up from this point forward ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# I was initially worried when mixture totals did not add up to what Kyle had requested to be run, however, I figured out that 
# the msat importer does not pull in zeros. This project had somewhere aroun 244 fish that completely failed and that is what 
# happened to the missing fish in the mixtures
load("Summer Troll + Sport Mixtures.RData")

### Read in Vials objects created by Serena to double check
## Get objects
SEAKobjects <- list.files(path = "Objects/Vials", pattern = ".txt", recursive = FALSE)
invisible(sapply(SEAKobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Objects/Vials", objct, sep = "/")), pos = 1) })); beep(2)



#~~~~~~~~~~~~~~~~~~
# Ketchikan
KTNSport_2017.vials <- setNames(object = list(na.omit(AttributesToIDs.GCL(silly = "KSPORT17", attribute = "SITE", matching = "KETCHIKAN"))), nm = "KSPORT17")
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% KTNSport_2017.vials[[1]]]) #386, with 397 expected
PoolCollections.GCL(collections = "KSPORT17", loci = GAPSLoci_reordered, IDs = KTNSport_2017.vials, newname = "KTNSport_2017")
table(KTNSport_2017.gcl$attributes$Biweek, KTNSport_2017.gcl$attributes$SITE)

#    KETCHIKAN
# 9          3
# 10        15
# 11        24
# 12       105
# 13        88
# 14        85
# 15        38
# 16        28

#~~~~~~~~~~~~~~~~~~
# Petersburg/Wrangell
# PBGWRNSport_2017.vials <- setNames(object = list(na.omit(AttributesToIDs.GCL(silly = "KSPORT17", attribute = "SITE", matching = c("PETERSBURG", "WRANGELL")))), nm = "KSPORT17")

## KS Tue Jan 09 13:34:11 2018
## Redoing Serena's mixture here, because we need to subsample by port and week to accurately represent harvest
## We have "more" genotypes than we need, so need to randomly subsample to get accurate picture of harvest

# Proportional harvest data from 'V:\Analysis\1_SEAK\Chinook\Mixture\SEAK17\Associated Data\Sport Extractions - Origins.xlsx'; tab Prelim Harvest Detail; H54:G64
PBGWRNSport_2017_Run <- read.table('clipboard', header = TRUE, stringsAsFactors = FALSE, colClasses = rep('numeric', 2))
PBGWRNSport_2017_Run$biweek <- 9:18

# Convert to tall format
require(tidyr)
PBGWRN_Run.df <- PBGWRNSport_2017_Run %>% 
  gather(site, samples, -biweek)

# Subsample by SITE (Port) and Biweek
PBGWRNSport_2017.vials <- list("KSPORT17" = sort(unlist(
  sapply(seq(nrow(PBGWRN_Run.df)), function(row) {
    my.df <- PBGWRN_Run.df[row, ]
    SITE <- ifelse(my.df$site == "PBG", "PETERSBURG", "WRANGELL")
    potential_samples <- na.omit(intersect(AttributesToIDs.GCL(silly = "KSPORT17", attribute = "SITE", matching = SITE),
                                           AttributesToIDs.GCL(silly = "KSPORT17", attribute = "Biweek", matching = my.df$biweek)))
    n_samples <- min(length(potential_samples), my.df$samples)
    if(n_samples > 0) {
      sample(x = potential_samples, size = n_samples, replace = FALSE)  
    }
  } )
)))

table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% PBGWRNSport_2017.vials[[1]]]) # pbg 205, WRN 131 (total 336), 344 expected
PoolCollections.GCL(collections = "KSPORT17", loci = GAPSLoci_reordered, IDs = PBGWRNSport_2017.vials, newname = "PBGWRNSport_2017")
table(PBGWRNSport_2017.gcl$attributes$Biweek, PBGWRNSport_2017.gcl$attributes$SITE)

# PETERSBURG WRANGELL
# 9           3        2
# 10          4       32
# 11         33       31
# 12         33       39
# 13         21       17
# 14          8        5
# 15          3        1
# 16          0        1

dput(x = PBGWRNSport_2017.vials, file = "Objects/Vials/PBGWRNSport_2017.vials.txt")



#~~~~~~~~~~~~~~~~~~
# Outside Period 1 (YAK, GUS, ELF, SIT, CRG through BW 13)
Per1Biweeks <- c("10","11","12","13")
OutsidePorts <- c("YAKUTAT","GUSTAVUS","ELFIN_COVE","SITKA","CRAIG_KLAWOCK")
attributes <- KSPORT17.gcl$attributes

# when I count how many OutsidePer1 samples were selected for extraction, I get a higher number than what is found on the "Prelim Harvest
# Detail" tab. 1474 vs 1273 and I have now ended up with 100 extra in this mixture. I will NOT run this one until Kyle gets back to verify.

# after talking with Sara, we figured out that the TOTAL number of samples were bumped up for some of the full season estimates,
# but we need to stick with the reduced set for the outside estimates, so that is what I have done below. The same goes for
# Period 2


# These are the total number of samples available for Biweek10
Biweek10All <- as.character(subset(attributes, Biweek %in% Per1Biweeks[1] & SITE %in% OutsidePorts)$FK_FISH_ID)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek10All]) 

# CRAIG_KLAWOCK    ELFIN_COVE         SITKA       YAKUTAT 
#      24             1            84             6 
# all are good, except I need to select 74 for Sitka

Biweek10Sitka.vials <- sample(as.character(subset(attributes, Biweek %in% Per1Biweeks[1] & SITE %in% OutsidePorts[4])$FK_FISH_ID), size = 74)
dput(Biweek10Sitka.vials, "Objects/Biweek10Sitka.vials.txt")
Biweek10All <- c(as.character(subset(attributes, Biweek %in% Per1Biweeks[1] & SITE %in% OutsidePorts[-4])$FK_FISH_ID), Biweek10Sitka.vials)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek10All]) 
# CRAIG_KLAWOCK    ELFIN_COVE         SITKA       YAKUTAT 
#      24             1            74             6 


# These are the total number of samples available for Biweek11
Biweek11All <- as.character(subset(attributes, Biweek %in% Per1Biweeks[2] & SITE %in% OutsidePorts)$FK_FISH_ID)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek11All]) 
#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#     53             7             1           213            12
# This time, Sitka and Craig need sample size reductions

Biweek11Sitka.vials <- sample(as.character(subset(attributes, Biweek %in% Per1Biweeks[2] & SITE %in% OutsidePorts[4])$FK_FISH_ID), size = 181)
dput(Biweek11Sitka.vials, "Objects/Biweek11Sitka.vials.txt")

Biweek11Craig.vials <- sample(as.character(subset(attributes, Biweek %in% Per1Biweeks[2] & SITE %in% OutsidePorts[5])$FK_FISH_ID), size = 40)
dput(Biweek11Craig.vials, "Objects/Biweek11Craig.vials.txt")

Biweek11All <- c(as.character(subset(attributes, Biweek %in% Per1Biweeks[2] & SITE %in% OutsidePorts[-c(4, 5)])$FK_FISH_ID), Biweek11Sitka.vials, Biweek11Craig.vials)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek11All]) 
#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#      40             7             1           181            12

# These are the total number of samples available for Biweek12
Biweek12All <- as.character(subset(attributes, Biweek %in% Per1Biweeks[3] & SITE %in% OutsidePorts)$FK_FISH_ID)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek12All]) 
# CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#      142            23             3           305            14 
# Again, Craig and Klawock need reductions

Biweek12Sitka.vials <- sample(as.character(subset(attributes, Biweek %in% Per1Biweeks[3] & SITE %in% OutsidePorts[4])$FK_FISH_ID), size = 298)
dput(Biweek12Sitka.vials, "Objects/Biweek12Sitka.vials.txt")

Biweek12Craig.vials <- sample(as.character(subset(attributes, Biweek %in% Per1Biweeks[3] & SITE %in% OutsidePorts[5])$FK_FISH_ID), size = 112)
dput(Biweek12Craig.vials, "Objects/Biweek12Craig.vials.txt")

Biweek12All <- c(as.character(subset(attributes, Biweek %in% Per1Biweeks[3] & SITE %in% OutsidePorts [-c(4, 5)])$FK_FISH_ID), Biweek12Sitka.vials, Biweek12Craig.vials)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek12All]) 

#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#       112            23             3           298            14 


Biweek13All <- as.character(subset(attributes, Biweek %in% Per1Biweeks[4] & SITE %in% OutsidePorts)$FK_FISH_ID)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek13All]) 
# CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#      157            28             3           293             4
# This time, only Craig needs to be reduced. The total expected for Sitka is 296

Biweek13Craig.vials <- sample(as.character(subset(attributes, Biweek %in% Per1Biweeks[4] & SITE %in% OutsidePorts[5])$FK_FISH_ID), size = 140)
dput(Biweek13Craig.vials, "Objects/Biweek13Craig.vials.txt")

Biweek13All <- c(as.character(subset(attributes, Biweek %in% Per1Biweeks[4] & SITE %in% OutsidePorts [-5])$FK_FISH_ID), Biweek13Craig.vials)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek13All]) 
#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#     140            28             3           293             4


OutsidePer1Sport_2017.vials <- c(Biweek10All, Biweek11All, Biweek12All, Biweek13All)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% OutsidePer1Sport_2017.vials]) # looks good
#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#    316            59             7           846            36


PoolCollections.GCL(collections = "KSPORT17", loci = GAPSLoci_reordered, IDs = list(OutsidePer1Sport_2017.vials), newname = "OutsidePer1Sport_2017")
table(OutsidePer1Sport_2017.gcl$attributes$Biweek, OutsidePer1Sport_2017.gcl$attributes$SITE)

#      CRAIG_KLAWOCK ELFIN_COVE GUSTAVUS SITKA YAKUTAT
#10            24          1        0    74       6
#11            40          7        1   181      12
#12           112         23        3   298      14
#13           140         28        3   293       4



#~~~~~~~~~~~~~~~~~~
# Outside Period 2 (YAK, GUS, ELF, SIT, CRG after BW 13)
Per2Biweeks <- c(14:18)

# These are the total number of samples available for Biweek14 
Biweek14All <- as.character(subset(attributes, Biweek %in% Per2Biweeks[1] & SITE %in% OutsidePorts)$FK_FISH_ID)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek14All]) 
#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#     147            13             3           182             2
# Sitka and Craig both need sample reduction

Biweek14Sitka.vials <- sample(as.character(subset(attributes, Biweek %in% Per2Biweeks[1] & SITE %in% OutsidePorts[4])$FK_FISH_ID), size = 166)
dput(Biweek14Sitka.vials, "Objects/Biweek14Sitka.vials.txt")

Biweek14Craig.vials <- sample(as.character(subset(attributes, Biweek %in% Per2Biweeks[1] & SITE %in% OutsidePorts[5])$FK_FISH_ID), size = 115)
dput(Biweek14Craig.vials, "Objects/Biweek14Craig.vials.txt")

Biweek14All <- c(as.character(subset(attributes, Biweek %in% Per2Biweeks[1] & SITE %in% OutsidePorts [-c(4, 5)])$FK_FISH_ID), Biweek14Sitka.vials, Biweek14Craig.vials)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek14All]) 
#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#       115            13             3           166             2

# These are the total number of samples available for Biweek15 
Biweek15All <- as.character(subset(attributes, Biweek %in% Per2Biweeks[2] & SITE %in% OutsidePorts)$FK_FISH_ID)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek15All]) 
#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#       110            13             1           151             3
# Sitka and Craig both need sample reduction

Biweek15Sitka.vials <- sample(as.character(subset(attributes, Biweek %in% Per2Biweeks[2] & SITE %in% OutsidePorts[4])$FK_FISH_ID), size = 134)
dput(Biweek15Sitka.vials, "Objects/Biweek15Sitka.vials.txt")

Biweek15Craig.vials <- sample(as.character(subset(attributes, Biweek %in% Per2Biweeks[2] & SITE %in% OutsidePorts[5])$FK_FISH_ID), size = 97)
dput(Biweek15Craig.vials, "Objects/Biweek15Craig.vials.txt")

Biweek15All <- c(as.character(subset(attributes, Biweek %in% Per2Biweeks[2] & SITE %in% OutsidePorts [-c(4, 5)])$FK_FISH_ID), Biweek15Sitka.vials, Biweek15Craig.vials)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek15All]) 
#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#       97            13             1           134             3


# These are the total number of samples available for Biweek16
Biweek16All <- as.character(subset(attributes, Biweek %in% Per2Biweeks[3] & SITE %in% OutsidePorts)$FK_FISH_ID)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek16All]) 
#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#      101            19             3           103             23
# Sitka and Craig both need sample reduction

Biweek16Sitka.vials <- sample(as.character(subset(attributes, Biweek %in% Per2Biweeks[3] & SITE %in% OutsidePorts[4])$FK_FISH_ID), size = 93)
dput(Biweek16Sitka.vials,"Objects/Biweek16Sitka.vials.txt")

Biweek16Craig.vials <- sample(as.character(subset(attributes, Biweek %in% Per2Biweeks[3] & SITE %in% OutsidePorts[5])$FK_FISH_ID), size = 79)
dput(Biweek16Craig.vials,"Objects/Biweek16Craig.vials.txt")

Biweek16All <- c(as.character(subset(attributes, Biweek %in% Per2Biweeks[3] & SITE %in% OutsidePorts [-c(4, 5)])$FK_FISH_ID), Biweek16Sitka.vials, Biweek16Craig.vials)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% Biweek16All]) 
#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#       79            19             3            93             2

OutsidePer2Sport_2017.vials <- c(Biweek14All, Biweek15All, Biweek16All)
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% OutsidePer2Sport_2017.vials]) # looks good
#CRAIG_KLAWOCK    ELFIN_COVE      GUSTAVUS         SITKA       YAKUTAT 
#       291            45             7           393             7


PoolCollections.GCL(collections = "KSPORT17", loci = GAPSLoci_reordered, IDs = list(OutsidePer2Sport_2017.vials), newname = "OutsidePer2Sport_2017")
table(OutsidePer2Sport_2017.gcl$attributes$Biweek,OutsidePer2Sport_2017.gcl$attributes$SITE)
#      CRAIG_KLAWOCK ELFIN_COVE GUSTAVUS SITKA YAKUTAT
#14           115         13        3   166       2
#15            97         13        1   134       3
#16            79         19        3    93       2



#~~~~~~~~~~~~~~~~~~
# Inside (I verified with Sara and this is ONLY JNU)
InsideSport_2017.vials <- setNames(object = list(na.omit(AttributesToIDs.GCL(silly = "KSPORT17", attribute = "SITE", matching = "JUNEAU"))), nm = "KSPORT17")
table(KSPORT17.gcl$attributes$SITE[KSPORT17.gcl$attributes$FK_FISH_ID %in% InsideSport_2017.vials[[1]]]) # 271, 274 expected
PoolCollections.GCL(collections = "KSPORT17", loci = GAPSLoci_reordered, IDs = InsideSport_2017.vials, newname = "InsideSport_2017")
table(InsideSport_2017.gcl$attributes$Biweek, InsideSport_2017.gcl$attributes$SITE)

#     JUNEAU
#10      4
#11     16
#12     34
#13     91
#14     90
#15     16
#16     19
#17      1




#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SummerRet1_Mixtures <- paste0("SummerRet1", c("NI", "NO", "SI", "SO"), "_2017")
Sport_Mixtures <- paste0(c("CRG", "SIT", "KTN", "PBGWRN", "Inside", "OutsidePer1", "OutsidePer2"), "Sport_2017")

# Confirm sample sizes
sapply(c(SummerRet1_Mixtures, Sport_Mixtures), function(mix) {
  get(paste0(mix, ".gcl"))$n
} )

#SummerRet1NI_2017     SummerRet1NO_2017     SummerRet1SI_2017     SummerRet1SO_2017         CRGSport_2017         SITSport_2017         KTNSport_2017 
#340                   378                   181                   379                   734                  1331                   386 
#PBGWRNSport_2017      InsideSport_2017 OutsidePer1Sport_2017 OutsidePer2Sport_2017 
#233                   271                  1264                   743

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
invisible(sapply(c(SummerRet1_Mixtures, Sport_Mixtures), function(silly) {
  dput(x = get(paste0(silly, ".gcl")), file = paste0("Raw genotypes/OriginalCollections_Attributes_Strata/" , silly, ".txt"))
} )); beep(2)

# Saving new PNGWRN
dput(x = PBGWRNSport_2017.gcl, file = "Raw genotypes/OriginalCollections_Attributes_Strata/PBGWRNSport_2017.gcl.txt")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Data QC/Massage ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
require(xlsx)

K123_Strata <- c(SummerRet1_Mixtures, Sport_Mixtures)
dput(x = K123_Strata, file = "Objects/K123_Strata.txt")

K123_Strata_SampleSizes <- matrix(data = NA, nrow = length(K123_Strata), ncol = 4, 
                                  dimnames = list(K123_Strata, c("Genotyped", "Missing", "Duplicate", "Final")))

#### Check loci
## Get sample size by locus
Original_K123_Strata_SampleSizebyLocus <- SampSizeByLocus.GCL(sillyvec = K123_Strata, loci = GAPSLoci_reordered)
min(Original_K123_Strata_SampleSizebyLocus)  ## 177
apply(Original_K123_Strata_SampleSizebyLocus, 1, min) / apply(Original_K123_Strata_SampleSizebyLocus, 1, max)  

Original_K123_Strata_PercentbyLocus <- apply(Original_K123_Strata_SampleSizebyLocus, 1, function(row) {row / max(row)} )
which(apply(Original_K123_Strata_PercentbyLocus, 2, min) < 0.8)  # no re-runs!

require(lattice)
new.colors <- colorRampPalette(c("black", "white"))
levelplot(t(Original_K123_Strata_PercentbyLocus), 
          col.regions = new.colors, 
          at = seq(from = 0, to = 1, length.out = 100), 
          main = "% Genotyped", xlab = "SILLY", ylab = "Locus", 
          scales = list(x = list(rot = 90)), 
          aspect = "fill")  # aspect = "iso" will make squares

#### Check individuals
### Initial
## Get number of individuals per silly before removing missing loci individuals
Original_K123_Strata_ColSize <- sapply(paste0(K123_Strata, ".gcl"), function(x) get(x)$n)
K123_Strata_SampleSizes[, "Genotyped"] <- Original_K123_Strata_ColSize

### Missing
## Remove individuals with >20% missing data
K123_Strata_MissLoci <- RemoveIndMissLoci.GCL(sillyvec = K123_Strata, proportion = 0.8)
# all of these will be in addition to those that were never read in to R because they received
# all zeros during genotyping.
dput(x = K123_Strata_MissLoci, file = "Objects/K123_Strata_MissLoci.txt")

## Get number of individuals per silly after removing missing loci individuals
ColSize_K123_Strata_PostMissLoci <- sapply(paste0(K123_Strata, ".gcl"), function(x) get(x)$n)
K123_Strata_SampleSizes[, "Missing"] <- Original_K123_Strata_ColSize - ColSize_K123_Strata_PostMissLoci

### Duplicate
## Check within collections for duplicate individuals.
K123_Strata_DuplicateCheck95MinProportion <- CheckDupWithinSilly.GCL(sillyvec = K123_Strata, loci = GAPSLoci_reordered, quantile = NULL, minproportion = 0.95)
K123_Strata_DuplicateCheckReportSummary <- sapply(K123_Strata, function(x) K123_Strata_DuplicateCheck95MinProportion[[x]]$report)
K123_Strata_DuplicateCheckReportSummary
dput(x = K123_Strata_DuplicateCheckReportSummary, file = "Objects/K123_Strata_DuplicateCheckReportSummary.txt")

## Remove duplicate individuals
K123_Strata_RemovedDups <- RemoveDups.GCL(K123_Strata_DuplicateCheck95MinProportion)

## Get number of individuals per silly after removing duplicate individuals
ColSize_K123_Strata_PostDuplicate <- sapply(paste0(K123_Strata, ".gcl"), function(x) get(x)$n)
K123_Strata_SampleSizes[, "Duplicate"] <- ColSize_K123_Strata_PostMissLoci-ColSize_K123_Strata_PostDuplicate

### Final
K123_Strata_SampleSizes[, "Final"] <- ColSize_K123_Strata_PostDuplicate
K123_Strata_SampleSizes

# dir.create("Output")
write.xlsx(K123_Strata_SampleSizes, file = "Output/K123_Strata_SampleSizes.xlsx")
dput(x = K123_Strata_SampleSizes, file = "Objects/K123_Strata_SampleSizes.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Save PostQC .gcl's as back-up:
# dir.create("Raw genotypes/OriginalCollections_Attributes_Strata_PostQC")
#invisible(sapply(K123_Strata, function(silly) {
#  dput(x = get(paste(silly, ".gcl", sep = '')), file = paste0("Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/" , silly, ".txt"))
#} )); beep(8)
dput(x = SummerRet1NI_2017.gcl, file = "Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/SummerRet1NI_2017.txt")
dput(x = SummerRet1NO_2017.gcl, file = "Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/SummerRet1NO_2017.txt")
dput(x = SummerRet1SI_2017.gcl, file = "Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/SummerRet1SI_2017.txt")
dput(x = SummerRet1SO_2017.gcl, file = "Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/SummerRet1SO_2017.txt")

invisible(sapply(K123_Strata[-c(1:4)], function(silly) {
 dput(x = get(paste(silly, ".gcl", sep = '')), file = paste0("Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/" , silly, ".txt"))
} ))

save.image("Summer Troll + Sport Mixtures_SRO.RData")

# Saving new PNGWRN
dput(x = PBGWRNSport_2017.gcl, file = "Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/PBGWRNSport_2017.gcl.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Clean workspace; dget .gcl objects and Locus Control ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

rm(list = ls(all = TRUE))
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
# setwd("C:/Users/krshedd/Documents/R/SEAK17")
# This sources all of the new GCL functions to this workspace
source("C:/Users/krshedd/Documents/R/Functions.GCL.R")
# source("H:\\Desktop\\R\\Functions.GCL.r")


## Get objects
SEAKobjects <- list.files(path = "Objects", recursive = FALSE)
SEAKobjects <- SEAKobjects[-which(SEAKobjects == "Vials" | SEAKobjects == "OLD_BAD_LOCUSCONTROL")]
SEAKobjects

invisible(sapply(SEAKobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Objects", objct, sep = "/")), pos = 1) })); beep(2)

## Get un-altered mixtures
invisible(sapply(K123_Strata, function(silly) {assign(x = paste0(silly, ".gcl"), value = dget(file = paste0("Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/", silly, ".txt")), pos = 1)} )); beep(2)
objects(pattern = "\\.gcl")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Get/Create MSA Objects Summer Troll ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dir.create(path = "BAYES")
# sapply(c("Control", "Mixture", "Output"), function(folder) {dir.create(path = paste("BAYES", folder, sep = "/"))} )

#file.copy(from = "V:/Analysis/1_SEAK/Chinook/Baseline/GAPS3.0/Objects/SEAKPops357.txt", to = "Objects")
#file.copy(from = "V:/Analysis/1_SEAK/Chinook/Baseline/GAPS3.0/Objects/bayesfortran_357.txt", to = "Objects")
# dir.create("BAYES/Baseline")
#file.copy(from = "V:/Analysis/1_SEAK/Chinook/Baseline/GAPS3.0/BAYES/Baseline/GAPS357pops13loci.bse", to = "BAYES/Baseline")
#file.copy(from = "V:/Analysis/4_Westward/Sockeye/KMA Commercial Harvest 2014-2016/Mixtures/Objects/WASSIPSockeyeSeeds.txt", to = "Objects")
#GAPS357PopsInits <- MultiChainInits.GCL(npops = 357, nchains = 5, prop = 0.9)
#dput(x = GAPS357PopsInits, file = "Objects/GAPS357PopsInits.txt")
GAPS357PopsInits <- dget("Objects/GAPS357PopsInits.txt")
GroupNames5 <- c("Taku", "Andrew", "Stikine", "SSEAK", "Other")
#dput(x = GroupNames5, file = "Objects/GroupNames5.txt")
#GroupVec5RG_357 <- as.numeric(readClipboard())
#dput(x = GroupVec5RG_357, file = "Objects/GroupVec5RG_357.txt")
GroupVec5RG_357 <- dget("Objects/GroupVec5RG_357.txt")
GroupNames3 <- c("TakuStikine", "Andrew", "Other")
#dput(x = GroupNames3, file = "Objects/GroupNames3.txt")
GroupVec3 <- c(1, 2, 1, 3, 3)
#dput(x = GroupVec3, file = "Objects/GroupVec3.txt")
GroupNames2 <- c("TakuStikine", "Other")
#dput(x = GroupNames2, file = "Objects/GroupNames2.txt")
GroupVec2 <- c(1, 2, 1, 2, 2)
#dput(x = GroupVec2, file = "Objects/GroupVec2.txt")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dumping Mixture Files. I am doing the summer retention mixtures separately, because I am not ready to dump the sport mixtures yet.
# Sara asked that I prioritize getting the summer retention estimates done asap per email from Ed and Randy. 12/21 SRO
mixfortran <- CreateMixture.GCL(sillys = "SummerRet1NI_2017", loci = GAPSLoci_reordered, IDs = NULL, mixname = "SummerRet1NI_2017", dir = "BAYES/Mixture", type = "BAYES", PT = FALSE)
#dput(x = mixfortran, file = "Objects/mixfortran.txt")
mixfortran <- CreateMixture.GCL(sillys = "SummerRet1NO_2017", loci = GAPSLoci_reordered, IDs = NULL, mixname = "SummerRet1NO_2017", dir = "BAYES/Mixture", type = "BAYES", PT = FALSE)

mixfortran <- CreateMixture.GCL(sillys = "SummerRet1SI_2017", loci = GAPSLoci_reordered, IDs = NULL, mixname = "SummerRet1SI_2017", dir = "BAYES/Mixture", type = "BAYES", PT = FALSE)

mixfortran <- CreateMixture.GCL(sillys = "SummerRet1SO_2017", loci = GAPSLoci_reordered, IDs = NULL, mixname = "SummerRet1SO_2017", dir = "BAYES/Mixture", type = "BAYES", PT = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create Priors
## First get 2016 Summer Ret1 Estimates
Summer1NI_2016.26RGs <- as.numeric(readClipboard())
dput(Summer1NI_2016.26RGs, "Associated Data/Summer1NI_2016.26RGsEstimates.txt")

Summer1NO_2016.26RGs <- as.numeric(readClipboard())
dput(Summer1NO_2016.26RGs, "Associated Data/Summer1NO_2016.26RGsEstimates.txt")

Summer1SI_2016.26RGs <- as.numeric(readClipboard())
dput(Summer1SI_2016.26RGs, "Associated Data/Summer1SI_2016.26RGsEstimates.txt")

Summer1SO_2016.26RGs <- as.numeric(readClipboard())
dput(Summer1SO_2016.26RGs, "Associated Data/Summer1SO_2016.26RGsEstimates.txt")

# Now create the priors for each
Prior.Summer1NI_2017.26RGs <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = Summer1NI_2016.26RGs, minval = 0.01)
dput(x = Prior.Summer1NI_2017.26RGs, file = "Objects/Prior.Summer1NI_2017.26RGs.txt")

Prior.Summer1NO_2017.26RGs <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = Summer1NO_2016.26RGs, minval = 0.01)
dput(x = Prior.Summer1NO_2017.26RGs, file = "Objects/Prior.Summer1NO_2017.26RGs.txt")

Prior.Summer1SI_2017.26RGs <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = Summer1SI_2016.26RGs, minval = 0.01)
dput(x = Prior.Summer1SI_2017.26RGs, file = "Objects/Prior.Summer1SI_2017.26RGs.txt")

Prior.Summer1SO_2017.26RGs <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = Summer1SO_2016.26RGs, minval = 0.01)
dput(x = Prior.Summer1SO_2017.26RGs, file = "Objects/Prior.Summer1SO_2017.26RGs.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Dumping Control files summer ret1 only
CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname = "SummerRet1NI_2017", basename = "GAPS357pops13loci", suffix = "", 
                        nreps = 40000, nchains = 5, groupvec = GroupVec26RG_357, priorvec = Prior.Summer1NI_2017.26RGs, initmat = 
                        GAPS357PopsInits, dir = "BAYES/Control", seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), mixfortran = mixfortran, 
                        basefortran = bayesfortran_357, switches = "F T F T T T F")

CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname = "SummerRet1NO_2017", basename = "GAPS357pops13loci", suffix = "", 
                      nreps = 40000, nchains = 5, groupvec = GroupVec26RG_357, priorvec = Prior.Summer1NO_2017.26RGs, initmat = 
                      GAPS357PopsInits, dir = "BAYES/Control", seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), mixfortran = mixfortran, 
                      basefortran = bayesfortran_357, switches = "F T F T T T F")

CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname = "SummerRet1SI_2017", basename = "GAPS357pops13loci", suffix = "", 
                      nreps = 40000, nchains = 5, groupvec = GroupVec26RG_357, priorvec = Prior.Summer1SI_2017.26RGs, initmat = 
                      GAPS357PopsInits, dir = "BAYES/Control", seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), mixfortran = mixfortran, 
                      basefortran = bayesfortran_357, switches = "F T F T T T F")

CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname = "SummerRet1SO_2017", basename = "GAPS357pops13loci", suffix = "", 
                      nreps = 40000, nchains = 5, groupvec = GroupVec26RG_357, priorvec = Prior.Summer1SO_2017.26RGs, initmat = 
                      GAPS357PopsInits, dir = "BAYES/Control", seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), mixfortran = mixfortran, 
                      basefortran = bayesfortran_357, switches = "F T F T T T F")

#sapply(TBR_Mixtures, function(Mix) {
#  CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname = Mix, basename = "GAPS357pops13loci", suffix = "", nreps = 40000, nchains = 5,
 #                       groupvec = GroupVec5RG_357, priorvec = TBR_Strata_Priors[, Mix], initmat = GAPS357PopsInits, dir = "BAYES/Control",
  #                      seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), mixfortran = mixfortran, basefortran = bayesfortran_357, switches = "F T F T T T F")
#} )


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Create output directories
sapply(K123_Strata[1:4], function(Mix) {dir.create(paste0("BAYES//Output/", Mix))} )



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summarize BAYES 26RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SummerRet1_2017_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", mixvec = SummerRet1_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)
dput(x = SummerRet1_2017_26RG_EstimatesStats, file = "Estimates objects/SummerRet1_2017_26RG_EstimatesStats.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dput EstimatesStats
# dir.create("Estimates objects")
invisible(sapply(objects(pattern = "RG_EstimatesStats"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, ".txt"))
} ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Check Gelman-Rubin
sapply(objects(pattern = "RG_EstimatesStats"), function(obj) {
  sapply(get(obj), function(Mix) {
    table(Mix[, "GR"] > 1.2)
  } )
} )

# No GR issues


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summarize BAYES 4RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SummerRet1_2017_4RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", mixvec = SummerRet1_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)
dput(x = SummerRet1_2017_4RG_EstimatesStats, file = "Estimates objects/SummerRet1_2017_4RG_EstimatesStats.txt")

SummerRet1_2017_8RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", mixvec = SummerRet1_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)
dput(x = SummerRet1_2017_8RG_EstimatesStats, file = "Estimates objects/SummerRet1_2017_8RG_EstimatesStats.txt")

SummerRet1_2017_18RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", mixvec = SummerRet1_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)
dput(x = SummerRet1_2017_18RG_EstimatesStats, file = "Estimates objects/SummerRet1_2017_18RG_EstimatesStats.txt")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dput EstimatesStats
# dir.create("Estimates objects")
invisible(sapply(objects(pattern = "4RG_EstimatesStats"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, ".txt"))
} ))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
SummerRet1_2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = SummerRet1_Mixtures, catchvec = c(10020, 40721, 4037, 9551), 
                          newname = "Stratified_SummerRet1_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)
dput(x = SummerRet1_2017_26RG_StratifiedEstimates, file = "Estimates objects/SummerRet1_2017_26RG_StratifiedEstimates.txt")

SummerRet1_2017_4RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = SummerRet1_Mixtures, catchvec = c(10020, 40721, 4037, 9551), 
                          newname = "Stratified_SummerRet1_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)
dput(x = SummerRet1_2017_4RG_StratifiedEstimates, file = "Estimates objects/SummerRet1_2017_4RG_StratifiedEstimates.txt")

SummerRet1_2017_8RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = SummerRet1_Mixtures, catchvec = c(10020, 40721, 4037, 9551), 
                          newname = "Stratified_SummerRet1_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)
dput(x = SummerRet1_2017_8RG_StratifiedEstimates, file = "Estimates objects/SummerRet1_2017_8RG_StratifiedEstimates.txt")

SummerRet1_2017_18RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = SummerRet1_Mixtures, catchvec = c(10020, 40721, 4037, 9551), 
                          newname = "Stratified_SummerRet1_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)
dput(x = SummerRet1_2017_18RG_StratifiedEstimates, file = "Estimates objects/SummerRet1_2017_18RG_StratifiedEstimates.txt")

AllTrollnoSummer_2017_8RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = c(EWint_Mixtures,LWint_Mixtures,SpringRet1_Mixtures,SpringRet2_Mixtures), 
                          catchvec = c(4989, 1599, 22509, 14782, 807, 2241, 1700, 283, 1471, 8507, 1819, 482),  newname = "Stratified_AllTrollnoSummer_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)
dput(AllTrollnoSummer_2017_8RG_StratifiedEstimates$Stats, "Estimates objects/AllTrollnoSummer_2017_8RG_StratifiedEstimates.txt")

AllYearTroll2017_8RG_StratifiedEstimatesStats <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = c(EWint_Mixtures,LWint_Mixtures,SpringRet1_Mixtures,SpringRet2_Mixtures, SummerRet1_Mixtures), 
                          catchvec = c(4989, 1599, 22509, 14782, 807, 2241, 1700, 283, 1471, 8507, 1819, 482, 10020, 40721, 4037, 9551),  newname = "Stratified_AllTroll_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)
dput(AllYearTroll2017_8RG_StratifiedEstimatesStats, "Estimates objects/AllYearTroll2017_8RG_StratifiedEstimatesStats.txt")
AllYearTroll2017_8RG_StratifiedEstimatesStats <- dget("Estimates objects/AllYearTroll2017_8RG_StratifiedEstimatesStats.txt")


AllYearTroll2017_18RG_StratifiedEstimatesStats <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = c(EWint_Mixtures,LWint_Mixtures,SpringRet1_Mixtures,SpringRet2_Mixtures, SummerRet1_Mixtures), 
                          catchvec = c(4989, 1599, 22509, 14782, 807, 2241, 1700, 283, 1471, 8507, 1819, 482, 10020, 40721, 4037, 9551),  newname = "Stratified_AllTroll_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)
dput(AllYearTroll2017_18RG_StratifiedEstimatesStats, "Estimates objects/AllYearTroll2017_18RG_StratifiedEstimatesStats.txt")
AllYearTroll2017_18RG_StratifiedEstimatesStats <- dget("Estimates objects/AllYearTroll2017_18RG_StratifiedEstimatesStats.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summarize BAYES 27CTC RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# New CTC 27RG that Bill needs
# This information is found in "V:\Analysis\1_SEAK\Chinook\Baseline\GAPS3.0\ChinookReportingGroups_CTC_NewModel_Jan2018.xlsx"; tab "GroupbyPop"; column M
Groups27RG_CTCMod2_357 <- readClipboard()
GroupNames27_CTCMod2 <- readClipboard()
dput(x = GroupNames27_CTCMod2, file = "Objects/GroupNames27_CTCMod2.txt")
GroupVec27RG_CTCMod2_357 <- as.numeric(factor(x = Groups27RG_CTCMod2_357, levels = unique(GroupNames27_CTCMod2)))
dput(x = GroupVec27RG_CTCMod2_357, file = "Objects/GroupVec27RG_CTCMod2_357.txt")


Troll_2017_27RG_CTCMod2_AnnualStratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec27RG_CTCMod2_357, groupnames = GroupNames27_CTCMod2, maindir = "BAYES/Output", ext = "BOT",
                          mixvec = c(EWint_Mixtures,LWint_Mixtures,SpringRet1_Mixtures,SpringRet2_Mixtures, SummerRet1_Mixtures), 
                          catchvec = c(4989, 1599, 22509, 14782, 807, 2241, 1700, 283, 1471, 8507, 1819, 482, 10020, 40721, 4037, 9551),  
                          newname = "Stratified_AllTroll_2017_90percentCI_27RG_CTCMod2", nchains = 5, xlxs = TRUE)
dput(Troll_2017_27RG_CTCMod2_AnnualStratifiedEstimates, "Estimates objects/Troll_2017_27RG_CTCMod2_AnnualStratifiedEstimates.txt")

Troll_2017_27RG_CTCMod2_AnnualStratifiedEstimates <- dget("Estimates objects/Troll_2017_27RG_CTCMod2_AnnualStratifiedEstimates.txt")
write.table(x = Troll_2017_27RG_CTCMod2_AnnualStratifiedEstimates, file = "Estimates tables/Troll_2017_27RG_CTCMod2_AnnualStratifiedEstimates_Formatted.txt", col.names = NA)

Troll_2017_SampleSizes <- sapply(c(EWint_Mixtures,LWint_Mixtures,SpringRet1_Mixtures,SpringRet2_Mixtures, SummerRet1_Mixtures), function(mix){
  dim(read.table(file = paste0("BAYES/Mixture/", mix, ".mix")))[1]
} )

write.table(x = cbind("Mixture" = c(EWint_Mixtures,LWint_Mixtures,SpringRet1_Mixtures,SpringRet2_Mixtures, SummerRet1_Mixtures),
                      "Catch" = c(4989, 1599, 22509, 14782, 807, 2241, 1700, 283, 1471, 8507, 1819, 482, 10020, 40721, 4037, 9551),
                      "n" = Troll_2017_SampleSizes),
            'clipboard', row.names = FALSE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Get/Create MSA Objects Sport ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Now I will dump mixtures for sport 2017
rm(list = ls(all = TRUE))
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
# This sources all of the new GCL functions to this workspace
source("H:\\Desktop\\R\\Functions.GCL.r")
source("~/../R/Functions.GCL.R")

## Get objects
SEAKobjects <- list.files(path = "Objects", recursive = FALSE)
SEAKobjects <- SEAKobjects[-which(SEAKobjects == "Vials" | SEAKobjects == "OLD_BAD_LOCUSCONTROL")]
SEAKobjects

invisible(sapply(SEAKobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Objects", objct, sep = "/")), pos = 1) })); beep(2)

## Get un-altered mixtures
invisible(sapply(K123_Strata[-c(1:4)], function(silly) {assign(x = paste0(silly, ".gcl"), value = dget(file = paste0("Raw genotypes/OriginalCollections_Attributes_Strata_PostQC/", silly, ".txt")), pos = 1)} )); beep(2)
objects(pattern = "\\.gcl")

## Create mixtures 
sapply(K123_Strata[-c(1:4)], function(Mix) {
  CreateMixture.GCL(sillys = Mix, loci = GAPSLoci_reordered, IDs = NULL, mixname = Mix, dir = "BAYES//Mixture", type = "BAYES", PT = FALSE)
} )

## Re-do PNGWRN
CreateMixture.GCL(sillys = "PBGWRNSport_2017", loci = GAPSLoci_reordered, IDs = NULL, mixname = "PBGWRNSport_2017", dir = "BAYES/Mixture", type = "BAYES", PT = FALSE)


## Create Priors
## First get 2016 Estimates
SITSport_2016.26RGs <- as.numeric(readClipboard())
dput(SITSport_2016.26RGs, "Objects/SITSport_2016.26RGsEstimates.txt")
SITSport_2016.26RGs <- dget("Objects/SITSport_2016.26RGsEstimates.txt")

CRGSport_2016.26RGs <- as.numeric(readClipboard())
dput(CRGSport_2016.26RGs, "Objects/CRGSport_2016.26RGsEstimates.txt")
CRGSport_2016.26RGs <- dget("Objects/CRGSport_2016.26RGsEstimates.txt")

KTNSport_2016.26RGs <- as.numeric(readClipboard())
dput(KTNSport_2016.26RGs, "Objects/KTNSport_2016.26RGsEstimates.txt")
KTNSport_2016.26RGs <- dget("Objects/KTNSport_2016.26RGsEstimates.txt")

PBGWRNSport_2016.26RGs <- as.numeric(readClipboard())
dput(PBGWRNSport_2016.26RGs, "Objects/PBGWRNSport_2016.26RGsEstimates.txt")
PBGWRNSport_2016.26RGs <- dget("Objects/PBGWRNSport_2016.26RGsEstimates.txt")

InsideSport_2016.26RGs <- as.numeric(readClipboard())
dput(InsideSport_2016.26RGs, "Objects/InsideSport_2016.26RGsEstimates.txt")
InsideSport_2016.26RGs <- dget( "Objects/InsideSport_2016.26RGsEstimates.txt")

OutsidePer1Sport_2016.26RGs <- as.numeric(readClipboard())
dput(OutsidePer1Sport_2016.26RGs, "Objects/OutsidePer1Sport_2016.26RGsEstimates.txt")
OutsidePer1Sport_2016.26RGs <- dget( "Objects/OutsidePer1Sport_2016.26RGsEstimates.txt")

OutsidePer2Sport_2016.26RGs <- as.numeric(readClipboard())
dput(OutsidePer2Sport_2016.26RGs, "Objects/OutsidePer2Sport_2016.26RGsEstimates.txt")
OutsidePer2Sport_2016.26RGs <- dget("Objects/OutsidePer2Sport_2016.26RGsEstimates.txt")


# Now create the priors for each
Prior.SITSport_2017.26RGs <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = SITSport_2016.26RGs, minval = 0.01)
dput(x = Prior.SITSport_2017.26RGs, file = "Objects/Prior.SITSport_2017.26RGs.txt")

Prior.CRGSport_2017.26RGs <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = CRGSport_2016.26RGs, minval = 0.01)
dput(x = Prior.CRGSport_2017.26RGs, file = "Objects/Prior.CRGSport_2017.26RGs.txt")

Prior.KTNSport_2017.26RGs <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = KTNSport_2016.26RGs, minval = 0.01)
dput(x = Prior.KTNSport_2017.26RGs, file = "Objects/Prior.KTNSport_2017.26RGs.txt")

Prior.PBGWRNSport_2017.26RGs <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = PBGWRNSport_2016.26RGs, minval = 0.01)
dput(x = Prior.PBGWRNSport_2017.26RGs, file = "Objects/Prior.PBGWRNSport_2017.26RGs.txt")

Prior.InsideSport_2017.26RGs <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = InsideSport_2016.26RGs, minval = 0.01)
dput(x = Prior.InsideSport_2017.26RGs, file = "Objects/Prior.InsideSport_2017.26RGs.txt")

Prior.OutsidePer1Sport_2017.26RGs <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = OutsidePer1Sport_2016.26RGs, minval = 0.01)
dput(x = Prior.OutsidePer1Sport_2017.26RGs, file = "ObjectsPrior.OutsidePer1Sport_2017.26RGs.txt")

Prior.OutsidePer2Sport_2017.26RGs <- Prior.GCL(groupvec = GroupVec26RG_357, groupweights = OutsidePer2Sport_2016.26RGs, minval = 0.01)
dput(x = Prior.OutsidePer2Sport_2017.26RGs, file = "ObjectsPrior.OutsidePer2Sport_2017.26RGs.txt")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dumping Control Files for sport only
CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname =  "SITSport_2017", basename = "GAPS357pops13loci", 
                      suffix = "", nreps = 40000, nchains = 5,groupvec = GroupVec26RG_357, priorvec = Prior.SITSport_2017.26RGs, 
                      initmat = GAPS357PopsInits,  dir = "BAYES/Control", seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), 
                      mixfortran = mixfortran, basefortran = bayesfortran_357, switches = "F T F T T T F")

CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname =  "CRGSport_2017", basename = "GAPS357pops13loci", 
                      suffix = "", nreps = 40000, nchains = 5,groupvec = GroupVec26RG_357, priorvec = Prior.CRGSport_2017.26RGs, 
                      initmat = GAPS357PopsInits,  dir = "BAYES/Control", seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), 
                      mixfortran = mixfortran, basefortran = bayesfortran_357, switches = "F T F T T T F")

CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname =  "KTNSport_2017", basename = "GAPS357pops13loci", 
                      suffix = "", nreps = 40000, nchains = 5,groupvec = GroupVec26RG_357, priorvec = Prior.KTNSport_2017.26RGs, 
                      initmat = GAPS357PopsInits,  dir = "BAYES/Control", seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), 
                      mixfortran = mixfortran, basefortran = bayesfortran_357, switches = "F T F T T T F")

CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname =  "PBGWRNSport_2017", basename = "GAPS357pops13loci", 
                      suffix = "", nreps = 40000, nchains = 5,groupvec = GroupVec26RG_357, priorvec = Prior.PBGWRNSport_2017.26RGs, 
                      initmat = GAPS357PopsInits,  dir = "BAYES/Control", seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), 
                      mixfortran = mixfortran, basefortran = bayesfortran_357, switches = "F T F T T T F")

CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname =  "InsideSport_2017", basename = "GAPS357pops13loci", 
                      suffix = "", nreps = 40000, nchains = 5,groupvec = GroupVec26RG_357, priorvec = Prior.InsideSport_2017.26RGs, 
                      initmat = GAPS357PopsInits,  dir = "BAYES/Control", seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), 
                      mixfortran = mixfortran, basefortran = bayesfortran_357, switches = "F T F T T T F")

CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname =  "OutsidePer1Sport_2017", basename = "GAPS357pops13loci", 
                      suffix = "", nreps = 40000, nchains = 5,groupvec = GroupVec26RG_357, priorvec = Prior.OutsidePer1Sport_2017.26RGs, 
                      initmat = GAPS357PopsInits,  dir = "BAYES/Control", seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), 
                      mixfortran = mixfortran, basefortran = bayesfortran_357, switches = "F T F T T T F")

CreateControlFile.GCL(sillyvec = SEAKPops357, loci = GAPSLoci_reordered, mixname =  "OutsidePer2Sport_2017", basename = "GAPS357pops13loci", 
                      suffix = "", nreps = 40000, nchains = 5,groupvec = GroupVec26RG_357, priorvec = Prior.OutsidePer2Sport_2017.26RGs, 
                      initmat = GAPS357PopsInits,  dir = "BAYES/Control", seeds = WASSIPSockeyeSeeds, thin = c(1, 1, 100), 
                      mixfortran = mixfortran, basefortran = bayesfortran_357, switches = "F T F T T T F")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summarize BAYES 8RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sport_2017_33RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir = "BAYES/Output", mixvec = Sport_Mixtures,
                               prior = "", ext = "BOT", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)
names(Sport_2017_33RG_EstimatesStats)
dput(x = Sport_2017_33RG_EstimatesStats, file = "Estimates objects/Sport_2017_33RG_EstimatesStats.txt")
Sport_2017_33RG_EstimatesStats <- dget("Estimates objects/Sport_2017_33RG_EstimatesStats.txt")

Sport_2017_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", mixvec = Sport_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

round(Sport_2017_26RG_EstimatesStats$PBGWRNSport_2017, 2)
Sport_2017_26RG_EstimatesStats <- dget("Estimates objects/Sport_2017_26RG_EstimatesStats.txt")


Sport_2017_8RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", mixvec = Sport_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

Sport_2017_18RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", mixvec = Sport_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)
dput(x = Sport_2017_18RG_EstimatesStats, file = "Estimates objects/Sport_2017_18RG_EstimatesStats.txt")

Sport_2017_4RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", mixvec = Sport_Mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)


sapply(Sport_2017_8RG_EstimatesStats, function(mix) {
  all(mix[, "GR"] < 1.2)
} )
round(Sport_2017_8RG_EstimatesStats$PBGWRNSport_2017, 2)

# Summarizing 80K PGGWRN
PBGWRNSport_2017_80Kestimates <- CustomCombineBAYESOutput.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", mixvec = "PBGWRNSport_2017_80K",
                             prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)
# Running longer chains did not remove the GR issues. 12/27/17 SRO

dput(PBGWRNSport_2017_80Kestimates, file = "Estimates objects/PBGWRNSport_2017_80Kestimates.txt")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Dput EstimatesStats
invisible(sapply(objects(pattern = "RG_EstimatesStats"), function(obj) {
  dput(x = get(obj), file = paste0("Estimates objects/", obj, ".txt"))
} ))


# Rerunning PBGWRN with 80K iterations as there were 2 groups with high GR. WACoast (GR = 5.0) and Other (GR = 2.9).
# I do not think that extending the iterations will help, however it is protocol. We will likely need to combine
# the chains that converge and drop those that don't for final estimates.

# Outside
OutsideSport_2017_26RG_StratifiedEstimatesStats <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = Sport_Mixtures[6:7], 
                          catchvec = c(20870, 12114),  newname = "Stratified_OutsideSport_2017_90percentCI_26RG", nchains = 5, xlxs = TRUE)
dput(OutsideSport_2017_26RG_StratifiedEstimatesStats, "Estimates objects/OutsideSport_2017_26RG_StratifiedEstimatesStats.txt")
OutsideSport_2017_26RG_StratifiedEstimatesStats <- dget("Estimates objects/OutsideSport_2017_26RG_StratifiedEstimatesStats.txt")

OutsideSport_2017_8RG_StratifiedEstimatesStats <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = Sport_Mixtures[6:7], 
                          catchvec = c(20870, 12114),  newname = "Stratified_OutsideSport_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)
dput(OutsideSport_2017_8RG_StratifiedEstimatesStats, "Estimates objects/OutsideSport_2017_8RG_StratifiedEstimatesStats.txt")
OutsideSport_2017_8RG_StratifiedEstimatesStats <- dget("Estimates objects/OutsideSport_2017_8RG_StratifiedEstimatesStats.txt")

OutsideSport_2017_18RG_StratifiedEstimatesStats <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = Sport_Mixtures[6:7], 
                          catchvec = c(20870, 12114),  newname = "Stratified_OutsideSport_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)
dput(OutsideSport_2017_18RG_StratifiedEstimatesStats, "Estimates objects/OutsideSport_2017_18RG_StratifiedEstimatesStats.txt")

OutsideSport_2017_4RG_StratifiedEstimatesStats <- 
  StratifiedEstimator.GCL(groupvec = GroupVec4, groupnames = GroupNames4, maindir = "BAYES/Output", 
                          mixvec = Sport_Mixtures[6:7], 
                          catchvec = c(20870, 12114),  newname = "Stratified_OutsideSport_2017_90percentCI_4RG", nchains = 5, xlxs = TRUE)
dput(OutsideSport_2017_4RG_StratifiedEstimatesStats, "Estimates objects/OutsideSport_2017_4RG_StratifiedEstimatesStats.txt")
OutsideSport_2017_4RG_StratifiedEstimatesStats <- dget("Estimates objects/OutsideSport_2017_4RG_StratifiedEstimatesStats.txt")


# All Year Sport
Sport_Mixtures

AllYearSport2017_8RG_StratifiedEstimatesStats <- 
  StratifiedEstimator.GCL(groupvec = GroupVec8, groupnames = GroupNames8, maindir = "BAYES/Output", 
                          mixvec = Sport_Mixtures[-c(1:2)], 
                          catchvec = c(13227, 2521, 3382, 20870, 12114),  newname = "Stratified_AllSport_2017_90percentCI_8RG", nchains = 5, xlxs = TRUE)
dput(AllYearSport2017_8RG_StratifiedEstimatesStats, "Estimates objects/AllYearSport2017_8RG_StratifiedEstimatesStats.txt")
AllYearSport2017_8RG_StratifiedEstimatesStats <- dget("Estimates objects/AllYearSport2017_8RG_StratifiedEstimatesStats.txt")


AllYearSport2017_18RG_StratifiedEstimatesStats <- 
  StratifiedEstimator.GCL(groupvec = GroupVec18, groupnames = GroupNames18, maindir = "BAYES/Output", 
                          mixvec = Sport_Mixtures[-c(1:2)], 
                          catchvec = c(13227, 2521, 3382, 20870, 12114),  newname = "Stratified_AllSport_2017_90percentCI_18RG", nchains = 5, xlxs = TRUE)
dput(AllYearSport2017_18RG_StratifiedEstimatesStats, "Estimates objects/AllYearSport2017_18RG_StratifiedEstimatesStats.txt")
AllYearSport2017_18RG_StratifiedEstimatesStats <- dget("Estimates objects/AllYearSport2017_18RG_StratifiedEstimatesStats.txt")


AllYearSport2017_33RG_StratifiedEstimatesStats <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir = "BAYES/Output", ext = "BOT"
                          mixvec = Sport_Mixtures[-c(1:2)], 
                          catchvec = c(13227, 2521, 3382, 20870, 12114),  newname = "Stratified_AllSport_2017_90percentCI_33RG", nchains = 5, xlxs = TRUE)
dput(AllYearSport2017_33RG_StratifiedEstimatesStats, "Estimates objects/AllYearSport2017_33RG_StratifiedEstimatesStats.txt")
AllYearSport2017_33RG_StratifiedEstimatesStats <- dget("Estimates objects/AllYearSport2017_33RG_StratifiedEstimatesStats.txt")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Retro Troll Stratified Estimates 26RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2017
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")


EWintTroll2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = EWint_Mixtures, catchvec = c(4989, 1599), nchains = 5, xlxs = FALSE)

LWintTroll2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = LWint_Mixtures, catchvec = c(22509, 14782), nchains = 5, xlxs = FALSE)

SpringTroll2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = c(SpringRet1_Mixtures, SpringRet2_Mixtures), catchvec = c(807, 2241, 1700, 283, 1471, 8507, 1819, 482), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2017_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", 
                          mixvec = SummerRet1_Mixtures, catchvec = c(10020, 40721, 4037, 9551), nchains = 5, xlxs = FALSE)

Troll2017_26RG_StratifiedEstimates <- list("EWintAllQuad_2017" = EWintTroll2017_26RG_StratifiedEstimates,
                                               "LWintAllQuad_2017" = LWintTroll2017_26RG_StratifiedEstimates,
                                               "SpringAllQuad_2017" = SpringTroll2017_26RG_StratifiedEstimates,
                                               "SumRet1AllQuad_2017" = SumRet1Troll2017_26RG_StratifiedEstimates)
dput(x = Troll2017_26RG_StratifiedEstimates, file = "Estimates objects/Troll2017_26RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2016
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK16")


EWintTroll2016_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_to26RG, groupnames = GroupNames26, maindir = "BAYES/Output/33RG/EWint_2016", 
                          mixvec = c("EWintNISISO_2016", "EWintNO_2016"), catchvec = c(4216, 25147), nchains = 5, xlxs = FALSE)

LWintTroll2016_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_to26RG, groupnames = GroupNames26, maindir = "BAYES/Output/33RG/LWint_2016", 
                          mixvec = c("LWintNISISO_2016", "LWintNO_2016"), catchvec = c(5248, 17680), nchains = 5, xlxs = FALSE)

SpringTroll2016_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_to26RG, groupnames = GroupNames26, maindir = "BAYES/Output/33RG/Spring_2016", 
                          mixvec = c("SpringNI_2016", "SpringNO_2016", "SpringSI_2016", "SpringSO_2016"), catchvec = c(9270, 17012, 15160, 1031), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2016_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_to26RG, groupnames = GroupNames26, maindir = "BAYES/Output/33RG/SumRet1_2016", 
                          mixvec = c("Summer1NI_2016", "Summer1NO_2016", "Summer1SI_2016", "Summer1SO_2016"), catchvec = c(3805, 80323, 3618, 18888), 
                          nchains = 5, xlxs = FALSE)

SumRet2Troll2016_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_to26RG, groupnames = GroupNames26, maindir = "BAYES/Output/33RG/SumRet2_2016", 
                          mixvec = c("Summer2NI_2016", "Summer2NO_2016", "Summer2SI_2016", "Summer2SO_2016"), catchvec = c(2147, 56208, 1774, 14111), 
                          nchains = 5, xlxs = FALSE)

Troll2016_26RG_StratifiedEstimates <- list("EWintAllQuad_2016" = EWintTroll2016_26RG_StratifiedEstimates,
                                           "LWintAllQuad_2016" = LWintTroll2016_26RG_StratifiedEstimates,
                                           "SpringAllQuad_2016" = SpringTroll2016_26RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2016" = SumRet1Troll2016_26RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2016" = SumRet2Troll2016_26RG_StratifiedEstimates)
dput(x = Troll2016_26RG_StratifiedEstimates, file = "Estimates objects/Troll2016_26RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2015
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK15")


EWintTroll2015_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/EWint_2015",
                          mixvec = c("EWintNISISO_2015", "EWintNO_2015"), catchvec = c(3297, 20841), nchains = 5, xlxs = FALSE)

LWintTroll2015_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/LWint_2015",
                          mixvec = c("LWintNISISO_2015", "LWintNO_2015"), catchvec = c(5417, 21113), nchains = 5, xlxs = FALSE)

SpringTroll2015_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/Spring_2015",
                          mixvec = c("SpringNI_2015", "SpringNO_2015", "SpringSI_2015", "SpringSO_2015"), catchvec = c(12780, 22464, 17769, 711), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2015_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/SumRet1_2015",
                          mixvec = c("SumRet1NISISO_2015", "SumRet1NO_2015"), catchvec = c(40157, 124454), nchains = 5, xlxs = FALSE)

Troll2015_26RG_StratifiedEstimates <- list("EWintAllQuad_2015" = EWintTroll2015_26RG_StratifiedEstimates,
                                           "LWintAllQuad_2015" = LWintTroll2015_26RG_StratifiedEstimates,
                                           "SpringAllQuad_2015" = SpringTroll2015_26RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2015" = SumRet1Troll2015_26RG_StratifiedEstimates)
dput(x = Troll2015_26RG_StratifiedEstimates, file = "Estimates objects/Troll2015_26RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2014
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK14")


EWintTroll2014_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = c(1, 1:26), groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2014",
                          mixvec = c("EWintNISISO.2014", "EWintNO.2014"), catchvec = c(3602, 10669), nchains = 5, xlxs = FALSE)

LWintTroll2014_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = c(1, 1:26), groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2014",
                          mixvec = c("LWintNISISO.2014", "LWintNO.2014"), catchvec = c(6974, 35289), nchains = 5, xlxs = FALSE)

SpringTroll2014_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = c(1, 1:26), groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2014",
                          mixvec = c("SpringNI.2014", "SpringNO.2014", "SpringSI.2014", "SpringSO.2014"), catchvec = c(7702, 22393, 11215, 1238),
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2014_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = c(1, 1:26), groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2014",
                          mixvec = c("SumRet1NISISO.2014", "SumRet1NO.2014"), catchvec = c(41323, 158108), nchains = 5, xlxs = FALSE)

SumRet2Troll2014_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = c(1, 1:26), groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2014",
                          mixvec = c("SumRet2NISISO.2014", "SumRet2NO.2014"), catchvec = c(24365, 31288), nchains = 5, xlxs = FALSE)


Troll2014_26RG_StratifiedEstimates <- list("EWintAllQuad_2014" = EWintTroll2014_26RG_StratifiedEstimates,
                                           "LWintAllQuad_2014" = LWintTroll2014_26RG_StratifiedEstimates,
                                           "SpringAllQuad_2014" = SpringTroll2014_26RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2014" = SumRet1Troll2014_26RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2014" = SumRet2Troll2014_26RG_StratifiedEstimates)
dput(x = Troll2014_26RG_StratifiedEstimates, file = "Estimates objects/Troll2014_26RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2013
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK13")


EWintTroll2013_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2013",
                          mixvec = c("EWintNISISO.2013", "EWintNO.2013"), catchvec = c(2569, 5619), nchains = 5, xlxs = FALSE)

LWintTroll2013_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2013",
                          mixvec = c("LWintNISISO.2013", "LWintNO.2013"), catchvec = c(7946, 10414), nchains = 5, xlxs = FALSE)

SpringTroll2013_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2013",
                          mixvec = c("SpringNI.2013", "SpringNO.2013", "SpringSI.2013"), catchvec = c(11073, 16502, 9733), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2013_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2013",
                          mixvec = c("SumRet1NISISO.2013", "SumRet1NO.2013"), catchvec = c(30361, 54289), nchains = 5, xlxs = FALSE)


Troll2013_26RG_StratifiedEstimates <- list("EWintAllQuad_2013" = EWintTroll2013_26RG_StratifiedEstimates,
                                           "LWintAllQuad_2013" = LWintTroll2013_26RG_StratifiedEstimates,
                                           "SpringAllQuad_2013" = SpringTroll2013_26RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2013" = SumRet1Troll2013_26RG_StratifiedEstimates)
dput(x = Troll2013_26RG_StratifiedEstimates, file = "Estimates objects/Troll2013_26RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2012
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK12")


EWintTroll2012_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2012",
                          mixvec = c("EWintNISISO.2012", "EWintNO.2012"), catchvec = c(4119, 6566), nchains = 5, xlxs = FALSE)

LWintTroll2012_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2012",
                          mixvec = c("LWintNISISO.2012", "LWintNO.2012"), catchvec = c(9005, 28212), nchains = 5, xlxs = FALSE)

SpringTroll2012_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2012",
                          mixvec = c("SpringNI.2012", "SpringNO.2012", "SpringSI.2012"), catchvec = c(6274, 11466, 7031), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2012_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2012",
                          mixvec = c("SumRet1NISISO.2012", "SumRet1NO.2012"), catchvec = c(27286, 34338), nchains = 5, xlxs = FALSE)

SumRet2Troll2012_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Output/AllYearTroll_2012",
                          mixvec = c("SumRet2NISISO.2012", "SumRet2NO.2012"), catchvec = c(20056, 53914), nchains = 5, xlxs = FALSE)


Troll2012_26RG_StratifiedEstimates <- list("EWintAllQuad_2012" = EWintTroll2012_26RG_StratifiedEstimates,
                                           "LWintAllQuad_2012" = LWintTroll2012_26RG_StratifiedEstimates,
                                           "SpringAllQuad_2012" = SpringTroll2012_26RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2012" = SumRet1Troll2012_26RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2012" = SumRet2Troll2012_26RG_StratifiedEstimates)
dput(x = Troll2012_26RG_StratifiedEstimates, file = "Estimates objects/Troll2012_26RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2011
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK11")


EWintTroll2011_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2011",
                          mixvec = c("EWintNISISO.2011", "EWintNO.2011"), catchvec = c(4890, 7977), nchains = 5, xlxs = FALSE)

LWintTroll2011_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2011",
                          mixvec = c("LWintNISISO.2011", "LWintNO.2011"), catchvec = c(9385, 28574), nchains = 5, xlxs = FALSE)

SpringTroll2011_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2011",
                          mixvec = c("SpringNI.2011", "SpringNO.2011", "SpringSI.2011"), catchvec = c(8859, 17531, 12546), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2011_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2011",
                          mixvec = c("SumRet1NISISO.2011", "SumRet1NO.2011"), catchvec = c(40714, 80202), nchains = 5, xlxs = FALSE)

SumRet2Troll2011_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2011",
                          mixvec = c("SumRet2NISISO.2011", "SumRet2NO.2011"), catchvec = c(13364, 16372), nchains = 5, xlxs = FALSE)


Troll2011_26RG_StratifiedEstimates <- list("EWintAllQuad_2011" = EWintTroll2011_26RG_StratifiedEstimates,
                                           "LWintAllQuad_2011" = LWintTroll2011_26RG_StratifiedEstimates,
                                           "SpringAllQuad_2011" = SpringTroll2011_26RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2011" = SumRet1Troll2011_26RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2011" = SumRet2Troll2011_26RG_StratifiedEstimates)
dput(x = Troll2011_26RG_StratifiedEstimates, file = "Estimates objects/Troll2011_26RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2010
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK10")


EWintTroll2010_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2010",
                          mixvec = c("EarlyWinterNISISO.2010", "EarlyWinterNO.2010"), catchvec = c(4255, 4460), nchains = 5, xlxs = FALSE)

LWintTroll2010_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2010",
                          mixvec = c("LateWinterNISISO.2010", "LateWinterNO.2010"), catchvec = c(6623, 27198), nchains = 5, xlxs = FALSE)

SpringTroll2010_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2010",
                          mixvec = c("SpringNI.2010", "SpringNO.2010", "SpringSI.2010"), catchvec = c(7517, 10258, 10789), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2010_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2010",
                          mixvec = c("SummerR1NISISO.2010", "SummerR1NO.2010"), catchvec = c(18643, 56069), nchains = 5, xlxs = FALSE)

SumRet2Troll2010_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2010",
                          mixvec = c("SummerR2NISISO.2010", "SummerR2NO.2010"), catchvec = c(22025, 26430), nchains = 5, xlxs = FALSE)


Troll2010_26RG_StratifiedEstimates <- list("EWintAllQuad_2010" = EWintTroll2010_26RG_StratifiedEstimates,
                                           "LWintAllQuad_2010" = LWintTroll2010_26RG_StratifiedEstimates,
                                           "SpringAllQuad_2010" = SpringTroll2010_26RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2010" = SumRet1Troll2010_26RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2010" = SumRet2Troll2010_26RG_StratifiedEstimates)
dput(x = Troll2010_26RG_StratifiedEstimates, file = "Estimates objects/Troll2010_26RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2009
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK09")


EWintTroll2009_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2009",
                          mixvec = c("EarlyWinterNISISO", "EarlyWinterNO"), catchvec = c(2711, 2800), nchains = 5, xlxs = FALSE)

LWintTroll2009_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2009",
                          mixvec = c("LateWinterNISISO", "LateWinterNO"), catchvec = c(3794, 15584), nchains = 5, xlxs = FALSE)

SpringTroll2009_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2009",
                          mixvec = c("SpringNI.2009", "SpringNO.2009", "SpringSI.2009"), catchvec = c(7790, 16629, 8162), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2009_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2009",
                          mixvec = c("SumRet1NISI_2009", "SumRet1NOSO_2009"), catchvec = c(4112, 80463), nchains = 5, xlxs = FALSE)

SumRet2Troll2009_26RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir="BAYES/Troll/Output/AllYearTroll_2009",
                          mixvec = c("SumRet2NISI_2009", "SumRet2NOSO_2009"), catchvec = c(1796, 31216), nchains = 5, xlxs = FALSE)


Troll2009_26RG_StratifiedEstimates <- list("EWintAllQuad_2009" = EWintTroll2009_26RG_StratifiedEstimates,
                                           "LWintAllQuad_2009" = LWintTroll2009_26RG_StratifiedEstimates,
                                           "SpringAllQuad_2009" = SpringTroll2009_26RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2009" = SumRet1Troll2009_26RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2009" = SumRet2Troll2009_26RG_StratifiedEstimates)
dput(x = Troll2009_26RG_StratifiedEstimates, file = "Estimates objects/Troll2009_26RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Double check 8RG vs. 26RG

# setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
# Troll2017_26RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2017_26RG_StratifiedEstimates.txt")
# Troll2017_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2017_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2017_26RG_StratifiedEstimates[[2]][which(GroupVec8 == which(GroupNames8 == RG)), "mean"])} ),
          Troll2017_8RG_StratifiedEstimates[[2]][, "mean"])

# setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK16")
# Troll2016_26RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2016_26RG_StratifiedEstimates.txt")
# Troll2016_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2016_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2016_26RG_StratifiedEstimates[[2]][which(GroupVec8 == which(GroupNames8 == RG)), "mean"])} ),
          Troll2016_8RG_StratifiedEstimates[[2]][, "mean"])

# setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK15")
# Troll2015_26RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2015_26RG_StratifiedEstimates.txt")
# Troll2015_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2015_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2015_26RG_StratifiedEstimates[[2]][which(GroupVec8 == which(GroupNames8 == RG)), "mean"])} ),
          Troll2015_8RG_StratifiedEstimates[[2]][, "mean"])

# setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK14")
# Troll2014_26RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2014_26RG_StratifiedEstimates.txt")
# Troll2014_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2014_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2014_26RG_StratifiedEstimates[[2]][which(GroupVec8 == which(GroupNames8 == RG)), "mean"])} ),
          Troll2014_8RG_StratifiedEstimates[[2]][, "mean"])

# setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK13")
# Troll2013_26RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2013_26RG_StratifiedEstimates.txt")
# Troll2013_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2013_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2013_26RG_StratifiedEstimates[[2]][which(GroupVec8 == which(GroupNames8 == RG)), "mean"])} ),
          Troll2013_8RG_StratifiedEstimates[[2]][, "mean"])

# setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK12")
# Troll2012_26RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2012_26RG_StratifiedEstimates.txt")
# Troll2012_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2012_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2012_26RG_StratifiedEstimates[[2]][which(GroupVec8 == which(GroupNames8 == RG)), "mean"])} ),
          Troll2012_8RG_StratifiedEstimates[[2]][, "mean"])

# setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK11")
# Troll2011_26RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2011_26RG_StratifiedEstimates.txt")
# Troll2011_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2011_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2011_26RG_StratifiedEstimates[[2]][which(GroupVec8 == which(GroupNames8 == RG)), "mean"])} ),
          Troll2011_8RG_StratifiedEstimates[[2]][, "mean"])

# setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK10")
# Troll2010_26RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2010_26RG_StratifiedEstimates.txt")
# Troll2010_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2010_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2010_26RG_StratifiedEstimates[[2]][which(GroupVec8 == which(GroupNames8 == RG)), "mean"])} ),
          Troll2010_8RG_StratifiedEstimates[[2]][, "mean"])

# setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK09")
# Troll2009_26RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2009_26RG_StratifiedEstimates.txt")
# Troll2009_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2009_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2009_26RG_StratifiedEstimates[[2]][which(GroupVec8 == which(GroupNames8 == RG)), "mean"])} ),
          Troll2009_8RG_StratifiedEstimates[[2]][, "mean"])

i = 3
sapply(2009:2017, function(yr) {
  all.equal(sapply(GroupNames8, function(RG) {sum(get(paste0("Troll", yr, "_26RG_StratifiedEstimates"))[[i]][which(GroupVec8 == which(GroupNames8 == RG)), "mean"])} ),
            get(paste0("Troll", yr, "_8RG_StratifiedEstimates"))[[i]][, "mean"])
} )


Troll2009_2017_26RG_StratifiedEstimates <- 
  c(Troll2009_26RG_StratifiedEstimates,
  Troll2010_26RG_StratifiedEstimates,
  Troll2011_26RG_StratifiedEstimates,
  Troll2012_26RG_StratifiedEstimates,
  Troll2013_26RG_StratifiedEstimates,
  Troll2014_26RG_StratifiedEstimates,
  Troll2015_26RG_StratifiedEstimates,
  Troll2016_26RG_StratifiedEstimates,
  Troll2017_26RG_StratifiedEstimates)
dput(x = Troll2009_2017_26RG_StratifiedEstimates, file = "Estimates objects/Troll2009_2017_26RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Retro Troll Stratified Estimates 33RG ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Finest scale 33RG from GAPS 3.0
# This information is found in "V:\Analysis\1_SEAK\Chinook\Baseline\GAPS3.0\ChinookReportingGroups_CTC_NewModel_Jan2018.xlsx"; tab "GroupbyPop"; column S

Groups33RG_357 <- readClipboard()
GroupNames33
GroupVec33RG_357 <- as.numeric(factor(x = Groups33RG_357, levels = unique(GroupNames33)))
dput(x = GroupVec33RG_357, file = "Objects/GroupVec33RG_357.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2017
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")


EWintTroll2017_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir = "BAYES/Output", ext = "BOT",
                          mixvec = EWint_Mixtures, catchvec = c(4989, 1599), nchains = 5, xlxs = FALSE)

LWintTroll2017_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir = "BAYES/Output", ext = "BOT",
                          mixvec = LWint_Mixtures, catchvec = c(22509, 14782), nchains = 5, xlxs = FALSE)

SpringTroll2017_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir = "BAYES/Output", ext = "BOT",
                          mixvec = c(SpringRet1_Mixtures, SpringRet2_Mixtures), catchvec = c(807, 2241, 1700, 283, 1471, 8507, 1819, 482), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2017_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir = "BAYES/Output", ext = "BOT",
                          mixvec = SummerRet1_Mixtures, catchvec = c(10020, 40721, 4037, 9551), nchains = 5, xlxs = FALSE)

Troll2017_33RG_StratifiedEstimates <- list("EWintAllQuad_2017" = EWintTroll2017_33RG_StratifiedEstimates,
                                           "LWintAllQuad_2017" = LWintTroll2017_33RG_StratifiedEstimates,
                                           "SpringAllQuad_2017" = SpringTroll2017_33RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2017" = SumRet1Troll2017_33RG_StratifiedEstimates)
dput(x = Troll2017_33RG_StratifiedEstimates, file = "Estimates objects/Troll2017_33RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2016
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK16")


EWintTroll2016_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:33, groupnames = GroupNames33, maindir = "BAYES/Output/33RG/EWint_2016", 
                          mixvec = c("EWintNISISO_2016", "EWintNO_2016"), catchvec = c(4216, 25147), nchains = 5, xlxs = FALSE)

LWintTroll2016_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:33, groupnames = GroupNames33, maindir = "BAYES/Output/33RG/LWint_2016", 
                          mixvec = c("LWintNISISO_2016", "LWintNO_2016"), catchvec = c(5248, 17680), nchains = 5, xlxs = FALSE)

SpringTroll2016_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:33, groupnames = GroupNames33, maindir = "BAYES/Output/33RG/Spring_2016", 
                          mixvec = c("SpringNI_2016", "SpringNO_2016", "SpringSI_2016", "SpringSO_2016"), catchvec = c(9270, 17012, 15160, 1031), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2016_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:33, groupnames = GroupNames33, maindir = "BAYES/Output/33RG/SumRet1_2016", 
                          mixvec = c("Summer1NI_2016", "Summer1NO_2016", "Summer1SI_2016", "Summer1SO_2016"), catchvec = c(3805, 80323, 3618, 18888), 
                          nchains = 5, xlxs = FALSE)

SumRet2Troll2016_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = 1:33, groupnames = GroupNames33, maindir = "BAYES/Output/33RG/SumRet2_2016", 
                          mixvec = c("Summer2NI_2016", "Summer2NO_2016", "Summer2SI_2016", "Summer2SO_2016"), catchvec = c(2147, 56208, 1774, 14111), 
                          nchains = 5, xlxs = FALSE)

Troll2016_33RG_StratifiedEstimates <- list("EWintAllQuad_2016" = EWintTroll2016_33RG_StratifiedEstimates,
                                           "LWintAllQuad_2016" = LWintTroll2016_33RG_StratifiedEstimates,
                                           "SpringAllQuad_2016" = SpringTroll2016_33RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2016" = SumRet1Troll2016_33RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2016" = SumRet2Troll2016_33RG_StratifiedEstimates)
dput(x = Troll2016_33RG_StratifiedEstimates, file = "Estimates objects/Troll2016_33RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2015
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK15")


EWintTroll2015_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/EWint_2015", ext = "BOT",
                          mixvec = c("EWintNISISO_2015", "EWintNO_2015"), catchvec = c(3297, 20841), nchains = 5, xlxs = FALSE)

LWintTroll2015_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/LWint_2015", ext = "BOT",
                          mixvec = c("LWintNISISO_2015", "LWintNO_2015"), catchvec = c(5417, 21113), nchains = 5, xlxs = FALSE)

SpringTroll2015_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/Spring_2015", ext = "BOT",
                          mixvec = c("SpringNI_2015", "SpringNO_2015", "SpringSI_2015", "SpringSO_2015"), catchvec = c(12780, 22464, 17769, 711), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2015_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/SumRet1_2015", ext = "BOT",
                          mixvec = c("SumRet1NISISO_2015", "SumRet1NO_2015"), catchvec = c(40157, 124454), nchains = 5, xlxs = FALSE)

Troll2015_33RG_StratifiedEstimates <- list("EWintAllQuad_2015" = EWintTroll2015_33RG_StratifiedEstimates,
                                           "LWintAllQuad_2015" = LWintTroll2015_33RG_StratifiedEstimates,
                                           "SpringAllQuad_2015" = SpringTroll2015_33RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2015" = SumRet1Troll2015_33RG_StratifiedEstimates)
dput(x = Troll2015_33RG_StratifiedEstimates, file = "Estimates objects/Troll2015_33RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2014
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK14")


EWintTroll2014_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2014", ext = "BOT",
                          mixvec = c("EWintNISISO.2014", "EWintNO.2014"), catchvec = c(3602, 10669), nchains = 5, xlxs = FALSE)

LWintTroll2014_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2014", ext = "BOT",
                          mixvec = c("LWintNISISO.2014", "LWintNO.2014"), catchvec = c(6974, 35289), nchains = 5, xlxs = FALSE)

SpringTroll2014_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2014", ext = "BOT",
                          mixvec = c("SpringNI.2014", "SpringNO.2014", "SpringSI.2014", "SpringSO.2014"), catchvec = c(7702, 22393, 11215, 1238),
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2014_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2014", ext = "BOT",
                          mixvec = c("SumRet1NISISO.2014", "SumRet1NO.2014"), catchvec = c(41323, 158108), nchains = 5, xlxs = FALSE)

SumRet2Troll2014_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2014", ext = "BOT",
                          mixvec = c("SumRet2NISISO.2014", "SumRet2NO.2014"), catchvec = c(24365, 31288), nchains = 5, xlxs = FALSE)


Troll2014_33RG_StratifiedEstimates <- list("EWintAllQuad_2014" = EWintTroll2014_33RG_StratifiedEstimates,
                                           "LWintAllQuad_2014" = LWintTroll2014_33RG_StratifiedEstimates,
                                           "SpringAllQuad_2014" = SpringTroll2014_33RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2014" = SumRet1Troll2014_33RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2014" = SumRet2Troll2014_33RG_StratifiedEstimates)
dput(x = Troll2014_33RG_StratifiedEstimates, file = "Estimates objects/Troll2014_33RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2013
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK13")


EWintTroll2013_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2013", ext = "BOT",
                          mixvec = c("EWintNISISO.2013", "EWintNO.2013"), catchvec = c(2569, 5619), nchains = 5, xlxs = FALSE)

LWintTroll2013_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2013", ext = "BOT",
                          mixvec = c("LWintNISISO.2013", "LWintNO.2013"), catchvec = c(7946, 10414), nchains = 5, xlxs = FALSE)

SpringTroll2013_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2013", ext = "BOT",
                          mixvec = c("SpringNI.2013", "SpringNO.2013", "SpringSI.2013"), catchvec = c(11073, 16502, 9733), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2013_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2013", ext = "BOT",
                          mixvec = c("SumRet1NISISO.2013", "SumRet1NO.2013"), catchvec = c(30361, 54289), nchains = 5, xlxs = FALSE)


Troll2013_33RG_StratifiedEstimates <- list("EWintAllQuad_2013" = EWintTroll2013_33RG_StratifiedEstimates,
                                           "LWintAllQuad_2013" = LWintTroll2013_33RG_StratifiedEstimates,
                                           "SpringAllQuad_2013" = SpringTroll2013_33RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2013" = SumRet1Troll2013_33RG_StratifiedEstimates)
dput(x = Troll2013_33RG_StratifiedEstimates, file = "Estimates objects/Troll2013_33RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2012
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK12")


EWintTroll2012_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2012", ext = "BOT",
                          mixvec = c("EWintNISISO.2012", "EWintNO.2012"), catchvec = c(4119, 6566), nchains = 5, xlxs = FALSE)

LWintTroll2012_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2012", ext = "BOT",
                          mixvec = c("LWintNISISO.2012", "LWintNO.2012"), catchvec = c(9005, 28212), nchains = 5, xlxs = FALSE)

SpringTroll2012_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2012", ext = "BOT",
                          mixvec = c("SpringNI.2012", "SpringNO.2012", "SpringSI.2012"), catchvec = c(6274, 11466, 7031), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2012_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2012", ext = "BOT",
                          mixvec = c("SumRet1NISISO.2012", "SumRet1NO.2012"), catchvec = c(27286, 34338), nchains = 5, xlxs = FALSE)

SumRet2Troll2012_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Output/AllYearTroll_2012", ext = "BOT",
                          mixvec = c("SumRet2NISISO.2012", "SumRet2NO.2012"), catchvec = c(20056, 53914), nchains = 5, xlxs = FALSE)


Troll2012_33RG_StratifiedEstimates <- list("EWintAllQuad_2012" = EWintTroll2012_33RG_StratifiedEstimates,
                                           "LWintAllQuad_2012" = LWintTroll2012_33RG_StratifiedEstimates,
                                           "SpringAllQuad_2012" = SpringTroll2012_33RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2012" = SumRet1Troll2012_33RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2012" = SumRet2Troll2012_33RG_StratifiedEstimates)
dput(x = Troll2012_33RG_StratifiedEstimates, file = "Estimates objects/Troll2012_33RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2011
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK11")


EWintTroll2011_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2011", ext = "BOT",
                          mixvec = c("EWintNISISO.2011", "EWintNO.2011"), catchvec = c(4890, 7977), nchains = 5, xlxs = FALSE)

LWintTroll2011_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2011", ext = "BOT",
                          mixvec = c("LWintNISISO.2011", "LWintNO.2011"), catchvec = c(9385, 28574), nchains = 5, xlxs = FALSE)

SpringTroll2011_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2011", ext = "BOT",
                          mixvec = c("SpringNI.2011", "SpringNO.2011", "SpringSI.2011"), catchvec = c(8859, 17531, 12546), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2011_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2011", ext = "BOT",
                          mixvec = c("SumRet1NISISO.2011", "SumRet1NO.2011"), catchvec = c(40714, 80202), nchains = 5, xlxs = FALSE)

SumRet2Troll2011_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2011", ext = "BOT",
                          mixvec = c("SumRet2NISISO.2011", "SumRet2NO.2011"), catchvec = c(13364, 16372), nchains = 5, xlxs = FALSE)


Troll2011_33RG_StratifiedEstimates <- list("EWintAllQuad_2011" = EWintTroll2011_33RG_StratifiedEstimates,
                                           "LWintAllQuad_2011" = LWintTroll2011_33RG_StratifiedEstimates,
                                           "SpringAllQuad_2011" = SpringTroll2011_33RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2011" = SumRet1Troll2011_33RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2011" = SumRet2Troll2011_33RG_StratifiedEstimates)
dput(x = Troll2011_33RG_StratifiedEstimates, file = "Estimates objects/Troll2011_33RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2010
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK10")


EWintTroll2010_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2010", ext = "BOT",
                          mixvec = c("EarlyWinterNISISO.2010", "EarlyWinterNO.2010"), catchvec = c(4255, 4460), nchains = 5, xlxs = FALSE)

LWintTroll2010_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2010", ext = "BOT",
                          mixvec = c("LateWinterNISISO.2010", "LateWinterNO.2010"), catchvec = c(6623, 27198), nchains = 5, xlxs = FALSE)

SpringTroll2010_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2010", ext = "BOT",
                          mixvec = c("SpringNI.2010", "SpringNO.2010", "SpringSI.2010"), catchvec = c(7517, 10258, 10789), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2010_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2010", ext = "BOT",
                          mixvec = c("SummerR1NISISO.2010", "SummerR1NO.2010"), catchvec = c(18643, 56069), nchains = 5, xlxs = FALSE)

SumRet2Troll2010_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2010", ext = "BOT",
                          mixvec = c("SummerR2NISISO.2010", "SummerR2NO.2010"), catchvec = c(22025, 26430), nchains = 5, xlxs = FALSE)


Troll2010_33RG_StratifiedEstimates <- list("EWintAllQuad_2010" = EWintTroll2010_33RG_StratifiedEstimates,
                                           "LWintAllQuad_2010" = LWintTroll2010_33RG_StratifiedEstimates,
                                           "SpringAllQuad_2010" = SpringTroll2010_33RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2010" = SumRet1Troll2010_33RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2010" = SumRet2Troll2010_33RG_StratifiedEstimates)
dput(x = Troll2010_33RG_StratifiedEstimates, file = "Estimates objects/Troll2010_33RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
### 2009
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK09")


EWintTroll2009_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2009", ext = "BOT",
                          mixvec = c("EarlyWinterNISISO", "EarlyWinterNO"), catchvec = c(2711, 2800), nchains = 5, xlxs = FALSE)

LWintTroll2009_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2009", ext = "BOT",
                          mixvec = c("LateWinterNISISO", "LateWinterNO"), catchvec = c(3794, 15584), nchains = 5, xlxs = FALSE)

SpringTroll2009_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2009", ext = "BOT",
                          mixvec = c("SpringNI.2009", "SpringNO.2009", "SpringSI.2009"), catchvec = c(7790, 16629, 8162), 
                          nchains = 5, xlxs = FALSE)

SumRet1Troll2009_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2009", ext = "BOT",
                          mixvec = c("SumRet1NISI_2009", "SumRet1NOSO_2009"), catchvec = c(4112, 80463), nchains = 5, xlxs = FALSE)

SumRet2Troll2009_33RG_StratifiedEstimates <- 
  StratifiedEstimator.GCL(groupvec = GroupVec33RG_357, groupnames = GroupNames33, maindir="BAYES/Troll/Output/AllYearTroll_2009", ext = "BOT",
                          mixvec = c("SumRet2NISI_2009", "SumRet2NOSO_2009"), catchvec = c(1796, 31216), nchains = 5, xlxs = FALSE)


Troll2009_33RG_StratifiedEstimates <- list("EWintAllQuad_2009" = EWintTroll2009_33RG_StratifiedEstimates,
                                           "LWintAllQuad_2009" = LWintTroll2009_33RG_StratifiedEstimates,
                                           "SpringAllQuad_2009" = SpringTroll2009_33RG_StratifiedEstimates,
                                           "SumRet1AllQuad_2009" = SumRet1Troll2009_33RG_StratifiedEstimates,
                                           "SumRet2AllQuad_2009" = SumRet2Troll2009_33RG_StratifiedEstimates)
dput(x = Troll2009_33RG_StratifiedEstimates, file = "Estimates objects/Troll2009_33RG_StratifiedEstimates.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Double check 8RG vs. 26RG

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
Troll2017_33RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2017_33RG_StratifiedEstimates.txt")
Troll2017_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2017_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2017_33RG_StratifiedEstimates[[2]][which(GroupVec33RG_to8RG == which(GroupNames8 == RG)), "mean"])} ),
          Troll2017_8RG_StratifiedEstimates[[2]][, "mean"])

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK16")
Troll2016_33RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2016_33RG_StratifiedEstimates.txt")
Troll2016_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2016_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2016_33RG_StratifiedEstimates[[2]][which(GroupVec33RG_to8RG == which(GroupNames8 == RG)), "mean"])} ),
          Troll2016_8RG_StratifiedEstimates[[2]][, "mean"])

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK15")
Troll2015_33RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2015_33RG_StratifiedEstimates.txt")
Troll2015_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2015_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2015_33RG_StratifiedEstimates[[2]][which(GroupVec33RG_to8RG == which(GroupNames8 == RG)), "mean"])} ),
          Troll2015_8RG_StratifiedEstimates[[2]][, "mean"])

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK14")
Troll2014_33RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2014_33RG_StratifiedEstimates.txt")
Troll2014_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2014_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2014_33RG_StratifiedEstimates[[2]][which(GroupVec33RG_to8RG == which(GroupNames8 == RG)), "mean"])} ),
          Troll2014_8RG_StratifiedEstimates[[2]][, "mean"])

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK13")
Troll2013_33RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2013_33RG_StratifiedEstimates.txt")
Troll2013_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2013_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2013_33RG_StratifiedEstimates[[2]][which(GroupVec33RG_to8RG == which(GroupNames8 == RG)), "mean"])} ),
          Troll2013_8RG_StratifiedEstimates[[2]][, "mean"])

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK12")
Troll2012_33RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2012_33RG_StratifiedEstimates.txt")
Troll2012_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2012_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2012_33RG_StratifiedEstimates[[2]][which(GroupVec33RG_to8RG == which(GroupNames8 == RG)), "mean"])} ),
          Troll2012_8RG_StratifiedEstimates[[2]][, "mean"])

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK11")
Troll2011_33RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2011_33RG_StratifiedEstimates.txt")
Troll2011_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2011_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2011_33RG_StratifiedEstimates[[2]][which(GroupVec33RG_to8RG == which(GroupNames8 == RG)), "mean"])} ),
          Troll2011_8RG_StratifiedEstimates[[2]][, "mean"])

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK10")
Troll2010_33RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2010_33RG_StratifiedEstimates.txt")
Troll2010_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2010_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2010_33RG_StratifiedEstimates[[2]][which(GroupVec33RG_to8RG == which(GroupNames8 == RG)), "mean"])} ),
          Troll2010_8RG_StratifiedEstimates[[2]][, "mean"])

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK09")
Troll2009_33RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2009_33RG_StratifiedEstimates.txt")
Troll2009_8RG_StratifiedEstimates <- dget(file = "Estimates objects/Troll2009_8RG_StratifiedEstimatesStats.txt")
all.equal(sapply(GroupNames8, function(RG) {sum(Troll2009_33RG_StratifiedEstimates[[2]][which(GroupVec33RG_to8RG == which(GroupNames8 == RG)), "mean"])} ),
          Troll2009_8RG_StratifiedEstimates[[2]][, "mean"])

i = 1
sapply(2009:2017, function(yr) {
  all.equal(sapply(GroupNames8, function(RG) {sum(get(paste0("Troll", yr, "_33RG_StratifiedEstimates"))[[i]][which(GroupVec8 == which(GroupNames8 == RG)), "mean"])} ),
            get(paste0("Troll", yr, "_8RG_StratifiedEstimates"))[[i]][, "mean"])
} )


Troll2009_2017_33RG_StratifiedEstimates <- 
  c(Troll2009_33RG_StratifiedEstimates,
    Troll2010_33RG_StratifiedEstimates,
    Troll2011_33RG_StratifiedEstimates,
    Troll2012_33RG_StratifiedEstimates,
    Troll2013_33RG_StratifiedEstimates,
    Troll2014_33RG_StratifiedEstimates,
    Troll2015_33RG_StratifiedEstimates,
    Troll2016_33RG_StratifiedEstimates,
    Troll2017_33RG_StratifiedEstimates)
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
dput(x = Troll2009_2017_33RG_StratifiedEstimates, file = "Estimates objects/Troll2009_2017_33RG_StratifiedEstimates.txt")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create 2017 Troll HeatMaps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# dir.create("Figures")

EstimatesStats <- c(dget("Estimates objects/Troll2017_8RG_StratifiedEstimatesStats.txt"),
                    list("SumRet1AllQuad_2017" = dget("Estimates objects/SummerRet1_2017_8RG_StratifiedEstimates.txt")),
                    dget(file = "Estimates objects/EWint_2017_8RG_EstimatesStats.txt")["EWintNO_2017"],
                    dget(file = "Estimates objects/LWint_2017_8RG_EstimatesStats.txt")["LWintNO_2017"],
                    dget(file = "Estimates objects/Troll2017_8RG_EstimatesStats.txt")["SpringNO_2017"],
                    dget(file = "Estimates objects/SummerRet1_2017_8RG_EstimatesStats.txt")["SummerRet1NO_2017"])
str(EstimatesStats)

# Create layout
layoutmat <- matrix(c(9,1,2,11,
                      9,3,4,11,
                      9,5,6,11,
                      9,7,8,11,
                      12,10,10,13), ncol=4,nrow=5,byrow=T)
SEAKTrollLayout <- layout(layoutmat,widths=c(0.25,1,1,0.25),heights=c(1,1,1,1,0.25))
layout.show(SEAKTrollLayout)

# Set color ramp
library('lattice')
WhiteRedColPalette <- colorRampPalette(colors=c("white","red"))
WhiteRedcol <- level.colors(x=seq(from=0,to=1,by=0.01), at = seq(from=0,to=1,by=0.01), col.regions = WhiteRedColPalette(100))

# Mixture names
mixnames <- names(EstimatesStats)[c(5, 1, 6, 2, 7, 3, 8, 4)]

# Create list object with by RG stock comps
HeatmapEstimates <- sapply(GroupNames8, function(RG) {
  matrix(data = sapply(mixnames, function(mix) {EstimatesStats[[mix]][RG, "mean"] }),
         nrow = 2, ncol = 4, dimnames = list(c("NO", "AllQuad"), c("EWint", "LWint", "Spring", "SumRet1"))
  )
}, simplify = FALSE)
zmax <- max(sapply(HeatmapEstimates, max))
zmax <- 0.91  # Standardize with Sport Heatmap

Testing <- matrix(c(seq(from = 0, to = zmax, length.out = 102), seq(from = 0, to = zmax, length.out = 102)), nrow = 2, ncol = 102, byrow = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot: Can't do a nested layout, writing out as pdf then pasting in other pdf

GroupNames8Pub2 <- c(GroupNames8Pub[1:2], "West Vancouver",GroupNames8Pub[4:8])
names(GroupNames8Pub2) <- GroupNames8

# pdf("Figures/2016TrollByFisheryQuadrant.pdf", family = "Times", width = 6.5, height = 6.5, title = "2016 Troll By Fishery and Quadrant")
png("Figures/2017TrollByFisheryQuadrant.png", family = "Times", width = 6.5, height = 6.5, units = "in", res = 300)
# x11(width = 6.5, height = 6.5)
par(xaxt = "n", yaxt = "n", omi = rep(0.1, 4), mar = rep(0.1, 4), family = 'serif')
layout(layoutmat,widths=c(0.3,1,1,0.25),heights=c(1,1,1,1,0.4))

## Loop through Reporting Group plots
sapply(GroupNames8, function(RG) {
  image(t(HeatmapEstimates[[RG]])[, c("AllQuad", "NO")], zlim = c(0, zmax), col = WhiteRedcol, xlab = "", ylab = "", breaks = seq(from = 0, to = zmax, length.out = 102), useRaster = TRUE)
  abline(h = 0.5, lwd = 2, col = 'grey')
  abline(v = c(0.175, 0.5, 0.83), lwd= 2 , col = 'grey')
  abline(h = c(-0.5, 1.5), v = c(-0.17, 1.17),lwd = 5, col = 'black')
  # abline(v = c(0.135, 0.38, 0.63, 0.875), lwd= 2 , col = 'grey')
  # abline(h = c(-0.5, 1.5), v = c(-0.125, 1.125),lwd = 5, col = 'black')
  text(labels = substitute(expr = italic(x), env = list(x = GroupNames8Pub2[RG])), cex = 2, adj = c(0, 0.5), x = -0.1, y = 1)
})

## Plot 10 - Y-axis label
plot.new()
text(labels = "Quadrant", cex = 3, srt = 90, x = 0.3, y = 0.5, adj = c(0.5, 0))
text(labels = "NO", cex = 2, x = 0.99, y = c(0.97, 0.7, 0.43, 0.16), adj = c(1, 0.5))
text(labels = "All", cex = 2, x = 0.99, y = c(0.97, 0.7, 0.43, 0.16) - 0.135, adj = c(1, 0.5))

## Plot 11 - X-axis label
plot.new()
text(labels = "Fishery", cex = 3, adj = c(0.5, 0.5), x = 0.5, y = 0.35)
text(labels = "EW", cex = 2, adj = c(0.5, 0.5), x = c(0.04, 0.57), y = 0.8)
text(labels = "LW", cex = 2, adj = c(0.5, 0.5), x = c(0.04 + 0.125, 0.57 + 0.13), y = 0.8)
text(labels = "SP", cex = 2, adj = c(0.5, 0.5), x = c(0.04 + 0.255, 0.57 + 0.27), y = 0.8)
text(labels = "SU1", cex = 2, adj = c(0.5, 0.5), x = c(0.04 + 0.385, 0.57 + 0.4), y = 0.8)
# text(labels = "Fishery", cex = 3, adj = c(0.5, 0.5), x = 0.5, y = 0.35)
# text(labels = "EW", cex = 2, adj = c(0.5, 0.5), x = c(0.02, 0.56), y = 0.8)
# text(labels = "LW", cex = 2, adj = c(0.5, 0.5), x = c(0.02 + 0.115, 0.56 + 0.115), y = 0.8)
# text(labels = "SP", cex = 2, adj = c(0.5, 0.5), x = c(0.02 + 0.22, 0.56 + 0.22), y = 0.8)
# text(labels = "SU1", cex = 2, adj = c(0.5, 0.5), x = c(0.02 + 0.33, 0.56 + 0.33), y = 0.8)
# text(labels = "SU2", cex = 2, adj = c(0.5, 0.5), x = c(0.02 + 0.43, 0.56 + 0.43), y = 0.8)

## Plot 13 - Legend
image(Testing, col = WhiteRedcol, xlab = "", ylab = "", breaks = seq(from = 0, to = zmax, length.out = 102))
text(labels = "0%", cex = 2.8, adj = c(0.5, 0.5), x = 0.5, y = 0.03)
text(labels = paste0(round(zmax * 100), "%"), cex = 2.8, adj = c(0.5, 0.5), x = 0.5, y = 0.98)  # formerly 50%
abline(h = c(-0.005,  1.005),  v  =  c(-0.5,  1.5), lwd = 5, col = 'black')
dev.off()
dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Create 2017 Sport HeatMaps ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

EstimatesStats <- dget(file = "Estimates objects/Sport_2017_8RG_EstimatesStats.txt")

# Create layout
layoutmat <- matrix(c(9,1,2,11,
                      9,3,4,11,
                      9,5,6,11,
                      9,7,8,11,
                      12,10,10,13), ncol=4,nrow=5,byrow=T)
SEAKTrollLayout <- layout(layoutmat,widths=c(0.25,1,1,0.25),heights=c(1,1,1,1,0.25))
layout.show(SEAKTrollLayout)

# Set color ramp
library('lattice')
WhiteRedColPalette <- colorRampPalette(colors=c("white","red"))
WhiteRedcol <- level.colors(x=seq(from=0,to=1,by=0.01), at = seq(from=0,to=1,by=0.01), col.regions = WhiteRedColPalette(100))

# Mixture names
mixnames <- c(rep(x = names(EstimatesStats)[3:5], each = 2), names(EstimatesStats)[6:7])

# Create list object with by RG stock comps
HeatmapEstimates <- sapply(GroupNames8, function(RG) {
  matrix(data = sapply(mixnames, function(mix) {EstimatesStats[[mix]][RG, "mean"] }),
         nrow = 2, ncol = 4, dimnames = list(c("Early", "Late"), c("KTN", "PBGWRN", "Inside", "Outside"))
  )
}, simplify = FALSE)
zmax <- max(sapply(HeatmapEstimates, max))
zmax <- 0.91

Testing <- matrix(c(seq(from = 0, to = zmax, length.out = 102), seq(from = 0, to = zmax, length.out = 102)), nrow = 2, ncol = 102, byrow = T)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Plot: Can't do a nested layout, writing out as pdf then pasting in other pdf

GroupNames8Pub2 <- c(GroupNames8Pub[1:2], "West Vancouver",GroupNames8Pub[4:8])
names(GroupNames8Pub2) <- GroupNames8

# pdf("Figures/2017SportByFishery.pdf", family = "Times", width = 6.5, height = 6.5, title = "2016 Troll By Fishery and Quadrant")
png("Figures/2017SportByArea.png", family = "Times", width = 6.5, height = 6.5, units = "in", res = 300)
# x11(width = 6.5, height = 6.5)
par(xaxt = "n", yaxt = "n", omi = rep(0.1, 4), mar = rep(0.1, 4), family = 'serif')
layout(layoutmat,widths=c(0.3,1,1,0.25),heights=c(1,1,1,1,0.4))

## Loop through Reporting Group plots
sapply(GroupNames8, function(RG) {
  image(t(HeatmapEstimates[[RG]])[, ], zlim = c(0, zmax), col = WhiteRedcol, xlab = "", ylab = "", breaks = seq(from = 0, to = zmax, length.out = 102), useRaster = TRUE)
  # abline(h = 0.5, lwd = 2, col = 'grey')
  segments(x0 = 0.835, x1 = 1.165, y0 = 0.5, y1 = 0.5, lwd = 2, col = 'grey')
  abline(v = c(0.175, 0.5, 0.835), lwd= 2 , col = 'grey')
  abline(h = c(-0.5, 1.5), v = c(-0.165, 1.165),lwd = 5, col = 'black')
  text(labels = substitute(expr = italic(x), env = list(x = GroupNames8Pub2[RG])), cex = 2, adj = c(0, 0.5), x = -0.1, y = 1)
})

## Plot 10 - Y-axis label
plot.new()
text(labels = "Period", cex = 3, srt = 90, x = 0.3, y = 0.5, adj = c(0.5, 0))
text(labels = "Early", cex = 2, x = 0.99, y = c(0.97, 0.7, 0.43, 0.16), adj = c(1, 0.5))
text(labels = "Late", cex = 2, x = 0.99, y = c(0.97, 0.7, 0.43, 0.16) - 0.135, adj = c(1, 0.5))

## Plot 11 - X-axis label
plot.new()
text(labels = "Area", cex = 3, adj = c(0.5, 0.5), x = 0.5, y = 0.35)
text(labels = "KTN", cex = 1.5, adj = c(0.5, 0.5), x = c(0.03, 0.57), y = 0.8)
text(labels = "PB-WR", cex = 1.5, adj = c(0.5, 0.5), x = c(0.03 + 0.14, 0.57 + 0.14), y = 0.8)
text(labels = "Inside", cex = 1.5, adj = c(0.5, 0.5), x = c(0.03 + 0.27, 0.57 + 0.27), y = 0.8)
text(labels = "Outside", cex = 1.5, adj = c(0.5, 0.5), x = c(0.03 + 0.40, 0.57 + 0.40), y = 0.8)

## Plot 13 - Legend
image(Testing, col = WhiteRedcol, xlab = "", ylab = "", breaks = seq(from = 0, to = zmax, length.out = 102))
text(labels = "0%", cex = 2.8, adj = c(0.5, 0.5), x = 0.5, y = 0.03)
text(labels = paste0(zmax * 100, "%"), cex = 2.8, adj = c(0.5, 0.5), x = 0.5, y = 0.98)
abline(h = c(-0.005,  1.005),  v  =  c(-0.5,  1.5), lwd = 5, col = 'black')
dev.off()
dev.off()
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summary Tables ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

objects(pattern = "SampleSizes")
FinalSampleSizes <- c(K119_K120_Strata_SampleSizes[, "Final"], K123_Strata_SampleSizes[, "Final"])
FinalSampleSizes[FinalSampleSizes > 100]

TrollMixtures <- list("EWintAllQuad_2017" = EWint_Mixtures,
                      "EWintNO_2017" = EWint_Mixtures[1],
                      "LWintAllQuad_2017" = LWint_Mixtures,
                      "LWintNO_2017" = LWint_Mixtures[1],
                      "SpringAllQuad_2017" = c(SpringRet1_Mixtures, SpringRet2_Mixtures),
                      "SpringNO_2017" = c(SpringRet1_Mixtures[2], SpringRet2_Mixtures[2]),
                      "SpringNI_2017" = c(SpringRet1_Mixtures[1], SpringRet2_Mixtures[1]),
                      "SpringSO_2017" = c(SpringRet1_Mixtures[4], SpringRet2_Mixtures[4]),
                      "SpringSI_2017" = c(SpringRet1_Mixtures[3], SpringRet2_Mixtures[3]),
                      "SummerRet1AllQuad_2017" = SummerRet1_Mixtures,
                      "SummerRet1NO_2017" = SummerRet1_Mixtures[2],
                      "AllYearTroll_2017" = c(EWint_Mixtures, LWint_Mixtures, SpringRet1_Mixtures, SpringRet2_Mixtures, SummerRet1_Mixtures))

SportMixtures <- list("KTNSport_2017" = Sport_Mixtures[3],
                      "PBGWRNSport_2017" = Sport_Mixtures[4],
                      "InsideSport_2017" = Sport_Mixtures[5],
                      "OutsideSport_2017" = Sport_Mixtures[6:7],
                      "OutsidePer1Sport_2017" = Sport_Mixtures[6],
                      "OutsidePer2Sport_2017" = Sport_Mixtures[7],
                      "AllYearSport_2017" = Sport_Mixtures[3:7])

TrollSampleSizes <- sapply(TrollMixtures, function(mix) {sum(FinalSampleSizes[mix])})
SportSampleSizes <- sapply(SportMixtures, function(mix) {sum(FinalSampleSizes[mix])})
AllSampleSizes <- c(TrollSampleSizes, SportSampleSizes)


## Get objects
SEAKobjects <- list.files(path = "Estimates objects", pattern = "2017", recursive = FALSE)
invisible(sapply(SEAKobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Estimates objects", objct, sep = "/")), pos = 1) } )); rm(SEAKobjects)#; beep(2)


Troll2017_4RG_EstimatesStats_Report <- 
  list("EWintAllQuad_2017" = EWint_2017_4RG_StratifiedEstimatesStats,
       "EWintNO_2017" = EWint_2017_4RG_EstimatesStats$EWintNO_2017,
       "LWintAllQuad_2017" = LWint_2017_4RG_StratifiedEstimatesStats,
       "LWintNO_2017" = LWint_2017_4RG_EstimatesStats$LWintNO_2017,
       "SpringNO_2017" = SpringNO_2017_4RG_StratifiedEstimatesStats,
       "SpringNI_2017" = SpringNI_2017_4RG_StratifiedEstimatesStats,
       "SpringSO_2017" = SpringSO_2017_4RG_StratifiedEstimatesStats,
       "SpringSI_2017" = SpringSI_2017_4RG_StratifiedEstimatesStats,
       "SummerRet1AllQuad_2017" = SummerRet1_2017_4RG_StratifiedEstimates,
       "SummerRet1NO_2017" = SummerRet1_2017_4RG_EstimatesStats$SummerRet1NO_2017)


Troll2017_8RG_EstimatesStats_Report <- 
  list("EWintAllQuad_2017" = EWint_2017_8RG_StratifiedEstimatesStats,
       "EWintNO_2017" = EWint_2017_8RG_EstimatesStats$EWintNO_2017,
       "LWintAllQuad_2017" = LWint_2017_8RG_StratifiedEstimatesStats,
       "LWintNO_2017" = LWint_2017_8RG_EstimatesStats$LWintNO_2017,
       "SpringAllQuad_2017" = Spring_2017_8RG_StratifiedEstimatesStats,
       "SpringNO_2017" = SpringNO_2017_8RG_StratifiedEstimatesStats,
       "SpringSI_2017" = SpringSI_2017_8RG_StratifiedEstimatesStats,
       "SummerRet1AllQuad_2017" = SummerRet1_2017_8RG_StratifiedEstimates,
       "SummerRet1NO_2017" = SummerRet1_2017_8RG_EstimatesStats$SummerRet1NO_2017,
       "AllYearTroll_2017" = AllYearTroll2017_8RG_StratifiedEstimatesStats)

Troll2017_18RG_EstimatesStats_Report <- 
  list("EWintAllQuad_2017" = EWint_2017_18RG_StratifiedEstimatesStats,
       "EWintNO_2017" = EWint_2017_18RG_EstimatesStats$EWintNO_2017,
       "LWintAllQuad_2017" = LWint_2017_18RG_StratifiedEstimatesStats,
       "LWintNO_2017" = LWint_2017_18RG_EstimatesStats$LWintNO_2017,
       "SpringAllQuad_2017" = Spring_2017_18RG_StratifiedEstimatesStats,
       "SpringNO_2017" = SpringNO_2017_18RG_StratifiedEstimatesStats,
       "SpringSI_2017" = SpringSI_2017_18RG_StratifiedEstimatesStats,
       "SummerRet1AllQuad_2017" = SummerRet1_2017_18RG_StratifiedEstimates,
       "SummerRet1NO_2017" = SummerRet1_2017_18RG_EstimatesStats$SummerRet1NO_2017)

Troll2017_26RG_EstimatesStats_Report <- 
  list("EWintAllQuad_2017" = EWint_2017_26RG_StratifiedEstimatesStats,
       "EWintNO_2017" = EWint_2017_26RG_EstimatesStats$EWintNO_2017,
       "LWintAllQuad_2017" = LWint_2017_26RG_StratifiedEstimatesStats,
       "LWintNO_2017" = LWint_2017_26RG_EstimatesStats$LWintNO_2017,
       "SpringAllQuad_2017" = Spring_2017_26RG_StratifiedEstimatesStats,
       "SpringNO_2017" = SpringNO_2017_26RG_StratifiedEstimatesStats,
       "SpringSI_2017" = SpringSI_2017_26RG_StratifiedEstimatesStats,
       "SummerRet1AllQuad_2017" = SummerRet1_2017_26RG_StratifiedEstimates,
       "SummerRet1NO_2017" = SummerRet1_2017_26RG_EstimatesStats$SummerRet1NO_2017)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Sport2017_4RG_EstimatesStats_Report <- 
  c(Sport_2017_4RG_EstimatesStats[3:5],
    list("OutsideSport_2017" = OutsideSport_2017_4RG_StratifiedEstimatesStats),
    Sport_2017_4RG_EstimatesStats[6:7])

Sport2017_8RG_EstimatesStats_Report <- 
  c(Sport_2017_8RG_EstimatesStats[3:5],
    list("OutsideSport_2017" = OutsideSport_2017_8RG_StratifiedEstimatesStats),
    Sport_2017_8RG_EstimatesStats[6:7],
    list("AllYearSport_2017" = AllYearSport2017_8RG_StratifiedEstimatesStats))

Sport2017_18RG_EstimatesStats_Report <- 
  c(Sport_2017_18RG_EstimatesStats[3:5],
    list("OutsideSport_2017" = OutsideSport_2017_18RG_StratifiedEstimatesStats),
    Sport_2017_18RG_EstimatesStats[6:7])

Sport2017_26RG_EstimatesStats_Report <- 
  c(Sport_2017_26RG_EstimatesStats[3:5],
    list("OutsideSport_2017" = OutsideSport_2017_26RG_StratifiedEstimatesStats),
    Sport_2017_26RG_EstimatesStats[6:7])


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
TrollMixPub2017 <- setNames(object = c("Early Winter All Quadrants",
                                       "Early Winter Northern Outside Quadrant",
                                       "Late Winter All Quadrants",
                                       "Late Winter Northern Outside Quadrant",
                                       "Spring All Quadrants",
                                       "Spring Northern Outside Quadrant",
                                       "Spring Northern Inside Quadrant",
                                       "Spring Southern Outside Quadrant",
                                       "Spring Southern Inside Quadrant",
                                       "Summer Retention 1 All Quadrants",
                                       "Summer Retention 1 Northern Outside Quadrant",
                                       "AY 2017 Annual"), 
                            nm = names(TrollMixtures))

SportMixPub2017 <-  setNames(
  object = c("Ketchikan",
             "Petersburg-Wrangell",
             "Northern Inside",
             "Outside All Year",
             "Outside Biweek 9-13",
             "Outside Biweek 14-18",
             "AY 2017 Annual"),
  nm = names(SportMixtures)
)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Create fully formatted spreadsheat

table_format.f <- function(EstimatesStats, SampSizes, PubNames, filename, sheetname) {
  
  for(mix in names(EstimatesStats)) {
    
    nRG <- nrow(EstimatesStats[[mix]])
    
    TableX <- matrix(data = "", nrow = (3 + nRG), ncol = 7)
    TableX[1, 3] <- paste0(PubNames[mix], "(n=", SampSizes[mix], ")")
    TableX[2, 6] <- "90% CI"
    TableX[3, 2:7] <- c("Reporting Group", "Mean", "SD", "Median", "5%", "95%")
    TableX[4:(nRG + 3), 1] <- seq(nRG)
    TableX[4:(nRG + 3), 2] <- rownames(EstimatesStats[[mix]])
    TableX[4:(nRG + 3), 3:7] <- formatC(x = EstimatesStats[[mix]][, c("mean", "sd", "median", "5%", "95%")], digits = 3, format = "f")
    
    write.xlsx(x = TableX, file = paste0("Estimates tables/", filename, ".xlsx"),
               col.names = FALSE, row.names = FALSE, sheetName = paste(mix, sheetname), append = TRUE)
    
  }
}

EstimatesStats <- Troll2017_26RG_EstimatesStats_Report
SampSizes <- AllSampleSizes
PubNames <- TrollMixPub2017
filename <- "Troll2016_26RG_StratifiedEstimatesStats_FormattedPretty"
sheetname <- "Troll 26RG"

require(xlsx)
table_format.f(EstimatesStats = Troll2017_26RG_EstimatesStats_Report, 
               SampSizes = AllSampleSizes, 
               PubNames = TrollMixPub2017, 
               filename = "Troll2017_26RG_StratifiedEstimatesStats_FormattedPretty", 
               sheetname = "Troll 26RG")

table_format.f(EstimatesStats = Sport2017_26RG_EstimatesStats_Report, 
               SampSizes = AllSampleSizes, 
               PubNames = SportMixPub2017, 
               filename = "Sport2017_26RG_StratifiedEstimatesStats_FormattedPretty", 
               sheetname = "Sport 26RG")

table_format.f(EstimatesStats = Troll2017_18RG_EstimatesStats_Report, 
               SampSizes = AllSampleSizes, 
               PubNames = TrollMixPub2017, 
               filename = "Troll2017_18RG_StratifiedEstimatesStats_FormattedPretty", 
               sheetname = "Troll 18RG")

table_format.f(EstimatesStats = Sport2017_18RG_EstimatesStats_Report, 
               SampSizes = AllSampleSizes, 
               PubNames = SportMixPub2017, 
               filename = "Sport2017_18RG_StratifiedEstimatesStats_FormattedPretty", 
               sheetname = "Sport 18RG")

table_format.f(EstimatesStats = Troll2017_8RG_EstimatesStats_Report, 
               SampSizes = AllSampleSizes, 
               PubNames = TrollMixPub2017, 
               filename = "Troll2017_8RG_StratifiedEstimatesStats_FormattedPretty", 
               sheetname = "Troll 8RG")

table_format.f(EstimatesStats = Sport2017_8RG_EstimatesStats_Report, 
               SampSizes = AllSampleSizes, 
               PubNames = SportMixPub2017, 
               filename = "Sport2017_8RG_StratifiedEstimatesStats_FormattedPretty", 
               sheetname = "Sport 8RG")

table_format.f(EstimatesStats = Troll2017_4RG_EstimatesStats_Report, 
               SampSizes = AllSampleSizes, 
               PubNames = TrollMixPub2017, 
               filename = "Troll2017_4RG_StratifiedEstimatesStats_FormattedPretty", 
               sheetname = "Troll 4RG")

table_format.f(EstimatesStats = Sport2017_4RG_EstimatesStats_Report, 
               SampSizes = AllSampleSizes, 
               PubNames = SportMixPub2017, 
               filename = "Sport2017_4RG_StratifiedEstimatesStats_FormattedPretty", 
               sheetname = "Sport 4RG")


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
SEAKobjects <- SEAKobjects[-which(SEAKobjects == "Vials" | SEAKobjects == "OLD_BAD_LOCUSCONTROL")]
SEAKobjects

invisible(sapply(SEAKobjects, function(objct) {assign(x = unlist(strsplit(x = objct, split = ".txt")), value = dget(file = paste(getwd(), "Objects", objct, sep = "/")), pos = 1) })); beep(2)

## Get preQC mixtures
ONCOR_Mixtures <- c("KSPORT17", "KTROL16EW", "KTROL17LW", "KTROL17SP", "KTROL17SU")
dput(x = ONCOR_Mixtures, file = "Objects/ONCOR_Mixtures.txt")
invisible(sapply(ONCOR_Mixtures, function(silly) {assign(x = paste0(silly, ".gcl"), value = dget(file = paste0("Raw genotypes/OriginalCollections_Attributes/", silly, ".txt")), pos = 1)} )); beep(2)
objects(pattern = "\\.gcl")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Data QC/Massage ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
ONCOR_Mixtures_Miss <- RemoveIndMissLoci.GCL(sillyvec = ONCOR_Mixtures, proportion = 0.8)

ONCOR_Mixtures_DuplicateCheck95MinProportion <- CheckDupWithinSilly.GCL(sillyvec = ONCOR_Mixtures, loci = GAPSLoci, quantile = NULL, minproportion = 0.95)
ONCOR_Mixtures_Dups <- RemoveDups.GCL(ONCOR_Mixtures_DuplicateCheck95MinProportion)

dput(x = ONCOR_Mixtures_Miss, file = "Objects/ONCOR_Mixtures_Miss.txt")
dput(x = ONCOR_Mixtures_Dups, file = "Objects/ONCOR_Mixtures_Dups.txt")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### ONCOR Individual Assignment ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Export genotypes to mixture files for ONCOR
# This is for "Origins" in order to send the correct scale cards to respective agencies for aging
# Using basline and reporting group files from the "SEAK16" directory

# I will need the following files for input into ONCOR
# baseline file: in Genepop format       (Use file "SEAK357Pops", from 2013 baseline)
# reporting group file: column 1 is pops, column 2 is reporting group    (Use files 1) "CTCgroups_21ProposalRG", 2) "CTCgroups_30RG", 3) "GAPSgroups_26RG")
# mixture file: in Genepop format, only one "Pop" designation

# dir.create(path = "ONCOR/Mixture")

# Use "all individuals" or "subsampled mixture" individuals?
# Sara has used "all individuals" in the past to get them sent out for ages, need to check with Randy
# Thu Jan 25 11:28:06 2018; spoke with Randy and he confirmed that we want ALL samples that have been genotyped
date()

gcl2Genepop.GCL(sillyvec = ONCOR_Mixtures[1], path = "ONCOR/Mixture/Sport2017.gen", loci = GAPSLoci, VialNums = TRUE, usat = TRUE)

gcl2Genepop.GCL(sillyvec = ONCOR_Mixtures[-1], path = "ONCOR/Mixture/Troll2017.gen", loci = GAPSLoci, VialNums = TRUE, usat = TRUE)
# Remove "Pop" designations
rawdat <- scan(file = "ONCOR/Mixture/Troll2017.gen", what = '', sep = '\n')
moddat <- rawdat[-grep(pattern = "Pop", x = rawdat)[-1]]
write.table(x = moddat, file = "ONCOR/Mixture/Troll2017.gen", quote = FALSE, row.names = FALSE, col.names = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Sport 2017 Scales for Origins ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Sport attributes to join with Oncor Output
str(KSPORT17.gcl$attributes)
sport_attributes <- c("SillySource", "Date", "SITE", "ScaleCardNumber", "ScaleCardRow", "Length", "TAGNUM", "OTOLITH")
sport_attributes.df <- KSPORT17.gcl$attributes[, sport_attributes]
str(sport_attributes.df)

## Read in Oncor output
rawsport <- scan(file = "ONCOR/Output/Sport2017_21RG_out.txt", what = '', sep = '\n', blank.lines.skip = FALSE)
skip = grep(pattern = "PROBABILITY OF EACH INDIVIDUAL IN THE MIXTURE BELONGING TO EACH REPORTING GROUP", x = rawsport) + 1
dat <- read.table(file = "ONCOR/Output/Sport2017_21RG_out.txt", header = TRUE, skip = skip)
str(dat)
GroupNames21 <- colnames(dat)
dput(x = GroupNames21, file = "Objects/GroupNames21.txt")

## Base answer
# x <- t(apply(data.matrix(dat), 1, function(ind) {
#   c(GroupNames21[which.max(ind)], max(ind))
# } ))

## Tidy answer
library(tidyverse)

# Add FishID as factor
dat.mod <- dat %>% 
  mutate(FishID = factor(x = rownames(dat), levels = rownames(dat)))

# 1st probability RG
dat.tdy.1 <- dat.mod %>% 
  gather(RG, prob, -FishID) %>% 
  group_by(FishID) %>% 
  slice(which.max(prob))
head(dat.tdy.1)

# 2nd probability RG
dat.tdy.2 <- dat.mod %>% 
  gather(RG, prob, -FishID) %>% 
  group_by(FishID) %>% 
  filter(rank(desc(prob)) == 2) %>% 
  arrange(FishID)
head(dat.tdy.2)

# Join 1st and 2nd probability RG with sport attributes and filter for only outside caught fish (Sitka and Craig) that have an otolith number
sport.dat.tdy <- left_join(x = dat.tdy.1, y = dat.tdy.2, by = "FishID") %>% 
  rename(RG.1 = RG.x, prob.1 = prob.x, RG.2 = RG.y, prob.2 = prob.y) %>% 
  filter(prob.1 >= 0.8) %>%
  left_join(sport_attributes.df, by = c("FishID" = "SillySource")) %>% 
  # filter(SITE %in% c("CRAIG_KLAWOCK", "SITKA")) %>%  # only filter for Craig and Sitka fish?
  filter(OTOLIT != ".")  # filter for only fish with otolith number
head(sport.dat.tdy)
str(sport.dat.tdy, max.level = 1)
dput(x = sport.dat.tdy, file = "Objects/sport.dat.tdy.txt")


## Create individual sheets for different agencies
sport_colnames <- c("IND", "Best Estimate", "Probability 1", "2nd Best Estimate", "Probability 2", "Date", "Port", "CardNo", "IndNo", "Length", "CWT", "Otolith")

# Canada
canada.sport.dat.tdy <- sport.dat.tdy %>% 
  filter(RG.1 %in% c("FraserEarly", "NorCentBC", "WCVI")) %>% 
  arrange(RG.1, desc(prob.1))
colnames(canada.sport.dat.tdy) <- sport_colnames
head(canada.sport.dat.tdy)

# Washington
washington.sport.dat.tdy <- sport.dat.tdy %>% 
  filter(RG.1 %in% c("WACoast")) %>% 
  arrange(RG.1, desc(prob.1))
colnames(washington.sport.dat.tdy) <- sport_colnames
head(washington.sport.dat.tdy)

# Oregon
oregon.sport.dat.tdy <- sport.dat.tdy %>% 
  filter(RG.1 %in% c("NORCoast")) %>% 
  arrange(RG.1, desc(prob.1))
colnames(oregon.sport.dat.tdy) <- sport_colnames
head(oregon.sport.dat.tdy)

# Columbia
columbia.sport.dat.tdy <- sport.dat.tdy %>% 
  filter(RG.1 %in% c("IntColSuFa")) %>% 
  arrange(RG.1, desc(prob.1))
colnames(columbia.sport.dat.tdy) <- sport_colnames
head(columbia.sport.dat.tdy)

# Alaska
alaska.sport.dat.tdy <- sport.dat.tdy %>% 
  filter(RG.1 %in% c("SSEAK")) %>% 
  arrange(RG.1, desc(prob.1))
colnames(alaska.sport.dat.tdy) <- sport_colnames
head(alaska.sport.dat.tdy)



## Write out data for Anne
# dir.create("Origins")
# write.csv(x = canada.sport.dat.tdy, file = "Origins/Sport_2017Scales_toread.csv", row.names = FALSE)

require(xlsx)
write.xlsx(x = as.data.frame(canada.sport.dat.tdy), file = "Origins/Sport_2017Scales_toread.xlsx", sheetName = "Canada", row.names = FALSE, append = TRUE)
write.xlsx(x = as.data.frame(washington.sport.dat.tdy), file = "Origins/Sport_2017Scales_toread.xlsx", sheetName = "Washington", row.names = FALSE, append = TRUE)
write.xlsx(x = as.data.frame(oregon.sport.dat.tdy), file = "Origins/Sport_2017Scales_toread.xlsx", sheetName = "Oregon", row.names = FALSE, append = TRUE)
write.xlsx(x = as.data.frame(columbia.sport.dat.tdy), file = "Origins/Sport_2017Scales_toread.xlsx", sheetName = "Columbia", row.names = FALSE, append = TRUE)
write.xlsx(x = as.data.frame(alaska.sport.dat.tdy), file = "Origins/Sport_2017Scales_toread.xlsx", sheetName = "Alaska", row.names = FALSE, append = TRUE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Troll 2017 Scales for Origins ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Need troll data for Winter, Spring, and Summer

## Troll ASL data to join with Oncor Output
troll_ASL.df <- read.csv(file = "Associated Data/All 2016-2017 Troll Harvest - Detailed ASL Samples.csv", stringsAsFactors = FALSE)
str(troll_ASL.df)
troll_ASL.df$Date <- as.Date(x = troll_ASL.df$Sample.Date, format = c("%m/%d/%Y"))

troll_attributes <- c("Dna.Specimen.No", "Date", "Port.Code", "District", "Scale.Card.No", "Specimen.Number", "Length.Millimeters", "Cwt.Strap.Tag.No", "Adipose.Clipped.No.Signal")
troll_attributes.df <- troll_ASL.df[, troll_attributes]
str(troll_attributes.df)

## Read in Oncor output
rawtroll <- scan(file = "ONCOR/Output/Troll2017_21RG_out.txt", what = '', sep = '\n', blank.lines.skip = FALSE)
skip = grep(pattern = "PROBABILITY OF EACH INDIVIDUAL IN THE MIXTURE BELONGING TO EACH REPORTING GROUP", x = rawtroll) + 1
dat <- read.table(file = "ONCOR/Output/Troll2017_21RG_out.txt", header = TRUE, skip = skip)
str(dat)
# GroupNames21 <- colnames(dat)
# dput(x = GroupNames21, file = "Objects/GroupNames21.txt")

## Base answer
# x <- t(apply(data.matrix(dat), 1, function(ind) {
#   c(GroupNames21[which.max(ind)], max(ind))
# } ))

## Tidy answer
library(tidyverse)

# Add FishID as factor
dat.mod <- dat %>% 
  mutate(FishID = factor(x = rownames(dat), levels = rownames(dat)))

# 1st probability RG
dat.tdy.1 <- dat.mod %>% 
  gather(RG, prob, -FishID) %>% 
  group_by(FishID) %>% 
  slice(which.max(prob))
head(dat.tdy.1)

# 2nd probability RG
dat.tdy.2 <- dat.mod %>% 
  gather(RG, prob, -FishID) %>% 
  group_by(FishID) %>% 
  filter(rank(desc(prob)) == 2) %>% 
  arrange(FishID)
head(dat.tdy.2)

# Join 1st and 2nd probability RG with troll attributes
troll.dat.tdy <- left_join(x = dat.tdy.1, y = dat.tdy.2, by = "FishID") %>% 
  rename(RG.1 = RG.x, prob.1 = prob.x, RG.2 = RG.y, prob.2 = prob.y) %>% 
  separate(col = FishID, into = c("SILLY", "ID"), sep = "_", remove = FALSE) %>% 
  mutate(ID = as.numeric(ID)) %>% 
  select(-SILLY) %>% 
  filter(prob.1 >= 0.8) %>%
  left_join(troll_attributes.df, by = c("ID" = "Dna.Specimen.No"))
head(troll.dat.tdy)
str(troll.dat.tdy, max.level = 1)
dput(x = troll.dat.tdy, file = "Objects/troll.dat.tdy.txt")


## Create individual sheets for different agencies
troll_colnames <- c("IND", "Dna.Specimin.No", "Best Estimate", "Probability 1", "2nd Best Estimate", "Probability 2", "Date", "Port", "Quad/Dist", "CardNo", "IndNo", "Length", "CWT", "AdClipNoSignal")

# Canada
canada.troll.dat.tdy <- troll.dat.tdy %>% 
  filter(RG.1 %in% c("FraserEarly", "NorCentBC", "WCVI")) %>% 
  arrange(RG.1, desc(prob.1))
colnames(canada.troll.dat.tdy) <- troll_colnames
head(canada.troll.dat.tdy)

# Washington
washington.troll.dat.tdy <- troll.dat.tdy %>% 
  filter(RG.1 %in% c("WACoast")) %>% 
  arrange(RG.1, desc(prob.1))
colnames(washington.troll.dat.tdy) <- troll_colnames
head(washington.troll.dat.tdy)

# Oregon
oregon.troll.dat.tdy <- troll.dat.tdy %>% 
  filter(RG.1 %in% c("NORCoast")) %>% 
  arrange(RG.1, desc(prob.1))
colnames(oregon.troll.dat.tdy) <- troll_colnames
head(oregon.troll.dat.tdy)

# Columbia
columbia.troll.dat.tdy <- troll.dat.tdy %>% 
  filter(RG.1 %in% c("IntColSuFa")) %>% 
  arrange(RG.1, desc(prob.1))
colnames(columbia.troll.dat.tdy) <- troll_colnames
head(columbia.troll.dat.tdy)

# Alaska
alaska.troll.dat.tdy <- troll.dat.tdy %>% 
  filter(RG.1 %in% c("SSEAK")) %>% 
  arrange(RG.1, desc(prob.1))
colnames(alaska.troll.dat.tdy) <- troll_colnames
head(alaska.troll.dat.tdy)



## Write out data for Anne
# dir.create("Origins")
# write.csv(x = canada.troll.dat.tdy, file = "Origins/Troll_2017Scales_toread.csv", row.names = FALSE)

require(xlsx)
write.xlsx(x = as.data.frame(canada.troll.dat.tdy), file = "Origins/Troll_2017Scales_toread.xlsx", sheetName = "Canada", row.names = FALSE, append = TRUE)
write.xlsx(x = as.data.frame(washington.troll.dat.tdy), file = "Origins/Troll_2017Scales_toread.xlsx", sheetName = "Washington", row.names = FALSE, append = TRUE)
write.xlsx(x = as.data.frame(oregon.troll.dat.tdy), file = "Origins/Troll_2017Scales_toread.xlsx", sheetName = "Oregon", row.names = FALSE, append = TRUE)
write.xlsx(x = as.data.frame(columbia.troll.dat.tdy), file = "Origins/Troll_2017Scales_toread.xlsx", sheetName = "Columbia", row.names = FALSE, append = TRUE)
write.xlsx(x = as.data.frame(alaska.troll.dat.tdy), file = "Origins/Troll_2017Scales_toread.xlsx", sheetName = "Alaska", row.names = FALSE, append = TRUE)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Confirm All Year Troll Sample Sizes ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 2017
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
mixtures <- c(EWint_Mixtures,
              LWint_Mixtures,
              SpringRet1_Mixtures,
              SpringRet2_Mixtures, 
              SummerRet1_Mixtures)
sum(sapply(mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Mixture/", mix, ".mix")))[1]} ))

All2017_Strata_SampleSizes <- rbind(K119_K120_Strata_SampleSizes, K123_Strata_SampleSizes)
dput(x = All2017_Strata_SampleSizes, file = "Objects/All2017_Strata_SampleSizes.txt")

# 2016
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK16")
mixtures <- c("EWintNISISO_2016", "EWintNO_2016", "LWintNISISO_2016", "LWintNO_2016", 
              "SpringNI_2016", "SpringNO_2016", "SpringSI_2016", "SpringSO_2016", 
              "Summer1NI_2016", "Summer1NO_2016", "Summer1SI_2016", "Summer1SO_2016", 
              "Summer2NI_2016", "Summer2NO_2016", "Summer2SI_2016", "Summer2SO_2016")
sum(sapply(mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Mixture/", mix, ".mix")))[1]} ))


# 2015
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK15")
mixtures <- c("EWintNISISO_2015", "EWintNO_2015", 
              "LWintNISISO_2015", "LWintNO_2015", 
              "SpringNI_2015", "SpringNO_2015", "SpringSI_2015", "SpringSO_2015", 
              "SumRet1NISISO_2015", "SumRet1NO_2015")
sum(sapply(mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Mixture/", mix, ".mix")))[1]} ))


# 2014
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK14")
mixtures <- c("EWintNISISO.2014", "EWintNO.2014", 
              "LWintNISISO.2014", "LWintNO.2014", 
              "SpringNI.2014", "SpringNO.2014", "SpringSI.2014", "SpringSO.2014", 
              "SumRet1NISISO.2014", "SumRet1NO.2014", 
              "SumRet2NISISO.2014", "SumRet2NO.2014")
sum(sapply(mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Mixture/", mix, ".mix")))[1]} ))


# 2013
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK13")
mixtures <- c("EWintNISISO.2013", "EWintNO.2013", 
              "LWintNISISO.2013", "LWintNO.2013", 
              "SpringNI.2013", "SpringNO.2013", "SpringSI.2013", 
              "SumRet1NISISO.2013", "SumRet1NO.2013")
sum(sapply(mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Mixture/", mix, ".mix")))[1]} ))


# 2012
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK12")
mixtures <- c("EWintNISISO.2012", "EWintNO.2012", 
              "LWintNISISO.2012", "LWintNO.2012", 
              "SpringNI.2012", "SpringNO.2012", "SpringSI.2012", 
              "SumRet1NISISO.2012", "SumRet1NO.2012", 
              "SumRet2NISISO.2012", "SumRet2NO.2012")
sum(sapply(mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Mixture/", mix, ".mix")))[1]} ))


# 2011
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK11")
mixtures <- c("EarlyWinterNISISO", "EarlyWinterNO", 
              "LateWinterNISISO", "LateWinterNO", 
              "SpringNI", "SpringNO", "SpringSI", 
              "SummerR1NISISO", "SummerR1NO", 
              "SummerR2NISISO", "SummerR2NO")
  sum(sapply(mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Troll/Mixture/", mix, ".mix")))[1]} ))


# 2010
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK10")
mixtures <- c("EarlyWinterNISISO.2010", "EarlyWinterNO.2010", 
              "LateWinterNISISO.2010", "LateWinterNO.2010", 
              "SpringNI.2010", "SpringNO.2010", "SpringSI.2010", 
              "SummerR1NISISO.2010", "SummerR1NO.2010", 
              "SummerR2NISISO.2010", "SummerR2NO.2010")
  sum(sapply(mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Troll/Mixture/", mix, ".mix")))[1]} ))


# 2009
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK09")
mixtures <- c("EarlyWinterNISISO", "EarlyWinterNO", 
              "LateWinterNISISO", "LateWinterNO", 
              "SpringNI", "SpringNO", "SpringSI", 
              "SummerR1NISISO", "SummerR1NO", 
              "SummerR2NISISO", "SummerR2NO")
sum(sapply(mixtures, function(mix) {dim(read.table(file = paste0("BAYES/Troll/Mixture/", mix, ".mix")))[1]} ))
  
sum(sapply(c("SumRet1NISI_2009", "SumRet1NOSO_2009", "SumRet2NISI_2009", "SumRet2NOSO_2009"), function(mix) {dim(read.table(file = paste0("BAYES/1_GAPS3.0/Mix/", mix, ".mix")))[1]} ),
    sapply(mixtures[1:7], function(mix) {dim(read.table(file = paste0("BAYES/Troll/Mixture/", mix, ".mix")))[1]} )
)
  
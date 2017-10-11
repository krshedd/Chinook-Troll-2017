#### Extraction List ####
# AY2017 SU1 and Sport (no SU2)
# Kyle Shedd
# Created Wed Oct 04 15:57:19 2017
date()

# Read in WGC data
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
# load("Extraction Lists/K123ExtractionLists.RData")
require(xlsx)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Summer 1st Retention ####
#~~~~~~~~~~~~~~~~~~
# 171
# Subsample 380
SU1_WGC.dat <- read.xlsx(file = "Associated Data/Troll Extractions - MTA Lab Troll Harvest Data.xlsx", sheetName = "SU 1 AY2017", header = TRUE)
str(SU1_WGC.dat)
SU1_WGC.dat$X..Tissues[SU1_WGC.dat$Whatman.Card.. == 6288] <- 20  # Fix error in data sheet from Iris

SU1_WGC_171.dat <- subset(SU1_WGC.dat, Fishery == "Trad Troll"  & Dist.Quad == 171)
str(SU1_WGC_171.dat)

SU1_WGC2Sample_171 <- sample(SU1_WGC_171.dat$Whatman.Card..)
SU1_WGC2Sample_171_order <- match(SU1_WGC2Sample_171, SU1_WGC_171.dat$Whatman.Card..)

any(cumsum(SU1_WGC_171.dat[SU1_WGC2Sample_171_order, "X..Tissues"]) == 380)
max2run_171 <- which(cumsum(SU1_WGC_171.dat[SU1_WGC2Sample_171_order, "X..Tissues"]) == 380)  # 380 samples from 171
SU1_WGC2Run_171 <- SU1_WGC2Sample_171[seq(max2run_171)]

#~~~~~~~~~~~~~~~~~~
# 172
# Subsample 380
SU1_WGC_172.dat <- subset(SU1_WGC.dat, Fishery == "Trad Troll"  & Dist.Quad == 172)
str(SU1_WGC_172.dat)

SU1_WGC2Sample_172 <- sample(SU1_WGC_172.dat$Whatman.Card..)
SU1_WGC2Sample_172_order <- match(SU1_WGC2Sample_172, SU1_WGC_172.dat$Whatman.Card..)

any(cumsum(SU1_WGC_172.dat[SU1_WGC2Sample_172_order, "X..Tissues"]) == 380)
max2run_172 <- which(cumsum(SU1_WGC_172.dat[SU1_WGC2Sample_172_order, "X..Tissues"]) == 380)  # 380 samples from 172
SU1_WGC2Run_172 <- SU1_WGC2Sample_172[seq(max2run_172)]

#~~~~~~~~~~~~~~~~~~
# 173
# Run all 340
SU1_WGC_173.dat <- subset(SU1_WGC.dat, Fishery == "Trad Troll"  & Dist.Quad == 173)
str(SU1_WGC_173.dat)
sum(SU1_WGC_173.dat$X..Tissues)

SU1_WGC_173.dat$X..Tissues[SU1_WGC_173.dat$Whatman.Card.. == 6288] <- 20  # Fix error in data sheet from Iris

SU1_WGC2Run_173 <- SU1_WGC_173.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~
# 174
# Run all 182
SU1_WGC_174.dat <- subset(SU1_WGC.dat, Fishery == "Trad Troll"  & Dist.Quad == 174)
str(SU1_WGC_174.dat)
sum(SU1_WGC_174.dat$X..Tissues)

SU1_WGC2Run_174 <- SU1_WGC_174.dat$Whatman.Card..


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Final extraction list for SU1
SU1_WGC2Run <- sort(c(SU1_WGC2Run_171, SU1_WGC2Run_172, SU1_WGC2Run_173, SU1_WGC2Run_174))

SU1_WGC_Run.dat <- SU1_WGC.dat[match(SU1_WGC2Run, SU1_WGC.dat$Whatman.Card..), ]
str(SU1_WGC_Run.dat)
sum(SU1_WGC_Run.dat$X..Tissues)
aggregate(X..Tissues ~ Dist.Quad, data = SU1_WGC_Run.dat, sum)

SU1_WGC_Run.dat$WGC <- sapply(SU1_WGC_Run.dat$Whatman.Card.., function(WGC) {ifelse(nchar(WGC) == 10, WGC, paste0(paste(rep(0, 10-nchar(WGC)), collapse = ''), WGC))})

# dir.create("Extraction Lists")
write.xlsx(x = SU1_WGC_Run.dat[, c(13, 3)], 
           file = "Extraction Lists/K123 Summer Troll Sport Extraction.xlsx", sheetName = "For LAB KTROL17SU1", append = TRUE, row.names = FALSE)
write.xlsx(x = SU1_WGC_Run.dat[, c(13, 1:9)], file = "Extraction Lists/K123 Summer Troll Sport Extraction.xlsx", 
           sheetName = "SU1 Extraction Data", append = TRUE, row.names = FALSE)

save.image("Extraction Lists/K123ExtractionLists.RData")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Sport ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
date()  # Tue Oct 10 13:41:29 2017
## Read in WGC data from K120 (TBR D8/11 Sport)
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
# load("Extraction Lists/K120ExtractionLists.RData")

# PBG/WRN
table(Sport_ASL_Gen_D108_allCards.dat$Biweek, Sport_ASL_Gen_D108_allCards.dat$SITE)
# Number of characters in WGC card and fish ID
table(nchar(Sport_ASL_Gen_D108_allCards.dat$GSI_CARD))
table(nchar(Sport_ASL_Gen_D108_allCards.dat$GsiCardRow))

# Create FISH_ID of GSI_CARD and GsiCardRow
Sport_ASL_Gen_D108_allCards.dat$FISH_ID <- paste(Sport_ASL_Gen_D108_allCards.dat$GSI_CARD, Sport_ASL_Gen_D108_allCards.dat$GsiCardRow, sep = "_")

# Vector of FISH_ID already extracted for K120 TBR
PBG_Already_Extracted <- Sport_ASL_Gen_D108_allCards.dat$FISH_ID[Sport_ASL_Gen_D108_allCards.dat$SITE == "PETERSBURG"]
WRN_Already_Extracted <- Sport_ASL_Gen_D108_allCards.dat$FISH_ID[Sport_ASL_Gen_D108_allCards.dat$SITE == "WRANGELL"]
#~~~~~~~~~~~~~~~~~~
# JNU
table(Sport_ASL_Gen_D111_allCards.dat$Biweek, Sport_ASL_Gen_D111_allCards.dat$SITE)
# Number of characters in WGC card and fish ID
table(nchar(Sport_ASL_Gen_D111_allCards.dat$GSI_CARD))
table(nchar(Sport_ASL_Gen_D111_allCards.dat$GsiCardRow))

# Create FISH_ID of GSI_CARD and GsiCardRow
Sport_ASL_Gen_D111_allCards.dat$FISH_ID <- paste(Sport_ASL_Gen_D111_allCards.dat$GSI_CARD, Sport_ASL_Gen_D111_allCards.dat$GsiCardRow, sep = "_")

# Vector of FISH_ID already extracted for K120 TBR
JNU_Already_Extracted <- Sport_ASL_Gen_D111_allCards.dat$FISH_ID[Sport_ASL_Gen_D111_allCards.dat$SITE == "JUNEAU"]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in WGC data from all year sport
rm(list = ls()[!ls() %in% c("PBG_Already_Extracted", "WRN_Already_Extracted", "JNU_Already_Extracted")])
# save.image("Extraction Lists/K123ExtractionLists.RData")
# load("Extraction Lists/K123ExtractionLists.RData")

require(xlsx)
Sport_ASL.dat <- read.xlsx(file = "Associated Data/Sport Extractions - Origins.xlsx", sheetName = "SEAK Sport", header = TRUE)
str(Sport_ASL.dat)

# Add FISH_ID
Sport_ASL.dat$FISH_ID <- paste(Sport_ASL.dat$GSI_CARD, Sport_ASL.dat$GsiCardRow, sep = "_")
table(table(Sport_ASL.dat$FISH_ID))
which(table(Sport_ASL.dat$FISH_ID) == 2)  # changed the GsiCardRow for these fish where appropriate to avoid duplicates

# Read in Table of Fish to extract
Selected.dat <- read.xlsx(file = "Associated Data/Sport Extractions - Origins.xlsx", sheetName = "Selected for Extraction", header = TRUE)

# Sample fish without including fish already run in TBR (K120)
SF_FISH_ID2Run <- 
  sapply(Selected.dat$Biweek, function(bw) {
    sapply(colnames(Selected.dat)[-1], function(port) {
      sample(x = setdiff(Sport_ASL.dat$FISH_ID[Sport_ASL.dat$Biweek == bw & Sport_ASL.dat$SITE == port & Sport_ASL.dat$SPECCODE == 410],
                         c(PBG_Already_Extracted, WRN_Already_Extracted, JNU_Already_Extracted)), 
             size = Selected.dat[which(Selected.dat$Biweek == bw), port])
    } )  # port
  } )  # bw
str(SF_FISH_ID2Run)
dim(SF_FISH_ID2Run)
dimnames(SF_FISH_ID2Run)

SF_FISH_ID2Run.vec <- unlist(SF_FISH_ID2Run)

Sport_ASL_2Run.dat <- subset(x = Sport_ASL.dat, subset = FISH_ID %in% SF_FISH_ID2Run.vec)
str(Sport_ASL_2Run.dat)
table(Sport_ASL_2Run.dat$Biweek, Sport_ASL_2Run.dat$SITE)[, colnames(Selected.dat)[-1]]
Selected.dat[-10, -1]

Sport_ASL_2Run_ordered.dat <- Sport_ASL_2Run.dat[order(Sport_ASL_2Run.dat$FISH_ID), ]

#~~~~~~~~~~~~~~~~~~
write.xlsx(x = Sport_ASL_2Run_ordered.dat[, c("GSI_CARD", "GsiCardRow")], 
           file = "Extraction Lists/K123 Summer Troll Sport Extraction.xlsx", sheetName = "For LAB KSPORT17", append = TRUE, row.names = FALSE)
write.xlsx(x = Sport_ASL_2Run_ordered.dat, file = "Extraction Lists/K123 Summer Troll Sport Extraction.xlsx", 
           sheetName = "Sport Extraction Data", append = TRUE, row.names = FALSE)

save.image("Extraction Lists/K123ExtractionLists.RData")

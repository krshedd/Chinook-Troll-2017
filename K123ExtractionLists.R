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
SU1_WGC.dat <- read.xlsx(file = "Associated Data/MTA Lab Troll Harvest Data.xlsx", sheetName = "SU 1 AY2017", header = TRUE)
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

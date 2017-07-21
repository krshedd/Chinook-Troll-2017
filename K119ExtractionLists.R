#### Extraction List ####
# Kyle Shedd
# Created Mon Jul 17 13:32:46 2017
# Modified Wed Jul 19 11:14:21 2017
# Modified again Thu Jul 20 15:29:22 2017 to completely re-do after addition of Spring 2 fish
date()

# Read in WGC data
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
# load("Extraction Lists/K119ExtractionLists.RData")
require(xlsx)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Early Winter ####
#~~~~~~~~~~~~~~~~~~
# 171
# Subsample 293
EW_WGC.dat <- read.xlsx(file = "Associated Data/MTA Lab Troll Harvest Data.xlsx", sheetName = "EW AY2017", header = TRUE)
str(EW_WGC.dat)

EW_WGC_171.dat <- subset(EW_WGC.dat, Fishery == "Troll"  & Dist.Quad == 171 | Fishery == "Troll matched axillary" & Dist.Quad == 171)
str(EW_WGC_171.dat)

EW_WGC2Sample_171 <- sample(EW_WGC_171.dat$Whatman.Card..)
EW_WGC2Sample_171_order <- match(EW_WGC2Sample_171, EW_WGC_171.dat$Whatman.Card..)

any(cumsum(EW_WGC_171.dat[EW_WGC2Sample_171_order, "X..Tissues"]) == 293)
max2run_171 <- which(cumsum(EW_WGC_171.dat[EW_WGC2Sample_171_order, "X..Tissues"]) == 293)  # 293 samples from 171
EW_WGC2Run_171 <- EW_WGC2Sample_171[seq(max2run_171)]

#~~~~~~~~~~~~~~~~~~
# 172
# Run all
EW_WGC_172.dat <- subset(EW_WGC.dat, Fishery == "Troll"  & Dist.Quad == 172 | Fishery == "Troll matched axillary" & Dist.Quad == 172)
str(EW_WGC_172.dat)
sum(EW_WGC_172.dat$X..Tissues)

EW_WGC2Run_172 <- EW_WGC_172.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~
# 173
# Run all
EW_WGC_173.dat <- subset(EW_WGC.dat, Fishery == "Troll"  & Dist.Quad == 173 | Fishery == "Troll matched axillary" & Dist.Quad == 173)
str(EW_WGC_173.dat)
sum(EW_WGC_173.dat$X..Tissues)

EW_WGC2Run_173 <- EW_WGC_173.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~
# 174
# Run all
EW_WGC_174.dat <- subset(EW_WGC.dat, Fishery == "Troll"  & Dist.Quad == 174 | Fishery == "Troll matched axillary" & Dist.Quad == 174)
str(EW_WGC_174.dat)
sum(EW_WGC_174.dat$X..Tissues)

EW_WGC2Run_174 <- EW_WGC_174.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Final extraction list for EW
EW_WGC2Run <- sort(c(EW_WGC2Run_171, EW_WGC2Run_172, EW_WGC2Run_173, EW_WGC2Run_174))

EW_WGC_Run.dat <- EW_WGC.dat[match(EW_WGC2Run, EW_WGC.dat$Whatman.Card..), ]
str(EW_WGC_Run.dat)
sum(EW_WGC_Run.dat$X..Tissues)
aggregate(X..Tissues ~ Dist.Quad, data = EW_WGC_Run.dat, sum)

# dir.create("Extraction Lists")
write.xlsx(x = EW_WGC_Run.dat, file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", 
           sheetName = "EW Extraction Data", append = TRUE, row.names = FALSE)
write.xlsx(x = matrix(data = sapply(EW_WGC_Run.dat$Whatman.Card.., function(WGC) {ifelse(nchar(WGC) == 10, WGC, paste0("000000", WGC))}), 
                      ncol = 1, dimnames = list(seq(EW_WGC2Run), "Whatman Cards to Extract")), 
           file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", sheetName = "For LAB KTROL16EW", append = TRUE, row.names = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Late Winter ####
#~~~~~~~~~~~~~~~~~~
# 171
# Subsample 302
LW_WGC.dat <- read.xlsx(file = "Associated Data/MTA Lab Troll Harvest Data.xlsx", sheetName = "LW AY2017", header = TRUE)
str(LW_WGC.dat)

LW_WGC_171.dat <- subset(LW_WGC.dat, Fishery == "Late Winter Troll"  & Dist.Quad == 171)
str(LW_WGC_171.dat)

LW_WGC2Sample_171 <- sample(LW_WGC_171.dat$Whatman.Card..)
LW_WGC2Sample_171_order <- match(LW_WGC2Sample_171, LW_WGC_171.dat$Whatman.Card..)

any(cumsum(LW_WGC_171.dat[LW_WGC2Sample_171_order, "X..Tissues"]) == 302)
max2run_171 <- which(cumsum(LW_WGC_171.dat[LW_WGC2Sample_171_order, "X..Tissues"]) == 302)  # 302 samples from 171
LW_WGC2Run_171 <- LW_WGC2Sample_171[seq(max2run_171)]


#~~~~~~~~~~~~~~~~~~
# 172
# Subsample 42
LW_WGC_172.dat <- subset(LW_WGC.dat, Fishery == "Late Winter Troll"  & Dist.Quad == 172)
str(LW_WGC_172.dat)

LW_WGC2Sample_172 <- sample(LW_WGC_172.dat$Whatman.Card..)
LW_WGC2Sample_172_order <- match(LW_WGC2Sample_172, LW_WGC_172.dat$Whatman.Card..)

any(cumsum(LW_WGC_172.dat[LW_WGC2Sample_172_order, "X..Tissues"]) == 42)
max2run_172 <- which(cumsum(LW_WGC_172.dat[LW_WGC2Sample_172_order, "X..Tissues"]) == 42)  # 42 samples from 172
LW_WGC2Run_172 <- LW_WGC2Sample_172[seq(max2run_172)]

#~~~~~~~~~~~~~~~~~~
# 173
# Run all
LW_WGC_173.dat <- subset(LW_WGC.dat, Fishery == "Late Winter Troll"  & Dist.Quad == 173)
str(LW_WGC_173.dat)
sum(LW_WGC_173.dat$X..Tissues)

LW_WGC2Run_173 <- LW_WGC_173.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~
# 174
# Subsample 95
LW_WGC_174.dat <- subset(LW_WGC.dat, Fishery == "Late Winter Troll"  & Dist.Quad == 174)
str(LW_WGC_174.dat)

LW_WGC2Sample_174 <- sample(LW_WGC_174.dat$Whatman.Card..)
LW_WGC2Sample_174_order <- match(LW_WGC2Sample_174, LW_WGC_174.dat$Whatman.Card..)

any(cumsum(LW_WGC_174.dat[LW_WGC2Sample_174_order, "X..Tissues"]) == 95)
max2run_174 <- which(cumsum(LW_WGC_174.dat[LW_WGC2Sample_174_order, "X..Tissues"]) == 95)  # 95 samples from 174
LW_WGC2Run_174 <- LW_WGC2Sample_174[seq(max2run_174)]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Final extraction list for LW
LW_WGC2Run <- sort(c(LW_WGC2Run_171, LW_WGC2Run_172, LW_WGC2Run_173, LW_WGC2Run_174))

LW_WGC_Run.dat <- LW_WGC.dat[match(LW_WGC2Run, LW_WGC.dat$Whatman.Card..), ]
str(LW_WGC_Run.dat)
sum(LW_WGC_Run.dat$X..Tissues)
aggregate(X..Tissues ~ Dist.Quad, data = LW_WGC_Run.dat, sum)

write.xlsx(x = LW_WGC_Run.dat, file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", 
           sheetName = "LW Extraction Data", append = TRUE, row.names = FALSE)
write.xlsx(x = matrix(data = sapply(LW_WGC_Run.dat$Whatman.Card.., function(WGC) {ifelse(nchar(WGC) == 10, WGC, paste0("000000", WGC))}), 
                      ncol = 1, dimnames = list(seq(LW_WGC2Run), "Whatman Cards to Extract")), 
           file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", sheetName = "For LAB KTROL17LW", append = TRUE, row.names = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Spring 1 ####
#~~~~~~~~~~~~~~~~~~
# 171
# Subsample 222
SP1_WGC.dat <- read.xlsx(file = "Associated Data/MTA Lab Troll Harvest Data.xlsx", sheetName = "SP 1 AY2017", header = TRUE)
str(SP1_WGC.dat)

SP1_WGC_171.dat <- subset(SP1_WGC.dat, Fishery == "Spring Troll"  & Dist.Quad == 171)
str(SP1_WGC_171.dat)

SP1_WGC2Sample_171 <- sample(SP1_WGC_171.dat$Whatman.Card..)
SP1_WGC2Sample_171_order <- match(SP1_WGC2Sample_171, SP1_WGC_171.dat$Whatman.Card..)

any(cumsum(SP1_WGC_171.dat[SP1_WGC2Sample_171_order, "X..Tissues"]) == 222)
max2run_171 <- which(cumsum(SP1_WGC_171.dat[SP1_WGC2Sample_171_order, "X..Tissues"]) == 222)  # 222 samples from 171
SP1_WGC2Run_171 <- SP1_WGC2Sample_171[seq(max2run_171)]

#~~~~~~~~~~~~~~~~~~
# 172
# Run all
SP1_WGC_172.dat <- subset(SP1_WGC.dat, Fishery == "Spring Troll"  & Dist.Quad == 172)
str(SP1_WGC_172.dat)
sum(SP1_WGC_172.dat$X..Tissues)

SP1_WGC2Run_172 <- SP1_WGC_172.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~
# 173
# Run all
SP1_WGC_173.dat <- subset(SP1_WGC.dat, Fishery == "Spring Troll"  & Dist.Quad == 173)
str(SP1_WGC_173.dat)
sum(SP1_WGC_173.dat$X..Tissues)

SP1_WGC2Run_173 <- SP1_WGC_173.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~
# 174
# Run all
SP1_WGC_174.dat <- subset(SP1_WGC.dat, Fishery == "Spring Troll"  & Dist.Quad == 174)
str(SP1_WGC_174.dat)
sum(SP1_WGC_174.dat$X..Tissues)

SP1_WGC2Run_174 <- SP1_WGC_174.dat$Whatman.Card..

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Final extraction list for SP1
SP1_WGC2Run <- sort(c(SP1_WGC2Run_171, SP1_WGC2Run_172, SP1_WGC2Run_173, SP1_WGC2Run_174))

SP1_WGC_Run.dat <- SP1_WGC.dat[match(SP1_WGC2Run, SP1_WGC.dat$Whatman.Card..), ]
str(SP1_WGC_Run.dat)
sum(SP1_WGC_Run.dat$X..Tissues)
aggregate(X..Tissues ~ Dist.Quad, data = SP1_WGC_Run.dat, sum)

write.xlsx(x = SP1_WGC_Run.dat, file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", 
           sheetName = "SP1 Extraction Data", append = TRUE, row.names = FALSE)
write.xlsx(x = matrix(data = sapply(SP1_WGC_Run.dat$Whatman.Card.., function(WGC) {ifelse(nchar(WGC) == 10, WGC, paste0("000000", WGC))}), 
                      ncol = 1, dimnames = list(seq(SP1_WGC2Run), "Whatman Cards to Extract")), 
           file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", sheetName = "For LAB KTROL17SP1", append = TRUE, row.names = FALSE)



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Spring 2 ####
#~~~~~~~~~~~~~~~~~~
# Run all
SP2_WGC.dat <- read.xlsx(file = "Associated Data/MTA Lab Troll Harvest Data.xlsx", sheetName = "SP 2 AY2017", header = TRUE)
str(SP2_WGC.dat)
sum(SP2_WGC.dat$X..Tissues)
aggregate(X..Tissues ~ Dist.Quad, data = SP2_WGC.dat, sum)

SP2_WGC_Run.dat <- SP2_WGC.dat[match(sort(SP2_WGC.dat$Whatman.Card..), SP2_WGC.dat$Whatman.Card..), ]


write.xlsx(x = SP2_WGC_Run.dat, file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", 
           sheetName = "SP2 Extraction Data", append = TRUE, row.names = FALSE)
write.xlsx(x = matrix(data = sapply(SP2_WGC_Run.dat$Whatman.Card.., function(WGC) {ifelse(nchar(WGC) == 10, WGC, paste0("000000", WGC))}), 
                      ncol = 1, dimnames = list(seq_along(SP2_WGC_Run.dat$Whatman.Card..), "Whatman Cards to Extract")), 
           file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", sheetName = "For LAB KTROL17SP2", append = TRUE, row.names = FALSE)

# Combine SP1 and SP2
sort(c(SP1_WGC2Run, SP2_WGC_Run.dat$Whatman.Card..))

write.xlsx(x = matrix(data = sapply(sort(c(SP1_WGC2Run, SP2_WGC_Run.dat$Whatman.Card..)), function(WGC) {ifelse(nchar(WGC) == 10, WGC, paste0("000000", WGC))}), 
                      ncol = 1, dimnames = list(seq_along(sort(c(SP1_WGC2Run, SP2_WGC_Run.dat$Whatman.Card..))), "Whatman Cards to Extract")), 
           file = "Extraction Lists/K119 Winter Spring Troll Extraction.xlsx", sheetName = "For LAB KTROL17SP", append = TRUE, row.names = FALSE)





# save.image("Extraction Lists/K119ExtractionLists.RData")



#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Resolving extraction list issues
# Wed Jul 19 11:14:21 2017

## Resolving LW missing WGC issue
# What Iris sent
LW_WGC.dat$Whatman.Card..

# What is in LOKI
LW_WGC_LOKI <- as.numeric(readClipboard())

setdiff(LW_WGC.dat$Whatman.Card.., LW_WGC_LOKI)  # 8959 is missing from LOKI, but I wanted it extracted
setdiff(LW_WGC_LOKI, LW_WGC.dat$Whatman.Card..)  # 7859 is in LOKI but not on Iris' sheet, run that card

## Resolving EW missing fish issue
# Fish 1 on WGC 6248 is missing
sort(EW_WGC2Run_171)

sort(setdiff(EW_WGC2Sample_171, EW_WGC2Run_171))

# Use 6240 fish #1
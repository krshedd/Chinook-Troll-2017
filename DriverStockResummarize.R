# Randy needs 2009-2017 26RG estimates for CRG and SIT sport + NO troll (winter and summer fisheries only)
# Fri Feb 09 15:50:30 2018
date()

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
#### CRG and SIT Sport ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2017
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
Sport_CRG_SIT_2017_26RG_Estimates <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", mixvec = c("CRGSport_2017", "SITSport_2017"),
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2016
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK16")
Sport_CRG_SIT_2016_26RG_Estimates <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec33RG_to26RG, groupnames = GroupNames26, maindir = "BAYES/Output/33RG", mixvec = c("CRGSport_2016", "SITSport_2016"),
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2015
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK15")
Sport_CRG_SIT_2015_26RG_Estimates <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", mixvec = c("CRGSport_2015", "SITSport_2015"),
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2014
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK14")
Sport_CRG_SIT_2014_26RG_Estimates <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output/26RG", mixvec = c("CRGSport.2014", "SITSport.2014"),
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2013
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK13")
Sport_CRG_SIT_2013_26RG_Estimates <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", mixvec = c("CRGSport.2013", "SITSport.2013"),
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2012
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK12")
Sport_CRG_SIT_2012_26RG_Estimates <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output/356 pop baseline", mixvec = c("CRGSport.2012", "SITSport.2012"),
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2011
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK11")
Sport_SIT_2011_26RG_Estimates <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/SportSitka/Output/KSPOR11S_26RG/", mixvec = c("KSPOR11S"),
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)



## CRG and SIT specific mixtures only exist for 2012-2017
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Concatenate
Sport_CRG_SIT_2012_2017_26RG_Estimates <- do.call(c, lapply(2012:2017, function(yr) {get(paste0("Sport_CRG_SIT_", yr, "_26RG_Estimates"))}))
names(Sport_CRG_SIT_2012_2017_26RG_Estimates) <- gsub(pattern = "\\.", replacement = "_", x = names(Sport_CRG_SIT_2012_2017_26RG_Estimates))
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
dput(x = Sport_CRG_SIT_2012_2017_26RG_Estimates, file = "Estimates objects/Sport_CRG_SIT_2012_2017_26RG_Estimates.txt")

dput(x = Sport_SIT_2011_26RG_Estimates, file = "Estimates objects/Sport_SIT_2011_26RG_Estimates.txt")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### NO and SO Troll ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## 2017
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
NO_mixtures <- c("EWintNO_2017", "LWintNO_2017", "SummerRet1NO_2017")
Troll_NO_2017_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output", mixvec = NO_mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2016
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK16")
NO_mixtures <- c("EWintNO_2016", "LWintNO_2016", "Summer1NO_2016", "Summer2NO_2016")
Troll_NO_2016_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = GroupVec33RG_to26RG, groupnames = GroupNames26, maindir = "BAYES/Output/33RG/AllYearTroll_2016", mixvec = NO_mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2015
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK15")
NO_mixtures <- c("EWintNO_2015", "LWintNO_2015", "SumRet1NO_2015")
Troll_NO_2015_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output/AllYearTroll_2015", mixvec = NO_mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2014
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK14")
NO_mixtures <- c("EWintNO.2014", "LWintNO.2014", "SumRet1NO.2014", "SumRet2NO.2014")
Troll_NO_2014_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = c(1:2, 3, 3, 4:26), groupnames = GroupNames26, maindir = "BAYES/Output/AllYearTroll_2014", mixvec = NO_mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2013
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK13")
NO_mixtures <- c("EWintNO.2013", "LWintNO.2013", "SumRet1NO.2013")
Troll_NO_2013_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output/AllYearTroll_2013", mixvec = NO_mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2012
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK12")
NO_mixtures <- c("EWintNO.2012", "LWintNO.2012", "SumRet1NO.2012", "SumRet2NO.2012")
Troll_NO_2012_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Output/AllYearTroll_2012", mixvec = NO_mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2011
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK11")
NO_mixtures <- c("EWintNO.2011", "LWintNO.2011", "SumRet1NO.2011", "SumRet2NO.2011")
Troll_NO_2011_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Troll/Output/AllYearTroll_2011", mixvec = NO_mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2010
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK10")
NO_mixtures <- c("EarlyWinterNO.2010", "LateWinterNO.2010", "SummerR1NO.2010", "SummerR2NO.2010")
Troll_NO_2010_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Troll/Output/AllYearTroll_2010", mixvec = NO_mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

## 2009
# Fails because Summer1 and 2 are 25RG
setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK09")
NO_mixtures <- c("EarlyWinterNO", "LateWinterNO", "SummerR1NO", "SummerR2NO")
Troll_NO_2009_26RG_EstimatesStats <- 
  CustomCombineBAYESOutput.GCL(groupvec = 1:26, groupnames = GroupNames26, maindir = "BAYES/Troll/Output/AllYearTroll_2009", mixvec = NO_mixtures,
                               prior = "", ext = "RGN", nchains = 5, burn = 0.5, alpha = 0.1, PosteriorOutput = FALSE)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Concatenate
Troll_NO_2010_2017_26RG_Estimates <- do.call(c, lapply(2010:2017, function(yr) {get(paste0("Troll_NO_", yr, "_26RG_EstimatesStats"))}))
names(Troll_NO_2010_2017_26RG_Estimates)
names(Troll_NO_2010_2017_26RG_Estimates) <- gsub(pattern = "\\.", replacement = "_", x = names(Troll_NO_2010_2017_26RG_Estimates))
names(Troll_NO_2010_2017_26RG_Estimates) <- gsub(pattern = "EarlyWinter", replacement = "EWint", x = names(Troll_NO_2010_2017_26RG_Estimates))
names(Troll_NO_2010_2017_26RG_Estimates) <- gsub(pattern = "LateWinter", replacement = "LWint", x = names(Troll_NO_2010_2017_26RG_Estimates))
names(Troll_NO_2010_2017_26RG_Estimates) <- gsub(pattern = "SummerRet", replacement = "SumRet", x = names(Troll_NO_2010_2017_26RG_Estimates))
names(Troll_NO_2010_2017_26RG_Estimates) <- gsub(pattern = "SummerR", replacement = "SumRet", x = names(Troll_NO_2010_2017_26RG_Estimates))
names(Troll_NO_2010_2017_26RG_Estimates) <- gsub(pattern = "Summer", replacement = "SumRet", x = names(Troll_NO_2010_2017_26RG_Estimates))

setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17")
dput(x = Troll_NO_2010_2017_26RG_Estimates, file = "Estimates objects/Troll_NO_2010_2017_26RG_Estimates.txt")

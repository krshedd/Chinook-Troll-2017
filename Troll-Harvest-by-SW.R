setwd("V:/Analysis/1_SEAK/Chinook/Mixture/SEAK17/")
rm(list = ls())

require(xlsx)
library(tidyverse)
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Harvest Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in harvest data
troll_harvest.df <- read.xlsx(file = "CE001401.xlsx", sheetName = "CE001401", startRow = 23, header = TRUE)
str(troll_harvest.df)
troll_harvest.df <- troll_harvest.df[!is.na(troll_harvest.df$N.Catch), ]


## Manipulate harvest data
# Year as factor
troll_harvest.df$Year <- factor(troll_harvest.df$Year)

# Variable for fishery
t(troll_harvest.df %>% 
    filter(Year == "2009", Harvest == "TRAD", Time.Value >= 26, Time.Value <= 37) %>% 
    select(Time.Value) %>% 
    unique())

troll_harvest.df$Fishery <- NA
troll_harvest.df$Fishery[troll_harvest.df$Harvest == "SP TROLL"] <- "Spring"
troll_harvest.df$Fishery[troll_harvest.df$Harvest == "TRAD"
                         & troll_harvest.df$Time.Value <= 18] <- "Late Winter"
troll_harvest.df$Fishery[troll_harvest.df$Harvest == "TRAD"
                         & troll_harvest.df$Time.Value >= 41] <- "Early Winter"
troll_harvest.df$Fishery[troll_harvest.df$Harvest == "TRAD"
                         & troll_harvest.df$Time.Value >= 26
                         & troll_harvest.df$Time.Value <= 31] <- "Summer Ret 1"
troll_harvest.df$Fishery[troll_harvest.df$Harvest == "TRAD"
                         & troll_harvest.df$Time.Value >= 32
                         & troll_harvest.df$Time.Value <= 36] <- "Summer Ret 2"


troll_harvest.df$Fishery <- factor(troll_harvest.df$Fishery, levels = c("Late Winter", "Spring", "Summer Ret 1", "Summer Ret 2", "Early Winter"))

troll_harvest.df$N.Catch[is.na(troll_harvest.df$Fishery)]

# Accounting Year
# troll_harvest.df$AY <- as.numeric(as.character(troll_harvest.df$Year))
# troll_harvest.df$AY[troll_harvest.df$Fishery == "Early Winter"] <- troll_harvest.df$AY[troll_harvest.df$Fishery == "Early Winter"] + 1
# troll_harvest.df$AY <- factor(troll_harvest.df$AY)



# Harvest by Year and SW (all quad)
# Calendar Year
troll_harvest.df %>% 
  # filter(Area.Value == "NW") %>% 
  filter(Fishery %in% c("Late Winter", "Spring", "Early Winter")) %>% 
  filter(Year %in% as.character(2009:2017)) %>% 
  select(Year, Time.Value, Fishery, N.Catch) %>% 
  group_by(Year, Time.Value, Fishery) %>% 
  summarize(Harvest = sum(N.Catch)) %>% 
  # spread(Time.Value, Harvest)
  ggplot(aes(Time.Value, Harvest, color = Fishery)) +
  geom_col() +
  facet_wrap(~ Year) +
  xlab("Stat Week")

# Accounting Year
# troll_harvest.df %>% 
#   # filter(Area.Value == "NW") %>% 
#   filter(Fishery %in% c("Late Winter", "Spring", "Early Winter")) %>% 
#   select(AY, Time.Value, Fishery, N.Catch) %>% 
#   group_by(AY, Time.Value, Fishery) %>% 
#   summarize(Harvest = sum(N.Catch)) %>% 
#   # spread(Time.Value, Harvest)
#   ggplot(aes(Time.Value, Harvest, color = Fishery)) +
#   geom_col() +
#   facet_wrap(~ AY) +
#   xlab("Stat Week")

# Harvest by Year and SW and Fishery (all quad)
troll_harvest_SW_Year_fishery.f <- function(fishery) {
  troll_harvest.df %>% 
    filter(Fishery == fishery) %>% 
    select(Year, Time.Value, N.Catch) %>% 
    group_by(Year, Time.Value) %>% 
    summarize(Harvest = sum(N.Catch)) %>% 
    # spread(Time.Value, Harvest)
    ggplot(aes(Time.Value, Harvest)) +
    geom_col() +
    facet_wrap(~ Year) +
    xlab("Stat Week")
}

troll_harvest_SW_Year_fishery.f(fishery = "Late Winter")
troll_harvest_SW_Year_fishery.f(fishery = "Spring")
troll_harvest_SW_Year_fishery.f(fishery = "Summer Ret 1")
troll_harvest_SW_Year_fishery.f(fishery = "Summer Ret 2")
troll_harvest_SW_Year_fishery.f(fishery = "Early Winter")


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### ASL Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Read in ASL data
troll_ASL.df <- read.csv(file = "Harvest - Detailed ASL Samples.csv")
str(troll_ASL.df)

## Manipulate harvest data
# Year as factor
troll_ASL.df$Year <- factor(troll_ASL.df$ï..Year)

# Variable for fishery
t(troll_ASL.df %>% 
    filter(Year == "2009", Harvest == "Traditional State Managed Fisheries", Stat.Week >= 26, Stat.Week <= 37) %>% 
    select(Stat.Week) %>% 
    unique())

troll_ASL.df$Fishery <- NA
troll_ASL.df$Fishery[troll_ASL.df$Harvest == "Spring Troll Fishery"] <- "Spring"
troll_ASL.df$Fishery[troll_ASL.df$Harvest == "Traditional State Managed Fisheries"
                         & troll_ASL.df$Stat.Week <= 18] <- "Late Winter"
troll_ASL.df$Fishery[troll_ASL.df$Harvest == "Traditional State Managed Fisheries"
                         & troll_ASL.df$Stat.Week >= 41] <- "Early Winter"
troll_ASL.df$Fishery[troll_ASL.df$Harvest == "Traditional State Managed Fisheries"
                         & troll_ASL.df$Stat.Week >= 26
                         & troll_ASL.df$Stat.Week <= 31] <- "Summer Ret 1"
troll_ASL.df$Fishery[troll_ASL.df$Harvest == "Traditional State Managed Fisheries"
                         & troll_ASL.df$Stat.Week >= 32
                         & troll_ASL.df$Stat.Week <= 36] <- "Summer Ret 2"


troll_ASL.df$Fishery <- factor(troll_ASL.df$Fishery, levels = c("Late Winter", "Spring", "Summer Ret 1", "Summer Ret 2", "Early Winter"))


troll_ASL.df %>% 
  filter(Fishery %in% c("Late Winter", "Spring", "Early Winter")) %>% 
  filter(Year%in% as.character(2009:2017)) %>% 
  select(Year, Stat.Week, Fishery, Dna.Specimen.No) %>% 
  group_by(Year, Stat.Week, Fishery) %>% 
  summarise(Samples = sum(!is.na(Dna.Specimen.No))) %>% 
  ggplot(aes(Stat.Week, Samples, color = Fishery)) +
  geom_col() +
  facet_wrap(~ Year) +
  xlab("Stat Week")
  

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#### Join Data ####
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
troll_harvest_SW.df <- troll_harvest.df %>% 
  # filter(Area.Value == "NW") %>% 
  # filter(Fishery %in% c("Late Winter", "Spring", "Early Winter")) %>% 
  filter(Year %in% as.character(2009:2017)) %>% 
  select(Year, Time.Value, Fishery, N.Catch) %>% 
  group_by(Year, Time.Value, Fishery) %>% 
  summarize(Harvest = sum(N.Catch))
str(troll_harvest_SW.df)

troll_ASL_SW.df <- troll_ASL.df %>% 
  # filter(Fishery %in% c("Late Winter", "Spring", "Early Winter")) %>% 
  filter(Year%in% as.character(2009:2017)) %>% 
  select(Year, Stat.Week, Fishery, Dna.Specimen.No) %>% 
  group_by(Year, Stat.Week, Fishery) %>% 
  summarise(Samples = sum(!is.na(Dna.Specimen.No)))
str(troll_ASL_SW.df)

## Full Join
troll_ASL_harvest_SW.df <- full_join(x = troll_ASL_SW.df, y = troll_harvest_SW.df, 
                                  by = c("Year" = "Year", "Stat.Week" = "Time.Value", "Fishery" = "Fishery"))
str(troll_ASL_harvest_SW.df)

# Summarise total harvest/samples per fishery per year
troll_ASL_harvest_SW.df %>% 
  filter(Fishery %in% c("Late Winter", "Spring", "Early Winter")) %>% 
  group_by(Year, Fishery) %>% 
  mutate(freq_samp = Samples / sum(Samples, na.rm = TRUE) * 100) %>% 
  mutate(freq_harvest = Harvest / sum(Harvest, na.rm = TRUE) * 100) %>% 
  ggplot(aes(Stat.Week, freq_samp, color = Fishery)) +
  geom_col() +
  facet_wrap(~ Year) +
  xlab("Stat Week") +
  ylab("Fishery Samples %")

# Plot


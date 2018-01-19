setwd("C:/Users/krshedd/Documents/r/SEAK17/")
rm(list = ls())

require(xlsx)
require(reshape2)
require(dplyr)
require(tidyr)
require(ggplot2)

## Read in harvest data
troll_harvest.df <- read.xlsx(file = "CE001376.xlsx", sheetName = "CE001376", startRow = 23, header = TRUE)
str(troll_harvest.df)
troll_harvest.df <- troll_harvest.df[!is.na(troll_harvest.df$N.Catch), ]


## Manipulate harvest data
# Year as factor
troll_harvest.df$Year <- factor(troll_harvest.df$Year)

# Variable for fishery
t(troll_harvest.df %>% 
    filter(Year == "2010", Harvest == "TRAD", Time.Value >= 26, Time.Value <= 37) %>% 
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




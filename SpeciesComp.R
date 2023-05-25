### Species Comp Data ###

library(tidyverse)
library(readxl)


#### Reading Data In####

PBG_Data <- read.csv("PBG011.csv") #Read in our data
sp_list <- read_excel("sp_list.xlsx")

#### Filtering for year 2022 ####

PBG_Data_2022 <- PBG_Data %>% filter(PBG_Data$RecYear== "2022") #Filtering for 2022 data only


#### Adding Midpoint values to the dataset

MidPoints <- c(0.5, 3.0, 15.0, 37.5, 62.5, 85, 97.5)


#Uses the MidPoints data set add a new column called "MidPoint" based on our CoverClass column values. 
 
PBG_Data_MidPoints <- PBG_Data_2022 %>%
  mutate(MidPoint = ifelse(grepl("^1$", CoverClass), MidPoints[1],
                           ifelse(grepl("^2$", CoverClass), MidPoints[2],
                                  ifelse(grepl("^3$", CoverClass), MidPoints[3],
                                         ifelse(grepl("^4$", CoverClass), MidPoints[4],
                                                ifelse(grepl("^5$", CoverClass), MidPoints[5],
                                                       ifelse(grepl("^6$", CoverClass), MidPoints[6],
                                                              ifelse(grepl("^7$", CoverClass), MidPoints[7], NA))))))))


#### Make a column for each species that identifies the average cover across all plots ####

species_avg <- PBG_Data_MidPoints %>%
  group_by(SpeCode) %>%
  summarise(AvgCover = mean(MidPoint)) %>%
  ungroup()


#### Make a column for each species that identifies the frequency across all plots ####


species_freq <- PBG_Data_MidPoints %>%
  group_by(SpeCode) %>%
  summarise(Frequency = n_distinct(Plot) / n_distinct(PBG_Data_MidPoints$Plot)) %>%
  ungroup()


#### Join species_avg and species_freq into the PBG_Joined dataframe ####


PBG_Joined <- left_join(species_avg, species_freq, by = "SpeCode")


#### Calculating Dominance Candidate Index (DCi). ####

# DCi = (average relative abundance + relative frequency)/2


# Calculate DCi
PBG_DCI <- PBG_Joined %>%
  mutate(DCi = (AvgCover + Frequency) / 2)


#### Joining sp_list for final file ####

sp_list2 <- sp_list %>% rename_at('code', ~'SpeCode') #updating file for join


PBG_Final_Comp <- left_join(PBG_DCI, sp_list2, by = "SpeCode")

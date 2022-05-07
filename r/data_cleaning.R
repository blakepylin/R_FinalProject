library(tidyverse)

shoe_data <- read.csv("shoe_data.csv")
shoe_characteristics = read.csv("shoe_characteristics.csv")
ncaa_teams = read.csv("ncaa.csv")

shoe_data$sale_price = as.numeric(gsub("[\\$,]", "", shoe_data$sale_price))
shoe_data$retail_price = as.numeric(gsub("[\\$,]", "", shoe_data$retail_price))

shoe_data = shoe_data %>% 
  drop_na(sale_price)

# drop teams that no longer exist as of 2020
ncaa_teams = ncaa_teams %>%
  filter(To >= 2019 & From <= 2019) 

# aggregate teams by state
df = data.frame(
ncaa_teams %>%
  group_by(State) %>%
  summarise(across(c("Overall_Win_Loss_Percentage", "Win_Percentage_2019","Win_Percentage_2018", "Yrs", "AP_Rank_2019", "AP_Ranked_2018"), ~ mean(.x, na.rm = TRUE)))
,
  ncaa_teams %>%
  group_by(State) %>%
  summarise(across(c("NCAA_Tournament_Appearances", "Final_Four_Appearances","NCAA_Championships", "AP_Final_Poll_Appearances" ), ~ sum(.x, na.rm = TRUE)))
)



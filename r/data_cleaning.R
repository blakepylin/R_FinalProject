library(tidyverse)
library(lubridate)
library(zoo)

monthly_retail_df <- read_csv("data/monthly_retail.csv")
monthly_retail_df1 <- read_csv("data/monthly_retail.csv")
sp_index_df <- read_csv("data/sp_index_df.csv")
state_pop_income <- read_csv("data/state_pop_income.csv")
shoe_data <- read_csv("data/shoe_data.csv")


# put on time stamp for every data sets
shoe_data = shoe_data %>% 
  mutate(time_stamp=mdy(shoe_data$order_date), .before=order_date)
shoe_data$year = year(shoe_data$time_stamp)

#drop commas and dollar signs
shoe_data$sale_price = as.numeric(gsub("[\\$,]", "", shoe_data$sale_price))
shoe_data$retail_price = as.numeric(gsub("[\\$,]", "", shoe_data$retail_price))
state_pop_income$disposable_per_cap_income = as.numeric(gsub("[\\$,]", "", state_pop_income$disposable_per_cap_income))


#sporting good index
monthly_retail_df=monthly_retail_df %>% 
  mutate(time_stamp=mdy(Period), .before = Period)
monthly_retail_df = monthly_retail_df[ ,-(2)]

monthly_retail_df = monthly_retail_df%>%
  group_by(time_stamp) %>%
  expand(time_stamp = seq(floor_date(time_stamp, unit = "month"),
                          ceiling_date(time_stamp, unit="month")-days(1), by="day"), sporting_goods) %>% as.data.frame()

# retail monthly index
monthly_retail_df1=monthly_retail_df1 %>% 
  mutate(time_stamp=mdy(Period), .before = Period)
monthly_retail_df1 = monthly_retail_df1[ ,-(2)]

monthly_retail_df1 = monthly_retail_df1%>%
  group_by(time_stamp) %>%
  expand(time_stamp = seq(floor_date(time_stamp, unit = "month"),
                          ceiling_date(time_stamp, unit="month")-days(1), by="day"), monthly_retail) %>% as.data.frame()


#sp index
sp_index_df = sp_index_df[ ,-(3:10)]

sp_index_df = sp_index_df%>% 
  mutate(time_stamp=mdy(date), .before= date)

sp_index_df = sp_index_df[ ,-(2)]

# merge 
shoe_data = shoe_data %>%
  left_join(monthly_retail_df, by='time_stamp') %>%
  left_join(monthly_retail_df1, by='time_stamp')%>%
  left_join(sp_index_df, by='time_stamp')

shoe_data = na.locf(shoe_data)

#team performance stuff
ncaa_teams = read.csv("data/ncaa.csv")
ncaa_teams = ncaa_teams %>%
  filter(To >= 2019 & From <= 2019) 

# aggregate teams by state
agg_ncaa = data.frame(
  ncaa_teams %>%
    group_by(State) %>%
    summarise(across(c("Overall_Win_Loss_Percentage", "Win_Percentage_2019","Win_Percentage_2018", "Yrs", "AP_Rank_2019", "AP_Ranked_2018"), ~ mean(.x, na.rm = TRUE)))
  ,
  ncaa_teams %>%
    group_by(State) %>%
    summarise(across(c("NCAA_Tournament_Appearances", "Final_Four_Appearances","NCAA_Championships", "AP_Final_Poll_Appearances"), ~ sum(.x, na.rm = TRUE)))
)

shoe_data = merge(shoe_data, agg_ncaa, by.x = "buyer_region", by.y="State", all.x = TRUE)

#shoe characteristics
shoe_characteristics = read.csv("data/shoe_characteristics.csv")

shoe_data = merge(shoe_data, shoe_characteristics, by.x = "sneaker_name", by.y="Shoe", all.x = TRUE)


# state population and income
shoe_data = merge(shoe_data, state_pop_income, by.x = c("buyer_region","year"), by.y = c("buyer_region","year"), all.x = TRUE)
shoe_data$Year_month <- format(as.Date(shoe_data$time_stamp), "%Y-%m")


write.csv(shoe_data, "data/shoe_final.csv")


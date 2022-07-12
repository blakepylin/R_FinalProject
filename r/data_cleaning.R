library(tidyverse)
library(lubridate)
library(zoo)

shoe_data = read_csv("data/shoe_data.csv", show_col_types = FALSE)
monthly_retail_df = read_csv("data/monthly_retail.csv",show_col_types = FALSE)
sp_index_df = read_csv("data/sp_index_df.csv",show_col_types = FALSE)
state_pop_income = read_csv("data/state_pop_income.csv",show_col_types = FALSE)
ncaa_teams = read.csv("data/ncaa.csv")
shoe_characteristics = read.csv("data/shoe_characteristics.csv")

#reformat dates, columns, misc
shoe_data = shoe_data %>% 
  mutate(order_date=mdy(shoe_data$order_date), .before=order_date) %>%
  mutate(release_date=mdy(shoe_data$release_date), .before=release_date)
shoe_data$order_year = year(shoe_data$order_date)
shoe_data$Year_month <- format(as.Date(shoe_data$order_date), "%Y-%m") # wanted this column to use later

sp_index_df = sp_index_df %>%
  mutate(date=mdy(sp_index_df$date), .before=date) %>%
  select(date, sp_index)

monthly_retail_df = monthly_retail_df %>%
  mutate(period=mdy(monthly_retail_df$Period), .before=Period) %>%
  select(period, sporting_goods, monthly_retail)

state_pop_income = state_pop_income %>%
  mutate(state_pop_year = State_pop_year*1000000) %>%
  select(year, buyer_region, state_pop_year, disposable_per_cap_income)

#drop commas and dollar signs
shoe_data$sale_price = as.numeric(gsub("[\\$,]", "", shoe_data$sale_price))
shoe_data$retail_price = as.numeric(gsub("[\\$,]", "", shoe_data$retail_price))
state_pop_income$disposable_per_cap_income = as.numeric(gsub("[\\$,]", "", state_pop_income$disposable_per_cap_income))


##### merging

# for monthly indexes we have to fill in the dates
monthly_retail_df_test = monthly_retail_df

monthly_retail_df = monthly_retail_df %>%
  mutate(floor = floor_date(monthly_retail_df$period, "month")) %>%
  mutate(ceiling = ceiling_date(monthly_retail_df$period, "month") - days(1))

monthly_retail_df = monthly_retail_df %>%
  rowwise() %>%
  do(data.frame(monthly_retail = .$monthly_retail, dates = seq(.$floor, .$ceiling, by = 1), sporting_goods = .$sporting_goods, date = seq(.$floor, .$ceiling, by = 1))) %>%
  select(dates, monthly_retail, sporting_goods)

#team performance stuff
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
agg_ncaa$AP_Ranked_2018[is.na(agg_ncaa$AP_Ranked_2018)] = 0


# merge everything
shoe_data = shoe_data %>%
  left_join(monthly_retail_df, by=c("order_date" = "dates")) %>%
  left_join(sp_index_df, by=c("order_date" = "date")) %>%
  left_join(agg_ncaa, by=c("buyer_region" = "State")) %>%
  left_join(shoe_characteristics, by=c("sneaker_name" = "Shoe")) %>%
  left_join(state_pop_income, by=c("buyer_region", "order_year" = "year")) %>%
  fill(sp_index) %>%
  select(-ends_with(".1"))

shoe_data = shoe_data %>%
  mutate(premium = sale_price - retail_price) %>%
  mutate(relative_premium = (sale_price - retail_price)/retail_price)



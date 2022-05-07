library(tidyverse)
library(lubridate)
library(zoo)
library(knitr)
library(usmap)

shoe_data <- read.csv("data/shoe_data.csv")
shoe_data_with_pop <- read.csv("data/shoe_data_with_pop.csv")

##Cleaning stuff
shoe_data$sale_price = as.numeric(gsub("[\\$,]", "", shoe_data$sale_price))
shoe_data$retail_price = as.numeric(gsub("[\\$,]", "", shoe_data$retail_price))
shoe_data_with_pop$disposable_per_cap_income = as.numeric(gsub("[\\$,]", "", shoe_data_with_pop$disposable_per_cap_income))

shoe_data = shoe_data %>% 
  mutate(time_stamp=mdy(shoe_data$order_date), .before=order_date)

shoe_data = shoe_data %>% 
  mutate(time_stamp2=mdy(shoe_data$release_date), .before=release_date)

shoe_data$Year_month <- format(as.Date(shoe_data$time_stamp), "%Y-%m")


## Graphs for order volume and sales dollars
sales_dollars = shoe_data %>%
  group_by(brand, Year_month) %>%
  summarise(monthly_sales = sum(sale_price))
sales_dollars = sales_dollars %>%
  group_by(brand) %>%
  mutate(total_sales = cumsum(monthly_sales))

ggplot(sales_dollars) +
  geom_line(aes(x=Year_month,y=monthly_sales, group = brand, color = brand))

ggplot(sales_dollars) +
  geom_line(aes(x=Year_month,y=total_sales, group = brand, color = brand))


sales_vol = shoe_data %>%
  group_by(brand, Year_month) %>%
  summarise(monthly_orders = n())
sales_vol = sales_vol %>%
  group_by(brand) %>%
  mutate(total_orders = cumsum(monthly_orders))

ggplot(sales_vol) +
  geom_line(aes(x=Year_month,y=monthly_orders, group = brand, color = brand))

ggplot(sales_vol) +
  geom_line(aes(x=Year_month,y=total_orders, group = brand, color = brand))


## how has resale premium changed over time by brand
shoe_data$resale_premium = shoe_data$sale_price - shoe_data$retail_price
shoe_data$resale_premium_percent = 100*(shoe_data$resale_premium / shoe_data$retail_price)

brand_premium = shoe_data %>%
  group_by(brand, Year_month) %>%
  summarise(average_premium = mean(resale_premium_percent))

ggplot(brand_premium) +
  geom_line(aes(x=Year_month,y=average_premium, group = brand, color = brand))
## sneakers brands dont exactly age like wine.

# sneaker_premium = shoe_data %>%
#   group_by(sneaker_name, Year_month) %>%
#   summarise(average_premium = mean(resale_premium))





#### Let's look at regional data ###
resale_prem = shoe_data %>%
  group_by(year = year(time_stamp), buyer_region) %>%
  summarise(avg_resale_premium = mean(resale_premium_percent)) %>%
  select(state = buyer_region, year, avg_resale_premium)

total_order_count = shoe_data %>%
  group_by(year = year(time_stamp), buyer_region) %>%
  summarise(total_order_count = n()) %>%
  select(state = buyer_region, year, total_order_count)

state_pop_income = distinct(shoe_data_with_pop %>%
                              group_by(buyer_region, year) %>%
                              select(year, buyer_region, State_pop_year, disposable_per_cap_income) %>%
                              arrange(buyer_region, year))

state_year_aggs = merge(merge(resale_prem, total_order_count, by = c("state", "year")), state_pop_income, by.x = c("state","year"), by.y=c("buyer_region","year"), all.x = TRUE)

############ lets make some maps ############
  
## resale premium
map_1 = plot_usmap(data = state_year_aggs %>% filter(year=="2017"), values = "avg_resale_premium", color = "red") + 
  scale_fill_continuous(low = "white", high = "orange", name = "Average Resale Premium", label = scales::comma) + 
  theme(legend.position = "right") + labs(title = "2017 Average Resale Premium by State")

map_2 = plot_usmap(data = state_year_aggs %>% filter(year=="2018"), values = "avg_resale_premium", color = "red") + 
  scale_fill_continuous(low = "white", high = "orange", name = "Average Resale Premium", label = scales::comma) + 
  theme(legend.position = "right") + labs(title = "2018 Average Resale Premium by State")

map_3 = plot_usmap(data = state_year_aggs %>% filter(year=="2019"), values = "avg_resale_premium", color = "red") + 
  scale_fill_continuous(low = "white", high = "orange", name = "Average Resale Premium", label = scales::comma) + 
  theme(legend.position = "right") + labs(title = "2019 Average Resale Premium by State")

map_1
map_2
map_3

## total orders

#column is in millions so we gotta * 1000000
state_year_aggs$State_pop_year = state_year_aggs$State_pop_year*1000000
state_year_aggs$orders_per_capita = state_year_aggs$total_order_count / state_year_aggs$State_pop_year
state_year_aggs$orders_per_10000 = 10000*(state_year_aggs$total_order_count / state_year_aggs$State_pop_year)

map_4 = plot_usmap(data = state_year_aggs %>% filter(year=="2017"), values = "orders_per_10000", color = "red") + 
  scale_fill_continuous(low = "white", high = "orange", name = "Orders", label = scales::comma) + 
  theme(legend.position = "right") + labs(title = "2017 Total Order Count per 10000")

map_5 = plot_usmap(data = state_year_aggs %>% filter(year=="2018"), values = "orders_per_10000", color = "red") + 
  scale_fill_continuous(low = "white", high = "orange", name = "Orders", label = scales::comma) + 
  theme(legend.position = "right") + labs(title = "2018 Total Order Count per 10000")

map_6 = plot_usmap(data = state_year_aggs %>% filter(year=="2019"), values = "orders_per_10000", color = "red") + 
  scale_fill_continuous(low = "white", high = "orange", name = "Orders", label = scales::comma) + 
  theme(legend.position = "right") + labs(title = "2019 Total Order Count per 10000")

map_4
map_5
map_6 #oregon emerges as a major sneaker buyer market



## let's look at correlation between time between release and order and resale price
shoe_data$time_since_release = as.numeric(shoe_data$time_stamp) - as.numeric(shoe_data$time_stamp2)
x = data.frame(shoe_data$shoe_size, shoe_data$resale_premium_percent, shoe_data$time_since_release)
kable(cor(x))

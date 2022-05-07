library(tidyverse)
library(lubridate)
library(zoo)
library(knitr)
library(ggmap)

shoe_data <- read.csv("data/shoe_data.csv")

##Cleaning stuff
shoe_data$sale_price = as.numeric(gsub("[\\$,]", "", shoe_data$sale_price))
shoe_data$retail_price = as.numeric(gsub("[\\$,]", "", shoe_data$retail_price))

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

brand_premium = shoe_data %>%
  group_by(brand, Year_month) %>%
  summarise(average_premium = mean(resale_premium))

ggplot(brand_premium) +
  geom_line(aes(x=Year_month,y=average_premium, group = brand, color = brand))
## sneakers brands dont exactly age like wine.

# sneaker_premium = shoe_data %>%
#   group_by(sneaker_name, Year_month) %>%
#   summarise(average_premium = mean(resale_premium))

## let's look at correlation between time between release and order and resale price
shoe_data$time_since_release = as.numeric(shoe_data$time_stamp) - as.numeric(shoe_data$time_stamp2)
x = data.frame(shoe_data$shoe_size, shoe_data$resale_premium, shoe_data$time_since_release)
kable(cor(x))

## let's look at maps

mainland_usa <- c(left = -125, bottom = 25, right = -65, top = 50)
us <- get_stamenmap(bbox = mainland_usa, maptype = "toner-lite", zoom = 5)
ggmap(us)

alaska_bbox <- c(left = -170, bottom = 50, right = -140, top = 72)
alaska <- get_stamenmap(bbox = alaska_bbox, maptype = "toner-lite", zoom = 5)  
ggmap(alaska)



California <- get_stamenmap(bbox = c(left = -124.409591, bottom = 32.534156, right = -114.131211, top = 42.009518), 
                            maptype = "toner-lite", zoom=8)
ggmap(California) + 
  geom_point(data=housing, mapping=aes(x=longitude, y=latitude, color = medianHouseValue), size = 0.3) + scale_fill_continuous(label=comma) + 
  labs(title="Median House Value", x="Longitude" , y ="Latitude")



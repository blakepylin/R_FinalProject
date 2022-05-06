library(tidyverse)
library(purrr)


billboard = read.csv("billboard.csv")
billboard$ye_song = grepl('Kanye West', billboard$performer)
billboard$week_id <- format(as.Date(billboard$week_id, format = "%m/%d/%Y"), "%Y-%m-%d")

billboard = billboard %>%
  filter(ye_song == TRUE & year > 2016 & year < 2020) %>%
  select(week_id, week_position, song_id) %>%
  arrange(week_id)


kanye_billboard = data.frame(billboard %>%
  group_by(week_id) %>%
  summarise(avg_pos = mean(week_position))
, billboard %>%
  group_by(week_id) %>%
  tally())

kanye_billboard = kanye_billboard %>%
  select(week_id, avg_pos, num_songs = n)

# Billboard week starts Friday and ends on Thursday
# Friday, January 1 – tracking-week begins for sales, streaming and airplay
# Thursday, January 7 – tracking-week ends for sales, streaming and airplay
# Tuesday, January 12 – new chart released, with issue post-dated Saturday, January 16

kanye_billboard$day_begin = as.Date(kanye_billboard$week_id) - 15
kanye_billboard$day_end = as.Date(kanye_billboard$week_id) - 9

df = kanye_billboard %>%
  transmute(week_id, month = map2(day_begin, day_end, seq, by = "1 day")) %>%
  unnest %>%
  distinct

kanye_billboard = merge(test, kanye_billboard, by = "week_id", all.x = TRUE)

kanye_billboard = kanye_billboard %>%
  select(week_id, date = month, avg_pos, num_songs)



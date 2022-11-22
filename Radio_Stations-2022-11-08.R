#------------------------------
# Title: Tidy Tuesday: radio stations
# Date: Tue Nov  8 10:16:18 2022
# Author: Corey Clatterbuck
#------------------------------

## load libraries ----
library(purrr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(janitor)
library(rvest)


## load & join data ----

# load data
state_stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/state_stations.csv')

station_info <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-08/station_info.csv')

# join data
joined_data <- state_stations |> dplyr::right_join(station_info, by = c("call_sign"))


## data cleaning ----

# Issue: there are a lot of hybrid stations, separated by many different characters. Maybe it would be interesting to explore these stations?

# edit a specific row
joined_data[joined_data$facility_id == "38607",5] = c("Classical music, Fine arts, Talk")

# clean the format genres as much as possible & put into long format
long_data <- joined_data |>
  mutate(format_edit = str_replace(format, " \\s*\\([^\\)]+\\)", ""), ## remove parentheses
         format_edit = gsub("'", "", format_edit),
         format_edit = gsub("-", "", format_edit),
         format_edit = gsub("R&B", "R And B", format_edit),
         format_edit = gsub("N.P.R.", "Npr", format_edit),
         # format_edit = gsub("and", "&", format_edit),
         format_edit = str_replace_all(format_edit,"[:punct:]"," / "), ## make / the only punct
         format_edit = gsub("  ", " ", format_edit), ## remove double spaces
         format_edit = str_to_title(format_edit), ## make first word capitalized
         format_edit = gsub("Ac", "AC", format_edit), ## make Ac -> AC
         format_edit = gsub("ACtive", "Active", format_edit), ## make ACtive -> Active
         format_edit = gsub(" / ", "/", format_edit), ## make slash only to aid separate
         format_edit = gsub("Hawaiian", "Hawai'ian", format_edit), ## get it right
         format_edit = gsub("Hiphop", "Hip-hop", format_edit), ## get it right
         format_edit = gsub("Albumoriented", "Album-oriented", format_edit), ## get it right
         format_edit = gsub("ACleaning", "AC-leaning", format_edit), ## get it right
         format_edit = gsub("Americanoriented", "American-oriented", format_edit), ## get it right
         format_edit = gsub("Allnews", "All News", format_edit), ## get it right
         format_edit = gsub("R And B", "R&B", format_edit)) |> ## get it right
  separate(format_edit, c("format_1", "format_2", "format_3", "format_4"), sep = "/", remove = FALSE) |>
  mutate(across(format_1:format_4, trimws)) %>%
  select(facility_id, city, state, service, format_1:format_4) %>%
  pivot_longer(cols = starts_with("format"),
               names_to = "genre_order",
               values_to = "genre",
               values_drop_na = TRUE)

## most common genres?
common_genres <- long_data |>
  dplyr::select(state, genre) |>
  distinct() %>%
  count(genre)

## rank & filter top 10 genres by state, removing those genres that only appear in a single station
top_ten_states <- long_data |> 
  count(state, genre) %>%
  ungroup() %>%
  arrange(state, n) %>%
  group_by(state) %>%
  mutate(rank = rank(-n, ties.method = "first")) %>%
  filter(rank < 11 & n > 1) %>%
  ungroup()

## how many genres are there in the top 10 genres?
n_distinct(top_ten_states$genre) ## 47!

## Do any top 10 genres appear only once? May want to note these in the viz. 
top_ten_states |>
  count(genre) |>
  filter(n == 1) |>
  nrow() ## 17!


# Any station with a / or ; in `format` will become "hybrid"
# joined_data$format_2 <- ifelse(grepl("/", joined_data$format),"Hybrid",joined_data$format)
# joined_data$format_2 <- ifelse(grepl(";", joined_data$format_2),"Hybrid",joined_data$format_2)
# joined_data$format_2 <- ifelse(grepl(",", joined_data$format_2),"Hybrid",joined_data$format_2)
# joined_data$format_2 <- str_to_title(joined_data$format_2)
# try2 <- as.data.frame(table(joined_data$format_2))

# issue: hybrid stations represent a significant portion of the total radio stations! Maybe that would be fun to play with instead...


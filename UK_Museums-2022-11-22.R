#------------------------------
# Title: Tidy Tuesday
# Date: Tue Nov 22 09:01:50 2022
# Author: Corey Clatterbuck
#------------------------------

# libraries
library(tidyverse)
library(maps)
library(ggimage)

# read data
museums <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-11-22/museums.csv')

# some exploration
colnames(museums)
table(museums$Area_Deprivation_index_services)
table(museums$Area_Geodemographic_supergroup, useNA = "always")
table(museums$Year_closed, useNA = "always")
table(museums$DOMUS_Subject_Matter, useNA = "always")

maritime <- museums %>%
  mutate(yr_closed = str_sub(Year_closed, -4),
         status = if_else(yr_closed == 9999, "open", "closed")) %>%
  filter(DOMUS_Subject_Matter == "maritime") %>%
  dplyr::select(museum_id, Latitude, Longitude, DOMUS_Subject_Matter, status)


# base map, from https://bookdown.org/yann_ryan/r-for-newspaper-data/mapping-with-r-geocode-and-map-the-british-librarys-newspaper-collection.html
worldmap = map_data('world')
UKmap <- ggplot() + 
  geom_polygon(data = worldmap, 
               aes(x = long, y = lat, 
                   group = group), 
               fill = 'gray90', 
               color = 'black') + 
  coord_fixed(ratio = 1.3, 
              xlim = c(-10,3), 
              ylim = c(49, 59)) +
  theme_void()


# add maritime data
UKmap +
  geom_point(data = maritime,
             aes(x = Longitude, y = Latitude, color = status),
             size = 3)  +
  theme(legend.position = c(0.87, 0.75),
        legend.background = element_rect(fill = "white", color = "black")) +
  labs(title = "Maritime Museums in the United Kingdom",
       subtitle = "Most open maritime museums are in rural, coastal areas of England",
       color = "Able to visit?",
       caption = "Data from https://museweb.dcs.bbk.ac.uk/data | author: Corey Clatterbuck")


# get cool icons on map instead, following https://www.littlemissdata.com/blog/iconmap
maritime2 <-maritime %>% 
  mutate(Image = case_when(status == "open" ~ "https://raw.githubusercontent.com/cclatterbuck/tidy-tuesday/master/images/sailingship_color.png",
                           status == "closed" ~ "https://raw.githubusercontent.com/cclatterbuck/tidy-tuesday/master/images/sinkingship_color.png"))

UKmap +
  geom_image(data = maritime2, aes(x = Longitude, y = Latitude, image=Image), size = 0.1, asp = 1) + 
  theme(legend.position = c(0.87, 0.75),
        legend.background = element_rect(fill = "white", color = "black")) +
  labs(title = "Maritime Museums in the United Kingdom",
       subtitle = "Most open maritime museums are in rural, coastal areas of England",
       image = "Able to visit?",
       caption = "Data from https://museweb.dcs.bbk.ac.uk/data | author: Corey Clatterbuck")

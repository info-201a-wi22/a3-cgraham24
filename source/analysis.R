library(tidyverse)
library(maps)
library("dplyr")



incarcerations <-read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")


#5 variables
first_year <- min(incarcerations$year)

last_year <- max(incarcerations$year)

num_row <- nrow(incarcerations)

num_counties <- incarcerations %>%
  summarize(n = n_distinct(county_name))

name_regions <- unique(incarcerations$region)


#Chart over time

data <- incarcerations %>%
  select(black_prison_pop, year, state, county_name) %>%
  filter(state == "WA")
surrounding_counties <- data %>%
  filter(county_name == "King County" | county_name == "Kitsap County" | county_name == "Pierce County" | county_name == "Thurston County" |county_name == "Snohomish County")
over_time <- ggplot(data = surrounding_counties, aes(x = year, y = black_prison_pop, group = county_name)) + 
  geom_line(aes(color=county_name)) + xlim(1990,2018) + labs(
    title = " Puget Sound Counties Black Jail Population Over Time",
    x = "Year",
    y = "Black Prison Population"
  )


#Variable Comparison Chart
urb_city_data <- incarcerations %>%
  select(black_prison_pop, total_pop, urbanicity, state, total_jail_pop) %>%
  filter(state == "WA")%>% 
mutate(rate = black_prison_pop/total_jail_pop) %>%
  group_by(urbanicity) 

wa_rural_black<- sum(urb_city_data[which(urb_city_data$urbanicity== "rural"), 1], na.rm = T  )
wa_rural_tot <- sum(urb_city_data[which(urb_city_data$urbanicity== "rural"), 5], na.rm = T  )
wa_rural_rate <- wa_rural_black/wa_rural_tot

wa_suburban_black <- sum(urb_city_data[which(urb_city_data$urbanicity== "suburban"), 1], na.rm = T  )
wa_suburban_tot <- sum(urb_city_data[which(urb_city_data$urbanicity== "suburban"), 5], na.rm = T  )
wa_suburban_rate <- wa_suburban_black/wa_suburban_tot

wa_smallmid_black <- sum(urb_city_data[which(urb_city_data$urbanicity== "small/mid"), 1], na.rm = T  )
wa_smallmid_tot <- sum(urb_city_data[which(urb_city_data$urbanicity== "small/mid"), 5], na.rm = T  )
wa_smallmid_rate <- wa_smallmid_black/wa_smallmid_tot

wa_urban_black <- sum(urb_city_data[which(urb_city_data$urbanicity== "urban"), 1], na.rm = T  )
wa_urban_tot <- sum(urb_city_data[which(urb_city_data$urbanicity== "urban"), 5], na.rm = T  )
wa_urban_rate <- wa_urban_black/wa_urban_tot

rates <- data.frame(geog_type= c("rural", "small/mid", "suburban", "urban"),
          rate = c(wa_rural_rate, wa_smallmid_rate, wa_suburban_rate, wa_urban_rate))

variable_chart <- ggplot(data = rates, aes(x = geog_type, y = rate)) + geom_bar(stat = "identity") + labs(
  title = "Washington Black Jail Inmate Proportion at Different Population Densities",
  x = "Population Density Classification",
  y = "Black Jail Inmate Proportion"
)

#map chart



  recent_data <- incarcerations %>%
    select(year, state, black_jail_pop, white_jail_pop)%>%
    filter(year == "2018")%>%
    group_by(state)%>%
    mutate(black_white_rate = black_jail_pop/white_jail_pop, state = state.name[match(state,state.abb)], state = tolower(state))%>%
    group_by(state)

states_list <- unique(recent_data$state)

sum_data <- data.frame(state = states_list, black_white_rate = summarize(recent_data, state = mean(black_white_rate, na.rm = T)))
state_map <- map_data("state")%>%
  rename(state = region) %>%
  left_join(sum_data, by="state")%>%
  group_by(state)

map_chart <- ggplot(state_map) + geom_polygon(mapping = aes(x = long, y = lat, group = group, fill = state.1),
    color = "white",
     size = .01) + coord_map() + scale_fill_continuous(low = "132B43", high = "Red") +
  labs(title = "US Black Inmates in Jail per White Inmate", x = "Longitude", y = "Latitude", fill = "Black/White Ratio")

  
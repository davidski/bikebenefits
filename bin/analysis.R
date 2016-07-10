library(jsonlite)
library(purrr)
library(httr)
library(dplyr)

# fetch all cities
cities_url <- 'http://www.bicyclebenefits.org/search/cities'
cities <- fromJSON(content(GET(cities_url), as = "text"), flatten = TRUE)
save(cities, file = "data/bikebenifits_cities.Rdata")

# fetch the details for a given city
get_biz <- function(city_id, base_url = 'http://www.bicyclebenefits.org/search/members?city_id=')  {
  search_url <- paste0(base_url, city_id)
  dat <- fromJSON(content(GET(search_url), as = "text"), flatten = TRUE)
  dat$members$city_id <- city_id
  dat$city$city_id <- city_id
  return(dat)
}

dat <- map(cities$id, get_biz)

# coerce member business into a dataframe and peel off categories into 
# a more normalized table
members <- map(dat, "members") %>% compact %>% bind_rows() %>% filter(!is.na(id))
categories <- members %>% distinct(category.id, category.name, category.logo)
members <- members %>% select(-c(category.name, category.logo))
save(dat, file = "data/bikebenifits.Rdata")

# coerce the states into a dataframe
states <- map(dat, "city")
states <- do.call(rbind, states) %>% as_data_frame
states$state_name <- map_chr(states$state, "name")
states <- select(states, -state) %>% simplify_all %>% bind_rows

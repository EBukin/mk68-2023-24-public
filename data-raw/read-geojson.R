

library(tidyverse)
library(jsonlite)
library(sf)

#data
js_file <- read_json("./data-raw/SO_geojson_2020_A.json")


# Exploring
js_file$features %>% str(max.level = 1)


js_file$features[[3]] %>% str(max.level = 1)
js_file$features[[3]]$type
js_file$features[[3]]$properties

# Functions
parse_point <- function(x) {
  unlist(x)
}

parse_poly <- function(x) {
  x %>%
    map(~ {
      vapply(.x, FUN = parse_point, FUN.VALUE = numeric(2)) %>%
        t()
    }) %>%
    sf::st_polygon()
}

parse_multipoly <- function(x) {
  x %>%
    map(parse_poly) %>%
    sf::st_multipolygon()
}

parse_feature <- function(feat) {
  coords <- feat$geometry$coordinates
  n_poly <- coords %>% length()
  if (n_poly > 1) type <- "multipoly"
  if (n_poly == 1) type <- "poly"
  
  if (type == "poly") {
    geom <- coords %>% parse_poly()
  }
  
  if(type == "multipoly") {
    geom <- coords %>% parse_multipoly()
  }
  
  dta <- 
    tibble(geometry = st_geometry(geom)) %>% 
    st_as_sf() %>% 
    st_set_crs(3857) %>% 
    bind_cols(feat$properties %>% as_tibble())
  
  dta 
}


# Exploring 2 
js_file$features %>% map_dfr(~tibble(n = .x$geometry$coordinates %>% length())) %>% count(n)


feat <- js_file$features[[10]]

coords <- feat$geometry$coordinates
n_poly <- coords %>% length()

if (n_poly > 1) type <- "multipoly"
if (n_poly == 1) type <- "poly"


if (type == "poly") {
  geom <- coords %>% parse_poly()
}

if(type == "multipoly") {
  geom <- coords %>% parse_multipoly()
}

dta <- 
  tibble(geometry = st_geometry(geom)) %>% 
  st_as_sf() %>% 
  st_set_crs(3857) %>% 
  bind_cols(feat$properties %>% as_tibble())

# Testing:
js_file$features[[1]] %>% parse_feature()


# Full data 
dta <- js_file$features %>%  map_dfr(parse_feature)

dta %>% 
  select(estimated_population) %>% 
  plot()


# dta %>% write_rds()

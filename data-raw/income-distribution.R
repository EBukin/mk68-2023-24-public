# The raw data is downloaded from the internet directly


# Proper reference is: The data was made available to Our World In Data by the 
#  two authors. The data up to 2008 is published with the main publication
#  Milanovic and Lakner (2015) – Global Income Distribution. Available online 
#  at the World Bank: http://elibrary.worldbank.org/doi/abs/10.1596/1813-9450-6719.




# Saving data in a file
library(dplyr)
library(readr)
library(readxl)
library(here)
library(ggplot2)
library(tidyr)
library(forcats)
library(magrittr)
library(stringr)
library(jsonlite)
library(purrr)


# Funs --------------------------------------------------------------------

read_one_json <- function(one_file) {
  
  
  dta <- fromJSON(one_file, flatten = F)
  one_file %>% str_extract("\\d{4}")
  dta$values[[1]] %>% 
    as_tibble(.name_repair = "unique") %>% 
    set_colnames(c("Income", "value")) %>% 
    mutate(region = dta$key,
           year = as.numeric(one_file %>% str_extract("\\d{4}")))
}


# Loading data ---------------------------------------------
all_dta <- 
  list.files("data-raw/income-distribution/.",
             pattern = "json",
             full.names = T, recursive = T) %>% 
  map_dfr(read_one_json)


# Constructing total number of people in each country ------------------

pop <- tibble(year = c(1988, 1993, 1998, 2003, 2008, 2011),
              pop = c(5.1, 5.54, 5.95, 6.35, 6.76, 7 ))

clean_dta <-
  all_dta %>% 
  # filter(Income < 100) %>% 
  group_by(Income, year) %>% 
  mutate(reg_total = sum(value, na.rm = TRUE),
         coun_in_reg = value / reg_total) %>% 
  group_by(year) %>% 
  mutate(tot_world_share = reg_total / sum(value) * coun_in_reg ) %>% 
  ungroup() %>% 
  left_join(pop) %>% 
  mutate(freq = pop * 1000000 * tot_world_share) %>% 
  select(region, year, income = Income, freq)


clean_dta %>% 
  group_by(year) %>% 
  summarise(sum(freq))

# saving
clean_dta %>% write_rds("materials/data/income-distribution.rds", compress = "gz")


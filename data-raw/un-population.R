library(dplyr)
library(readr)
library(readxl)
library(here)
library(ggplot)
library(tidyr)
library(forcats)
fl <- "./data-raw/un-population/WPP2022_Population1JanuaryBySingleAgeSex_Medium_1950-2021.zip"
dta <- read_csv(fl)
dta  %>%
  select(SortOrder, ISO3_code, LocID, LocTypeID, LocTypeName, ParentID, Location,
         Year = Time,
         Age = AgeGrpStart, PopMale, PopFemale, PopTotal) %>%
  filter(Year %in% c(1960, 1990, 2020))  %>%
  mutate(LocTypeName = as_factor(LocTypeName),
         Location = as_factor(Location)) %>%
  pivot_longer(
    cols = c(PopMale, PopFemale, PopTotal),
    names_to = "Variable",
    values_to = "Population"
    ) %>%
  mutate(
    Variable = ifelse(Variable == "PopTotal", "Total", Variable),
    Variable = ifelse(Variable == "PopFemale", "Female", Variable),
    Variable = ifelse(Variable == "PopMale", "Male", Variable),
    Variable = factor(Variable, levels = c("Female", "Male", "Total"))
    ) %>%
  select(-SortOrder, -ISO3_code)%>%
  write_rds("./materials/data/un-population.rds", compress = "gz")

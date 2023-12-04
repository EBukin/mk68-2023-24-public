library(dplyr)
library(readr)
library(readxl)
library(here)
library(ggplot)
library(tidyr)
library(forcats)

fl <- "./data-raw/brazil-food-consumption/BRA_00036.zip"

dta <- read_csv(fl)

individuals <- 
  read_csv("./data-raw/brazil-food-consumption/subject_user.zip") %>% 
  select(SUBJECT, ADM0_NAME, ADM1_NAME, WEIGHTING_FACTOR, AREA_TYPE, SEX, 
         WEIGHT, HEIGHT, HOUSEHOLD, AGE_YEAR) %>% 
  mutate(
    SEX = 
      ifelse(SEX == 1, "Male", "Female") %>% 
      factor(levels = c("Female", "Male")), 
    AREA_TYPE = 
      ifelse(AREA_TYPE == 1, "Rural", "Urban") %>% 
      factor(levels = c("Rural", "Urban")), 
    ADM0_NAME = factor(ADM0_NAME),
    ADM1_NAME = factor(ADM1_NAME) 
    )

br_dta <- 
  dta %>% 
  select(SUBJECT, SURVEY_DAY, ENERGY_kcal, PROTEIN_g, CARBOH_g, SUGAR_g, FAT_g, FIBTG_g) %>% 
  group_by(SUBJECT, SURVEY_DAY) %>% 
  summarise(across(everything(), ~sum(., na.rm = TRUE))) %>% 
  ungroup() %>% 
  right_join(individuals, by = "SUBJECT")

br_dta %>% write_rds("./materials/data/brazil-nutrition.rds", compress = "gz")



br_dta %>%
  ggplot() +
  aes(x = ENERGY_kcal#, weight = WEIGHTING_FACTOR
      ) +
  geom_density() +
  geom_density(aes(weight = WEIGHTING_FACTOR)) +
  scale_x_log10()
  



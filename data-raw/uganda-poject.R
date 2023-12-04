library(tidyverse)
library(haven)
library(labelled)
# install.packages("labelled")

dta <- read_sav("data-raw/uganda-poject/Uganda_B_E_agri and nutrition 04.06.2018.sav")

names(dta)


dta %>% 
  select(1:100) %>% 
  labelled::generate_dictionary(values = FALSE, details  = "none")

dta %>% 
  # select() %>% 
  labelled::generate_dictionary(values = FALSE, details  = "none")


dta %>% 
  look_for("Minimum dietary diversity")
dta %>% 
  look_for("contr")
dta %>% 
  look_for("overweight")



dta %>% look_for("inco")

dta %>% look_for("SUPPLEMIRON")
dta %>% look_for("SUPPLEMVITA")

dta %>% look_for("SUPPL")
dta %>% look_for("NUTREneu_E")


ugd_dta <- 
  dta %>%
  select(
    GROUP_B,
    GROUP_E,
    NUTREneu_E,
    TOTHHMEM,
    # NE_RATE_B,,
    contains("HAZ"),
    contains("WAZ"),
    contains("WHZ"),
    contains("WI"),
    contains("WI"),
    contains("EDUYEARS"),
    contains("SUPPLEMIRON"),
    contains("SUPPLEMVITA"),
    contains("MDDWSCORE")
    ) %>% 
  # mutate(Group = ifelse(GROUP_B %in% c(2,5,6), "Educ. Treatment", "Control")) %>% 
  filter(!is.na(NUTREneu_E)) %>% 
  mutate(Group = ifelse(NUTREneu_E %in% c(1), "Treatment", "Control") %>% 
           factor(levels = c("Control",  "Treatment")))

ugd_dta %>% 
  select(TOTHHMEM, NUTREneu_E) %>% 
  look_for()

ugd_dta %>% 
  # select(TOTHHMEM, Group, NUTREneu_E) %>% 
  unlabelled() %>% 
  # mutate(NUTREneu_E = )
  ggplot() + 
  aes(y = HAZ_E, x = Group, color = Group) + 
  geom_jitter(width = 0.02) + 
  geom_boxplot(aes(x = Group, color = Group,group = Group),
               width = 0.2, alpha = 0.3) 


var.test(SUPPLEMIRON_E     ~ Group, data = ugd_dta) 
t.test(SUPPLEMIRON_E ~ Group, data = ugd_dta, var.equal = TRUE) 


ugd_dta %>% 
  select("MDDWSCORE_E") %>% 
  # look_for("MDDWSCORE_E") %>% 
  unlabelled()

ugd_dta %>% 
get_sum_stat(var = c("MDDWSCORE_E", "MDDWSCORE_B"), group_var = "Group")
 

stat <- 
  

# test <

ugd_dta %>% 
  select(var, group_var)

aa <- var.test(TOTHHMEM ~ Group, data = ugd_dta) 

broom::tidy(aa)

tt1 <- 
t.test(TOTHHMEM ~ NUTREneu_E, data = ugd_dta, var.equal = TRUE)

t.test(TOTHHMEM ~ NUTREneu_E, data = ugd_dta, var.equal = FALSE)



ugd_dta %>% 
  group_by(NUTREneu_E) %>% 
  # select(Group, TOTHHMEM) %>%
  count()
  skimr::skim()
  





# Key variables:
# NUTREneu_E         control versus nutrition education only at endline 
# WI_B               Wealth Index Baseline Uganda 
# WI_E               Wealth Index Endline Uganda      
# ageindays_B        ageindays of children at baseline
# EDUYEARS_B         education of respondent in years  
# EDUCLEV_B          3.10a What is the highest level of school you completed?
# EDUCHH_B           3.12a What is the highest level of school the head of the HH completed?    
# CHWEIGHHAV_E weight child average                                        dbl    
# CHWEIGHHAV_B weight child average                                        dbl 
# 19  WAZ_B        Weight-for-age z-score                                      dbl                       
# 20  WHZ_B        Weight-for-length/height z-score                            dbl                       
# 23  WAZ_E        Weight-for-age z-score                                      dbl                       
# 24  WHZ_E        Weight-for-length/height z-score                            dbl 
# 235 CHHEIGHHAV_B      height child average                                  dbl   
# 235 CHHEIGHHAV_E      height child average                                  dbl   

# https://www.who.int/tools/child-growth-standards/standards/weight-for-age


dta %>% 
  ggplot() + 
  aes(x = CHHEIGHHAV_B) + 
  geom_boxplot()


dta %>% 
  look_for("CHHEIGHHAV_B")

# Checking Weight for age measurements:
dta

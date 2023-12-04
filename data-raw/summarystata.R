

library(knitr)
library(tidyverse)
library(report)
library(skimr)





library(scales)


clear_skim_stats <- 
  function(dta, digits = 2, new_line = FALSE) {
    new_line_here <<- new_line
    mf <- scales::number_format(accuracy = 1 / 10 ^ digits, big.mark = " ")
    dta <- 
      dta %>%
      as_tibble() %>%
      mutate(
        `N val.obs.` = n_Obs - n_Missing,
        `Mean (SD)` =
          paste0(mf(Mean),
                 if (new_line)
                   "\n("
                 else
                   " (",
                 mf(SD), ")"),
        `[Min; Max]` = paste0("[", mf(Mean), "; ", mf(SD), "]")
      ) %>%
      select(any_of("Group"), Variable, `N val.obs.`, `Mean (SD)`)
    
    if ("Group" %in% names(dta)) {
      dta <- 
        dta    %>%
        pivot_wider(
          names_from = Group,
          values_from = c(`N val.obs.`, `Mean (SD)`),
          names_glue = "{Group}{if (new_line_here) '\n' else '|'}{.value}"
        )
    }
    dta
    
  }



arrange_report_table <- 
  function(dta,
           group_by = NULL,
           digits = 2,
           new_line = FALSE) {
    new_line_here <<- new_line
    mf <-
      scales::number_format(accuracy = 1 / 10 ^ digits, big.mark = " ")
    dta <- 
      dta %>%
      as_tibble() %>%
      mutate(
        `N val.obs.` = n_Obs - n_Missing,
        `Mean (SD)` =
          paste0(mf(Mean),
                 if (new_line)
                   "\n("
                 else
                   " (",
                 mf(SD), ")"),
        `[Min; Max]` = paste0("[", mf(Mean), "; ", mf(SD), "]")
      ) %>%
      select(any_of("Group"), Variable, `N val.obs.`, `Mean (SD)`)
    
    if ("Group" %in% names(dta)) {
      dta <- 
        dta    %>%
        pivot_wider(
          names_from = Group,
          values_from = c(`N val.obs.`, `Mean (SD)`),
          names_glue = "{Group}{if (new_line_here) '\n' else '|'}{.value}"
        )
    }
    dta
    
  }

# options(scipen = 10)
# 11^17
# 
# dta <- 
  mtcars %>% 
  report_sample(group_by = "vs")
  group_by(vs ) %>%
  skim_without_charts() %>% 
  as_tibble()
  
  
  mtcars %>% 
    report_sample(group_by = "vs")
  
  dta %>%
    report_sample(group_by = "country") 
  
      
    
  # report_table() %>% 
  #   arrange_report_table()



format(1212123.1231231, )      

library(tidyverse)
library(readxl)

who_lhfa <- 
  read_excel("data-raw/who/lhfa-boys-zscore-expanded-tables.xlsx") %>% 
  mutate(gender = "boys") %>% 
  bind_rows(
    read_excel("data-raw/who/lhfa-girls-zscore-expanded-tables.xlsx") %>% 
      mutate(gender = "girls"))

who_lhfa %>% 
  write_rds("materials/data/who_lhfa.rds", compress = "gz")


who_lhfa %>% 
  filter(Day < 730) %>% 
  filter(gender == "girls") %>%
  ggplot() + 
  aes(x = Day, colour = gender, fill = gender) + 
  geom_line(aes(y = SD0), size = 1) + 
  geom_ribbon(aes(ymin = SD0, ymax = SD2), alpha = 0.05) + 
  geom_ribbon(aes(ymin = SD2neg , ymax = SD0), alpha = 0.05) + 
  geom_ribbon(aes(ymin = SD2, ymax = SD3), alpha = 0.15) + 
  geom_ribbon(aes(ymin = SD3neg , ymax = SD2neg), alpha = 0.15) + 
  geom_ribbon(aes(ymin = SD3, ymax = SD4), alpha = 0.25) + 
  geom_ribbon(aes(ymin = SD4neg , ymax = SD3neg), alpha = 0.25) + 
  scale_color_manual(values = c("#c51010", "#088da5"))+ 
  scale_fill_manual(values = c("#c51010", "#088da5")) + 
  facet_grid(rows = "gender") + 
  xlab("Length/Height in cm") + 
  ylab("Age") + 
  scale_x_continuous(
    breaks = c(0, 14, 35, 60, 90, 120, 150, 180, 270, 360, 15*30, 18*30, 21*30, 24*30),
    labels = c(0, "2W", "5W", "2M", "3M", "4M", "5M", "6M", "9M", "12M", "15M", "18M", "21M", "2Y"),
    minor_breaks = NULL
  )
            


```{r}
# dist_dta <- 
#   who_lhfa %>% 
#   filter(Day %in% c(0, 14, 360),
#          gender == "boys")  %>% 
#   rowwise() %>% 
#   mutate(
#     mu = SD0,
#     sigma = SD1 - SD0,
#     data = map2(mu, sigma, ~{rnorm(25000, .x, .y)})
#   ) %>% 
#   unnest(data) %>%
#   mutate(
#     z = (data - mu) / sigma,
#     stunting = case_when(
#       abs(z) <= 2 ~ "No",
#       abs(z) > 2 & abs(z) <= 3  ~ "Severe",
#       abs(z) > 3  ~ "Critical"
#     ),
#     y = pmap_dbl(list(z, mu, sigma), ~ {dnorm(..1)})
#   ) %>% 
#   select(Day, data, stunting, z, y, mu, sigma)
# 
# dist_dta %>% 
#   ggplot() + 
#   aes(x = data, ymin = 0, ymax = y, fill = stunting, group = stunting) + 
#   geom_ribbon(alpha = 0.2) + 
#   facet_grid(rows = "Day")
# 
# 
# 
# dist_dta %>% 
#   ggplot() + 
#   aes(x = data, y = y) + 
#   geom_density_ridges(scale = 3, rel_min_height = 0.01)
# 
# library(ggridges)
# 
# ggridges::geom
```

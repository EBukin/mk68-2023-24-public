
library(tidyverse)
# library(stringr)
library(ggplot2)
library(ggbrace)
library(ggpmisc)
# library(here)
library(patchwork)
# library(synthpop)

n_pop <- 2*10^6
bta0 =  656.15
bta1 =  810.95
set.seed(12)
land_pop <-
  tibble(area = runif(n_pop, 0.1, 5),
         noise = rnorm(n_pop, mean = 0, 515),
         value = bta0 + bta1 * area + noise) %>% 
  select(-noise) #%>% filter(value > 0)

fit0 <- lm(value ~ area, land_pop)

on_x <- 2.75
plot_pop_dta <- land_pop %>% sample_n(600) 
plot_pop_dta2 <- plot_pop_dta %>% sample_n(50) 
# err_margin <- plot_pop_dta %>% 
#   mutate(err = abs(bta1 * area + bta0 - value)) %>% 
#   pull(err) %>% sort(decreasing = T) %>% magrittr::extract2(24)
# hghl <- 
#   c(3.5, 3.9, 4.3, 4.55) %>% 
#   map_dfr(~{plot_pop_dta %>%  filter(area > .x, area < .x + 0.1) %>% sample_n(1)})
# range_at <- 4.25

pp_1 <-
  plot_pop_dta %>%
  ggplot() +
  aes(x = area, y = value) +
  geom_point(
    alpha = 0.25,
    colour = "black",
    size = 1.25,
    fill = "black",
    shape = 21
  ) +
  scale_x_continuous(
    breaks = scales::breaks_extended(10),
    expand = c(0, 0),
    limits = c(0, max(plot_pop_dta$area))
  ) +
  scale_y_continuous(
    breaks = scales::breaks_extended(10),
    expand = c(0, 0),
    limits = c(0, max(plot_pop_dta$value))
  ) +
  expand_limits(x = 0, y = 0) +
  theme_minimal()  +
  theme(axis.line = element_line(
    colour = "darkgray",
    size = 0.5,
    linetype = "solid",
    arrow = arrow(
      type = "closed",
      length = unit(0.15, "inches"),
      angle = 15
    )
  )) + 
  xlab("Plot size, ha") + 
  ylab("Land price, Euro / ha")


pp_2a <- 
  pp_1 +
  geom_abline(aes(slope = bta1, intercept = bta0),
              colour = "#e41a1c",
              linewidth = 1.5) 

pp_2 <-
  pp_2a  +
  ggplot2::annotate(
    geom = "curve",
    x = 0.75 ,
    y = 5350,
    xend = 0.5,
    yend = 0.5 * bta1 + bta0,
    curvature = .1,
    colour = "#e41a1c",
    arrow = arrow(length = unit(.025, "npc") , type = "closed"),
    size  = 1
  ) +
  ggplot2::annotate(
    "label",
    alpha = 0.5,
    x = 0.75,
    y =  5500,
    label = "PRF",
    hjust = 0.5,
    vjust = 0,
    colour = "#e41a1c",
    size = 4
  ) 


pp_3 <-
  pp_2 +
  geom_segment(
    x = 0,
    xend = 1.5,
    y = bta0,
    yend = bta0,
    linewidth = 1.5,
    colour = "#377eb8",
    linetype = "longdash"
  ) +
  geom_errorbar(
    aes(x = 1.5, ymin = 0, ymax = bta0),
    width = 0.1,
    colour = "#377eb8",
    linewidth = 1.5
  ) +
  ggplot2::annotate(
    "label",
    alpha = 0.9,
    x = 1.6,
    y = bta0 / 2,
    label = expression(beta[0] == 656.15),
    hjust = 0,
    colour = "#377eb8",
    size = 5
  )


pp_4 <-
  pp_3 +
  geom_segment(
    x = 2,
    xend = 3,
    y = bta0 + 2 * bta1,
    yend = bta0 + 2 * bta1,
    linetype = 2,
    colour = "#4daf4a",
    size = 1.5
  ) +
  geom_errorbar(
    aes(
      x = 3,
      ymin =  bta0 + 2 * bta1,
      ymax =  bta0 + 3 * bta1
    ),
    width = 0.1,
    colour = "#4daf4a",
    size = 1.5
  ) +
  ggplot2::annotate(
    "label",
    alpha = 0.9,
    x = 3.1,
    y = bta0 + 1 * bta1 + bta1 * 1.5,
    label = expression(beta[1] == 810.95),
    hjust = 0,
    colour = "#4daf4a",
    size = 5
  ) +
  geom_brace(
    aes(x = c(2, 3),
        y = c(1.5, 2) * bta1 + bta0),
    rotate = 180,
    colour = "#4daf4a",
    size = 1.5,
    inherit.aes = FALSE,
    inherit.data = FALSE
  ) +
  annotate(
    "label",
    alpha = 0.9,
    x = 2.5,
    y = bta0 + 1.25 * bta1,
    label = "1 unit change in X",
    hjust = 0.5,
    colour = "#4daf4a",
    size = 4
  )

high <-
  tibble(area = c(0.71, 4.07, 4.57, 1.15)) %>%
  mutate(
    fitline = area * bta1 + bta0,
    value = fitline + c(1315, -1813, 890, -551))

pp_5 <-
  pp_4 +
  geom_point(
    data = high,
    fill = "#ff7f00",
    colour = "#ff7f00",
    size = 3
  ) 


pp_6 <-
  pp_4 +
  geom_segment(
    data = high %>% slice(2:4),
    aes(
      x = area,
      xend = area,
      y = 0,
      yend = value
    ),
    colour = "#a65628",
    size = 1
  ) +
  ggplot2::annotate(
    geom = "curve",
    x = 3.25 ,
    y = 1000,
    xend = high$area[[2]],
    yend = high$value[[2]],
    curvature = -.2,
    colour = "#a65628",
    arrow = arrow(length = unit(.025, "npc") , type = "closed"),
    size  = 1
  ) +
  # ggplot2::annotate(geom = "curve", x = 3.75 , y = 1000,
  #          xend = high$area[[3]], yend = high$value[[3]],
  #          curvature = -.2, colour = "#a65628",
  #          arrow = arrow(length = unit(.025, "npc") , type="closed"),
  #          size  = 1
  #          ) +
  ggplot2::annotate(
    geom = "curve",
    x = 3.25 ,
    y = 1000,
    xend = high$area[[4]],
    yend = high$value[[4]],
    curvature = .1,
    colour = "#a65628",
    arrow = arrow(length = unit(.025, "npc"), type = "closed"),
    size  = 1
  ) +
  ggplot2::annotate(
    "label",
    alpha = 0.5,
    x = 3.25 ,
    y = 700,
    label = "Actual values",
    hjust = 0.5,
    vjust = 0,
    colour = "#a65628",
    size = 4
  ) +
  geom_point(
    data = high,
    fill = "#ff7f00",
    colour = "#ff7f00",
    size = 3
  )

pp_7 <-
  pp_6 +
  ggplot2::annotate(
    "label",
    alpha = 0.5,
    x = 3.5 ,
    y = 5400,
    label = "Fitted values",
    hjust = 0.5,
    vjust = 0,
    colour = "#555555",
    size = 4
  ) +
  ggplot2::annotate(
    geom = "curve",
    x = 3.5 ,
    y = 5500,
    xend = high$area[[3]],
    yend = high$fitline[[3]],
    curvature = .1,
    colour = "#777777",
    arrow = arrow(length = unit(.025, "npc") , type = "closed"),
    size  = 1
  ) +
  ggplot2::annotate(
    geom = "curve",
    x =  3.50 ,
    y = 5500,
    xend = high$area[[2]],
    yend = high$fitline[[2]],
    curvature = .1,
    colour = "#777777",
    arrow = arrow(length = unit(.025, "npc") , type = "closed"),
    size  = 1
  ) +
  ggplot2::annotate(
    geom = "curve",
    x =  3.50 ,
    y = 5500,
    xend = high$area[[4]],
    yend = high$fitline[[4]],
    curvature = .1,
    colour = "#777777",
    arrow = arrow(length = unit(.025, "npc") , type = "closed"),
    size  = 1
  ) +
  geom_point(
    data = high,
    aes(y = fitline),
    fill = "#333333",
    colour = "black",
    size = 3
  )


pp_8 <-
  pp_7 +
  ggplot2::annotate(
    "label",
    alpha = 0.5,
    x = 2.4 ,
    y = 5000,
    label = "Error term / Random disturbance",
    hjust = 0.5,
    vjust = 0,
    colour = "#984ea3",
    size = 4
  ) +
  ggplot2::annotate(
    geom = "curve",
    x = 2.4 ,
    y = 5000,
    xend = high$area[[4]] + 0.05,
    yend = high$value[[4]] + 450,
    curvature = -.2,
    colour = "#984ea3",
    arrow = arrow(length = unit(.025, "npc") , type = "closed"),
    size  = 1
  ) +
  ggplot2::annotate(
    geom = "curve",
    x = 2.4 ,
    y = 5000,
    xend = high$area[[1]] + 0.05,
    yend = high$value[[1]] - 120,
    curvature = -.3,
    colour = "#984ea3",
    arrow = arrow(length = unit(.025, "npc") , type = "closed"),
    size  = 1
  ) +
  geom_errorbar(
    data = high,
    aes(x = area, ymin =  fitline, ymax =  value),
    width = 0.1,
    colour = "#984ea3",
    size = 1.5
  ) +
  geom_point(
    data = high,
    fill = "#ff7f00",
    colour = "#ff7f00",
    size = 3
  ) +
  geom_point(
    data = high,
    aes(y = fitline),
    fill = "#333333",
    colour = "#333333",
    size = 3
  )

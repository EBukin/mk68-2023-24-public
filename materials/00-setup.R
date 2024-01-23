#' Generic setup for analysis

library(pacman)
# devtools::install_github("nicolash2/ggbrace")
# remotes::install_github("datalorax/equatiomatic")
pacman::p_load(here, tidyverse, stringi, glue, pins, 
               lubridate, janitor, knitr, patchwork,
               scales, ggpubr, cowplot, matrixStats,
               ggstatsplot, mosaic, statar, palmerpenguins,
               haven, labelled, alr4, plm, GGally, report,
               ggpmisc, ggbrace, parameters, performance,
               AER, equatiomatic, magick, modelsummary)

# pkgload::load_all()
# source(here("R", "support_fn.R"))
# source(here("R", "support_fn.R"))

knitr::opts_knit$set(verbose = FALSE)
# knitr::opts_knit$set(root.dir =  here::here("."))
# knitr::opts_chunk$set(warning=FALSE, message = FALSE, error = FALSE)
# # knitr::opts_chunk$set(cache.path = here::here("docs", "cache", "."))
# knitr::opts_chunk$set(cache = TRUE)
knitr::opts_chunk$set(fig.width = 11)
knitr::opts_chunk$set(fig.height = 6)
# knitr::opts_chunk$set(fig.asp = 0.5454)
knitr::opts_chunk$set(fig.retina = 2)
knitr::opts_chunk$set(out.width = "100%")
# if (!exists("params")) {
#   params <- list()
#   params$cache.rebuild <- FALSE
# }
# knitr::opts_chunk$set(cache.rebuild = params$cache.rebuild)
# 
# options(knitr.duplicate.label = "allow")

# GGplot2 --------------------------------------------------------------------
ggplot2::theme_set(ggplot2::theme_minimal())
# options(ggplot2.continuous.colour = "brew")
# options(ggplot2.discrete.colour = "brew")

# # Output folders -------------------------------------------------------------
# data_clean <- "data-clean/"
# data_raw <- "data-raw/"
# data_temp <- "~/kaz-cad-raw-harvest/"
# data_export <- paste0(data_clean, "09-export/")
# 
# # Boards --------------------------------------------------------------------
# adm_board_clean <- 
#   here(data_clean, "01.01-admin-geoms") %>% 
#   board_folder(versioned = FALSE)
# 
# adm_board_raw <-
#   here(data_raw, "01.01-admin-geoms") %>% 
#   board_folder(versioned = FALSE)
# 
# plot_board_clean <- 
#   here(data_clean, "01.02-plots") %>% 
#   board_folder(versioned = FALSE)
# 
# plot_board_raw <- 
#   here(data_raw, "01.02-plots") %>% 
#   board_folder(versioned = FALSE)
# 
# ugodia_board_clean <- 
#   here(data_clean, "02-ugodia") %>% 
#   board_folder(versioned = FALSE)
# 
# plot_ugod_board <- 
#   here(data_clean, "03-plot-ugodia") %>% 
#   board_folder(versioned = FALSE)
# 
# # plot_board_raw <- 
# #   here(data_raw, "01.02-plots") %>% 
# #   board_folder(versioned = FALSE)
# 
# 
# # force_reharvest ------------------------------------------------------------
# if (!exists("force_reharvest")) {
#   force_reharvest <- FALSE
# }

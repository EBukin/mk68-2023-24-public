
make_two_dist_compare <- function(dff, CL = 0.95, suffix = " SD/SE",
                                  angle = 45) {
  # CL <- 0.95
  sig_l <- (1 - CL)/2
  # dff <- 2
  t_95 <- qt(p = sig_l, df = dff) %>% round(2)
  z_95 <- qnorm(p = sig_l) %>% round(2)
  rg <- c(-5,5)
  
  tfun <- function(x) dt(x, df = dff)
  
  gg_nt0 <-
    ggplot(NULL, aes(rg))  +
    labs(x = NULL, 
         y = "Probability density",
         title = "Confidence intervals for different distributions", 
         subtitle = "Each area under curve is 1") +
    scale_y_continuous(breaks = NULL, expand = expansion(mult = 0, add = 0)) +
    theme(
      axis.line.x = element_line(), 
      axis.ticks.x = element_line(),
      axis.text.x = element_text(angle = angle, vjust = 1, hjust=1),
      legend.position = c(0.9, 0.9)
    ) +
    scale_x_continuous(
      breaks = rg[[1]]:rg[[2]], 
      minor_breaks = NULL,
      labels = 
        rg[[1]]:rg[[2]] %>% 
        str_replace("^0$", "Mean") %>% 
        str_c(., suffix) %>% 
        ifelse(str_detect(., "Mean"), "Mean", .)
    ) + 
    geom_line(
      aes(colour = "Normal"),
      stat = "function", fun = dnorm, xlim = rg, size = 1, linetype = 2 ) +
    geom_vline(xintercept = 0) + 
    geom_area(stat = "function", fun = dnorm, xlim = c(z_95, -z_95),
              fill = "#eeeee4", alpha = 0.150) +
    geom_area(stat = "function", fun = dnorm, xlim = c(rg[[1]], z_95),
              fill = "#09629c", alpha = 0.150) +
    geom_area(stat = "function", fun = dnorm, xlim = c(-z_95, rg[[2]]),
              fill = "#09629c", alpha = 0.150)  +
    geom_label(aes(label =  str_c(CL * 100, "%"), x = c(0.75), y = c(0.38)), 
               fill = "#eeeee4", alpha = 0.150) + 
    geom_label(
      aes(  
        label = c(str_c(sig_l * 100, "%"), str_c(sig_l * 100, "%")), 
        x = c(z_95 - 0.5, -z_95 + 0.5), 
        y = c(0.15, 0.15)
      ), 
      fill = "#09629c",  
      alpha = 0.15
    ) +
    scale_x_continuous(
      breaks = 
        c(rg[[1]]:rg[[2]], z_95, -z_95) %>% 
        sort() %>% 
        `[`(! (.) %in% c(-2, 2)) , 
      minor_breaks = NULL,
      labels = 
        c(rg[[1]]:rg[[2]], z_95, -z_95) %>% 
        sort() %>% 
        `[`(! (.) %in% c(-2, 2))  %>% 
        str_replace("^0$", "Mean") %>% 
        str_c(., suffix)
    ) + 
    geom_vline(xintercept = c(z_95, -z_95), linetype = 4, colour = "#09629c",
               size = 1, alpha = 0.50) + 
    scale_color_manual(values = c("grey", "red")) + 
    guides(color = guide_legend(title = "Distribution"))
  
  
  gg_nt1 <- 
    gg_nt0 + 
    geom_line(
      aes(colour = str_c("t (df=", dff, ")")),
      stat = "function", fun = tfun, xlim = rg, size = 1.25, linetype = 1) + 
    geom_area(stat = "function", fun = tfun, xlim = c(t_95, -t_95),
              fill = "#de7f82", alpha = 0) +
    geom_area(stat = "function", fun = tfun, xlim = c(rg[[1]], t_95),
              fill = "#ce4247", alpha = 0.50) +
    geom_area(stat = "function", fun = tfun, xlim = c(-t_95, rg[[2]]),
              fill = "#ce4247", alpha = 0.50) +
    geom_label(aes(label = str_c(CL * 100, "%"), x = c(0), y = c(0.3))) + 
    geom_label(aes(
      label = c(str_c(sig_l * 100, "%"), str_c(sig_l * 100, "%")), 
      x = c(t_95 - 1, -t_95 + 1), 
      y = c(0.05, 0.05)
    ),
    fill = "#ce4247", 
    alpha = 0.5) + 
    geom_vline(xintercept = c(t_95, -t_95), linetype = 4, colour = "#ce4247",
               size = 1, alpha = 0.50) +
    scale_x_continuous(
      breaks = 
        c(rg[[1]]:rg[[2]], z_95, -z_95, t_95, -t_95) %>% 
        sort() %>% 
        `[`(! (.) %in% c(-2, 2)) , 
      minor_breaks = NULL,
      labels = 
        c(rg[[1]]:rg[[2]], z_95, -z_95, t_95, -t_95) %>% 
        sort() %>% 
        `[`(! (.) %in% c(-2, 2))  %>% 
        str_replace("^0$", "Mean") %>% 
        str_c(., suffix) %>% 
        ifelse(str_detect(., "Mean"), "Mean", .)
    ) 
  
  list(gg_nt0, gg_nt1)
}

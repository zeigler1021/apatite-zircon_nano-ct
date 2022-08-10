#' ggplot theme for all scatter plots in zeigler et al., 2022 (submitted)
#' 
#' @description This function specifies theme elements for all scatter plots.
#' @return A theme you can use the same as any other (eg. theme_bw(), theme_classic())


theme_nanoct <- function () {
  theme(panel.grid = element_blank(), 
        legend.position = c(0.85,0.15), 
        legend.background = element_rect(color = "black", size = 0.3), 
        axis.text = element_text(size = 8, color = "black"), 
        legend.text = element_text(size = 9), 
        axis.title = element_text(size = 9), 
        legend.title = element_blank(), 
        axis.ticks.length = unit(-0.15, "cm"),
        axis.ticks = element_line(color = "black"),
        panel.border = element_rect(size = 1, color = "black")) 
  
}

theme_uncert <- function () { 
  theme(panel.grid.minor = element_blank(),
      legend.title = element_blank(),
      legend.position = c(0.85,0.13), 
      legend.text = element_text(size = 9),
      legend.background = element_rect(color = "black", 
                                       size = 0.3), 
      axis.text = element_text(size = 8, color = "black"), 
      axis.title = element_text(size = 9), 
      panel.border = element_rect(size = 1, color = "black"), 
      axis.title.y = element_text(margin = margin(0, 5, 0, 5)), 
      axis.title.x = element_text(margin = margin(5, 0, 5, 0)))
}


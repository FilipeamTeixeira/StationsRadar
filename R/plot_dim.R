#Plot dim and plot dim selection functions

plot_dim <- function(x) {
  radarplot_dim(x, type, value, weight) +
    coord_polar(start = -1.5708, theta = "x", clip = "off") +
    theme_minimal() +
    theme(axis.ticks = element_blank(),
          axis.text.x = element_text(size = 8),
          axis.text = element_blank(),
          axis.title = element_blank(),
          axis.line = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_blank(),
          plot.margin=unit(c(0,-1,0,-1),"cm"),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5, face = "bold"),
          legend.position = "none"
    ) +
    scale_fill_manual(values = c("INVLOEDSGEBIED" = "forestgreen", "TREIN" = "gray58",
                                 "PLAATS" = "tomato2", "KNOOP" = "royalblue3",
                                 "MOTIVATIE" = "darkorange", "GEBRUIKERSINTENSITEIT" = "gray92")) +
    geom_segment(aes(x = 25, y = 0, xend = 25, yend = 10),
                 color = "royalblue3", size = 0.2) +
    geom_segment(aes(x = 50, y = 0, xend = 50, yend = 10),
                 color = "royalblue3", size = 0.2) +
    geom_segment(aes(x = 130, y = 0, xend = 130, yend = 10),
                 color = "tomato2", size = 0.2) +
    geom_segment(aes(x = 155, y = 0, xend = 155, yend = 10),
                 color = "tomato2", size = 0.2) +
    geom_segment(aes(x = 198.75, y = 0, xend = 198.75, yend = 10),
                 color = "darkorange", size = 0.2) +
    geom_segment(aes(x = 217.5, y = 0, xend = 217.5, yend = 10),
                 color = "darkorange", size = 0.2) +
    geom_segment(aes(x = 236.25, y = 0, xend = 236.25, yend = 10),
                 color = "darkorange", size = 0.2) +
    geom_segment(aes(x = 265, y = 0, xend = 265, yend = 10),
                 color = "gray", size = 0.2) +
    geom_segment(aes(x = 275, y = 0, xend = 275, yend = 10),
                 color = "gray", size = 0.2) +
    geom_segment(aes(x = 310, y = 0, xend = 310, yend = 10),
                 color = "forestgreen", size = 0.2) +
    geom_segment(aes(x = 335, y = 0, xend = 335, yend = 10),
                 color = "forestgreen", size = 0.2) +
    geom_segment(aes(x = 255, y = 0, xend = 255, yend = 10),
                 color = "gray24", size = 1) +
    geom_segment(aes(x = 285, y = 0, xend = 285, yend = 10),
                 color = "gray24", size = 1) +
    geom_segment(aes(x = 75, y = 0, xend = 75, yend = 10),
                 color = "gray24", size = 1) +
    geom_segment(aes(x = 0, y = 0, xend = 0, yend = 10), 
                 color = "gray24", size = 1) +
    geom_segment(aes(x = 105, y = 0, xend = 105, yend = 10),
                 color = "gray24", size = 1) +
    geom_segment(aes(x = 180, y = 0, xend = 180, yend = 10),
                 color = "gray24", size = 1)
}

# Plot Vervoerregio 23 Gent

library(tidyverse)
library(readxl)

    #Plot Dimensions ####
    
dir.create(file.path("plotgtfs/VR_23_Gent"))
dir.create(file.path("plotgtfs/VR_23_Gent/dim"))
dir.create(file.path("plotgtfs/VR_23_Gent/ind"))
    
    dimensions <- read_excel("csv/Rose_23stations_14122018.xlsx", 
                             sheet = "DIMENSIONS", col_names = FALSE)
    
    dimensions_base <- as.data.frame(t(dimensions[1:2,3:19]))
    colnames(dimensions_base) <- c("type", "dimension")
    rownames(dimensions_base) <- seq(1:17)
    dimensions_base$weight <- c(rep(25,3), 30, rep(25, 3), rep(18.75, 4), rep(10, 3), rep(25,3))
    
    dimensions_df <- dimensions[3:25,] #Selection 33 stations
    
    
    for (i in 1:3){
      city <- slice(dimensions_df, i)
      id <- paste(city$...1)
      name <- paste(city$...2)
      city <- as.data.frame(t(city[3:19]))
      colnames(city) <- "value"
      dimensions_sel <- cbind(city, dimensions_base)
      dimensions_sel$value <- as.numeric(as.character((dimensions_sel$value)))
      
      dimensions_sel <- dimensions_sel %>% mutate(natest = ifelse(is.na(value), 10, 0),
                                                  value = ifelse(is.na(value), 0, value))
      
      radarplot(dimensions_sel, type, value, weight) +
        coord_polar(start = -1.5708, theta = "x", clip = "off") +
        theme_minimal() +
        theme(axis.ticks = element_blank(),
              axis.text.x = element_text(size = 12),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.line = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.margin=unit(c(.5,-2,.5,-1.5),"cm"),
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
      
      ggsave(paste("plotgtfs/VR_23_Gent/dim/", name, "_", id, ".png", sep = ""),
             width = 9, height = 8)
      print(paste(name, "_", id, sep = ""))
    }
    
    
    
    
    #Plot Indicators ####
    
    indicators <- read_excel("csv/Rose_23stations_14122018.xlsx",
                             sheet = "INDICATORS", col_names = FALSE)
    
    indicators_base <- as.data.frame(t(indicators[1:3,3:33]))
    colnames(indicators_base) <- c("type", "dimension", "indicators")
    rownames(indicators_base) <- seq(1:31)
    indicators_base$weight <- c(rep(12.5, 2), rep(12.5, 2), rep(12.5, 2),
                                rep(5, 6), rep(5, 5), rep(25, 1), rep(25/3, 3),
                                rep(18.75, 4), rep(10, 3), rep(25,3))
    
    
    indicators_df <- indicators[4:26,] #Selection 23 stations
    
    for (i in 1:23){
      city <- slice(indicators_df, i)
      id <- paste(city$X__1)
      name <- paste(city$X__2)
      city <- as.data.frame(t(city[3:33]))
      colnames(city) <- "value"
      indicators_sel <- cbind(city, indicators_base)
      indicators_sel$value <- as.numeric(as.character((indicators_sel$value)))
      
      indicators_sel <- indicators_sel %>% mutate(natest = ifelse(is.na(value), 10, 0),
                                                  value = ifelse(is.na(value), 0, value))
      
      radarplot_ind(indicators_sel, type, value, weight) +
        coord_polar(start = -1.5708, theta = "x", clip = "off") +
        theme_minimal() +
        theme(axis.ticks = element_blank(),
              axis.text.x = element_text(size = 14,
                                         angle = 6.25-ang$value),
              axis.text = element_blank(),
              axis.title = element_blank(),
              axis.line = element_blank(),
              panel.grid.minor.x = element_blank(),
              panel.grid.major.x = element_blank(),
              plot.margin=unit(c(1,-1,-1,-1),"cm"),
              panel.grid = element_blank(),
              panel.border = element_blank(),
              plot.title = element_text(hjust = 0.5, face = "bold"),
              legend.position = "none"
        ) +
        scale_fill_manual(values = c("INVLOEDSGEBIED" = "forestgreen", "TREIN" = "gray58",
                                     "PLAATS" = "tomato2", "KNOOP" = "royalblue3",
                                     "MOTIVATIE" = "darkorange", "GEBRUIKERSINTENSITEIT" = "gray92")) +
        geom_segment(aes(x = 12.5, y = 0, xend = 12.5, yend = 10),
                     color = "royalblue3", size = 0.2) +
        geom_segment(aes(x = 25, y = 0, xend = 25, yend = 10),
                     color = "royalblue3", size = 0.2) +
        geom_segment(aes(x = 37.5, y = 0, xend = 37.5, yend = 10), #car
                     color = "royalblue3", size = 0.2) +
        geom_segment(aes(x = 50, y = 0, xend = 50, yend = 10), #car
                     color = "royalblue3", size = 0.2) +
        geom_segment(aes(x = 62.5, y = 0, xend = 62.5, yend = 10), #car
                     color = "royalblue3", size = 0.2) +
        geom_segment(aes(x = 80, y = 0, xend = 80, yend = 10),
                     color = "gray", size = 0.2) +
        geom_segment(aes(x = 85, y = 0, xend = 85, yend = 10),
                     color = "gray", size = 0.2) +
        geom_segment(aes(x = 90, y = 0, xend = 90, yend = 10),
                     color = "gray", size = 0.2) +
        geom_segment(aes(x = 95, y = 0, xend = 95, yend = 10),
                     color = "gray", size = 0.2) +
        geom_segment(aes(x = 100, y = 0, xend = 100, yend = 10),
                     color = "gray", size = 0.2) +
        geom_segment(aes(x = 110, y = 0, xend = 110, yend = 10),
                     color = "tomato2", size = 0.2) +
        geom_segment(aes(x = 115, y = 0, xend = 115, yend = 10),
                     color = "tomato2", size = 0.2) +
        geom_segment(aes(x = 120, y = 0, xend = 120, yend = 10),
                     color = "tomato2", size = 0.2) +
        geom_segment(aes(x = 125, y = 0, xend = 125, yend = 10),
                     color = "tomato2", size = 0.2) +
        geom_segment(aes(x = 130, y = 0, xend = 130, yend = 10),
                     color = "tomato2", size = 0.2) +
        #    geom_segment(aes(x = 142.5, y = 0, xend = 142.5, yend = 10),
        #                 color = "tomato2", size = 0.2) +
        geom_segment(aes(x = 155, y = 0, xend = 155, yend = 10),
                     color = "tomato2", size = 0.2) +
        geom_segment(aes(x = 155+(25/3), y = 0, xend = 155+(25/3), yend = 10),
                     color = "tomato2", size = 0.2) +
        geom_segment(aes(x = 155+(25/3)*2, y = 0, xend = 155+(25/3)*2, yend = 10),
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
        geom_segment(aes(x = 75, y = 0, xend = 75, yend = 10),
                     color = "gray24", size = 1) +
        geom_segment(aes(x = 0, y = 0, xend = 0, yend = 10), 
                     color = "gray24", size = 1) +
        geom_segment(aes(x = 105, y = 0, xend = 105, yend = 10),
                     color = "gray24", size = 1) +
        geom_segment(aes(x = 180, y = 0, xend = 180, yend = 10),
                     color = "gray24", size = 1) +
        geom_segment(aes(x = 255, y = 0, xend = 255, yend = 10),
                     color = "gray24", size = 1) +
        geom_segment(aes(x = 285, y = 0, xend = 285, yend = 10),
                     color = "gray24", size = 1)
      
      
      ggsave(paste("plotgtfs/VR_23_Gent/ind/", name, "_", id, ".png", sep = ""),
             width = 7, height = 7)
      print(paste(name, "_", id, sep = ""))
    }

    
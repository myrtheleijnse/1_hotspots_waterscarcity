# Libraries
library(ggplot2)
library(dplyr)
library(maps)
library(mapproj)

# Get the world polygon and extract UK
World <- map_data("world") 
plot <- ggplot() +
  geom_map(data = World, map = World, aes(long, lat, map_id = region), color = "white", fill = "lightgrey", size =0.1) +
  ylim(-50,70)+
  theme(
    panel.background = element_rect(fill = "transparent",
                                    colour = NA_character_), # necessary to avoid drawing panel outline
    panel.grid.major = element_blank(), # get rid of major grid
    panel.grid.minor = element_blank(), # get rid of minor grid
    plot.background = element_rect(fill = "transparent",
                                   colour = NA_character_), # necessary to avoid drawing plot outline
    legend.background = element_rect(fill = "transparent"),
    legend.box.background = element_rect(fill = "transparent"),
    legend.key = element_rect(fill = "transparent")) +
  theme_bw()
plot

ggsave(
  plot = plot,
  filename = "test.png",
  bg = "transparent"
)

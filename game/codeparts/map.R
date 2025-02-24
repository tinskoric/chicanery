### MAP for Chicanery
### Generate background to save process time in-game

library(htmltools)
library(ggiraph)
library(ggthemes)
library(pals)
library(nngeo)
library(sf)
library(svglite)
library(tidyverse)

map_data <- read_rds("data/gamedata.rds")

# generate background image (basically: maps [tile, prov, region] w/o country control)

#tile
tile_bg <- ggplot() +
  geom_sf(data = (map_data %>% filter(substr(tile, 1, 3) == "SEA")), fill = "navy", color = "navy") +
  geom_sf(data = (map_data %>% filter(substr(tile, 1, 4) == "TILE")), fill = "beige", color = "beige") +
  geom_sf(data = (map_data %>% filter(substr(tile, 1, 4) == "TILE")), color = "black", linetype = "dashed", alpha = 0.3) +
  geom_sf(data = st_as_sf(as.data.frame(map_data) %>% filter(center == TRUE) %>% select(centroid)), color = "black", size = 1.5) +
  geom_sf(data = st_as_sf(as.data.frame(map_data) %>% filter(center == TRUE) %>% select(centroid)), color = "white", size = 1) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
  )
ggsave(tile_bg, filename = "data/map/tilemap.png", width = 2147, height = 2160, units = "px", dpi = 320)

#province (here, we put the lines by province!)
# drop the megaprovince: filter(substr(ofProvince, 1, 10) == "PROVINCE_2")
province_bg <- ggplot() +
  geom_sf(data = (map_data %>% filter(substr(tile, 1, 3) == "SEA")), fill = "navy", color = "navy") +
  geom_sf(data = (map_data %>% filter(substr(tile, 1, 4) == "TILE")), fill = "beige", color = "beige") +
  geom_sf(data = (map_data %>% filter(substr(tile, 1, 4) == "TILE") %>% group_by(ofProvince) %>% summarize(geometry = st_union(geometry)) %>% st_remove_holes()), color = "black", linetype = "dashed", alpha = 0.3) +
  geom_sf(data = st_as_sf(as.data.frame(map_data) %>% filter(center == TRUE) %>% select(centroid)), color = "black", size = 1.5) +
  geom_sf(data = st_as_sf(as.data.frame(map_data) %>% filter(center == TRUE) %>% select(centroid)), color = "white", size = 1) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
  )
ggsave(province_bg, filename = "data/map/provincemap.png", width = 2147, height = 2160, units = "px", dpi = 320)

#region (here, we put the lines by region!)
# drop the megaregion: filter(substr(ofRegion, 1, 9) == "REGION_1")
region_bg <- ggplot() +
  geom_sf(data = (map_data %>% filter(substr(tile, 1, 3) == "SEA")), fill = "navy", color = "navy") +
  geom_sf(data = (map_data %>% filter(substr(tile, 1, 4) == "TILE")), fill = "beige", color = "beige") +
  geom_sf(data = (map_data %>% filter(substr(tile, 1, 4) == "TILE") %>% group_by(ofRegion) %>% summarize(geometry = st_union(geometry)) %>% st_remove_holes()), color = "black", linetype = "dashed", alpha = 0.3) +
  geom_sf(data = st_as_sf(as.data.frame(map_data) %>% filter(center == TRUE) %>% select(centroid)), color = "black", size = 1.5) +
  geom_sf(data = st_as_sf(as.data.frame(map_data) %>% filter(center == TRUE) %>% select(centroid)), color = "white", size = 1) +
  theme_void() +
  theme(
    legend.position = "none",
    axis.line = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
  )
ggsave(region_bg, filename = "data/map/regionmap.png", width = 2147, height = 2160, units = "px", dpi = 320)
#ggsave(region_bg, filename = "data/map/regionmap.png")
#ggsave(region_bg, filename = "data/map/regionmap.png", width = 9, height = 6, units = "in", dpi = 1024)

map_plot <- ggplot() + 
  geom_sf(data = map_data, color = "white") +
  geom_sf(data = (map_data %>% filter(substr(tile, 1,3) == "SEA")), fill = "blue") +
  geom_sf(data = (map_data %>% filter(occupied_by == "unoccupied") %>% filter(!is.na(ofRegion))), fill = "beige") +
  geom_sf(data = (map_data %>% filter(occupied_by != "unoccupied")), aes(fill = occupied_by)) +
  geom_sf(data = st_as_sf(as.data.frame(map_data) %>% filter(center == TRUE) %>% select(centroid)), color = "black", size = 1.5) +
  geom_sf(data = st_as_sf(as.data.frame(map_data) %>% filter(center == TRUE) %>% select(centroid)), color = "white", size = 1) +
  geom_sf_text(data = (map_data %>% filter(unit != "none")), aes(label = unit), size = 2) +
  scale_fill_brewer(palette = "Set1", direction = 1) +
  theme_map() +
  theme(
    legend.position = "none"
  )

interactive_plot <- girafe(
  ggobj = map_plot,
  options = list(
    opts_hover(css = "fill:orange;"),
    opts_hover_inv(css = "opacity:0.6;"),
    opts_selection(type = "single", only_shiny = FALSE)
  )
)
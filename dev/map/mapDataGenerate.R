# Tin Skoric
# Generating Map Info

# Yes, this is in R; problem?
# NOTE!!!: Remember to generate your own R project in the directory of this
# file so the dependencies work. (I deleted those b/c it looked messy)

#####
# NOTE: 4K files with a lot of detail will take up ram! Leaving transparency where possible is good at reducing this!
#####

library(dplyr)
library(ggplot2)
library(geojsonsf)
library(nngeo)
library(plotly)
library(png)
library(sf)
library(smoothr)
library(tidyr)
library(tidyverse)
library(jsonlite)

# Load raw map image files
regions <- readPNG("data/raw/REGIONS.png")
provinces <- readPNG("data/raw/PROVINCES.png")
tiles <- readPNG("data/raw/TILES.png")
seas <- readPNG("data/raw/SEAS.png")
coasts <- readPNG("data/raw/COASTS.png")
centers <- readPNG("data/raw/CENTERS.png")

###
# Some Helper Functions
###

# Get colors from image (RGB(0:1,0:1,0:1))

getColor <- function(dataframe){
  data <- expand.grid(nrow(dataframe):1, 1:ncol(dataframe)) %>% 
    mutate(R = as.vector(dataframe[,,1]),
           G = as.vector(dataframe[,,2]),
           B = as.vector(dataframe[,,3])) %>% 
    rename(Y = 1, X = 2)
  return(data)
}

# Keep only unique colors

getUnique <- function(dataframe){
  data <- dataframe %>% 
    distinct(R, G, B) %>% 
    arrange(R, G, B)
  return(data)
}

# Get x,y pixel coordinates of those colors.

getCoords <- function(dataframe){
  dataRGB <- getColor(dataframe)
  uniqueRGB <- getUnique(getColor(dataframe))
  coordList <- list()
  for (i in 1:nrow(uniqueRGB)) {
    iRGB <- uniqueRGB[i,]
    pixelRGB <- dataRGB %>% 
      filter(R == iRGB$R & G == iRGB$G & B == iRGB$B) %>% 
      select(X, Y)
    coordList[[i]] <- pixelRGB
  }
  # Remove the huge empty seas
  coordList <- coordList[sapply(coordList, nrow) < 2000000]
  # Remove small polygons (imperfections from map creation <=100 pixel shit)
  coordList <- coordList[sapply(coordList, nrow) > 1000]
  # Later the holes created from removing these will be filled with `st_remove_holes(thing_with_holes)`
  # Critique: "ohhh but what about if the states are small!!! Have you considered making larger states?"
  return(coordList)
}

###
# The Big Function
###

unravelCoords <- function(dataframe, shape_type){
  coordList <- getCoords(dataframe)
  shapeList <- data.frame(paste(shape_type, 1, sep="_"),
                          st_as_sf(coordList[[1]], coords = c("X", "Y"), remove = FALSE) %>% 
                            st_as_sfc(st_bbox()) %>% 
                            st_union() %>% 
                            st_buffer(dist = 1) %>% 
                            st_remove_holes() %>% 
                            st_geometry() %>% 
                            st_as_sf()) %>% 
    rename("shape" = 1, "geometry" = 2)
  for (i in 2:length(coordList)) {
    shapeList[i,1] <- paste(shape_type, i, sep="_")
    shapeList[i,2] <- st_as_sf(coordList[[i]], coords = c("X", "Y"), remove = FALSE) %>% 
      st_as_sfc(st_bbox()) %>% 
      st_union() %>% 
      st_buffer(dist = 1) %>% 
      st_remove_holes() %>% 
      st_geometry() %>% 
      st_as_sf()
  }
  # Compute centroid of shape
  shapeList <- shapeList %>% 
    mutate(centroid = st_centroid(geometry))
  return(shapeList)
}

##
# Mapping!
##

regionData <- unravelCoords(regions, "REGION") %>% 
  st_as_sf()
provinceData <- unravelCoords(provinces, "PROVINCE") %>% 
  st_as_sf()
tileData <- unravelCoords(tiles, "TILE") %>% 
  st_as_sf()

# Save a little memory by dropping this after loading the sf objects
rm(regions, provinces, tiles)

write_rds(regionData, "data/outputs/regionData.rds")
write_rds(provinceData, "data/outputs/provinceData.rds")
write_rds(tileData, "data/outputs/tileData.rds")

# Make tils "belong" to provinces and regions according to centroid:
# (e.g., if the centroid of a province is within the geometry of a region, add the name of the region in the "of[Region/Province]" column)

# Tiles as Domains of Provinces
tileData <- tileData %>% 
  mutate(ofProvince = NA)
for (i in 1:nrow(tileData)) {
  # Check if centroid of tile[i] is inside geometry of province[i] (or later region[i])
  if(!is.na(as.numeric(as.data.frame(st_intersects(provinceData$geometry, tileData$centroid[i])))[1])){
    provinceDomainID <- as.numeric(as.data.frame(st_intersects(provinceData$geometry, tileData$centroid[i])) %>% 
                                     filter(row_number() == 1) %>% 
                                     select(row.id))
  } else {
    # For some tiles, the centroid might lay outside the shape itself (think of a very curved shape for example), so do this in that case.
    provinceDomainID <- as.numeric(as.data.frame(st_intersects(provinceData$geometry, st_cast(tileData$geometry[i], "POLYGON"))) %>% 
                                     filter(row_number() == 1) %>% 
                                     select(row.id))
  }
  tileData[i,4] <- provinceData$shape[provinceDomainID]
}
# Tiles as Domains of Regions
tileData <- tileData %>% 
  mutate(ofRegion = NA)
for (i in 1:nrow(tileData)) {
  # Check if centroid of tile[i] is inside geometry of province[i] (or later region[i])
  if(!is.na(as.numeric(as.data.frame(st_intersects(regionData$geometry, tileData$centroid[i])))[1])){
    regionDomainID <- as.numeric(as.data.frame(st_intersects(regionData$geometry, tileData$centroid[i])) %>% 
                                   filter(row_number() == 1) %>% 
                                   select(row.id))
  } else {
    # For some tiles, the centroid might lay outside the shape itself (think of a very curved shape for example), so do this in that case.
    regionDomainID <- as.numeric(as.data.frame(st_intersects(regionData$geometry, st_cast(tileData$geometry[i], "POLYGON"))) %>% 
                                   filter(row_number() == 1) %>% 
                                   select(row.id))
  }
  tileData[i,5] <- regionData$shape[regionDomainID]
}

# Save a little memory by dropping this
rm(provinceData, regionData)

# Now add the coasts!
# Same procedure as domain, but instead of the domain ID, just set coast = TRUE
# for the tiles that match.
coastData <- unravelCoords(coasts, "COAST") %>% 
  st_as_sf()
rm(coasts) # Free up memory
tileData <- tileData %>% 
  mutate(coastal = NA)
for (i in 1:nrow(tileData)) {
  coastalBool <- !is.na((as.data.frame(st_intersects(coastData$centroid, tileData$centroid[i])))[1,1])
  tileData[i,6] <- coastalBool
}
rm(coastData) # Free up memory
rm(coastalBool, provinceDomainID, regionDomainID, i) # Free up memory

# tiles + seas now! (this is the important combined data for the game map)

seaData <- unravelCoords(seas, "SEA") %>% 
  st_as_sf() %>% 
  mutate(ofProvince = NA,
         ofRegion = NA,
         coastal = FALSE)
rm(seas) # Free up memory
gameMapShapes <- tileData %>% 
  rbind(seaData)

# Save a little memory by dropping this
rm(tileData, seaData)

# plot(gameMapShapes$geometry[223]); just a rando sea to check
# plot(gameMapShapes$geometry[215]) # impassable land from tile map
# plot(gameMapShapes$geometry[216]) # impassable sea from sea map

## Drop known imperfections or just unnecessary stuff:
# plot(gameMapShapes$geometry[38])
#### This was from before I made the maps into simple squares!!!
# row 42 is full of extra transparency bullshit because GNU image manipulator sucks
#gameMapShapes <- gameMapShapes %>% 
#  filter(row_number() != 42) %>% 
## Now renumbering tiles manually
## gameMapShapes_test <- gameMapShapes
#for (i in 1:nrow(gameMapShapes)) {
#  if(substr(gameMapShapes$shape[i], 1, 4) == "TILE") {
#    if(i > 41) {
#    gameMapShapes$shape[i] <- as.character(paste(as.list((strsplit(gameMapShapes$shape[i],"_"))[[1]])[[1]], as.numeric(as.list((strsplit(gameMapShapes$shape[i],"_"))[[1]])[[2]])-1, sep="_"))
#  }} else {
#    gameMapShapes$shape[i] <- gameMapShapes$shape[i]
#  }
#}

# let's make a neat logo; you can actually see here how the seas and lands dont match up
# perfectly, but its close enough to not matter for the game and even adds some character.

favlogo <- ggplot(gameMapShapes) + geom_sf(color = "black", fill = "transparent") +
  theme_void()
favlogo_dark <- ggplot(gameMapShapes) + geom_sf(color = "white", fill = "transparent") +
  theme_void()
ggsave("data/extras/favlogo.png", favlogo, width = 2, height = 2)
ggsave("data/extras/favlogo_dark.png", favlogo_dark, width = 2, height = 2)
  
# Now search for adjacencies!
# NOTE: this is going to be inefficient, since I will actually reformat things
# latter as a list to make it nice for json, but I want to do it here first
# because reasons dataframes are easier for my primate mind.

gameMapShapes <- gameMapShapes %>% 
  mutate(adjacency_list = NA)
for (i in 1:nrow(gameMapShapes)) {
  # Check what tiles each tile intersects with for each
  adj_id <- as.data.frame(st_intersects(gameMapShapes$geometry, st_cast(gameMapShapes$geometry[i], "POLYGON"))) %>% 
    distinct(row.id) %>%
    select(row.id)
  adj_id_list <- list()
  adj_id_temp_list <- as.list(adj_id)
  for (j in 1:nrow(adj_id)) { # Not keeping self in adjacency matrix. Will add game-rule for holding own tile.
    if(unlist(adj_id_temp_list)[j] != i){
      adj_id_list[[j]] <- unlist(adj_id_temp_list)[j]
    }
  }
  gameMapShapes[i,7] <- toString(list(gameMapShapes$shape[unlist(adj_id_list)]))
}

rm(adj_id, adj_id_list, adj_id_temp_list)

# Now add the centers!
# Same procedure as coasts but center = TRUE instead of coast = TRUE
# for the tiles that match.
centerData <- unravelCoords(centers, "CENTER") %>% 
  st_as_sf()
rm(centers) # Free up memory
gameMapShapes <- gameMapShapes %>% 
  mutate(center = NA)
for (i in 1:nrow(gameMapShapes)) {
  centerBool <- !is.na((as.data.frame(st_intersects(centerData$centroid, gameMapShapes$centroid[i])))[1,1])
  gameMapShapes[i,8] <- centerBool
}
rm(centerData) # Free up memory
rm(centerBool, i) # Free up memory

# Now add "ofSea"; basically, if you have an island entirely surrounded by a sea, it is considered part of it
# this prevents cases of small islands being stacked for sea defense---e.g., in the base game:
# you cant have a ship in the canaries defending a ship in the see that surrounds them
# This is written as a nested list because imagine trying to parse two statements in that first part.
# https://stackoverflow.com/questions/24256044/comma-separated-string-to-list-in-r

# remember this vital shit: as.list(strsplit(gsub("\n","", gsub(" ", "", gsub("\"", "",gsub("[c()]", "", (gameMapShapes$adjacency_list[i]))))), ",")[[1]])

gameMapShapes <- gameMapShapes %>% 
  mutate(ofSea = FALSE)
for (i in 1:nrow(gameMapShapes)) {
  if(length(as.list(strsplit(gsub("\n","", gsub(" ", "", gsub("\"", "",gsub("[c()]", "", (gameMapShapes$adjacency_list[i]))))), ",")[[1]])) == 1) {
    if(substr(as.list(strsplit(gsub("\n","", gsub(" ", "", gsub("\"", "",gsub("[c()]", "", (gameMapShapes$adjacency_list[i]))))), ",")[[1]]), 1, 3) == "SEA") {
      gameMapShapes[i,9] <- TRUE
    }
  }
}

# Now add the booleans for buildings
# In Chicanery, there are forts and supply hubs:
# - Forts: add +1 defense to a tile
# - Supply hubs: allow you to build units as if they were in a center (moot if hub is built in a center) and allows +1 movements (to own units)
# At the start of the base game, there are no tiles with any forts/hubs (yes, ofc irl there were pre-WW1 forts, don't think about it this is a GAME)
# You *can* have a tile with both a fort and a supply hub
# If you wanted to make a map where there are forts/hubs in the base game, you could follow the same steps as earlier with the centers/coasts/etc.

gameMapShapes <- gameMapShapes %>% 
  mutate(fort = FALSE,
         supply_hub = FALSE)

# Okay, `gameMapShapes` now has everything necessary *except* country assignments. In the game,
# many tiles (and all seas) begin as unoccupied, neutral lands, but of course some begin life
# already part of a country
# This is *manual*, sowwy!

# To do this, print the map again, but with the name of each tile printed, then just write down
# what country that land belongs to. Not every tile/center should be assigned a country to start.

#labels_test <- ggplot(gameMapShapes) + geom_sf() + geom_sf_text(aes(label = shape), size = 0.5)
#ggsave("data/extras/labels_test.png", labels_test, width = 16, height = 9, dpi = 720)


# Okay, time to manually assigned some stuff!
# GBR = Britain
# FRA = France
# GER = Germany
# RUS = Russia
# AHE = Austria-Hungary
# ITA = Italy
# OTT = Ottomans

# gameMapShapes <- gameMapShapes %>% rename(shape = tile)

gameMapShapes <- gameMapShapes %>% 
  mutate(occupied_by = case_when(
    ### GBR: 6 (Notice how it holds 2 non-centers at start... stretched thin!)
    shape == "TILE_139" ~ "GBR", #center
    shape == "TILE_87" ~ "GBR", #center
    shape == "TILE_148" ~ "GBR", #center
    shape == "TILE_116" ~ "GBR", #center
    shape == "TILE_134" ~ "GBR",
    shape == "TILE_29" ~ "GBR", #center
    shape == "TILE_160" ~ "GBR", #center
    shape == "TILE_49" ~ "GBR",
    ### FRA: 5
    shape == "TILE_80" ~ "FRA", # center
    shape == "TILE_205" ~ "FRA", # center
    shape == "TILE_169" ~ "FRA", # center
    shape == "TILE_101" ~ "FRA", # center
    shape == "TILE_121" ~ "FRA", # center
    ### GER: 6 (same count as britain, but all in one place)
    shape == "TILE_194" ~ "GER", #center
    shape == "TILE_97" ~ "GER", #center
    shape == "TILE_54" ~ "GER", #center
    shape == "TILE_161" ~ "GER", #center
    shape == "TILE_10" ~ "GER", #center
    shape == "TILE_118" ~ "GER", #center
    ### RUS: 7
    shape == "TILE_165" ~ "RUS", #center
    shape == "TILE_6" ~ "RUS", #center
    shape == "TILE_152" ~ "RUS", #center
    shape == "TILE_43" ~ "RUS", #center
    shape == "TILE_212" ~ "RUS", #center
    shape == "TILE_40" ~ "RUS", #center
    shape == "TILE_209" ~ "RUS", #center
    ### AHE: 4
    shape == "TILE_5" ~ "AHE", #center
    shape == "TILE_188" ~ "AHE", #center
    shape == "TILE_79" ~ "AHE", #center
    shape == "TILE_14" ~ "AHE", #center
    ### ITA: 4 (spread between europe and north africa like a mini-france)
    shape == "TILE_195" ~ "ITA", #center
    shape == "TILE_130" ~ "ITA", #center
    shape == "TILE_26" ~ "ITA", #center
    shape == "TILE_164" ~ "ITA", #center
    ### OTT
    shape == "TILE_213" ~ "OTT", #center
    shape == "TILE_183" ~ "OTT", #center
    shape == "TILE_142" ~ "OTT", #center
    shape == "TILE_170" ~ "OTT", #center
    shape == "TILE_48" ~ "OTT", #center
    TRUE ~ "unoccupied"
  ))

# and, of course, add the starting units!
# can only be army or navy

gameMapShapes <- gameMapShapes %>% 
  mutate(unit = case_when(
    ### GBR: 6 (Notice how it holds 2 non-centers at start... stretched thin!)
    shape == "TILE_139" ~ "NAVY", #center
    # shape == "TILE_87" ~ "GBR", no unit
    shape == "TILE_148" ~ "ARMY", #center
    shape == "TILE_116" ~ "NAVY", #center
    shape == "TILE_134" ~ "NAVY",
    shape == "TILE_29" ~ "ARMY", #center
    # shape == "TILE_160" ~ "GBR", no unit
    shape == "TILE_49" ~ "NAVY",
    ### FRA: 5
    shape == "TILE_80" ~ "NAVY", # center
    shape == "TILE_205" ~ "ARMY", # center
    shape == "TILE_169" ~ "ARMY", # center
    shape == "TILE_101" ~ "ARMY", # center
    shape == "TILE_121" ~ "NAVY", # center
    ### GER: 6 (same count as britain, but all in one place)
    shape == "TILE_194" ~ "ARMY", #center
    shape == "TILE_97" ~ "ARMY", #center
    shape == "TILE_54" ~ "NAVY", #center
    shape == "TILE_161" ~ "ARMY", #center
    shape == "TILE_10" ~ "NAVY", #center
    shape == "TILE_118" ~ "ARMY", #center
    ### RUS: 7
    shape == "TILE_165" ~ "ARMY", #center
    shape == "TILE_6" ~ "NAVY", #center
    shape == "TILE_152" ~ "NAVY", #center
    shape == "TILE_43" ~ "ARMY", #center
    shape == "TILE_212" ~ "ARMY", #center
    shape == "TILE_40" ~ "ARMY", #center
    shape == "TILE_209" ~ "ARMY", #center
    ### AHE: 4
    shape == "TILE_5" ~ "ARMY", #center
    shape == "TILE_188" ~ "NAVY", #center
    shape == "TILE_79" ~ "ARMY", #center
    shape == "TILE_14" ~ "ARMY", #center
    ### ITA: 4 (spread between europe and north africa like a mini-france)
    shape == "TILE_195" ~ "ARMY", #center
    shape == "TILE_130" ~ "NAVY", #center
    shape == "TILE_26" ~ "ARMY", #center
    shape == "TILE_164" ~ "ARMY", #center
    ### OTT
    shape == "TILE_213" ~ "NAVY", #center
    shape == "TILE_183" ~ "ARMY", #center
    shape == "TILE_142" ~ "ARMY", #center
    shape == "TILE_170" ~ "ARMY", #center
    shape == "TILE_48" ~ "NAVY", #center
    TRUE ~ "none"
  ))

colnames(gameMapShapes)

# Now rearrange the columns (this is for neatness in formatting)
gameMapShapes <- gameMapShapes %>% 
  select(shape, coastal, center, ofProvince, ofRegion, ofSea, fort, supply_hub, occupied_by, unit, adjacency_list, geometry, centroid) %>% 
  rename(tile = shape) # yes I changed my mind on this, but it makes sense to be called `shape` earlier. dont think about it.

##
# Data; using `tile` as the point of reference for both files
##

## data
# create a list of everything other than geometry data and the adjacencies sublists
gameMapData_no_adj <- as.data.frame(gameMapShapes) %>% 
  select(tile, ofProvince, ofRegion, ofSea, coastal, center, fort, supply_hub, occupied_by, unit) %>% 
  as.list()

# now comes the reformatting as a list: I want a nested JSON for the adjacencies, and dataframes dont like this.
# do the same thing as "ofSea"
# remember this vital shit? as.list(strsplit(gsub("\n","", gsub(" ", "", gsub("\"", "",gsub("[c()]", "", (gameMapShapes$adjacency_list[i]))))), ",")[[1]])

adj_nested_list <- list()
for (i in 1:nrow(gameMapShapes)) {
  temp_adj_i_list <- list()
  for (j in 1:length(as.list(strsplit(gsub("\n","", gsub(" ", "", gsub("\"", "",gsub("[c()]", "", (gameMapShapes$adjacency_list[i]))))), ",")[[1]]))) {
    temp_adj_i_list[[j]] <-  toString(as.list(strsplit(gsub("\n","", gsub(" ", "", gsub("\"", "",gsub("[c()]", "", (gameMapShapes$adjacency_list[i]))))), ",")[[1]])[j])
  }
  adj_nested_list[[i]]  <- temp_adj_i_list
}

gameMapData <- gameMapData_no_adj
gameMapData[["adj"]] <- adj_nested_list
write_json(gameMapData, "data/data.json")
# if using "fromJSON" the format to get each item of the sublist is test[["adj"]][[1]][1]

# remove holes mmmmgfhgb
gameMapShapes <- gameMapShapes %>% st_remove_holes()

## geometries (in game centroids have to be calculated again)
gameMapGeometries <- gameMapShapes %>% select(tile, geometry) # it just drops the centroid anyways
st_write(gameMapGeometries, "data/map.geojson", append = FALSE)

### Saving as RData just in case
write_rds(gameMapShapes, "data/gamedata.rds")

###
# Woo-hoo!
###

# That is a complete dataframe! Let's plot something nice with it!

labels <- ggplot() + 
  geom_sf(data = gameMapShapes, color = "white") +
  geom_sf(data = (gameMapShapes %>% filter(substr(tile, 1,3) == "SEA")), fill = "blue") +
  geom_sf(data = (gameMapShapes %>% filter(substr(tile, 1,4) == "TILE")), fill = "black") +
  geom_sf(data = (gameMapShapes %>% filter(occupied_by != "unoccupied")), aes(fill = occupied_by), alpha = 0.7) +
  geom_sf_text(data = (gameMapShapes), aes(label = tile), size = 2, color = "white") +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  theme_void() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    fill = "Country"
  )
ggsave("data/extras/labels.png", labels, width = 16, height = 9)

map_plot <- ggplot() + 
  geom_sf(data = gameMapShapes, color = "white") +
  geom_sf(data = (gameMapShapes %>% filter(substr(tile, 1,3) == "SEA")), fill = "blue") +
  geom_sf(data = (gameMapShapes %>% filter(occupied_by == "unoccupied") %>% filter(!is.na(ofRegion))), fill = "beige") +
  geom_sf(data = (gameMapShapes %>% filter(occupied_by != "unoccupied")), aes(fill = occupied_by)) +
  geom_sf(data = st_as_sf(as.data.frame(gameMapShapes) %>% filter(center == TRUE) %>% select(centroid)), color = "black", size = 1.5) +
  geom_sf(data = st_as_sf(as.data.frame(gameMapShapes) %>% filter(center == TRUE) %>% select(centroid)), color = "white", size = 1) +
  geom_sf_text(data = (gameMapShapes %>% filter(unit != "none")), aes(label = unit), size = 2) +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  theme_void() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    fill = "Country"
  )
ggsave("data/extras/map.png", map_plot, width = 16, height = 9)


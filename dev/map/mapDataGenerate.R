# Tin Skoric
# Generating Map Info

# Yes, this is in R; problem?

library(dplyr)
library(ggplot2)
library(nngeo)
library(plotly)
library(png)
library(sf)
library(smoothr)
library(tidyr)

# Load raw map image files
regions <- readPNG("raw/REGIONS.png")
provinces <- readPNG("raw/PROVINCES.png")
tiles <- readPNG("raw/TILES.png")
seas <- readPNG("raw/SEAS.png")
coasts <- readPNG("raw/COASTS.png")
centers <- readPNG("raw/CENTERS.png")

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
  coordList <- coordList[sapply(coordList, nrow) < 5000000]
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

# Look at that!

#gplot(gameMapShapes, aes(fill = coastal), color = "white") + geom_sf() +
#  theme(
#    legend.position = "none"
#  )

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
  gameMapShapes[i,7] <- centerBool
}
rm(centerData) # Free up memory
rm(centerBool, i) # Free up memory

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

#ggplot(gameMapShapes) + geom_sf() + geom_sf_text(aes(label = shape), size = 1)

# Okay, time to manually assigned some stuff!
# GBR = Britain
# FRA = France
# GER = Germany
# RUS = Russia
# AHE = Austria-Hungary
# ITA = Italy
# OTT = Ottomans

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
    shape == "TILE_50" ~ "GBR",
    ### FRA: 5
    shape == "TILE_81" ~ "FRA", # center
    shape == "TILE_205" ~ "FRA", # center
    shape == "TILE_169" ~ "FRA", # center
    shape == "TILE_101" ~ "FRA", # center
    shape == "TILE_121" ~ "FRA", # center
    ### GER: 6 (same count as britain, but all in one place)
    shape == "TILE_194" ~ "GER", #center
    shape == "TILE_97" ~ "GER", #center
    shape == "TILE_55" ~ "GER", #center
    shape == "TILE_161" ~ "GER", #center
    shape == "TILE_10" ~ "GER", #center
    shape == "TILE_118" ~ "GER", #center
    ### RUS: 7
    shape == "TILE_165" ~ "RUS", #center
    shape == "TILE_6" ~ "RUS", #center
    shape == "TILE_152" ~ "RUS", #center
    shape == "TILE_44" ~ "RUS", #center
    shape == "TILE_212" ~ "RUS", #center
    shape == "TILE_40" ~ "RUS", #center
    shape == "TILE_209" ~ "RUS", #center
    ### AHE: 4
    shape == "TILE_5" ~ "AHE", #center
    shape == "TILE_188" ~ "AHE", #center
    shape == "TILE_80" ~ "AHE", #center
    shape == "TILE_156" ~ "AHE", #center
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
    shape == "TILE_49" ~ "OTT", #center
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
    shape == "TILE_50" ~ "NAVY",
    ### FRA: 5
    shape == "TILE_81" ~ "NAVY", # center
    shape == "TILE_205" ~ "ARMY", # center
    shape == "TILE_169" ~ "ARMY", # center
    shape == "TILE_101" ~ "ARMY", # center
    shape == "TILE_121" ~ "NAVY", # center
    ### GER: 6 (same count as britain, but all in one place)
    shape == "TILE_194" ~ "ARMY", #center
    shape == "TILE_97" ~ "ARMY", #center
    shape == "TILE_55" ~ "NAVY", #center
    shape == "TILE_161" ~ "ARMY", #center
    shape == "TILE_10" ~ "NAVY", #center
    shape == "TILE_118" ~ "ARMY", #center
    ### RUS: 7
    shape == "TILE_165" ~ "ARMY", #center
    shape == "TILE_6" ~ "NAVY", #center
    shape == "TILE_152" ~ "NAVY", #center
    shape == "TILE_44" ~ "ARMY", #center
    shape == "TILE_212" ~ "ARMY", #center
    shape == "TILE_40" ~ "ARMY", #center
    shape == "TILE_209" ~ "ARMY", #center
    ### AHE: 4
    shape == "TILE_5" ~ "ARMY", #center
    shape == "TILE_188" ~ "NAVY", #center
    shape == "TILE_80" ~ "ARMY", #center
    shape == "TILE_156" ~ "ARMY", #center
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
    shape == "TILE_49" ~ "NAVY", #center
    TRUE ~ "none"
  ))

###
# Woo-hoo!
###

# That is a complete dataframe! Let's plot something nice with it!

map_plot <- ggplot() + 
  geom_sf(data = gameMapShapes, color = "white") +
  geom_sf(data = (gameMapShapes %>% filter(occupied_by == "unoccupied") %>% filter(!is.na(ofRegion))), fill = "beige") +
  geom_sf(data = (gameMapShapes %>% filter(occupied_by == "unoccupied") %>% filter(is.na(ofRegion))), fill = "blue") +
  geom_sf(data = (gameMapShapes %>% filter(occupied_by != "unoccupied")), aes(fill = occupied_by)) +
  geom_sf_text(data = (gameMapShapes %>% filter(unit != "none")), aes(label = unit), size = 2) +
  scale_fill_brewer(palette = "Dark2", direction = 1) +
  theme_linedraw() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    fill = "Country"
  )
ggsave("base_map.png", map_plot, width = 16, height = 9)

##
# Data
##

writeLines(c("ID : Geometry"), "map_data/REGIONS.txt")
for (i in 1:length(region_shapes)) {
  id <- paste("REGION", i, sep = "_")
  color <- paste("(",(paste(extractColor(regions)[i,], collapse = ",")),")", sep = "")
  shapes <- paste(apply(region_shapes[[i]], 1, function(x) paste(x, collapse = ",")), collapse = "; ")

  writeLines(paste(id, color, shapes, sep = " : "), "map_data/REGIONS.txt", append = TRUE)
}

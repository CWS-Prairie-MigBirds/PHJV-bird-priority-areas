#This script quantifies the amount of priority habitat for 7 grassland bird species that occur within PHJV waterfowl target landscapes. Grassland bird priority areas were developed by the Central Grassland Birds Working Group
#and obtained from Chris Latimer at Bird Conservancy of the Rockies. These raster are presented on the sparro online tool (https://apps.birdconservancy.org/sparro/)
#Waterfowl target landscapes were developed by Ducks Unlimited Canada and obtained from the PHJV Information Repository

#The analysis is being done for a press release by Fall Flights to promote the values of waterfowl conservation work in Canada for grassland birds. 

#load packages
library(sf)
library(terra)
library(dplyr)
library(exactextractr)

#load data
#grassland bird priority areas
raster.list <- list.files("Data/GrasslandBird_PA", full.names = T)
species <- regmatches(raster.list, regexpr("(?<=binary_)[A-Z]{4}", raster.list, perl = TRUE))
gb <- rast(raster.list) 
names(gb) <- species

#waterfowl target landscapes
wf <- read_sf("Data/Waterfowl_TL/TargetLandscapes_2026_Final.shp") |>
  st_transform(crs = crs(gb))

#crop the grassland bird rasters to the waterfowl target landscapes
gb <- crop(gb, wf)

#count number of pixels of priority habitat for each bird group in each target landscape polygon
pixel.count <- exact_extract(gb, wf, fun = 'count')

#transform to area (acres) and bind results with the wf shapefile
gb.area <- pixel.count * prod(res(gb)) * 0.000247105 #(converting from m^2 to acres for the Americans)
colnames(gb.area) <- gsub("count", "area", colnames(gb.area))
wf.gb <- cbind(wf, gb.area)

plot(wf.gb |> select(area.BAIS))
plot(wf.gb |> select(area.CCLO))
plot(wf.gb |> select(area.GRSP))
plot(wf.gb |> select(area.LARB))
plot(wf.gb |> select(area.LBCU))
plot(wf.gb |> select(area.SPPI))
plot(wf.gb |> select(area.TBLO))

#The Fall Flights newsletter probably can't display 7 maps, so try summarizing to represent acres of priority area for any of the 7 grassland birds
gb.all <- any(gb, na.rm = T)
#changes 0s to NA
gb.all[gb.all == 0] <- NA
pixel.count.all <- exact_extract(gb.all, wf, fun = 'count')
wf.gb <- wf.gb |>
  mutate(
    area.all = pixel.count.all * prod(res(gb)) * 0.000247105
  )
plot(wf.gb |> select(area.all))

st_write(wf.gb, "Output/waterfowlTL_grassbirds.shp")

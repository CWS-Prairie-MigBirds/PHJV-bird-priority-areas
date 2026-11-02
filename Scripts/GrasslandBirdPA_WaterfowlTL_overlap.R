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

#first create a raster for all species that is not cropped
gb.all.full <- any(gb, na.rm = T)
#changes 0s to NA
gb.all.full[gb.all.full == 0] <- NA
#export
writeRaster(gb.all.full, "Output/grassBirdsPA_All_full.tif", overwrite = T)

#sum the rasters to create a layer representing number of species that have priority habitat
gb.sum.full <- sum(gb, na.rm = T)
#export
writeRaster(gb.sum.full, "Output/grassBirdsPA_sum_full.tif", overwrite = T)

#The above shows too much habitat. Try just the Fab 4
gb.fab4 <- gb[[names(gb) %in% c("BAIS", "CCLO", "SPPI", "TBLO")]]
#first create a raster for all species that is not cropped
gb.fab4.full <- any(gb.fab4, na.rm = T)
#changes 0s to NA
gb.fab4.full[gb.fab4.full == 0] <- NA
#export
writeRaster(gb.fab4.full, "Output/grassBirdsPA_Fab4_full.tif", overwrite = T)

#waterfowl target landscapes
wf <- read_sf("Data/Waterfowl_TL/TargetLandscapes_2026_Final.shp") |>
  st_transform(crs = crs(gb))

#crop the grassland bird rasters to the waterfowl target landscapes
gb.sum.crop <- crop(gb.sum.full, wf, mask = T, touches = F)
#export
writeRaster(gb.sum.crop, "Output/grassBirdsPA_sum_wfTL.tif", overwrite = T)

#calculate area of priority habitat for different numbers of species within waterfowl target landscapes
area.by.nsp <- expanse(gb.sum.crop, unit = "m", byValue = T)
names(area.by.nsp) <- c("layer", "n_species", "area_m2")
area.by.nsp$area_acres <- area.by.nsp$area_m2 * 0.000247105381

#total area for at least 1 species would be the sum of all. 
area.tot <- sum(area.by.nsp$area_acres)

#area for at least 7 species
area.7 <- area.by.nsp |> 
  filter(n_species > 6) |> 
  pull(area_acres) |> 
  sum()

#percentage of waterfowl target landscapes
area.perc <- sum(area.by.nsp$area_m2)/sum(st_area(wf))*100
area.perc.7 <- (area.7/(sum(st_area(wf))*0.000247105381))*100

#now try to demonstrate that a few specific target landscapes are made up almost entirely of high priorty habtiat for a large number of species
# Pull every cell value plus its area for each polygon
gb.species.by.TL <- extract(c(gb.sum.crop, cellSize(gb.sum.crop, unit = "m")),
                wf, touches = FALSE)
names(gb.species.by.TL) <- c("row", "n_species", "cell_m2")
gb.species.by.TL$PHJV_Name <- wf$PHJV_Name[gb.species.by.TL$row]

m2_to_acres <- 0.000247105381
hi <- 7 #threshold for whats considered a high number of species

TL.summary <- gb.species.by.TL |>
  group_by(PHJV_Name) |>
  summarise(
    poly_acres     = sum(cell_m2) * m2_to_acres,
    habitat_acres  = sum(cell_m2[!is.na(n_species)]) * m2_to_acres,
    pct_habitat    = habitat_acres / poly_acres * 100,
    mean_nsp       = weighted.mean(n_species, cell_m2, na.rm = TRUE),
    pct_poly_high  = sum(cell_m2[n_species >= hi], na.rm = TRUE) / sum(cell_m2) * 100,
    pct_habit_high = sum(cell_m2[n_species >= hi], na.rm = TRUE) /
      sum(cell_m2[!is.na(n_species)]) * 100
  ) |>
  arrange(desc(pct_poly_high))

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

#Produce a raster that only shows priority areas for grassland birds that are within waterfowl target landscapes
gb.wf <- mask(gb.all, wf)
plot(gb.wf)
writeRaster(gb.wf, "Output/grassBirdsPA_inWaterfowlTL.tif")
writeRaster(gb.all, "Output/grassBirdsPA_All.tif")



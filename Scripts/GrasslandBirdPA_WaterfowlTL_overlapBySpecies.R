#This scprit quantified the total area of priority habitat of each species that fall within waterfowl target landscapes

#load packages
library(sf)
library(terra)
library(dplyr)
library(tidyr)
options(scipen = 999)
#load data
#grassland bird priority areas
raster.list <- list.files("Data/GrasslandBird_PA", full.names = T)
species <- regmatches(raster.list, regexpr("(?<=binary_)[A-Z]{4}", raster.list, perl = TRUE))
gb <- rast(raster.list) 
names(gb) <- species

#waterfowl target landscapes
wf <- read_sf("Data/Waterfowl_TL/TargetLandscapes_2026_Final.shp") |>
  st_transform(crs = crs(gb))

#calculate area of priority habitat within each TL polygon
gb.wf <- extract(gb, wf, fun = "sum", na.rm = T, exact = T)

#add target landscape name and convert pixel count to area
gb.wf$TL_name <- wf$PHJV_Name

#pivot longer
gb.wf.long <- gb.wf |>
  pivot_longer(-c(ID, TL_name),
               names_to = "species",
               values_to = "count"
  ) |>
  select(-ID) |>
  #change NaN to 0 and convert to area
  mutate(count = coalesce(count, 0),
         area_acre = round(count*prod(res(gb))/4046.8564224, 0)
  )

#calculate total area across all TL polygons
gb.wf.tot <- gb.wf.long |>
  group_by(species) |>
  summarize(total_area_acre = sum(area_acre)) |>
  arrange(desc(total_area_acre))


# Project: Functional value of macroalgal biodiversity

# Title: Create a study-site map

# load the GIS layers
library(readr)
library(dplyr)
library(sp)
library(sf)
library(raster)
library(here)

# read the detailed coastline
swe_coast <- st_read("C:/Users/james/Desktop/Other/GIS_layers/Sweden_coast/land_skagerrak_kattegat.shp")
projection(swe_coast)

# convert Swedish coastline to the same projection as the exposure and salinity maps
swe_rep <- st_transform(swe_coast, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
class(swe_rep)
projection(swe_rep)

# keep only non-empty geometries
swe_rep <- swe_rep[!st_is_empty(swe_rep),,drop=FALSE]

# drop the z-dimensions
swe_rep <- st_zm(swe_rep)

# crop this file to a smaller focal area

# stage 1
e <- c(xmin = 10, xmax = 12, ymin = 57.8, ymax = 59.5)
swe_tja <- st_crop(swe_rep, e)
plot(swe_tja)

# stage 2
e <- extent(11.03, 11.18, 58.8, 58.9)
swe_tja <- crop(swe_rep, e)


# convert to spatial objects
swe_rep  <- as(swe_rep, "Spatial")
plot(swe_rep)

# load the salinity map
sal_map <- raster("C:/Users/james/Desktop/Other/GIS_layers/Salinity_Skagerrak_Kattegat.tif")
projection(sal_map)
# plot(sal_map)
sal_map@extent
res(sal_map)
extent(sal_map)

# convert the salinity map into a different projection
# convert Swedish coastline to the same projection as the exposure and salinity maps
sal_map <- projectRaster(sal_map, crs = "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
class(sal_map)
projection(sal_map)

# check if the reprojection worked
plot(sal_map)
plot(swe_rep, add = T)


# swe_tja <- st_crop(swe_rep, xmin = 273750, xmax = 280000, ymin = 6527500, ymax = 6540000)
# tja_spat <- as(swe_tja, 'Spatial')

# crop the salinity map to the extent of the Tjarno area
# sal_tja <- crop(sal_map, extent(tja_spat) )
# plot(sal_tja)


# load the study site gps points
site_info <- read_csv(file = here("preliminary_supporting_data/tile_experiment_data_site_information.csv"))

# rename the longitude and latitude data
site_info <- 
  site_info %>%
  rename(latitude = site_lat,
         longitude = site_lon)

site_points <- SpatialPointsDataFrame(coords = site_info[, c("latitude", "longitude")],
                                      proj4string = crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"),
                                      data = site_info[, c("island_name", "island_code")])
projection(site_points)
bbox(site_points)
site_points@coords



plot(swe_rep)
plot(sal_map)
plot(site_points, pch = 19, col = "red", add = T)
dev.off()


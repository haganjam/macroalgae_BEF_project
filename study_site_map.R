
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
swe_rep <- st_read("C:/Users/james/Desktop/Other/GIS_layers/Sweden_coast/land_skagerrak_kattegat.shp")
projection(swe_rep)

# keep only non-empty geometries
swe_rep <- swe_rep[!st_is_empty(swe_rep),,drop=FALSE]

# drop the z-dimensions
swe_rep <- st_zm(swe_rep)

# get the second multipolygon
swe_rep <- st_geometry(swe_rep)

# load the salinity map
sal_map <- raster("C:/Users/james/Desktop/Other/GIS_layers/Salinity_Skagerrak_Kattegat.tif")
projection(sal_map)
# plot(sal_map)
sal_map@extent
res(sal_map)
extent(sal_map)

# convert Swedish coastline to the same projection as the salinity map
swe_rep <- st_transform(swe_rep, projection(sal_map))
class(swe_rep)

# crop to the coordinates of tjarno
swe_tja <- st_crop(swe_rep, xmin = 273750, xmax = 280000, ymin = 6527500, ymax = 6540000 )

# convert Swedish coastline to the same projection as the exposure and salinity maps
swe_tja <- st_transform(swe_tja, "+proj=longlat +datum=WGS84 +no_defs")
class(swe_tja)
projection(swe_tja)

# convert the salinity map into a different projection
# convert Swedish coastline to the same projection as the exposure and salinity maps
sal_map <- projectRaster(sal_map, crs = crs(swe_tja))
class(sal_map)

# check the projections
projection(sal_map)
projection(swe_tja)

# crop the raster
st_bbox(swe_tja)
e <- as(extent(st_bbox(swe_tja)), 'SpatialPolygons')
crs(e) <- crs(swe_tja)
sal_map_r <- crop(sal_map, e)
plot(sal_map_r)

# check if the reprojection worked
plot(sal_map_r)
plot(swe_tja, add = T)

plot(swe_tja)

# load the study site gps points
site_info <- read_csv(file = here("preliminary_supporting_data/tile_experiment_data_site_information.csv"))

# rename the longitude and latitude data
site_info <- 
  site_info %>%
  rename(latitude = site_lat,
         longitude = site_lon)

site_points <- SpatialPointsDataFrame(coords = site_info[, c("longitude", "latitude")],
                                      proj4string = crs(swe_tja),
                                      data = site_info[, c("island_name", "island_code")])
projection(site_points)
bbox(site_points)
site_points@coords

st_bbox(swe_tja)
bbox(site_points)

plot(swe_tja, axes = T)
plot(sal_map_r, add = T)
plot(site_points, pch = 19, col = "red", add = T)
dev.off()

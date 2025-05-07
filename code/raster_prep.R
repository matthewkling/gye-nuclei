
# prep rasters from files too large to include in repo ---------------------------------


# load packages
library(terra)
library(tidyverse)
library(sf)
library(patchwork)
library(geojsonsf)


# climate data (CHELSA mean annual temperature)
r <- rast("/Volumes/T7/CHELSA/v2/raw/CHELSA_bio1_1981-2010_V.2.1.tif")
r2 <- list.files("/Volumes/T7/CHELSA/v2/cmip/", full.names = T)
r2 <- r2[grepl("2041", r2) & grepl("585_tas_", r2)] %>%
      rast()


# elevation data (for vis)
elev <- rast("../nuclei_figures/data/dem_USGS30m.tif") %>%
      aggregate(3) %>%
      terrain(c("slope", "aspect"), unit = "radians")
hsr <- shade(elev$slope, elev$aspect) %>%
      aggregate(3)
writeRaster(hsr, "data/hillshade.tif", overwrite = T)


# GYE & YNP boundaries
gye <- read_csv("data/gye.csv") %>% # from https://www.arcgis.com/home/item.html?id=b55466380b614c1dbf686d2baf149b5b%2F1000
      select(x, y) %>%
      mutate(group = 1) %>%
      st_as_sf(coords = c("x", "y")) %>%
      group_by(group) %>%
      summarise(do_union = FALSE) %>%
      st_cast("POLYGON") %>%
      st_set_crs(3857) %>%
      st_transform(crs(r))
yell <- st_read( "data/YELL_tracts/YELL_boundary.shp") %>%
      st_transform(crs(r)) %>%
      st_as_sf()


# mask rasters to GYE
y <- r %>% crop(gye) %>% mask(yell)
r <- r %>% crop(gye) %>% mask(gye)
r2 <- r2 %>% crop(gye) %>% mask(gye) %>% mean()
r <- c(r, r2, y) %>%
      setNames(c("t1", "t2", "park"))
writeRaster(r, "data/climate.tif", overwrite = T)







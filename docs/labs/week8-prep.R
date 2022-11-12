library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(here)
library(tmap)

rm(list = ls())
here::i_am("labs/week8.Rmd")

landsat_20180612 <-rast(here("labs", "data", "week8", "landsat_20180612.tif"))
landsat_20180815 <- rast(here("labs", "data", "week8", "landsat_20180815.tif"))
landsat_20181018 <- rast(here("labs", "data", "week8", "landsat_20181018.tif"))
landsat_20181103 <- rast(here("labs", "data", "week8", "landsat_20181103.tif"))
landsat_20190122 <- rast(here("labs", "data", "week8", "landsat_20190122.tif"))
landsat_20190223 <- rast(here("labs", "data", "week8", "landsat_20190223.tif"))
landsat_20190412 <- rast(here("labs", "data", "week8", "landsat_20190412.tif"))
landsat_20190701 <- rast(here("labs", "data", "week8", "landsat_20190701.tif"))

names(landsat_20180612) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
names(landsat_20180815) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
names(landsat_20181018) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
names(landsat_20181103) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
names(landsat_20190122) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
names(landsat_20190223) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
names(landsat_20190412) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
names(landsat_20190701) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")

ndvi_fun = function(nir, red){
  (nir - red) / (nir + red)
}

ndvi_20180612 <- lapp(landsat_20180612[[c(4, 3)]], fun = ndvi_fun)
ndvi_20180815 <- lapp(landsat_20180815[[c(4, 3)]], fun = ndvi_fun)
ndvi_20181018 <- lapp(landsat_20181018[[c(4, 3)]], fun = ndvi_fun)
ndvi_20181103 <- lapp(landsat_20181103[[c(4, 3)]], fun = ndvi_fun)
ndvi_20190122 <- lapp(landsat_20190122[[c(4, 3)]], fun = ndvi_fun)
ndvi_20190223 <- lapp(landsat_20190223[[c(4, 3)]], fun = ndvi_fun)
ndvi_20190412 <- lapp(landsat_20190412[[c(4, 3)]], fun = ndvi_fun)
ndvi_20190701 <- lapp(landsat_20190701[[c(4, 3)]], fun = ndvi_fun)

all_ndvi <- c(ndvi_20180612,
              ndvi_20180815,
              ndvi_20181018,
              ndvi_20181103,
              ndvi_20190122,
              ndvi_20190223,
              ndvi_20190412,
              ndvi_20190701)

names(all_ndvi) <- c("2018-06-12",
                     "2018-08-15",
                     "2018-10-18",
                     "2018-11-03",
                     "2019-01-22",
                     "2019-02-23",
                     "2019-04-12",
                     "2019-07-01")

sites <- st_read(here("labs","data","week8","study_sites.shp"))

tm_shape(ndvi_20180612) +
  tm_raster() +
  tm_shape(sites) +
  tm_polygons()

sites_ndvi <- terra::extract(all_ndvi, sites, fun = "mean")

sites_annotated <- cbind(sites, sites_ndvi)

sites_clean <- sites_annotated %>%
  st_drop_geometry() %>%
  select(-ID) %>%
  pivot_longer(!study_site) %>%
  rename("NDVI" = value) %>%
  mutate("year" = str_sub(name, 2, 5),
         "month" = str_sub(name, 7, 8),
         "day" = str_sub(name, -2, -1)) %>%
  unite("date", 4:6, sep = "-") %>%
  mutate("date" = lubridate::as_date(date))

ggplot(sites_clean,
       aes(x = date, y = NDVI,
           group = study_site, col = study_site)) +
  geom_line()


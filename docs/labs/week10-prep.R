rm(list = ls())
# Load needed packages
library(terra)
library(sf)
library(tidyverse)
library(tmap)
library(here)

# set working directory to ensure R can find the file we wish to import and where
here::i_am("labs/week10-prep.R")
setwd(here())

dsm <- rast("./labs/data/week10/SJER2013_DSM.tif")
dtm <- rast("./labs/data/week10/SJER2013_DTM.tif")

# use raster math to create CHM
chm <- dsm - dtm

## ----read-veg--------------------------------------------------------



# import the centroid data and the vegetation structure data

# read in plot centroids
centroids <- st_read("./labs/data/week10/PlotCentroids/SJERPlotCentroids_Buffer.shp")


# read in vegetation heights
# this means all strings of letter coming in will remain character
options(stringsAsFactors=FALSE)

vegStr <- read.csv("./labs/data/week10/VegetationData/D17_2013_vegStr.csv") %>%
  group_by(plotid) %>%
  summarise("insituHeight" = max(stemheight, na.rm = TRUE))


tm_shape(chm) +
  tm_raster() +
  tm_shape(centroids) +
  tm_polygons()

extract_height <- terra::extract(chm, centroids, fun = max) %>%
  rename(chmHeight = SJER2013_DSM) %>%
  select(chmHeight)

centroids <- cbind(centroids, extract_height) %>%
  left_join(.,vegStr, by = c("Plot_ID" = "plotid"))

ggplot(centroids, aes(y=chmHeight, x=insituHeight)) +
  geom_abline(slope=1, intercept=0, alpha=.5, lty=2) + #plotting our "1:1" line
  geom_point() +
  geom_smooth(method = lm) + # add regression line and confidence interval
  ggtitle("Validating Lidar measurements") +
  xlab("Maximum Measured Height (m)") +
  ylab("Maximum Lidar Height (m)")

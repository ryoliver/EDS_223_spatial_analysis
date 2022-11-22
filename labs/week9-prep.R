library(sf)
library(terra)
library(here)
library(dplyr)
library(rpart)
library(rpart.plot)
library(tmap)

rm(list = ls())

here::i_am("labs/week9-prep.R")
setwd(here())

filelist <- list.files("./labs/data/week9/landsat-data/", full.names = TRUE)
landsat_20070925 <- rast(filelist)

names(landsat_20070925) <- c("blue", "green", "red", "NIR", "SWIR1", "SWIR2")
plotRGB(landsat_20070925, r = 3, g = 2, b = 1, stretch = "lin")

SB_county_south <- st_read("./labs/data/week9/SB_county_south.shp")

SB_county_south <- st_transform(SB_county_south, crs = crs(landsat_20070925))

landsat_cropped <- crop(landsat_20070925, SB_county_south)

landsat_masked <- mask(landsat_cropped, SB_county_south)

rm(landsat_20070925, SB_county_south, landsat_cropped)

rcl <- matrix(c(-Inf, 7273, NA,
                 43636, Inf, NA), ncol = 3, byrow = TRUE)


landsat <- classify(landsat_masked, rcl = rcl)
plotRGB(landsat, r = 3, g = 2, b = 1, stretch = "lin")
summary(landsat)

landsat <- (landsat * 0.0000275 - 0.2) * 100

training_data <- st_read("./labs/data/week9/trainingdata.shp") %>%
  st_transform(., crs = crs(landsat))

training_data_values <- extract(landsat, training_data, df = TRUE)

training_data_attributes <- training_data %>%
  st_drop_geometry()

SB_training_data <- left_join(training_data_values, training_data_attributes,
                              by = c("ID" = "id")) %>%
  mutate(type = as.factor(type))

SB_formula <- type ~ red + green + blue + NIR + SWIR1 + SWIR2

SB_decision_tree <- rpart(formula = SB_formula,
                          data = SB_training_data,
                          method = "class",
                          na.action = na.omit)

prp(SB_decision_tree)


SB_classification <- predict(landsat, SB_decision_tree, type = "class", na.rm = TRUE)
SB_classification[[1]]
levels(SB_training_data$type)
plot(SB_classification)

tm_shape(SB_classification) +
  tm_raster(style = "cat",
            palette = c("#8DB580", "#F2DDA4", "#7E8987", "#6A8EAE"),
            labels = c("green vegetation",
                           "soil/dead grass",
                           "urban",
                           "water"),
            title = "land cover") +
  tm_layout(legend.position = c("left", "bottom"))



library(terra)
library(dplyr)
library(spData)
library(spDataLarge)
library(tmap)

# Let's create a SpatRaster object using a digital elevation model for Zion National Park

raster_filepath = system.file("raster/srtm.tif", package = "spDataLarge")
my_rast = rast(raster_filepath)
class(my_rast)

# typing the name into the console, gives us some info on the raster object
my_rast


plot(my_rast)

tm_shape(my_rast) +
  tm_raster()

# we can also create rasters from scratch using the rast function
# here we create 36 cells centerd around (0,0)
# by default the CRS is set to WGS84, but we could change this with the crs argument
# because we are working in WGS84, the resolution is in units of degress
# raster fills the values of the cells row-wise starting in the upper left corner.

new_raster = rast(nrows = 6, ncols = 6, resolution = 0.5,
                  xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
                  vals = 1:36)

tm_shape(new_raster) +
  tm_raster()

# The SpatRaster class can also handle multiple layers
multi_raster_file = system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast = rast(multi_raster_file)
multi_rast

nlyr(multi_rast)

# we can subset layers using either the layer number or name
multi_rast3 = subset(multi_rast, 3)
multi_rast4 = subset(multi_rast, "landsat_4")

# we can combine SpatRaster objects into one, using the c function
multi_rast34 = c(multi_rast3, multi_rast4)

# let's create an example raster for elevation
elev = rast(nrows = 6, ncols = 6, resolution = 0.5,
            xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
            vals = 1:36)

# rasters can also hold categorial data
# let's create one soil types

grain_order = c("clay", "silt", "sand")
grain_char = sample(grain_order, 36, replace = TRUE)
grain_fact = factor(grain_char, levels = grain_order)
grain = rast(nrows = 6, ncols = 6, resolution = 0.5,
             xmin = -1.5, xmax = 1.5, ymin = -1.5, ymax = 1.5,
             vals = grain_fact)

grain_char
cats(grain)

## raster subsetting
# we can index rasters using, row-column indexing, cell IDs, coordinates, other spatial object

# row 1, column 1
elev[1, 1]
# cell ID 1
elev[1]

# if we had a two layered raster, subsetting would return the values in both layers
two_layers = c(grain, elev)
two_layers[1]

# we can also modify/overwrite cell values
elev[1, 1] = 0
elev[]

# replacing values in multi-layer rasters requires a matrix with as many columns as layers and rows as replaceable cells
two_layers[1] = cbind(c(1), c(4))
two_layers[]

# summarizing raster objects
# we can get info on raster values just by typing the name or using the summary function
elev

# we can get global summaries, such as standard deviation
global(elev, sd)

# or we can use freq to get the counts with categories
freq(grain)

hist(elev)

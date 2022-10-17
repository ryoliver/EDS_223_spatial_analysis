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

## spatial subsetting
# we can move from subsetting based on specific cell IDs to extract info based on spatial objects

# to use coordinates for subsetting, we can "translate" coordinates into a cell ID
# with the **terra** function "cellFromXY()" or "terra::extract()"

id = cellFromXY(elev, xy = matrix(c(0.1, 0.1), ncol = 2))
elev[id]
# the same as
terra::extract(elev, matrix(c(0.1, 0.1), ncol = 2))

# raster objects can also subset with another raster object
# here we extract the values of our elevation raster that fall within the extent
# of a masking raster
clip = rast(xmin = 0.9, xmax = 1.8, ymin = -0.45, ymax = 0.45,
            resolution = 0.3, vals = rep(1, 9))

elev[clip]

# we can also use extract
terra::extract(elev, ext(clip))

# in the previous example, we just got the values back
# in some cases, we might want the output to be the raster cells themselves
# we can do this use the "[" operator and setting "drop = FALSE"

# this example returns the first 2 cells of the first row of the "elev" raster
elev[1:2, drop = FALSE]

# another common use of spatial subsetting is when we use one raster with the same
# extent and resolution to mask the another
# in this case, the masking raster needs to be composed of logicals or NAs

# create raster mask of the same resolution and extent
# randomly replace values with NA and TRUE to use as a mask
rmask <- elev
values(rmask) <- sample(c(NA, TRUE), 36, replace = TRUE)

# spatial subsetting
elev[rmask, drop = FALSE]           # with [ operator
mask(elev, rmask)                   # with mask()

# we can also use a similar approach to replace values that we suspect are incorrect

elev[elev < 20] = NA

## map algebra
# here we define map algebra as the set of operations that modify or summarize raster cell values
# with reference to surrounding cells, zones, or statistical functions that apply to every cell

# local operations
# local operations are computed on each cell individually
# we can use oridinary arithemetic or logical statements

elev + elev
elev^2
log(elev)
elev > 5


# we can also classify intervals of values into groups
# for example, we could classify a DEM into low, middle, and high elevation cells

# first we need to construct a reclassification matrix
# the first column corresponds to the lower end of the class
# the second column corresponds to the upper end of the class
# the third column corresponds to the new value for the specified ranges in columns 1 and 2

rcl = matrix(c(0, 12, 1, 12, 24, 2, 24, 36, 3), ncol = 3, byrow = TRUE)
rcl

# we then use this matrix to reclassify our elevation matrix
recl <- classify(elev, rcl = rcl)
recl

# for more efficient processing, we can use a set of map algebra functions
# "app()" applies a function to each cell of a raster to summarize the values of multiple layers into one layer
# "tapp()" is an

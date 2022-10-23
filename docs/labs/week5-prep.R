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
# "tapp()" is an extension of "app()" that allows us to apply on operation on a subset of layers
# "lapp()" allows us to apply a function to each cell using layers as arguments

# we can use the "lapp()" function to compute the Normalized Difference Vegetation Index (NDVI)

# let's calculate NDVI for Zion National Park using multispectral satellite data
multi_raster_file <- system.file("raster/landsat.tif", package = "spDataLarge")
multi_rast <- rast(multi_raster_file)

# we need to define a function to calculate NDVI
ndvi_fun = function(nir, red){
  (nir - red) / (nir + red)
}

# so now we can use "lapp()" to calculate NDVI in each raster cell
# to do so, we just need the NIR and red bands
ndvi_rast <- lapp(multi_rast[[c(4, 3)]], fun = ndvi_fun)

tm_shape(ndvi_rast) +
  tm_raster()

# focal operations
# local operations operate on one cell, though from multiple layers
# focal operations take into account a central (focal) cell and its neighbors
# the neighborhood (or kernel, moving window, filter) can take any size or shape
# a focal operation applies an aggregation function to all cells in the neighborhood
# and updates the value of the central cell before moving on to the next central cell

# we can use the focal() function to perform spatial filtering
# we define the size, shape, and weights of the moving window using a matrix
# here we find the minimum

r_focal <- focal(elev, w = matrix(1, nrow = 3, ncol = 3), fun = min)

plot(elev)
plot(r_focal)

# zonal operations
# similar to focal operations, zonal operations apply an aggregation function to mutliple cells
# however, instead of applying operations to neighbors, zonal operations aggregate based on "zones"
# zones can are defined using a categorical raster and do not necessarily have to be neighbros

# for example, we could find the average elevation for different soil grain sizes
zonal(elev, grain, fun = "mean")

## geometric operations

# extent and origin

# when merging or performing map algebra, rasters need to match in their resolution,
# projection, origin, and/or extent

# in the simplest case, two images differ only in their extent
# let's start by increasing the extent of a elevation raster

elev = rast(system.file("raster/elev.tif", package = "spData"))
elev_2 = extend(elev, c(1, 2)) # add one row and two columns

plot(elev)
plot(elev_2)

# performing algebraic operations on objects with different extents doesn't work
elev + elev_2

# we can align the extent of the 2 rasters using the extend() function.
# here we extend the elev object to the extent of elev_2 by adding NAs

elev_4 <- extend(elev, elev_2)

# the origin function returns the coordinates of the cell corner closes to the coordinates (0,0)
# we can also manually change the origin
origin(elev_4)
origin(elev_4) <- c(0.25, 0.25)
origin(elev_4)

# aggregation and disaggregation

# raster datasets can also differ in their resolution
# to match resolutions we can decrease the resolution by aggregating
# or increase the resolution by disaggregating

# let's start by changing the resolution of a DEM by a factor of 5, by taking the mean

dem <- rast(system.file("raster/dem.tif", package = "spDataLarge"))
dem_agg <-  aggregate(dem, fact = 5, fun = mean)

plot(dem)
plot(dem_agg)

# we have some choices when increasing the resolution
# here we try the bilinear method

dem_disagg <- disagg(dem_agg, fact = 5, method = "bilinear")
identical(dem, dem_disagg)
plot(dem_disagg)

# resampling

# aggregation/disaggregation work when both rasters have the same origins
# what do we do in the case where we have two or more rasters with different origins and resolutions?
# resampling computes values for new pixel locations based on custom resolutions and origins

# in most cases, the target raster would be an object you are already working with
# but here we define a target raster
target_rast = rast(xmin = 794600, xmax = 798200,
                   ymin = 8931800, ymax = 8935400,
                   resolution = 150, crs = "EPSG:32717")

dem_resampl = resample(dem, y = target_rast, method = "bilinear")


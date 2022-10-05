library(sf)
library(dplyr)
library(spData)
library(tmap)
rm(list = ls())
canterbury <- nz %>%
  filter(Name == "Canterbury")

c_height <- nz_height[canterbury, ]

tm_shape(nz) +
  tm_polygons() +
  tm_shape(nz_height) +
  tm_dots(col = "red")

tm_shape(nz) +
  tm_polygons() +
  tm_shape(canterbury) +
  tm_polygons(col = "blue") +
  tm_shape(c_height) +
  tm_dots(col = "red")

outside_height <- nz_height[canterbury, , op = st_disjoint]

tm_shape(nz) +
  tm_polygons() +
  tm_shape(canterbury) +
  tm_polygons(col = "blue") +
  tm_shape(outside_height) +
  tm_dots(col = "red")



sel_sgbp <- st_intersects(x = nz_height, y = canterbury)
sel_sgbp
sel_logical <- lengths(sel_sgbp) > 0
c_height2 <- nz_height[sel_logical, ]

c_height3 <- nz_height %>%
  st_filter(y = canterbury, .predicate = st_intersects)

outside_height2 <- nz_height %>%
  st_filter(y = canterbury, .predicate = st_disjoint)


set.seed(2018)
bb <- st_bbox(world)

random_df <- data.frame(
  x = runif(n = 10, min = bb[1], max = bb[3]),
  y = runif(n = 10, min = bb[2], max = bb[4])
)

random_points <- random_df %>%
  st_as_sf(coords = c("x", "y")) %>%
  st_set_crs("EPSG:4326")

tm_shape(world) +
  tm_fill() +
  tm_shape(random_points) +
  tm_dots(col = "red")

world_random <- world[random_points, ]
tm_shape(world) +
  tm_fill() +
  tm_shape(world_random) +
  tm_fill(col = "red")


random_joined  <- st_join(random_points, world)

tm_shape(world) +
  tm_fill() +
  tm_shape(random_joined) +
  tm_dots(col = "name_long")

random_joined_inner <- st_join(random_points, world, left = FALSE)

tmap_mode("view")
tm_shape(cycle_hire) +
  tm_dots(col = "blue", alpha = 0.5) +
  tm_shape(cycle_hire_osm) +
  tm_dots(col = "red", alpha = 0.5)

any(st_touches(cycle_hire, cycle_hire_osm, sparse = FALSE))

head(cycle_hire_osm)
head(cycle_hire)

sel <- st_is_within_distance(cycle_hire, cycle_hire_osm, dist = 20)
summary(lengths(sel) > 0)


z <- st_join(cycle_hire, cycle_hire_osm, st_is_within_distance, dist = 20)
nrow(cycle_hire)
nrow(z)

z <- z %>%
  group_by(id) %>%
  summarise(capacity = mean(capacity))

plot(cycle_hire_osm["capacity"])
plot(z["capacity"])

nz_agg <- st_join(nz, nz_height) %>%
  group_by(Name) %>%
  summarise(elevation = mean(elevation, na.rm = TRUE))
tmap_mode("plot")
tm_shape(nz_agg) %>%
  tm_dots()

head(incongruent)
head(aggregating_zones)

tm_shape(incongruent) +
  tm_polygons() +
  tm_shape(aggregating_zones) +
  tm_borders(col = "red")

iv <- incongruent["value"]
agg_aw <- st_interpolate_aw(iv, aggregating_zones, extensive = TRUE)

tm_shape(agg_aw) +
  tm_fill(col = "value")

nz_highest <- nz_height %>%
  slice_max(n = 1, order_by = elevation)

canterbury_centroid = st_centroid(canterbury)
st_distance(nz_highest, canterbury_centroid)

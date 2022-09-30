rm(list = ls())

library(tmap)
library(spData)

lnd_point = st_point(c(0.1, 51.5))
lnd_geom = st_sfc(lnd_point, crs = 4326)

lnd_attrib = data.frame(
  name = "London",
  temperature = 25,
  date = as.Date("2017-06-21")
)
lnd_sf = st_sf(lnd_attrib, geometry = lnd_geom)

lnd_sf
class(lnd_sf)


class(world)
dim(world)

summary(world$lifeExp)

world_df <- st_drop_geometry(world)
class(world_df)
ncol(world)
world

world1 <- world %>%
  select(name_long, pop)

world1

world2 <- world %>%
  select(-subregion, -area_km2)

names(world2)

world3 <- world %>%
  select(name_long, population = pop)

names(world3)

world4 <- world %>%
  filter(area_km2 < 10000)
summary(world4$area_km2)

world5 <- world %>%
  filter(lifeExp >= 80)
nrow(world5)


world %>%
  filter(continent == "Asia") %>%
  select(name_long, continent, lifeExp) %>%
  slice_max(lifeExp)

continents <- world %>%
  group_by(continent) %>%
  summarize(population = sum(pop, na.rm = TRUE))

tm_shape(continents) +
  tm_polygons(col = "population",
              style = "cont")

world %>%
  group_by(continent) %>%
  summarize(population = sum(pop, na.rm = TRUE),
            area_km2 = sum(area_km2, na.rm = TRUE),
            n_countries = n())

world %>%
  st_drop_geometry() %>%
  group_by(continent) %>%
  summarize(population = sum(pop, na.rm = TRUE),
            area_km2 = sum(area_km2, na.rm = TRUE),
            n_countries = n()) %>%
  mutate(density = round(population/area_km2)) %>%
  slice_max(density, n = 3) %>%
  arrange(desc(n_countries))

head(coffee_data)
nrow(coffee_data)
nrow(world)

world_coffee <- left_join(world, coffee_data,
                          by = "name_long")

names(world_coffee)

tm_shape(world_coffee) +
  tm_polygons(col = "coffee_production_2017")

world_coffee_inner <- inner_join(world, coffee_data)
nrow(world_coffee_inner)

setdiff(coffee_data$name_long, world$name_long)

drc = stringr::str_subset(world$name_long, "Dem*.+Congo")

coffee_data$name_long[grepl("Congo,", coffee_data$name_long)] = drc

world_coffee_inner <- inner_join(world, coffee_data)
nrow(world_coffee_inner)

tm_shape(world_coffee_inner) +
  tm_polygons(col = "coffee_production_2017")

coffee_world = left_join(coffee_data, world, by = "name_long")
class(coffee_world)
names(coffee_world)

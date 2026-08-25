##CALCULATING DISTANCE BETWEEN NESTS AT SAME SITE


library(geosphere)

library(sf)
#> Linking to GEOS 3.8.0, GDAL 3.0.4, PROJ 6.3.1

nests25 <- read.csv("nests_25_location.csv")

nests25_sf <- st_as_sf(nests25, coords = c("long", "lat"), crs = 4326)

st_distance(nests25_sf)

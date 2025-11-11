install.packages("spatstat")
install.packages("sp")
library(spatstat)
library(here)
library(sp)
library(tmap)
library(sf)
library(tmaptools)
library(stringr)
library(dplyr)

#loading in London Borough Spatial Data
london_borough <- st_read(here::here("statistical-gis-boundaries-london", "statistical-gis-boundaries-london", "ESRI", "London_Borough_Excluding_MHW.shp"))
plot(london_borough)

borough_map <- london_borough%>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(.,27700)
qtm(borough_map)

#loading in blue plaque data
blue_plaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")

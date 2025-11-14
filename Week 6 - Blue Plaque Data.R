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
library(tidyverse)

#loading in London Borough Spatial Data
london_borough <- st_read(here::here("statistical-gis-boundaries-london", "statistical-gis-boundaries-london", "ESRI", "London_Borough_Excluding_MHW.shp"))
plot(london_borough)

#THIS IS JUST DUE DILLIGENCE, YOU NEVER KNOW IF/WHEN THE DATASET CHANGES/UPDATES
borough_map <- london_borough%>%
  dplyr::filter(str_detect(GSS_CODE, "^E09"))%>%
  st_transform(.,27700) #CRs 27700 is British Nat'l Grid
qtm(borough_map) #qtm stands for "quick thematic map"

#loading in blue plaque data
blue_plaques <- st_read("https://s3.eu-west-2.amazonaws.com/openplaques/open-plaques-london-2018-04-08.geojson")
blue_plaques <- blue_plaques%>%
  st_transform(.,27700)

#plotting blue plaques on map
tmap_mode("plot") #static map
tm_shape(borough_map) + 
  tm_polygons(fill_alpha = 0.5) + #alpha = transparency
tm_shape(blue_plaques) + #fillin with blue plaque data
  tm_dots(fill = "blue", size = 0.1) #symbol shape(color, size)

#getting rid of duplicate data values
blue_plaques <- distinct(blue_plaques)

#selecting only points within London borough boundaries
blue_plaques_sub <- blue_plaques[borough_map, ]

#mapping the new blue_plaques_sub data
tm_shape(borough_map) + 
  tm_polygons(fill_alpha = 0.5) + #alpha = transparency
tm_shape(blue_plaques_sub) + #fillin with blue plaque data
  tm_dots(fill = "blue", size = 0.1) #symbol shape(color, size)

#creates new df with just plaque values that fall within borough_map boundaries
intersect_indices <-st_intersects(borough_map, blue_plaques) 

#performing Ripley's K analysis on ONE borough
#this is because it requires a lot of computing power, too many points may tank the analysis
harrow <- borough_map%>%
  filter(., NAME == "Harrow")

#displays Harrow borough on map
tm_shape(harrow) +
  tm_polygons(col = NA, fill_alpha = 0.5)

#clipping so that blue_plaque data from ONLY Harrow displays on map
blue_plaques_harrow <- blue_plaques[harrow, ]

#mapping bluq_plaques_harrow within harrow map
tm_shape(harrow) +
  tm_polygons(fill_alpha = 0.5) +
tm_shape(blue_plaques_harrow) +
  tm_dots(fill = "blue", size = 0.1)

#setting a window to start analysis (spatstat requires a window for ppp)
window <- as.owin(harrow)
plot(window)

#converting blue_plaques_sub to sp object suitable for spatstat ppp analysis
blue_plaques_harrow_sub <- blue_plaques_harrow%>%
  as(., "Spatial") #'.' means "take all current data and turn it 'spatial'

#starting ppp analysis
blue_plaques_harrow_sub_ppp <- ppp(x = blue_plaques_harrow_sub@coords[,1], #means "all rows, first column (x)
                            y = blue_plaques_harrow_sub@coords[,2], #means "all rows, second column (y)
                            window = window) #uses window of harrow from line 69
#PLOTTING; pch = plotted point shape, 25 options (1-25); 
#cex = character expansion aka plotted point size, 1 = default, 0.5 = half, 2 = double;
#main = title text
plot(blue_plaques_harrow_sub_ppp, pch = 16, cex = .5, main = "Blue Plaques - Harrow", cols = "blue") 

#plotting Kernel Density Estimation
blue_plaques_harrow_sub_ppp%>%
  density(., sigma = 500)%>%  #'.' means "use all current data", blue_plaques_harrow_sub_ppp;
  #sigma = the spread of the heat map around the point in units as CRS (meters in this case),
  # sigma = 100 (small), sigma = 500 (medium), sigma = 1000 (large)
  plot(., main = "Kernel Density Estimation - Harrow Blue Plaques")




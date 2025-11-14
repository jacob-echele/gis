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

#plotting Kernel Density Estimation ("?density.ppp" for help)
blue_plaques_harrow_sub_ppp%>%
  density(., sigma = 500)%>%  #'.' means "use all current data", blue_plaques_harrow_sub_ppp;
  #sigma = the spread of the heat map around the point in units as CRS (meters in this case),
  # sigma = 100 (small), sigma = 500 (medium), sigma = 1000 (large)
  plot(., main = "Kernel Density Estimation - Harrow Blue Plaques")

#QUADRAT ANALYSIS
#plot Quadrat Analysis (counted number of points within grided window) ("?quadratcount.ppp" for help)
blue_plaques_harrow_sub_ppp%>%
  quadratcount(., nx = 6, ny = 6)%>% #nx= no. of horizontal squares, ny = no. of vertical squares
  plot(., col = "red", main = "Quadrat Count - Harrow Blue Plaques") #col colors the count inside each square
#then plot blue plaque points
#add = TRUE means you add the next plot (plaque locations) onto the quadrat map, not making a new one
plot(blue_plaques_harrow_sub_ppp, pch = 16, cex = 0.5, add = TRUE, col = "blue", main = "Blue Plaques - Harrow")

#turning the Quadrat results into a frequency table (df)
quad_count <- blue_plaques_harrow_sub_ppp%>%
  quadratcount(., nx = 6, ny = 6)%>%
  as.data.frame()%>% #creates df
  dplyr::count(Var1 = Freq)%>% #squares with 0-7 values ("Freq" = frequency)
  dplyr::rename(Freqquadrantcount = n) #frequency of squares with values of 1-7; n = number of times a square has x points within it

#making sure the data type is numeric (integer)
quad_count%>%
  summarize_all(class)

###########################################################
#   CALCULATING EXPECTED VALUES FOR POISSON DISTRIBUTION
###########################################################
sums <- quad_count%>%
  mutate(total = Var1 * Freqquadrantcount)%>% #adding new column with total (Var1 * Freqquadrantcount)
  dplyr::summarize(across(everything(), sum))%>% #sums up all the columns in the df
  dplyr::select(-Var1) #removes Var1 column from the result; leaves with just total no. of squares and total blue plaques

lambda <- quad_count%>% #lambda = mean for Poisson Distribution
  mutate(total = Var1 * Freqquadrantcount)%>% #adding new column with total
  dplyr::summarize(across(everything(), sum))%>% #sums all the columns in the df
  mutate(lambda=total/Freqquadrantcount)%>% #calculating lambda (mean); average no. of plaques per quadrant
  dplyr::select(lambda)%>% #selecting (keeping) only the lambda column
  pull(lambda) #making lambda a numeric value, NOT a df

quad_count_table <- quad_count%>% #calculating probability and expected values
  mutate(probability =((lambda^Var1)*exp(-lambda))/factorial(Var1))%>% #new column for calculating probability; exp = exponent, factorial = x!
  mutate(Expected=(round(probability*sums$Freqquadrantcount, 0))) #new column fo

####################################################################
#   PLOTTING OBSERVED VS EXPECTED VALUES FOR POISSON DISTRIBUTION
####################################################################
plot(c(1,7), c(0, 14), type = "n", #setting x and y axis ranges
xlab = "Number of Blue Plaques (Red = Observed, Blue = Expected)",
  ylab = "Frequency of Occurances",
  main = "Observed vs. Expected Values - Blue Plaques in Harrow")
points(quad_count_table$Freqquadrantcount, #selecting observed values
       col = "red",
       type = "o", #"overplotted points and lines"; https://www.datacamp.com/doc/r/line for full list for Line Charts in R
       lwd = 3) #line width
points(quad_count_table$Expected, #selecting expected values
       col = "blue",
       type = "o",
       lwd = 3)


install.packages("spatstat")
install.packages("sp")
install.packages("fpc")
install.packages("dbscan")
install.packages("ggspatial")
install.packages("prettymapr")
library(spatstat)
library(here)
library(sp)
library(tmap)
library(sf)
library(tmaptools)
library(stringr)
library(dplyr)
library(tidyverse)
library(fpc) #for DBSCAN analysis
library(dbscan)
library(ggplot2)
library(ggspatial) #for mapping convex polygon shapes
library(prettymapr)

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

####################################################
#   PERFORMING RIPLEY'S K ANALYSIS ON ONE BOROUGH
####################################################
#this is because it requires a lot of computing power, too many points may tank the analysis

#selecting Harrow borough b/c it has a manageable amount of bluq plaque points
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

############################
#   STARTING PPP ANALYSIS
############################
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

#######################
#   QUADRAT ANALYSIS
#######################

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
       type = "o", #"overplotted points and lines"; https://www.datacamp.com/doc/r/line for full list of options for Line Charts in R
       lwd = 3) #line width
points(quad_count_table$Expected, #selecting expected values
       col = "blue",
       type = "o",
       lwd = 3)

#######################
#   CHI-SQUARED TEST
#######################
test_stats <- quadrat.test(blue_plaques_harrow_sub_ppp, nx = 6, ny = 6, method = "Chisq")
#produces p-value of 0.25939 impying complete spatial randomness (CSR, NOT to be confused with CRS)

#plotting Chi-Squared
plot(blue_plaques_harrow_sub_ppp, pch = 16, cex = .5, main = "Chi-Squared: Blue Plauqes - Harrow") #original Harrow map w/ blue plaque points
  plot(blue_plaques_harrow_sub_ppp, add = TRUE, col = "blue") #turning points blue
plot(test_stats, add = TRUE, col = "red") #adding chi-squared to map

######################
#   Ripley's K Test
######################
#tells us if data is clustering or if it is dispersed and random
K_test <- blue_plaques_harrow_sub_ppp%>% #calculates the average density of points for a circle w/ radius r
  Kest(., correction = "border")%>%
  plot()

#turning Ripley's K into a df
K_test_values <- as.data.frame(Kest(blue_plaques_harrow_sub_ppp, correction = "Ripley"))
  #Kpois(r) = theoretical value of K for each distance window (r) under a Poisson assumption of Complete Spatial Randomness
  #Kbord(r) or the black line = estimated values of K accounting for the effects of the edge of the study area
  #where the value of K (Kpois(r)) falls above the line (Kbord(r)), the data appear to be clustered at that distance
  #where the value of K is below the line, the data are dispersed
  #in this case, data is clustered until about 1300m and then is random and dispersed from 1600-2100m
  #height of clustering is ~700m

######################
#   DBSCAN Analysis
######################
#tells us WHERE in the area of interest the data clustering is occurring

#extracting points from the spatial points df
blue_plaques_harrow_sub_points <- blue_plaques_harrow_sub%>%
  coordinates(.)%>%
  as.data.frame()

#running analysis
harrow_dbscan <- blue_plaques_harrow_sub_points%>%
  fpc::dbscan(., 700, MinPts = 4) 
#fpc::dbscan(x, y, z);
#y = eps (epsilon) = radius (meters, in this case) within which the algorithm should search for clusters
#z = minimum no. of points that should be considered a cluster

#plotting DBSCAN analysis
plot(harrow_dbscan, blue_plaques_harrow_sub_points, main = "DBSCAN Output - Harrow", frame = TRUE)
plot(borough_map$geometry, add = TRUE)

############################################
#   USING kNNdisplot TO FIND BEST EPSILON
############################################
blue_plaques_harrow_sub_points%>%
  dbscan::kNNdistplot(., k = 4)
  #use the "knee" for epsilon (eps) in fpc::dbscan function
  #the knee is the point on the graph where a sharp change occurs along the k-distance curve
  #in this case, about 700m

#######################################
#   CREATING BETTER MAP FOR CLUSTERS
#######################################
  #also going to be adding convex polygons enclosing all the points in each cluster

blue_plaques_harrow_sub_points <- blue_plaques_harrow_sub_points%>%
  mutate(dbcluster = harrow_dbscan$cluster) #adds column showing which cluster every blue plaque belongs to

#convert to sf for mapping purposes
blue_plaques_harrow_sub_points_sf <- st_as_sf(blue_plaques_harrow_sub_points, #conversion from st to sf
                                              coords = c("coords.x1", "coords.x2"), #columns from blue_plaques_harrow_sub_points
                                              crs = 27700) #british nat'l grid CRS

#making polygons
convex_polygons <- blue_plaques_harrow_sub_points_sf%>%
  filter(dbcluster>0)%>% #removing any points not in a cluster; cluster >= 1
  group_by(dbcluster)%>% #keeping clusters together; 1s, 2s, 3s, etc. together
  summarize(geometry = st_combine(geometry))%>% #combining the blue plaque points
  mutate(geometry= st_convex_hull(geometry))%>% #creating convex polygons
  st_as_sf() #converting to sf

#creating map with polygons USING GGSPATIAL PACKAGE
ggplot() +
  annotation_map_tile(zoom = 13) + #higher the zoom level, more close up the map will be
  geom_sf(data = blue_plaques_harrow_sub_points_sf, aes(color=dbcluster), size = 3) + #colors the points based on their cluster; aes = "aesthetic"
  geom_sf(data = convex_polygons, aes(fill = dbcluster), #colors the polygons based on their cluster
          alpha = 0.5,
          color = NA, #removes the border lines of the polygons
          show.legend = FALSE) #hides legend for polygons b/c the colors already shows the different clusters

theme_bw()

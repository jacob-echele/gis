library(here)
library(janitor)
library(sf)
library(tidyverse)
library(tmap)
library(stringr)
library(dplyr)
library(spdep)
install.packages("RColorBrewer")

#read the ward data in
#Geographic London Ward Data
LondonWards <- st_read(here::here("statistical-gis-boundaries-london", "ESRI", "London_Ward.shp"))

LondonWardsMerged <- st_read(here::here("statistical-gis-boundaries-london", 
                                            "ESRI",
                                            "London_Ward_CityMerged.shp"))%>% #adding city data
  st_transform(.,27700) #change CRS to British standard

#ward demographic data
WardData <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv",
                         locale = locale(encoding = "latin1"),
                         na = c("NA", "n/a")) %>% 
  clean_names()

#joining spatial data with ward data, keeping important columns for spatial autocorrelation
LondonWardsMerged <- LondonWardsMerged %>% 
  left_join(WardData, 
            by = c("GSS_CODE" = "new_code"))%>%
  dplyr::distinct(GSS_CODE, .keep_all = T)%>%
  dplyr::select(GSS_CODE, ward_name, average_gcse_capped_point_scores_2014)

LondonWardsMerged <- st_transform(LondonWardsMerged, 27700)

#look at CRS
st_crs(LondonWardsMerged)

#load in blue plaque data
BluePlaques <- read_csv(here::here("archive", "open-plaques-all-2017-06-19.csv")) %>%
  drop_na(longitude, latitude) %>%              # remove missing coords
  st_as_sf(coords = c("longitude", "latitude"), # create point geometry
           crs = 4326)%>%
  filter(., area == "London")

BluePlaques <- st_transform(BluePlaques, 27700)

st_crs(BluePlaques)

#quick look of plaques on wards
tmap_mode("plot")
tm_shape(LondonWardsMerged) +
  tm_polygons(fill_alpha = 0.5) +
  tm_shape(BluePlaques) +
  tm_dots(fill = "blue", size=.1)

##############
#   EXAMPLE
##############

#example map
example <- st_intersects(LondonWardsMerged, BluePlaques)
example

#check example
Hidecheck_example <- LondonWardsMerged%>%
  st_join(BluePlaques)%>%
  filter(ward_name=="Kingston upon Thames - Coombe Hill")

#example map
points_sf_joined <- LondonWardsMerged%>%
  mutate(n = lengths(st_intersects(., BluePlaques)))%>%
  janitor::clean_names()%>%
  #calculate area
  mutate(area=st_area(.))%>%
  #then density of the points per ward
  mutate(density=n/area)%>%
  #select density and some other variables 
  dplyr::select(density, ward_name, gss_code, n, average_gcse_capped_point_scores_2014)

points_sf_joined<- points_sf_joined %>%                    
  group_by(gss_code) %>%         
  summarise(density = first(density),
            wardname= first(ward_name),
            plaquecount= first(n))

tm_shape(points_sf_joined) +
  tm_polygons(
    fill = "density",
    fill.scale = tm_scale_intervals(
      values = "brewer.blues",
      style="jenks"),
    # set the legend
    fill.legend = tm_legend(title="Blue Plaque Density",
                            title.size=0.85,
                            size=0.8,
                            # plot outside of the main map
                            #explained below
                            position=tm_pos_out("right", 
                                                "center",
                                                pos.v = "center")))
############################
#   SPATIAL WEIGHT MATRIX
############################

#spatial weight matrix mapped
coordsW <- points_sf_joined%>%
  st_centroid()%>% #from center of each polygon
  st_geometry()

plot(coordsW, axes = TRUE) #axes are latitude and longitude?

#create a neighbours list
LWard_nb <- points_sf_joined %>%
  poly2nb(., queen=T)
summary(LWard_nb)

# -----------------------
#   PLOTTING NEIGHBORS
# -----------------------

#plot them
plot(LWard_nb, st_geometry(coordsW), col="red")
#add a map underneath
plot(points_sf_joined$geometry, add=T)

#different matrix style
#create a spatial weights matrix from these weights
Lward.lw <- LWard_nb %>%
  nb2mat(., style="B") #binary style

sum(Lward.lw) #produces 3680 neighbors
sum(Lward.lw[1,]) #number of neighbors in row 1

####################################################
#   MORAN'S I (ARE VALUES CLUSTERED OR DISPERSED)
####################################################

Lward.lw <- LWard_nb %>%
  nb2listw(., style="C") #globally standardized

I_LWard_Global_Density <- points_sf_joined %>% #take geographic data
  pull(density) %>% #take density (calculated earlier) from geographic data
  as.vector()%>% #keep as vector
  moran.test(., Lward.lw) 
  #"moran.tes(., zero.policy = TRUE)" would allow for the function to permit spatial units with no polygons

I_LWard_Global_Density #produces 0.66, some clustering (across entire study area)

#####################################################################
#   GEARY'S C (ARE SIMILAR VALUES OR DISSIMILAR VALUES CLUSTERED?)
#####################################################################

C_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  geary.test(., Lward.lw)

C_LWard_Global_Density #produces .42, neighbors with similar values are clustering (compared to random chance); neighbors are similar

################################################################
#   GETIS ORD (TELLS US IF HIGH OR LOW VALUES ARE CLUSTERING)
################################################################

G_LWard_Global_Density <- 
  points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  globalG.test(., Lward.lw)

G_LWard_Global_Density #produces G = 1.1367 e-02, expectation = 1.6026e-03; G > E, therefore high values are clustering

###################################################################################
#   LOCAL MORAN'S I (DIFFERENCE COMPARED TO NEIGHBORING VALUES WITHIN A POLYGON)
###################################################################################

#use the localmoran function to generate I for each ward in the city
I_LWard_Local_count <- points_sf_joined %>%
  pull(plaquecount) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

I_LWard_Local_Density <- points_sf_joined %>%
  pull(density) %>%
  as.vector()%>%
  localmoran(., Lward.lw)%>%
  as_tibble()

#what does the output (the localMoran object) look like?
slice_head(I_LWard_Local_Density, n=5)

#mutating for mapping purposes
points_sf_joined <- points_sf_joined %>%
  mutate(plaque_count_I = as.numeric(I_LWard_Local_count$Ii))%>%
  mutate(plaque_count_Iz =as.numeric(I_LWard_Local_count$Z.Ii))%>%
  mutate(density_I =as.numeric(I_LWard_Local_Density$Ii))%>%
  mutate(density_Iz =as.numeric(I_LWard_Local_Density$Z.Ii))

# -----------------------------
#    MAPPING LOCAL MORAN'S I
# ----------------------------=

#setting breaks
breaks1<-c(-1000,-2.58,-1.96,-1.65,1.65,1.96,2.58,1000)

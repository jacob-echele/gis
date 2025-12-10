library(tidyverse)
library(tmap)
library(plotly)
library(broom)
library(mapview)
library(sf)
library(sp)
library(spdep)
library(car)
library(fs)
library(janitor)
library(here)
library(dplyr)
library(tidypredict)
library(tidymodels)

#################################
#   LOADING IN GEOGRAPHIC DATA
#################################

#download file with boundaries
download.file("https://data.london.gov.uk/download/statistical-gis-boundary-files-london/9ba8c833-6370-4b11-abdc-314aa020d5e0/statistical-gis-boundaries-london.zip", 
              destfile="statistical-gis-boundaries-london.zip")

#getting the .zip file and extracting it
listfiles <- dir_info(here::here()) %>%
  dplyr::filter(str_detect(path, ".zip")) %>%
  dplyr::select(path) %>%
  pull() %>%
  #print out the .gz file
  print() %>%
  as.character() %>%
  utils::unzip(exdir = here::here())

#looking what's inside the .zip
Londonwards<-fs::dir_info(here::here("statistical-gis-boundaries-london", 
                                     "ESRI"))%>%
  #$ means exact match
  dplyr::filter(str_detect(path, 
                           "London_Ward_CityMerged.shp$"))%>%
  dplyr::select(path)%>%
  dplyr::pull()%>%
  #read in the file in
  sf::st_read()

#checking the data
qtm(Londonwards)

################################
#   LOADING IN ATTRIBUTE DATA
################################

#read in attribute data
LondonWardProfiles <- read_csv("https://data.london.gov.uk/download/f33fb38c-cb37-48e3-8298-84c0d3cc5a6c/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                               col_names = TRUE, 
                               locale = locale(encoding = 'Latin1'))

#check all of the columns have been read in correctly; see column data types
Datatypelist <- LondonWardProfiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")
Datatypelist

# -----------------
#   CLEANING DATA
# -----------------

#read in some data - couple of things here. Read in specifying a load of likely 'n/a' values, also specify Latin1 as encoding as there is a pound sign (£) in one of the column headers - just to make things fun!
LondonWardProfiles <- read_csv("https://data.london.gov.uk/download/ward-profiles-and-atlas/772d2d64-e8c6-46cb-86f9-e52b4c7851bc/ward-profiles-excel-version.csv", 
                               na = c("", "NA", "n/a"), 
                               locale = locale(encoding = 'Latin1'), 
                               col_names = TRUE)

#check all of the columns have been read in correctly; see column data types
Datatypelist <- LondonWardProfiles %>% 
  summarise_all(class) %>%
  pivot_longer(everything(), 
               names_to="All_variables", 
               values_to="Variable_class")

Datatypelist

############################
#   MAPPING ORIGINAL DATA
############################

#merge boundaries and data
LonWardProfiles <- Londonwards%>%
  left_join(.,
            LondonWardProfiles, 
            by = c("GSS_CODE" = "New code"))


#let's map our dependent variable to see if the join has worked:
tmap_mode("plot")
qtm(LonWardProfiles, 
    fill = "Average GCSE capped point scores - 2014", 
    fill.palette = "Blues")

#################################
#   LOADING IN ADDITIONAL DATA
#################################

#secondary schools
#might be a good idea to see where the secondary schools are in London too
london_schools <- read_csv("https://data.london.gov.uk/download/146392df-b051-42ad-b8ec-454e440f0f8b/57046151-39a0-45d9-8dc0-27ea7fd02de8/all_schools_xy_2016.csv")

#from the coordinate values stored in the x and y columns, which look like they are latitude and longitude values, create a new points dataset
lon_schools_sf <- st_as_sf(london_schools, 
                           coords = c("x","y"), 
                           crs = 4326)

#create mappable sf data frame
lond_sec_schools_sf <- lon_schools_sf %>%
  filter(PHASE=="Secondary")

# --------------------------------
#   MAPPING OF SECONDARY SCHOOLS
# --------------------------------

tmap_mode("plot")
qtm(lond_sec_schools_sf, size=0.2)

###########################
#   REGRESSION MODELLING
###########################

# -------------------
#    TEST PLOTTING 
# -------------------

#plotting Absence % vs GCSE score
q <- qplot(x = `Unauthorised Absence in All Schools (%) - 2013`, 
           y = `Average GCSE capped point scores - 2014`, 
           data=LonWardProfiles)

#plot with a regression line
q + stat_smooth(method="lm", se=FALSE, size=1) + 
  geom_jitter() #rounds x-scale
#shows results that could indicate a relationship

# --- cleaning data then running model ---
#run the linear regression model and store its outputs in an object called model1
Regressiondata<- LonWardProfiles%>%
  clean_names()%>%
  dplyr::select(average_gcse_capped_point_scores_2014, 
                unauthorised_absence_in_all_schools_percent_2013)

#now model
model1 <- Regressiondata %>%
  lm(average_gcse_capped_point_scores_2014 ~
       unauthorised_absence_in_all_schools_percent_2013,
     data=.)

#shows summary statistics (see 8.5.3.1 in practical for interpretation)
summary(model1)

# --- cleaner model ---
tidy(model1) #makes a cleaned tibble with statistical findings from model
glance(model1) #basically <variable>.head() in python; more summary information

#shows results from each point in model and adds to column; in this case modelling GCSE values with
#unauthorized absenses
Regressiondata%>%
  tidypredict_to_column(model1)

# ------------------------------------------
#   EXPERIMENTING WITH TIDYMODELS PACKAGES
# ------------------------------------------

#tidymodels cannot do spatial modelling currently (as of 12/10/2025)

# set the model
lm_mod <- linear_reg()

# fit the model
lm_fit <- 
  lm_mod %>% 
  fit(average_gcse_capped_point_scores_2014 ~
        unauthorised_absence_in_all_schools_percent_2013,
      data=Regressiondata)

# we cover tidy and glance in a minute...
tidy(lm_fit)
glance(lm_fit)

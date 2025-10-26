library(dplyr)
library(here)
library(janitor)
library(tmap)
library(tmaptools)
library(sf)
library(ggplot2)
library(countrycode)
library(tidyverse)
library(usethis)

#loading in data
world_data <- read_csv("HDR25_Composite_indices_complete_time_series.csv",
                                         na="NULL",
                                         locale = locale(encoding = "latin1"))

#loading in geopackage
world_outline <- st_read("World_Countries_(Generalized)_9029012925078512962.geojson")%>%
  select(ISO)
plot(world_outline)
        
        
#creating df for 2010 data
world_data_2010 <- world_data%>%
  dplyr::select(
    contains("iso3"),
    contains("country"),
    contains("gdi_2010"),
  )

#creating df for 2019 data
world_data_2019 <- world_data%>%
  dplyr::select(
    contains("iso3"),
    contains("country"),
    contains("gdi_2019"),
  )

#creating df for difference
world_data_df <- world_data_2010%>% #initial 2010 data
  select(c(iso3, country, gdi_2010))
world_data_df <- world_data_df%>% #adding 2019 data
  left_join(.,world_data_2019)

world_data_difference <- world_data_df%>% #creating the difference
  select(iso3, country, gdi_2019, gdi_2010)%>% #keeping columns
  mutate( #actual math
    gdi_difference = gdi_2019 - gdi_2010
  )
str(world_data_difference)

#cleaning names
world_data_difference <- world_data_difference%>%
  clean_names()

#creating values with iso2 country codes from iso3  
iso2 <- countrycode(world_data_difference$iso3, 
                                          origin = "iso3c", 
                                          destination = "iso2c", 
                                          warn = TRUE,
                                          nomatch = NA,
                                          custom_dict = NULL,
                                          custom_match = NULL,
                                          origin_regex = NULL
)

iso2 #Produces "Some values were not matched unambiguously: ZZA.VHHD, ZZB.HHD, ZZC.HHD, ..."

#adding iso2 values to world_data_difference
world_data_difference <- world_data_difference%>%
  add_column(iso2, .before= "iso3")

#merging world_data_difference and world_outline
merged_world_difference <- world_outline%>%
  left_join(world_data_difference, by = c("ISO" = "iso2"))

#plotting merged_data_difference
GGII_merged_world_difference <- ggplot(merged_world_difference) +
  geom_sf(aes(fill = gdi_difference), alpha = 1) +
  scale_fill_gradient2(
    low = "#C72000",     
    mid = "white",       
    high = "#00B029",    
    midpoint = 0,        
    na.value = "grey", 
    name = "GDI Difference",
    labels = scales::label_number(accuracy = 0.01),
    limits = c(-0.09, 0.09) #limits to provide more detail through color variation; Yemen is extreme outlier and made most of the map white
  ) +
  labs(
    title = "Difference in Global Gender Inequality Index (2010–2019)",
  ) +
  theme_minimal() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 10),
    legend.title = element_text(size = 10),
    plot.title = element_text(size = 16, face = "bold"),
    panel.grid = element_blank()
  )
plot(GGII_merged_world_difference)

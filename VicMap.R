###make a map for govhack 2024

#install.packages(c("leaflet","sf","janitor"))
library("leaflet")
library("sf")
library("janitor")
library("tidyverse")

vic_lga_shapefile <- read_sf("Data/LGA_2024_AUST_GDA94/LGA_2024_AUST_GDA94.shp") %>% 
  clean_names() %>% 
  filter(ste_name21 == "Victoria") %>%
  rename(lga_code = lga_code24) %>% 
  mutate(lga_code = as.numeric(lga_code)) %>% 
  #importing from randomforest code
  left_join(service_demand_2031, by ="lga_code")  %>%
  slice(1:79) %>% 
  mutate(additional_workforce_need = pmax(additional_workforce_need, 0)) %>% 
  #replace(is.na(.), 0) %>% 
  st_transform('+proj=longlat +datum=WGS84')

bins <- c(0, 100, 200, 500, 1000, 1500, 2000,10000)

# Create the leaflet map
map <- leaflet() %>%
  addProviderTiles(providers$OpenStreetMap) %>%  # Add base map tiles
  addPolygons(data = vic_lga_shapefile,  # Add LGA polygons
              fillColor = ~colorBin("Blues", vic_lga_shapefile$additional_workforce_need,bins=bins)(additional_workforce_need), # Assuming 'population' is a field
              color = "black",
              weight = 0.5,
              opacity = 1,
              fillOpacity = 0.7,
              popup = ~paste0("LGA: ", vic_lga_shapefile$lga_name24, "<br>Additional health and workforce staff required in LGA: ", vic_lga_shapefile$additional_workforce_need)) %>%  # Popup content
  addLegend(pal = colorBin("Blues", vic_lga_shapefile$additional_workforce_need,bins=bins), 
            values = as.numeric(vic_lga_shapefile$additional_workforce_need), 
            title = "Additional health and social service workforce demand by 2031",
            position = "bottomright")  # Add a legend

# Print the map
map

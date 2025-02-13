library(tidyverse)
library(sf)
library(mapview) 
library(webshot)
library(leaflet)
library(rnaturalearth)

# read data
location <- read.csv(here::here("data/service_delivery_site_data.csv"))

stock_tidy <- read_rds(here::here("data/tidy_data/stock_tidy.rds")) |> 
  drop_na() |> 
  group_by(site_code) |> 
  summarise(stock_distributed = sum(stock_distributed), .groups = 'drop') |> 
  left_join(location, by = 'site_code') |> 
  filter(stock_distributed > 0)


# Define the custom color palette
custom_palette <- c("Hospital" = "#8B0000",       # Dark Red
                    "University Hospital/National Institute" = "#00008B",  # Dark Blue
                    "Health Center" = "#006400")  # Dark Green

# Create the color palette function
pal <- colorFactor(palette = custom_palette, domain = stock_tidy$site_type)

# Get Côte d'Ivoire map data
civ_map <- ne_countries(country = "Ivory Coast", returnclass = "sf")


# Create the leaflet map
m <- leaflet(stock_tidy) %>%
  addTiles() %>%
  setView(lng = -5.5, lat = 7.5, zoom = 7) %>%
  addPolygons(data = civ_map, fill = FALSE, color = "black", weight = 2) %>%
  addCircleMarkers(
    ~site_longitude, ~site_latitude, 
    radius = ~sqrt(stock_distributed) / 15,
    color = ~pal(site_type),
    stroke = TRUE, 
    fillOpacity = 0.6, 
    popup = ~paste("Site:", site_code, "<br>",
                   "Stock Distributed:", stock_distributed, "<br>",
                   "Type:", site_type, "<br>",
                   "Region:", site_region, "<br>",
                   "District:", site_district)
  ) %>%
  addLegend(pal = pal, values = ~site_type, opacity = 1, title = "Site Type", position = "topright")

# Display the map
m

mapshot(m, file = "manuscript/phd_wp1_hybrid_model/map/Cote_dIvoire_map.png")





# Script gets all the locations for all the stops
# and plots those on a map.
# The purpose of this is just a sanity check to 
# see if the locations make any sense.


options('stringsAsFactors' = FALSE)
setwd('druktemeter/druktemeter_ftp')
library(data.table)
library(dplyr)
library(leaflet)
library(leaflet.extras)

library(readxl)
library(ggplot2)

# read ritten data
ritten <- fread('parsed_data/GVB/ritten.csv', sep = ';')

# get locations of stations
locations_start <- ritten %>%
  filter(!duplicated(halte_start)) %>%
  select_('ort_start', 'halte_start', 'lat_start', 'lng_start') %>%
  rename(
    ort = ort_start,
    halte = halte_start,
    lat = lat_start,
    lng = lng_start
  )
locations_end <- ritten %>%
  filter(!duplicated(halte_end)) %>%
  select_('ort_end', 'halte_end', 'lat_end', 'lng_end') %>%
  rename(
    ort = ort_end,
    halte = halte_end,
    lat = lat_end,
    lng = lng_end
  )

# combine all locations of stations
locations <- bind_rows(locations_start, locations_end) %>% unique()

# fire up map of locations
ritten %>%
  filter(start_time == '12:00:00') %>%
  leaflet() %>%
  setView(lng = 4.889545, lat = 52.35, zoom = 12) %>%
  addProviderTiles(
    providers$CartoDB.Positron,
    options = providerTileOptions(minZoom = 5, maxZoom = 20)) %>%
  addHeatmap(
    lat = ~lat_end,
    lng = ~lng_end,
    intensity = ~tot_ritten,
    radius = 10,
    min = 0,
    max = 3300)

library(shiny)
library(shinythemes)
library(shinyBS)
library(data.table)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(reshape2)
library(RColorBrewer)
library(dplyr)
library(tidyr)
library(readxl)
library(maptools)
library(magrittr)
library(scales)

invnorm <- function(var) {
  out <- rank(var) - 0.5
  out[is.na(var)] <- NA
  out <- out/(max(out, na.rm = T) + 0.5)
  out <- qnorm(out)
  return(out)
}

# focus map settings
all_sets <- list(
  'Overview' = list(zoom = 14, lng = 4.904415, lat = 52.375086),
  'Rembrandtplein' = list(zoom = 16, lng = 4.895347, lat = 52.363869),
  'Wallengebied' = list(zoom = 16, lng = 4.895516, lat = 52.371904),
  'Westerpark' = list(zoom = 16, lng = 4.871038, lat = 52.38526),
  'GVB' = list(zoom = 12, lng = 4.889545, lat = 52.35)
)

# read historical popularity data
popularity_historical <- fread('data/popularity_historical.txt', data.table = FALSE)

# list of types available
all_types <- popularity_historical$types %>%
  strsplit(split = ';') %>%
  unlist %>%
  unique() %>%
  sort()
types_categories <- list(
  'Alle' = all_types,
  'Bar' = c('bar', 'cafe', 'night_club'),
  'Restaurant' = c('restaurant', 'food', 'meal_takeaway'),
  'Supermarkt' = c('convenience_store', 'grocery_or_supermarket'),
  'Winkels' = c('book_store', 'clothing_store', 'department_store', 'furniture_store', 'home_goods_store', 'jewelry_store', 'liquor_store', 'shoe_store', 'shopping_mall', 'store')
)

# persbakken data
persbak_data <- read_xlsx('data/persbak_data_edit.xlsx', sheet = 'inworppatroon', col_names = TRUE)
persbak_locaties <- read_xlsx('data/persbak_data_edit.xlsx', sheet = 'econview', col_names = TRUE) %>%
  filter(!is.na(X), !is.na(Y))

# fix dagen
persbak_data$dag <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')[persbak_data$weekdag]

# rembrandtplein locations
rembrandt_locations <- c(
  'data/locations_rembrandtplein_1.csv',
  'data/locations_rembrandtplein_2.csv',
  'data/locations_rembrandtplein_3.csv',
  'data/locations_rembrandtplein_4.csv'
) %>%
  lapply(FUN = fread, data.table = FALSE) %>%
  bind_rows()

# merge rembrandtplein popularity en persbak
drukte_data <- popularity_historical %>%
  filter(place_id %in% rembrandt_locations$place_id)
afval_data <- persbak_data %>%
  group_by(dag, uur) %>%
  summarize(overall_afval = mean(inworpen_per_uur)) %>%
  mutate(uur = uur + 1) %>%
  mutate(unique_id = paste(dag, uur))

# voetganger hotspots
shapes <- 'data/voetganger_hotspots.kml' %>%
  getKMLcoordinates(ignoreAltitude = TRUE) %>%
  lapply(FUN = function(x) {
    x %>%
      Line() %>%
      list() %>%
      Lines(ID = 1) %>%
      list() %>%
      SpatialLines(proj4string = CRS('+init=epsg:4326'))
  })

# GVB data
days <- c(
  'ma' = 'Monday', 'di' = 'Tuesday', 'wo' = 'Wednesday', 'do' = 'Thursday',
  'vr' = 'Friday', 'za' = 'Saturday', 'zo' = 'Sunday')
gvb_ritten <- fread('data/gvb_ritten_long.csv', sep = ';') %>%
  mutate(
    day = days[Weekdag],
    hour = hour - 1
  ) %>%
  filter(hour > 5)

# aantal instappers per halte, per tijdsperiode
gvb_instap <- gvb_ritten %>%
  group_by(day, hour, instaphalte) %>%
  summarize(
    in_uit = 'instap',
    lat = mean(instaphalte_lat),
    lng = mean(instaphalte_lng),
    tot_instap = sum(tot_ritten)
  ) %>%
  rename(halte = instaphalte) %>%
  mutate(merge_id = paste(day, hour, lat, lng, halte, sep = '_'))

# aantal instappers per halte, per tijdsperiode
gvb_uitstap <- gvb_ritten %>%
  group_by(day, hour, uitstaphalte) %>%
  summarize(
    in_uit = 'uitstap',
    lat = mean(uitstaphalte_lat),
    lng = mean(uitstaphalte_lng),
    tot_uitstap = sum(tot_ritten)
  ) %>%
  rename(halte = uitstaphalte) %>%
  mutate(merge_id = paste(day, hour, lat, lng, halte, sep = '_'))

# specifiek voor Rembrandtplein
f <- function(x) {
  indx <- x == max(x)
  x[indx] <- max(x[!indx])
  return(x)
}
rembrandt <- gvb_ritten %>%
  filter(uitstaphalte == 'Rembrandtplein') %>%
  filter(instaphalte != uitstaphalte) %>%
  group_by(Weekdag, day, hour, instaphalte, uitstaphalte) %>%
  summarize(
    passagiers = sum(tot_ritten),
    instaphalte_lat = mean(instaphalte_lat),
    instaphalte_lng = mean(instaphalte_lng),
    uitstaphalte_lat = mean(uitstaphalte_lat),
    uitstaphalte_lng = mean(uitstaphalte_lng)
  ) %>%
  group_by(day) %>%
  mutate(passagiers = as.vector(log(passagiers + 1)),
         passagiers = f(passagiers))

# totaal aantal mensen bij halte
gvb_totaal <- merge(
  gvb_instap[, c('merge_id', 'tot_instap')],
  gvb_uitstap[, c('merge_id', 'tot_uitstap')],
  by = 'merge_id', all = TRUE
) %>%
  mutate(
    day = merge_id %>% strsplit(split = '_') %>% sapply(FUN = function(x) x[1]),
    hour = merge_id %>% strsplit(split = '_') %>% sapply(FUN = function(x) x[2]) %>% as.numeric(),
    lat = merge_id %>% strsplit(split = '_') %>% sapply(FUN = function(x) x[3]) %>% as.numeric(),
    lng = merge_id %>% strsplit(split = '_') %>% sapply(FUN = function(x) x[4]) %>% as.numeric(),
    halte = merge_id %>% strsplit(split = '_') %>% sapply(FUN = function(x) x[5])
  )
gvb_totaal$merge_id <- NULL
gvb_totaal$tot_instap[is.na(gvb_totaal$tot_instap)] <- 0
gvb_totaal$tot_uitstap[is.na(gvb_totaal$tot_uitstap)] <- 0
gvb_totaal$totaal <- gvb_totaal$tot_instap + gvb_totaal$tot_uitstap

# transform
gvb_totaal <- gvb_totaal %>%
  group_by(day, halte) %>%
  mutate(
    tot_instap = drop(scale(tot_instap)), #invnorm(tot_instap),
    tot_uitstap = drop(scale(tot_uitstap)), #invnorm(tot_uitstap),
    totaal = drop(scale(tot_instap + tot_uitstap)) #invnorm(tot_instap + tot_uitstap)
  )

# CMSA data vs. Google
cmsa <- fread('data/CMSA_google_to_shiny.csv')
cmsa$popularity <- cmsa$popularity / 13
cmsa$In_3_normalized <- cmsa$Time <- cmsa$week <- NULL
cmsa$In_3_normalized <- 100 * cmsa$In_3_normalized


time_plot <- function(x, y1, y2, isdate = TRUE) {
  xrng <- range(x)
  yrng1 <- range(y1)
  yrng2 <- range(y2)
  
  # plot Google expected vs. afval (Rembrandtplein)
  font_size <- 2
  line_width <- 3
  par(mar = c(5,5,2,5))
  
  # plot points
  plot(x, y1,
       type = 'l', col = 'red', axes = FALSE, lwd = line_width,
       xlab = NA, ylab = NA, xlim = xrng, ylim = yrng1)
  
  # create x-axis
  axis(side = 1, at = c(7, 10, 13, 16, 19, 22))
  mtext(side = 1, line = 3, 'Tijd', cex = font_size)
  
  # create first y-axis
  axis(side = 2, col = 'red', col.axis = "red")
  mtext(side = 2, line = 3, 'Instappers Centraal Station', cex = font_size)
  
  par(new = TRUE)
  
  # plot points
  plot(x, y2,
       type = 'l', col = 'blue', axes = FALSE, lwd = line_width,
       xlab = NA, ylab = NA, cex = 1.2, ylim = yrng2)
  
  # create second y-axis
  axis(side = 4, col = 'blue', col.axis = 'blue')
  mtext(side = 4, line = 3, 'Uitstappers Rembrandtplein', cex = font_size)
}

# Reads MORA data and writes as csv

read_mora <- function() {
  # inlezen data, fix datum
  mora <- fread('druktemeter/druktemeter_ftp/raw_data/MORA/MORA_data_data.csv', sep = ';') %>%
    filter(Hoofdrubriek != '')
  
  # fix header names
  names(mora) <- gsub(' ', '_', names(mora))
  
  # fix datum, selecteer kolommen
  days <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
  mora <- mora %>%
    mutate(
      year = substr(Datum_melding, 7, 10) %>% as.numeric(),
      month = substr(Datum_melding, 4, 5) %>% as.numeric(),
      day = substr(Datum_melding, 1, 2) %>% as.numeric(),
      datum = as.Date(Datum_melding, format = '%d-%m-%Y'),
      day_of_week_num = format(datum, format = '%u') %>% as.numeric(),
      day_of_week = days[day_of_week_num]
    ) %>%
    select_('Hoofdrubriek', 'Subrubriek', 'Wijknaam', 'Lattitude', 'Longitude',
            'Tijdstip_registratie_melding', 'datum', 'day_of_week',
            'year', 'month', 'day')
  return(mora)
}

options('stringsAsFactors' = FALSE)

library(data.table)
library(dplyr)
library(bit64)
library(ggplot2)

# read data
mora <- read_mora()

# write formatted data
write.table(mora, file = 'druktemeter/druktemeter_ftp/parsed_data/MORA/mora.csv', sep = ';',
            row.names = FALSE, col.names = TRUE, quote = FALSE)

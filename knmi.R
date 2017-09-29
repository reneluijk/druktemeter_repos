# Reads the historical KNMI data,
# manipulates some columns,
# and writes as csv

# Data gathered from:
# http://www.knmi.nl/nederland-nu/klimatologie/uurgegevens

# Explanation of columns used
# YYYYMMDD = datum (YYYY=jaar,MM=maand,DD=dag)
# HH = uur
# FH = Uurgemiddelde windsnelheid (in 0.1 m/s)
# T = Temperatuur (in 0.1 graden Celsius)
# SQ = Duur van de zonneschijn (in 0.1 uren) per uurvak
# DR = Duur van de neerslag (in 0.1 uur) per uurvak
# RH = Uursom van de neerslag (in 0.1 mm) (-1 voor <0.05 mm)
# U = Relatieve vochtigheid (in procenten)
# M = Mist 0=niet voorgekomen, 1=wel voorgekomen in het voorgaande uur en/of tijdens de waarneming
# R = Regen 0=niet voorgekomen, 1=wel voorgekomen in het voorgaande uur en/of tijdens de waarneming
# S = Sneeuw 0=niet voorgekomen, 1=wel voorgekomen in het voorgaande uur en/of tijdens de waarneming
# O = Onweer 0=niet voorgekomen, 1=wel voorgekomen in het voorgaande uur en/of tijdens de waarneming
# Y = IJsvorming 0=niet voorgekomen, 1=wel voorgekomen in het voorgaande uur en/of tijdens de waarneming

read_knmi <- function() {
  df <- fread('druktemeter/druktemeter_ftp/raw_data/KNMI/uurgeg_240_2011-2020.txt', skip = 31)
  hdr <- readLines('druktemeter/druktemeter_ftp/raw_data/KNMI/uurgeg_240_2011-2020.txt', n = 32) %>%
    tail(1) %>%
    strsplit(split = ',') %>%
    unlist() %>%
    gsub(pattern = ' ', replacement = '') %>%
    gsub(pattern = '#', replacement = '')
  names(df) <- hdr
  
  df <- df %>%
    select_('YYYYMMDD', 'HH', 'FH', 'T', 'SQ', 'DR',
            'RH', 'U', 'M', 'R', 'S', 'O', 'Y') %>%
    mutate(
      year = substr(YYYYMMDD, 1, 4) %>% as.numeric(),
      month = substr(YYYYMMDD, 5, 6) %>% as.numeric(),
      day = substr(YYYYMMDD, 7, 8) %>% as.numeric(),
      date = as.POSIXct(sprintf('%s-%s-%s', year, month, day)) %>%
        as.POSIXct(format = '%Y-%m-%d')
    ) %>%
    rename(
      hour = HH,
      windsnelheid = FH,
      temp = T,
      duur_zon = SQ,
      duur_neerslag = DR,
      uursom_neerslag = RH,
      luchtvochtigheid = U,
      mist = M,
      regen = R,
      sneeuw = S,
      onweer = O,
      ijsvorming = Y
    ) %>%
    select_('date', 'year', 'month', 'day', 'hour', 'windsnelheid', 'temp', 'duur_zon', 'duur_neerslag',
            'uursom_neerslag', 'luchtvochtigheid', 'mist', 'regen', 'sneeuw', 'onweer', 'ijsvorming') %>%
    filter(year >= 2014)
  return(df)
}


knmi_per_dag <- function() {
  knmi_dag <- knmi %>%
    arrange(date) %>%
    group_by(year, month, day) %>%
    summarize(date = date[1],
              min_temp = min(temp),
              max_temp = max(temp),
              mean_temp = mean(temp),
              tot_dur_zon = sum(duur_zon),
              tot_duur_neerslag = sum(duur_neerslag),
              min_luchtvocht = min(luchtvochtigheid),
              max_luchtvocht = max(luchtvochtigheid),
              mean_luchtvocht = mean(luchtvochtigheid),
              mist = sum(mist),
              regen = sum(regen),
              sneeuw = sum(sneeuw),
              onweer = sum(onweer),
              ijs = sum(ijsvorming))
  return(knmi_dag)
}

setwd('~/Documents/druktemeter/druktemeter_ftp')
options('stringsAsFactors' = FALSE)

library(data.table)
library(dplyr)
library(ggplot2)

# inlezen data, fix datum
knmi <- read_knmi()

# write as csv
write.table(knmi, file = 'druktemeter/druktemeter_ftp/parsed_data/KNMI/knmi.csv', sep = ';',
            row.names = FALSE, col.names = TRUE, quote = FALSE)

# daily stats
knmi_dag <- knmi_per_dag()

# write as csv
write.table(knmi_dag, file = 'druktemeter/druktemeter_ftp/parsed_data/KNMI/knmi_per_dag.csv', sep = ';',
            row.names = FALSE, col.names = TRUE, quote = FALSE)

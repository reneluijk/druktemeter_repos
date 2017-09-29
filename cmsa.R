# Script to read the CMSA data and write as csv file

read_cmsa <- function() {
  # get list of files
  files <- list.files('raw_data/CMSA/InControl@Wallen', pattern = 'Cam_loc', full.names = TRUE)
  
  # different cameras
  locs <- paste('Cam_loc', 1:4, sep = '')
  
  # read files per locations
  df <- lapply(locs, function(loc) {
    loc_files <- grep(loc, files, value = TRUE)
    out <- lapply(loc_files, fread) %>%
      bind_rows() %>%
      mutate(location = loc)
    return(out)
  }) %>%
    bind_rows() # combine resulting data frames
  
  # data manipulation with respect to dates, times, years
  df <- df %>% 
    mutate(
      year = substr(Time, 1, 4) %>% as.numeric(),
      month = substr(Time, 6, 7) %>% as.numeric(),
      day = substr(Time, 9, 10) %>% as.numeric(),
      hour = substr(Time, 12, 13) %>% as.numeric(),
      datum = substr(Time, 1, 10) %>% as.Date()
    ) %>%
    group_by(location, year, month, day, hour) %>% # sum observations per location, date, time
    summarize(
      datum = as.Date(unique(datum)),
      In = sum(In),
      Out = sum(Out),
      total = In + Out
    ) %>%
    select_('location', 'datum', 'year', 'month', 'day', 'hour', 'In', 'Out', 'total') %>%
    arrange(datum)
  return(df)
}

setwd('~/Documents/druktemeter/druktemeter_ftp/')
options('stringsAsFactors' = FALSE)

library(dplyr)
library(data.table)

# read all data
cmsa <- read_cmsa()

# write as csv
write.table(cmsa, file = 'parsed_data/CMSA/camera_tellingen.csv', sep = ';',
            row.names = FALSE, col.names = TRUE, quote = FALSE)

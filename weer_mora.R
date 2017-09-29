# Script to plot weather parameter versus MORA data
# Note that this is still a mess


meldingen_per_cat <- function(categorieen) {
  # Function to get the number of meldingen per category
  
  out <- lapply(categorieen, function(categorie) {
    indx <- mora_cats[, categorie] %>% unlist()
    hoofdrubrieken <- mora_cats$Hoofdrubriek[indx] %>% unique()
    subrubrieken <- mora_cats$Subrubriek[indx] %>% unique()
    
    mora %>%
      filter(Hoofdrubriek %in% hoofdrubrieken & Subrubriek %in% subrubrieken) %>%
      group_by(datum) %>%
      summarize(
        day_of_week = unique(day_of_week),
        week_weekend = unique(week_weekend),
        year = unique(year),
        month = unique(month),
        day = unique(day),
        meldingen_var = n()) %>%
      mutate(categorie = categorie)
  }) %>%
    bind_rows()
  return(out)
}


time_plot <- function(x, y1, y2) {
  # Function to plot x agains y1 and y2, 
  # where y1 and y2 are on different y-axes
  
  par(mfrow = c(2, 1))
  xrng <- range(x)
  yrng1 <- range(y1)
  yrng2 <- range(y2)
  
  plot(y1, y2, xlab = 'Temperatuur', ylab = 'Meldingen')
  
  s <- c(1, 5, 10, 100, 1000, 5000)
  by_1 <- diff(yrng1) / 5
  by_1 <- s[tail(which(s <= by_1), 1)]
  
  by_2 <- diff(yrng2) / 5
  by_2 <- s[tail(which(s <= by_2), 1)]
  
  # plot Google expected vs. afval (Rembrandtplein)
  font_size <- 2
  line_width <- 3
  par(mar = c(5,5,2,5))
  
  # plot points
  plot(x, y1,
       type = 'l', col = 'red', axes = FALSE, lwd = line_width,
       xlab = NA, ylab = NA, xlim = xrng, ylim = yrng1)
  
  # create x-axis
  axis.Date(side = 1, x = x)
  mtext(side = 1, line = 3, 'Datum', cex = font_size)
  
  # create first y-axis
  axis(side = 2, at = seq(yrng1[1], yrng1[2], by = by_1), col = 'red', col.axis = "red")
  mtext(side = 2, line = 3, 'Temperatuur', cex = font_size)
  
  par(new = TRUE)
  
  # plot points
  plot(x, y2,
       type = 'l', col = 'blue', axes = FALSE, lwd = line_width,
       xlab = NA, ylab = NA, cex = 1.2, ylim = yrng2)
  
  # create second y-axis
  axis(side = 4, at = seq(yrng2[1], yrng2[2], by = by_2), col = 'blue', col.axis = 'blue')
  mtext(side = 4, line = 3, 'Meldingen', cex = font_size)
}

options('stringsAsFactors' = FALSE)
library(readxl)
library(dplyr)
library(data.table)
library(bit64)
library(ggplot2)
library(forecast)
library(zoo)

# read MORA data
mora <- fread('druktemeter/druktemeter_ftp/parsed_data/MORA/mora.csv', sep = ';', data.table = FALSE) %>%
  mutate(
    datum = as.character(as.Date(datum) - 1),
    year = substr(datum, 1, 4),
    month = substr(datum, 6, 7),
    day = substr(datum, 9,10)
  )

# read mora categories as indicated by Chennard
mora_cats <- read_excel('raw_data/MORA/mora_categorieen.xlsx') %>%
  filter(!is.na(Hoofdrubriek) & !is.na(Subrubriek)) %>%
  mutate(schoon = !is.na(Schoon),
         asset = !is.na(Assetmanagement),
         handhaving = !is.na(Handhaving),
         ggd = !is.na(GGD),
         waternet = !is.na(Waternet)) %>%
  select_('Hoofdrubriek', 'Subrubriek',
          'schoon', 'asset', 'handhaving', 'ggd', 'waternet')

# read KNMI data (observations per hour)
knmi <- fread('druktemeter/druktemeter_ftp/parsed_data/KNMI/knmi.csv', sep = ';', data.table = FALSE)

# read KNMI data (pre-computed statistics per day)
knmi_dag <- fread('druktemeter/druktemeter_ftp/parsed_data/KNMI/knmi_per_dag.csv', sep = ';', data.table = FALSE)


##########################
# KNMI mean temp residuals
##########################

# aiming to remove any seasonal trends
# by no means is this perfect
knmi_dag$knmi_var <- knmi_dag$mean_temp
fit <- lm(knmi_var ~ as.factor(year) + as.factor(month), data = knmi_dag)
knmi_dag$knmi_var_res <- residuals(fit)


##########################
# MORA per categorie
##########################

# preprocessing
mora <- mora %>%
  mutate(week_weekend = ifelse(day_of_week %in% c('Saturday', 'Sunday'), 'weekend', 'week')) %>%
  filter(Hoofdrubriek != '' & Subrubriek != '') %>%
  filter(datum < '2017-09-01') %>% # for September only the first week is covered, so leave that out
  arrange(datum)

# meldingen per categorie
meldingen <- meldingen_per_cat(names(mora_cats)[-(1:2)])

# regress out seasonal effects, day of week
meldingen <- meldingen %>%
  group_by(categorie) %>%
  mutate(meldingen_var_res = lm(meldingen_var ~ factor(year) + factor(month) * factor(day_of_week)) %>% residuals())


##########################
# merge MORA, KNMI
##########################

df <- merge(
  meldingen[, c('datum', 'year', 'month', 'day', 'meldingen_var', 'meldingen_var_res', 'day_of_week', 'week_weekend', 'categorie')],
  knmi_dag[, c('date', 'knmi_var', 'knmi_var_res')],
  by.x = 'datum', by.y = 'date'
) %>%
  filter(!is.na(meldingen_var_res) & !is.na(knmi_var_res)) %>%
  mutate(datum = as.Date(datum)) %>%
  arrange(datum)


##########################
# plot
##########################

# select category to plot
cat <- 'schoon'
tmp <- filter(df, categorie == cat)

# plot meldingen for this category against relevant KNMI variable
time_plot(tmp$datum, tmp$knmi_var_res, tmp$meldingen_var_res)
print(cor(tmp$knmi_var_res, tmp$meldingen_var_res))



#########################################################
# Code below is more than a mess, do not use
#########################################################


# #########################################################
# # PLOT PER GESELECTEERDE CATEGORIE
# # TAKE LAG INTO ACCOUNT
# # Warm weer op dag 1 zorgt mogelijk voor
# # meer meldingen op dag 2
# #########################################################

# counts <- mora %>%
#   group_by(Hoofdrubriek, Subrubriek) %>%
#   summarize(n = n()) %>%
#   arrange(desc(n)) %>%
#   as.data.frame()
# head(counts, 20)

# meldingen <- mora %>%
#   # filter(Hoofdrubriek == 'Overlast Bedrijven en Horeca' &
#   #          Subrubriek %in% c('Geluidsoverlast muziek', 'Overlast terrassen')) %>%
#   filter(Hoofdrubriek == 'Afval' &
#            Subrubriek %in% c('Veeg- / zwerfvuil', 'Container is vol', 'Prullenbak is vol')) %>%
#   group_by(datum) %>%
#   summarize(
#     meldingen_var = n(),
#     day_of_week = unique(day_of_week),
#     week_weekend = unique(week_weekend),
#     year = unique(year),
#     month = unique(year)
#   ) %>%
#   arrange(datum)

# meldingen$meldingen_var_res <- residuals(lm(meldingen_var ~ factor(year) * factor(month) * factor(day_of_week), data = meldingen))

# df <- merge(
#   meldingen[, c('datum', 'meldingen_var', 'meldingen_var_res', 'day_of_week', 'week_weekend')],
#   knmi_dag[, c('date', 'knmi_var', 'knmi_var_res')],
#   by.x = 'datum', by.y = 'date'
# ) %>%
#   filter(!is.na(meldingen_var_res) & !is.na(knmi_var_res)) %>%
#   mutate(datum = as.Date(datum)) %>%
#   arrange(datum)


# ##########################
# # plot
# ##########################

# time_plot(df$datum, df$knmi_var_res, df$meldingen_var_res)
# print(cor(df$knmi_var_res, df$meldingen_var_res, method = 'spearman'))

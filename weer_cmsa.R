# Visualization of historical data and CMSA counts

time_plot <- function(x, y1, y2, isdate = TRUE) {
  # Function plts x versus y1 and y2 on different y-axes
  
  par(mfrow = c(2, 1))
  xrng <- range(x)
  yrng1 <- range(y1)
  yrng2 <- range(y2)
  
  plot(y1, y2, xlab = 'Weer', ylab = 'CMSA Tellingen')

  # get right 'by' argument for axis() function
  s <- c(0.001, 0.01, 0.1, 0.5, 1, 5, 10, 100, 1000, 5000)
  by_1 <- diff(yrng1) / 5
  by_1 <- s[tail(which(s <= by_1), 1)]
  
  by_2 <- diff(yrng2) / 5
  by_2 <- s[tail(which(s <= by_2), 1)]
  
  font_size <- 2
  line_width <- 3
  par(mar = c(5,5,2,5))
  
  # plot points
  plot(x, y1,
       type = 'l', col = 'red', axes = FALSE, lwd = line_width,
       xlab = NA, ylab = NA, xlim = xrng, ylim = yrng1)
  
  # create x-axis
  if (isdate) axis.Date(side = 1, x = x)
  if (!isdate) axis(side = 1)
  mtext(side = 1, line = 3, 'Datum', cex = font_size)
  
  # create first y-axis
  axis(side = 2, at = seq(yrng1[1], yrng1[2], by = by_1), col = 'red', col.axis = "red")
  mtext(side = 2, line = 3, 'Weer', cex = font_size)
  
  par(new = TRUE)
  
  # plot points
  plot(x, y2,
       type = 'l', col = 'blue', axes = FALSE, lwd = line_width,
       xlab = NA, ylab = NA, cex = 1.2, ylim = yrng2)
  
  # create second y-axis
  axis(side = 4, at = seq(yrng2[1], yrng2[2], by = by_2), col = 'blue', col.axis = 'blue')
  mtext(side = 4, line = 3, 'CMSA Tellingen', cex = font_size)
}

options('stringsAsFactors' = FALSE)
library(readxl)
library(dplyr)
library(data.table)
library(bit64)
library(ggplot2)
library(forecast)
library(zoo)

# read data files
cmsa <- fread('druktemeter/druktemeter_ftp/parsed_data/CMSA/camera_tellingen.csv')
knmi <- fread('druktemeter/druktemeter_ftp/parsed_data/KNMI/knmi.csv', sep = ';', data.table = FALSE)


##########################
# KNMI mean temp residuals
##########################

indx <- knmi$uursom_neerslag < 0
knmi$uursom_neerslag[indx] <- 0
knmi$knmi_var <- log(knmi$uursom_neerslag + 1)
hist(knmi$knmi_var)
summary(knmi$knmi_var)

fit <- lm(knmi_var ~ as.factor(year) + as.factor(month) + factor(hour), data = knmi)
knmi$knmi_var_res <- residuals(fit)


##########################
# CMSA residuals
##########################

cmsa <- cmsa %>%
  group_by(datum, year, month, day, hour) %>%
  summarize(cmsa_var = sum(total)) %>%
  mutate(day_of_week = format(as.POSIXct(datum), '%u'),
         hour = hour + 1)
cmsa$cmsa_var_res <- residuals(lm(cmsa_var ~  factor(day_of_week) + factor(hour), data = cmsa))


##########################
# merge MORA, KNMI
##########################

cmsa$merge_id <- paste(cmsa$datum, cmsa$hour)
knmi$merge_id <- paste(knmi$date, cmsa$hour)

df <- merge(
  cmsa[, c('merge_id', 'datum', 'year', 'month', 'day', 'hour', 'cmsa_var', 'cmsa_var_res')],
  knmi[, c('merge_id', 'date', 'knmi_var', 'knmi_var_res', 'regen', 'windsnelheid', 'duur_zon', 'uursom_neerslag')],
  by = 'merge_id'
) %>%
  filter(!is.na(knmi_var_res) & !is.na(cmsa_var_res)) %>%
  mutate(datum = as.Date(datum)) %>%
  arrange(datum, hour)


##########################
# plot
##########################

x <- seq(nrow(df))
y1 <- df$uursom_neerslag
y2 <- df$cmsa_var_res
time_plot(x, y1, y2, FALSE)
print(cor(y1, y2))

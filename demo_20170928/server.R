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

######################
# server
######################

function(input, output, session) {
  # get historical (expected) google data, function of inputs day and hour
  get_historical_data <- reactive({
    patt <- types_categories[[input$category]] %>%
        paste(collapse = '|')
    df_historical <- popularity_historical %>%
      filter(
        day == input$day,
        hour == input$hour
      ) %>%
      filter(grepl(pattern = patt, x = types))
    return(df_historical)
  })
  
  get_gvb_data <- reactive({
    df_gvb <- gvb_totaal %>%
      filter(
        day == input$gvb_day,
        hour == input$gvb_hour
      )
    return(df_gvb)
  })

  get_gvb_locatie_data <- reactive({
    # if (input$gvb_specifiek == 'Vertrekkende passagiers') {
    #   tmp <- rembrandt %>%
    #     filter(day == input$rembrandt_day, hour == input$rembrandt_hour, instaphalte == 'Rembrandtplein') %>%
    #     rename(lat = uitstaphalte_lat, lng = uitstaphalte_lng)
    # }
    # if (input$gvb_specifiek == 'Aankomende passagiers') {
    #   tmp <- rembrandt %>%
    #     filter(day == input$rembrandt_day, hour == input$rembrandt_hour, uitstaphalte == 'Rembrandtplein') %>%
    #     rename(lat = instaphalte_lat, lng = instaphalte_lng)
    # }
    tmp <- rembrandt %>%
      filter(day == input$rembrandt_day & hour == input$rembrandt_hour) %>%
      rename(lat = instaphalte_lat, lng = instaphalte_lng)
    return(tmp)
  })
  
  
  # maps google drukte
  output$map_drukte <- renderLeaflet({
  	# different zooms have different locations to zoom into
    current_historical_data <- get_historical_data()
    current_gvb_data <- get_gvb_data()
    map_sets <- all_sets[[input$focus]]
    
    if (input$maptype == 'Heat Map') {
      m <- current_historical_data %>%
        leaflet() %>%
        setView(lng = map_sets$lng, lat = map_sets$lat, zoom = map_sets$zoom) %>%#setView(lng = 4.883886, lat = 52.375086, zoom = 14) %>%
        addProviderTiles(
          providers$CartoDB.Positron,
          options = providerTileOptions(minZoom = 5, maxZoom = 20)) %>%
        addHeatmap(
          lng = ~lng, lat = ~lat,
          intensity = ~popularity,
          radius = 10,
          min = 0,
          max = 100
        ) %>%
        addCircleMarkers( # add persbakken locations
          lat = ~persbak_locaties$X,
          lng = ~persbak_locaties$Y,
          color = 'blue',
          opacity = 0.2
        ) %>%
        addCircleMarkers( # add camera location
          lat = 52.373837,
          lng = 4.898615,
          color = 'blue',
          opacity = 0.2
        )
    }

    if (input$maptype == 'Scatter Map') {
      m <- leaflet(current_historical_data) %>%
        setView(lng = map_sets$lng, lat = map_sets$lat, zoom = map_sets$zoom) %>%
        addProviderTiles(
          providers$CartoDB.Positron,
          options = providerTileOptions(minZoom = 5, maxZoom = 20)) %>%
        addCircleMarkers(
          lng = ~lng, lat = ~lat,
          color = ~clr,
          radius = 7,
          popup = ~name,
          opacity = ~(as.numeric(popularity != 0) + 0.05),
          fillOpacity = 0
        ) %>%
        addCircleMarkers( # add persbakken locations
          lat = ~persbak_locaties$X,
          lng = ~persbak_locaties$Y,
          color = 'red',
          opacity = 0.2
        )
    }
    return(m)
  })

  # inzet drukte vs afval Rembrandtplein
  output$drukte_afval_plot <- renderPlot({
    # get right types
    patt <- types_categories[[input$category]] %>%
      paste(collapse = '|')
    tmp <- drukte_data %>%
      filter(grepl(pattern = patt, x = types)) %>% # correct types of locations (types are defined by Google: )
      filter(day == input$day) %>%
      group_by(day, hour) %>%
      summarize(overall_drukte = mean(popularity)) %>%
      mutate(unique_id = paste(day, hour))
    
    # merge expected Google data with afval data
    drukte_afval <- merge(tmp, afval_data, by = 'unique_id') %>%
      mutate(overall_drukte = overall_drukte-min(overall_drukte)) %>%
      mutate(overall_drukte = 100*overall_drukte/max(overall_drukte)) %>%
      select(day, hour, overall_drukte, overall_afval) %>%
      arrange(day, hour)
    
    
    xrng <- c(0, 100)
    tmp <- drukte_afval %>%
      filter(day == input$day)
    
    xrng <- c(1, 24)
    yrng1 <- c(0, 100) # ylims for Google data (fixed to 0-100)
    yrng2 <- c(0, max(afval_data$overall_afval)) # ylims for afval data
    
    # plot Google expected vs. afval (Rembrandtplein)
    font_size <- 2
    line_width <- 3
    par(mar = c(5,5,2,5))
    with(tmp,
         plot(hour, overall_drukte, type = 'l', col = 'red', axes = FALSE, lwd = line_width,
              xlab = NA, ylab = NA,
              xlim = xrng, ylim = yrng1))
    axis(side = 1, at = 1:24)
    mtext(side = 1, line = 3, 'Tijdstip (uur)', cex = font_size)
    
    axis(side = 2, at = 10*(0:10), col = 'red', col.axis = "red")
    mtext(side = 2, line = 3, 'Google drukte', cex = font_size)
    
    par(new = TRUE)
    with(tmp, plot(hour, overall_afval, type='l', col = 'blue', lwd = line_width,
                   axes = FALSE, xlab = NA, ylab = NA, cex = 1.2,
                   ylim = yrng2))
    axis(side = 4, at = 10*(0:25), col = 'blue', col.axis = 'blue')
    mtext(side = 4, line = 3, 'Aantal inwerpingen per uur', cex = font_size)
  })

  # inzet drukte vs CMSA tellingen Wallengebied
  output$drukte_cmsa_plot <- renderPlot({
    xrng <- c(1, 24)
    yrng1 <- c(0, 100)
    yrng2 <- c(0, 100)
    
    tmp <- subset(cmsa, day == input$day)
    
    # plot Google expected vs. afval (Rembrandtplein)
    font_size <- 2
    line_width <- 3
    par(mar = c(5,5,2,5))
    with(tmp,
         plot(hour, popularity, type = 'l', col = 'red', axes = FALSE, lwd = line_width,
              xlab = NA, ylab = NA,
              xlim = xrng, ylim = yrng1))
    axis(side = 1, at = 1:24)
    mtext(side = 1, line = 3, 'Tijdstip (uur)', cex = font_size)
    
    axis(side = 2, at = 10*(0:10), col = 'red', col.axis = "red")
    mtext(side = 2, line = 3, 'Google drukte', cex = font_size)
    
    par(new = TRUE)
    with(tmp, plot(hour, In_3_normalized, type='l', col = 'blue', lwd = line_width,
                   axes = FALSE, xlab = NA, ylab = NA, cex = 1.2,
                   ylim = yrng2))
    axis(side = 4, at = 10*(0:10), col = 'blue', col.axis = 'blue')
    mtext(side = 4, line = 3, 'Aantal tellingen', cex = font_size)
  })
  
  # map GVB data
  output$map_gvb <- renderLeaflet({
    # get GVB data for current day, time
    current_gvb_data <- get_gvb_data()
    
    # only one zoom level
    map_sets <- all_sets[["GVB"]]
    
    # set variable
    if (input$gvb_in_uit == 'Instappers') current_gvb_data$passagiers <- current_gvb_data$tot_instap#sqrt(current_gvb_data$tot_instap)
    if (input$gvb_in_uit == 'Uitstappers') current_gvb_data$passagiers <- current_gvb_data$tot_uitstap#sqrt(current_gvb_data$tot_uitstap)
    if (input$gvb_in_uit == 'Beide') current_gvb_data$passagiers <- current_gvb_data$totaal#sqrt(current_gvb_data$totaal)
    
    m <- current_gvb_data %>%
      leaflet() %>%
      setView(lng = map_sets$lng, lat = map_sets$lat, zoom = map_sets$zoom) %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 5, maxZoom = 20))
    
    if (nrow(current_gvb_data) > 0) {
      current_gvb_data$passagiers[which.max(current_gvb_data$passagiers)] <- max(current_gvb_data$passagiers[-which.max(current_gvb_data$passagiers)])
      m <- m %>%
        addHeatmap(
          lng = ~current_gvb_data$lng, lat = ~current_gvb_data$lat,
          intensity = ~current_gvb_data$passagiers,
          radius = 10,
          min = 0,
          max = max(current_gvb_data$passagiers, na.rm = TRUE)
        )
    }
    return(m)
  })

  # map GVB data
  output$map_gvb_specifiek <- renderLeaflet({
    # get GVB data for current day, time
    current_gvb_data <- get_gvb_locatie_data()
    
    # only one zoom level
    map_sets <- all_sets[["GVB"]]
    
    m <- current_gvb_data %>%
      leaflet() %>%
      setView(lng = map_sets$lng, lat = map_sets$lat, zoom = map_sets$zoom) %>%
      addProviderTiles(
        providers$CartoDB.Positron,
        options = providerTileOptions(minZoom = 5, maxZoom = 20))
    
    if (nrow(current_gvb_data) > 0) {
      # current_gvb_data$passagiers[which.max(current_gvb_data$passagiers)] <- max(current_gvb_data$passagiers[-which.max(current_gvb_data$passagiers)])
      m <- m %>%
        addHeatmap(
          lng = ~current_gvb_data$lng, lat = ~current_gvb_data$lat,
          intensity = ~current_gvb_data$passagiers,
          radius = 10,
          min = 0,
          max = max(current_gvb_data$passagiers, na.rm = TRUE)
        )
    }
    return(m)
  })
  
  output$cs_rem_plot <- renderPlot({
    cs_in <- subset(gvb_instap, halte == 'Centraal Station' & day == input$rembrandt_day) %>%
      arrange(day, hour)
    rem_uit <- subset(gvb_uitstap, halte == 'Rembrandtplein' & day == input$rembrandt_day) %>%
      arrange(day, hour)
    
    time_plot(cs_in$hour, cs_in$tot_instap, rem_uit$tot_uitstap, FALSE)
  })
}

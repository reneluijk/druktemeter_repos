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

source('helpers.R')


navbarPage("Stad In Balans", id = "nav",
           
           # Tabblad voor Google data
           tabPanel("Verwachte drukte",
                    div(class = "outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map_drukte", width = "100%", height = "100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default",
                                      fixed = TRUE, right = 10, left = "auto",
                                      top = "auto", bottom = 7,
                                      width = "350px", height = "510px", draggable = TRUE,
                                      
                                      h4("Control Panel"),
                                      selectInput('category', 'Type locatie', # Only show certain types of locations. Categories may not be mutually exclusive, as Google appoints multiple types to one location (https://developers.google.com/places/supported_types)
                                                  choices = names(types_categories),
                                                  selected = 'Alle'),
                                      selectInput('day', 'Dag van de week',
                                                  choices = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),
                                                  selected = 'Monday'
                                      ),
                                      sliderInput('hour', 'Time of day', # slider for time of day (hour)
                                                  min = 1, max = 24,
                                                  value = 1,
                                                  animate = TRUE),
                                      radioButtons('maptype', 'Type of Map', # buttons to indicate type of map: scatter or heatmap
                                                   choices = list('Scatter Map', 'Heat Map'),
                                                   selected = 'Heat Map',
                                                   inline = TRUE),
                                      radioButtons('focus', 'Focus on area', # buttons to focus on certain areas
                                                   choices = list('Overview', 'Rembrandtplein', 'Wallengebied', 'Westerpark'),
                                                   selected = 'Overview',
                                                   inline = TRUE),
                                      checkboxInput('show_waste', 'Show waste', TRUE), # show Google expected vs. afval
                                      #checkboxInput('show_pedestrians', 'Show hotspots', FALSE), # show pedestrian hotspots
                                      checkboxInput('show_cmsa', 'Show CMSA', TRUE) # show Google vs. CMSA camera data
                        ),
                        
                        # absolutePanel, alleen als focus==Rembrandtplein
                        conditionalPanel(condition = "input.focus == 'Rembrandtplein' && input.show_waste == true", # js conditional statement
                                         absolutePanel(id = "drukte_afval_panel", class = "panel panel-default",
                                                       fixed = !TRUE, right = "auto", left = 5,
                                                       bottom = 190, top = "auto",
                                                       width = 550, height = 200, draggable = TRUE,
                                                       plotOutput('drukte_afval_plot')
                                         )
                        ),
                        
                        # absolutePanel, alleen als focus==Wallengebied
                        conditionalPanel(condition = "input.focus == 'Wallengebied' && input.show_cmsa == true", # js conditional statement
                                         absolutePanel(id = "drukte_cmsa_panel", class = "panel panel-default",
                                                       fixed = !TRUE, right = "auto", left = 5,
                                                       bottom = 190, top = "auto",
                                                       width = 550, height = 200, draggable = TRUE,
                                                       plotOutput('drukte_cmsa_plot')
                                         )
                        )
                    )
           ),
           
           # Tabblad voor GVB
           tabPanel("GVB",
                    div(class = "outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map_gvb", width = "100%", height = "100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default",
                                      fixed = TRUE, right = 10, left = "auto",
                                      top = 65, bottom = "auto",
                                      width = "350px", height = "300px", draggable = TRUE,
                                      
                                      h4("Control Panel"),
                                      selectInput('gvb_in_uit', 'Instap, uitstap',
                                                  choices = c('Instappers', 'Uitstappers', 'Beide'),
                                                  selected = 'Uitstappers'
                                      ),
                                      selectInput('gvb_day', 'Dag van de week',
                                                  choices = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),
                                                  selected = 'Monday'
                                      ),
                                      sliderInput('gvb_hour', 'Time of day',
                                                  min = 7, max = 24,
                                                  value = 7,
                                                  animate = TRUE)
                        )
                    )
           ),

           # Tabblad voor GVB, specifieke locatie
           tabPanel("GVB Rembrandtplein",
                    div(class = "outer",
                        tags$head(
                          # Include our custom CSS
                          includeCSS("styles.css"),
                          includeScript("gomap.js")
                        ),
                        
                        leafletOutput("map_gvb_specifiek", width = "100%", height = "100%"),
                        
                        # Shiny versions prior to 0.11 should use class="modal" instead.
                        absolutePanel(id = "controls", class = "panel panel-default",
                                      fixed = TRUE, right = 10, left = "auto",
                                      top = 65, bottom = "auto",
                                      width = "350px", height = "300px", draggable = TRUE,
                                      
                                      h4("Control Panel"),
                                      # selectInput('gvb_specifiek', 'Van, naar',
                                      #             choices = c('Vertrekkende passagiers', 'Aankomende passagiers'),
                                      #             selected = 'Vertrekkende passagiers'
                                      # ),
                                      selectInput('rembrandt_day', 'Dag van de week',
                                                  choices = c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday'),
                                                  selected = 'Monday'
                                      ),
                                      sliderInput('rembrandt_hour', 'Time of day',
                                                  min = 7, max = 24,
                                                  value = 7,
                                                  animate = TRUE),
                                      checkboxInput('show_cs_rem', 'Instap CS', TRUE)
                        ),
                        conditionalPanel(condition = "input.show_cs_rem == true", # js conditional statement
                                         absolutePanel(id = "cs_rem", class = "panel panel-default",
                                                       fixed = !TRUE, right = "auto", left = 5,
                                                       bottom = 190, top = "auto",
                                                       width = 550, height = 200, draggable = TRUE,
                                                       plotOutput('cs_rem_plot')
                                         )
                        )
                    )
           )
)

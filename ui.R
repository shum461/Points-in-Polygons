library(ggmap)
library(sp)
library(rgeos)
library(sp)
library(rgdal)
library(mapview)
library(sf)
library(shiny)
library(leaflet)
library(raster)
library(rhandsontable)
library(DT)
library(shinyjs)
library(dplyr)
library(spdplyr)

#setwd("C:/Users/vvt97279/Desktop/Teaching Shiny/Poly")

##################### START OF THE UI#######################
shinyUI(bootstrapPage(
  tags$head(tags$script(src = "message-handler.js")),
  useShinyjs(),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("incidentmap",width = "100%", height = "100%"),
  absolutePanel(
    top = 50, left = 20,

   ###########where the file will be uploaded################# 
   fileInput("file1", "Choose CSV File",
              multiple = TRUE,
              accept = c("text/csv",
                         "text/comma-separated-values,text/plain",
                         ".csv")),
   ##############################################################
   actionButton("goButton", "reset"),
    fixedPanel(
      top = 70, right = 10,  
    sliderInput("opac","Opacity",min=0, max=1, value=1),
    actionButton("hideT","Show/Hide Table",value = TRUE),
    
    #a(id = "toggleAdvanced", "Show/hide Table", href = "#"),
    shinyjs::hidden(
      div(id = "hidetable",
  DT::dataTableOutput("table")
      )
)
    ))
))
  


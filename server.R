library(GISTools)
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
library(DT)
library(shinyjs)
library(dplyr)
library(spdplyr)
library(htmlTable)
library(htmltools)
####global environment#########
#install.packages(c("GISTools", "ggmap", "sp", "rgdal", "mapview", "sf", "shiny", "leaflet", "raster", "DT", "shinyjs", "dplyr", "sdplyr"))#########
#setwd("C:/Users/vvt97279/Desktop/Teaching Shiny/Poly")
  
IP <- readOGR("IP_TMDL2.shp")
WGS84 <-CRS("+proj=longlat +ellps=WGS84 +datum=WGS84")  
ip=spTransform(IP, WGS84)
  
factpal <- colorFactor(brewer.pal(6,"BrBG"), IP$REGION) 
  
############################################################  

####################SERVER#################################
shinyServer(function(input, output, session) {

  shape= reactive({
    
    ip5=ip 
    
    
    })
  
  shape2= reactive({
    
   ip6=ip
  
   ip6 %>% dplyr::filter(!is.na(EPA_APPROV))
   
     })
 ###########read the file input#############
  
  DF =reactive({
    
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    })
  
  
  ##############when the button is clicked#################
  
  observeEvent(input$goButton, {
    
    reset("file1")
    
  })
  
#################################################  
  
raw= reactive({
  
  data_raw=DF()
  
  coordinates(data_raw) = ~Longitude + Latitude
  ip2=spTransform(shape(), CRS("+proj=longlat +datum=WGS84"))
  proj4string(data_raw)= CRS("+proj=longlat +datum=WGS84")
  
B=cbind(Lat=data_raw$Latitude,Lon=data_raw$Longitude,Point_Name=data_raw$Point_Name,over(data_raw,ip2)) 
B %>% mutate(In_IP=ifelse(is.na(IP_NAME),"NO","YES"),Approved=ifelse(is.na(EPA_APPROV),"NO","YES"))

})
  
 
 #observe ({
  # shinyjs::onclick("toggleAdvanced",
  #                  shinyjs::toggle(id = "hidetable", anim = TRUE)) 
 #}) 
  
  observeEvent(input$hideT,{
    shinyjs::toggle(id = "hidetable", anim = TRUE)
    
  })
  
 output$table <-  renderDataTable({  
DT::datatable(
   raw() %>% dplyr::select(Point_Name,IP_NAME,In_IP,Approved),
   extensions = 'Buttons', options = list(
      initComplete = JS(
       "function(settings, json) {",
       "$(this.api().table().header()).css({'background-color': '#000', 'color': '#fff'});",
       "}"),
     rownames= FALSE,
     columnDefs = list(list(className = 'dt-center')),
     pageLength = 10,
     scrollX='400px',
     rownames= FALSE,
     lengthMenu = c(5, 10),
     dom = 'Bfrtip',
     buttons = 
       list('copy', 'print', list(
         extend = 'collection',
         buttons = c('csv', 'excel', 'pdf'),
         text = 'Download'
       ))
     
   )
   )
 
 })


  
  output$incidentmap <- renderLeaflet({
    
    leaflet() %>%
    addProviderTiles("Esri.WorldImagery", group="Imagery") %>%
    addTiles(options = providerTileOptions(noWrap = TRUE), group="Street") %>%
    addLayersControl(baseGroups = c("Imagery","Street"),
     options = layersControlOptions(collapsed = TRUE)) %>%
      setView(lng = -78.39844, lat = 37.82715, zoom=8) %>%
    addMouseCoordinates(style = "basic") 
    })
    
 
  



  
 
  icons1 <- reactive ({
    awesomeIcons(
    icon = ifelse(raw()$In_IP=="YES" & raw()$Approved =="YES",'ion-checkmark-circle','ion-close-circle'),
    iconColor = 'black',
    library = 'ion',
    markerColor = ifelse(raw()$In_IP=="YES" & raw()$Approved =="YES",'green','red'))
  })


 

observe({
  leafletProxy("incidentmap") %>%
    clearMarkers()%>%
    clearShapes() %>%
    clearControls() %>%
    addPolygons(data=shape(),stroke = TRUE, color= "black",weight = 0.2, group="All", smoothFactor = 0.2, fillOpacity = input$opac,
                fillColor = ~factpal(REGION),
                popup = popupTable(shape(), zcol = 1:15),     
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label=shape()$WATERSHE_1,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "20px",
                  direction = "auto")) %>% 
    
    
    
    addPolygons(data=shape2(),stroke = TRUE, color= "black",weight = 0.2, group="Approved", smoothFactor = 0.2, fillOpacity = input$opac,
                fillColor = ~factpal(REGION),
                
                popup = popupTable(shape(), zcol = 1:15),   
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.7,
                  bringToFront = TRUE),
                label=shape2()$WATERSHE_1,
                labelOptions = labelOptions(
                  style = list("font-weight" = "normal", padding = "3px 8px"),
                  textsize = "20px",
                  direction = "auto")) %>% 
  
    addAwesomeMarkers(data=raw(),label=~Point_Name,icon=icons1(), group="Points",    
    #addCircleMarkers(data=raw(),radius=10,fillOpacity=50,fill=TRUE,label=~Point_Name,weight=5,
    #                 color=ifelse(raw()$In_IP=="YES" & raw()$Approved =="YES",'green','red'),fillColor=ifelse(raw()$In_IP=="YES" & raw()$Approved =="YES",'green','red'),group="Points",         
     labelOptions = labelOptions(noHide = T, textOnly = FALSE,
              style = list(
             'color' = 'grey',
            'font-family' = 'serif',
             'font-style' = 'bold'#,                                
            #'box-shadow' = '3px 3px rgba(0,0,0,0.25)',                                
            #'font-size' = '12px',                               
            # 'border-color' = 'rgba(0,0,0,0.5)'                                                                 
                                           ))) %>%  
                leaflet::addLegend(pal = factpal, 
                       values = shape()$REGION, 
                       position = "bottomleft", 
                       title = "DEQ Regions") %>%
    leaflet::addLegend(colors=c('red','green'),values = unique(raw()$In_IP),labels = unique(raw()$In_IP), 
                       position = "bottomleft", 
                       title = "Acceptable Site?") %>%

    #addLegend("bottomright", pal = , values =In_IP,
    #          title = "In IP ?") %>%
    addLayersControl(
      baseGroups= c("Imagery","Street"),
      overlayGroups = c("All","Approved", "Points"),
      options = layersControlOptions(collapsed = TRUE)
    )
  
})




  
#Final
  })

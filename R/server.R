#edit
library(leaflet)
library(RColorBrewer)
library(tidyverse)
library(rgdal)
library(shinyjs)
library(kableExtra)

options(shiny.port = 3840)

function(input, output, session) {

  source("radar_ind.R")
  source("radarplot_dim.R")
  
  ## Create reactive output ####
  output$stationselection <- renderUI({
    
    if(input$filter == "All"){
#      selection <- indicators_df$X__2
       selection <- stations_all$station_name
      
    }
    if(input$filter == "VR_26_Leuven"){
     # selection <- indicators_df_VR_23_Gent$X__2
      selection <- stations_VR_26_Leuven$station_name
      
    }
#    if(input$filter == "FR_74_Gent"){
#      selection <- indicators_df_FR_74_Gent$X__2
#       selection <- stations_FR_74_Gent$station_name
#      
#    }
    selectInput("station", "Station:", choices = unique(selection))
  })
  
  #observe({
    
  #})
  
  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(group = "Detail") %>%
      addProviderTiles("CartoDB.Positron", group = "Neutraal") %>%
      setView(lat = 50.7, lng = 4.35, zoom = 7) %>%
      addLayersControl(
        baseGroups = c("Neutraal", "Detail")) %>%
      addScaleBar(position = "topleft", scaleBarOptions(imperial = FALSE))
  })
  


# Plot radar graph --------------------------------------------------------
  
#1  output$image <- renderImage({
  #1    
  #1    if(input$filter == "All"){
  #1      folder <- "All"
  #1    }
  #1    if(input$filter == "VR_26_Leuven"){
  #1      folder <- "VR_26_Leuven"
  #1    }
  #1    if(input$filter == "FR_74_Gent"){
  #1      folder <- "FR_74_Gent"
  #1    }
  #1    
  #1    if(input$graph == "Dimensions"){
  #1      graph <- "dim"
  #1    }
  #1    if(input$graph == "Indicators"){
  #1      graph <- "ind"
  #1    }
#    city <- dplyr::filter(indicators_df, X__2 == input$station)
#    name <- paste(city$X__2)
#    id <- as.character(city$X__1)
  #1     city <- dplyr::filter(stations_all, station_name == input$station)
  #1     name <- paste(city$station_name)
  #1     id <- as.character(city$station)
  #1    path <- paste("images/", folder, "/", graph, "/", name, "_", id, ".png", sep = "")

  #1    list(src = path,
  #1         contentType = 'image/png',
  #1         width = 350,
  #1         height = 350)
  #1  }, deleteFile = FALSE)
  

  ## Observer ####
  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
    colorBy <- input$centrality
    raster <- input$raster
    overige <- input$overige
    knoop <- input$knoop
#    flow <- input$flows
    
    if (colorBy == "TR Centrality Weekend") {
      gtfs_cent <- deg_week
      value <- deg_week$value
      colorData <- ~pal(1/value)
      pal <- colorQuantile(c("#eff3ff", "#bdd7e7", "#6baed6", "#2171b5"), domain = NULL)
      radius <- 3
    }
    if (colorBy == "TR Centrality Tuesday") {
      gtfs_cent <- deg_tuesday
      value <- deg_tuesday$value
      colorData <- ~pal(1/value)
      pal <- colorQuantile(c("#eff3ff", "#bdd7e7", "#6baed6", "#2171b5"), domain = NULL)
      radius <- 3
    }
    if (colorBy == "TT Centrality Weekend") {
      gtfs_cent <- clos_week
      value <- clos_week$value
      colorData <- ~pal(1/value)
      pal <- colorQuantile(c("#feedde", "#fdbe85", "#fd8d3c", "#d94701"), domain = NULL)
      radius <- 3
    }
    if (colorBy == "TT Centrality Tuesday") {
      gtfs_cent <- clos_tuesday
      value <- clos_tuesday$value
      colorData <- ~pal(1/value)
      pal <- colorQuantile(c("#feedde", "#fdbe85", "#fd8d3c", "#d94701"), domain = NULL) 
      radius <- 3
    }

    if (colorBy == "Stations") {
      gtfs_cent <- stations
      value <- 1
      colorData <- ~pal(1/value)
      pal <- colorBin("gray25", domain = NULL)
      radius <- 2.5
    }
    
    
    
#1    if (colorBy == "Stations") {
#1      if(input$filter == "All"){
#1        gtfs_cent <- stations
#      }
    #1       if(input$filter == "VR_26_Leuven"){
    #1        gtfs_cent <- stations_VR_26_Leuven
    #1      }
#1      if(input$filter == "FR_74_Gent"){
#1        gtfs_cent <- stations_FR_74_Gent
#1      }
#1          value <- 1
#1          colorData <- ~pal(1/value)
#1          pal <- colorBin("gray25", domain = NULL)
#1          radius <- 2.5
#        }


    if(raster == "landuse"){
      raster_map <- "landuse"
      rasterPalette <- c("#2b83ba", "#abdda4", "#d7191c")
      rasterLabels <- c("werken", "voorzieningen", "wonen")
    }
    if(raster == "basic"){
      raster_map <- "basic"
      rasterPalette <- c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c")
      rasterLabels <- c("0", "0.25", "0.5", "0.75", "1")
    }
    if(raster == "job"){
      raster_map <- "job"
      rasterPalette <- c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c")
      rasterLabels <- c("0", "3", "9", "27", "81")
    }
    if(raster == "metropol"){
      raster_map <- "metropol"
      rasterPalette <- c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c")
      rasterLabels <- c("0", "0.25", "0.5", "0.75", "1")
    }
    if(raster == "regional"){
      raster_map <- "regional"
      rasterPalette <- c("#2b83ba", "#abdda4", "#ffffbf", "#fdae61", "#d7191c")
      rasterLabels <- c("0", "0.25", "0.5", "0.75", "1")
    }
    if(overige == "Fietssnelwegen netwerk"){
      overige_data <- fiets
      #"#74c476"
    }
    if(overige == "Vervoerregios"){
      overige_data <- regios
    }
    if(overige == "Wandelbuffer"){
      overige_data <- wandel
    }
    if(overige == "Aarschot"){
      overige_data <- aarschot
    }
    if(overige == "Haacht"){
      overige_data <- haacht
    }
    if(overige == "Testelt"){
      overige_data <- testelt
    }
    if(overige == "Wespelaar"){
      overige_data <- wespelaar
    }
    if(overige == "Wezemaal"){
      overige_data <- wezemaal
    }
    if(overige == "Wijgmaal"){
      overige_data <- wijgmaal
    }
    if(knoop == "Selectie haltes De Lijn"){
      knoop_data <- selectionstops
      knoop_data <- dplyr::filter(knoop_data, type == "DeLijn")
    }
    if(knoop == "Selectie haltes TEC"){
      knoop_data <- selectionstops
      knoop_data <- dplyr::filter(knoop_data, type == "TEC")
    }
    if(knoop == "Selectie haltes MIVB"){
      knoop_data <- selectionstops
      knoop_data <- dplyr::filter(knoop_data, type == "MIVB")
    }
    if(knoop == "Alle haltes De Lijn"){
      knoop_data <- allstops
      knoop_data <- dplyr::filter(knoop_data, type == "DeLijn")
    }
    if(knoop == "Alle haltes TEC"){
      knoop_data <- allstops
      knoop_data <- dplyr::filter(knoop_data, type == "TEC")
    }
    if(knoop == "Alle haltes MIVB"){
      knoop_data <- allstops
      knoop_data <- dplyr::filter(knoop_data, type == "MIVB")
    }

    ## Proxy main map ####
    if(colorBy == "Stations"){
      proxy <- leafletProxy("map", data = gtfs_cent) %>%
        clearMarkers() %>%
        clearImages() %>%
        clearShapes() %>%
        addCircleMarkers(~longitude, ~latitude, radius=radius, 
                         stroke=TRUE, weight = .7, color = "gray", fillOpacity=0.8,
                         fillColor=colorData, layerId =~station,
                         popup =~paste("Station:", station_name, "</br>",
                                      #"Aantal opstappers (okt. '17):", opstappers, "</br>",
                                      #"</br>",
                                      #"A1 - Totaal autoparking:", `A1 - Totaal autoparking`, "</br>",
                                      #"A2 - Opstappers ochtendspits verder dan 3 km:", round(`A2 - Opstappers ochtendspits verder dan 3 km`), "</br>",
                                      #"Ratio A2/A1:", round(`Ratio A2/A1`,1), "</br>",
                                      #"</br>",
                                      #"F1 - Totaal fietsparking:", `F1 - Totaal fietsparking`, "</br>",
                                      #"F2 - Opstappers ochtendspits dichter dan 3 km:", round(`F2 - Opstappers ochtendspits dichter dan 3 km`), "</br>",
                                      #"Ratio F2/F1:", round(`Ratio F2/F1`,1), "</br>",
                                       sep =" ")
                         ) %>%
        addLegend("bottomleft", pal=pal, values=~value, title="Kwantiel",
                  layerId ="colorLegend")
    }else{
      proxy <- leafletProxy("map", data = gtfs_cent) %>%
        clearMarkers() %>%
        clearImages() %>%
        clearShapes() %>%
        addCircleMarkers(~longitude, ~latitude, radius=radius, 
                         stroke=TRUE, weight = .7, color = "gray", fillOpacity=0.8,
                         fillColor=colorData, layerId =~station,
                         popup =~paste("Station:", station_name, sep =" ")) %>%
        addLegend("bottomleft", pal=pal, values=~value, title="Kwantiel",
                  layerId ="colorLegend")
    }
    shinyjs::hideElement(id = "loading")
    
#    if(raster == "Geen selectie" & overige == "Geen selectie"){
#      proxy
#    }
#    if(raster != "Geen selectie" & overige == "Geen selectie"){
#      shinyjs::showElement(id = "loading")
#      proxy %>%
#        removeTiles(layerId = "raster") %>%
#        addTiles(urlTemplate = paste("map/",raster_map,"/{z}/{x}/{y}.png", sep = ""),
#                 options = tileOptions(tms = TRUE, opacity = .4), layerId = "raster") %>%
#      addLegend(position = "bottomleft",
#                colors = rasterPalette,
#                labels = rasterLabels,
#                title = "Plaats",
#                layerId = "rasterLegend"
#      )
#    }
#    if(raster != "Geen selectie" & overige != "Geen selectie"){
#      proxy %>%
#        removeTiles(layerId = "raster") %>%
#        addTiles(urlTemplate = paste("map/",raster_map,"/{z}/{x}/{y}.png", sep = ""),
#                 options = tileOptions(tms = TRUE, opacity = .4), layerId = "raster") %>%
#        addLegend(position = "bottomleft",
#                  colors = rasterPalette,
#                  labels = rasterLabels,
#                  title = "Plaats",
#                  layerId = "rasterLegend"
#        ) %>%
#        addPolylines(data = overige_data, color = "#949494", weight = 1.5, smoothFactor = 0.2,
#                     opacity = 0.8)
#    }
#    if(raster == "Geen selectie" & overige != "Geen selectie"){
#      proxy %>%
#        removeTiles(layerId = "raster") %>%
#        clearImages() %>%
#        addPolylines(data = overige_data, color = "#949494", weight = 1.5, smoothFactor = 0.5,
#                     opacity = 0.8)
#    }
    
    #Plot Overige ####
    if(overige == "Fietssnelwegen netwerk" |
       overige ==  "Vervoerregios" | overige == "Wandelbuffer" |
       overige ==  "Aarschot" | overige ==  "Haacht" | overige ==  "Testelt" |
       overige ==  "Wespelaar" | overige ==  "Wezemaal" |
       overige ==  "Wijgmaal"){
      shinyjs::showElement(id = "loading")
      proxy_overige <- leafletProxy("map", data = gtfs_cent) %>%
        clearImages() %>%
        addPolylines(data = overige_data, color = "#3d87ff", weight = 1.5, smoothFactor = 0.5,
                     opacity = 0.8)
      shinyjs::hideElement(id = "loading")
    }
    if(overige == "Cambio plaatsen"){
      shinyjs::showElement(id = "loading")
      proxy_overige <- leafletProxy("map", data = gtfs_cent) %>%
        clearImages() %>%
        addCircleMarkers(data = cambio, popup =~paste("Naam:", Naam),
                         radius=2.5, 
                         stroke=TRUE, weight = .7, color = "gray", fillOpacity=0.6,
                         fillColor="forestgreen")
      shinyjs::hideElement(id = "loading")
    }
    if(overige == "Geen selectie"){
      proxy_overige <- proxy %>%
        clearImages()
    }
    
    #Plot raster ####
    if(raster != "Geen selectie"){
      shinyjs::showElement(id = "loading")
      proxy_raster <- leafletProxy("map", data = gtfs_cent) %>%
        removeTiles(layerId = "raster") %>%
        addTiles(urlTemplate = paste("map/",raster_map,"/{z}/{x}/{y}.png", sep = ""),
                 options = tileOptions(tms = TRUE, opacity = .4), layerId = "raster") %>%
        addLegend(position = "bottomleft",
                  colors = rasterPalette,
                  labels = rasterLabels,
                  title = "Plaats",
                  layerId = "rasterLegend")
      shinyjs::hideElement(id = "loading")
    }
    if(raster == "Geen selectie"){
      proxy_raster <- proxy %>%
        removeTiles(layerId = "raster") %>%
        removeControl(layerId = "rasterLegend")
    }
    
    # Plot Knoop ####
    if(knoop != "Geen selectie"){
      proxy_knoop <- leafletProxy("map", data = gtfs_cent) %>%
        clearMarkerClusters() %>%
        addMarkers(data = knoop_data, icon = ~transportIcons["bus"],
                   label = ~as.character(stop_name),
                         clusterOptions = markerClusterOptions()) 
    }
    
    if(knoop == "Geen selectie"){
      proxy_knoop <- proxy %>%
        clearMarkerClusters()
    }
    
    # Plot Flows ####
#     pal <- colorFactor(
#      palette = c("#2b83ba", "#d7191c"),
#      domain = flows$direction
#    )
    

#    if(flow == "Bestemmingen"){
#      station <- input$station
#      id <- stations_all[stations_all$station_name == station,][[1]]
#      print(id)
#      flows_filtered <- flows[flows$origins == id & flows$direction == "from", ]
#      proxy_flows <- leafletProxy('map') %>% # use the proxy to save computation
#        addPolylines(data = flows_filtered, weight = ~log(sqrt(percentage)*2)*2, group = ~origins,
#                     color = ~pal(direction), opacity = .6) #%>%
#    }
    
#    if(flow == "Herkomsten"){
#      station <- input$station
#      id <- stations_all[stations_all$station_name == station,][[1]]
#      print(id)
#      flows_filtered <- flows[flows$origins == id & flows$direction == "to", ]
#      proxy_flows <- leafletProxy('map') %>% # use the proxy to save computation
#        addPolylines(data = flows_filtered, weight = ~log(sqrt(percentage)*2)*2, group = ~origins,
#                     color = ~pal(direction), opacity = .6) #%>%
#    }
    
#    if(flow == "Geen selectie"){
#      proxy_flows <- proxy
#    }
    

    
  })
  


  
#  addLegend(position = "bottomleft",
#            colors = flowsPalette,
#            labels = flowsLabels,
#            title = "Flows",
#            layerId = "flowsLegend") %>%

  # When map is clicked, show a popup with city info
#  observe({
#    leafletProxy("map") %>% clearPopups()
#    event <- input$map_shape_click
#    if (is.null(event))
#      return()

#    isolate({
#      showCityPopup(event$id, event$lat, event$lng)
#    })
#  })
  
  output$metadata <- function() {
    metadata %>%
      mutate(
        VELD = ifelse(VELD == "KNOOP", cell_spec(VELD, color = "RoyalBlue"), 
               ifelse(VELD == "TREIN", cell_spec(VELD, color = "DarkGray"),
               ifelse(VELD == "PLAATS", cell_spec(VELD, color = "Tomato"),
               ifelse(VELD == "MOTIVATIE", cell_spec(VELD, color = "DarkOrange"),
               ifelse(VELD == "GEBRUIKERSINTENSITEIT", cell_spec(VELD, color = "LightGray"),
               ifelse(VELD == "INVLOEDSGEBIED", cell_spec(VELD, color = "ForestGreen"),
                                       ""))))))) %>%
      knitr::kable("html", escape = FALSE) %>%
      kable_styling("bordered", full_width = FALSE) %>%
      column_spec(1, bold = T) %>%
      column_spec(4, width = "10em") %>%
      collapse_rows(columns = c(1:2,5,6), valign = "top")
    
  }
      
    output$overzicht <- function() {
      overzicht %>%
        knitr::kable("html", escape = FALSE) %>%
        kable_styling("bordered", full_width = FALSE,
                      bootstrap_options = c("striped", "hover"),
                      font_size = 11) %>%
        row_spec(c(1,7:8,14,19,23:25), bold = TRUE) %>%
        add_header_above(c(" " = 1, "FIETS", "AUTO", "OV" = 2,
                           "TREIN" = 8, "PLAATS" = 9,
                           "GEBRUIKERSINTENSITEIT" = 1)) %>%
        footnote(general = c("Omwille van contractvoorwaarden met NMBS kan de brondata m.b.t. de velden 'motivatie', 'gebruikersintensiteit' en 'invloedsgebied' niet worden weergegeven.",
                             "*Omwille van contractvoorwaarden met NMBS kunnen hier enkel de parkeerdata voor de eigenlijke cases worden weergegeven."), general_title = "")
                 
  }
  
  
}


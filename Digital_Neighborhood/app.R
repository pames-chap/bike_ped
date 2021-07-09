library(shiny)
library(rgdal)
library(leaflet)

ctry <- readOGR(here::here(), layer = "ctry")
proj4string(ctry) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

shinyApp(
  
  ui = fluidPage(leafletOutput("map")),
  
  server = function(input, output, session) {
    
    output$map <- renderLeaflet({
      leaflet() %>% 
        addTiles() %>% 
        addPolygons(data = ctry, 
                    fillColor = "gray", 
                    fillOpacity = 1, 
                    weight = 2, 
                    stroke = T, 
                    color = "blue", 
                    opacity = 1,
                    group = "Countries", 
                    layerId = ~admin)
    })
    
    observeEvent(input$map_shape_click, {
      click <- input$map_shape_click
      
      if(is.null(click))
        return()   
      
      #pulls lat and lon from shiny click event
      lat <- click$lat
      lon <- click$lng
      
      #puts lat and lon for click point into its own data frame
      coords <- as.data.frame(cbind(lon, lat))
      
      #converts click point coordinate data frame into SP object, sets CRS
      point <- SpatialPoints(coords)
      proj4string(point) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      
      #retrieves country in which the click point resides, set CRS for country
      selected <- ctry[point,]
      proj4string(selected) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
      
      proxy <- leafletProxy("map")
      if(click$id == "Selected"){
        proxy %>% removeShape(layerId = "Selected")
      } else {
        proxy %>% addPolygons(data = selected, 
                              fillColor = "black",
                              fillOpacity = 1, 
                              color = "red",
                              weight = 3, 
                              stroke = T,
                              layerId = "Selected")
      } 
    })
  })
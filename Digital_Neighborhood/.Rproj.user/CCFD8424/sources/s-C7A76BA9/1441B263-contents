


###################################################
#
# Server for Map Tab
#
###################################################



###################################################
# Leaflet Map... https://rstudio.github.io/leaflet/
###################################################

vals <- reactiveValues(cur_ct_data=NULL)

output$map <- renderLeaflet({
  
  # Identify items to include in the map hover (str_split is used to strip out the tract information only)
  polygon_popup <- paste0(str_split(norfolk_tract_sp@data$NAME, ',', simplify = TRUE)[,1])
  
  leaflet(options = leafletOptions(zoomControl = FALSE,
                                        minZoom = 12,
                                        maxZoom = 12,
                                        doubleClickZoom= FALSE,
                                        scrollWheelZoom= F,
                                        dragging = FALSE,
                                        attributionControl = FALSE), 
               data = norfolk_tract_sp) %>%
    setView(-76.256, 36.897, zoom = 12) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(data = norfolk_tract_sp, 
                layerId = norfolk_tract_sp@data$GEOID,   # layerId is passed on map clicks
                fillColor = light_blue_color,
                fillOpacity = 0.1,
                color = dark_blue_color,
                weight = 1, 
                smoothFactor = 0.5,
                opacity = 1.0, 
                label = polygon_popup
                ) 
})



###################################################
# Observe Map Click
###################################################

observeEvent(input$map_shape_click, {
  
  event <- input$map_shape_click

  if(is.null(event))
    return()

  # Event id is the GEOID (aka fips code).  The id points to the layerId in the addPolygons in the map.
  event_id <- paste0(event$id)

  # Set tract name using fips code
  tract_name <- paste0(norfolk_tract_sp@data$NAME[ which(norfolk_tract_sp@data$GEOID == event_id)])

  # Output the tract name to ui
  output$map.tract <- renderText({ tract_name })
  
  # Update the drop down to equal the clicked map
  updateSelectInput(session, "tract_locations", selected = tract_name)

})



###################################################
# Observe Drop Down
###################################################

observeEvent(input$tract_locations, {
  
  selected_tract <- input$tract_locations

  # Output the tract name to ui 
  output$map.tract <- renderText({ selected_tract })
  
  # Subset the dataframe to just the selected tract
  selected <- norfolk_tract_sp[norfolk_tract_sp@data$NAME == selected_tract,]
  
  # Identify items to include in the proxy map hover
  polygon_popup <- paste0(str_split(selected@data$NAME, ',', simplify = TRUE)[,1])
  
  # Proxy the leaflet map
  proxy <- leafletProxy("map")
  

  # Add the polygons from the dataframe subset
  proxy %>% addPolygons(data = selected,
                        label = polygon_popup,
                        fillColor = dark_blue_color,
                        fillOpacity = 1,
                        color = dark_gray_color,
                        weight = 3,
                        stroke = T,
                        #layerId = selected@data$Name)
                        layerId = "Selected")
  
  
})



###################################################
# Propogate the dropdown with tract names
###################################################

output$tract_location <- renderUI({
  
  # Get unique names from data to propagate the select input
  map.tract.names <- as.vector( unique(norfolk_tract_sp@data$NAME) )
  selectInput("tract_locations",
              label = "Choose Location",  
              choices=map.tract.names, 
              multiple=F)
})
    
#PEARSON'S CODE:
  output$table <- DT::renderDataTable(
    DT::datatable(rownames = FALSE, {
      for_display <- norfolk_tract %>% filter(NAME == input$tract_locations)
      for_display <- for_display %>% select(NAME, diabetes_health_improvement, diabetes_mileage_increase, high_blood_pressure_health_improvement, 
                                            high_blood_pressure_mileage_increase,	poor_physical_health_health_improvement,	poor_physical_health_mileage_increase)
      
    
      for_display <- for_display %>%  select(-geometry)
      vals$cur_ct_data = for_display
      for_display
      
    }))
  
  output$tract_diabetes_improvement <- renderText({paste("The current improvement for diabetes in this census tract is", vals$cur_ct_data$diabetes_health_improvement, "percent")})
  output$tract_bp_improvement <- renderText({paste("The current improvement for blood pressure in this census tract is", vals$cur_ct_data$high_blood_pressure_health_improvement, "percent")})
  output$tract_poor_phys_health_improvement <- renderText({paste("The current improvement for poor physical health in this census tract is", vals$cur_ct_data$poor_physical_health_health_improvement, "percent")})
  
  output$title_panel <- renderText({input$tract_locations})
  
  
 #removing data table while keeping text output, moving text output to the right of the map. Possibly adding it to map fluidRow().

  # for (i in norfolk_tract$GEOID) {
  #   if (i == bike_ped_model_sweep$census_tract) {
  #     norfolk_tract <- norfolk_tract[input$tract_locations == norfolk_tract$NAME,]
  #     #bike_ped_model_sweep <- bike_ped_model_sweep[bike_ped_model_sweep$census_tract == norfolk_tract$GEOID,]
  #   }
  #   #Can't figure out how to connect GEOID tag to the census tract name. Would it be better to merge/concat the datasets
  #   # or is there another method I am not seeing? Maybe merging them together as vectors or getting this for loop to work.
  # }
  # #bike_ped_model_sweep
  # norfolk_tract



    

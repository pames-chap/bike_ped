



###################################################
#
# The ui.R is designed to use separate files for each 
# tab in the UI.  The ui tab files are located in the
# ui folder.  Each file is accessed via "source" (see
# tab panel 2 "Map").  
#
###################################################




shinyUI(navbarPage(title = "Digital Neighborhoods",
                   fluid = TRUE, 
                   collapsible = TRUE,
                   
                   ####################################
                   # tab panel 1 - Home
                   ####################################
                   tabPanel("Home"),
                   
                   ####################################
                   # tab panel 2 - Neighborhood Browser
                   ####################################
                   tabPanel("Map", source(here::here("ui", "ui_map.R"),  local = TRUE)$value)
))





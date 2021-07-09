



###################################################
#
# Similar to ui.R, the server.R is designed to use 
# separate files for each tab in the UI.  The server
# files are located in the server folder.  Each file
# is accessed via "source" (see tab panel 2 "Map").  
#
###################################################



shinyServer(function(input, output, session) {
  
  source(here::here("server", "server_map.R"),  local = TRUE)$value
  
}) # End shinyServer




###################################################
#
# UI for Map Tab
#
###################################################


fluidPage(
  titlePanel(textOutput("title_panel")),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(width = 12,
               uiOutput('tract_location')
        ),
        column(width = 12,
               textOutput("tract_diabetes_improvement"),
               textOutput("tract_bp_improvement"),
               textOutput("tract_poor_phys_health_improvement"))
      ),
      width = 4),
    mainPanel(
      tagList(
        div(class = "container-fluid",
            #h1("Neighborhood Browser"),
            # fluidRow(
            #   column(width = 2,
            #          uiOutput('map_location')
            #   )
            # ),
            
            fluidRow(
              #  Turn the leaflet background to white
              tags$head(
                tags$style(HTML(".leaflet-container { background: #ffffff; }"))
              ),
              # column(width = 3, offset = 1,
              #        h2(textOutput("map.tract"))
              #),
              column(width = 12,
                     leafletOutput("map", width = "100%", height = 700)
              )
            ),
        ),
      ),
      width = 8),
    
    position = "left",
    fluid = FALSE
  ),
  fluidRow(
    column(width = 5,
           DT::dataTableOutput('table'))
  )
)





#-------------------------------------------------------
# 
# ###################################################
# #
# # UI for Map Tab
# #
# ###################################################
# 
# 
# tagList(
#   div(class = "container-fluid",
#       #h1("Neighborhood Browser"),
#       # fluidRow(
#       #   column(width = 2,
#       #          uiOutput('map_location')
#       #   )
#       # ),
#       
#       
#       fluidRow(
#         #  Turn the leaflet background to white
#         tags$head(
#           tags$style(HTML(".leaflet-container { background: #ffffff; }"))
#         ),
#         column(5,
#                leafletOutput("map", height = 700)
#         ),
#         column(width = 2, offset = 1,
#                h2(textOutput("map.tract"))
#         ),
#         
#         column(width = 2,
#                uiOutput('tract_location')
#         ),
#         
#         column(width = 10,
#                textOutput("tract_diabetes_improvement"),
#                textOutput("tract_bp_improvement"),
#                textOutput("tract_poor_phys_health_improvement"))
#       ),
#       
#   ),
#   
#   fluidPage(
#     sidebarLayout(
#       fluidRow(column(width = 10,
#                       textOutput("tract_diabetes_improvement"),
#                       textOutput("tract_bp_improvement"),
#                       textOutput("tract_poor_phys_health_improvement")))
#     )
#   ),
#   
#   DT::dataTableOutput('table')
#   
#   
# )


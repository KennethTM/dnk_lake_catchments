library(shiny)
library(leaflet)
library(sf)

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session) {
    
    geo_list <- readRDS(paste0(getwd(), "/geo_list.rds"))

    map_text <- tags$div(
        HTML("<b>About</b>
            <p>
            Lake catchments in the Øresund region, Denmark:
            <br>
            <ul>
            <li>Select a lake to show the catchment</li>
            <li>Select a catchment to show land cover info</li>
            </ul>
            <em></em>
            </p>
            <br>
            <p>
            <em>Data based on a digital elevation model (1.6 m resolution)
            <br>
            and Corine Land Cover (2012). Polygons are simplied for 
            <br>
            viewing. Contact for national data and info.
            </em>
            </p>
            <br>
            <p>
            <b>Contact</b>
            <br>
            Kenneth Thorø Martinsen
            <br>
            kenneth2810@gmail.com
            <br>
            <a href='www.datainwater.com'>www.datainwater.com</a>
            </p>")
    )
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>%
            addMapPane("lakes", zIndex=420) %>% 
            addMapPane("catchment",zIndex=410) %>% 
            addMapPane("basin",zIndex=400) %>% 
            addControl(map_text, position = "topright") %>% 
            addPolygons(data = geo_list$basins, fill = FALSE, color = gray(0.3), options = pathOptions(pane = "basin")) %>% 
            addPolygons(data = geo_list$lakes, color = "cornflowerblue", popup = ~label, layerId = ~lake_id, options = pathOptions(pane = "lakes"))
    })
    
    observeEvent(input$map_shape_click, {
        id <- input$map_shape_click$id
        catchments_sub <- geo_list$catchments[geo_list$catchments$lake_id == id, ]
        
        leafletProxy('map') %>% 
            addPolygons(data = catchments_sub, color = "blueviolet", popup = ~label, group = "catch", options = pathOptions(pane = "catchment"))
    })
    
    observeEvent(input$map_click, {
        leafletProxy('map') %>% 
            clearGroup("catch")
    })

}

shinyApp(ui, server)

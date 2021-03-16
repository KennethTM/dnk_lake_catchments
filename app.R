library(shiny)
library(leaflet)
library(sf)
library(tidyverse)

ui <- bootstrapPage(
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
    leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session) {
    
    #collect data in one .rds object or .sqlite database
    basin_id <- 19
    basins <- paste0(getwd(), "/data/basin_sub/basin_", basin_id, ".shp")
    lakes <- paste0(getwd(), "/data/lakes_sub/basin_lakes_", basin_id, ".shp")
    catchments <- paste0(getwd(), "/data/catchments_sub/basin_catchments_", basin_id, ".shp")
    
    basins_sf <- st_read(basins) %>% 
        st_transform(4326)
    
    catchments_sf <- st_read(catchments) %>% 
        mutate(area=as.numeric(st_area(geometry)),
               circumference=as.numeric(st_length(st_cast(geometry, "LINESTRING"))),
               label = paste0("Catchment area: ", round(area, 0), " m<sup>2</sup>", "<br>",
                              "Catchment circumference: ", round(circumference, 0), " m")) %>% 
        st_transform(4326)
    
    lakes_sf <- st_read(lakes) %>% 
        filter(lake_id %in% catchments_sf$lake_id) %>% 
        mutate(area=as.numeric(st_area(geometry)),
               circumference=as.numeric(st_length(geometry)),
               label = paste0("Lake area: ", round(area, 0), " m<sup>2</sup>", "<br>",
                              "Lake circumference: ", round(circumference, 0), " m")) %>% 
        st_transform(4326)
    
    output$map <- renderLeaflet({
        leaflet() %>% 
            addTiles() %>%
            addMapPane("lakes", zIndex=420) %>% 
            addMapPane("catchment",zIndex=410) %>% 
            addMapPane("basin",zIndex=400) %>% 
            addPolygons(data = basins_sf, fill = FALSE, color = "black", options = pathOptions(pane = "basin")) %>% 
            addPolygons(data = lakes_sf, color = "blue", popup = ~label, layerId = ~lake_id, options = pathOptions(pane = "lakes"))
    })
    
    observeEvent(input$map_shape_click, {
        id <- input$map_shape_click$id
        catchments_sf_sub <- catchments_sf[catchments_sf$lake_id == id, ]
        
        leafletProxy('map') %>% 
            addPolygons(data = catchments_sf_sub, color = "red", popup = ~label, group = "catch", options = pathOptions(pane = "catchment"))
    })
    
    observeEvent(input$map_click, {
        leafletProxy('map') %>% 
            clearGroup("catch")
    })

}

shinyApp(ui, server)

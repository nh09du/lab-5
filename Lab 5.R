#-----------------------------------------------
# Lab 5: Shiny Web Map Development
# Name: Nazir Hoosein
# ID: 4570776
#
#------------------------------------------------

install.packages(c("raster", "maptools", "sp", "rgdal", "rgeos", "maps", "mapproj", "shiny", "leaflet"))

library(sp)     # needed for transformations to other coordinate systems
library(raster)
library(maps)
library(maptools)
library(mapproj)
library(rgdal)  # needed for reading in shapefiles
library(rgeos)  # needed for buffers and other vector operations
library(leaflet) # provides basemap

setwd("E:/ERSC 3P95/Lab 5 - Aquifer_Shiny_App/Aquifer_Shiny_App for lab/data")

US_Aquifers<-readOGR("Aquifers_US/Aquifer_US.shp", layer="Aquifer_US")
US_Ag_Census <- readOGR("Agriculture_Census_2002/Agri_Census_2002.shp", layer="Agri_Census_2002")
US_Precip<- readOGR("Average_annual_Precipitation_1961-90-US/Precip_US-1961-90.shp", layer="Precip_US-1961-90")
US_Urban<- readOGR("Urban_areas-US/Urban_areas-US.shp", layer="Urban_areas-US")

HP_Aquifer <- US_Aquifers[US_Aquifers@data$AQ_NAME == "High Plains aquifer", ]
High_Irrigation <- US_Ag_Census[US_Ag_Census@data$M089_02 >= 25, ]
Low_Precip <- US_Precip[US_Precip@data$RANGE <= 22.5, ]

HP_Aquifer_proj <- spTransform(HP_Aquifer, CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
US_Urban_proj <- spTransform(US_Urban, CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
High_Irrigation_proj <- spTransform(High_Irrigation, CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))
Low_Precip_proj <- spTransform(Low_Precip, CRS("+proj=lcc +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs"))

#### ------------- Make buffer

Aquifer_Buff150 <- gBuffer(HP_Aquifer_proj, width = 241400) # 150 miles = 241.4 km 

# Aquifer_Buff150 is of class 'SpatialPolygons'
blank_df <- data.frame( ID=1, row.names = 1)
Aquifer_Buff150_df <- SpatialPolygonsDataFrame(Sr=Aquifer_Buff150, data=blank_df, match.ID = FALSE)

# You have to convert SpatialPolygons to SpatialPolygonsDataFrame if exporting ESRI shapefile
writeOGR(Aquifer_Buff150_df,".",layer="Made_in_R/Aquifer_Buff150", driver="ESRI Shapefile", overwrite_layer = TRUE)

#### Select urban areas inside buffer
Urban_inside_Aq <- !is.na(over(US_Urban_proj, Aquifer_Buff150))
Urban_inside_Aq <- US_Urban_proj[Urban_inside_Aq, ]
# Urban_inside_Aq is of class 'SpatialPolygonsDataFrame'

### Final spatial query - urban areas inside water stressed areas

Urban_high_irrig <- !is.na(over(Urban_inside_Aq, as(High_Irrigation_proj, "SpatialPolygons")))
Urban_inside_high_irrig <- Urban_inside_Aq[Urban_high_irrig, ]

Urban_low_precip <- !is.na(over(Urban_inside_Aq, as(Low_Precip_proj, "SpatialPolygons")))
Urban_inside_low_precip <- Urban_inside_Aq[Urban_low_precip, ]

#### Make shapefiles from the data
writeOGR(Urban_inside_high_irrig,".",layer="Made_in_R/Urban_inside_high_irrig", driver="ESRI Shapefile", overwrite_layer = TRUE)
writeOGR(Urban_inside_low_precip,".",layer="Made_in_R/Urban_inside_low_precip", driver="ESRI Shapefile", overwrite_layer = TRUE)

leaflet(states)
  addProviderTiles(providers$Esri.NatGeoWorldMap)

### ui.R remaining code
# ui.R  - this programs the user interface, what the user sees. The different components are 
#         given id's (in quotes) so that the server program knows where to get the data
ui <- fluidPage(
  titlePanel("US Water Aquifers"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Set size of buffer around aquifer"),
      
      sliderInput("size",
                  label = "Range of interest (km):",
                  min = 0,
                  max = 500,
                  value = 250),
      
      selectInput("Aquifer", label="Choose a US aquifer", choices = unique(US_Aquifers@data$AQ_NAME), selected = "High Plains aquifer")
      
    ),
    
    mainPanel(plotOutput("map")) #Output: Map ----
   
  )
)


### Server.r remaining code
# This program can be built up to do analysis on US groundwater aquifers
US_Aquifer_proj <- spTransform(US_Aquifers, CRS("+proj=poly +lat_0=0 +lon_0=-100 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs")) # polyconic projection

server <- function(input, output) {
  
  output$map <- renderPlot({
    
    Aquifer_focus <- US_Aquifer_proj[US_Aquifer_proj@data$AQ_NAME == input$Aquifer, ]
    Aquifer_Buff <- gBuffer(Aquifer_focus, width = input$size*1000)
    
    plot(Aquifer_Buff)
    
  })
}

shinyApp(ui = ui, server = server)







#setwd("H:/USER/JGenser/Pot Holes/Program")
library(leaflet)
library(shiny)
library(plyr)

##import cleaned pothole data
df = read.csv("mappingData.csv",header=T, sep =",")
many = read.csv("manyFills.csv", header=T, sep=",")

##if lag is negative, set to 0
df$lag[which(df$lag == -1 )] <- 0

##if lag is greater than 3 weeks, set to 22 days
df$lag[which(df$lag > 21  )] <- 22



##create color palate
cpal <- colorNumeric(palette = colorRamp(c("dodgerblue4", "Dark Red")), domain = sort(df$lag, decreasing=F))

cznData = subset(df, grepl("Citizen", Source))
ctyData = subset(df, grepl("City", Source))
callData = subset(df, grepl("Constituent", Source))
selfData = subset(df, grepl("Self", Source))
empData = subset(df, grepl("Employee", Source))
cambData = df[df$Source == "",]

ui = fluidPage(
  
  leafletOutput("map", height = 720, width = 1280) 
)

server <- function(input, output) {
  



  ##render map and add circles
  output$map <- renderLeaflet ({
    leaflet(df) %>%
    setView(-71.083, 42.353, 13) %>%
    addProviderTiles("Stamen.TonerLite", options = providerTileOptions(noWrap=T)) %>%
    addLegend("bottomright", pal = cpal, values = df$lag, title ="Days Until Repair" , opacity = 1) %>%
    addCircles(data = cznData, radius=1, color=cpal(df$lag), fillColor = cpal(df$lag),
               fillOpacity = 0.2, opacity = 0.2, 
               group = "Citizens Connect App") %>%
    addCircles(data = ctyData, radius=1, color=cpal(df$lag), fillColor = cpal(df$lag),
               fillOpacity = 0.2, opacity = 0.2,
               group = "City Workers App") %>%
    addCircles(data = callData, radius=1, color=cpal(df$lag), fillColor = cpal(df$lag),
               fillOpacity = 0.2, opacity = 0.2,
               group = "Constituent Call-in") %>%
    addCircles(data = selfData, radius=1, color=cpal(df$lag), fillColor = cpal(df$lag),
               fillOpacity = 0.2, opacity = 0.2,
               group = "Self Service") %>%
    addCircles(data = empData, radius=1, color=cpal(df$lag), fillColor = cpal(df$lag),
               fillOpacity = 0.2, opacity = 0.2,
               group = "Employee Generated") %>%
    addCircles(data = cambData, radius=1, color=cpal(df$lag), fillColor = cpal(df$lag),
               fillOpacity = 0.2, opacity = 0.2,
               group = "Cambridge (Source NA)") %>%
    addCircles(data = many, radius = 1, color = "black", fillColor = "grey", fillOpacity = 0.5, opacity=0.5,
               group = "Repeat Offender")  %>%
    addLayersControl(overlayGroups = 
                      c("Citizens Connect App", "City Workers App", "Constituent Call-in", "Self Service", "Employee Generated", "Cambridge (Source NA)", "Repeat Offender"),
                        options = layersControlOptions(collapsed=F)) %>% hideGroup(c("City Workers App", "Constituent Call-in", "Self Service", "Employee Generated", "Cambridge (Source NA)", "Repeat Offender"))

  })
  
}

shinyApp(ui = ui, server = server)
  
  
  




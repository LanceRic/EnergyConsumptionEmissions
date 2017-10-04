#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(sp)
library(ggplot2)
library(tools)
library(colorRamps)
library(dplyr)

# mead in monthly powere plant consumption data
power <- read.csv("PowerPlants.csv")
power[,c(9,10,16:213)] <- data.frame(lapply(power[,c(9,10,16:213)], as.numeric), stringsAsFactors=FALSE)

# read in greenhouse gas emissions data
emissions <- read.csv("emission_annual.csv")
emissions$CO2..Metric.Tons. <- as.numeric(gsub(",", "",as.character(emissions$CO2..Metric.Tons.)))
emissions$SO2..Metric.Tons. <- as.numeric(gsub(",", "",as.character(emissions$SO2..Metric.Tons.)))
emissions$NOx..Metric.Tons. <- as.numeric(gsub(",", "",as.character(emissions$NOx..Metric.Tons.)))

# read in geojson file for US
states <- geojsonio::geojson_read("us-states.geojson", what = "sp")
class(states)
names(states)

# read in csv with state names to abbreviations 
state_abbrev <- read.csv(paste(my_dir, "states.csv", sep= ""))

# Merge states spatial polygons data frame with state_abbrev data frame
states <- merge(states, state_abbrev, by.x = "NAME", by.y = "State")

ui <- fluidPage(
  
  navbarPage("Explore United States Energy: Consumption and Emissions", id = "nav",
    tabPanel("Monthly Fuel Consumption of Power Plants",
             div(class="outer",
                 tags$head(
                   # Include our custom CSS
                   includeCSS("styles.css")
                 ),
                 
                 leafletOutput("map", width="100%", height="100%"),
                 
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                               width = 330, height = "auto",
                               
                               selectInput("year", "Year:",
                                           c("2001" = "2001",
                                             "2002" = "2002",
                                             "2003" = "2003",
                                             "2004" = "2004",
                                             "2005" = "2005",
                                             "2006" = "2006",
                                             "2007" = "2007",
                                             "2008" = "2008",
                                             "2009" = "2009",
                                             "2010" = "2010",
                                             "2011" = "2011",
                                             "2012" = "2012",
                                             "2013" = "2013",
                                             "2014" = "2014",
                                             "2015" = "2015",
                                             "2016" = "2016",
                                             "2017" = "2017"
                                           )
                               ),
                               conditionalPanel(
                                 condition = "input.year == '2001' || input.year == '2002' || input.year == '2003' || input.year == '2004' || 
                                input.year == '2005' || input.year == '2006' || input.year == '2007' || input.year == '2008' || 
                                input.year == '2009' || input.year == '2010' || input.year == '2011' || input.year == '2012' || 
                                input.year == '2013' || input.year == '2014' || input.year == '2015' || input.year == '2016'",
                                 selectInput("month", "Month:",
                                             c("January" = "January",
                                               "February" = "February",
                                               "March" = "March",
                                               "April" = "April",
                                               "May" = "May",
                                               "June" = "June",
                                               "July" = "July",
                                               "Augest" = "Augest",
                                               "September" = "September",
                                               "October" = "October",
                                               "November" = "November",
                                               "December" = "December"
                                             )
                                 )
                               ),
                               conditionalPanel(
                                 condition = "input.year == '2017'",
                                 selectInput("month", "Month:",
                                             c("January" = "January",
                                               "February" = "February",
                                               "March" = "March",
                                               "April" = "April",
                                               "May" = "May",
                                               "June" = "June"
                                             )
                                 )
                               ),
                               selectInput("type", "Fuel Type:",
                                           c("Natural Gas" = "natural gas",
                                             "Coal" = "bituminous coal"
                                           )
                               )
                 )
      )
    ),
    tabPanel("Annual Greenhouse Gas Emissions by State",
             div(class="outer",
                 tags$head(
                   # Include our custom CSS
                   includeCSS("styles.css")
                 ),
                 
                 leafletOutput("map2", width="100%", height="100%"),
                 
                 absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                               draggable = TRUE, top = 100, left = "auto", right = 20, bottom = "auto",
                               width = 330, height = "auto",
                               
                               selectInput("year2", "Year:",
                                           c("1990" = "1990",
                                             "1991" = "1991",
                                             "1992" = "1992",
                                             "1993" = "1993",
                                             "1994" = "1994",
                                             "1995" = "1995",
                                             "1996" = "1996",
                                             "1997" = "1997",
                                             "1998" = "1998",
                                             "1999" = "1999",
                                             "2000" = "2000",
                                             "2001" = "2001",
                                             "2002" = "2002",
                                             "2003" = "2003",
                                             "2004" = "2004",
                                             "2005" = "2005",
                                             "2006" = "2006",
                                             "2007" = "2007",
                                             "2008" = "2008",
                                             "2009" = "2009",
                                             "2010" = "2010",
                                             "2011" = "2011",
                                             "2012" = "2012",
                                             "2013" = "2013",
                                             "2014" = "2014",
                                             "2015" = "2015"
                                           )
                               ),
                               selectInput("emission", "Emission Type:",
                                           c("Carbon Dioxide" = "CO2..Metric.Tons.",
                                             "Sulfer Dioxide" = "SO2..Metric.Tons.",
                                             "Nitrogen Oxides" = "NOx..Metric.Tons."
                                           )
                               ),
                               selectInput("source", "Source:",
                                           c("All Sources" = "All Sources",
                                             "Coal" = "Coal",
                                             "Natural Gas" = "Natural Gas",
                                             "Other" = "Other",
                                             "Petroleum" = "Petroleum"
                                           )
                               )
             )
      )
    )
  )
)


server <- function(input, output) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -115, lat = 52, zoom = 3)
  })
  
  myLabelFormat = function(..., reverse_order = FALSE){ 
    if(reverse_order){ 
      function(type = "numeric", cuts){ 
        cuts <- sort(cuts, decreasing = T)
        cuts <- format(cuts,big.mark=",",scientific=FALSE)
      } 
    }else{
      labelFormat(...)
    }
  }
  
  # Use variables given by user to add circles and legend to leaflet map
  observe({
    chosen_year <- input$year
    chosen_month <- input$month
    chosen_type <- input$type
    chosen_subtype <- "all primemovers"
    
    color_by <- paste(chosen_month, chosen_year, sep = ".")
    
    month.power <- na.omit(power[which(power$fuel_type == chosen_type & power$fuel_subtype == chosen_subtype), 
                                 c("plant_name", "fuel_type", "lat", "lon", color_by)])
    
    coords = cbind(month.power$lon, month.power$lat)
    
    # make spatial data frame
    spdf = SpatialPointsDataFrame(coords, month.power)
    
    colorData <- spdf[[color_by]]
    
    pal <- colorNumeric(colorRamps::matlab.like(100), colorData)
    negpal <- colorNumeric(rev(colorRamps::matlab.like(100)), colorData)
    
    radius <- spdf[[color_by]]/max(spdf[[color_by]]) * 20000
    
    leafletProxy("map", data = spdf) %>%
      clearShapes() %>%
      addCircles(~lon, ~lat, radius = radius, color = pal(colorData), 
                 fillOpacity=0.8, fillColor=pal(colorData)) %>%
      addLegend("bottomleft", pal=negpal, values=colorData, layerId="colorLegend", 
                labFormat = myLabelFormat(reverse_order = T), title = "Fuel Consumption MMBtu")
  })
  
  # Popup graph to show in popup
  pop <- function(plant, i) {
    fldr <- tempfile()
    dir.create(fldr)
    
    g <- ggplot(data=subset(plant, !is.na(electric_fuel_consumption_MMBtu)), aes(date, electric_fuel_consumption_MMBtu)) +
      geom_point() +
      geom_line() +
      ggtitle(paste("Fuel Consumption at Power Plant")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      xlab("Time") +
      ylab("Fuel Consumption MMBtu")
    filename <- paste(fldr, "test.svg", sep = "/")
    
    svg(filename = filename,
        width = 300 * 0.01334, height = 250 * 0.01334)
    print(g)
    dev.off()
    
    pic <- paste(readLines(filename), collapse = "")
    
    return(pic)
  }
  
  # Show a popup at the given location
  showPlantPopup <- function(lng, lat) {
    chosen_year <- input$year
    chosen_month <- input$month
    chosen_type <- input$type
    chosen_subtype <- "all primemovers"
    
    color_by <- paste(chosen_month, chosen_year, sep = ".")
    
    month.power <- na.omit(power[which(power$fuel_type == chosen_type & power$fuel_subtype == chosen_subtype),
                                 c("plant_name", "fuel_type", "lat", "lon", color_by)])
    
    selectedPlant <- month.power[which(month.power$lat == lat & month.power$lon == lng),]
    
    text <- as.character(tagList(
      tags$strong(HTML(sprintf("Total Consumption for %s:", gsub("\\.", " ", color_by)))), 
      sprintf(" %s MMBtu", selectedPlant[color_by]), tags$br(),
      tags$strong(HTML(sprintf("Plant Name (Number):"))),
      sprintf(" %s", selectedPlant$plant_name), tags$br(),
      tags$strong(HTML(sprintf("Fuel Type:"))),
      sprintf(" %s", toTitleCase(as.character(selectedPlant$fuel_type))), tags$br()
    ))
    
    which_plant <- which(power$lat == lat & power$lon == lng & 
                           power$fuel_type == chosen_type & power$fuel_subtype == chosen_subtype)
    plant <- as.data.frame(t(power[which_plant,16:213]))
    plant$series_id <- power$series_id[which_plant]
    plant$date <- paste(rownames(plant),"01", sep = ".")
    plant$date <- as.Date(plant$date,format = "%B.%Y.%d")
    colnames(plant)[1] <- "electric_fuel_consumption_MMBtu"
    
    content <- paste(text, pop(plant,1))
    
    leafletProxy("map") %>%
      addPopups(lng, lat, popup = content, options=popupOptions(maxWidth = 500))
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showPlantPopup(event$lng, event$lat)
    })
  })
  
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -115, lat = 52, zoom = 3)
  })
  
  observe({
    chosen_year <- input$year2
    chosen_emission <- input$emission
    chosen_source <- input$source
    prod_type <- "Total Electric Power Industry"
    
    # Create subset of emission data frame for producer type and energy sourcen and filter out "US-TOTAL"
    emissions_sub <-  emissions[which(emissions$State != "US-TOTAL" &
                                        emissions$Year == chosen_year &
                                        emissions$Producer.Type == prod_type &
                                        emissions$Energy.Source == chosen_source),]
    
    # Merge states spatial polygons data frame with emissions data frame
    emissions.spdf <- merge(states, emissions_sub, by.x = "Abbreviation", by.y = "State")
    
    colorData <- emissions.spdf[[chosen_emission]]
    
    pal <- colorNumeric(colorRamps::matlab.like(100), colorData)
    negpal <- colorNumeric(rev(colorRamps::matlab.like(100)), colorData)
    
    title <- case_when(
      chosen_emission == "CO2..Metric.Tons." ~ "CO2 Metric Tons",
      chosen_emission == "SO2..Metric.Tons." ~ "SO2 Metric Tons",
      chosen_emission == "NOx..Metric.Tons." ~ "NOx Metric Tons"
    )
    
    leafletProxy("map2", data = emissions.spdf) %>%
      #clearShapes() %>%
      addPolygons(
        fillColor = pal(colorData),
        weight = 2,
        opacity = 1,
        color = "white",
        dashArray = "3",
        fillOpacity = 0.7) %>%
      addLegend("bottomleft", pal=negpal, values=colorData, layerId="colorLegend", 
                labFormat = myLabelFormat(reverse_order = T), title = title)
  })
}


# Run the application
shinyApp(ui = ui, server = server)
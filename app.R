# Dependencies
library(shiny)
library(shinydashboard)
library(leaflet)
library(plotly)
library(httr)
library(rgdal)

### Load Data
load("./neighborhoodPolygon.rdata")
# StreetEasyCombined = read.csv("./StreetEasyCombined.csv", stringsAsFactors = FALSE)
# GeoCode = read.csv("./NbhGeocode.csv", stringsAsFactors = FALSE)[,2:5]
# r <- GET('http://data.beta.nyc//dataset/0ff93d2d-90ba-457c-9f7e-39e47bf2ac5f/resource/35dd04fb-81b3-479b-a074-a27a37888ce7/download/d085e2f8d0b54d4590b1e7d1f35594c1pediacitiesnycneighborhoods.geojson')
# nyc_neighborhoods <- readOGR(content(r,'text'), 'OGRGeoJSON', verbose = F)

### UI ###
ui = dashboardPage(

  ## Dashboard Header
  dashboardHeader(title = "RExplorer",
                  dropdownMenuOutput("messageMenu")
  ), # close dashboard header

  ## Sidebar Menu
  dashboardSidebar(width = 150, sidebarMenu(
      # menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Widgets", tabName = "widgets", icon = icon("tasks")),
      menuItem("Charts", tabName = "charts", icon = icon("table"))
  )), # close sidebar menu

  ## Dashboard Body
  dashboardBody(

    ## Tab Items Pages
    tabItems(

      # ## Home
      # tabItem(tabName = "home",
      #         box(height = NULL, width = NULL,
      #             leaflet(height = "890px") %>%
      #                 addTiles() %>%
      #                 setView(-73.87, 40.73, zoom = 11))), # close home page

      ## Widgets
      tabItem(tabName = "widgets",
              column(width = 9,
                     # Main View Box
                     box(width = NULL, status = "danger", height = 425, leafletOutput("mapPlot")),
                     fluidRow(
                         box(width = 6, status = "success", plotlyOutput("salePricePlot")),
                         box(width = 6, status = "success", plotlyOutput("rentalPricePlot")))
                     ), # close main view box
              column(width = 3,

                     # Input Selector Box
                     box(width = NULL, status = "warning",
                         selectInput("boro", "Borough",
                                     choices = c("Manhattan", "Brooklyn", "Queens"), selected = "Manhattan"),
                         
                         selectInput("neighborhood", "Neighborhood",
                                     choices = c("Chelsea", "Long Island City", "Bushwick"), selected = "Chelsea"),

                         selectInput("saleType", "Type of Sales",
                                     choices = c("Condo", "Townhouse/Single Family"), selected = "Condo"),

                         selectInput("rentalType", "Type of Rental",
                                     choices = c("Studio", "1 Bedroom", "2 Bedrooms", "3+ Bedrooms"),
                                     selected = "1 Bedroom")
                         ), # close input selector box
                     
                     # Add some white spaces
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),
                     br(),

                     # Infoboxes
                     fluidRow(infoBox("Current Month, Number of Listing", "232",
                                      fill = TRUE, width = NULL, col = "blue", icon = icon("list")),
                              infoBox("Current Month, Median Sale Price", "2,500,000",
                                      fill = TRUE, width = NULL, col = "light-blue"),
                              infoBox("Current Month, Median Rental Price", textOutput("rentalRent"),
                                      fill = TRUE, width = NULL, col = "light-blue"),
                              infoBox("Expected Return of Investment in 1 year", "8%",
                                      fill = TRUE, width = NULL, col = "black", icon = icon("money"))
                              ) # close infoboxes
                     ) # close column
              ), # close widget page
      tabItem(tabName = "charts", h2("charts"))
      ) # close tab item
    ) # close body
) # end UI

### Server ###
server = function(input, output) {
    
    SalePriceInfo = reactive({
        StreetEasyCombined %>% 
            select(., Area, time, 
                   condoMedAskPrice, 
                   condoMedSalePrice, 
                   sfMedAskPrice, 
                   sfMedSalePrice) %>% 
            group_by(., Area) %>% 
            arrange(., time) %>% 
            filter(., Area == input$neighborhood)
    })

    RentalPriceInfo = reactive({
        StreetEasyCombined %>% 
            select(., Area, time, 
                   rentalStudioMedPrice, 
                   rentalOneBdMedPrice, 
                   rentalTwoBdMedPrice, 
                   rentalThreeBdMedPrice) %>% 
            group_by(., Area) %>% 
            arrange(., time) %>% 
            filter(., Area == input$neighborhood)
    })
    
    observeEvent(input$neighborhood, {
        print(paste0("You have chosen: ", input$neighborhood))
        print(paste0("Median Rental Price: ", getRentalMedRent(input$rentalType)))
    })

    
    getLatLon = function (area) {
        StreetEasyCombined %>% 
            filter(Area == area) %>% 
            head(1) %>% 
            left_join(GeoCode, by = "Area") %>% 
            select(lon, lat) %>% 
            return()
    }
    
    getRentalMedRent = function(type) {
        if (type == "None") {"No Rental Type Selected"}
        else if (type == "Studio") {
            RentalPriceInfo() %>% ungroup() %>% select(rentalStudioMedPrice) %>% tail(1) %>% as.numeric()}
        else if (type == "1 Bedroom") {
            RentalPriceInfo() %>% ungroup() %>% select(rentalOneBdMedPrice) %>% tail(1) %>% as.numeric()}
        else if (type == "2 Bedrooms") {
            RentalPriceInfo() %>% ungroup() %>% select(rentalTwoBdMedPrice) %>% tail(1) %>% as.numeric()}
        else if (type == "3+ Bedrooms") {
            RentalPriceInfo() %>% ungroup() %>% select(rentalThreeBdMedPrice) %>% tail(1) %>% as.numeric()}
    }
    
    output$mapPlot = renderLeaflet({
        if (input$neighborhood == "None") {
            leaflet(height = "400px") %>%
                addTiles() %>%
                setView(-73.87, 40.73, zoom = 10)
        } else {
            nyc_neighborhoods[nyc_neighborhoods@data$neighborhood == input$neighborhood,] %>% 
                leaflet(height = "400px") %>%
                addTiles() %>%
                setView(getLatLon(input$neighborhood)$lon, getLatLon(input$neighborhood)$lat, zoom = 13) %>% 
                addPolygons(popup = ~neighborhood, weight = 1, smoothFactor = 0.5)
        }
    })

    output$salePricePlot = renderPlotly({
        if (input$neighborhood == "None") {plotly_empty()} else {
            SalePriceInfo() %>% ungroup() %>% 
                plot_ly(., x = ~time, y = ~condoMedAskPrice, type = "scatter", mode = "lines+markers", name = "Condo Ask", connectgaps = TRUE) %>%
                add_trace(., x = ~time, y = ~condoMedSalePrice, mode = "lines+markers", name = "Condo Sale", connectgaps = TRUE) %>%
                # add_trace(., x = ~time, y = ~sfMedAskPrice, mode = "lines+markers", name = "Townhouse/SF Ask", connectgaps = TRUE) %>%
                # add_trace(., x = ~time, y = ~sfMedSalePrice, mode = "lines+markers", name = "Townhouse/SF Sale", connectgaps = TRUE) %>% 
                layout(legend = list(x = 0, y = -0.5, orientation = "h"),
                       xaxis = list(title=""), 
                       yaxis = list(title=""),
                       hovermode = "compare")}
    })
    
    output$rentalPricePlot = renderPlotly({
        if (input$neighborhood == "None") {plotly_empty()} else {
            RentalPriceInfo() %>% 
                plot_ly(., x = ~time, y = ~rentalStudioMedPrice, type = "scatter", mode = "lines+markers", name = "Studio") %>%
                add_trace(., x = ~time, y = ~rentalOneBdMedPrice, mode = "lines+markers", name = "One Bedroom") %>%
                add_trace(., x = ~time, y = ~rentalTwoBdMedPrice, mode = "lines+markers", name = "Two Bedroom") %>%
                add_trace(., x = ~time, y = ~rentalThreeBdMedPrice, mode = "lines+markers", name = "Three Bedroom") %>% 
                layout(legend = list(x = 0, y = -0.5, orientation = "h"),
                       xaxis = list(title=""), 
                       yaxis = list(title=""),
                       hovermode = "compare")}
    })
    
    output$rentalRent = renderText({
        getRentalMedRent(input$rentalType)
    })
    
} # end server

### Run ###
shinyApp(ui, server)

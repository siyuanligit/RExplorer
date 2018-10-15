# Dependencies
library(shiny)
library(shinydashboard)
library(dplyr)
library(markdown)
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
  dashboardSidebar(collapsed = FALSE, 
                   sidebarMenu(
                       menuItem("Home", tabName = "home", icon = icon("home")),
                       menuItem("Visualizations", tabName = "widgets", icon = icon("tasks")),
                       menuItem("About", tabName = "about", icon = icon("user"))
                       )), # close sidebar menu

  ## Dashboard Body
  dashboardBody(

    ## Tab Items Pages
    tabItems(

      ## Home
      tabItem(tabName = "home",
              fluidRow(
                  column(width = 1),
                  column(width = 8,
                         includeMarkdown("./intro.md")),
                  column(width = 3))
              ), # close home page

      ## Widgets
      tabItem(tabName = "widgets",
              column(width = 9,
                     # Main View Box
                     box(width = NULL, status = "danger", height = 425, leafletOutput("mapPlot")),
                     fluidRow(
                         tabBox(title = " ", height = 425, width = 6,
                                tabPanel("Sales Summary", htmlOutput("SalesSummaryText")),
                                tabPanel("Sales Chart", plotlyOutput("salePricePlot", height = 400))),
                         tabBox(title = " ", height = 425, width = 6,
                                tabPanel("Rental Chart", plotlyOutput("rentalPricePlot", height = 400))))
                     ), # close main view box
              column(width = 3,

                     # Input Selector Box
                     box(width = NULL, status = "danger",
                         selectInput("boro", "Borough:",
                                     choices = c("Manhattan", "Brooklyn", "Queens"), selected = "Manhattan"),

                         uiOutput("neighborhoodSelect")
                         ), # close input selector box
                     
                     # Selector and Slider for ROI
                     box(width = NULL, status = "warning",
                         sliderInput("budget", "Budget for buying property:", 
                                      min = 500000, max = 5000000, step = 100000, value = 1000000),
                         selectInput("rentalType", "Type of Rental:",
                                     choices = c("Studio", "1 Bedroom", "2 Bedrooms", "3+ Bedrooms"),
                                     selected = "1 Bedroom")
                     ), # close input selector box

                     # Add some white spaces
                     br(),
                     br(),
                     br(),

                     # Infoboxes
                     fluidRow(infoBox("Past Year, Median Sales List Price", textOutput("saleAskPrice"),
                                      fill = TRUE, width = NULL, col = "blue"),
                              infoBox("Past Year, Median Sales Sale Price", textOutput("saleSalePrice"),
                                      fill = TRUE, width = NULL, col = "blue"),
                              infoBox("Past Year, Median Rental Price", textOutput("rentalRent"),
                                      fill = TRUE, width = NULL, col = "light-blue"),
                              infoBox("Expected Return of on Rent:", textOutput("roi"),
                                      fill = TRUE, width = NULL, col = "black", icon = icon("money"))
                              ) # close infoboxes
                     ) # close column
              ), # close widget page
      tabItem(tabName = "about", 
              column(width = 1),
              column(width = 11,
                     h2("About Author:"),
                     img(src = "face.jpg"),
                     h3("Siyuan Li, Derek"),
                     h4("Data Science Fellow at NYC Data Science Academy"),
                     h4("Volunteer at DataKind.org"),
                     br(),
                     h4("Master of Applied Statisitcs, UCLA 2016 - 2018"),
                     h4("Bachelor of Financial Mathematics and Statistics, UCSB 2011 - 2015"),
                     br(),
                     h4("Rental Real Estate Broker Assistant, Underwriting 2018"),
                     h4("Quantitative Analyst Intern, Mingshi Investment Management, Shanghai, China 2016"),
                     h4("Investment Analyst Intern, Soochow Securities, Suzhou, Jiangsu, China 2015"))
              ) # close about page
      ) # close tab item
    ) # close body
) # end UI

### Server ###
server = function(input, output) {

    ## Reactive functions
    # Locations availbale by Borough, Neighborhood, longitude and latitude
    locationList = reactive({
        StreetEasyCombined %>%
            select(Boro, Area) %>%
            distinct() %>%
            inner_join(nyc_neighborhoods@data$neighborhood %>% as.data.frame() %>% distinct() %>% setNames("Area") %>% mutate(Area = as.character(Area)), by = "Area") %>%
            left_join(., GeoCode, by = c("Boro", "Area"))
    })

    # Pricing History for Sales listings
    SalePriceInfo = reactive({
        StreetEasyCombined %>%
            select(., Area, time,
                   condoMedAskPrice,
                   condoMedSalePrice) %>%
            arrange(., time) %>%
            filter(., Area == input$neighborhood,
                   !is.na(condoMedAskPrice),
                   !is.na(condoMedSalePrice))
    })

    # Pricing History for Rental Listings
    RentalPriceInfo = reactive({
        StreetEasyCombined %>%
            select(., Area, time,
                   rentalStudioMedPrice,
                   rentalOneBdMedPrice,
                   rentalTwoBdMedPrice,
                   rentalThreeBdMedPrice) %>%
            group_by(., Area) %>%
            arrange(., time) %>%
            filter(., Area == input$neighborhood,
                   !is.na(rentalStudioMedPrice),
                   !is.na(rentalOneBdMedPrice),
                   !is.na(rentalTwoBdMedPrice),
                   !is.na(rentalThreeBdMedPrice))
    })
    
    SalesYearlySummary = reactive({
        SalePriceInfo() %>% 
            mutate(., year = format(as.Date(time), "%Y")) %>% 
            group_by(., year) %>% 
            summarise(., 
                      medianAsk = median(condoMedAskPrice), 
                      medianSale = median(condoMedAskPrice)) %>% 
            ungroup()
    })
    
    RentalYearlySummary = reactive({
        RentalPriceInfo() %>% 
            mutate(., year = format(as.Date(time), "%Y")) %>% 
            group_by(., year) %>% 
            summarise(., 
                      medianStudio = median(rentalStudioMedPrice), 
                      medianOneBd = median(rentalOneBdMedPrice),
                      medianTwoBd = median(rentalTwoBdMedPrice),
                      medianThreeBd = median(rentalThreeBdMedPrice)) %>% 
            ungroup()
    })

    ## Console output for event listening
    observeEvent(input$neighborhood, {
        print(paste0("You have chosen: ", input$neighborhood))
        print(paste0("Median Rental Price: ", input$rentalType, getRentalMedRent(input$rentalType)))
        print(paste0("nrow sales: ", SalePriceInfo() %>% nrow()))
        print(paste0("nrow rental: ", RentalPriceInfo() %>% nrow()))
        print(paste0("list price sales: ", getSaleAskPrice()))
        print(paste0("sale price sales: ", getSaleSalePrice()))
        print(paste0("mortgage payment:", input$budget * ((0.0504/12)/(1-((1+0.0504/12)^(-360))))))
    })

    ## Helper functions
    # Return longitude and latitude based on location selected, for map output
    getLatLon = function (area) {
        locationList() %>%
            filter(Area == area) %>%
            select(lon, lat)
    }
    
    # Return sales ask price for the last year
    getSaleAskPrice = function(){
        SalePriceInfo() %>% 
            filter(as.Date(time) >= as.Date("2017-08-01")) %>% 
            select(condoMedAskPrice) %>% 
            pull() %>% 
            median() %>% 
            as.numeric()
    }
    
    # Return sales sale price for the last year
    getSaleSalePrice = function(){
        SalePriceInfo() %>% 
            filter(as.Date(time) >= as.Date("2017-08-01")) %>% 
            select(condoMedSalePrice) %>% 
            pull() %>% 
            median() %>% 
            as.numeric()
    }

    # Return rental median price for the last year
    getRentalMedRent = function(type) {
        if (type == "None") {"No Rental Type Selected"}
        else if (type == "Studio") {
            RentalPriceInfo() %>% 
                ungroup() %>% 
                filter(as.Date(time) >= as.Date("2017-08-01")) %>% 
                select(rentalStudioMedPrice) %>% 
                pull() %>% 
                median() %>% 
                as.numeric()}
        else if (type == "1 Bedroom") {
            RentalPriceInfo() %>% 
                ungroup() %>% 
                filter(as.Date(time) >= as.Date("2017-08-01")) %>% 
                select(rentalOneBdMedPrice) %>% 
                pull() %>% 
                median() %>% 
                as.numeric()}
        else if (type == "2 Bedrooms") {
            RentalPriceInfo() %>% 
                ungroup() %>% 
                filter(as.Date(time) >= as.Date("2017-08-01")) %>% 
                select(rentalTwoBdMedPrice) %>% 
                pull() %>% 
                median() %>% 
                as.numeric()}
        else if (type == "3+ Bedrooms") {
            RentalPriceInfo() %>% 
                ungroup() %>% 
                filter(as.Date(time) >= as.Date("2017-08-01")) %>% 
                select(rentalThreeBdMedPrice) %>% 
                pull() %>% 
                median() %>% 
                as.numeric()}
    }

    ## Plot renders
    # render the neighborhood selector input
    output$neighborhoodSelect = renderUI({
        selectInput("neighborhood", "Neighborhood:",
                    choices = c("None", locationList() %>% filter(Boro == input$boro) %>% select(Area) %>% pull()),
                    selected = "None")
    })
    
    # Render the leaflet plot for showing the location of the selected neighborhood
    output$mapPlot = renderLeaflet({
        if (input$neighborhood == "None") {
            leaflet(height = "380px") %>%
                addTiles() %>%
                setView(-73.87, 40.73, zoom = 10)
        } else {
            nyc_neighborhoods[nyc_neighborhoods@data$neighborhood == input$neighborhood,] %>%
                leaflet(height = "380px") %>%
                addTiles() %>%
                setView(getLatLon(input$neighborhood)$lon, getLatLon(input$neighborhood)$lat, zoom = 13) %>%
                addPolygons(popup = ~neighborhood,
                            weight = 1,
                            fillColor = "Green", fillOpacity = 0.35)
        }
    })

    # Render the line plot for historical sale price information
    output$salePricePlot = renderPlotly({
        if (input$neighborhood == "None" | SalePriceInfo() %>% nrow() < 10) {plotly_empty()} else {
            SalePriceInfo() %>%
                plot_ly(., x = ~time, y = ~condoMedAskPrice, type = "scatter", mode = "lines+markers", name = "Condo Ask", connectgaps = TRUE) %>%
                add_trace(., x = ~time, y = ~condoMedSalePrice, mode = "lines+markers", name = "Condo Sale", connectgaps = TRUE) %>%
                layout(legend = list(x = 0, y = -0.5, orientation = "h"),
                       xaxis = list(title=""),
                       yaxis = list(title=""),
                       hovermode = "compare")}
    })

    # Render the line plot for historical rental price information
    output$rentalPricePlot = renderPlotly({
        if (input$neighborhood == "None" | RentalPriceInfo() %>% nrow() < 10) {plotly_empty()} else {
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

    getSaleTrend = function(){
        start = SalesYearlySummary() %>% select(medianSale) %>% head(1) %>% pull()
        end2017 = SalesYearlySummary() %>% select(medianSale) %>% tail(2) %>% head(1) %>% pull()
        if (end2017 > start & abs(end2017-start)>100000){
            paste0(" has seen an <b>increase</b> of <b>", 
                   round((end2017 - start)*100/start), 
                   "%</b> over the last four years. ")
        } else if (end2017 < start & abs(end2017-start)>100000){
            paste0(" has seen an <b>decline</b> of <b>", 
                   round((start - end2017)*100/start), 
                   "%</b> over the last four years. ")
        } else {
            paste0(" has been stable over the last four years. ")
        }
    }
    
    getSale2018 = function(){
        end2018 = SalePriceInfo() %>% select(condoMedSalePrice) %>% tail(1) %>% pull() %>% as.numeric()
        paste0("<h3>The median sale price for August 2018 is ", 
               paste0("$", formatC(end2018, format="f", digits=2, big.mark=",")), 
               ".</h3>")
    }
    
    # render sales summary
    output$SalesSummaryText = renderUI({
        if (input$neighborhood == "None") {
            HTML("<h3>Please select a neighborhood.</h3>")
        } else if (SalePriceInfo() %>% nrow() < 10){
            HTML("<h3>Not enough data.</h3>")
        } else {
            HTML(
                paste(paste0("<h2>", input$neighborhood, "</h2>"), 
                      paste0("<h3>", 
                             "Median sale price for properties in ", 
                             input$neighborhood, 
                             getSaleTrend(),
                             "</h3>"),  
                      getSale2018(),
                      sep="<br/>")
            )
        }
    })
    
    # render rental summary
    output$RentalSummaryText = renderUI({
        if (input$neighborhood == "None" | RentalPriceInfo() %>% nrow() < 10) {
            HTML("<h4>Not enough data.</h4>")
        } else {
            HTML(
                paste(paste0("<h4>", input$neighborhood, "</h4>"), 
                      input$neighborhood, 
                      sep="<br/>")
            )
        }
    })
    
    # render the infoBox text
    # median listing price
    output$saleAskPrice = renderText({
        paste0("$", formatC(getSaleAskPrice(), format="f", digits=2, big.mark=","))
    })
    
    # median sale price
    output$saleSalePrice = renderText({
        paste0("$", formatC(getSaleSalePrice(), format="f", digits=2, big.mark=","))
    })
    
    # median rental price
    output$rentalRent = renderText({
        paste0("$", formatC(getRentalMedRent(input$rentalType), format="f", digits=2, big.mark=","))
    })
    
    # ROI
    output$roi = renderText({
        mortgageRate = (0.0504/12)/(1-((1+0.0504/12)^(-360)))
        monthlyMortgage = input$budget*0.8*mortgageRate
        rent = getRentalMedRent(input$rentalType)
        paste(round((rent - monthlyMortgage)*100/input$budget), "%", sep = "")
    })
    
} # end server

### Run ###
shinyApp(ui, server)

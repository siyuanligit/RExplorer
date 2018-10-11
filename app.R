# Dependencies
library(shiny)
library(shinydashboard)

### UI ###
ui = dashboardPage(

  ## Dashboard Header
  dashboardHeader(title = "RExplorer",
                  dropdownMenuOutput("messageMenu")
  ), # close dashboard header

  ## Sidebar Menu
  dashboardSidebar(sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Widgets", tabName = "widgets", icon = icon("tasks"))
  )), # close sidebar menu

  ## Dashboard Body
  dashboardBody(

    ## Tab Items Pages
    tabItems(

      ## Home
      tabItem(tabName = "home",
              column(width = 2),
              column(width = 8,
                     img(src='animate.gif', height = "800px", width = "800px")),
              column(width = 2)
      ), # close home page

      ## Widgets
      tabItem(tabName = "widgets",
              column(width = 9,
                     box(width = NULL, status = "primary")),
              column(width = 3,

                     # Input Selector Box
                     box(width = NULL, status = "warning",
                         selectInput("neighborhood", "Neighborhood",
                                     choices = c("Chelsea", "Greenich")),

                         selectInput("saleType", "Type of Sales",
                                     choices = c("Condo", "Townhouse/Single Family")),

                         selectInput("rentalType", "Type of Rental",
                                     choices = c("Studio", "1 Bedroom", "2 Bedrooms", "3+ Bedrooms"))
                         ), # close input selector box

                     # Infoboxes

                     fluidRow(infoBox("Current Month Number of Listing", 200,
                                      fill = TRUE, width = NULL, col = "blue", icon = icon("list"))),

                     fluidRow(infoBox("Current Month Median Listing Price", 200,
                                      fill = TRUE, width = NULL, col = "light-blue")),

                     fluidRow(infoBox("Current Month Median Sale Price", 200,
                                      fill = TRUE, width = NULL, col = "light-blue")),

                     fluidRow(infoBox("Expected Return of Investment in 1 year", 200,
                                      fill = TRUE, width = NULL, col = "black", icon = icon("money")))
                     ) # close column
              ) # close widget page
      ) # close tab item
    ) # close body
) # end UI

### Server ###
server = function(input, output) {

} # end server

### Run ###
shinyApp(ui, server)

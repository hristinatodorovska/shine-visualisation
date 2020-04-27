## ui.R ##
library(shinydashboard)
library(DT)

# load the data
df <- read.table("bike_suburb_crashes_by_year.csv", header=TRUE, sep=",")

# vector of all the years
years <-  unique(df$Year)

# vector of all the suburbs
suburbs <-  unique(df$Suburb)

# vector of all the accident Type
accidentType <-  unique(df$ACCIDENT_TYPE)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Bike Crash Statistics")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
  sidebarMenu(
    sidebarMenu(
      menuItem("Filter", icon = icon("bar-chart-o"),
               selectInput("plotPer", "Show Accident per", choices=c("Day of Week", "Month"),selected="Month"),
               checkboxInput("filterYears", "Filter by Year", FALSE),
               conditionalPanel(condition="input.filterYears == true",
                                checkboxGroupInput("varyear", "",years)
               )
      ))))
# General Overview Row 1
frow1 <- fluidRow(
  box(DTOutput("tbl"),width = 4),
  box(DTOutput("bi"),width = 2),
  box(DTOutput("inj"),width = 2),
  box(DTOutput("fatal"),width = 2),
  box(DTOutput("alcohol"),width = 2)
)
# General Overview Row 2
frow2 <- fluidRow(
  box(plotOutput("hist"),width = 10),
  box(plotOutput("gender"),width = 2)
)

frow3 <- fluidRow(
  box(checkboxInput("allSuburbs", "All Suburbs", TRUE),
      conditionalPanel(condition="input.allSuburbs == false",
                       selectInput("filtersuburb", "Filter by Suburb",choices =  as.vector(suburbs), multiple=TRUE)
      ),
      checkboxInput("accidentType", "Filter by Accident Type", FALSE),
      conditionalPanel(condition="input.accidentType == true",
                       checkboxGroupInput("filteraccident", "Accident Type",accidentType)),
      width =2),
  box(leafletOutput("myMap",height = 550), width =10)
)

# combine the three fluid rows to make the body
body <- dashboardBody(tags$h3("Bike Crashes: July 2013 - March 2019"),
                      tabsetPanel(
                        tabPanel("General Overview", frow1, frow2),
                        tabPanel("Map", frow3)
                      )
)


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Bike Crash Statistics1', header, sidebar, body, skin='green')


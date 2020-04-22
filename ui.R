## ui.R ##
library(shinydashboard)
library(DT)

df <- read.table("bike_suburb_crashes_by_year.csv", header=TRUE, sep=",")
years <-  unique(df$Year)

ui <- dashboardPage(skin="green",
        dashboardHeader(title = "Bike Crash Statistics",titleWidth = 280),
        dashboardSidebar(),
        dashboardBody(tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "style.css")),
                      
        # Boxes need to be put in a row (or column)
        fluidRow(
          box(DTOutput("tbl")),
          box(DTOutput("bi")),
          box(plotOutput("tableAll", height = 250)),
          box(title = "Controls", sliderInput("slider", "Number of observations:", 1, 100, 50)),
          checkboxGroupInput("year", "Year:",years)
    )
  )
)




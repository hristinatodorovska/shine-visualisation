library(shinydashboard)
library(DT)
library(dplyr)

# load the data
df <- read.table("bike_suburb_crashes_by_year.csv", header=TRUE, sep=",")

# vector of all the years
years <-  unique(df$Year)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Bike Crash Statistics")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
            sidebarMenu(
             menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
             sidebarMenu(
               menuItem("Filter", icon = icon("bar-chart-o"),
               checkboxGroupInput("varyear", "Filter by Year:",years)#,
               #conditionalPanel(condition="input.year == false",
               #selectInput("year", "Filter by Year",choices =  as.vector(years), multiple=TRUE))
      ))))

frow1 <- fluidRow(
  
  box(DTOutput("tbl")),
  box(DTOutput("bi"),width = 2),
  box(DTOutput("inj"),width = 2),
  box(DTOutput("fatal"),width = 2)
)

frow2 <- fluidRow(
  box(plotOutput("hist"),width = 12)
 # box(title = "Controls", sliderInput("slider", "Number of observations:", 1, 100, 50)),
  #checkboxGroupInput("year", "Year:",years)
)

# combine the three fluid rows to make the body
body <- dashboardBody(frow1, frow2)


#completing the ui part with dashboardPage
ui <- dashboardPage(title = 'Bike Crash Statistics1', header, sidebar, body, skin='green')

server <- function(input, output,session) {
  
  output$tableAll <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  #Table: Total Crashes By Year
  output$tbl <- renderDT({
    # filter year is selected
    if (!is.null(input$varyear)) {
      df <- df[df$Year %in% input$varyear,]
    }
    totalData <- as.data.frame(t(as.matrix(data.frame(table(df$Year)))))
    },
    rownames = FALSE, 
    caption = htmltools::tags$caption(
      style = 'text-align: center;color:black; font-weight:bold;','Total Bike Crashes by Year'),
    options = list(ordering=FALSE, dom = 't',
                   headerCallback = JS("function(thead, data, start, end, display){",
                          "$(thead).remove();$('table.dataTable.no-footer').css('border-bottom', 'none');",
                          "}"))
    )
  
  #Table: Cyclist Involved
  output$bi <- renderDT({
    # filter year is selected 
    if (!is.null(input$varyear)) {
      df <- df[df$Year %in% input$varyear,]
    }
    # total BICYCLIST
    bike <- sum(df$BICYCLIST)
    # total Not BICYCLIST
    noBike <- sum(df$TOTAL_PERSONS) - bike
    tmpBike <- c(bike, noBike)
    biData <- data.frame(tmpBike)
    },
    rownames = c("Yes", "No"), 
    caption = htmltools::tags$caption(
      style = 'text-align: center;color:black; font-weight:bold;','Cyclist Involved'),
    options = list(ordering=FALSE, dom = 't',
                   headerCallback = JS("function(thead, data, start, end, display){",
                                       "$(thead).remove();",
                                        "}"))
    )
  
  #Table: Injuries Caused by Crash
  output$inj <- renderDT({
    # filter year is selected 
    if (!is.null(input$varyear)) {
      df <- df[df$Year %in% input$varyear,]
    }
    # total INJ_OR_FATAL
    inj <- sum(df$INJ_OR_FATAL)
    # total NONINJURED
    nonInj <- sum(df$NONINJURED)
    tmpInj <- c(inj, nonInj)
    injData <- data.frame(tmpInj)
  },
  rownames = c("Yes", "No"), 
  caption = htmltools::tags$caption(
    style = 'text-align: center;color:black; font-weight:bold;','Injuries Caused by Crash'),
  options = list(ordering=FALSE, dom = 't',
                 headerCallback = JS("function(thead, data, start, end, display){",
                                     "$(thead).remove();",
                                     "}"))
  )
  
  #Table: Fatal Accident
  output$fatal <- renderDT({
    # filter year is selected 
    if (!is.null(input$varyear)) {
      df <- df[df$Year %in% input$varyear,]
      
    }
    fatalDf <- df[df$FATALITY>0,]
    # fatal accident
    fatal = nrow(fatalDf)
    # no fatal accident
    nonFatal <- nrow(df) - fatal
    tmpFatal <- c(fatal, nonFatal)
    fatalData <- data.frame(tmpFatal)
  },
  rownames = c("Yes", "No"), 
  caption = htmltools::tags$caption(
    style = 'text-align: center;color:black; font-weight:bold;','Fatal Accident'),
  options = list(ordering=FALSE, dom = 't',
                 headerCallback = JS("function(thead, data, start, end, display){",
                                     "$(thead).remove();",
                                     "}"))
  )

  # Barchar: Number of Accident per Month
  output$hist <- renderPlot({
    # filter year is selected
    if (!is.null(input$varyear)) {
      df <- df[df$Year %in% input$varyear,]
      
    }
    data <- df %>% group_by(Year, Month) %>% summarise(Freq=n())
    data <- data[order(data$Year, data$Month),]
    
    barplot(data$Freq, names.arg = paste( month.abb[data$Month],data$Year, sep=" "),
            las=2, col = "green", main="Crashes by Month",ylim=c(0,200)
            )

  })
  
}


shinyApp(ui, server)

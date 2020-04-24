library(shinydashboard)
library(DT)
library(dplyr)
library(leaflet)
library(ggplot2)

# load the data
df <- read.table("bike_suburb_crashes_by_year.csv", header=TRUE, sep=",")

# vector of all the years
years <-  unique(df$Year)

# vector of all the suburbs
suburbs <-  unique(df$Suburb)

#Dashboard header carrying the title of the dashboard
header <- dashboardHeader(title = "Bike Crash Statistics")

#Sidebar content of the dashboard
sidebar <- dashboardSidebar(
            sidebarMenu(
             #menuItem("", tabName = "dashboard", icon = icon("dashboard")),
             sidebarMenu(
               menuItem("Filter", icon = icon("bar-chart-o"),
               selectInput("plotPer", "Show Accident per", choices=c("Day of Week", "Month"),selected="Month"),
               checkboxInput("filterYears", "Filter by Year", FALSE),
               conditionalPanel(condition="input.filterYears == true",
                                checkboxGroupInput("varyear", "",years)
               )
      ))))

frow1 <- fluidRow(
  
  box(DTOutput("tbl"),width = 4),
  box(DTOutput("bi"),width = 2),
  box(DTOutput("inj"),width = 2),
  box(DTOutput("fatal"),width = 2),
  box(DTOutput("alcohol"),width = 2)
)

frow2 <- fluidRow(
  box(plotOutput("hist"),width = 10),
  box(plotOutput("gender"),width = 2)
 # box(title = "Controls", sliderInput("slider", "Number of observations:", 1, 100, 50)),
  #checkboxGroupInput("year", "Year:",years)
)

frow3 <- fluidRow(
  box(leafletOutput("myMap",height = 550), width =10),
  box(checkboxInput("allSuburbs", "All Suburbs", TRUE),
           conditionalPanel(condition="input.allSuburbs == false",
                            selectInput("filtersuburb", "Filter by Suburb",
                                        choices =  as.vector(suburbs), multiple=TRUE)),width =2)
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
    if(input$filterYears) {
      if (!is.null(input$varyear)) {
        df <- df[df$Year %in% input$varyear,]
      }
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
      style = 'text-align: center;color:black; font-weight:bold;','Bycycle Involved'),
    options = list(ordering=FALSE, dom = 't',
                   headerCallback = JS("function(thead, data, start, end, display){",
                                       "$(thead).remove();",
                                        "}"))
    )
  
  #Table: Injuries Caused by Crash
  output$inj <- renderDT({
    # filter year is selected 
    if(input$filterYears) {
      if (!is.null(input$varyear)) {
        df <- df[df$Year %in% input$varyear,]
      }
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
    if(input$filterYears) {
      if (!is.null(input$varyear)) {
        df <- df[df$Year %in% input$varyear,]
      }
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

  ##Table: ALCOHOL Time Accident
  output$alcohol <- renderDT({
    # filter year is selected 
    if(input$filterYears) {
      if (!is.null(input$varyear)) {
        df <- df[df$Year %in% input$varyear,]
      }
    }
    
    df <- df %>% group_by(ALCOHOLTIME) %>% summarise(Freq=n())
    df <- df[c(2,1),]
    alcholicData <- data.frame(df)
  },
  rownames =FALSE, 
  caption = htmltools::tags$caption(
    style = 'text-align: center;color:black; font-weight:bold;','Alcohol Times'),
  options = list(ordering=FALSE, dom = 't',
                 headerCallback = JS("function(thead, data, start, end, display){",
                                     "$(thead).remove();",
                                     "}"))
  )
  
  # Barchar: Number of Accident per Month/Week
  output$hist <- renderPlot({
    # filter year is selected
    if(input$filterYears) {
      if (!is.null(input$varyear)) {
        df <- df[df$Year %in% input$varyear,]
      }
    }
    if(input$plotPer == 'Month'){
      data <- df %>% group_by(Year, Month) %>% summarise(Freq=n())
      data <- data[order(data$Year, data$Month),]
      
      barplot(data$Freq, names.arg = paste( month.abb[data$Month],data$Year, sep=" "),
              las=2, col = "#00A65A", main="Crashes by Month",ylab="Number of Crashes",ylim=c(0,200)
      )
    }
    else {
      weekDays <- c(1,2,3,4,5,6,7,8)
      names(weekDays) <- c("Monday", "Tuesday", "Wednesday", "Thursday","Friday", "Saturday", "Sunday", "Unknown")
      
      data <- df %>% group_by(DAY_OF_WEEK) %>% summarise(Freq=n())
      data <- data.frame(data)
      for (row in 1:nrow(data)) {
        if(data[row,"DAY_OF_WEEK"] == ""){
          data[row,"day_number"] <- 8
        }
        else{
          day = as.character(data[row,"DAY_OF_WEEK"])
          data[row,"day_number"] <- weekDays[[day]]
        }
      }
      
      levels(data$DAY_OF_WEEK)[levels(data$DAY_OF_WEEK)==""] <- "Unknown"
      data <- data[order(data$day_number),]
      barplot(data$Freq, names.arg = data$DAY_OF_WEEK,
              las=2, col = "#00A65A", main="Crashes by Day of Week",ylab="Number of Crashes"
      )
    }
  })
  
  # Barchar: Number of People Involved in Accident per Gender
  output$gender <- renderPlot({
    # filter year is selected
    if(input$filterYears) {
      if (!is.null(input$varyear)) {
        df <- df[df$Year %in% input$varyear,]
      }
    }
    male <- sum(df$MALES)
    femaile <- sum(df$FEMALES)
    freq <- c(male, femaile)
    gender <- c("Male", "Female")
    genderDf = data.frame(gender,freq)
    
    ggplot(data=genderDf,aes(x=gender, y=freq, fill=gender))+geom_bar(stat="identity",show.legend = FALSE)+ylab("People Involved")+xlab("Gender")
    
  })
  
  #MAp
  output$myMap <- renderLeaflet({
    tag.map.title <- tags$style(HTML("
                  .leaflet-control.map-title { 
                     transform: translate(-50%,20%);
                     position: fixed !important;
                     left: 50%;
                     text-align: center;
                     padding-left: 10px; 
                     padding-right: 10px; 
                     background: rgba(255,255,255,0.75);
                     font-weight: bold;
                     font-size: 28px;
                    }
                       "))
    title <- tags$div(
      tag.map.title, HTML("Accident Map")
    )
    # filter year is selected 
    if(input$filterYears) {
      if (!is.null(input$varyear)) {
        df <- df[df$Year %in% input$varyear,]
      }
    }
    
    if (input$allSuburbs) {
      selected <- df[df$Suburb %in% suburbs,]
      
    }
    else if (!is.null(input$filtersuburb)) {
      selected <- df[df$Suburb %in% input$filtersuburb,]
      
    }else {
      selected <- df[df$Suburb %in% suburbs,]
    }
      leaflet(data = selected) %>%  
        addTiles() %>%
        addCircles(~LONGITUDE,
                   ~LATITUDE
        ) %>%
        addControl(title, position = "bottomleft")
      })
}

shinyApp(ui, server)

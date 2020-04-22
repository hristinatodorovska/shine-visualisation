library(DT)

df <- read.table("bike_suburb_crashes_by_year.csv", header=TRUE, sep=",")

# DataFrame to Show by Year
dateByYear <- data.frame(table(df$Year))
dateByYear <- as.data.frame(t(as.matrix(dateByYear)))


# DataFrame to Show by Year
cyclist <- sum(df$BICYCLIST)
#cyclist <- as.data.frame(t(as.matrix(BICYCLIST)))
years <-  unique(df$Year)

server <- function(input, output) {
  set.seed(122)
  histdata <- rnorm(500)
  
  output$tableAll <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  output$plot2 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })
  
  #Table: Total Crashes By Year
  output$tbl = renderDT(dateByYear,rownames = FALSE,colnames = rep("", ncol(dateByYear)), 
                        caption = htmltools::tags$caption(
                          style = 'text-align: center;color:black; font-weight:bold;',
                          'Total Bike Crashes by Year'),
                        options = list( ordering=FALSE, dom = 't')
  )
  
  #Table: Bycyclist Involved
  output$bi = renderDT(dateByYear,rownames = FALSE,colnames = rep("", ncol(dateByYear)), 
                       caption = htmltools::tags$caption(
                         style = 'text-align: center;color:black; font-weight:bold;',
                         'Cyclist Involved'),
                       options = list( ordering=FALSE, dom = 't')
  )
  
}
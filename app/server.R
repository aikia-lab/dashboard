#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DBI)

connect_to_DB <- function(ip = "217.91.79.198", mydb){
  
  Checkmydb <- tryCatch(DBI::dbIsValid(mydb),
                        error=function(e) e)
  if(inherits(Checkmydb, "simpleError")){
    
    mydb <- DBI::dbConnect(RMySQL::MySQL(), user='ceilert', password='ceilert', dbname='monkey', 
                           host = ip, port= 3306, encoding = "utf8")
  }
}


# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$distPlot <- renderPlot({

        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
        

        mydb <- connect_to_DB()
        
        DBI::dbListTables(mydb)
# Financial Sector Plots --------------------------------------------------

        

    })

})

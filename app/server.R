

library(shiny)
library(magrittr)
library(DBI)




# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  

  # Temprorary Initial load of data Frames
 
  mydb <- connect_to_DB()
  
  index_mapping <- DBI::dbGetQuery(conn = mydb,
                                   "SELECT ticker_yh,
                                   supersector,
                                   country,
                                   name
                                   FROM fin_index_meta_data") %>% 
    dplyr::mutate(name = stringr::str_trim(name),
                  name = stringr::str_remove(name, 
                                             "STOXX Europe 600 |
                                             Dow Jones US |
                                             Dow Jones US Total Market |
                                             Dow Jones U.S. ")) %>% 
    dplyr::mutate(name = stringr::str_remove(name, "Total Market "))
  
  vola_history <- DBI::dbGetQuery(conn = mydb,
                                  stringr::str_c(
                                    "SELECT *
                                  FROM v_fin_index_expanded")) %>% 
    dplyr::mutate(date = lubridate::as_date(date))


  DBI::dbDisconnect(mydb)
  
  write_counter_to_sql()
  

# Resizing aikia logo -----------------------------------------------------
  
  # reactive value 4 sidebar collapsing
  vals<-reactiveValues()
  vals$collapsed=FALSE
  observeEvent(input$SideBar_col_react,{
    vals$collapsed=!vals$collapsed
  })
  
  # reactive logo   
  size <- reactive({
    if(vals$collapsed){
      return("50px") 
    } else {
      return("120px")
    }
  }) 
  

  
  output$picture <- shiny::renderImage({
    
    return(list(src = "C:/Users/admin/Documents/dashboard/app/www/aikia_logo.svg", 
                contentType = "image/svg+xml", 
                width = size(),
                height = size(),
                alt = "Analytics")) #style = 'position: absolute; left: 50%; transform: translateX(-50%);'
    
  },
  deleteFile = FALSE) 
  
  
  
# Financial Sector Plots --------------------------------------------------
  sector_vola <- shiny::reactiveVal(NULL)
  
  shiny::observe({
    
    sector_vola_plotly <- sector_vola_plotly_fun(input$val_date,
                                                 input$index_location, 
                                                 index_mapping = index_mapping, 
                                                 vola_history = vola_history)
    plotly::event_register(sector_vola_plotly, "plotly_click")
    
    sector_vola(sector_vola_plotly)
  })
  
  output$sector_vola <- plotly::renderPlotly(expr = {
    sector_vola()
    
  })
  
  output$sector_line <- plotly::renderPlotly({
    click_data <- plotly::event_data("plotly_click") %>%
      dplyr::as_tibble()

    if(nrow(click_data) != 0){
  
      index <- plotly_click_mapping %>%
        dplyr::filter(curve == click_data[["curveNumber"]],
                      y == click_data[["y"]]) %>%
        dplyr::pull(ticker_yh)
      
      sector_line_chart_fun(index_id = index,
                        date = input$val_date,
                        index_mapping = index_mapping, 
                        vola_history = vola_history)
    } else {
     
      validate(
          need(nrow(click_data) != 0, 
                "                  Click on Heatmap Cells for Index Price Chart")
      )
      
    }

  })
  

# FED Funds Rate ----------------------------------------------------------
  fed_rates <- shiny::reactiveVal(NULL)
  meeting_date <- shiny::reactiveVal(NULL)
  
  # Reactive Value for Fed Funds Curve
  shiny::observe({
    fed_rates_plotly <- get_fed_rates_fun(input$fed_date_1,
                                         input$fed_date_2)
    plotly::event_register(fed_rates_plotly, "plotly_click")
    
    fed_rates(fed_rates_plotly)
  })
  
  # Reactive Value for Meeting Date Variables
  shiny::observe({
    meeting_plotly <- meeting_data(input$meeting_d)
    plotly::event_register(meeting_plotly, "plotly_click")
    
    meeting_date(meeting_plotly)
  })
  
  # Output for Fed Funds Curve
  output$fed_rates <- plotly::renderPlotly({
    fed_rates()
  })
  
  # Output for Meeting Date variables
  output$meeting_date <- plotly::renderPlotly({
    meeting_date()
  })
  
  
})

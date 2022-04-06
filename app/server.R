

library(shiny)
library(magrittr)
library(DBI)




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  
  ###############################                               
  #       Update BODY-TABS      #
  ###############################
  
#  observeEvent(input$sidebarItemExpanded, {
#    if(input$sidebarItemExpanded=="sector_volas"){
#        updateTabItems(session,"mysidebar","sector_volas")
#        index_mapping <<- get_index_meta()
#    }
#    else if (input$sidebarItemExpanded=="index_entropy")
#      updateTabItems(session,"mysidebar","index_entropys")
#    else if (input$sidebarItemExpanded=="score")
#      updateTabItems(session,"mysidebar","scores")
#    else if (input$sidebarItemExpanded=="idea")
#      updateTabItems(session,"mysidebar","ideas")
#    else if (input$sidebarItemExpanded=="manage")
#      updateTabItems(session,"mysidebar","manages")
#  })
  
  
  
  # Temprorary Initial load of data Frames
 
  mydb <- connect_to_DB()
  
  index_mapping <- DBI::dbGetQuery(conn = mydb,
                                   "SELECT ticker_yh,
                                   supersector,
                                   country,
                                   name
                                   FROM fin_index_meta_data") %>% 
    dplyr::mutate(name = stringr::str_trim(name),
                  name = stringr::str_remove(name, "STOXX Europe 600 |Dow Jones US |Dow Jones US Total Market |Dow Jones U.S. ")) %>% 
    dplyr::mutate(name = stringr::str_remove(name, "Total Market "))
  
  vola_history <- DBI::dbGetQuery(conn = mydb,
                                  stringr::str_c("SELECT *
                                  FROM v_fin_index_expanded")) %>% 
    dplyr::mutate(date = lubridate::as_date(date))
  
  DBI::dbDisconnect(mydb)
  
  
  # Financial Sector Plots --------------------------------------------------
  sector_vola <- shiny::reactiveVal(NULL)
  
  shiny::observe({
    
    sector_vola_plotly <- sector_vola_plotly_fun(input$val_date, input$index_location, index_mapping = index_mapping, vola_history = vola_history)
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
          need( nrow(click_data) != 0, "                  Click on Heatmap Cells for Index Price Chart")
      )
      
    }
    
    # LOAD idx_price_history_global in BACKGROPUND
  })
  
  
  
})

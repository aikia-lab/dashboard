

library(shiny)
library(shinyBS)
library(magrittr)
library(DBI)



# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {

  # Update to last available date
  shiny::updateDateInput(
          session, 
          "val_date",
          value = lubridate::as_date(
                    bizdays::offset(lubridate::today(),
                            -1,
                            'UnitedStates/NYSE'))
  )
  
  shiny::updateDateInput(
    session, 
    "fed_date_1",
    value = lubridate::as_date(
      bizdays::offset(lubridate::today(),
                      -1,
                      'UnitedStates/NYSE'))
  )
  
  shiny::updateDateInput(
    session, 
    "fed_date_2",
    value = lubridate::as_date(
      bizdays::offset(lubridate::today(),
                      -2,
                      'UnitedStates/NYSE'))
  )
  #----------------------------------------------------------
  
  
  
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
  
  ticker_mapping <- DBI::dbGetQuery(conn = mydb,
                                   "SELECT 
                                    ticker_yh,
                                    name ,
                                    industry_group,
                                    issuer_industry,
                                    bics_level_1_sector_name,
                                    bics_level_2_industry_group_name
                                   FROM fin_ticker_meta_data") %>% 
    dplyr::rename(BIC_1 = bics_level_1_sector_name,
                  BIC_2 = bics_level_2_industry_group_name)
  
  vola_history <- DBI::dbGetQuery(conn = mydb,
                                  stringr::str_c(
                                    "SELECT *
                                  FROM v_fin_index_expanded")) %>% 
    dplyr::mutate(date = lubridate::as_date(date)) %>% 
    dplyr::distinct(ticker_yh, date, .keep_all = T)
  
  
  # Get IDX TIC Relation
  idx_tic_relation <- DBI::dbGetQuery(mydb, "SELECT * 
                                           FROM fin_ticker_index_relation") %>% # get ISINs for indices 
    dplyr::as_tibble()
  
  tic_meta <- DBI::dbGetQuery(mydb, "SELECT isin, ticker_yh 
                                   FROM fin_ticker_meta_data") %>%  # get ticker_yh for ISINs
    dplyr::as_tibble()
  
  idx_tic_rel <<- dplyr::left_join(idx_tic_relation, tic_meta, by = "isin") %>% # get ticker_yh for indices
    dplyr::as_tibble()
  #  --- --- --- --- --- --- --- --- ---
  
  
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
      return("120px") 
    } else {
      return("50px")
    }
  }) 
  

  
  output$picture <- shiny::renderImage({
    
    return(list(src = "www/aikia_logo.svg", 
                contentType = "image/svg+xml", 
                width = size(),
                height = size(),
                alt = "Analytics")) #style = 'position: absolute; left: 50%; transform: translateX(-50%);'
    
  },
  deleteFile = FALSE) 
  
  
  
# Financial Sector Plots --------------------------------------------------
  
  # TAB VOLA PLOTS
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
  
  
  # TAB RETURN PLOTS
  sector_returns <- shiny::reactiveVal(NULL)
  
  shiny::observe({
    
    sector_returns_ggplot <- sector_return_plot_fun(input$val_date,
                                                 input$index_location, 
                                                 index_mapping = index_mapping, 
                                                 idx_history = vola_history)
    
    sector_returns(sector_returns_ggplot)
  })
  

  output$dtd_ret <- shiny::renderPlot({
    sector_returns()$day_plot
  })
  output$wtd_ret <- shiny::renderPlot({
    sector_returns()$week_plot
  })
  output$qtd_ret <- shiny::renderPlot({
    sector_returns()$quarter_plot
  })
  output$ytd_ret <- shiny::renderPlot({
    sector_returns()$year_plot
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
  
  
  
# Index Entropy ----------------------------------------------------------
  get_idx_entropy <- shiny::reactiveVal(NULL)
  
  # Reactive Value for Fed Funds Curve
  shiny::observe({
    entropy_pnl_plotly <- entropy_pnl_fun(input$val_date,
                                              input$choose_idx)
    plotly::event_register(entropy_pnl_plotly, "plotly_click")
    
    if(exists("click_data")){rm(click_data)}
    
    get_idx_entropy(entropy_pnl_plotly)
  })
  

  
  
  # Output for Index Timeline
  output$idx_entrop <- plotly::renderPlotly({
    get_idx_entropy()
  })
  
  
  # Output Correlation Network
  output$date_entropy <- plotly::renderPlotly({
    
    click_data <<- plotly::event_data("plotly_click") %>%
      dplyr::as_tibble()
    
    if(nrow(click_data) != 0){
      
      new_date <- click_data %>% 
        dplyr::filter(y == click_data[["y"]]) %>%
        dplyr::pull(x)
    
      entrop_tic_group_fun(start_date = new_date, 
                           cur_idx = input$choose_idx, 
                           corr_th = input$choose_entropy_th,
                           sector_info = ticker_mapping,
                           grouping = input$choose_grouping)
    } else {
      
      validate(
        need(nrow(click_data) != 0, 
             "                  Click on History Plot for Entropy Calculation")
      )
      
    }
    
  })
  

# actual vs forecasts -----------------------------------------------------

  eco_plot <- reactive({
    req(input$choose_eco_country)
    eco_fc_fun(country_id = input$choose_eco_country,
               type_id = input$choose_eco_type)
    
  })
  
  output$eco_forecasts <- plotly::renderPlotly({
    
    validate(
        need(!is.null(eco_plot()), 
               "                  No data for this Country and Economic Type")
    )
    
    eco_plot()
    
  })  

  
})





















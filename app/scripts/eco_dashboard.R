





eco_dashboard_fun <- function(ctry){
  
  

  
  
  library(magrittr)
  library(gt)
  
  dash_fcts <- tibble::tribble(
    ~fcts, ~group,  
    "Government Bond 10Y", "Market",
    "Consumer Price Index CPI", "Inflationary",
    "Inflation Rate", "Inflationary",
    "Core Inflation Rate", "Inflationary",
    "Producer Prices", "Inflationary",
    "GDP Annual Growth Rate", "Productivity",
    "Current Account to GDP", "Productivity",
    "Initial Jobless Claims", "Workforce",
    "Continuing Jobless Claims", "Workforce",
    "Job Vacancies", "Workforce",
    "Job Offers", "Workforce",
    "Services PMI", "Confidence",
    "Manufacturing PMI","Confidence",
    "Business Confidence","Confidence",
    "Business	Inventories", "Productivity",
    "Changes in Inventories", "Productivity",
    "Bankruptcies","Productivity",
    "Labour Costs","Workforce",
    "Productivity","Productivity",
    "Balance of Trade","Productivity",
    "Imports","Productivity",
    "Exports","Productivity")
  
  
  

  # latest forecasts  
  test <- dash_fcts %>%
    dplyr::select(fcts) %>% 
    dplyr::nest_by(fcts) %>% 
    dplyr::mutate(plot = list(get_forecast_curves(fcts,country=ctry))) %>% 
    dplyr::select(-data)
  
  
  
  #get all dots
  gt_act_table <- 
    lapply(test$plot, function(l) l[[1]]) %>% 
    tibble::tibble() %>% 
    tidyr::unnest(cols = c(.))
  
  # save latest update
  latest_update <- gt_act_table %>% dplyr::group_by(type) %>% dplyr::summarise(latest = max(period_date),.groups = "drop")
  
  
  # list for forecast plot
  fcts_prep <- 
    lapply(test$plot, function(l) l[[2]]) %>% 
    tibble::tibble() %>% 
    tidyr::unnest(cols = c(.)) %>%
    dplyr::group_by(type) %>%
    # must end up with list of data for each row in the input dataframe
    dplyr::summarize(fcts_dots = list(dots), .groups = "drop") 
  
  
  table_prep <- gt_act_table %>%
    dplyr::select(-period_date) %>% 
    dplyr::arrange(desc(latest_period)) %>% 
    tidyr::pivot_wider(names_from = latest_period, values_from = dots) %>%
    dplyr::left_join(fcts_prep, by = "type") %>%
    dplyr::left_join(latest_update, by ="type") %>% 
    dplyr::relocate(latest,.before = 'fcts_dots')
  
  
  all <- table_prep %>% 
    dplyr::left_join(dash_fcts,by=c("type"="fcts")) %>% 
    dplyr::rename(Indicator = type) %>% 
    
    dplyr::arrange(match(Indicator, c('Government Bond 10Y',      #  1  bad     
                                      'Consumer Price Index CPI', #  2  bad     
                                      'Inflation Rate',           #  3  bad
                                      'Core Inflation Rate',      #  4  bad
                                      'Producer Prices',          #  5  bad
                                      'GDP Annual Growth Rate',   #  6    good
                                      'Current Account to GDP',   #  7    good
                                      'Initial Jobless Claims',   #  8  bad
                                      'Continuing Jobless Claims',#  9  bad
                                      'Job Vacancies',            # 10    good
                                      'Job Offers',               # 11    good
                                      'Services PMI',             # 12    good
                                      'Manufacturing PMI',        # 13    good
                                      'Business Confidence',      # 14    good
                                      'Business\tInventories',    # 15  bad
                                      'Changes in Inventories',   # 16  bad
                                      'Bankruptcies',             # 17  bad
                                      'Labour Costs',             # 18  bad
                                      'Productivity',             # 19    good
                                      'Balance of Trade',         # 20    good
                                      'Imports',                  # 21  bad
                                      'Exports'                   # 22    good
    ))) %>%  
    
    dplyr::group_by(group) %>% # .[,1] %>% print(n = 21)
    # View()
    gt::gt() %>%
    gt::tab_spanner(label = "last 10 historical Periods",
                    columns = c(2:11)) %>% 
    
    # color rows where high is BAD
    gt::data_color(columns = 2:11,
                   rows = c(1,2,3,4,5,8,9,15,16,17,18,21),
                   direction = "row",
                   method = "numeric",
                   palette = c("#0F7B7B","#61B7B7","grey","#CC9719","#CC6A19")
    ) %>%
    
    # color rows where high is GOOD
    gt::data_color(columns = 2:11,
                   rows = c(6,7,10,11,12,13,14,19,20,22),
                   direction = "row",
                   method = "numeric",
                   palette = c("#CC6A19","#CC9719","grey","#61B7B7","#0F7B7B")
    ) %>% 
    
    # format numbers big figures
    gt::fmt_number(columns = 2:11,
                   rows = c(2,5,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22),
                   decimals = 1) %>% 
    
    # format numbers percent figures
    gt::fmt_percent(columns = 2:11,
                    rows = c(1,3,4,6,7),
                    scale_values = FALSE,
                    decimals = 2) %>% 
    
    # rendering the spark lines
    gtExtras::gt_plt_sparkline(fcts_dots, fig_dim = c(5,30), same_limit = F) %>% 
    
    gt::tab_style(style = gt::cell_fill(color = "#729C69"),
                  locations = gt::cells_body(columns = "latest", 
                                             rows = (latest > lubridate::floor_date(lubridate::today(), 'month')))) %>% 
    
    gt::cols_label(
      fcts_dots = "Forecast",
      latest = gt::md("latest<br>Update")
    ) %>% 
    
    aikia::gt_theme_aikia() %>% 
    gt::tab_options(row_group.font.size = "16px",
                    row_group.font.weight = "bold") 
  
  

  return(all)
}











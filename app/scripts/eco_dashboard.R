





eco_dashboard_fun <- function(){
  
  
  
print("start") 
  
  
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
  
  
  
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  #
  #
  #
  
  # !!! FORECAST START MUST BE AFTER LAST DOTS !!!!!!!!!!!!!!!!!!
  
  #
  #
  #
  # !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  
  # latest forecasts  
  test <- dash_fcts %>%
    dplyr::select(fcts) %>% 
    dplyr::nest_by(fcts) %>% 
    dplyr::mutate(plot = list(get_forecast_curves(fcts))) %>% 
    dplyr::select(-data)
  
  
  #get all dots
  gt_act_table <- 
    lapply(test$plot, function(l) l[[1]]) %>% 
    tibble::tibble() %>% 
    tidyr::unnest(cols = c(.))
  
  fcts_prep <- 
    lapply(test$plot, function(l) l[[2]]) %>% 
    tibble::tibble() %>% 
    tidyr::unnest(cols = c(.))
  
  
  
  # df of plots for each country
  plots <- fcts_prep %>%
    dplyr::group_by(type) %>%
    tidyr::nest() %>%
    dplyr::mutate(
      spark = purrr::map2(type, data, plot_spark))
  
  
  
  table_prep <- gt_act_table %>%
    dplyr::arrange(desc(latest_period)) %>% 
    tidyr::pivot_wider(names_from = latest_period, values_from = dots) %>%
    dplyr::inner_join(plots, by = "type") %>%
    dplyr::mutate(ggplot1 = NA)
  
  
  all <- table_prep %>% 
    dplyr::left_join(dash_fcts,by=c("type"="fcts")) %>% 
    dplyr::rename(Indicator = type) %>% 
    
    dplyr::arrange(match(Indicator, c('Government Bond 10Y','Consumer Price Index CPI','Inflation Rate','Core Inflation Rate','Producer Prices','GDP Annual Growth Rate','Current Account to GDP','Initial Jobless Claims','Continuing Jobless Claims','Job Vacancies','Job Offers','Services PMI','Manufacturing PMI','Business Confidence','Business\tInventories','Changes in Inventories','Bankruptcies','Labour Costs','Productivity','Balance of Trade','Imports','Exports'))) %>% 
    
    dplyr::group_by(group) %>% # .[,1] %>% print(n = 21)
    gt::gt() %>%
    gt::cols_label(
      ggplot1 = "Forecast"
    ) %>%
    gt::tab_spanner(label = "last 10 historical Periods",
                    columns = c(2:11)) %>% 
    
    # color rows where high is BAD
    gt::data_color(columns = 2:11,
                   rows = c(1,3,4,5,8,15,16,17,20),
                   direction = "row",
                   method = "numeric",
                   palette = c("#0F7B7B","#61B7B7","grey","#CC9719","#CC6A19")
    ) %>%
    
    # color rows where high is GOOD
    gt::data_color(columns = 2:11,
                   rows = c(2,6,7,9,10,11,12,13,14,18,19,21),
                   direction = "row",
                   method = "numeric",
                   palette = c("#CC6A19","#CC9719","grey","#61B7B7","#0F7B7B")
    ) %>% 
    
    # format numbers big figures
    gt::fmt_number(columns = 2:11,
                   rows = c(2,5,8,9,10,11,12,13,14,16,17,18,19,20,21),
                   decimals = 1) %>% 
    
    # format numbers percent figures
    gt::fmt_percent(columns = 2:11,
                    rows = c(1,3,4,6,7,15),
                    scale_values = FALSE,
                    decimals = 2) %>% 
    
    # rendering the spark lines
    gt::text_transform(
      locations = gt::cells_body(columns = c(ggplot1)),
      fn = function(x) {
        purrr::map(table_prep$spark, ggplot_image, height = px(30), aspect_ratio = 6)
      }
    ) %>%
    
    # bessere sparkline funktion !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    # https://jthomasmock.github.io/gtExtras/  
    #  gtExtras::gt_plt_sparkline(mpg_data) %>%
    
    
    aikia::gt_theme_aikia() %>% 
    gt::tab_options(row_group.font.size = "16px",
                    row_group.font.weight = "bold") %>% 
    gt::cols_hide(c(data,spark)) 
  
print("all")  
  return(all)
}











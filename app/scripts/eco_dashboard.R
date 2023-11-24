



# ctry <- 'united-states'

eco_dashboard_fun <- function(ctry){
  
  

  
  
  library(magrittr)
  library(gt)
  
  dash_fcts <- tibble::tribble(
    ~fcts, ~group,  
    "Government Bond 10Y", "Market",
    
    "Currency", "Market",
    
 #   "Consumer Price Index CPI", "Inflationary",
    "Inflation Rate", "Inflationary",
    "Core Inflation Rate", "Inflationary",
    "Producer Prices Change", "Inflationary", # YoY
    "Core Producer Prices YoY", "Inflationary",
    "CPI seasonally adjusted", "Inflationary",
    "GDP Annual Growth Rate", "Productivity",
    "Current Account to GDP", "Productivity",
    "Initial Jobless Claims", "Workforce",
    "Continuing Jobless Claims", "Workforce",

        
# NEU !!!!!!!!!!!!!!!!!!!!!!!!!    

    

    "Non Farm Payrolls", "Workforce",
    "Unemployment Rate", "Workforce",
    "Average Hourly Earnings YoY", "Workforce",
    "Average Weekly Hours", "Workforce",
    "Wage Growth", "Workforce",
    "Industrial Production", "Productivity",

    "Factory Orders", "Productivity",

    "Private Sector Credit", "Consumption",
    "Consumer Credit","Consumption",
    "Bank Lending Rate","Consumption",
    "Consumer Spending","Consumption",
    "Personal Spending","Consumption",
    "Retail Sales MoM","Consumption",
    "Retail Sales YoY","Consumption",
    "Mortgage Rate","Consumption",


    "Banks Balance Sheet", "Money",
    "Central Bank Balance Sheet", "Money",
    "Money Supply M0", "Money", # Federal Reserve Notes + US Notes + Coins. It is not relevant whether the currency is held inside or outside of the private banking system as reserves.
    "Money Supply M1", "Money", # M0 (cash/coin) outside of the private banking system + the amount of demand deposits, travelers checks and other checkable deposits + most savings accounts.
    "Money Supply M2", "Money", # M1 + money market accounts, retail money market mutual funds, and small denomination time deposits (certificates of deposit of under $100,000)
 #   "Money Supply M3", "Money", # M2 + all other CDs (large time deposits, institutional money market mutual fund balances), deposits of eurodollars and repurchase agreements.


# # # # #


    "Job Vacancies", "Workforce",
    "Job Offers", "Workforce",
    "Services PMI", "Confidence",
    "Manufacturing PMI","Confidence",

# NEU

    "Consumer Confidence","Confidence",

# # # # #


    "Business Confidence","Confidence",
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
                                         
                                                                        # 'High' is good?  Number format    GROUP Order by 
    dplyr::arrange(match(Indicator, c("Government Bond 10Y"             #   1  bad           %                 MARKET
                                     ,"Currency"                        #   2  bad          #
                                     ,"Inflation Rate"                  #   3  bad           %                 Inflationary
                                     ,"Core Inflation Rate"             #   4  bad           %                 
                                     ,"CPI seasonally adjusted"         #   5  bad          #
                                     ,"Producer Prices Change"          #   6  bad           %
                                     ,"Core Producer Prices YoY"        #   7  bad           %
                                     ,"Manufacturing PMI"               #   8      good     #                  Confidence
                                     ,"Services PMI"                    #   9      good     # 
                                     ,"Consumer Confidence"             #  10      good     # 
                                     ,"Money Supply M0"                 #  11      good     #                  Money
                                     ,"Money Supply M1"                 #  12      good     #                
                                     ,"Money Supply M2"                 #  13      good     #           
                                     ,"Balance of Trade"                #  14      good     #                  Productivity  
                                     ,"Imports"                         #  15  bad          #                 
                                     ,"Exports"                         #  16      good     #  
                                     ,"Current Account to GDP"          #  17  bad           %
                                     ,"GDP Annual Growth Rate"          #  18      good      %
                                     ,"Consumer Spending"               #  19      good     #                  Consumption 
                                     ,"Consumer Credit"                 #  20      good     #
                                     ,"Bank Lending Rate"               #  21  bad           %
                                     ,"Mortgage Rate"                   #  22  bad           %
                                     ,"Unemployment Rate"               #  23  bad           %                 Workforce
                                     ,"Initial Jobless Claims"          #  24  bad          #
                                     ,"Continuing Jobless Claims"       #  25  bad          #
                                     ,"Non Farm Payrolls"               #  26      good     #
                                     ,"Average Hourly Earnings YoY"     #  27      good      %                 
                                     ,"Average Weekly Hours"            #  28      good     #
                                     ,"Banks Balance Sheet"             #  29      good     #  
                                     ,"Business Confidence"             #  30      good     #
                                     ,"Central Bank Balance Sheet"      #  31      good     # 
                                     ,"Changes in Inventories"          #  32  bad           %
                                     ,"Factory Orders"                  #  33      good     #
                                     ,"Industrial Production"           #  34      good      %
                                     ,"Job Offers"                      #  35      good     # 
                                     ,"Job Vacancies"                   #  36      good     # 
                                     ,"Labour Costs"                    #  37  bad          #
                                     ,"Personal Spending"               #  38      good      %
                                     ,"Private Sector Credit"           #  39      good     #
                                     ,"Productivity"                    #  40      good     # 
                                     ,"Retail Sales MoM"                #  41      good      %
                                     ,"Retail Sales YoY"                #  42      good      %
                                     ,"Wage Growth"                     #  43      good      %
                                     ,"Bankruptcies"                    #  44  bad          #
    ))) %>%             
    dplyr::mutate(Indicator = ifelse(Indicator == "Currency", "USD Index", Indicator))%>%
    dplyr::group_by(group) %>%  #.[,1] %>% print(n = 44)
    gt::gt() %>%
    gt::tab_spanner(label = "last 10 historical Periods",
                    columns = c(2:11)) %>% 
    
    # color rows where high is BAD
    gt::data_color(columns = 2:11,
                   rows = c(1:7,15,17,21:25,32,37,44),
                   direction = "row",
                   method = "numeric",
                   palette = c("#0F7B7B","#61B7B7","grey","#CC9719","#CC6A19")
    ) %>%
    
    # color rows where high is GOOD
    gt::data_color(columns = 2:11,
                   rows = c(8:14,16,18,19,20,26:31,33:36,38:43),
                   direction = "row",
                   method = "numeric",
                   palette = c("#CC6A19","#CC9719","grey","#61B7B7","#0F7B7B")
    ) %>% 
    
    # format numbers big figures
    gt::fmt_number(columns = 2:11,
                   rows = c(2,5,8:16,19,20,24,25,26,28:31,33,35:37,39,40,44),
                   decimals = 1) %>% 
    
    # format numbers percent figures
    gt::fmt_percent(columns = 2:11,
                    rows = c(1,3,4,6,7,17,18,21,22,23,27,32,34,38,41,42,43),
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











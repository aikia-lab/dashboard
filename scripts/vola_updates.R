

sector_vola_plotly_fun <- function(date = NULL, location = 1,index_mapping, vola_history){
  

  #  mydb <- DBI::dbConnect(RMySQL::MySQL(), user = "ceilert", password = "ceilert", dbname = "fin_data", host = "oben")
  
#  coll_dates <- format(c(
#    lubridate::as_date(date),
#    bizdays::offset(lubridate::as_date(date), -5,'UnitedStates/NYSE'),
#    bizdays::offset(lubridate::as_date(date),-10,'UnitedStates/NYSE'),
#    bizdays::offset(lubridate::as_date(date),-15,'UnitedStates/NYSE'),
#    bizdays::offset(lubridate::as_date(date),-20,'UnitedStates/NYSE'),
#    bizdays::offset(lubridate::as_date(date),-25,'UnitedStates/NYSE')
#  ), "%Y-%m-%d")  
#  coll_dates <- glue::glue_sql("{coll_dates*}", .con = mydb)
#  
#  
#  location_sql <- index_mapping %>% dplyr::filter(country %in% ifelse(location==1, c("EU","DE"),"US")) %>% dplyr::pull(ticker_yh)
#  location_sql <- glue::glue_sql("{location_sql*}", .con = mydb)
  
 
  
#  vola_history <- DBI::dbGetQuery(conn = mydb,
#                                  stringr::str_c("SELECT *
#                                  FROM v_fin_index_expanded
#                                  WHERE date IN (",coll_dates,")
#                                  AND ticker_yh IN (",location_sql,")")) %>% 
#    dplyr::mutate(date = lubridate::as_date(date))
 
# GLOBAL GELADEN   
#  index_mapping <- DBI::dbGetQuery(conn = mydb,
#                                   "SELECT ticker_yh,
#                                   supersector,
#                                   name
#                                   FROM fin_index_meta_data")
  
#  DBI::dbDisconnect(mydb)
  
  location_idx <- index_mapping %>% dplyr::filter(country %in% ifelse(location==1, c("EU","DE"),"US"))# %>% dplyr::pull(ticker_yh)
  

  vola_change <- vola_history %>% 
    dplyr::filter(ticker_yh %in% (location_idx %>% dplyr::pull(ticker_yh))) %>% 
    dplyr::left_join(index_mapping, by = "ticker_yh") %>% 
    dplyr::group_by(ticker_yh) %>%
    dplyr::arrange(desc(date)) %>%
    dplyr::mutate(lead_vola = dplyr::lead(vola_d),
                  vola_chg = (vola_d - lead_vola) /  lead_vola) %>%  # calculate Day to Day change
    dplyr::filter(date %in% head(unique(date),5)) %>%
    dplyr::mutate(date = forcats::fct_rev(forcats::as_factor(as.character(date))))
    
  
  limits <- c(sign(min(vola_change$vola_chg[!is.na(vola_change$vola_chg)]))*ceiling(abs(min(vola_change$vola_chg[!is.na(vola_change$vola_chg)]))*200)/200 - 0.001,
              sign(max(vola_change$vola_chg[!is.na(vola_change$vola_chg)]))*ceiling(abs(max(vola_change$vola_chg[!is.na(vola_change$vola_chg)]))*200)/200 - 0.001)
  
  maxlimit <- c(-max(abs(limits)), max(abs(limits)))
  
  
  aspect_vector <- location_idx %>% 
    dplyr::count(supersector) %>% 
    dplyr::filter(supersector != "Bond") %>%
    .[c(9,1:8,10),] %>%
    tibble::rowid_to_column(var = "id") %>% 
    dplyr::mutate(height = n/sum(n),
                  i = dplyr::case_when(
                            id%%3 == 0 ~ -0.4,
                            id%%3 == 1 ~ -0.35,
                            id%%3 == 2 ~ -0.3))
  
  sector_list <- dplyr::pull(aspect_vector, supersector)
  
  click_mapping <- tibble:::tibble(supersector = sector_list,
                                   curve = c(0:9))
  
  plotly_click_mapping <<- index_mapping %>% 
    dplyr::left_join(click_mapping, by = "supersector") %>% 
    dplyr::filter(!is.na(curve)) %>% 
    dplyr::arrange(curve) %>% 
    dplyr::left_join(vola_history %>% 
                       dplyr::distinct(ticker_yh), by = "ticker_yh") %>% 
    dplyr::arrange(desc(name)) %>% 
    dplyr::group_by(curve) %>% 
    dplyr::mutate(y = dplyr::row_number()) %>% 
    dplyr::arrange(curve)
  
  sector_plots <- purrr::map(unique(sector_list),
                             function(sect, data, i, limit){
                               
                               i <- i %>% 
                                 dplyr::filter(supersector == sect) %>% 
                                 dplyr::pull(i)
                               
                               vol <- data %>% 
                                 dplyr::filter(supersector == sect) %>% 
                                 dplyr::ungroup() %>% 
                                 dplyr::select(name, date, vola_chg) %>% 
                                 dplyr::arrange(., name, date) %>% 
                                 tidyr::pivot_wider(names_from = date,
                                                    values_from = vola_chg) %>% 
                                 dplyr::mutate_all(~replace(.,is.na(.),0)) %>% 
                                 tibble::column_to_rownames(var = "name") %>% 
                                 as.matrix()
                               
                               # Custom Hover Text
                               label_names <- colnames(vol)
                               text_mat <- as.data.frame(vol)
                               text_mat[] <- lapply(seq_along(text_mat), function(x){
                                 paste0("Date: ",
                                        label_names[x],
                                        "\nVola Change: ",
                                        scales::percent(vol[, x], accuracy = 0.0001),
                                        "\nSupersector: ",
                                        sect)
                               })
                               
                               hm <- heatmaply::heatmaply(
                                 vol,
                                 colors = colorRampPalette(c('blue','white',"red")),
                                 hide_colorbar = TRUE,
                                 limits = limit,
                                 label_names = c("Index", "Day:", "Vola Change in %:"),
                                 dendrogram = "none",
                                 plot_method = "plotly",
                                 custom_hovertext = text_mat) %>% 
                                 plotly::layout(annotations = list(text = stringr::str_c("<b>",sect,"</b>"), 
                                                                   font = list(size = 12),
                                                                   xref = "paper",
                                                                   x = i,
                                                                   yref = "paper",
                                                                   y = 0.5,
                                                                   yshift = 0,
                                                                   yanchor = "middle",
                                                                   showarrow = FALSE,
                                                                   textangle = 270,
                                                                   font = list(size = 10,
                                                                               color = "black")
                                                                   ),
                                                yaxis = list(title = list(text = " ",
                                                                          font = list(size = 8),
                                                                          standoff = 50)),
                                                margin = list(l = 500, # 250
                                                              r = 20, 
                                                              t = 10,
                                                              b = 100,
                                                              autoexpand = FALSE)
                                                )
                               
                               return(hm)
                             },
                             data = vola_change,
                             i = aspect_vector,
                             limit = maxlimit) %>% 
    setNames(unique(sector_list))
  
print(aspect_vector$height)  
  ind_subs <- plotly::subplot(
    sector_plots,
    margin = 0.02, 
    nrows = length(sector_plots),
    heights = aspect_vector$height,
    shareX = TRUE,
    shareY = TRUE
  )

  return(ind_subs)
}

sector_line_chart_fun <- function(index_id, date,index_mapping, vola_history){
  

#  #mydb <- connect_to_DB()
#  mydb <- DBI::dbConnect(RMySQL::MySQL(), user = "ceilert", password = "ceilert", dbname = "fin_data", host = "oben")
#  
#
#  idx_history <- DBI::dbGetQuery(conn = mydb,
#                                 stringr::str_c("SELECT *
#                                              FROM v_fin_index_expanded
#                                              WHERE ticker_yh = '",index_id,"'
#                                              ORDER BY date DESC")) %>% 
#    dplyr::mutate(date = lubridate::as_date(date))
#  
# DBI::dbDisconnect(mydb)
  
  ticker_history <- vola_history %>%
    dplyr::filter(ticker_yh == index_id) %>% 
    dplyr::arrange(desc(date)) %>% 
    dplyr::left_join(index_mapping[,c("ticker_yh", "name")], by = "ticker_yh")
  index_name <- unique(ticker_history$name)
  
 index_line <- ticker_history %>% 
     plotly::plot_ly(x = ~date, y = ~close,
                   type = 'scatter',
                   mode = 'lines',
                   text = ~name,
                   color = ~ticker_yh,
                   hovertemplate = "Index: %{text}<br>Price: %{y:.0}<br>Date: %{x}<extra></extra>",
                   colors = main_color) %>% 
     plotly::layout(xaxis = list(title = "Date",
                                 range = c(bizdays::offset(lubridate::as_date(date),-50,'UnitedStates/NYSE'),lubridate::as_date(date)),
                                 rangeselector = list(
                                   buttons = list(
                                     list(
                                       count = 3,
                                       label = "3 mo",
                                       step = "month",
                                       stepmode = "backward"),
                                     list(
                                       count = 6,
                                       label = "6 mo",
                                       step = "month",
                                       stepmode = "backward"),
                                     list(
                                       count = 1,
                                       label = "1 yr",
                                       step = "year",
                                       stepmode = "backward"),
                                     list(
                                       count = 1,
                                       label = "YTD",
                                       step = "year",
                                       stepmode = "todate"),
                                     list(step = "all"))),
                                 rangeslider = list(type = "date")
                                 ),
                    yaxis = list(title = "Index Level"),
                    title = list(
                      text = stringr::str_c("Index Price and daily Returns for<br>", index_name)
                    )
                    
     )
  
  index_bar <- ticker_history %>% 
    plotly::plot_ly(x = ~date, y = ~dtd_return,
                    type = 'bar',
                    text = ~name,
                    color = ~ticker_yh,
                    hovertemplate = 'Index: %{text}<br>Return: %{y:.2}<br>Date: %{x}',
                    colors = main_color) %>% 
   plotly::layout(xaxis = list(title = "Date",
                               range = c(bizdays::offset(lubridate::as_date(date),-50,'UnitedStates/NYSE'),lubridate::as_date(date)),
                               rangeselector = list(
                                 buttons = list(
                                   list(
                                     count = 3,
                                     label = "3 mo",
                                     step = "month",
                                     stepmode = "backward"),
                                   list(
                                     count = 6,
                                     label = "6 mo",
                                     step = "month",
                                     stepmode = "backward"),
                                   list(
                                     count = 1,
                                     label = "1 yr",
                                     step = "year",
                                     stepmode = "backward"),
                                   list(
                                     count = 1,
                                     label = "YTD",
                                     step = "year",
                                     stepmode = "todate"),
                                   list(step = "all"))),
                               rangeslider = list(type = "date"),
                               mirror = TRUE,
                               ticks = 'outside',
                               showline = TRUE),
                  yaxis = list(title = "Daily Returns",
                               tickformat = ".0%",
                               mirror = TRUE,
                               ticks = 'outside',
                               showline = TRUE),
                  showlegend = FALSE
    )
 
 plotly::subplot(
   index_line,
   index_bar,
   nrows = 2,
   heights = c(0.8,0.2),
   shareX = TRUE,
   margin = 0.04
 )

   
}





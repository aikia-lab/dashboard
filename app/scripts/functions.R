

connect_to_DB <- function(mydb, group = "fin_data"){
  
  Checkmydb <- tryCatch(DBI::dbIsValid(mydb),
                        error=function(e) e)
  if(inherits(Checkmydb, "simpleError")){
    
    if(Sys.info()[[1]] == "Windows"){ #For testing on Windows
      
      mydb <- DBI::dbConnect(RMariaDB::MariaDB(), 
                             group = group, 
                             default.file = stringr::str_c(here::here(),
                                                           "/my.cnf"))
      
    } else { #Production on Linux (No Macs here ;))
      
      mydb <- DBI::dbConnect(RMariaDB::MariaDB(), group = group)
      
    }
  }
}


get_fed_meeting_dates <- function(){
  
  mydb <- connect_to_DB()
  fed_meeting_dates <- DBI::dbGetQuery(mydb, "SELECT
                                       DISTINCT(meeting_date) AS meeting_date
                                       FROM fin_data.eco_fed_funds_rate") %>% 
    dplyr::filter(meeting_date >= lubridate::today()) %>% 
    dplyr::pull(meeting_date)
  
  DBI::dbDisconnect(mydb)
  
  return(fed_meeting_dates)
}


get_available_indices <- function(){
  
  mydb <- connect_to_DB()
  
  get_indices <- DBI::dbGetQuery(mydb, "SELECT 
                                       DISTINCT(ticker_yh) AS indices
                                       FROM fin_index_history") %>%
    dplyr::pull(indices)
  
  DBI::dbDisconnect(mydb)
  
  return(get_indices)
  
}



write_counter_to_sql <- function(){

  tstamp <- tibble::tibble(timestamp = format(
    lubridate::now(tzone = "CET"), 
    "%Y-%m-%d %H:%M:%S")
    )
  
   mydb <- connect_to_DB()

   DBI::dbWriteTable(mydb, 
                      name= "usage_dashbard", 
                      value = tstamp, 
                      row.names = FALSE, 
                      header = TRUE,
                      append = TRUE)
    
    DBI::dbDisconnect(mydb)
    
}


get_fed_rates_fun <- function(date1 = NULL, date2 = NULL){
  
  mydb <- connect_to_DB()
  
  if(!is.null(date1)){
  fed_curve_date1 <- DBI::dbGetQuery(mydb,
                  stringr::str_c(
                    "SELECT * 
                  FROM fin_data.eco_fed_funds_rate
                  WHERE date = '",date1,"'"))
  }
  
  if(!is.null(date2)){
  fed_curve_date2 <- DBI::dbGetQuery(mydb,
                                     stringr::str_c(
                                       "SELECT * 
                  FROM fin_data.eco_fed_funds_rate
                  WHERE date = '",date2,"'"))  
  }

  fed_plotly <- plotly::plot_ly(
    data = fed_curve_date1,
    name = date1,
    x = ~meeting_date,
    y = ~no.steps,
    text = ~p_change,
    type = "scatter",
    mode = "lines+markers",
    line = list(
      color = main_color),
    marker = list(
      color = main_color),
    hovertemplate = paste0("<extra><b>Curve Date: ",date1,"</b><br>Combined expected hikes: %{y:.2f}<br>Expected hikes in this meeting: %{text:.2f}</extra>")) %>% 
    plotly::add_annotations(
      data = fed_curve_date1,
      x = ~meeting_date,
      y = ~no.steps + 0.5,
      text = ~ scales::percent(p_change, accuracy = 0.01),
      showarrow = FALSE,
      font = list(
        color = main_color
      )
    ) 
  
  if(!is.null(date2)) {
    
    fed_plotly <- fed_plotly %>% 
      plotly::add_trace(
      data = fed_curve_date2,
      name = date2,
      x = ~meeting_date,
      y = ~no.steps,
      type = "scatter",
      mode = "lines+markers",
      line = list(
        color = palette_main[4]),
      marker = list(
        color = palette_main[4]),
      hovertemplate = paste0("<extra><b>Curve Date: ",date2,"</b><br>Combined expected hikes: %{y:.2f}<br>Expected hikes in this meeting: %{text:.2f}</extra>")) %>% 
      plotly::add_annotations(
        data = fed_curve_date2,
        x = ~meeting_date,
        y = ~no.steps - 0.5,
        text = ~ scales::percent(p_change, accuracy = 0.01),
        showarrow = FALSE,
        font = list(
          color = palette_main[4]
        )
      ) 
    
  }
  
  plotly_output <- fed_plotly %>% 
    plotly::layout(
      xaxis = list(
        title = "FED Meeting Date"
      ),
      yaxis = list(
        title = "No. of Implied Rate Hikes"
      ),
      hovermode = "x unified"
    )
  DBI::dbDisconnect(mydb)
  return(plotly_output)
  
}


meeting_data <- function(act_meeting_date = NULL){


  mydb <- connect_to_DB()
  
  meeting <- DBI::dbGetQuery(
    mydb,
    stringr::str_c(
      "SELECT *
        FROM fin_data.eco_fed_funds_rate AS effr
        WHERE meeting_date = '",act_meeting_date,"'"
    )
  ) %>%
  dplyr::as_tibble() %>% 
  dplyr::mutate(p_nochange = ifelse(p_nochange < 0, 0, p_nochange)) %>% 
  dplyr::arrange(date) %>% 
  dplyr::select(date,
                "Rate change probability" = p_change,
                "No rate change probability" = p_nochange,
                "Implied rate" = impl.rate,
                "No. of expected steps" = no.steps) %>% 
    tidyr::pivot_longer(-date,
                        names_to = "symbols",
                        values_to = "values")
  
  
  DBI::dbDisconnect(mydb)

  # helper function for plotly update
  getbuttons <- function(symbols){
    
    list(method = "restyle",
         args = list("transforms[0].value", symbols),
         label = symbols)
    
  }
  
  buttons_filter <- purrr::map(unique(meeting$symbols), getbuttons)
  
  
  meeting_plotly <- plotly::plot_ly(data = meeting,
                  x = ~date,
                  y = ~values,
                  type = "scatter",
                  mode = "lines",
                  hovertemplate = "Date:%{x}<br>Value:%{y:.2f} <extra></extra>",
                  line = list(
                    color = palette_main[4]),
                  transforms = list(
                    list(
                      type = 'filter',
                      target = ~symbols,
                      operation = 'in',
                      value = unique(meeting$symbols)[1]))
                  ) %>%
    plotly::layout(xaxis = list(title = "Date",
                                rangeslider = list(visible = F)),
                   updatemenus = list(
                     list(
                       y = 1.1,
                       x = 0.1,
                       type = "dropdown",
                       active = 0,
                       buttons = buttons_filter
                     )
                   )
    )

  
  return(meeting_plotly)
}



entr_pnl_plotly_fun <- function(valuation_date, index){
    
  
  date_hist <- bizdays::offset(lubridate::today(), -200, 'UnitedStates/NYSE')
  
  
  mydb <- connect_to_DB()
  
  index_hist <- DBI::dbGetQuery(mydb, paste0("SELECT * 
                         FROM fin_index_history
                         WHERE date <= '", valuation_date,"'
                         AND date > '", date_hist, "'
                         AND ticker_yh = '", index,"'"))
  
  entropy_hist <- DBI::dbGetQuery(mydb, paste0("SELECT * 
                         FROM fin_index_entropy_history
                         WHERE date <= '", valuation_date,"'
                         AND date > '", date_hist, "'
                         AND indices = '", index,"'"))
  
  DBI::dbDisconnect(mydb)
  
  
  index_price_plot <- index_hist %>% 
    dplyr::arrange(date) %>%  
    plotly::plot_ly(x = ~date, y = ~close,
                    name = "", 
                    type = 'scatter',
                    mode = 'lines', 
                    line = list(color = main_color),
                    hovertemplate = paste0("<extra><b>Curve Date: %{x}</b><br>Index Level: %{y:.0f}<br></extra>")) %>%
    plotly::layout(xaxis = list(title = "Date"),
                   yaxis = list(title = "Index Level"),
                   showlegend = FALSE
    )
  
  index_pnl_plot <- index_hist %>% 
     dplyr::arrange(date) %>% 
     dplyr::mutate(log_ret = log(close/dplyr::lag(close)),
                   pnl_color = ifelse(log_ret >= 0, "green", "red")) %>% 
    plotly::plot_ly(x = ~date, y = ~log_ret, 
                    name = ~ifelse(log_ret < 0,"loss","profit"), 
                    type = 'bar', 
                    color = ~log_ret < 0, 
                    colors = c(palette_main[1], palette_main[4])) %>%
      plotly::layout(xaxis = list(title = "Date"),
                     yaxis = list(title = "Log Returns"),
                     showlegend = FALSE
                     )
  
  index_vol_plot <- 
    index_hist %>% 
    dplyr::arrange(date) %>% 
    plotly::plot_ly(x = ~date, y = ~volume , 
                    name = "Volume", 
                    type = 'bar', 
                    marker = list(color = palette_main[3])) %>% 
      plotly::layout(xaxis = list(title = "Date"),
                   yaxis = list(title = "Traded Volume"),
                   showlegend = FALSE
    )
      
    
    entropy_subs <- plotly::subplot(
      index_price_plot,index_pnl_plot,index_vol_plot,
      margin = 0.005, 
      nrows = 3,
      heights = c(0.5,0.25,0.25),
      shareX = TRUE,
      shareY = TRUE
    ) %>% 
      plotly::layout(annotations = list(
        list(x = -0.2 , 
             y = -0.15, 
             text = "<b>click on date to see correlation groups</b>",
             showarrow = F,
             xref='paper',
             yref='paper'))
      )
    


}


entrop_tic_group_fun <- function(start_date, cur_idx, corr_th = 0.7){

  
  end_date <- bizdays::offset(lubridate::today(), -200, 'UnitedStates/NYSE')
  
  
  
  
  
  # !!!!!!!!!!!!!!!!!!!!!!!!!
  
#  idx_tic_rel muss erst noch geladen werden - am besten 1x global !!!!!!!!!!!!!
  
  test_tic <- idx_tic_rel %>% dplyr::filter(index == cur_idx) %>% dplyr::pull(ticker_yh) %>% 
    paste0(., collapse = "', '")
  
#  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  
  
  
  
  
  
  
  mydb <- connect_to_DB()
  ticker_hist <- DBI::dbGetQuery(mydb, paste0("SELECT *
                                 FROM fin_ticker_history
                                 WHERE date <= '",start_date,"' 
                                 AND date > '",end_date,"'
                                 AND ticker_yh IN ('",test_tic,"')"))
  DBI::dbDisconnect(mydb)
  
  
  suppressWarnings(
    cor_matrix <- ticker_hist %>%
      dplyr::arrange(date) %>% 
      dplyr::group_by(ticker_yh) %>%
      tidyr::fill(close, .direction = "updown") %>% 
      dplyr::mutate(log_ret = log(close/dplyr::lag(close))) %>% 
      tidyr::drop_na() %>%
      dplyr::add_count() %>% 
      dplyr::ungroup() %>% 
      dplyr::filter(n == max(n)) %>%
      dplyr::select(ticker_yh, log_ret) %>% 
      tidyr::pivot_wider(names_from = ticker_yh, values_from = log_ret) %>% # values_fn = length 
      tidyr::unnest(cols = dplyr::everything()) %>% 
      cor()
  )
  
  # Adjacency
  g  <- igraph::graph.adjacency(abs(cor_matrix) > as.numeric(corr_th), 
                                mode = "upper", 
                                weighted=TRUE, 
                                diag = FALSE) # !!!!!!!!!!!!! ABSOLUTE CORR VALUES 
  
  
  #Louvain Comunity Detection
  #cluster <- igraph::cluster_louvain(g)
  #groupd_df <- data.frame(group = cluster$membership,
  #                        nodes = cluster$names)
  #
  #single_groups <- groupd_df %>% dplyr::count(group) %>% dplyr::filter(n == 1) %>% dplyr::pull(group)
  #
  #
  ##Create df for plotting with ggplot
  #gplot_netw <- ggnetwork::ggnetwork(g, 
  #                                   layout = igraph::layout.fruchterman.reingold(g))
  #gplot_netw <- dplyr::left_join(gplot_netw, groupd_df, by = c("name"="nodes"))
  #
  #plot <- gplot_netw %>% 
  #  ggplot2::ggplot(ggplot2::aes(x = x, y = y, xend = xend, yend = yend)) +
  #  ggnetwork::geom_edges(ggplot2::aes(linetype = "x"), color = "grey80", curvature = 0.1) +
  #  ggnetwork::geom_nodes(ggplot2::aes(color = factor(group)), size = 3, alpha = 0.4) +
  #  ggplot2::labs(title = stringr::str_c("<span style = 'font-size: 20pt; color:",main_color,"'> ",
  #                                       start_date, "</span>"),
  #                subtitle = stringr::str_c("<span style = 'font-size: 8pt'> Number of Independent Groups </span>",
  #                                          "<span style = 'font-size: 20pt'> ", 
  #                                          max(groupd_df$group) ,"</span>"),
  #                y = "Logarithmic Returns") +
  #  ggplot2::theme_void() +
  #  ggplot2::theme(legend.position = "none",
  #                 plot.title = ggtext::element_markdown(hjust = 0.95),
  #                 plot.subtitle = ggtext::element_markdown(hjust = 0.95, 
  #                                                          colour = main_color_light, 
  #                                                          face = "bold")) 
  #
  #plotly::ggplotly(plot)
 
  vs <- igraph::V(g)
  es <- as.data.frame(igraph::get.edgelist(g)) # Get edgelist
  Ne <- length(es[1]$V1) #number of edges
  
  node.data<-igraph::get.data.frame(g,what="vertices")
  L <- igraph::layout.fruchterman.reingold(g)
  Xn <- L[,1]
  Yn <- L[,2]
  
  #Creates the nodes (plots the points)
  network <- plotly::plot_ly(x = ~Xn, y = ~Yn, #Node points
                     mode = "markers", 
                     text = vs$name, 
                     hoverinfo = "text",
                     color =as.factor(node.data$name) )
  
  #Create edges
  edge_shapes <- list()
  names(Xn) <- names(vs)
  names(Yn) <- names(vs)
  for(i in 1:Ne) {
    v0 <- as.character(es[i,]$V1)
    v1 <- as.character(es[i,]$V2)
    
    edge_shape = list(
      type = "line",
      line = list(color = "gray", width = 0.3),
      x0 = Xn[v0],
      y0 = Yn[v0],
      x1 = Xn[v1],
      y1 = Yn[v1]
    )
    
    edge_shapes[[i]] <- edge_shape
  }
  
  axis <- list(title = "", showgrid = FALSE, 
               showticklabels = FALSE, zeroline = FALSE)
  
  graph <- plotly::layout(
    network,
    title = 'Networks & Plotly',
    shapes = edge_shapes,
    xaxis = axis,
    yaxis = axis,
    showlegend=FALSE
  )
  
  return(graph)
  
  
  
}











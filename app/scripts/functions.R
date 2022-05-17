

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


date_seq_fun <- function(ticker,end_date_fun,start_date_fun){
  date_seq <- tibble::tibble(date = bizdays::bizseq(end_date_fun, start_date_fun, 'UnitedStates/NYSE'),
                             ticker_yh = ticker)
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
  fed_curve_date1 <<- DBI::dbGetQuery(mydb,
                  stringr::str_c(
                    "SELECT * 
                  FROM fin_data.eco_fed_funds_rate
                  WHERE date = '",date1,"'"))
  }
  
  if(!is.null(date2)){
  fed_curve_date2 <<- DBI::dbGetQuery(mydb,
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



entropy_pnl_fun <- function(valuation_date, index){
    
  
#  date_hist <- bizdays::offset(lubridate::today(), -200, 'UnitedStates/NYSE')
  date_hist <- "2020-01-02" # to include Corona Crisis

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
                    hovertemplate = paste0("<extra><b>Curve Date: %{x}</b><br>Index Level: %{y:,.0f}<br></extra>")) %>%
    plotly::layout(xaxis = list(title = "Date", type="date", tick0 = ~min(date), dtick = "M2"),
                   #xaxis = list(title = "", showticklabels = F, zeroline = F,  showline = F),
                   yaxis = list(title = list(text='Index\n Level', standoff = 10),tickformat = "digits"),
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
                    colors = c(palette_main[1], palette_main[4]),
                    hovertemplate = paste0("<extra><b>P&L Date: %{x}</b><br>:P&L %{y:.2%}<br></extra>")) %>%
      plotly::layout(xaxis = list(title = "Date", type="date", tick0 = ~min(date), dtick = "M2"),
        #xaxis = list(title = "", showticklabels = F, zeroline = F,  showline = F),
                     yaxis = list(title = list(text='Log\n Returns', standoff = 10), 
                                  tickformat = "%",
                                  tickvals = list(0)),
                     showlegend = FALSE
                     )
                     
  
  index_vol_plot <- index_hist %>% 
    dplyr::arrange(date) %>% 
    plotly::plot_ly(x = ~date, y = ~volume , 
                    name = "Volume", 
                    type = 'bar', 
                    marker = list(color = palette_main[3]),
                    hovertemplate = paste0("<extra><b>Trade Date: %{x}</b><br>Volume: %{y:,.0f}<br></extra>")) %>% 
      plotly::layout(xaxis = list(title = "Date", type="date", tick0 = ~min(date), dtick = "M2"),
        #xaxis = list(title = "", showticklabels = F, zeroline = F,  showline = F),
                   yaxis = list(title = list(text='Traded\n Volume', standoff = 20)),
                   showlegend = FALSE
    )

  entropy_tl <- entropy_hist %>% 
    dplyr::distinct(date, threshold, .keep_all = T) %>% 
    dplyr::arrange(date) %>% 
    plotly::plot_ly(x = ~date, y = ~entropy,
                    type = 'scatter',
                    mode = 'lines',
                    color = ~as.factor(threshold),
                    colors = c(palette_main[1],palette_main[2],palette_main[3],palette_main[4]),
                    text = ~threshold,
                    hovertemplate = paste0("<extra><b>Entropy Date: %{x}</b><br>threshold limit: %{text:.1f}<br>current Level: %{y:.1f}</extra>")) %>% 
      plotly::layout(xaxis = list(title = "Date", type="date", tick0 = ~min(date), dtick = "M2"),
                     yaxis = list(title = list(text='Entropy', standoff = 10)),
                     showlegend = FALSE
      )

    
    
    entropy_subs <- plotly::subplot(
      index_price_plot,index_pnl_plot,index_vol_plot,entropy_tl,
      margin = 0.005, 
      nrows = 4,
      heights = c(0.4,0.1,0.1,0.4),
      shareX = T,
      shareY = TRUE
    ) %>% 
      plotly::layout(annotations = list(
        list(x = -0.2 , 
             y = -0.3, 
             text = "<b>click on date to see correlation groups</b>",
             showarrow = F,
             xref='paper',
             yref='paper'))
      )
    


}


#start_date <- "2022-05-16"
#cur_idx <- "^NDX"
#corr_th <- 0.7
#grouping <- "Industry"
entrop_tic_group_fun <- function(start_date, cur_idx, corr_th, sector_info, grouping){

  
  end_date <- bizdays::offset(lubridate::as_date(start_date), -51, 'UnitedStates/NYSE')

  
  idx_tickers <- idx_tic_rel %>% dplyr::filter(index == cur_idx) %>% dplyr::pull(ticker_yh) %>% 
    paste0(., collapse = "', '")


  mydb <- connect_to_DB()
  
  entropy_groups <- DBI::dbGetQuery(mydb, paste0("SELECT COUNT(DISTINCT(membership)) AS n
                         FROM fin_index_entropy_history
                         WHERE date = '", start_date,"'
                         AND indices = '", cur_idx,"'
                         AND threshold = '", corr_th,"'")) %>% 
    dplyr::pull(n)
  
  
  ticker_hist <- DBI::dbGetQuery(mydb, paste0("SELECT *
                                 FROM fin_ticker_history
                                 WHERE date <= '",start_date,"' 
                                 AND date > '",end_date,"'
                                 AND ticker_yh IN ('",idx_tickers,"')")) %>% 
    dplyr::mutate(date = lubridate::as_date(date)) 
  
  
  DBI::dbDisconnect(mydb) 
  


  
  date_template <- purrr::map_df(unique(ticker_hist$ticker_yh), date_seq_fun, end_date_fun = end_date, start_date_fun = start_date)
  
  suppressWarnings(
    cor_matrix <- dplyr::left_join(date_template, ticker_hist[,c("close","ticker_yh","date")], by = c("ticker_yh","date")) %>%
      dplyr::arrange(date) %>% 
      dplyr::group_by(ticker_yh) %>%
      tidyr::fill(close, .direction = "updown") %>% 
      dplyr::mutate(log_ret = log(close/dplyr::lag(close))) %>% 
      tidyr::drop_na() %>%
      dplyr::ungroup() %>% 
      dplyr::select(ticker_yh, log_ret) %>% 
      tidyr::pivot_wider(names_from = ticker_yh, values_from = log_ret) %>% # values_fn = length 
      tidyr::unnest(cols = dplyr::everything()) %>% 
      cor()
  )
  
  
  # Error Handling in case a correlation fails and prints NA. The correlation to itself is still 1 which leads to ncol()-1
  cor_matrix <- cor_matrix[rowSums(is.na(cor_matrix)) != ncol(cor_matrix)-1, colSums(is.na(cor_matrix)) != nrow(cor_matrix)-1]
  
 
  # Adjacency
  g  <- igraph::graph.adjacency(abs(cor_matrix) > as.numeric(corr_th), 
                                mode = "upper", 
                                weighted=TRUE, 
                                diag = FALSE)

  # Plot Details
  comp_info <- tibble::tibble(ticker_yh = unique(ticker_hist$ticker_yh)) %>% 
    dplyr::left_join(.,sector_info, by = "ticker_yh")
  vs <- igraph::V(g)
  g <- igraph::set_vertex_attr(g, "Company", index = igraph::V(g), comp_info$ticker_yh)
  g <- igraph::set_vertex_attr(g, "Industry", index = igraph::V(g), comp_info$BIC_1)
  g <- igraph::set_vertex_attr(g, "Sector", index = igraph::V(g), comp_info$BIC_2)
  g <- igraph::set_vertex_attr(g, "Description", index = igraph::V(g), comp_info$name)
  
  es <- as.data.frame(igraph::get.edgelist(g)) # Get edgelist
  Ne <- length(es[1]$V1) #number of edges
  
#  node.data<-igraph::get.data.frame(g, what="vertices")
  L <- igraph::layout.fruchterman.reingold(g)
  Xn <- L[,1]
  Yn <- L[,2]


  #Creates the nodes (plots the points)
  network <- plotly::plot_ly(x = ~Xn, y = ~Yn, #Node points
                     type = "scatter",         
                     mode = "markers", 
                     size = 2, # probably
                     hovertemplate = paste0("Name: ",igraph::V(g)$Description,"\n",
                                            "Ticker: ", igraph::V(g)$Company,"\n",
                                            "Industry: ", igraph::V(g)$Industry,"\n",
                                            "Sector: ", igraph::V(g)$Sector,
                                            "<extra></extra>"),
                     color = igraph::get.vertex.attribute(g,grouping)#as.factor(igraph::V(g)$Industry)
                    )

  
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
    title = list(text = paste0('Correlation Network as of <b>',start_date,'</b><br>',
                               '<sup> Number of Groups </sup>', entropy_groups), 
                 x = 1),
    shapes = edge_shapes,
    xaxis = axis,
    yaxis = axis,
    showlegend=FALSE
  )
  
  return(graph)
  
  
  
}






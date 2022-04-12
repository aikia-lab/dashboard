

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


#meeting_data <- function(meeting_date = NULL){
#
#mydb <- connect_to_DB()
#
#meeting <- DBI::dbGetQuery(
#  mydb,
#  stringr::str_c(
#    "SELECT *
#      FROM fin_data.eco_fed_funds_rate AS effr
#      WHERE meeting_date = '",meeting_date,"'"
#  )
#)
#plotly::plot_ly(data = dec_meeting,
#                x = ~date,
#                y = ~p_change,
#                type = "scatter",
#                mode = "lines")
#
#DBI::dbDisconnect(mydb)
#return(meeting)
#}



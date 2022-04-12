

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
                  WHERE retrieval_date = '",date1,"'"))  
  }
  
  if(!is.null(date2)){
  fed_curve_date2 <- DBI::dbGetQuery(mydb,
                                     stringr::str_c(
                                       "SELECT * 
                  FROM fin_data.eco_fed_funds_rate
                  WHERE retrieval_date = '",date2,"'"))  
  }

  fed_plotly <- plotly::plot_ly(
    data = fed_curve_date1,
    x = ~meeting_date,
    y = ~no.steps,
    type = "scatter",
    mode = "lines+markers")
  
  if(!is.null(date2)) {
    
    fed_plotly <- fed_plotly %>% 
      plotly::add_trace(
      data = fed_curve_date2,
      x = ~meeting_date,
      y = ~no.steps,
      type = "scatter",
      mode = "lines+markers")
    
  }
  
  DBI::dbDisconnect(mydb)
  return(fed_plotly)
  
}


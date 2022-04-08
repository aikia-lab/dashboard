

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





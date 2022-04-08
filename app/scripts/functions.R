

connect_to_DB <- function(mydb, group = "fin_data"){
  
  Checkmydb <- tryCatch(DBI::dbIsValid(mydb),
                        error=function(e) e)
  if(inherits(Checkmydb, "simpleError")){
    
    if(Sys.info()[[1]] == "Windows"){ #For testing on Windows
      
      mydb <- DBI::dbConnect(RMariaDB::MariaDB(), group = group, 
                             default.file = stringr::str_c(here::here(),
                                                           "/my.cnf"))
      
    } else { #Production on Linux
      
      mydb <- DBI::dbConnect(RMariaDB::MariaDB(), group = group)
      
    }
  }
}


write_usage_to_sql <- function(data){
  
  new_usage <-data.frame(date = lubridate::now(), 
                         ip = data$ip,
                         city = data$city,
                         country = data$country,
                         loc = data$loc)
  new_usage <- new_usage %>% dplyr::filter(ip != "217.91.79.198")
  
  if(nrow(new_usage)>0){
    
    mydb <- connect_to_DB()
    
    DBI::dbWriteTable(mydb, 
                      name= "dashboard_usage", 
                      value = new_usage, 
                      row.names = FALSE, 
                      header = TRUE,
                      append = TRUE)
    
    DBI::dbDisconnect(mydb)
  }
  
  
}








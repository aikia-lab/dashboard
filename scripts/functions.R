

connect_to_DB <- function(mydb, group = "fin_data"){
  
  Checkmydb <- tryCatch(DBI::dbIsValid(mydb),
                        error=function(e) e)
  if(inherits(Checkmydb, "simpleError")){
    
    if(Sys.info()[[1]] == "Windows"){ #For testing on Windows
      
      mydb <- DBI::dbConnect(RMariaDB::MariaDB(), group = group, 
                             default.file = stringr::str_c(here::here(),
                                                           "/.my.cnf"))
      
    } else { #Production on Linux
      
      mydb <- DBI::dbConnect(RMariaDB::MariaDB(), group = group)
      
    }
  }
}











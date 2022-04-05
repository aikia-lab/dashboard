


connect_to_DB <- function(mydb, group = "fin_data"){
  
  Checkmydb <- tryCatch(DBI::dbIsValid(mydb),
                        error=function(e) e)
  if(inherits(Checkmydb, "simpleError")){
    
    mydb <- DBI::dbConnect(RMariaDB::MariaDB(), group = group,
                           user = "ceilert", password = "ceilert", host = "oben")
  }
}













library(tidyverse)
library(DBI)
library(scales)
library(lubridate)
library(dbplyr)
library(RSQLite)

working.dir <- "C:\\Users\\conor\\Desktop\\Thesis"

con <- dbConnect(RSQLite::SQLite(),
                 dbname=paste0(working.dir,
                               "targetsmart_data.db"))

dbListTables(con)

tsdata.1 <- tbl(con, "TSDATA_1_500")
tsdata.2 <- tbl(con, "TSDATA_501_1000")
tsdata.3 <- tbl(con, "TSDATA_1001_1435")


results1 <- data.frame(column=colnames(tsdata.1), num_nulls=0.0, stringsAsFactors=F)

for(i in 1:500) { #nrow(results)){
  this.col <- results1$column[i]
   holder <- tsdata.1 %>% 
     select(this.col) %>% 
     collect()
  
   results1$num_nulls[i] <- sum(is.na(holder[,1]))
  
   if(i %% 10 == 0) {
     print(paste0("Finished ",i," columns!"))
     flush.console()
   }
   
}


results2 <- data.frame(column=colnames(tsdata.2), num_nulls=0.0, stringsAsFactors=F)

for(i in 1:501) { #nrow(results)){
   this.col <- results2$column[i]
   holder <- tsdata.2 %>% 
      select(this.col) %>% 
      collect()
   
   results2$num_nulls[i] <- sum(is.na(holder[,1]))
   
   if(i %% 10 == 0) {
      print(paste0("Finished ",i," columns!"))
      flush.console()
   }
   
}

results3 <- data.frame(column=colnames(tsdata.3), num_nulls=0.0, stringsAsFactors=F)

for(i in 1:436) { #nrow(results)){
   this.col <- results3$column[i]
   holder <- tsdata.3 %>% 
      select(this.col) %>% 
      collect()
   
   results3$num_nulls[i] <- sum(is.na(holder[,1]))
   
   if(i %% 10 == 0) {
      print(paste0("Finished ",i," columns!"))
      flush.console()
   }
   
}


results4 <- rbind(results1,results2,results3)

write.csv(results4,"null_count.csv",row.names = F)

# Example of running a query. 
# union_all = function(table_a,table_b, list_of_columns){
#   # extract database connection
#   connection = table_a$src$con
#   
#   sql_query = build_sql(con = connection,
#                         sql_render(table_a),
#                         "\nUNION ALL\n",
#                         sql_render(table_b)
#   )
#   
#   return(tbl(connection, sql(sql_query)))
# }



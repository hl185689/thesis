library(tidyverse)
library(dbplyr)
library(DBI)
library(scales)
library(lubridate)
library(dbplyr)

#working.dir <- "C:\\Users\\jchan\\Dropbox\\Research\\LeonardHannah\\"
working.dir <- here::here()

con <- dbConnect(RSQLite::SQLite(),
                 dbname=paste0(working.dir,
                               "targetsmart_data.db"))

dbListTables(con)

tsdata.1 <- tbl(con, "TSDATA_1_500")
tsdata.2 <- tbl(con,"TSDATA_501_1000")

d.1 <- tsdata.1 %>% 
  select(voterbase_id,
         "vb.tsmart_first_name",
         "vb.tsmart_last_name",
         "vb.tsmart_full_address",
         "vb.tsmart_zip") %>% 
  collect()

d.2 <- tsdata.2 %>% 
  select(voterbase_id,
         contains("farm")) %>% 
  collect()

d <- d.1 %>% 
  left_join(d.2,by="voterbase_id")

d %>% 
  filter(vb.tsmart_last_name == "MOHR",
         vb.tsmart_first_name == "JACQUELINE") %>% 
  data.frame

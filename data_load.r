library(tidyverse)
library(dbplyr)
library(DBI)
library(scales)
library(lubridate)
library(dbplyr)

working.dir <- "C:\\Users\\conor\\Desktop\\Thesis\\tsmart_um_mt_analytic_20200210\\"
d <- readr::read_tsv(paste0(working.dir,"tsmart_um_mt_analytic_20200210.csv"))

con <- dbConnect(RSQLite::SQLite(),
                 dbname=paste0(working.dir,
                               "targetsmart_data.db"))

# Listing the tables
dbListTables(con)

copy_to(con, d %>% select(1:500), 
        "TSDATA_1_500",
        temporary = FALSE,
        overwrite=TRUE)

copy_to(con, d %>% select(1,501:1000), 
        "TSDATA_501_1000",
        temporary = FALSE,
        overwrite=TRUE)

copy_to(con, d %>% select(1,1001:1435), 
        "TSDATA_1001_1435",
        temporary = FALSE,
        overwrite=TRUE)

dbDisconnect(con)


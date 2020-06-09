library(here)
library(dplyr)
library(tidyverse)

data.file <- "/data/20200406_full_append.csv"

d <- read_csv(paste0(here(),data.file)) %>% 
  janitor::clean_names()

head(d)

colnull <- colSums(is.na(d))

write.csv(colnull,paste0(here(),"/data/colnull.csv"),
          row.names = T)

readr::write_csv(probs,paste0(here(),"/data/probability_comparison.csv"))

#mutate(d$,g.el =  )
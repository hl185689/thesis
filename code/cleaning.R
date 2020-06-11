library(here)
library(dplyr)
library(tidyverse)

data.file <- "/data/20200406_full_append.csv"

d <- read_csv(paste0(here(),data.file)) %>% 
  janitor::clean_names()

# creating df with hand selected variables
var.file <- "/data/20200610_selected_vars.csv"

d2 <- read.csv(paste0(here(),var.file)) %>% 
  janitor::clean_names()

d <- d[,c(2,29,d2$varnum)] # adding in FID and CE_yn

colnames(d)

# clean chosen variables
# 1. go through each variable level 
# 2. make sure categorical are only 2 variables
# 3. mutate voter data into new variable

table(d[,45])

# d <- d %>% 
#   mutate(vb_voterbase_gender = if_else(
#     vb_voterbase_gender == "Male",
#   1,
#   0))


tmp <- data.frame(d[,37:70])
tmp

tmp[] <- ifelse(tmp == "A" | tmp == "Y", 1, 0)
tmp
table(tmp$vhsyn_vf_g2018_synthetic)

colnames(tmp)

tmp <- tmp %>% mutate(g.elec = rowSums(select(., starts_with("vhsyn_vf_g"))),
                      p.elec = rowSums(select(., starts_with("vhsyn_vf_p"))),
                      pp.elec = rowSums(select(., starts_with("vhsyn_vf_pp")))
)


d <- cbind(d,tmp[,35:37])
colnames(d)


#################################
#colnull <- colSums(is.na(d))

#write.csv(colnull,paste0(here(),"/data/colnull.csv"),
#          row.names = T)
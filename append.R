#preparing data for append
setwd("/Users/conor/Desktop/Hannah")

# hashed out code below merges TS and trims by field names  (1's)
################
# #load data
# d1 <- read.csv("TSDATA_1_500.csv")
# d2 <- read.csv("TSDATA_501_1000.csv")
# d3 <- read.csv("TSDATA_1001_1435.csv")

# # merge data
# d <- merge(d1,d2,by="voterbase_id")
# bigd <- merge(d,d3,by="voterbase_id")
#
#
# #Scores 1 / 0 keeping the 1's
# inout <- read.csv("/Users/conor/Desktop/Hannah/null_count_scored.csv")
# subinout <- inout[inout$in.out==1,]
# head(subinout)
# fn <- subinout$column # field names that we want (1's)
#
# names.bigd <- names(bigd) #column names of bigd
#
# table(fn %in% names.bigd) #all field names in bigD
# # leaves 431 columns / variables
#
# # which columns in bigd are in the field name file (fn)
# which(names.bigd %in% fn) # these are the columns to keep in addition to column 1 which is the voterbaseID value
# 
# trimmed.D <- bigd[,c(1,which(names.bigd %in% fn))]
# 
# trimmed.D[1:10,1:10] # take a look
# 
# # write out appended data to new file
# write.csv(trimmed.D,
#           file = "/Users/conor/Desktop/Hannah/TSdata_Trimmed.csv",
#           row.names = F)
################

ts.data <- read.csv("/Users/conor/Desktop/Hannah/TSdata_Trimmed.csv")

# append zip and zipcode + 4
tL <- list()
for(i in 1:length(ts.data$vb.tsmart_zip)){
  tL[[i]] <- ifelse(is.na(ts.data$vb.tsmart_zip4[i]), ts.data$vb.tsmart_zip,
                    paste(ts.data$vb.tsmart_zip[i], ts.data$vb.tsmart_zip4[i], sep = "-"))
}

tmpKey <- do.call(rbind, tL)

ts.data$Key <-paste(ts.data$vb.tsmart_first_name, ts.data$vb.tsmart_last_name , tmpKey, sep ="")

ts.data$Key <- as.character(ts.data$Key)

#### 
# Load cadastral data

mt.cad <- read.csv("/Users/conor/Desktop/Hannah/cadastral_clean.csv")

mt.cad$Key <- as.character(mt.cad$Key)

length(mt.cad$Key)
length(ts.data$Key)

table(ts.data$Key %in% mt.cad$Key)
table(mt.cad$Key %in% ts.data$Key)

length(mt.cad$Key) - length(unique(mt.cad$Key)) # cadastral duplicates make sense because some landowners have many properties
length(ts.data$Key) - length(unique(ts.data$Key))# duplicates here make less sense 


#creating separate data set of mt cad data that does not have match in ts.data
nomatch.cad <- mt.cad[!which(mt.cad$Key %in% ts.data$Key)==T,]


# trim mt.cad to the ts.data key - keeping only matches before appending
mt.cad <- mt.cad[which(mt.cad$Key %in% ts.data$Key),]
dim(mt.cad)


# trim ts.data down to only those rows with corresponding key in mt.cad
ts.data <- ts.data[which(ts.data$Key %in% mt.cad$Key),]

#same unique number of keys in both mt.cad and ts.data
length(unique(mt.cad$Key))
length(unique(ts.data$Key))

# testing merges when duplicates are present in cad
df1 <- mt.cad[mt.cad$Key == "DARRELLAABY59044-3515",]
df2 <- ts.data[ts.data$Key == "DARRELLAABY59044-3515",]
df3 <- merge(df1, df2, by = "Key")

# for duplicates in ts.data
df1 <- mt.cad[mt.cad$Key =="MICHAELDOUBEK59401-1558",]
df2 <- ts.data[ts.data$Key == "MICHAELDOUBEK59401-1558",]
df3 <- merge(df1, df2, by = "Key")
# this illustrates the probelm when we have one entry in cadastral
# and two entries in the ts.data - want to avoid pseudo-replication by 
# creating two data points for same landowner and property
# so solution is to take only one of the ts.data rows for corresponding mt.cad entry

ts.data <- ts.data[!duplicated(ts.data$Key),] #keeps frist record where duplicate occurs in Key columns 


############
full <- merge(mt.cad, ts.data, by = "Key")

full <- droplevels(full) #rids of ghost info see counties that have zero entries
table(full$vb.tsmart_county_name)

table(full$CE_yn)

length(unique(full[full$CE_yn == 1,]$Key))

#should be same dim as mt.cad
dim(mt.cad)
dim(full)

write.csv(full,
          "full_append.csv",
          row.names = F)


#######################
### playing around

df <- trimmed.D[trimmed.D$vb.tsmart_county_name %in% c("MISSOULA","GALLATIN","LAKE"),]

library(dplyr)

summarize(group_by(df, vb.tsmart_county_name),
          mean = mean(xpg.tt_show_me_the_money   , na.rm = T),
          se = sd(xpg.tt_show_me_the_money   , na.rm = T)/sqrt(n()))


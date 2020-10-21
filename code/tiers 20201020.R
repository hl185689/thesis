library(dplyr)

setwd("~/Documents/UM/Thesis/Chapter 2/Data")

alex <- read.csv(file = "Alex Step 20201020.csv")
john <- read.csv(file = "John XGBoost 20201020.csv")
#hannah <- read.csv(file = "Hannah Step 20201020.csv")
hannah2 <- read.csv(file = "hannah step 2.csv")
combined <- read.csv(file = "Combined 20201020.csv")

180933/1800

groupv <- sort(rep(seq(1, 101), 1800))[1:180933]
unique(groupv)

alex$group <- groupv
john$group <- groupv
#hannah$group <- groupv
hannah2$group <- groupv
combined$group <- groupv

# base R
#data.frame(table(alex$group,alex$CE_yn))

# dplyr
alexoutput <- alex[alex$CE_yn==1,] %>% group_by(group) %>% summarize(count = n())
johnoutput <- john[john$CE_yn==1,] %>% group_by(group) %>% summarize(count = n())
#hannahoutput<- hannah[hannah$CE_yn==1,] %>% group_by(group) %>% summarize(count = n())
hannahoutput2 <- hannah2[hannah2$CE_yn==1,] %>% group_by(group) %>% summarize(count = n())
combinedoutput <- combined[combined$CE_yn==1,] %>% group_by(group) %>% summarize(count = n())

head(alexoutput)

alexoutput$model <- "alex"
johnoutput$model <- "john"
#hannahoutput$model <- "hannah"
hannahoutput2$model <- "hannah2"
combinedoutput$model <- "combined"

allmodel <- rbind(alexoutput,johnoutput,hannahoutput2,combinedoutput)

library(ggplot2)

ggplot(allmodel, aes(x = group, y = count, fill = model)) +
  geom_bar(stat = "identity")

ggplot(allmodel, aes(x = group, y = count, fill = model)) +
  geom_bar(stat = "identity") + facet_wrap(~model)

range(hannah[hannah$group==42,]$probability)

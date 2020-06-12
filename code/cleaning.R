library(here)
library(dplyr)
library(tidyverse)

data.file <- "/data/20200406_full_append.csv"

d <- read_csv(paste0(here(),data.file)) %>% 
  janitor::clean_names()

colnames(d)
# creating df with hand selected variables
var.file <- "/data/20200610_selected_vars.csv"

d2 <- read.csv(paste0(here(),var.file)) %>% 
  janitor::clean_names()

d <- d[,c(2,29,d2$varnum)] # adding in FID and CE_yn

colnames(d)

# reducing to complete cases
d <- d[complete.cases(d),]

##########################
# clean chosen variables #
##########################
# 1. go through each variable level 
# 2. make sure categorical are only 2 variables
# 3. mutate voter data into new variable

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

summary(d)

###############
# build model #
###############

length(which(d$sum_total_v == 0)) # 1 property has a value of $0

d <- d[-which(d$sum_total_v == 0),]
colnames(d)

# need to log10 transform land columns and creating new columns
d$loggisacr <- log10(d$sum_gis_acr)
d$logsumcontin <- log10(d$sum_contin)
d$logsumfallow <- log10(d$sum_fallow)
d$logsumfarm <- log10(d$sum_farmsi)
d$logsumforest <- log10(d$sum_forest)
d$logsumgrazin <- log10(d$sum_grazin)
d$logsumwildhay <- log10(d$sum_wild_ha)
d$logsumirriga <- log10(d$sum_irriga)
d$logsumtotala <- log10(d$sum_total_a)
d$logsumtotalb <- log10(d$sum_total_b)
d$logsumtotall <- log10(d$sum_total_l)
d$logsumtotalV <- log10(d$sum_total_v)

#CE total
table(d$ce_yn)


###########
# my code #
###########
#splitting data for sampling
ce.y <- d[d$ce_yn==1,]

ce.n <- d[d$ce_yn==0,]

#Randomly pull out 50-100 CE and 500-1000 NCE for your test set.
set.seed(20200525)

sam.cey <- sample(nrow(ce.y), 100)

test.cey <- ce.y[sam.cey, ]

sam.cen <- sample(nrow(ce.n), 1000)

test.cen <- ce.n[sam.cen, ]

test.set <- rbind(test.cey,test.cen)

nrow(test.set)

#Remaining CE for the training set.

train.cey <- ce.y[-sam.cey, ]

nrow(train.cey)

#Randomly pull 10x NCE for the remainder of the training set.

sec.sam.cen <- ce.n[-sam.cen, ]
nrow(sec.sam.cen)

sam.train <- sample(nrow(sec.sam.cen), 3250) #10x total number of CEs remaining

train.cen <- ce.n[sam.train, ]

train.set <- rbind(train.cey, train.cen)

#Fit model on the training set. 

table(train.set$ce_yn)

names(train.set)

# run model on new log variables and combined election scores
#test run 1 variable
####
### FIX THIS TO INCLUDE ALL CHOSEN VARIABLES!!!###

m1 <- glm(ce_yn ~ train.set[,87], data=train.set, family = "binomial")
summary(m1)
mnull <- glm(ce_yn ~ 1, data=train.set, family = "binomial") #null model, no predictors 
summary(mnull)

# likelihood ratio test (sig p value indicates improved model fit with included variable)
library(lmtest)
lrtest(mnull,m1)

models <- list()
lrtests <- list()

for(i in 5:dim(train.set)[2]){ # beginning loop on column five of train.set to end of columns
  
  models[[i]] <- glm(CE_yn ~ train.set[,i], data=train.set, family = "binomial")
  
  
  lrtests[[i]] <-  lrtest(mnull, models[[i]]) #lr tests of all univariate models against the null (does adding variable x improve model fit?)
}


m.global <- glm(CE_yn ~ train.set$vb.voterbase_age + train.set$vb.voterbase_registration_status + 
                  train.set$vb.voterbase_gender + train.set$vb.voterbase_marital_status +
                  train.set$xpg.length_of_residence + train.set$ts.tsmart_children_present_score +
                  train.set$ts.tsmart_college_graduate_score + train.set$ts.tsmart_high_school_only_score +
                  train.set$ts.tsmart_income_rank_score + train.set$ts.tsmart_partisan_score +
                  train.set$logGISAcr + train.set$logTotalV, data=train.set, family = "binomial")
hist(m.global$fitted.values)

library(car)
?vif
# to limit collinearity!
# vif values above 3 is beyond the threshold (take out variable about 3, run again, continue until all below 3)

vif(m.global)

train.global <- glm(CE_yn ~ train.set$vb.voterbase_age + train.set$vb.voterbase_registration_status + 
                      train.set$vb.voterbase_gender + train.set$vb.voterbase_marital_status +
                      train.set$xpg.length_of_residence +
                      train.set$ts.tsmart_college_graduate_score +
                      train.set$ts.tsmart_income_rank_score + train.set$ts.tsmart_partisan_score +
                      train.set$logGISAcr + train.set$logTotalV, data=train.set, family = "binomial",na.action = na.fail)

vif(train.global)
# removing high school (7)
# age and children present correlated, removing child present to see VIF score change
#  plot(train.set$vb.voterbase_age,train.set$ts.tsmart_children_present_score)

library(MASS)

?stepAIC
train.AIC <- stepAIC(train.global, direction = "backward")

# library(MuMIn)
# compare all combinations of variables
# dredge(train.global)



# from top backwards step model
train.final <- glm(CE_yn ~ train.set$vb.voterbase_age + train.set$vb.voterbase_gender + 
                     train.set$ts.tsmart_college_graduate_score + train.set$ts.tsmart_income_rank_score + 
                     train.set$ts.tsmart_partisan_score + train.set$logGISAcr, data=train.set, family = "binomial")
summary(train.final)

# intercept is reference level of categories in all models (this model is FEMALE)


# for plots adding fitted values to train data set
train.set$fittedvals <- train.final$fitted.values

plot(train.set$logGISAcr, train.set$fittedvals)

hist(train.set$fittedvals)

hist(train.set[train.set$CE_yn == 1, ]$fittedvals, main = "Histogram of the Probability of being a CE if in the train.set they are a CE", xlab="Probability (predicted values)")

hist(train.set[train.set$CE_yn == 0, ]$fittedvals, main = "Histogram of the Probability of being a CE if in the train.set they are NOT a CE", xlab="Probability (predicted values)")

# run on test
test.final <- glm(CE_yn ~ test.set$vb.voterbase_age + test.set$vb.voterbase_gender + 
                    test.set$ts.tsmart_college_graduate_score + test.set$ts.tsmart_income_rank_score + 
                    test.set$ts.tsmart_partisan_score + test.set$logGISAcr, data=test.set, family = "binomial")

summary(test.final)

test.set$fittedvals <- test.final$fitted.values


hist(test.set[test.set$CE_yn == 1, ]$fittedvals)


#################################
#colnull <- colSums(is.na(d))

#write.csv(colnull,paste0(here(),"/data/colnull.csv"),
#          row.names = T)
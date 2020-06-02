library(dplyr)
library(ggplot2)
library(scales)
library(broom)
library(readr)
library(lmtest)
library(car)

setwd("\\Users\\conor\\Desktop\\Hannah\\Data\\")
df <- read.csv("20200403_full_append.csv")

# 180933 landowners

# reducing to complete cases
df <- df[complete.cases(df),]
# 106403 landowners


length(which(df$SUM_TotalV == 0)) # 1 property has a value of $0

df <- df[-which(df$SUM_TotalV == 0),]

# need to log10 transform columns 3:4 and creating new columns
df$logGISAcr <- log10(df$SUM_GISAcr)
df$logTotalV <- log10(df$SUM_TotalV)

#row total
nrow(df)

#CE total
table(df$CE_yn)

# NCE = 106116, CE = 286


#splitting data for sampling
ce.y <- df[df$CE_yn==1,]

ce.n <- df[df$CE_yn==0,]

# nrow(ce.y)

#Randomly pull out 50-100 CE and 500-1000 NCE for your test set.
set.seed(50)


sam.cey <- sample(nrow(ce.y), 75)

test.cey <- ce.y[sam.cey, ]

sam.cen <- sample(nrow(ce.n), 750)

test.cen <- ce.n[sam.cen, ]

test.set <- rbind(test.cey,test.cen)

nrow(test.set)

#Remaining CE for the training set.

train.cey <- ce.y[-sam.cey, ]

nrow(train.cey)

#Randomly pull 10x NCE for the remainder of the training set.

sec.sam.cen <- ce.n[-sam.cen, ]
nrow(sec.sam.cen)

nrow(sec.sam.cen)*10

sam.train <- sample(nrow(sec.sam.cen), 1053660, replace = TRUE) 

train.cen <- ce.n[sam.train, ]

train.set <- rbind(train.cey, train.cen)

#Fit model on the training set. 

table(train.set$CE_yn)

names(train.set) # going to be running models on columns 30:end for new log10 variables 


#test run 1 variable
m1 <- glm(CE_yn ~ train.set[,30], data=train.set, family = "binomial")
summary(m1)

######### running into memory error here: 
# Error: cannot allocate vector of size 63.5 Gb


mnull <- glm(CE_yn ~ 1, data=train.set, family = "binomial") #null model, no predictors 
summary(mnull)

# likelihood ratio test (sig p value indicates improved model fit with included variable)
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
par(mfrow=c(1,1))
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


# When you're done with model fitting, Evaluate model on test set. 
# I think this is your best measure of model accuracy.


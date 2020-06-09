library(here)
library(xgboost)
library(tidyverse)

data.file <- "/data/20200406_full_append.csv"
logit.vars.file <- "/data/logit_variables.csv"

d <- read_csv(paste0(here(),data.file)) %>% 
  janitor::clean_names()

logit.vars <- read_csv(paste0(here(),logit.vars.file)) %>% 
  mutate(clean_var = janitor::make_clean_names(varname))

# Hmisc::describe(d)

simple.glm <- glm(ce_yn ~ scale(vb_voterbase_age) + 
                    scale(vb_voterbase_general_votes) +
                    scale(xpg_homeowner_probability_model) + 
                    scale(ts_tsmart_college_graduate_score),
                  data=d,
                  family="binomial")
# Lots of missing values for second and third

summary(simple.glm)
anova(simple.glm)


# Okay, let's figure out which columns to do xgboost on.
# We'll pass in an `xgb.DMatrix` object because that seems
# to be the one that both the test and train functions are 
# happiest with. (After 45 seconds of research.) This means
# we need everything numeric. For a first pass, we'll just look at
# numeric columns that have multiple values. If the number of 
# values is < 10, then we'll require the median frequency
# value to have at least 10% representation to avoid the 
# 99% single value variables. Any columns with
# more than 75% missing values isn't going to get included. 

potential.vars.to.include <- c("fid","ce_yn")

for(i in 1:ncol(d)){
  this.col <- names(d)[i]

  if(is.numeric(d[,this.col] %>% pull)){ # weird `pull` to extract col
    uni.vals <- unique(d[,this.col])
    
    frac.missing <- mean(is.na(d[,this.col] %>% pull))
    
    if(frac.missing > 0.75 || grepl("id",this.col)){
      next
    }
    
    if(nrow(uni.vals) > 10){
      potential.vars.to.include <- c(potential.vars.to.include,
                                     this.col)
      
    } else {
      value.dist <- table(d[,this.col])
      value.dist <- value.dist/sum(value.dist) # Normalize table
      if (median(value.dist) > 0.1){
        potential.vars.to.include <- c(potential.vars.to.include,
                                       this.col)
        
      }
    }
    
  }
}

# Get rid of the seed col duplicates
potential.vars.to.include <- unique(potential.vars.to.include)
vars.to.exclude <- c("sum_easeme",
                     "sum_total_a",
                     "sum_gis_acr",
                     "vb_tsmart_zip")

vars.to.include <- potential.vars.to.include[!(potential.vars.to.include %in% vars.to.exclude)]

## Now do some XGBoost. First, get the data ready.
# Steps
# 1. Sample d to balance CE Yes and No better.
# 2. Get test and training indices.
# 3. Make lists for xgboost function. 
set.seed(20200525)

sample.idx <- which(d$ce_yn==1)
sample.idx <- c(sample.idx,
                sample((1:nrow(d))[d$ce_yn==0],
                       size=9*length(sample.idx),
                       replace = F))

use.hl.vars <- F

if(!use.hl.vars){
  small.d <- d[sample.idx,vars.to.include]
} else {
  small.d <- d[sample.idx,c(logit.vars %>% pull(clean_var),
                            "ce_yn","fid")]
  small.d <- small.d %>% 
    mutate(vb_voterbase_registration_status = if_else(
      vb_voterbase_registration_status == "Registered",
      1,
      0),
      vb_voterbase_gender = if_else(
        vb_voterbase_gender == "Male",
      1,
      0),
      vb_voterbase_marital_status = if_else(
        vb_voterbase_marital_status == "Married",
        1,
        0)) %>% 
    rename(vb_voterbase_male = vb_voterbase_gender,
           vb_voterbase_married = vb_voterbase_marital_status)
  
}

# Let's split into test and training sets. Response has 468 rout of 181K are yes
# (0.3%), so let's make a new data set with 9 No for every yes. 

# This will define our "small" data set. Now split into 
# test and training, with 10% in test. 
test.idx <- sample(nrow(small.d),size=ceiling(nrow(small.d)*0.1),repl=F)
train.idx <- (1:nrow(small.d))[!((1:nrow(small.d)) %in% test.idx)]

data.train.small <- small.d %>% 
  select(-ce_yn,-fid) %>%
  slice(train.idx) %>% 
  data.frame

data.train.small <- xgb.DMatrix(as.matrix(data.train.small),
                                label=as.numeric(small.d %>% slice(train.idx) %>% pull(ce_yn)))


data.test.small <- small.d %>% 
  select(-ce_yn,-fid) %>% 
  slice(test.idx) %>% 
  data.frame

test.resp <- as.numeric(small.d %>% slice(test.idx) %>% pull(ce_yn))

data.test.small <- xgb.DMatrix(as.matrix(data.test.small),
                               label=test.resp)

# Whew, that took a while. Now modeling.

xgb.1 <- xgboost(
  data=data.train.small,
  params=list(eta=0.1,
              max_depth=4),
  nrounds=3,
  objective = "binary:logistic"
)

xgb.plot.importance(xgb.importance(model=xgb.1))

pred <- predict(xgb.1,data.test.small)
err <- mean(as.numeric(pred > 0.5) != test.resp) 
print(paste("test-error=", err))
# Not amazing performance. Worse than guessing "no" 


table(pred,small.d %>% slice(test.idx) %>% pull(ce_yn))

# trying for some plotting
imp_mat <- xgb.importance(colnames(data.train.small),
                          model=xgb.1)
xgb.plot.importance(imp_mat)

#######################################################################
## Let's see if we can figure out where the issue is with our test.  ##
#######################################################################

# Let's do groups of 20 covariates and see if 
# we can figure out why we're getting perfect fit with Hannah's

col.groups <- sample.int(10,size=ncol(small.d),replace=F)

test.idx <- sample(nrow(small.d),size=ceiling(nrow(small.d)*0.1),repl=F)
train.idx <- (1:nrow(small.d))[!((1:nrow(small.d)) %in% test.idx)]

data.train.small <- small.d %>% 
  select(-ce_yn,-fid) %>%
  slice(train.idx) %>% 
  data.frame

data.train.small <- xgb.DMatrix(as.matrix(data.train.small),
                                label=as.numeric(small.d %>% slice(train.idx) %>% pull(ce_yn)))


data.test.small <- small.d %>% 
  select(-ce_yn,-fid) %>% 
  slice(test.idx) %>% 
  data.frame

test.resp <- as.numeric(small.d %>% slice(test.idx) %>% pull(ce_yn))

data.test.small <- xgb.DMatrix(as.matrix(data.test.small),
                               label=test.resp)


# A version of XGBoost *without* the "size of parcel" variables, 
# just to see what pops. 

library(here)
library(xgboost)
library(tidyverse)
library(scales)

data.file <- "/data/20200406_full_append.csv"

d <- read_csv(paste0(here(),data.file)) %>% 
  janitor::clean_names()

# First, build a list of variables to include. 
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

# For the "best possible model, I'll exclude just the obvious ones
vars.to.exclude <- c("sum_easeme",
                     "vb_tsmart_zip",
                     "vb_tsmart_zip4",
                     "sum_total_a",
                     "sum_gis_acr",
                     "sum_grazin",
                     "sum_forest",
                     "sum_non_qua",
                     "sum_farmsi",
                     "sum_total_b",
                     "sum_total_v",
                     "sum_total_l",                     
#                     "ts_tsmart_urbanicity_rank",
                     "sum_irriga",
                     "sum_wild_ha",
                     "sum_fallow")

vars.to.include <- potential.vars.to.include[!(potential.vars.to.include %in% vars.to.exclude)]

## Now do some XGBoost. First, get the data ready.
# Steps
# 1. Sample d to balance CE Yes and No better.
# 2. Get test and training indices.
# 3. Make lists for xgboost function. 
set.seed(20200609)

sample.idx <- which(d$ce_yn==1)
sample.idx <- c(sample.idx,
                sample((1:nrow(d))[d$ce_yn==0],
                       size=9*length(sample.idx),
                       replace = F))

small.d <- d[sample.idx,vars.to.include]

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

xgb.1 <- xgboost(
  data=data.train.small,
  params=list(eta=0.1,
              max_depth=4),
  nrounds=3,
  objective = "binary:logistic"
)

xgb.plot.importance(xgb.importance(model=xgb.1))

# Let's take this model and attempt to score all the FIDs
score.d <- d[,vars.to.include]

score.d <- score.d %>% 
  select(-fid,-ce_yn) %>%
  data.frame

score.d <- as.matrix(score.d)
names(score.d) <- vars.to.include[!(vars.to.include %in% c("fid","ce_yn"))]

preds <- predict(xgb.1,newdata=score.d)

d <- d %>% 
  mutate(full_xgb_pred = preds)

# Let's measure F1 score with various cutoffs.
get.rule.performance <- function(data,cutoff=0.5){
  tp <- sum(data$ce_yn==1 & data$full_xgb_pred >= cutoff)
  fp <- sum(data$ce_yn==0 & data$full_xgb_pred >= cutoff)
  
  tn <- sum(data$ce_yn==0 & data$full_xgb_pred < cutoff)
  fn <- sum(data$ce_yn==1 & data$full_xgb_pred < cutoff) 
  
  precision <- tp/(tp+fp)
  recall <- tp/(tp+fn)
  
  f1 <- 2 * precision * recall / (precision + recall)
  
  return(list("precision" = precision,
              "recall"    = recall,
              "f1"        = f1))
  
}

get.rule.performance(d)

for.plot <- tibble(cutoffs=seq(0.35,0.65,length=20),
                   prec=0.0,
                   recall=0.0,
                   f1=0.0)

for(i in 1:nrow(for.plot)){
  holder <- get.rule.performance(d,cutoff=for.plot$cutoffs[i])
  
  for.plot$prec[i] <- holder$precision
  for.plot$recall[i] <- holder$recall
  for.plot$f1[i] <- holder$f1

}

ggplot(for.plot,
       aes(x=cutoffs,y=f1)) + 
  geom_point() + 
  theme_minimal() + 
  labs(x="Cutoff for Easement Probability",
       y="F1 Score") + 
  scale_x_continuous(label=percent) + 
  scale_y_continuous(label=percent) + 
  geom_vline(xintercept=0.539) + 
  geom_label(aes(x=0.575,y=0.125,label=paste0("Best F1: 54%")))





# Now copy into our probability list. 
alex.list.file <- "/data/probability_comparison_alex.csv"

probs <- readr::read_csv(file=paste0(here(),alex.list.file)) %>% 
  janitor::clean_names()

probs <- probs %>% 
  left_join(d %>% 
              select(fid,
                     full_xgb_pred),
            by="fid")

readr::write_csv(probs,paste0(here(),"/data/probability_comparison.csv"))

ggplot(probs,
       aes(x=step_wise_estimate,y=full_xgb_pred)) + 
  geom_point(alpha=0.05) + 
  stat_smooth() + 
  labs(x="Stepwise Logistic Regression",
       y="XGBoost Probabilities") + 
  theme_minimal()

cor(probs[,2],probs[,3])

######################################################################################

vars.to.exclude <- c("sum_easeme",
                     "sum_total_a",
                     "sum_gis_acr",
                     "vb_tsmart_zip")


# Testing
pred <- predict(xgb.1,data.test.small)
err <- mean(as.numeric(pred > 0.5) != test.resp) 
print(paste("test-error=", err))
# Not amazing performance. Worse than guessing "no" 

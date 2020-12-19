library(here)
library(tidyverse)
library(tidymodels)
library(scales)
library(broom)
library(readr)
library(lmtest)
library(forcats)

df <-read_csv(paste0(here(),"/data/clean dataset 20200824.csv"))


# Final model

final.md <- df %>% 
  select(CE_yn, 
         logGISAcr, 
         SUM_Fallow, 
         xpg.truetouch_online_mid_high, 
         ts.tsmart_activist_score, 
         ts.tsmart_trump_support_score,
         ts.tsmart_urbanicity_rank,
         SUM_Grazin,
         xpg.donor_contributes_to_political_charities,
         ts.tsmart_labor_union_support_score,
         xpg.hobbies_gardening,
         gsyn.synth_county_sum_fec_contribution_count_democrat)
mean(complete.cases(final.md))


m.final <-glm(CE_yn ~ logGISAcr 
              + SUM_Fallow
              + xpg.truetouch_online_mid_high
              + ts.tsmart_activist_score
              + ts.tsmart_trump_support_score
              + ts.tsmart_urbanicity_rank
              + SUM_Grazin
              + xpg.donor_contributes_to_political_charities
              + ts.tsmart_labor_union_support_score
              + xpg.hobbies_gardening
              + gsyn.synth_county_sum_fec_contribution_count_democrat, 
              data=final.md,
              family = "binomial") 

anova(m.final,test="Chisq")
summary(m.final)
fit.final <- tidy(m.final)

# JC: let's get the ROC curve and do some resampling to test the 
# model. This new version of tidymodels just came out, so I'm
# going to test it here. Here's the reference that 
# I'm using: https://www.tidymodels.org/start/resampling/

set.seed(20201209)
parcel.split <- initial_split(df, strat=CE_yn)

parcel.train <- training(parcel.split)
parcel.test <- testing(parcel.split)

nrow(parcel.train)
nrow(parcel.train)/nrow(df)

nrow(parcel.test)
nrow(parcel.test)/nrow(df)

# strata handles even splitting on CE_yn. Doesn't have the 
# overweighting from our original data, though. 
parcel.train %>% 
  count(CE_yn) %>% 
  mutate(prop = n/sum(n))

parcel.test %>% 
  count(CE_yn) %>% 
  mutate(prop = n/sum(n))

m.final <-glm(CE_yn ~ logGISAcr 
              + SUM_Fallow
              + xpg.truetouch_online_mid_high
              + ts.tsmart_activist_score
              + ts.tsmart_trump_support_score
              + ts.tsmart_urbanicity_rank
              + SUM_Grazin
              + xpg.donor_contributes_to_political_charities
              + ts.tsmart_labor_union_support_score
              + xpg.hobbies_gardening
              + gsyn.synth_county_sum_fec_contribution_count_democrat, 
              data=parcel.train, 
              family = "binomial") 

glm.test.pred <- 
  tibble(pred=predict(m.final, parcel.test,type="response")) %>% 
  bind_cols(parcel.test %>% 
              select(CE_yn) %>% 
              mutate(CE_yn = factor(CE_yn,
                                    levels=c(1,0), # ROC expects positive as first level
                                    labels=c("CE","No CE"))))

glm.test.pred %>% 
  roc_auc(truth=CE_yn,pred)


roc_curve(glm.test.pred,estimate = pred,truth=CE_yn) %>% autoplot()

####

fit.final <- fit.final %>% 
  mutate(exp_est = exp(estimate),
         lb = exp(estimate - 2*std.error),
         ub = exp(estimate + 2*std.error))

ggplot(fit.final,
       aes(x=exp_est,y=term)) +
  geom_point() + 
  geom_errorbarh(aes(xmin=lb,xmax=ub,y=term),height=0.1) + 
  theme_gray() + 
  labs(x="Odds Multiplier",y="Model Term") 


ggplot(fit.final %>% 
         filter(term!="logGISAcr",term!="(Intercept)"),
       aes(x=exp_est,y=term)) +
  geom_point() + 
  geom_errorbarh(aes(xmin=lb,xmax=ub,y=term),height=0.1) + 
  theme_gray() + 
  labs(x="Odds Multiplier",y="Model Term") + 
  geom_vline(xintercept = 1,col="gray70")


#####################
# apply on test set #
#####################

test.final <-glm(CE_yn ~ logGISAcr 
                 + SUM_Fallow
                 + xpg.truetouch_online_mid_high
                 + ts.tsmart_activist_score
                 + ts.tsmart_trump_support_score
                 + ts.tsmart_urbanicity_rank
                 + SUM_Grazin
                 + xpg.donor_contributes_to_political_charities
                 + ts.tsmart_labor_union_support_score
                 + xpg.hobbies_gardening
                 + gsyn.synth_county_sum_fec_contribution_count_democrat, 
                 data=test.set,
                 family = "binomial")

summary(test.final)

test.set$fittedvals <- test.final$fitted.values


hist(test.set[test.set$CE_yn == 1, ]$fittedvals) # for ones that are truely ones, 
# what does the model say the probability 
# that it actually is

####################
# apply to full df #
####################

full.final <- glm(CE_yn ~ logGISAcr 
                  + SUM_Fallow
                  + xpg.truetouch_online_mid_high
                  + ts.tsmart_activist_score
                  + ts.tsmart_trump_support_score
                  + ts.tsmart_urbanicity_rank
                  + SUM_Grazin
                  + xpg.donor_contributes_to_political_charities
                  + ts.tsmart_labor_union_support_score
                  + xpg.hobbies_gardening
                  + gsyn.synth_county_sum_fec_contribution_count_democrat, 
                  data = df,
                  family = "binomial")


for.desc <- df %>% 
  select(logGISAcr,
         SUM_Fallow,
         xpg.truetouch_online_mid_high,
         ts.tsmart_activist_score,
         ts.tsmart_trump_support_score,
         ts.tsmart_urbanicity_rank,
         SUM_Grazin,
         xpg.donor_contributes_to_political_charities,
         ts.tsmart_labor_union_support_score,
         xpg.hobbies_gardening,
         gsyn.synth_county_sum_fec_contribution_count_democrat)

Hmisc::describe(for.desc)

summary(full.final)

df$fittedvals <- full.final$fitted.values


hist(df[df$CE_yn == 1, ]$fittedvals)

#saving fitted values by FID
hannah.probs <- df[,c("FID", "fittedvals")]

head(hannah.probs)

write.csv(hannah.probs, file = "C:/Users/conor/Desktop/thesis/data/hannahprobs.csv", row.names = FALSE)

#merging files... this is a mess because I could not get the neat left_join code to work

ja.probs <-read.csv("C:/Users/conor/Desktop/thesis/data/probability_comparison.csv")
h.probs <-read.csv("C:/Users/conor/Desktop/thesis/data/hannahprobs.csv")
ce.actual <-read.csv("C:/Users/conor/Desktop/thesis/data/ce FID 20201218.csv")

probs.mer <- merge(ja.probs,h.probs, by = "FID", all = TRUE)

write.csv(probs.mer, file = "C:/Users/conor/Desktop/thesis/data/probability_comparison_merge_JAH.csv", row.names = FALSE)

full.probs <-read.csv("C:/Users/conor/Desktop/thesis/data/probability_comparison_merge_JAH.csv")

probs.mer.act <- merge(full.probs,ce.actual, by = "FID", all = TRUE)

write.csv(probs.mer.act, file = "C:/Users/conor/Desktop/thesis/data/probability_comparison_ALL.csv", row.names = FALSE)




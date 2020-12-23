# JC: doing a clean one of these to see if I can uncover what went wrong in the merge. 

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
  select(FID, # JC: adding this was necessary for the merge. 
         CE_yn, 
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


#merging files... this is a mess because I could not get the neat left_join code to work
# JC: I think the weirdness in the ROC curve came from the merge issues. Also, 
# if you'd use 'here()' on this it'd make life easier....

final.md <- final.md %>% 
  mutate(hand_tuned = predict(m.final,newdata=final.md,type="response")) %>% 
  rename(fid=FID,
         ce_yn=CE_yn)

ja.probs <- read_csv(paste0(here(),"/data/probability_comparison.csv"))
full.ce <- read_csv(paste0(here(),"/data/ce FID 20201222.csv"))

for.output <- ja.probs %>% 
  left_join(final.md %>% 
              select(fid,
                     hand_tuned),
            by="fid")

for.output <- for.output %>% 
  left_join(full.ce %>% 
              select(fid,
                     ce_yn),
            by="fid")

# for.output <- for.output %>% 
#   mutate(hand_tuned = if_else(is.na(hand_tuned),
#                               0.5*step_wise_estimate + 0.5*full_xgb_pred,
#                               hand_tuned))




Hmisc::describe(for.output)

write_csv(for.output,
          paste0(here(),"/data/probability_comparison_all_na_hl.csv"))

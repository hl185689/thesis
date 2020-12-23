# Builds ROC curves for our three models and estimates AUC (if I figure that out)
# Input is data file Hannah has put together, probability_comparison_all_no_na.csv

library(tidyverse)
library(scales)
library(here)
library(pROC)

d <- read_csv(paste0(here(),"/data/probability_comparison_all_na_hl.csv")) 

d <- d %>% 
  mutate(combined = (step_wise_estimate + full_xgb_pred + hand_tuned)/3)

# Build the rocs one at a time, then combine them for the final plot
step.roc <- roc(data=d,
                response=ce_yn,
                predictor=step_wise_estimate)

xgb.roc <- roc(data=d,
               response=ce_yn,
               predictor=full_xgb_pred)

hand.roc <- roc(data=d,
                   response=ce_yn,
                   predictor=hand_tuned)

# hand.roc.na <- roc(data=d,
#                 response=ce_yn,
#                 predictor=hand_tuned_na)

combined.roc <- roc(data=d,
                    response=ce_yn,
                    predictor=combined)

# Just for grins
par(mfrow=c(2,2))
plot(step.roc,main="Stepwise",print.auc=T)
plot(xgb.roc,main="XG Boost",print.auc=T)
plot(hand.roc,main="Hand-Tuned",print.auc=T)
plot(combined.roc,main="Combined",print.auc=T)

par(mfrow=c(1,1))

for.plot <- tibble(
  model="Stepwise",
  sensitivities = step.roc$sensitivities,
  specificities = step.roc$specificities
)

for.plot <- for.plot %>% 
  bind_rows(tibble(
    model="XG Boost",
    sensitivities = xgb.roc$sensitivities,
    specificities = xgb.roc$specificities)
    ) %>% 
  bind_rows(tibble(
    model="Hand-Tuned",
    sensitivities = hand.roc$sensitivities,
    specificities = hand.roc$specificities)
  ) %>% 
  bind_rows(tibble(
    model="Ensemble",
    sensitivities = combined.roc$sensitivities,
    specificities = combined.roc$specificities)
  ) 
  
for.plot <- for.plot %>% 
  mutate(model = fct_relevel(model,
                             "Stepwise",
                             "Hand-Tuned",
                             "XG Boost",
                             "Ensemble"))

caption.text <- 
  paste("Areas Under the Curve",
        paste("Stepwise",round(step.roc$auc,3),sep=": "),
        paste("XG Boost",round(xgb.roc$auc,3),sep=": "),
        paste("Hand-Tuned",round(hand.roc$auc,3),sep=": "),
        paste("Ensemble",round(combined.roc$auc,3),sep=": "),
        sep="\n")

ggplot(for.plot,
       aes(x=1-specificities,y=sensitivities,group=model,color=model)) + 
  geom_line(size=1) + 
  theme_minimal() + 
  geom_abline(aes(slope=1,intercept=0)) + 
  labs(x="1 - Specificity",
       y="Sensitivity",
       title="Comparison of Models",
       color="Model",
       caption=caption.text)



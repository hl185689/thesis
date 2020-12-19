# JC: For some of this code to work you're going to
# want to double click on the .Rproj file when you start work. Then 
# all the `here` stuff will work.

library(dplyr)
library(here)
library(tidyverse)
library(readr)
library(scales)


alex <- read_csv(paste0(here(),"/data/Alex Step 20201020.csv"))
john <- read_csv(paste0(here(),"/data/John XGBoost 20201020.csv"))
#HL probabilities with missing values substituted with the averages from JC and AM
hannah.wona <- read_csv(paste0(here(),"/data/hannah probs wo NA.csv"))
#HL probabilities WITH missing values
hannah.wna <- read_csv(paste0(here(),"/data/hannah probs w NA.csv"))
combined.wona <- read_csv(paste0(here(),"/data/combined prob wo NA.csv"))


n.groups <- 100

alex <- alex %>% 
  mutate(prob_group = cut(probability,
                          breaks=quantile(probability,
                                          probs=seq(0,1,length.out = n.groups))))

john <- john %>% 
  mutate(prob_group = cut(probability,
                          breaks=unique(quantile(probability,
                                          probs=seq(0,1,length.out = n.groups)))))

hannah.wna <- hannah.wna %>% 
  mutate(prob_group = cut(probability,
                          breaks=quantile(probability,
                                          probs=seq(0,1,length.out = n.groups),
                                          na.rm=T)))

hannah.wona <- hannah.wona %>% 
  mutate(prob_group = cut(probability,
                          breaks=unique(quantile(probability, #was getting an error so had to add unique()
                                          probs=seq(0,1,length.out = n.groups),
                                          na.rm=T))))

combined.wona <- combined %>% 
  mutate(prob_group = cut(probability,
                          breaks=quantile(probability,
                                          probs=seq(0,1,length.out = n.groups))))

for.plot <- alex %>% 
  group_by(prob_group) %>% 
  summarize(mean_prob = mean(probability,na.rm=T),
            actual_frac = mean(CE_yn,na.rm=T)) %>% 
  mutate(model="Stepwise Logistic")

for.plot <- for.plot %>% 
  bind_rows(john %>%
              group_by(prob_group) %>% 
              summarize(mean_prob = mean(probability,na.rm=T),
                        actual_frac = mean(CE_yn,na.rm=T)) %>% 
              mutate(model="XGBoost"))

for.plot <- for.plot %>% 
  bind_rows(hannah.wona %>%
              group_by(prob_group) %>% 
              summarize(mean_prob = mean(probability,na.rm=T),
                        actual_frac = mean(ce_yn,na.rm=T)) %>% 
              mutate(model="Hand-tuned Logistic"))

for.plot <- for.plot %>% 
  bind_rows(combined.wona %>%
              group_by(prob_group) %>% 
              summarize(mean_prob = mean(probability,na.rm=T),
                        actual_frac = mean(CE_yn,na.rm=T)) %>% 
              mutate(model="Combined"))



ggplot(for.plot,
       aes(x=mean_prob,y=actual_frac)) + 
  geom_point() +
  stat_smooth(se=F) + 
#  stat_smooth(method="lm",se=F,color="red") + 
  theme_gray() + 
  facet_wrap(~model) + 
  labs(x="Mean Estimated Probability in Group",
       y="Actual Easement Fraction in Group",
       title="Comparison of Estimated Probabilites\nTo Actual Easement Fractions") + 
  scale_x_continuous(label=percent) + 
  scale_y_continuous(label=percent) 


# Let's take a look at parcel-level agreement on probabilities. 

for.pairs <- alex %>% 
  select(FID,probability) %>% 
  rename(alex_prob = probability) %>% 
  left_join(john %>% 
              select(FID, probability) %>% 
              rename(john_prob=probability),
            by="FID") %>% 
  left_join(hannah.wona %>% 
              select(FID, probability) %>% 
              rename(hannah_prob=probability),
            by="FID")

car::spm(for.pairs %>% select(-FID) %>% slice_sample(prop=0.05))

cor(for.pairs %>% select(-FID),use="pairwise.complete.obs")




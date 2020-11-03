# JC: Going to do some mods here to make your code a bit
# easier to work with and read. It definitely appears you're not making
# full use of the R Project structure I set up a while back, so I'm 
# cutting over to that. For some of this code to work you're going to
# want to double click on the .Rproj file when you start work. Then 
# all the `here` stuff will work. I'll try to put "# JC" before
# any blocks I change, but you can also just compare the files.

library(dplyr)

# JC
library(here)
library(tidyverse)
library(readr)
library(scales)

# JC: Don't do this! It prevents your code from working 
# on other's computers. Use the "here" stuff below.
#setwd("~/Documents/UM/Thesis/Chapter 2/Data")

# JC: changed to readr functions
# The organization of the Box is kind of a disaster, btw. 
alex <- read_csv(paste0(here(),"/data/Alex Step 20201020.csv"))
john <- read_csv(paste0(here(),"/data/John XGBoost 20201020.csv"))
#hannah2 <- read_csv(paste0(here(),"/data/hannah step 2.csv"))
hannah2 <- read_csv(paste0(here(),"/data/Hannah step 20201020.csv"))
combined <- read_csv(paste0(here(),"/data/Combined 20201020.csv"))



# JC: I'm deleting some group stuff here because I'm 
# not 100% sure it's implemented correctly. You'd
# have different groups for each file, for instance. 
# I'm guessing the issues you ran into could be related 
# to our files being in different orders? 
# 
# The key is to create groups based on the probabilities, so that 
# the highest group is the one with the highest probability estimated
# from the model. 
# 
# As you observed, XGBoost didn't spit out many probabilities 
# (there are only 154 unique values), so I had to fiddle with the 
# code on that line. 
#
# Also, your probabilities are missing for 74K rows (41%). That required
# some fiddling too. That's 182 out of your 468 easements, so that's a 
# bit troubling.... Might be worth discussing. 

n.groups <- 100

alex <- alex %>% 
  mutate(prob_group = cut(probability,
                          breaks=quantile(probability,
                                          probs=seq(0,1,length.out = n.groups))))

john <- john %>% 
  mutate(prob_group = cut(probability,
                          breaks=unique(quantile(probability,
                                          probs=seq(0,1,length.out = n.groups)))))

hannah2 <- hannah2 %>% 
  mutate(prob_group = cut(probability,
                          breaks=quantile(probability,
                                          probs=seq(0,1,length.out = n.groups),
                                          na.rm=T)))

combined <- combined %>% 
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
  bind_rows(hannah2 %>%
              group_by(prob_group) %>% 
              summarize(mean_prob = mean(probability,na.rm=T),
                        actual_frac = mean(CE_yn,na.rm=T)) %>% 
              mutate(model="Hand-tuned Logistic"))

for.plot <- for.plot %>% 
  bind_rows(combined %>%
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
  left_join(hannah2 %>% 
              select(FID, probability) %>% 
              rename(hannah_prob=probability),
            by="FID")

car::spm(for.pairs %>% select(-FID) %>% slice_sample(prop=0.05))

cor(for.pairs %>% select(-FID),use="pairwise.complete.obs")




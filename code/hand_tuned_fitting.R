library(here)
library(tidyverse)
library(scales)
library(broom)
library(readr)
library(lmtest)
library(forcats)

df <-read_csv(paste0(here(),"/data/clean dataset 20200824.csv"))

table(df$CE_yn)

#splitting data for sampling
ce.y <- df[df$CE_yn==1,]
ce.n <- df[df$CE_yn==0,]


#Randomly pull out 50-100 CE and 500-1000 NCE for your test set.
set.seed(20201103)

test.cey <- ce.y %>% 
  slice_sample(n=75)

test.cen <- ce.n %>% 
  slice_sample(n=750)


test.set <-test.cey %>% 
  bind_rows(test.cen)

nrow(test.set)

#Remaining CE for the training set.

train.cey <- ce.y %>% 
  filter(!(FID %in% test.set$FID),
         CE_yn == 1)


train.cen <- ce.n %>% 
  filter(!(FID %in% test.set$FID),
         CE_yn == 0) %>% 
  slice_sample(prop=0.1)

train.set <- train.cey %>% 
  bind_rows(train.cen)

table(train.set$CE_yn)
names(train.set) # going to be running models on columns 30:end for new log10 variables 

var.keeps <- c(2:11,13:196)

#test run 1 variable
m1 <- glm(CE_yn ~ train.set$vb.voterbase_gender, data=train.set, family = "binomial")
summary(m1)


#null model to compare things to (baseline)
mnull <- glm(CE_yn ~ 1, data=train.set, family = "binomial") #null model, no predictors 
summary(mnull)

# likelihood ratio test (sig p value indicates improved model fit with included variable)
lrtest(mnull,m1)

var.test <- tibble(var=character(length(var.keeps)),
                   p_val = 0.0)

train.set.df <- as.data.frame(train.set)

for(i in 1:length(var.keeps)){ # beginning loop on column length(var.keeps)
  
  this.var <- names(train.set.df)[var.keeps[i]]
  this.model <- glm(CE_yn ~ train.set.df[,var.keeps[i]], data=train.set, family = "binomial")
  this.test <- rms::lrtest(mnull, this.model) 
  
  var.test$var[i] <- this.var
  var.test$p_val[i] <- as.numeric(this.test$stats[3])
  
}

var.test <- var.test %>% 
  mutate(p_val_group = cut(p_val,
                           breaks=quantile(p_val,
                                           probs=seq(0,1,length.out = 10)),
                           include.lowest=T)) %>% 
  mutate(var = fct_reorder(var,p_val))


for(grp in unique(var.test$p_val_group)){
  for.plot <- var.test %>% 
    filter(p_val_group==grp) %>% 
    arrange(p_val)
  
  p <- ggplot(for.plot,
              aes(x=p_val,y=var)) + 
    geom_point() +
    scale_x_continuous(label=percent)
  
  print(p)
}



glm(CE_yn ~ logGISAcr 
      + SUM_Fallow 
      + xpg.truetouch_online_mid_high 
      + ts.tsmart_activist_score  
      + ts.tsmart_trump_support_score
    # + SUM_Farmsi 
    # + SUM_TotalA
    # + vb.tsmart_county_name
     + ts.tsmart_urbanicity_rank
     + SUM_Grazin
    # + SUM_Irriga
    # + SUM_TotalL
    # + xpg.act_int_zoo_visitors
    # + xpg.buyer_young_adult_clothing_shoppers
    # + xpg.truetouch_mobile_video
    # + SUM_Forest
    # + xpg.financial_debit_card_user
    # + SUM_WildHa
    # + xpg.act_int_amusement_park_visitors
    # + xpg.act_int_eats_at_fast_food_restaurants 
    # + vb.voterbase_age 
    # + vote.additive 
    # + xpg.act_int_listens_to_alternative_music 
    # + xpg.act_int_music_download 
    # + SUM_TotalB 
    # + xpg.buyer_coupon_users 
    # + gsyn.synth_county_sum_fec_contribution_count_republican 
    # + xpg.act_int_listens_to_pop_music 
    # + xpg.act_int_plays_tennis 
    # + xpg.truetouch_mobile_display 
    # + xpg.act_int_listens_to_hip_hop_music 
    # + xpg.act_int_avid_runners 
    # + xpg.tt_decisionstyle_novelty_seekers 
    # + xpg.buyer_tablet_owners 
    # + ts.tsmart_children_present_score 
    # + xpg.act_int_listens_to_oldies_music 
    # + xpg.lifestyle_medicare_policy_holders 
    # + xpg.truetouch_direct_mail 
    # + xpg.act_int_listens_to_music 
    # + xpg.act_int_e_book_reader 
    # + xpg.act_int_plays_soccer 
    # + xpg.tt_conversion_online_deal_voucher 
    # + xpg.tt_conversion_mid_high_end_store 
    # + ts.tsmart_path_to_citizen_score 
    # + xpg.act_int_attend_order_educational_programs 
    # + xpg.lifestyle_hotel_guest_loyalty_program 
    # + xpg.buyer_laptop_owners 
    # + xpg.tt_conversion_discount_supercenters 
    # + xpg.act_int_listens_to_80s_music 
    # + xpg.act_int_audio_book_listener 
    # + xpg.act_int_photography 
    # + xpg.act_int_nhl_enthusiast 
    # + xpg.lifestyle_have_grandchildren 
    # + xpg.act_int_video_gamer 
    # + xpg.financial_corporate_credit_card_user 
    # + xpg.core_based_statistical_area_type 
    # + xpg.memberships_aarp_members 
    # + ts.tsmart_progressive_tax_score 
     + xpg.donor_contributes_to_political_charities 
    # + xpg.act_int_sports_enthusiast 
    # + xpg.truetouch_online_streaming_tv 
    # + xpg.act_int_plays_hockey 
    # + ts.tsmart_climate_change_score 
    # + ts.tsmart_evangelical_raw_score 
    # + xpg.truetouch_brick_mortar 
    # + ts.tsmart_veteran_score 
    # + xpg.act_int_pga_tour_enthusiast 
    # + xpg.tt_decisionstyle_deal_seeker 
    # + xpg.buyer_luxury_store_shoppers 
    # + xpg.truetouch_online_video 
    # + xpg.act_int_snow_sports 
    # + ts.tsmart_gunowner_score 
    # + xpg.lifestyle_military_active 
    # + xpg.tt_decisionstyle_savvy_researchers 
    # + xpg.lifestyle_high_frequency_business_traveler 
    # + xpg.buyer_luxury_home_goods_store_shopper 
    # + xpg.truetouch_online_bid_mrktplc 
    # + xpg.buyer_warehouse_club_members 
    # + xpg.act_int_political_viewing_on_tv_liberal_comedy 
    # + xpg.tt_conversion_ebid_sites 
    # + xpg.act_int_canoeing_kayaking 
    # + xpg.buyer_loyalty_card_user 
    # + xpg.act_int_listens_to_classical_music 
    # + xpg.act_int_pet_enthusiast 
    # + xpg.invest_brokerage_account_owner 
    # + xpg.truetouch_internet_radio 
    # + xpg.buyer_security_system_owners 
    # + xpg.truetouch_online_discount 
    # + xpg.tt_decisionstyle_brand_loyalists 
     + ts.tsmart_labor_union_support_score 
    # + xpg.truetouch_online_display 
    # + xpg.financial_credit_card_user 
    # + xpg.invest_mutual_fund_investor 
    # + xpg.lifestyle_high_frequency_cruise_enthusiast 
    # + xpg.tt_decisionstyle_in_the_moment_shoppers 
    # + xpg.act_int_mlb_enthusiast 
    # + xpg.act_int_eats_at_family_restaurants 
    # + xpg.buyer_high_end_spirit_drinkers 
    # + xpg.technology_adoption 
    # + ts.tsmart_gun_control_score 
    # + xpg.act_int_play_golf 
    # + xpg.tt_conversion_specialty_or_boutique 
    # + xpg.financial_premium_credit_card_user 
    # + ts.tsmart_prochoice_score 
    # + xpg.act_int_dog_owners 
    # + xpg.lifestyle_frequent_flyer_program_member 
    # + xpg.act_int_fishing 
    # + xpg.act_int_hunting_enthusiasts 
    # + xpg.act_int_healthy_living 
    # + xpg.invest_active_investor 
    # + logTotalV 
    # + xpg.tt_conversion_specialty_dept_store 
    # + xpg.lifestyle_interest_in_religion 
    # + xpg.length_of_residence 
    # + xpg.act_int_scrapbooking 
     + xpg.hobbies_gardening 
    # + xpg.act_int_listens_to_jazz_music 
    # + xpg.act_int_do_it_yourselfers 
    # + xpg.buyer_supercenter_shoppers 
    # + xpg.donor_contributes_to_charities 
    # + ts.tsmart_minimum_wage_score 
    # + xpg.tt_decisionstyle_mainstream_adopters 
    # + xpg.act_int_casino_gambling 
    # + ts.tsmart_ideology_enhanced_score 
    # + xpg.act_int_digital_magazine_newspapers_buyers 
    # + xpg.invest_participate_in_online_trading 
    # + xpg.truetouch_email_engagement 
    # + xpg.act_int_cultural_arts 
    # + xpg.lifestyle_high_frequency_domestic_vacationer 
    # + xpg.act_int_music_streaming 
    # + online.buyer 
    # + xpg.lifestyle_medical_insurance_policy_holders 
    # + xpg.number_of_children_in_living_unit 
    # + xpg.act_int_on_a_diet 
    # + xpg.act_int_nba_enthusiast 
    # + xpg.act_int_listens_to_country_music 
    # + xpg.act_int_gourmet_cooking 
    # + xpg.act_int_political_viewing_on_tv_conservative 
    # + xpg.memberships_union_member 
    # + xpg.lifestyle_military_inactive 
    # + xpg.buyer_prestige_makeup_user 
    # + xpg.donor_contributes_to_health_charities 
    # + xpg.financial_store_credit_card_user 
    # + xpg.tt_decisionstyle_recreational_shoppers 
    # + xpg.act_int_nascar_enthusiast 
    # + xpg.tt_conversion_etail_only 
    # + retail.buyer 
    # + xpg.tt_conversion_wholesale 
    # + vb.voterbase_gender 
    # + xpg.donor_contributes_by_volunteering 
    # + xpg.ind_lvl_occupation_code 
    # + xpg.home_business 
     + gsyn.synth_county_sum_fec_contribution_count_democrat 
    # + ts.tsmart_high_school_only_score
    ,
    data=train.set, 
    family = "binomial", 
    na.action = na.fail) %>% 
  anova(test="Chisq")

# Final model

final.md <- train.set %>% 
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
              data=train.set, #replace with final.md
              family = "binomial") 

anova(m.final,test="Chisq")
summary(m.final)
fit.final <- tidy(m.final)

# JC: let's get the ROC curve and do some resampling to test the 
# model. This new version of tidymodels just came out, so I'm
# going to test it here. Here's the reference that 
# I'm using: https://www.tidymodels.org/start/resampling/

library(tidymodels) # Normally this would go up top

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

glm.test.pred %>% 
  accuracy(truth=CE_yn, CE_yn)

roc_curve(glm.test.pred,pred,truth=CE_yn) %>% autoplot()


fit.final <- fit.final %>% 
  mutate(exp_est = exp(estimate),
         lb = exp(estimate - 2*std.error),
         ub = exp(estimate + 2*std.error))

ggplot(fit.final,
       aes(x=exp_est,y=term)) +
  geom_point() + 
  geom_errorbarh(aes(xmin=lb,xmax=ub,y=term),height=0.1) + 
  theme_gray() + 
  labs(x="Odds Multiplier",y="Model Term") +
  ggtitle("Willingness to Place Conservation Easement")


ggplot(fit.final %>% 
         filter(term!="logGISAcr",term!="(Intercept)"),
       aes(x=exp_est,y=term)) +
  geom_point() + 
  geom_errorbarh(aes(xmin=lb,xmax=ub,y=term),height=0.1) + 
  theme_gray() + 
  labs(x="Odds Multiplier",y="Model Term") + 
  ggtitle("Willingness to Place Conservation Easement") +
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
fid.probs <- df[,c("FID", "fittedvals")]

head(fid.probs)
         
# HANNAH, merge hand_tuned values to john+alex probs and create probs for missing
# FID's by averaging their two probabilities
# THEN, create new column with average of all three probs (combined probs)

         
##########
# m.global <- glm(CE_yn ~ logGISAcr +
#                   SUM_Farmsi +
#                   SUM_TotalA +
#                   vb.tsmart_county_name +
#                   ts.tsmart_urbanicity_rank +
#                   SUM_Grazin +
#                   SUM_Irriga +
#                   SUM_TotalL +
#                   xpg.act_int_zoo_visitors +
#                   xpg.buyer_young_adult_clothing_shoppers +
#                   xpg.truetouch_mobile_video +
#                   SUM_Forest +
#                   xpg.financial_debit_card_user +
#                   SUM_WildHa +
#                   xpg.act_int_amusement_park_visitors +
#                   SUM_Fallow +
#                   xpg.act_int_eats_at_fast_food_restaurants +
#                   vb.voterbase_age +
#                   vote.additive +
#                   xpg.act_int_listens_to_alternative_music +
#                   xpg.act_int_music_download +
#                   SUM_TotalB +
#                   xpg.buyer_coupon_users +
#                   gsyn.synth_county_sum_fec_contribution_count_republican +
#                   xpg.act_int_listens_to_pop_music +
#                   xpg.act_int_plays_tennis +
#                   xpg.truetouch_mobile_display +
#                   xpg.act_int_listens_to_hip_hop_music +
#                   xpg.act_int_avid_runners +
#                   xpg.tt_decisionstyle_novelty_seekers +
#                   xpg.buyer_tablet_owners +
#                   ts.tsmart_children_present_score +
#                   xpg.act_int_listens_to_oldies_music +
#                   xpg.lifestyle_medicare_policy_holders +
#                   xpg.truetouch_direct_mail +
#                   xpg.act_int_listens_to_music +
#                   xpg.act_int_e_book_reader +
#                   xpg.act_int_plays_soccer +
#                   xpg.tt_conversion_online_deal_voucher +
#                   xpg.tt_conversion_mid_high_end_store +
#                   ts.tsmart_path_to_citizen_score +
#                   xpg.act_int_attend_order_educational_programs +
#                   xpg.lifestyle_hotel_guest_loyalty_program +
#                   xpg.buyer_laptop_owners +
#                   xpg.tt_conversion_discount_supercenters +
#                   xpg.act_int_listens_to_80s_music +
#                   xpg.act_int_audio_book_listener +
#                   xpg.act_int_photography +
#                   xpg.act_int_nhl_enthusiast +
#                   xpg.lifestyle_have_grandchildren +
#                   xpg.truetouch_online_mid_high +
#                   xpg.act_int_video_gamer +
#                   xpg.financial_corporate_credit_card_user +
#                   xpg.core_based_statistical_area_type +
#                   xpg.memberships_aarp_members +
#                   ts.tsmart_progressive_tax_score +
#                   xpg.donor_contributes_to_political_charities +
#                   xpg.act_int_sports_enthusiast +
#                   xpg.truetouch_online_streaming_tv +
#                   xpg.act_int_plays_hockey +
#                   ts.tsmart_climate_change_score +
#                   ts.tsmart_evangelical_raw_score +
#                   xpg.truetouch_brick_mortar +
#                   ts.tsmart_veteran_score +
#                   ts.tsmart_activist_score +
#                   xpg.act_int_pga_tour_enthusiast +
#                   xpg.tt_decisionstyle_deal_seeker +
#                   xpg.buyer_luxury_store_shoppers +
#                   xpg.truetouch_online_video +
#                   xpg.act_int_snow_sports +
#                   ts.tsmart_gunowner_score +
#                   xpg.lifestyle_military_active +
#                   xpg.tt_decisionstyle_savvy_researchers +
#                   xpg.lifestyle_high_frequency_business_traveler +
#                   xpg.buyer_luxury_home_goods_store_shopper +
#                   xpg.truetouch_online_bid_mrktplc +
#                   xpg.buyer_warehouse_club_members +
#                   xpg.act_int_political_viewing_on_tv_liberal_comedy +
#                   xpg.tt_conversion_ebid_sites +
#                   xpg.act_int_canoeing_kayaking +
#                   xpg.buyer_loyalty_card_user +
#                   xpg.act_int_listens_to_classical_music +
#                   xpg.act_int_pet_enthusiast +
#                   xpg.invest_brokerage_account_owner +
#                   xpg.truetouch_internet_radio +
#                   xpg.buyer_security_system_owners +
#                   xpg.truetouch_online_discount +
#                   xpg.tt_decisionstyle_brand_loyalists +
#                   ts.tsmart_labor_union_support_score +
#                   xpg.truetouch_online_display +
#                   xpg.financial_credit_card_user +
#                   xpg.invest_mutual_fund_investor +
#                   xpg.lifestyle_high_frequency_cruise_enthusiast +
#                   xpg.tt_decisionstyle_in_the_moment_shoppers +
#                   xpg.act_int_mlb_enthusiast +
#                   xpg.act_int_eats_at_family_restaurants +
#                   xpg.buyer_high_end_spirit_drinkers +
#                   xpg.technology_adoption +
#                   ts.tsmart_gun_control_score +
#                   xpg.act_int_play_golf +
#                   xpg.tt_conversion_specialty_or_boutique +
#                   xpg.financial_premium_credit_card_user +
#                   ts.tsmart_prochoice_score +
#                   xpg.act_int_dog_owners +
#                   xpg.lifestyle_frequent_flyer_program_member +
#                   xpg.act_int_fishing +
#                   xpg.act_int_hunting_enthusiasts +
#                   xpg.act_int_healthy_living +
#                   xpg.invest_active_investor +
#                   logTotalV +
#                   xpg.tt_conversion_specialty_dept_store +
#                   ts.tsmart_trump_support_score +
#                   xpg.lifestyle_interest_in_religion +
#                   xpg.length_of_residence +
#                   xpg.act_int_scrapbooking +
#                   xpg.hobbies_gardening +
#                   xpg.act_int_listens_to_jazz_music +
#                   xpg.act_int_do_it_yourselfers +
#                   xpg.buyer_supercenter_shoppers +
#                   xpg.donor_contributes_to_charities +
#                   ts.tsmart_minimum_wage_score +
#                   xpg.tt_decisionstyle_mainstream_adopters +
#                   xpg.act_int_casino_gambling +
#                   ts.tsmart_ideology_enhanced_score +
#                   xpg.act_int_digital_magazine_newspapers_buyers +
#                   xpg.invest_participate_in_online_trading +
#                   xpg.truetouch_email_engagement +
#                   xpg.act_int_cultural_arts +
#                   xpg.lifestyle_high_frequency_domestic_vacationer +
#                   xpg.act_int_music_streaming +
#                   online.buyer +
#                   xpg.lifestyle_medical_insurance_policy_holders +
#                   xpg.number_of_children_in_living_unit +
#                   xpg.act_int_on_a_diet +
#                   xpg.act_int_nba_enthusiast +
#                   xpg.act_int_listens_to_country_music +
#                   xpg.act_int_gourmet_cooking +
#                   xpg.act_int_political_viewing_on_tv_conservative +
#                   xpg.memberships_union_member +
#                   xpg.lifestyle_military_inactive +
#                   xpg.buyer_prestige_makeup_user +
#                   xpg.donor_contributes_to_health_charities +
#                   xpg.financial_store_credit_card_user +
#                   xpg.tt_decisionstyle_recreational_shoppers +
#                   xpg.act_int_nascar_enthusiast +
#                   xpg.tt_conversion_etail_only +
#                   retail.buyer +
#                   xpg.tt_conversion_wholesale +
#                   vb.voterbase_gender +
#                   xpg.donor_contributes_by_volunteering +
#                   xpg.ind_lvl_occupation_code +
#                   xpg.home_business +
#                   gsyn.synth_county_sum_fec_contribution_count_democrat +
#                   ts.tsmart_high_school_only_score, 
#                 data=train.set, 
#                 family = "binomial", 
#                 na.action = na.fail)

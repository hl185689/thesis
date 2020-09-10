library(here)
library(dplyr)
library(ggplot2)
library(scales)
library(broom)
library(readr)
library(lmtest)
library(car)

# 
# data.file <- "data/clean dataset 20200824.csv"
# 
# df <- read_csv(paste0(here(),data.file)) %>% 
#   janitor::clean_names()
# 
# 


df <-read.csv("C:/Users/conor/Desktop/thesis/data/clean dataset 20200824.csv") 


#model formulation needs to be in dataframe, not tibble
df <-  data.frame(df)

df <- df[complete.cases(df),]

#row total
nrow(df)
names(df)
#CE total
table(df$CE_yn)

# NCE = 105868, CE = 283


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

#number of "no's" to keep (10 "no's" for every "yes")
nrow(train.cey)*10

sam.train <- sample(nrow(sec.sam.cen), 2110, replace = FALSE) 

train.cen <- ce.n[sam.train, ]

train.set <- rbind(train.cey, train.cen)

#Fit model on the training set. 

table(train.set$CE_yn)

names(train.set) # going to be running models on columns 30:end for new log10 variables 

var.keeps <- c(2:11,13:196)


#test run 1 variable
m1 <- glm(CE_yn ~ train.set[,var.keeps[12]], data=train.set, family = "binomial")
summary(m1)


#null model to compare things to (baseline)
mnull <- glm(CE_yn ~ 1, data=train.set, family = "binomial") #null model, no predictors 
summary(mnull)

# likelihood ratio test (sig p value indicates improved model fit with included variable)
lrtest(mnull,m1)

models <- list()
lrtests <- list()

for(i in 1:length(var.keeps)){ # beginning loop on column length(var.keeps)
  
  models[[i]] <- glm(CE_yn ~ train.set[,var.keeps[i]], data=train.set, family = "binomial")
  
  
  lrtests[[i]] <-  lrtest(mnull, models[[i]]) #lr tests of all univariate models against the null (does adding variable x improve model fit?)
}


# pull out the pvalues for each lr test
lr.pvalue <- list()
for(i in 1:length(lrtests)){
  lr.pvalue[[i]] <- lrtests[[i]]$`Pr(>Chisq)`[2]
}


lr_output.df <- data.frame(var.name = names(train.set)[var.keeps],
                           lr.test_pvalue = do.call(rbind, lr.pvalue))

#write.csv(lr_output.df, file = "C:/Users/conor/Desktop/thesis/data/Univariate_LRTest_Pvalues.csv", row.names = F)

m.global <- glm(CE_yn ~ logGISAcr +
                  SUM_Farmsi +
                  SUM_TotalA +
                  vb.tsmart_county_name +
                  ts.tsmart_urbanicity_rank +
                  SUM_Grazin +
                  SUM_Irriga +
                  SUM_TotalL +
                  xpg.act_int_zoo_visitors +
                  xpg.buyer_young_adult_clothing_shoppers +
                  xpg.truetouch_mobile_video +
                  SUM_Forest +
                  xpg.financial_debit_card_user +
                  SUM_WildHa +
                  xpg.act_int_amusement_park_visitors +
                  SUM_Fallow +
                  xpg.act_int_eats_at_fast_food_restaurants +
                  vb.voterbase_age +
                  vote.additive +
                  xpg.act_int_listens_to_alternative_music +
                  xpg.act_int_music_download +
                  SUM_TotalB +
                  xpg.buyer_coupon_users +
                  gsyn.synth_county_sum_fec_contribution_count_republican +
                  xpg.act_int_listens_to_pop_music +
                  xpg.act_int_plays_tennis +
                  xpg.truetouch_mobile_display +
                  xpg.act_int_listens_to_hip_hop_music +
                  xpg.act_int_avid_runners +
                  xpg.tt_decisionstyle_novelty_seekers +
                  xpg.buyer_tablet_owners +
                  ts.tsmart_children_present_score +
                  xpg.act_int_listens_to_oldies_music +
                  xpg.lifestyle_medicare_policy_holders +
                  xpg.truetouch_direct_mail +
                  xpg.act_int_listens_to_music +
                  xpg.act_int_e_book_reader +
                  xpg.act_int_plays_soccer +
                  xpg.tt_conversion_online_deal_voucher +
                  xpg.tt_conversion_mid_high_end_store +
                  ts.tsmart_path_to_citizen_score +
                  xpg.act_int_attend_order_educational_programs +
                  xpg.lifestyle_hotel_guest_loyalty_program +
                  xpg.buyer_laptop_owners +
                  xpg.tt_conversion_discount_supercenters +
                  xpg.act_int_listens_to_80s_music +
                  xpg.act_int_audio_book_listener +
                  xpg.act_int_photography +
                  xpg.act_int_nhl_enthusiast +
                  xpg.lifestyle_have_grandchildren +
                  xpg.truetouch_online_mid_high +
                  xpg.act_int_video_gamer +
                  xpg.financial_corporate_credit_card_user +
                  xpg.core_based_statistical_area_type +
                  xpg.memberships_aarp_members +
                  ts.tsmart_progressive_tax_score +
                  xpg.donor_contributes_to_political_charities +
                  xpg.act_int_sports_enthusiast +
                  xpg.truetouch_online_streaming_tv +
                  xpg.act_int_plays_hockey +
                  ts.tsmart_climate_change_score +
                  ts.tsmart_evangelical_raw_score +
                  xpg.truetouch_brick_mortar +
                  ts.tsmart_veteran_score +
                  ts.tsmart_activist_score +
                  xpg.act_int_pga_tour_enthusiast +
                  xpg.tt_decisionstyle_deal_seeker +
                  xpg.buyer_luxury_store_shoppers +
                  xpg.truetouch_online_video +
                  xpg.act_int_snow_sports +
                  ts.tsmart_gunowner_score +
                  xpg.lifestyle_military_active +
                  xpg.tt_decisionstyle_savvy_researchers +
                  xpg.lifestyle_high_frequency_business_traveler +
                  xpg.buyer_luxury_home_goods_store_shopper +
                  xpg.truetouch_online_bid_mrktplc +
                  xpg.buyer_warehouse_club_members +
                  xpg.act_int_political_viewing_on_tv_liberal_comedy +
                  xpg.tt_conversion_ebid_sites +
                  xpg.act_int_canoeing_kayaking +
                  xpg.buyer_loyalty_card_user +
                  xpg.act_int_listens_to_classical_music +
                  xpg.act_int_pet_enthusiast +
                  xpg.invest_brokerage_account_owner +
                  xpg.truetouch_internet_radio +
                  xpg.buyer_security_system_owners +
                  xpg.truetouch_online_discount +
                  xpg.tt_decisionstyle_brand_loyalists +
                  ts.tsmart_labor_union_support_score +
                  xpg.truetouch_online_display +
                  xpg.financial_credit_card_user +
                  xpg.invest_mutual_fund_investor +
                  xpg.lifestyle_high_frequency_cruise_enthusiast +
                  xpg.tt_decisionstyle_in_the_moment_shoppers +
                  xpg.act_int_mlb_enthusiast +
                  xpg.act_int_eats_at_family_restaurants +
                  xpg.buyer_high_end_spirit_drinkers +
                  xpg.technology_adoption +
                  ts.tsmart_gun_control_score +
                  xpg.act_int_play_golf +
                  xpg.tt_conversion_specialty_or_boutique +
                  xpg.financial_premium_credit_card_user +
                  ts.tsmart_prochoice_score +
                  xpg.act_int_dog_owners +
                  xpg.lifestyle_frequent_flyer_program_member +
                  xpg.act_int_fishing +
                  xpg.act_int_hunting_enthusiasts +
                  xpg.act_int_healthy_living +
                  xpg.invest_active_investor +
                  logTotalV +
                  xpg.tt_conversion_specialty_dept_store +
                  ts.tsmart_trump_support_score +
                  xpg.lifestyle_interest_in_religion +
                  xpg.length_of_residence +
                  xpg.act_int_scrapbooking +
                  xpg.hobbies_gardening +
                  xpg.act_int_listens_to_jazz_music +
                  xpg.act_int_do_it_yourselfers +
                  xpg.buyer_supercenter_shoppers +
                  xpg.donor_contributes_to_charities +
                  ts.tsmart_minimum_wage_score +
                  xpg.tt_decisionstyle_mainstream_adopters +
                  xpg.act_int_casino_gambling +
                  ts.tsmart_ideology_enhanced_score +
                  xpg.act_int_digital_magazine_newspapers_buyers +
                  xpg.invest_participate_in_online_trading +
                  xpg.truetouch_email_engagement +
                  xpg.act_int_cultural_arts +
                  xpg.lifestyle_high_frequency_domestic_vacationer +
                  xpg.act_int_music_streaming +
                  online.buyer +
                  xpg.lifestyle_medical_insurance_policy_holders +
                  xpg.number_of_children_in_living_unit +
                  xpg.act_int_on_a_diet +
                  xpg.act_int_nba_enthusiast +
                  xpg.act_int_listens_to_country_music +
                  xpg.act_int_gourmet_cooking +
                  xpg.act_int_political_viewing_on_tv_conservative +
                  xpg.memberships_union_member +
                  xpg.lifestyle_military_inactive +
                  xpg.buyer_prestige_makeup_user +
                  xpg.donor_contributes_to_health_charities +
                  xpg.financial_store_credit_card_user +
                  xpg.tt_decisionstyle_recreational_shoppers +
                  xpg.act_int_nascar_enthusiast +
                  xpg.tt_conversion_etail_only +
                  retail.buyer +
                  xpg.tt_conversion_wholesale +
                  vb.voterbase_gender +
                  xpg.donor_contributes_by_volunteering +
                  xpg.ind_lvl_occupation_code +
                  xpg.home_business +
                  gsyn.synth_county_sum_fec_contribution_count_democrat +
                  ts.tsmart_high_school_only_score, data=train.set, family = "binomial", na.action = na.fail)




# 
library(rms)
# ?vif
# # to limit collinearity!
# # vif values above 3 is beyond the threshold (take out variable about 3, run again, continue until all below 3)
# 

data.frame(vif(m.global))  # can see any have high correlations with each other

m.global.postVIF <- glm(CE_yn ~ logGISAcr +
                                SUM_Farmsi +
                                #SUM_TotalA +
                                #vb.tsmart_county_name +
                                ts.tsmart_urbanicity_rank +
                                SUM_Grazin +
                                SUM_Irriga +
                                SUM_TotalL +
                                xpg.act_int_zoo_visitors +
                                xpg.buyer_young_adult_clothing_shoppers +
                                xpg.truetouch_mobile_video +
                                SUM_Forest +
                                #xpg.financial_debit_card_user +
                                SUM_WildHa +
                                #xpg.act_int_amusement_park_visitors +
                                SUM_Fallow +
                                xpg.act_int_eats_at_fast_food_restaurants +
                                vb.voterbase_age +
                                vote.additive +
                                #xpg.act_int_listens_to_alternative_music +
                                #xpg.act_int_music_download +
                                SUM_TotalB +
                                xpg.buyer_coupon_users +
                                gsyn.synth_county_sum_fec_contribution_count_republican +
                                #xpg.act_int_listens_to_pop_music +
                                xpg.act_int_plays_tennis +
                                xpg.truetouch_mobile_display +
                                #xpg.act_int_listens_to_hip_hop_music +
                                xpg.act_int_avid_runners +
                                xpg.tt_decisionstyle_novelty_seekers +
                                #xpg.buyer_tablet_owners +
                                ts.tsmart_children_present_score +
                                xpg.act_int_listens_to_oldies_music +
                                xpg.lifestyle_medicare_policy_holders +
                                xpg.truetouch_direct_mail +
                                #xpg.act_int_listens_to_music +
                                #xpg.act_int_e_book_reader +
                                #xpg.act_int_plays_soccer +
                                xpg.tt_conversion_online_deal_voucher +
                                xpg.tt_conversion_mid_high_end_store +
                                ts.tsmart_path_to_citizen_score +
                                #xpg.act_int_attend_order_educational_programs +
                                #xpg.lifestyle_hotel_guest_loyalty_program +
                                #xpg.buyer_laptop_owners +
                                xpg.tt_conversion_discount_supercenters +
                                xpg.act_int_listens_to_80s_music +
                                xpg.act_int_audio_book_listener +
                                xpg.act_int_photography +
                                xpg.act_int_nhl_enthusiast +
                                xpg.lifestyle_have_grandchildren +
                                xpg.truetouch_online_mid_high +
                                #xpg.act_int_video_gamer +
                                xpg.financial_corporate_credit_card_user +
                                #xpg.core_based_statistical_area_type +
                                #xpg.memberships_aarp_members +
                                ts.tsmart_progressive_tax_score +
                                xpg.donor_contributes_to_political_charities +
                                #xpg.act_int_sports_enthusiast +
                                xpg.truetouch_online_streaming_tv +
                                xpg.act_int_plays_hockey +
                                ts.tsmart_climate_change_score +
                                ts.tsmart_evangelical_raw_score +
                                xpg.truetouch_brick_mortar +
                                #ts.tsmart_veteran_score +
                                ts.tsmart_activist_score +
                                xpg.act_int_pga_tour_enthusiast +
                                xpg.tt_decisionstyle_deal_seeker +
                                #xpg.buyer_luxury_store_shoppers +
                                xpg.truetouch_online_video +
                                #xpg.act_int_snow_sports +
                                ts.tsmart_gunowner_score +
                                xpg.lifestyle_military_active +
                                xpg.tt_decisionstyle_savvy_researchers +
                                xpg.lifestyle_high_frequency_business_traveler +
                                #xpg.buyer_luxury_home_goods_store_shopper +
                                xpg.truetouch_online_bid_mrktplc +
                                xpg.buyer_warehouse_club_members +
                                xpg.act_int_political_viewing_on_tv_liberal_comedy +
                                xpg.tt_conversion_ebid_sites +
                                xpg.act_int_canoeing_kayaking +
                                #xpg.buyer_loyalty_card_user +
                                #xpg.act_int_listens_to_classical_music +
                                #xpg.act_int_pet_enthusiast +
                                #xpg.invest_brokerage_account_owner +
                                xpg.truetouch_internet_radio +
                                xpg.buyer_security_system_owners +
                                xpg.truetouch_online_discount +
                                xpg.tt_decisionstyle_brand_loyalists +
                                ts.tsmart_labor_union_support_score +
                                xpg.truetouch_online_display +
                                #xpg.financial_credit_card_user +
                                xpg.invest_mutual_fund_investor +
                                xpg.lifestyle_high_frequency_cruise_enthusiast +
                                xpg.tt_decisionstyle_in_the_moment_shoppers +
                                xpg.act_int_mlb_enthusiast +
                                xpg.act_int_eats_at_family_restaurants +
                                xpg.buyer_high_end_spirit_drinkers +
                                xpg.technology_adoption +
                                #ts.tsmart_gun_control_score +
                                #xpg.act_int_play_golf +
                                xpg.tt_conversion_specialty_or_boutique +
                                #xpg.financial_premium_credit_card_user +
                                #ts.tsmart_prochoice_score +
                                #xpg.act_int_dog_owners +
                                #xpg.lifestyle_frequent_flyer_program_member +
                                xpg.act_int_fishing +
                                #xpg.act_int_hunting_enthusiasts +
                                #xpg.act_int_healthy_living +
                                #xpg.invest_active_investor +
                                logTotalV +
                                xpg.tt_conversion_specialty_dept_store +
                                ts.tsmart_trump_support_score +
                                #xpg.lifestyle_interest_in_religion +
                                xpg.length_of_residence +
                                #xpg.act_int_scrapbooking +
                                #xpg.hobbies_gardening +
                                xpg.act_int_listens_to_jazz_music +
                                xpg.act_int_do_it_yourselfers +
                                xpg.buyer_supercenter_shoppers +
                                #xpg.donor_contributes_to_charities +
                                #ts.tsmart_minimum_wage_score +
                                xpg.tt_decisionstyle_mainstream_adopters +
                                xpg.act_int_casino_gambling +
                                #ts.tsmart_ideology_enhanced_score +
                                #xpg.act_int_digital_magazine_newspapers_buyers +
                                #xpg.invest_participate_in_online_trading +
                                xpg.truetouch_email_engagement +
                                #xpg.act_int_cultural_arts +
                                #xpg.lifestyle_high_frequency_domestic_vacationer +
                                xpg.act_int_music_streaming +
                                online.buyer +
                                #xpg.lifestyle_medical_insurance_policy_holders +
                                xpg.number_of_children_in_living_unit +
                                #xpg.act_int_on_a_diet +
                                #xpg.act_int_nba_enthusiast +
                                xpg.act_int_listens_to_country_music +
                                xpg.act_int_gourmet_cooking +
                                xpg.act_int_political_viewing_on_tv_conservative +
                                xpg.memberships_union_member +
                                xpg.lifestyle_military_inactive +
                                xpg.buyer_prestige_makeup_user +
                                xpg.donor_contributes_to_health_charities +
                                #xpg.financial_store_credit_card_user +
                                xpg.tt_decisionstyle_recreational_shoppers +
                                #xpg.act_int_nascar_enthusiast +
                                xpg.tt_conversion_etail_only +
                                #retail.buyer +
                                xpg.tt_conversion_wholesale +
                                vb.voterbase_gender +
                                xpg.donor_contributes_by_volunteering +
                                xpg.ind_lvl_occupation_code +
                                xpg.home_business +
                                gsyn.synth_county_sum_fec_contribution_count_democrat +
                                ts.tsmart_high_school_only_score, data=train.set, family = "binomial", na.action = na.fail)


vif.df <- data.frame(name = rownames(data.frame(vif(m.global.postVIF))),
                     vif = data.frame(vif(m.global.postVIF)))  # remove high correlation predictors then re-run
vif.df[order(vif.df$vif.m.global.postVIF.),]


library(MASS)

?stepAIC
train.AIC <- stepAIC(m.global.postVIF, direction = "both")
#save(train.AIC, file = "C:/Users/conor/Desktop/thesis/data/stepAIC object 20200824.RData")
train.AIC$anova # this code shows the original and final model

### HANNAH, run the same train.AIC but with a forward direction and see how different the model is

# run on test
test.final <- glm(CE_yn ~ logGISAcr + ts.tsmart_urbanicity_rank + xpg.buyer_young_adult_clothing_shoppers + 
                    xpg.truetouch_mobile_video + SUM_Forest + SUM_Fallow + xpg.act_int_eats_at_fast_food_restaurants + 
                    vote.additive + SUM_TotalB + gsyn.synth_county_sum_fec_contribution_count_republican + 
                    xpg.truetouch_mobile_display + xpg.act_int_avid_runners + 
                    ts.tsmart_children_present_score + xpg.truetouch_direct_mail + 
                    xpg.tt_conversion_discount_supercenters + xpg.act_int_audio_book_listener + 
                    xpg.truetouch_online_mid_high + xpg.truetouch_online_streaming_tv + 
                    ts.tsmart_evangelical_raw_score + ts.tsmart_activist_score + 
                    xpg.act_int_pga_tour_enthusiast + ts.tsmart_gunowner_score + 
                    xpg.buyer_warehouse_club_members + xpg.act_int_mlb_enthusiast + 
                    ts.tsmart_trump_support_score + xpg.act_int_do_it_yourselfers + 
                    xpg.truetouch_email_engagement + xpg.act_int_music_streaming + 
                    xpg.act_int_listens_to_country_music + xpg.act_int_political_viewing_on_tv_conservative + 
                    xpg.memberships_union_member + xpg.donor_contributes_to_health_charities + 
                    xpg.tt_decisionstyle_recreational_shoppers + xpg.ind_lvl_occupation_code + 
                    xpg.home_business + gsyn.synth_county_sum_fec_contribution_count_democrat, data=test.set, family = "binomial")

# for fun, running model on acreage alone
# full.arc <- glm(CE_yn ~ logGISAcr, data = df, family = "binomial")
# 
# summary(full.arc)
# 
# df$fittedvals <- full.arc$fitted.values
# 
# 
# hist(df[df$CE_yn == 1, ]$fittedvals)


### back to test
summary(test.final)

test.set$fittedvals <- test.final$fitted.values


hist(test.set[test.set$CE_yn == 1, ]$fittedvals) # for ones that are truely ones, 
                                                #what does the model say the probability that it actually is


### Full Set RUN ###

full.final <- glm(CE_yn ~ logGISAcr + ts.tsmart_urbanicity_rank + xpg.buyer_young_adult_clothing_shoppers + 
                    xpg.truetouch_mobile_video + SUM_Forest + SUM_Fallow + xpg.act_int_eats_at_fast_food_restaurants + 
                    vote.additive + SUM_TotalB + gsyn.synth_county_sum_fec_contribution_count_republican + 
                    xpg.truetouch_mobile_display + xpg.act_int_avid_runners + 
                    ts.tsmart_children_present_score + xpg.truetouch_direct_mail + 
                    xpg.tt_conversion_discount_supercenters + xpg.act_int_audio_book_listener + 
                    xpg.truetouch_online_mid_high + xpg.truetouch_online_streaming_tv + 
                    ts.tsmart_evangelical_raw_score + ts.tsmart_activist_score + 
                    xpg.act_int_pga_tour_enthusiast + ts.tsmart_gunowner_score + 
                    xpg.buyer_warehouse_club_members + xpg.act_int_mlb_enthusiast + 
                    ts.tsmart_trump_support_score + xpg.act_int_do_it_yourselfers + 
                    xpg.truetouch_email_engagement + xpg.act_int_music_streaming + 
                    xpg.act_int_listens_to_country_music + xpg.act_int_political_viewing_on_tv_conservative + 
                    xpg.memberships_union_member + xpg.donor_contributes_to_health_charities + 
                    xpg.tt_decisionstyle_recreational_shoppers + xpg.ind_lvl_occupation_code + 
                    xpg.home_business + gsyn.synth_county_sum_fec_contribution_count_democrat, data=df, family = "binomial")

summary(full.final)

df$fittedvals <- full.final$fitted.values


hist(df[df$CE_yn == 1, ]$fittedvals)

#saving fitted values by FID
fid.probs <- df[,c("FID", "fittedvals")]

write.csv(fid.probs, file = "C:/Users/conor/Desktop/thesis/data/probability_comparison_hannah.csv", row.names = FALSE)

#merging files

ja.probs <-read.csv("C:/Users/conor/Desktop/thesis/data/probability_comparison.csv")
ha.probs <-read.csv("C:/Users/conor/Desktop/thesis/data/probability_comparison_hannah.csv")

probs.mer <- merge(ja.probs,ha.probs, by = "FID", all = TRUE)


write.csv(probs.mer, file = "C:/Users/conor/Desktop/thesis/data/probability_comparison_all.csv", row.names = FALSE)









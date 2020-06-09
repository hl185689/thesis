* Encoding: UTF-8.

DATASET ACTIVATE DataSet1.
LOGISTIC REGRESSION VARIABLES Easement
  /METHOD=FSTEP(COND) SUM_GISAcr SUM_Contin SUM_Fallow SUM_Farmsi SUM_Forest SUM_Grazin SUM_WildHa 
    SUM_Irriga SUM_NonQua SUM_TotalA SUM_TotalB SUM_TotalL SUM_TotalV vb.voterbase_dob vb.voterbase_age 
    vb.reg_latitude vb.reg_longitude vb.voterbase_general_votes vb.voterbase_primary_votes 
    vb.voterbase_voter_score vb.recession_sensitivity_ranking vb.recession_sensitivity_decile 
    vb.vehicle_value_decile vb.personal_voice_social_networker_demi_decile 
    vb.professional_social_networker_demi_decile vb.purely_social_networker_demi_decile 
    vb.social_networker_demi_decile gsyn.synth_hh_sum_total_votes_cast_count 
    gsyn.synth_hh_sum_dem_primary_votes_cast_count gsyn.synth_hh_sum_rep_primary_votes_cast_count 
    gsyn.synth_hh_sum_primary_votes_cast_count gsyn.synth_hh_distinct_last_names_in_hh 
    gsyn.synth_hh_distinct_races_in_hh gsyn.synth_hh_total_votes_per_person 
    gsyn.synth_hh_total_primary_votes_person gsyn.synth_zip5_sum_zip5_democrat 
    gsyn.synth_zip5_sum_zip5_republican gsyn.synth_zip5_pct_of_dems_per_reg_count 
    gsyn.synth_county_sum_fec_contribution_count_republican 
    gsyn.synth_county_sum_fec_contribution_count_democrat enh.addr_size enh.tsmart_enhanced_hh_size 
    enh.tsmart_enhanced_hh_num_males enh.tsmart_enhanced_hh_num_females 
    enh.tsmart_enhanced_hh_num_registered enh.tsmart_enhanced_hh_num_unregistered 
    enh.tsmart_enhanced_hh_num_dems enh.tsmart_enhanced_hh_num_reps enh.tsmart_enhanced_hh_num_others 
    xpg.ind_lvl_occupation_code xpg.ind_lvl_business_owner xpg.homeowner_probability_model 
    xpg.home_business xpg.length_of_residence xpg.number_of_children_in_living_unit 
    xpg.mor_bank_upscale_merchandise_buyer xpg.mor_bank_male_merchandise_buyer 
    xpg.mor_bank_female_merchandise_buyer xpg.mor_bank_crafts_hobby_merchandise_buyer 
    xpg.mor_bank_gardening_farming_buyer xpg.mor_bank_book_buyer 
    xpg.mor_bank_collect_special_foods_buyer xpg.mor_bank_gifts_and_gadgets_buyer 
    xpg.mor_bank_general_merchandise_buyer xpg.mor_bank_family_and_general_magazine 
    xpg.mor_bank_female_oriented_magazine xpg.mor_bank_male_sports_magazine 
    xpg.mor_bank_religious_magazine xpg.mor_bank_gardening_farming_magazine 
    xpg.mor_bank_culinary_interests_magazine xpg.mor_bank_health_and_fitness_magazine 
    xpg.mor_bank_do_it_yourselfers xpg.mor_bank_news_and_financial xpg.mor_bank_photography 
    xpg.mor_bank_opportunity_seekers_and_ce xpg.mor_bank_religious_contributor 
    xpg.mor_bank_political_contributor xpg.mor_bank_health_and_institution_contributor 
    xpg.mor_bank_general_contributor xpg.mor_bank_odds_and_ends 
    xpg.mortgage_home_purchase_home_purchase_price xpg.property_realty_home_year_built 
    xpg.property_realty_home_land_value xpg.property_realty_property_indicator 
    xpg.property_realty_year_built_confidence xpg.estimated_current_home_value xpg.green_aware 
    xpg.greenaware_tiers xpg.auto_in_the_market_new xpg.auto_in_the_market_used 
    xpg.auto_in_the_market_used_0_5_vehicle xpg.auto_in_the_market_used_6plus_vehicle 
    xpg.property_realty_total_tax xpg.property_realty_home_land_square_footage 
    xpg.mortgage_home_purchase_mortgage_amount xpg.mortgage_home_purchase_mortgage_term_in_months 
    xpg.mortgage_home_purchase_down_payment_pct xpg.mortgage_home_purchase_equity_amount_in_thousands 
    xpg.mortgage_home_purchase_equity_term xpg.mortgage_home_purchase_refinance_amount 
    xpg.mortgage_home_purchase_refinance_term xpg.investment_property_purchase_amount 
    xpg.investment_property_mortgage_amount xpg.investment_property_mortgage_term 
    xpg.investment_property_equity_amount xpg.investment_property_equity_term 
    xpg.investment_property_refinance_amount_in_thousands xpg.investment_property_refinance_term 
    xpg.investment_property_additional_investment_flag xpg.tran_24_month_credit_purchase 
    xpg.tran_ltd_purchase_frequency xpg.online_accessories xpg.online_active_outdoors_hard_goods 
    xpg.online_active_outdoors_soft_goods xpg.online_apparel xpg.online_computers 
    xpg.online_electronics_and_gadgets xpg.online_food_and_beverages xpg.online_furniture 
    xpg.online_general_misc xpg.online_hobbies_and_entertainment xpg.online_home_decor 
    xpg.online_home_domestics xpg.online_home_maintenance xpg.online_home_office xpg.online_kitchen 
    xpg.online_lawn_and_garden xpg.online_outdoor_living xpg.online_overall xpg.online_personal_health 
    xpg.online_pets xpg.online_seasonal_products xpg.online_shoes xpg.online_tabletop_and_dining 
    xpg.online_tools_and_automotive xpg.online_toys xpg.online_travel xpg.retail_accessories 
    xpg.retail_active_outdoors_hard_goods xpg.retail_active_outdoors_soft_goods xpg.retail_apparel 
    xpg.retail_computers xpg.retail_electronics_and_gadgets xpg.retail_food_and_beverages 
    xpg.retail_furniture xpg.retail_general_misc xpg.retail_hobbies_and_entertainment 
    xpg.retail_home_decor xpg.retail_home_domestics xpg.retail_home_maintenance xpg.retail_home_office 
    xpg.retail_kitchen xpg.retail_lawn_and_garden xpg.retail_outdoor_living xpg.retail_overall 
    xpg.retail_personal_health xpg.retail_pets xpg.retail_seasonal_products xpg.retail_shoes 
    xpg.retail_tabletop_and_dining xpg.retail_tools_and_automotive xpg.retail_toys xpg.retail_travel 
    xpg.tt_buy_american xpg.tt_show_me_the_money xpg.tt_go_with_the_flow xpg.tt_no_time_like_present 
    xpg.tt_never_show_up_empty_handed xpg.tt_on_the_road_again xpg.tt_look_at_me_now 
    xpg.tt_stop_and_smell_the_roses xpg.tt_work_hard_play_hard xpg.tt_penny_saved_a_penny_earned 
    xpg.tt_its_all_in_the_name xpg.truetouch_broadcast_cable_tv xpg.truetouch_direct_mail 
    xpg.truetouch_email_engagement xpg.truetouch_internet_radio xpg.truetouch_mobile_display 
    xpg.truetouch_mobile_video xpg.truetouch_online_video xpg.truetouch_online_display 
    xpg.truetouch_online_streaming_tv xpg.truetouch_satellite_radio xpg.truetouch_brick_mortar 
    xpg.truetouch_online_mid_high xpg.truetouch_e_tailer xpg.truetouch_online_discount 
    xpg.truetouch_online_bid_mrktplc xpg.act_int_amusement_park_visitors xpg.act_int_arts_and_crafts 
    xpg.act_int_attend_order_educational_programs xpg.act_int_audio_book_listener 
    xpg.act_int_avid_runners xpg.act_int_boating xpg.act_int_book_reader xpg.act_int_canoeing_kayaking 
    xpg.act_int_casino_gambling xpg.act_int_cat_owners xpg.act_int_coffee_connoisseurs 
    xpg.act_int_cultural_arts xpg.act_int_digital_magazine_newspapers_buyers xpg.act_int_dog_owners 
    xpg.act_int_do_it_yourselfers xpg.act_int_eats_at_family_restaurants 
    xpg.act_int_eats_at_fast_food_restaurants xpg.act_int_e_book_reader xpg.act_int_fishing 
    xpg.act_int_fitness_enthusiast xpg.act_int_gourmet_cooking xpg.act_int_healthy_living 
    xpg.act_int_home_improvement_spenders xpg.act_int_hunting_enthusiasts 
    xpg.act_int_listens_to_80s_music xpg.act_int_listens_to_alternative_music 
    xpg.act_int_listens_to_christian_music xpg.act_int_listens_to_classical_music 
    xpg.act_int_listens_to_country_music xpg.act_int_listens_to_hip_hop_music 
    xpg.act_int_listens_to_jazz_music xpg.act_int_listens_to_music xpg.act_int_listens_to_oldies_music 
    xpg.act_int_listens_to_pop_music xpg.act_int_listens_to_rock_music xpg.act_int_mlb_enthusiast 
    xpg.act_int_music_download xpg.act_int_music_streaming xpg.act_int_nascar_enthusiast 
    xpg.act_int_nba_enthusiast xpg.act_int_nfl_enthusiast xpg.act_int_nhl_enthusiast 
    xpg.act_int_on_a_diet xpg.act_int_outdoor_enthusiast xpg.act_int_pet_enthusiast 
    xpg.act_int_pga_tour_enthusiast xpg.act_int_photography xpg.act_int_play_golf 
    xpg.act_int_plays_hockey xpg.act_int_plays_soccer xpg.act_int_plays_tennis 
    xpg.act_int_political_viewing_on_tv_conservative xpg.act_int_political_viewing_on_tv_liberal 
    xpg.act_int_political_viewing_on_tv_liberal_comedy xpg.act_int_scrapbooking xpg.act_int_snow_sports 
    xpg.act_int_sports_enthusiast xpg.act_int_sweepstakes_lottery xpg.act_int_video_gamer 
    xpg.act_int_weight_conscious xpg.act_int_wine_lovers xpg.act_int_zoo_visitors 
    xpg.buyer_coupon_users xpg.buyer_high_end_spirit_drinkers xpg.buyer_laptop_owners 
    xpg.buyer_loyalty_card_user xpg.buyer_luxury_home_goods_store_shopper 
    xpg.buyer_luxury_store_shoppers xpg.buyer_non_prestige_makeup_brand_user 
    xpg.buyer_presence_of_automobile xpg.buyer_prestige_makeup_user xpg.buyer_security_system_owners 
    xpg.buyer_supercenter_shoppers xpg.buyer_tablet_owners xpg.buyer_warehouse_club_members 
    xpg.buyer_young_adult_clothing_shoppers xpg.memberships_aarp_members xpg.memberships_union_member 
    xpg.invest_active_investor xpg.invest_brokerage_account_owner 
    xpg.invest_have_a_retirement_financial_plan xpg.invest_mutual_fund_investor 
    xpg.invest_participate_in_online_trading xpg.lifestyle_frequent_flyer_program_member 
    xpg.lifestyle_have_grandchildren xpg.lifestyle_high_frequency_business_traveler 
    xpg.lifestyle_high_frequency_cruise_enthusiast xpg.lifestyle_high_frequency_domestic_vacationer 
    xpg.lifestyle_high_frequency_foreign_vacationer xpg.lifestyle_hotel_guest_loyalty_program 
    xpg.lifestyle_interest_in_religion xpg.lifestyle_life_insurance_policy_holders 
    xpg.lifestyle_medical_insurance_policy_holders xpg.lifestyle_medicare_policy_holders 
    xpg.lifestyle_military_active xpg.lifestyle_military_inactive xpg.working_couples_dual_income 
    xpg.hobbies_gardening xpg.donor_contributes_by_volunteering 
    xpg.donor_contributes_to_arts_culture_charities xpg.donor_contributes_to_charities 
    xpg.donor_contributes_to_education_charities xpg.donor_contributes_to_health_charities 
    xpg.donor_contributes_to_political_charities xpg.donor_contributes_to_private_foundations 
    xpg.financial_corporate_credit_card_user xpg.financial_credit_card_user 
    xpg.financial_debit_card_user xpg.financial_major_credit_card_user 
    xpg.financial_premium_credit_card_user xpg.financial_store_credit_card_user xpg.technology_adoption 
    xpg.email_engagement xpg.tt_decisionstyle_savvy_researchers 
    xpg.tt_decisionstyle_organic_and_natural xpg.tt_decisionstyle_brand_loyalists 
    xpg.tt_decisionstyle_trendsetters xpg.tt_decisionstyle_deal_seeker 
    xpg.tt_decisionstyle_recreational_shoppers xpg.tt_decisionstyle_quality_matters 
    xpg.tt_decisionstyle_in_the_moment_shoppers xpg.tt_decisionstyle_mainstream_adopters 
    xpg.tt_decisionstyle_novelty_seekers xpg.tt_conversion_online_deal_voucher 
    xpg.tt_conversion_discount_supercenters xpg.tt_conversion_ebid_sites xpg.tt_conversion_etail_only 
    xpg.tt_conversion_mid_high_end_store xpg.tt_conversion_specialty_dept_store 
    xpg.tt_conversion_specialty_or_boutique xpg.tt_conversion_wholesale ts.tsmart_partisan_score 
    ts.tsmart_presidential_general_turnout_score ts.tsmart_midterm_general_turnout_score 
    ts.tsmart_midterm_general_enthusiasm_score ts.tsmart_offyear_general_turnout_score 
    ts.tsmart_presidential_primary_turnout_score ts.tsmart_non_presidential_primary_turnout_score 
    ts.tsmart_local_voter_score ts.tsmart_teaparty_score ts.tsmart_ideology_score 
    ts.tsmart_moral_authority_score ts.tsmart_children_present_score ts.tsmart_college_graduate_score 
    ts.tsmart_high_school_only_score ts.tsmart_income_rank_score ts.tsmart_evangelical_raw_score 
    ts.tsmart_prochoice_score ts.tsmart_path_to_citizen_score ts.tsmart_climate_change_score 
    ts.tsmart_gun_control_score ts.tsmart_minimum_wage_score ts.tsmart_working_class_score 
    ts.tsmart_activist_score ts.tsmart_trump_resistance_score ts.tsmart_trump_support_score 
    ts.tsmart_gunowner_score ts.tsmart_veteran_score ts.tsmart_progressive_tax_score 
    ts.tsmart_ideology_enhanced_score ts.tsmart_labor_union_support_score ts.tsmart_urbanicity 
    ts.tsmart_urbanicity_rank predictwise.authoritarianism_score predictwise.compassion_score 
    predictwise.economic_populism_score predictwise.environmentalism_score predictwise.free_trade_score 
    predictwise.globalism_score predictwise.guns_score predictwise.healthcare_women_score 
    predictwise.healthcare_score predictwise.immigrants_score predictwise.military_score 
    predictwise.populism_score predictwise.poor_score predictwise.presidential_score 
    predictwise.racial_resentment_score predictwise.regulation_score 
    predictwise.religious_freedom_score predictwise.taxes_score predictwise.traditionalism_score 
    predictwise.trust_in_institutions_score 
  /SAVE=PRED
  /EXTERNAL
  /PRINT=GOODFIT CI(95)
  /CRITERIA=PIN(0.001) POUT(0.05) ITERATE(20) CUT(0.14).

RANK VARIABLES=PRE_15 (A)
  /NTILES(10)
  /PRINT=YES
  /TIES=MEAN.

GRAPH
  /BAR(GROUPED)=MEAN(Easement) BY NTI001 BY HoldOut.


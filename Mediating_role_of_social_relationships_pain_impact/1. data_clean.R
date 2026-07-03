#ALSPAC variable names

## Cohort profile
#kz011b
#kz021

## G0 quest mother
#a006
#c804 c755 c765
#b665
#e170 e172 e174 e176
#f020a f021a f518a f519a

## G1 clinic self
#FJPA001 FJPA010 FJPA011 FJPA141 FJPA140

## G1 quest self
#cct3506 cct3507 cct3508 cct3509 cct3510 cct3511 cct3512 cct3513 cct3514 cct3520 cct3521 cct3522 cct3523 cct3530 cct3531 cct3532 cct3535 cct3560
#YPA2000 YPA2010 YPA2020 YPA2030 YPA2040 YPA2050 YPA2060 YPA2070 YPA2080 YPA2090 YPA2100 YPA2110 YPA2120
#YPA2160 YPA2170 YPA2180 YPA2190 YPA2200 YPA2210 YPA2220
#YPC1150
#YPE7580 YPE7581 YPE7582 YPE7583 YPE7584 YPE7585 YPE7586 YPE7587 YPE7588 YPE7589 YPE7590 YPE7591 YPE7592 YPE7593 YPE7594 YPE7595
#YPE7600 
#YPE1160
#YPE7403
#YPE7420 YPE7430
#YPL6000 YPL6001 YPL6050 YPL6056 YPL6057 YPL6058
#covid4yp_6500 covid4yp_6501 covid4yp_6530 covid4yp_6531 
#covid4yp_6502 covid4yp_6503 covid4yp_6532 covid4yp_6533 
#covid4yp_6510 covid4yp_6511 covid4yp_6520 covid4yp_6521 
#covid4yp_6512 covid4yp_6513 covid4yp_6522 covid4yp_6523

#LOAD
library(haven)
library(dplyr)

#PATHS
path_to_data <- "//path/to/data/"
data <- read_dta(paste0(path_to_data,"formediation.dta"))
df <- data.frame(data[,c('aln','qlet')])

#CONFOUNDERS

##Alive @ 1yo
df$alive <- factor(data$kz011b,levels=c(1:2),labels=c('Yes','No'))

##Sex
df$sex <- factor(data$kz021,levels=c(1:2),labels=c('Male','Female'))

##Ethnicity
df$ethnicity <- factor(data$c804,levels=c(1:2),labels=c('White','Minority ethnic'))

##Smoking in pregnancy (excludes mothers who missed a questionnaire/answered don't know)
###first 3 months (B 18 weeks gestation)
df$m_f3m_pregsmk_0 <- ifelse(data$b665 %in% c(2:5), 1, NA)
df$m_f3m_pregsmk_0 <- ifelse(data$b665 %in% 1, 0, df$m_f3m_pregsmk_0)
df$m_f3m_pregsmk_0 <- factor(df$m_f3m_pregsmk_0,levels=c(0:1),labels=c('No','Yes'))

###last 2 months (E 8 weeks)
df$m_l2m_pregsmk_8w <- ifelse(data$e170 %in% 1 | data$e172 %in% 1 | data$e174 %in% 1 | data$e176 %in% 1, 1, NA)
df$m_l2m_pregsmk_8w <- ifelse(data$e170 %in% 2 & data$e172 %in% 2 & data$e174 %in% 2 & data$e176 %in% 2, 0, df$m_l2m_pregsmk_8w)
df$m_l2m_pregsmk_8w <- factor(df$m_l2m_pregsmk_8w,levels=c(0:1),labels=c('No','Yes'))

###any (B/E 18 weeks gestation/8 weeks)
df$m_pregsmk <- ifelse(df$m_f3m_pregsmk_0 %in% 'Yes' | df$m_l2m_pregsmk_8w %in% 'Yes', 1, NA)
df$m_pregsmk <- ifelse(is.na(df$m_pregsmk) & (df$m_f3m_pregsmk_0 %in% 'No' & df$m_l2m_pregsmk_8w %in% 'No'), 0, df$m_pregsmk)
df$m_pregsmk <- factor(df$m_pregsmk,levels=c(0:1),labels=c('No','Yes'))

##Parental social class (excludes armed forces)
###maternal (C 32 weeks gestation)
df$m_sc_0 <- ifelse(data$c755 %in% c(1:2), 1, NA)
df$m_sc_0 <- ifelse(data$c755 %in% c(3:4), 2, df$m_sc_0)
df$m_sc_0 <- ifelse(data$c755 %in% c(5:6), 3, df$m_sc_0)
df$m_sc_0 <- factor(df$m_sc_0,levels=c(1:3),labels=c('I/II','III','IV/V'))

###paternal (C 32 weeks gestation)
df$p_sc_0 <- ifelse(data$c765 %in% c(1:2), 1, NA)
df$p_sc_0 <- ifelse(data$c765 %in% c(3:4), 2, df$p_sc_0)
df$p_sc_0 <- ifelse(data$c765 %in% c(5:6), 3, df$p_sc_0)
df$p_sc_0 <- factor(df$p_sc_0,levels=c(1:3),labels=c('I/II','III','IV/V'))

###parental highest social class (C 32 weeks gestation)
df$parent_sc_0 <- ifelse(df$m_sc_0 %in% 'I/II' | df$p_sc_0 %in% 'I/II', 1, NA)
df$parent_sc_0 <- ifelse(is.na(df$parent_sc_0) & (df$m_sc_0 %in% 'III' | df$p_sc_0 %in% 'III'), 2, df$parent_sc_0)
df$parent_sc_0 <- ifelse(is.na(df$parent_sc_0) & (df$m_sc_0 %in% 'IV/V' | df$p_sc_0 %in% 'IV/V'), 3, df$parent_sc_0)
df$parent_sc_0 <- factor(df$parent_sc_0,levels=c(1:3),labels=c('I/II','III','IV/V'))

##Home ownership
###home ownership/rental status (A 8 weeks gestation) - excludes 'Other'
df$m_homown_0 <- ifelse(data$a006 %in% c(0:1), 1, NA)
df$m_homown_0 <- ifelse(data$a006 %in% c(2:6), 2, df$m_homown_0)
df$m_homown_0 <- factor(df$m_homown_0,levels=c(1:2),labels=c('Mortgaged/Owned','Rented'))

##Parental mental health (depression/anxiety since child was born)
###mother (F 8 months)
df$m_mhp_1 <- ifelse(data$f021a %in% 1 | data$f020a %in% 1, 1, NA)
df$m_mhp_1 <- ifelse(is.na(df$m_mhp_1) & (data$f021a %in% 2 | data$f020a %in% 2), 0, df$m_mhp_1) 
df$m_mhp_1 <- factor(df$m_mhp_1,levels=c(0:1),labels=c('No','Yes'))

###mother's partner (mother-reported)
df$p_mhp_1 <- ifelse(data$f518a %in% 1 | data$f519a %in% 1, 1, NA)
df$p_mhp_1 <- ifelse(is.na(df$p_mhp_1) & (data$f518a %in% 2 | data$f519a %in% 2), 0, df$m_mhp_1) 
df$p_mhp_1 <- factor(df$p_mhp_1,levels=c(0:1),labels=c('No','Yes'))

###mother had partner
df$m_nopartner_1 <- ifelse(data$f518a %in% -2, T, F)

###parental mental health condition (F 8 months) - missing participants where mother did not have mental health problem or answer partner questions
df$parent_mhp_1 <- ifelse((df$m_mhp_1 %in% 'No' & df$p_mhp_1 %in% 'No') | (df$m_mhp_1 %in% 'No' & df$m_nopartner_1 %in% T), 0, NA)
df$parent_mhp_1 <- ifelse((df$m_mhp_1 %in% 'Yes') | (df$m_mhp_1 %in% 'No' & df$p_mhp_1 %in% 'Yes'), 1, df$parent_mhp_1)
df$parent_mhp_1 <- factor(df$parent_mhp_1,levels=c(0:1),labels=c('No','Yes'))


#EARLY PAIN MEASURES (TF4 17.5 years)
painTF4_labs <- c('FJPA001','FJPA010','FJPA011','FJPA141','FJPA140')
df[,painTF4_labs] <- data[,painTF4_labs]
df[,painTF4_labs] <- sapply(df[,painTF4_labs], function(x){ifelse(x < 0, NA, x)})
df <- df %>%
  rename(
    painq_done_18 = FJPA001,
    pain_1day_mnth_18 = FJPA010,
    pain_more3mnths_18 = FJPA011,
    c_widespreadpain_18 = FJPA141,
    c_regionalpain_18 = FJPA140
  )


#EXPOSURES (SF36 LIMITATIONS IN ACTIVITIES/PAIN 18y)
exp_labs <- c('cct3506','cct3507','cct3508','cct3509','cct3510','cct3511','cct3512','cct3513','cct3514',
              'cct3520','cct3521','cct3522','cct3523',
              'cct3530','cct3531','cct3532',
              'cct3535','cct3560')
df[,exp_labs] <- data[,exp_labs]
df[,exp_labs] <- sapply(df[,exp_labs], function(x){ifelse(x < 0, NA, x)})
df <- df %>%
  rename(
    sf36_sc_modacts_18 = cct3506,
    sf36_sc_groceries_18 = cct3507,
    sf36_sc_manystairs_18 = cct3508,
    sf36_sc_onestair_18 = cct3509,
    sf36_sc_kneel_18 = cct3510,
    sf36_sc_walkm1mile_18 = cct3511,
    sf36_sc_walkhlfmile_18 = cct3512,
    sf36_sc_walk100m_18 = cct3513,
    sf36_sc_bathdress_18 = cct3514,
    sf36_w_cuttime_18 = cct3520,
    sf36_w_accomp_18 = cct3521,
    sf36_w_limitkind_18 = cct3522,
    sf36_w_difficulty_18 = cct3523,
    sf36_s_amount_18 = cct3530,
    sf36_s_accomp_18 = cct3531,
    sf36_s_lesscareful_18 = cct3532,
    sf36_s_extent_soc_18 = cct3535,
    sf36_s_amount_soc_18 = cct3560
  )

rev_labs <- c('sf36_sc_modacts_18' , 'sf36_sc_groceries_18', 'sf36_sc_onestair_18',
              'sf36_sc_kneel_18', 'sf36_sc_walkhlfmile_18', 'sf36_sc_walk100m_18', 'sf36_sc_bathdress_18',
              'sf36_s_amount_18', 'sf36_s_accomp_18', 'sf36_s_lesscareful_18', 'sf36_s_amount_soc_18',
              'sf36_w_cuttime_18', 'sf36_w_accomp_18', 'sf36_w_limitkind_18', 'sf36_w_difficulty_18')

reverse_scale <- function(x) {
  if (!is.numeric(x)) stop("Input must be numeric")
  x_reversed <- max(x, na.rm = TRUE) + 1 - x
  return(x_reversed)
}
df[,rev_labs] <- apply(df[,rev_labs],2,reverse_scale)


#INTERMEDIATE CONFOUNDERS (DEPRESSION/ANXIETY 21y)
dep_labs <- c('YPA2000','YPA2010','YPA2020','YPA2030','YPA2040',
              'YPA2050','YPA2060','YPA2070','YPA2080','YPA2090',
              'YPA2100','YPA2110','YPA2120')
df[,dep_labs] <- data[,dep_labs]
df[,dep_labs] <- sapply(df[,dep_labs], function(x){ifelse(x < 0, NA, x)})
df <- df %>%
  rename(
    dep_miserable_21 = YPA2000,
    dep_no_enjoyment_21 = YPA2010,
    dep_so_tired_21 = YPA2020,
    dep_restless_21 = YPA2030,
    dep_no_good_21 = YPA2040,
    dep_cried_21 = YPA2050,
    dep_hard_think_21 = YPA2060,
    dep_self_hatred_21 = YPA2070,
    dep_bad_person_21 = YPA2080,
    dep_lonely_21 = YPA2090,
    dep_not_loved_21 = YPA2100,
    dep_compare_21 = YPA2110,
    dep_all_wrong_21 = YPA2120
  )

mfq21_labs <- c('dep_miserable_21','dep_no_enjoyment_21','dep_so_tired_21','dep_restless_21','dep_no_good_21','dep_cried_21','dep_hard_think_21','dep_self_hatred_21','dep_bad_person_21','dep_lonely_21','dep_not_loved_21','dep_compare_21','dep_all_wrong_21')
df[,mfq21_labs] <- sapply(df[,mfq21_labs], function(x){ifelse(x < 0, NA, x)})
df[,mfq21_labs] <- sapply(df[,mfq21_labs], function(x){ifelse(x %in% 3, 0, ifelse(x %in% 2, 1, ifelse(x %in% 1, 2, NA)))})
df$mfq_21 <- rowSums(df[,mfq21_labs])
df$mfq_cutoff_21 <- ifelse(is.na(df$mfq_21), NA, ifelse(df$mfq_21 < 12, 0, 1))

anx_labs <- c('YPA2160','YPA2170','YPA2180','YPA2190','YPA2200','YPA2210','YPA2220')
df[,anx_labs] <- data[,anx_labs]
df[,anx_labs] <- sapply(df[,anx_labs], function(x){ifelse(x < 0, NA, x)})
df <- df %>%
  rename(
    anx_nerves_21 = YPA2160,
    anx_not_able_ctrl_21 = YPA2170,
    anx_worry_2_much_21 = YPA2180,
    anx_trbl_relax_21 = YPA2190,
    anx_restless_21 = YPA2200,
    anx_annoyed_21 = YPA2210,
    anx_afraid_21 = YPA2220
  )

gad21_labs <- c('anx_nerves_21','anx_not_able_ctrl_21','anx_worry_2_much_21','anx_trbl_relax_21','anx_restless_21','anx_annoyed_21','anx_afraid_21')
df[,gad21_labs] <- sapply(df[,gad21_labs], function(x){ifelse(x < 0, NA, x)})
df[,gad21_labs] <- sapply(df[,gad21_labs], function(x){ifelse(x %in% 1, 0, ifelse(x %in% 2, 1, ifelse(x %in% 3, 2, ifelse(x %in% 4, 3, NA))))})
df$gad_21 <- rowSums(df[,gad21_labs])
df$gad_cutoff_21 <- ifelse(is.na(df$gad_21), NA, ifelse(df$gad_21 < 10, 0, 1))


#MEDIATORS 

## SOCIAL NETWORK
socnet_items <- c(
  "partner", "siblings", "friends", #"children",
  "lives_with_someone", "sees_parent_monthly"
)

df_socnet <- data %>%
  mutate(
    
    partner = case_when(
      YPE7600 == 1 ~ 1,
      YPE7600 == 0 ~ 0
    ),
    
    siblings = case_when(
      YPC1150 == 1 ~ 1,
      YPC1150 == 0 ~ 0
    ),
    
    friends = case_when(
      YPE1160 %in% 1:2 ~ 1,
      YPE1160 == 0 ~ 0
    ),
    
    lives_with_someone = case_when( 
      YPE7403 == 0 ~ 1,
      YPE7403 == 1 ~ 0
    ),
    sees_mother_monthly = case_when(
      YPE7420 %in% 1:2 ~ 1,
      YPE7420 %in% 3:7 ~ 0
    ),
    sees_father_monthly = case_when(
      YPE7430 %in% 1:2 ~ 1,
      YPE7430 %in% 3:7 ~ 0
    ),
    
    sees_parent_monthly = case_when(
      sees_mother_monthly == 1 | sees_father_monthly == 1 ~ 1,
      sees_mother_monthly == 0 & sees_father_monthly == 0 ~ 0
    ),
    social_integration_sc_25 = 
      partner + siblings + friends + lives_with_someone + sees_parent_monthly,
    
    social_integration_sc_grp_25 = case_when(
      social_integration_sc_25 < 4 ~ 0,
      social_integration_sc_25 >= 4 ~ 1
    )
    
  ) %>%
  dplyr::select(all_of(c(socnet_items, 'social_integration_sc_25','social_integration_sc_grp_25'))) 

df <- bind_cols(df, df_socnet)

## PRACTICAL/EMOTIONAL SUPPORT 25y
emo_sup_items <- c('YPE7580','YPE7581','YPE7582','YPE7583','YPE7584','YPE7585','YPE7586','YPE7587')

prac_sup_items <- c('YPE7588','YPE7589','YPE7590','YPE7591','YPE7592','YPE7593','YPE7594','YPE7595')

df_supp <- data %>%
  dplyr::select(all_of(c(emo_sup_items, prac_sup_items))) %>%
  mutate(across(everything(), ~ replace(.x, .x < 0, NA))) %>%
  mutate(
    
    emo_supp_sc_25 = rowSums(pick(all_of(emo_sup_items))),
    prac_supp_sc_25 = rowSums(pick(all_of(prac_sup_items))),
    
    emo_supp_sc_grp_25 = case_when(
      if_any(all_of(emo_sup_items), ~ is.na(.x)) ~ NA,
      if_all(all_of(emo_sup_items), ~ .x %in% c(3, 4)) ~ 1,
      if_any(all_of(emo_sup_items), ~ .x %in% c(0, 1, 2)) ~ 0
    ),
    
    prac_supp_sc_grp_25 = case_when(
      if_any(all_of(prac_sup_items), ~ is.na(.x)) ~ NA,
      if_all(all_of(prac_sup_items), ~ .x %in% c(3, 4)) ~ 1,
      if_any(all_of(prac_sup_items), ~ .x %in% c(0, 1, 2)) ~ 0
    )
  ) %>%
  
  dplyr::select(all_of(c('emo_supp_sc_25','prac_supp_sc_25','emo_supp_sc_grp_25','prac_supp_sc_grp_25')))

df <- bind_cols(df, df_supp)

## COVID CONTACTS
covid_contact_items <- list(
  covid_inpers_cont_child = c(
    "covid4yp_6500", #f2f 0-4 years
    "covid4yp_6501", #f2f 5-17 years
    "covid4yp_6530", #physical contact 0-4 years
    "covid4yp_6531"  #physical contact 5-17 years
  ),
  
  covid_inpers_cont_adult = c(
    "covid4yp_6502", #f2f 18-69 years
    "covid4yp_6503", #f2f 70+ years
    "covid4yp_6532", #physical contact 18-69 years
    "covid4yp_6533"  #physical contact 70+ years
  ),
  
  covid4_dist_cont_child = c(
    "covid4yp_6510", #phone 0-4 years
    "covid4yp_6511", #phone 5-17 years
    "covid4yp_6520", #video 0-4 years
    "covid4yp_6521"  #video 5-17 years
  ),
  
  covid4_dist_cont_adult = c(
    "covid4yp_6512", #phone 18-69 years
    "covid4yp_6513", #phone 70+ years
    "covid4yp_6522", #video 18-69 years
    "covid4yp_6523"  #video 70+ years
  )
)

covid_contact_items <- c(
  list(
    covid4_inpers_cont_tot = c(
      covid_contact_items$covid_inpers_cont_child,
      covid_contact_items$covid_inpers_cont_adult
    ),
    covid4_dist_cont_tot = c(
      covid_contact_items$covid4_dist_cont_child,
      covid_contact_items$covid4_dist_cont_adult
    )
  ),
  covid_contact_items
)

df_covid <- data %>%
  dplyr::select(all_of(unique(unlist(covid_contact_items)))) %>%
  
  # Recode negative ALSPAC missing values to NA
  mutate(across(everything(), ~ ifelse(. < 0, NA, .))) %>%
  
  # Derive summed contact variables
  mutate(
    !!!map(
      covid_contact_items,
      ~ expr(rowSums(pick(all_of(!!.x))))
    )
  ) %>%
  
  # Keep only derived measures
  dplyr::select(all_of(names(covid_contact_items))) %>%
  
  # Create binary any/no contact variables
  mutate(
    across(
      everything(),
      ~ case_when(
        is.na(.) ~ NA,
        . < 1 ~ 0,
        . >= 1 ~ 1
      ),
      .names = "{.col}_grp_28"
    )
  )

df <- bind_cols(df, df_covid)

#OUTCOMES (PAIN IMPACT 30y)
out_labs <- c('YPL6000','YPL6001','YPL6050','YPL6056','YPL6057','YPL6058')
df[,out_labs] <- data[,out_labs]
df[,out_labs] <- sapply(df[,out_labs], function(x){ifelse(x < 0, NA, x)})
df <- df %>%
  rename(
    pain_1day_mnth_30 = YPL6000,
    pain_more3mnths_30 = YPL6001,
    pain_disc_onoff_3mnths_30 = YPL6050,
    o_daily_acts_30 = YPL6056,
    o_social_30 = YPL6057,
    o_work_30 = YPL6058
  )

#get total pain impact scores
df <- df %>% mutate(painimp_total_30 = rowSums(across(c(o_daily_acts_30, o_social_30, o_work_30)), na.rm = FALSE))

#recode pain variables
df$pain_1day_mnth_18[df$pain_1day_mnth_18 %in% 5] <- 1
df$pain_more3mnths_30 <- df$pain_more3mnths_30-1

#assign participants with no pain in past month lasting longer than a day as having 0 pain impact
#this does not overwrite pain impact scores in those with no pain past month
df <- df %>%
  mutate(
    across(
      .cols = c(o_daily_acts_30, o_social_30, o_work_30, painimp_total_30),
      .fns = ~ ifelse(is.na(.) & pain_1day_mnth_30 == 0, 0, .),
      .names = "{.col}_zeros"
    )
  )

#dichotomise pain impact measures
df$daily_spl_30 <- ifelse(is.na(df$o_daily_acts_30_zeros), NA, ifelse(df$o_daily_acts_30_zeros == 0, 0, 1))
df$social_spl_30 <- ifelse(is.na(df$o_social_30_zeros), NA, ifelse(df$o_social_30_zeros == 0, 0, 1))
df$work_spl_30 <- ifelse(is.na(df$o_work_30_zeros), NA, ifelse(df$o_work_30_zeros == 0, 0, 1))

#flag participants who say no to "aches/pains that have lasted for day or longer in past month" but yes to being "troubled by pain/discomfort, either all time or on off for more than 3 months"
df$flag_impact_nopain_pastmnth <- ifelse(df$pain_1day_mnth_30 %in% 0 & df$pain_disc_onoff_3mnths_30 %in% 1, 1, NA) #116
df$flag_impact_nopain_past3mnths <- ifelse(df$pain_more3mnths_30 %in% 0 & df$pain_disc_onoff_3mnths_30 %in% 1, 1, NA) #225

#flag participants who say yes to "aches/pains that have lasted for day or longer in past month" and that it started more than 3 months ago but said no to being "troubled by pain/discomfort, either all time or on off for more than 3 months"
df$flag_mismatch_pain_3mnths <- ifelse(df$pain_more3mnths_30 %in% 1 & df$painimp_total_30 %in% 0, 1, NA)


#CHANGE BINARY FACTORS TO NUMERIC
binary_vars <- colnames(df)[sapply(df, function(x) length(levels(x))) %in% 2]
df[,binary_vars] <- sapply(df[,binary_vars], function(x) as.numeric(x)-1)

#SAVE FULL DATASET
write_dta(data=df,path=paste0(path_to_data,"fulldata.dta"))
nrow(df) #15645

#REMOVE UNALIVE AT 1YR/MISSING SEX
df <- df %>% filter(sex >= 0, alive >= 0) #removes 644
nrow(df) #15001

#REMOVE SECOND BORNS
df <- df[df$qlet %in% 'A',] #removes 193
nrow(df) #14808 (837 removed in total)

#SAVE DATASET CONTAINING THOSE ALIVE AT 1YO, FIRST BORN
write_dta(data=df,path=paste0(path_to_data,"data.dta"))

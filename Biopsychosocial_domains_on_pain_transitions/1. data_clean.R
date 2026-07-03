#LOAD
library(haven)
library(dplyr)
data <- read_dta("//path/to/data/formultidomain.dta")
data <- data %>% mutate(across(where(is.double), ~ { y <- .x; y[haven::is_tagged_na(y)] <- NA_real_; y }))
data <- data.frame(data)
df <- data[,c('aln','qlet')]

# Completed YPF and YPL
nrow(data[data$YPF0002 %in% 1 & data$YPL0002 %in% 1,])
df$compl <- ifelse(data$YPF0002 %in% 1 & data$YPL0002 %in% 1, T, F)

#Participant was alive at 1 year of age
df$alive <- factor(data$kz011b,levels=c(1:2),labels=c('Yes','No'))

#Participant assigned sex at birth
df$sex <- factor(data$kz021,levels=c(1:2),labels=c('Male','Female'))

#Child ethnic background
df$ethnicity <- factor(data$c804,levels=c(1:2),labels=c('White','Minority ethnic'))

#Highest parental social class
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

#Pain problem in childhood (78 months)
df$pain_prob_7 <- factor(data$kp3130,levels=c(2:1),labels=c('No','Yes'))

#Parental pain - headache, back pain, arthritis, or rheumatism
df$m_pain_12 <- ifelse(data$s1012 %in% c(1:2) | data$s1014 %in% c(1:2) | data$s1028 %in% c(1:2) | data$s1029%in% c(1:2), 1, NA)
df$m_pain_12 <- ifelse(data$s1012 %in% 3 & data$s1014 %in% 3 & data$s1028 %in% 3 & data$s1029 %in% 3, 0, df$m_pain_12)

df$p_pain_12 <- ifelse(data$s3020 %in% c(1:2) | data$s3040 %in% c(1:2) | data$s3037 %in% c(1:2) | data$s3039 %in% c(1:2), 1, NA)
df$p_pain_12 <- ifelse(data$s3020 %in% 3 & data$s3040 %in% 3 & data$s3037 %in% 3 & data$s3039 %in% 3, 0, df$p_pain_12)

df$parent_pain_12 <- ifelse(df$m_pain_12 %in% 0 & df$p_pain_12 %in% 0, 0, NA)
df$parent_pain_12 <- ifelse(df$m_pain_12 %in% 0 & df$p_pain_12 %in% 0, 0, df$parent_pain_12)
df$parent_pain_12 <- ifelse(df$m_pain_12 %in% 1 | df$p_pain_12 %in% 1, 1, df$parent_pain_12)

# VALIDATED SCALES

## Short Form Health Survey
lab_sr_health <- c("Poor/Fair","Poor/Fair","Good","Very good","Excellent")
lab_pain_intf     <- c("Not at all","A Little bit","Moderately","Quite a bit/Extremely","Quite a bit/Extremely")
lab_mostall <- c("None of the time","A little of the time","Some of the time","Most/All of the time","Most/All of the time")
lab_nonelittle <- c('None/A little of the time', 'None/A little of the time', 'Some of the time', 'Most of the time', 'All of the time')

### age 22 (12-Item)

sf12_age22_items <- c('YPB1000', 'YPB1010', 'YPB1011', 'YPB1020', 'YPB1021', 'YPB1030',
                      'YPB1031', 'YPB1040', 'YPB1050', 'YPB1051', 'YPB1052', 'YPB1060')

sf12_age22_factors = data %>%
  select(all_of(sf12_age22_items)) %>%
  mutate(
    sf12_sr_hlth_22           = factor(YPB1000, ordered = T, levels = 5:1, labels = lab_sr_health),
    sf12_hlth_limt_mod_act_22 = factor(YPB1010, levels = 3:1, labels = c('No','Yes','Yes')),
    sf12_hlth_limt_stairs_22  = factor(YPB1011, levels = 3:1, labels = c('No','Yes','Yes')),
    sf12_ph_accomp_less_22    = factor(YPB1020, ordered = T, levels = 5:1, labels = lab_mostall),
    sf12_ph_limt_wrk_act_22   = factor(YPB1021, ordered = T, levels = 5:1, labels = lab_mostall),
    sf12_emo_accomp_less_22   = factor(YPB1030, ordered = T, levels = 5:1, labels = lab_mostall),
    sf12_emo_limt_wrk_act_22  = factor(YPB1031, ordered = T, levels = 5:1, labels = lab_mostall),
    sf12_pain_intf_wrk_22     = factor(YPB1040, ordered = T, levels = 1:5, labels = lab_pain_intf),
    sf12_calm_peace_22        = factor(YPB1050, ordered = T, levels = 5:1, labels = lab_nonelittle),
    sf12_energy_22            = factor(YPB1051, ordered = T, levels = 5:1, labels = lab_nonelittle),
    sf12_down_depr_22         = factor(YPB1052, ordered = T, levels = 5:1, labels = lab_mostall),
    sf12_limt_soc_act_22      = factor(YPB1060, ordered = T, levels = 5:1, labels = lab_mostall)
  ) %>%
  select(-all_of(sf12_age22_items))

df <- dplyr::bind_cols(df, sf12_age22_factors)

### age 18 (36-Item) - auxiliary for imputation
sf36_age18_items <- c('cct3500','cct3505','cct3506','cct3508','cct3512','cct3514','cct3521','cct3522','cct3531','cct3532',
                      'cct3536','cct3537','cct3540','cct3543','cct3544','cct3545','cct3547','cct3548','cct3560','cct3567')

sf36_age18_factors = data %>%
  select(all_of(sf36_age18_items)) %>%
  mutate(
    sf36_sr_hlth_18                = factor(cct3500, ordered = T, levels = 5:1, labels = lab_sr_health),
    sf36_hlth_limt_vig_act_18      = factor(cct3505, levels = 3:1, labels = c('No','Yes','Yes')),
    sf36_hlth_limt_mod_act_18      = factor(cct3506, ordered = T, levels = 3:1, labels = c('No','Yes','Yes')),
    sf36_hlth_limt_stairs_18       = factor(cct3508, ordered = T, levels = 3:1, labels = c('No','Yes','Yes')),
    sf36_hlth_limt_hlf_mile_18     = factor(cct3512, ordered = T, levels = 3:1, labels = c('No','Yes','Yes')),
    sf36_hlth_limt_bath_dress_18   = factor(cct3514, ordered = T, levels = 3:1, labels = c('No','Yes','Yes')),
    sf36_ph_accomp_less_18         = factor(cct3521, ordered = T, levels = 5:1, labels = lab_mostall),
    sf36_ph_limt_wrk_act_18        = factor(cct3522, ordered = T, levels = 5:1, labels = lab_mostall),
    sf36_emo_accomp_less_18        = factor(cct3531, ordered = T, levels = 5:1, labels = lab_mostall),
    sf36_emo_wrk_less_caref_18     = factor(cct3532, ordered = T, levels = 5:1, labels = lab_mostall),
    sf36_bodily_pain_18            = factor(cct3536, ordered = T, levels = 1:6, labels = c('None','Very Mild','Mild','Moderate','Severe','Severe')),
    sf36_bodpain_intf_wrk_18       = factor(cct3537, ordered = T, levels = 1:5, labels = lab_pain_intf),
    sf36_full_of_life_18           = factor(cct3540, ordered = T, levels = 5:1, labels = lab_nonelittle),
    sf36_calm_peace_18             = factor(cct3543, ordered = T, levels = 5:1, labels = lab_nonelittle),
    sf36_energy_18                 = factor(cct3544, ordered = T, levels = 5:1, labels = lab_nonelittle),
    sf36_down_low_18               = factor(cct3545, ordered = T, levels = 5:1, labels = lab_mostall),
    sf36_been_happy_18             = factor(cct3547, ordered = T, levels = 5:1, labels = lab_nonelittle),
    sf36_felt_tired_18             = factor(cct3548, ordered = T, levels = 5:1, labels = lab_mostall),
    sf36_limt_soc_act_18           = factor(cct3560, ordered = T, levels = 5:1, labels = lab_mostall),
    sf36_expect_hlth_wrs_18        = factor(cct3567, ordered = T, levels = c(3,4,2,1), labels = c('Definitely false','Mostly false','Mostly true','Definitely true')) #excludes don't knows
    ) %>%
  select(-all_of(sf36_age18_items))
df <- dplyr::bind_cols(df, sf36_age18_factors)


## 13-Item Short Mood and Feelings Questionnaire

### age 23
mfq_items <- c("YPC1650","YPC1651","YPC1653","YPC1654","YPC1655","YPC1656",
               "YPC1658","YPC1659","YPC1660","YPC1662","YPC1663","YPC1665","YPC1667")

mfq_dat <- data[,mfq_items]

mfq_dat <- mfq_dat %>%
  mutate(across(all_of(mfq_items), ~ case_when(
    . == 1 ~ 0,
    . == 2 ~ 1,
    . == 3 ~ 2
    )))

df$smfq_sc_23 <- rowSums(mfq_dat, na.rm=FALSE)

### age 18 - auxiliary for imputation
df$smfq_sc_18 <- as.numeric(data$cct2715)
df$smfq_sc_18[df$smfq_sc_18<0] <- NA

## ONS Wellbeing Indicators
ons_wellb_dat <- data %>%
  select(
    ons_wellb_life_satis_23 = YPC0450,
    ons_wellb_things_done_worthw_23 = YPC0460,
    ons_wellb_happy_yest_23 = YPC0470,
    ons_wellb_anxious_yest_23 = YPC0480
  ) %>%
  mutate(across(where(is.numeric), ~ as.integer(replace(.x, .x < 0, NA))))

df <- dplyr::bind_cols(df, ons_wellb_dat)


## 14-Item Warwick-Edinburgh Mental Wellbeing Scale

### age 23
df$wellbeing_sc_23 <- as.numeric(data$YPC0600)
df$wellbeing_sc_23[df$wellbeing_sc_23<0] <- NA

### age 17.5 - auxiliary for imputation
df$wellbeing_sc_18 <- as.numeric(data$CCXD814)
df$wellbeing_sc_18[df$wellbeing_sc_18<0] <- NA

## 4-Item Subjective Happiness Scale
df$subj_happy_sc_23 <- as.numeric(data$YPC0601)
df$subj_happy_sc_23[df$subj_happy_sc_23<0] <- NA


## 5-Item Satisfaction with Life Scale
df$life_satis_sc_23 <- as.numeric(data$YPC0602)
df$life_satis_sc_23[df$life_satis_sc_23<0] <- NA


## 10-Item Meaning in Life Questionnaire

### Total Composite
df$meaning_life_tot_sc_23 <- as.numeric(data$YPC0603)
df$meaning_life_tot_sc_23[df$meaning_life_tot_sc_23<0] <- NA

### Presence Subscale Composite
df$meaning_life_presc_sc_23 <- as.numeric(data$YPC0604)
df$meaning_life_presc_sc_23[df$meaning_life_presc_sc_23<0] <- NA

### Search Subscale Composite
df$meaning_life_search_sc_23 <- as.numeric(data$YPC0605)
df$meaning_life_search_sc_23[df$meaning_life_search_sc_23<0] <- NA


## 21-Item Basic Psychological Need Satisfaction Scale

### Total Composite
df$basic_psych_needs_tot_sc_23 <- as.numeric(data$YPC0606)
df$basic_psych_needs_tot_sc_23[df$basic_psych_needs_tot_sc_23<0] <- NA

### Autonomy Subscale Composite
df$basic_psych_needs_auton_sc_23 <- as.numeric(data$YPC0607)
df$basic_psych_needs_auton_sc_23[df$basic_psych_needs_auton_sc_23<0] <- NA

### Competence Subscale Composite
df$basic_psych_needs_compet_sc_23 <- as.numeric(data$YPC0608)
df$basic_psych_needs_compet_sc_23[df$basic_psych_needs_compet_sc_23<0] <- NA

### Relatedness Subscale Composite
df$basic_psych_needs_relat_sc_23 <- as.numeric(data$YPC0609)
df$basic_psych_needs_relat_sc_23[df$basic_psych_needs_relat_sc_23<0] <- NA


## 6-Item Gratitude Questionnaire
df$gratitude_sc_23 <- as.numeric(data$YPC0610)
df$gratitude_sc_23[df$gratitude_sc_23<0] <- NA


## 10-Item Life Orientation Test-Revised
df$optimism_sc_23 <- as.numeric(data$YPC0611)
df$optimism_sc_23[df$optimism_sc_23<0] <- NA


## Computerised Interview Schedule – Revised derived mental health diagnosis variables (Raw CIS-R FKDQ2000 to FKDQ7000)

### age 24
cisr_dat <- data %>%
  select(
    cisr_mild_depres_24 = FKDQ1000,
    cisr_mod_depres_24 = FKDQ1010,
    cisr_sev_depres_24 = FKDQ1020,
    cisr_general_anxiety_24 = FKDQ1030,
    cisr_soc_phobia_24 = FKDQ1050,
    cisr_spec_iso_phobia_24 = FKDQ1060,
    cisr_panic_disor_24 = FKDQ1070,
    cisr_panic_att_symp_24 = FKDQ1080,
    cisr_chron_fatig_24 = FKDQ1110
  ) %>%
  mutate(across(where(is.numeric), ~ as.integer(replace(.x, .x < 0, NA))))

cisr_dat$cisr_mild_depres_24 <- factor(cisr_dat$cisr_mild_depres_24, levels = 0:1, labels = c('No','Yes'))
cisr_dat$cisr_general_anxiety_24 <- factor(cisr_dat$cisr_general_anxiety_24, levels = 0:1, labels = c('No','Yes'))
cisr_dat$cisr_chron_fatig_24 <- factor(cisr_dat$cisr_chron_fatig_24, levels = 0:1, labels = c('No','Yes'))

df <- dplyr::bind_cols(df, cisr_dat)

### age 17.5 - auxiliary for imputation
df$cisr_mild_depres_18 <- factor(data$FJCI603, levels = 0:1, labels = c('No','Yes'))
df$cisr_general_anxiety_18 <- factor(data$FJCI602, levels = 0:1, labels = c('No','Yes'))

## CIS-R health conditions (diabetes, asthma/COPD, arthritis, heart disease/problems, stroke/cancer, kidney disease, mental health problems)

### age 24
df <- df %>%
  mutate(
    cisr_hlth_cond_22 = case_when(
      data$FKDQ2510 == 9 ~ 1,
      data$FKDQ2510 == 8 ~ 2,
      data$FKDQ2510 %in% 1:7 ~ 3
    ),
    cisr_hlth_cond_22 = factor(cisr_hlth_cond_22,
                               levels = 1:3,
                               labels = c('None', 'Mental health problems', 'Physical health conditions'))
  )


## 8-Item Standardised Assessment of Personality – Abbreviated Scale
sapas_dat <- data %>%
  select(
    sapas_diffic_friends_24 = FKPE1000,
    sapas_loner_24 = FKPE1010,
    sapas_trusts_oth_24 = FKPE1020,
    sapas_los_temper_24 = FKPE1030,
    sapas_impulsive_24 = FKPE1040,
    sapas_worrier_24 = FKPE1050,
    sapas_depends_oth_24 = FKPE1060,
    sapas_perfectionist_24 = FKPE1070
  ) %>%
  mutate(across(where(is.numeric), ~ as.integer(replace(.x, .x < 0, NA))))

sapas_dat$sapas_trusts_oth_24 <- ifelse(is.na(sapas_dat$sapas_trusts_oth_24), NA, ifelse(sapas_dat$sapas_trusts_oth_24 == 0, 1, 0))

df$sapas_personality_dis_sc_24 <- rowSums(sapas_dat, na.rm=F)


## Dysfunctional Attitude Scale score  - auxiliary for imputation
df$dis_func_att_sc_18 <- as.numeric(data$FJLE310)
df$dis_func_att_sc_18[df$dis_func_att_sc_18<0] <- NA


## 20-Item Short UPPS-P Impulsive Behavior Scale 
upps_map <- list(
  nu = c("YPD5050","YPD5070","YPD5120","YPD5140"), # Negative Urgency
  pu = c("YPD5020","YPD5090","YPD5160","YPD5190"), # Positive Urgency
  ss = c("YPD5080","YPD5130","YPD5150","YPD5170"), # Sensation Seeking
  lps = c("YPD5000","YPD5030","YPD5060","YPD5100"), # Lack of Perseverance
  lpm = c("YPD5010","YPD5040","YPD5110","YPD5180") # Lack of Premeditation
  )

upps_dat <- data[,unlist(upps_map)] %>%
  mutate(across(where(is.numeric), ~ as.integer(replace(.x, .x < 0, NA))))

upps_rev_items <- c(upps_map$nu, upps_map$pu, upps_map$ss)
upps_dat <- upps_dat %>% mutate(across(all_of(upps_rev_items), ~ 5L - .x))

supps_scores <- data.frame(
  supps_neg_urg_24 = rowMeans(upps_dat[upps_map$nu], na.rm=F),
  supps_pos_urg_24 = rowMeans(upps_dat[upps_map$pu], na.rm=F),
  supps_lack_premed_24 = rowMeans(upps_dat[upps_map$lpm], na.rm=F),
  supps_lack_persev_24 = rowMeans(upps_dat[upps_map$lps], na.rm=F),
  supps_sens_seek_24 = rowMeans(upps_dat[upps_map$ss], na.rm=F)
) %>%
  mutate(supps_total_25 = supps_neg_urg_24 + supps_pos_urg_24 + supps_lack_premed_24 + supps_lack_persev_24 + supps_sens_seek_24)

df <- dplyr::bind_cols(df, supps_scores)


## Arnett Inventory of Sensation Seeking subscales
df$aiss_novelty_sc_18 <- as.numeric(data$cct2030)
df$aiss_novelty_sc_18[df$aiss_novelty_sc_18<0] <- NA

df$aiss_intensity_sc_18 <- as.numeric(data$cct2031)
df$aiss_intensity_sc_18[df$aiss_intensity_sc_18<0] <- NA


## 25-Item Strengths and Difficulties Questionnaire (YPE1175-YPE1182 are SDQ impact items not part of core subscales)
sdq_map <- list(
  pro = c('YPE1150', 'YPE1153', 'YPE1158', 'YPE1166', 'YPE1169'),
  emo = c('YPE1152', 'YPE1157', 'YPE1162', 'YPE1165', 'YPE1173'),
  con = c('YPE1154', 'YPE1156', 'YPE1161', 'YPE1167', 'YPE1171'),
  hyp = c('YPE1151', 'YPE1159', 'YPE1164', 'YPE1170', 'YPE1174'),
  peer= c('YPE1155', 'YPE1160', 'YPE1163', 'YPE1168', 'YPE1172')
)

sdq_core <- data %>%
  select(YPE1150:YPE1174) %>% # the 25 core items
  mutate(across(everything(), ~ as.integer(replace(.x, .x < 0, NA))))

sdq_rev_items <- c("YPE1156","YPE1160","YPE1163","YPE1170","YPE1174")
sdq_core <- sdq_core %>% mutate(across(all_of(sdq_rev_items), ~ 2 - .x))

sdq_scores <- data.frame(
  sdq_prosocial_25 = rowSums(sdq_core[sdq_map$pro], na.rm=F),
  sdq_emotional_25 = rowSums(sdq_core[sdq_map$emo], na.rm=F),
  sdq_conduct_25 = rowSums(sdq_core[sdq_map$con], na.rm=F),
  sdq_hyperact_25 = rowSums(sdq_core[sdq_map$hyp], na.rm=F),
  sdq_peer_25 = rowSums(sdq_core[sdq_map$peer], na.rm=F)
  ) %>%
  mutate(sdq_total_25 = sdq_emotional_25 + sdq_conduct_25 + sdq_hyperact_25 + sdq_peer_25)

df <- dplyr::bind_cols(df, sdq_scores)

df <- df %>%
  mutate(
    sdq_any_diffic_25 = case_when(
      data$YPE1175 == 0 ~ 1, 
      data$YPE1175 == 1 ~ 2,
      data$YPE1175 %in% 2:3 ~ 3
    ),
    sdq_any_diffic_25 = factor(sdq_any_diffic_25,
                               levels = 1:3,
                               ordered = T,
                               labels = c('No', 'Yes, minor difficulties', 'Yes, definite/severe difficulties'))
  )


## 44-Item Screen for Adult Anxiety Related Disorders
scaared_map <- list(
  panic_somatic = c(# panic disorder/significant somatic symptoms
    "YPE2000","YPE2001","YPE2005","YPE2008","YPE2010","YPE2011","YPE2014",
    "YPE2016","YPE2017","YPE2018","YPE2021","YPE2024","YPE2027","YPE2031",
    "YPE2035","YPE2037","YPE2039"), 
  gad = c(# generalized anxiety disorder
    "YPE2004","YPE2006","YPE2007","YPE2013","YPE2020","YPE2022","YPE2023",
    "YPE2028","YPE2030","YPE2034","YPE2036","YPE2038","YPE2043"),
  separation = c(# separation anxiety
    "YPE2003","YPE2012","YPE2015","YPE2019","YPE2025","YPE2029","YPE2032"),
  social = c(# social phobia disorder
    "YPE2002","YPE2009","YPE2026","YPE2033","YPE2040","YPE2041","YPE2042")
  )

scaared_dat <- data %>%
  select(YPE2000:YPE2043) %>%
  mutate(across(everything(), ~ as.integer(replace(.x, .x < 0, NA))))

df$scaared_total_25 <- rowSums(scaared_dat, na.rm=F)


## 50-Item Adult Autism Spectrum Quotient
aq50_dat <- data %>%
  select(sprintf("YPE55%02d", 10:59)) %>%
  mutate(across(everything(), ~ as.integer(replace(.x, .x < 0, NA))))

agree_items_num <- c(2,4,5,6,7,9,12,13,16,18,19,20,21,22,23,26,33,35,39,41,42,43,45,46)
agree_items <- sprintf("YPE55%02d", 9 + agree_items_num)
disagree_items  <- setdiff(names(aq50_dat), agree_items)

aq_scored <- aq50_dat %>%
  dplyr::mutate(
    across(all_of(agree_items),    ~ dplyr::case_when(.x %in% c(2L,3L) ~ 1L, .x %in% c(0L,1L) ~ 0L)),
    across(all_of(disagree_items), ~ dplyr::case_when(.x %in% c(0L,1L) ~ 1L, .x %in% c(2L,3L) ~ 0L))
  )

df$aq_autism_quot_sc_25 <- rowSums(aq_scored, na.rm=F)

social_skill_nums      <- c(1,11,13,15,22,36,44,45,47,48)
attention_switch_nums  <- c(2,4,10,16,25,32,34,37,41,43)
attention_detail_nums  <- c(5,6,9,12,19,23,28,29,30,49)
communication_nums     <- c(7,17,18,26,27,31,33,35,38,39)
imagination_nums       <- c(3,8,14,20,21,24,40,42,46,50)

df$aq_social_skill_sc_25      <- rowSums(aq_scored[,social_skill_nums], na.rm=F)
df$aq_attention_switch_sc_25  <- rowSums(aq_scored[,attention_switch_nums], na.rm=F)
df$aq_attention_detail_sc_25  <- rowSums(aq_scored[,attention_detail_nums], na.rm=F)
df$aq_communication_sc_25     <- rowSums(aq_scored[,communication_nums], na.rm=F)
df$aq_imagination_sc_25       <- rowSums(aq_scored[,imagination_nums], na.rm=F)


## Affective Reactivity Index
ari_symptoms <- c("YPE5503","YPE5504","YPE5505","YPE5506","YPE5507","YPE5508")
ari_impair   <- "YPE5509" #impairment - irritability caused problems
ari_extras   <- c("YPE5500","YPE5501","YPE5502") #extras not in core ARI - temper, touchy/annoyed & angry/resentful

ari_dat <- data %>%
  select(all_of(c(ari_symptoms, ari_impair, ari_extras))) %>%
  mutate(across(everything(), ~ as.integer(replace(.x, .x < 0, NA))))

df$ari_affect_react_sc_25 <- rowSums(ari_dat[,ari_symptoms])


## NIH Toolbox Adult Toolbox Social Relationship Scales - 8-Item Emotional support & Instrumental support subscales
emo_items <- c("YPE7580","YPE7581","YPE7582","YPE7583","YPE7584","YPE7585","YPE7586","YPE7587") # Emotional support
ins_items <- c("YPE7588","YPE7589","YPE7590","YPE7591","YPE7592","YPE7593","YPE7594","YPE7595") # Instrumental support

atsrs_dat <- data %>%
  select(all_of(c(emo_items, ins_items))) %>%
  mutate(across(everything(), ~ as.integer(replace(.x, .x < 0, NA))))

atsrs_scores <- data.frame(
  atsrs_emo_support_sc_25 = rowSums(atsrs_dat[,emo_items], na.rm=F),
  atsrs_prac_support_sc_25 = rowSums(atsrs_dat[,ins_items], na.rm=F)
)
df <- dplyr::bind_cols(df, atsrs_scores)



# COGNITIVE TASKS

# Working Memory: Signal-detection ability (from N-back task) - Higher d’ indicates better performance (code from https://royalsocietypublishing.org/doi/full/10.1098/rsos.221161 )

## age 24
data$hits <- as.numeric(data$FKEP2030)
data$hits[data$hits<0] <- NA
data$hits_prop <- data$hits/8 #proportion of raw hits

data$false <- as.numeric(data$FKEP2020)
data$false[data$false<0] <- NA
data$false_prop <- data$false/40 #proportion of false alarms

data$hits_prop[data$hits_prop==1] <- 0.9375 # correct perfect hits to 1-1/(2n) where n = total hits (8)
data$hits_prop[data$hits_prop==0] <- 0.0625 

data$false_prop[data$false_prop==1] <- 0.9875 
data$false_prop[data$false_prop==0] <- 0.0125 # correct zero false alarms to 1/(2n) where n = total false alarms (40)

df$work_mem_24 <- qnorm(data$hits_prop) - qnorm(data$false_prop)

df$work_mem_24[df$work_mem_24<0] <- NA #remove people with d'< 0
df$work_mem_24[data$FKEP2050>24] <- NA #remove people who responded to < 50% trials

#age 18
# Function to calculate d-prime from N-back mean accuracy variables
get_dprime_from_acc <- function(target_acc, nontarget_acc, eps = 0.001) {
  
  # Convert to numeric
  hit_rate <- as.numeric(target_acc)
  nontarget_acc <- as.numeric(nontarget_acc)
  
  # Remove ALSPAC missing codes
  hit_rate[hit_rate<0] <- NA
  nontarget_acc[nontarget_acc<0] <- NA
  
  # False alarm rate = 1 - accuracy on non-target trials
  fa_rate <- 1 - nontarget_acc
  
  # Boundary correction, because qnorm(0) and qnorm(1) are infinite
  hit_rate <- pmin(pmax(hit_rate, eps), 1 - eps)
  fa_rate  <- pmin(pmax(fa_rate, eps), 1 - eps)
  
  # d-prime
  dprime <- qnorm(hit_rate) - qnorm(fa_rate)
  
  # Optional, matching your earlier code:
  # negative d-prime implies worse-than-chance discrimination
  dprime[dprime < 0] <- NA
  
  return(dprime)
}

df$work_mem_18 <- get_dprime_from_acc(
  target_acc    = data$FJNB100,
  nontarget_acc = data$FJNB050
)

#df$nback_3_dprime_18 <- get_dprime_from_acc(
#  target_acc    = data$FJNB300,
#  nontarget_acc = data$FJNB250
#)


# Response inhibition: Stop-Signal Task: 'Stop' signal reaction time (ms) - Lower SSRTs indicate better response inhibition
df$resp_inhib_24 <- as.numeric(data$FKEP3060)
df$resp_inhib_24[df$resp_inhib_24<0] <- NA

# Response inhibition: Affective Go/No-Go Task: Errors/false alarms = failures to inhibit response - Lower values indicate better response inhibition
df$resp_inhib_18 <- as.numeric(data$FJGO200)
df$resp_inhib_18[df$resp_inhib_18<0] <- NA


# PHYSICAL ACTIVITY/SLEEP/(DIET)/WEIGHT CHANGE

## Make regular walking or cycling journeys every day or on most days
df <- df %>%
  mutate(
    actv_jrnys_22 = case_when(
      data$YPB2000 == 1 ~ 0,
      data$YPB2000 %in% 2:4 ~ 1
    ),
    actv_jrnys_22 = factor(actv_jrnys_22,
                           levels = 0:1,
                           labels = c('No', 'Yes'))
  )

## Frequency taking part in strenuous/vigorous activity
df <- df %>%
  mutate(
    vigact_freq_22  = case_when(#excludes participates in physical activity, but unsure on frequency
      data$YPB2040 == 1 ~ 1,
      data$YPB2040 %in% 2:3 ~ 2,
      data$YPB2040 %in% 4:6 ~ 3 
    ),
    vigact_freq_22 = factor(vigact_freq_22,
                            levels = 1:3,
                            ordered = T,
                            labels = c('Never', 'Once a fortnight or less', 'Weekly or more'))
  )

## Frequency respondent exercised (going to gym, brisk walking, any sports activity) during past year
df <- df %>%
  mutate(
    exercise_18  = case_when(
      data$cct4105 == 5 ~ 1,
      data$cct4105 %in% 3:4 ~ 2,
      data$cct4105 %in% 1:2 ~ 3 
    ),
    exercise_18 = factor(exercise_18,
                            levels = 1:3,
                            ordered = T,
                            labels = c('Never', '1-3 times a month or less', 'Weekly or more'))
  )


## Screen time - based on number of hours per day spent sitting and watching TV/playing video games/using a computer or laptop/using phone, tablet or e-book on an average weekday/weekend
wd <- c(tv="YPB2060", vg="YPB2061", pc="YPB2062", ph="YPB2063")
we <- c(tv="YPB2070", vg="YPB2071", pc="YPB2072", ph="YPB2073")
wd_vars <- unname(wd)
we_vars <- unname(we)
band_labels <- c("None", "Less than 1 hour", "1–2 hours", "3–4 hours", "5–6 hours", "7–8 hours", "9 hours or more")

screen_maxband <- data %>%
  select(all_of(c(wd_vars, we_vars))) %>%
  mutate(across(everything(), ~ {
    x <- as.numeric(.x) # labelled<double> to numeric codes
    x[x < 0] <- NA # set ALSPAC negative missing codes to NA
    x
  })) %>%
  rowwise() %>%
  mutate(
    max_band_wd = if (all(is.na(c_across(all_of(wd_vars))))) NA
    else max(c_across(all_of(wd_vars)), na.rm=T),
    max_band_we = if (all(is.na(c_across(all_of(we_vars))))) NA
    else max(c_across(all_of(we_vars)), na.rm=T),
    
    # combine weekday + weekend into one weekly summary band
    max_band_weekly = if (is.na(max_band_wd) | is.na(max_band_we)) NA
    else round((5 * max_band_wd + 2 * max_band_we) / 7)
    ) %>%
  ungroup() %>%
  mutate(
    max_band_wd = factor(max_band_wd, levels = 1:7, ordered = T, labels = band_labels),
    max_band_we = factor(max_band_we, levels = 1:7, ordered = T, labels = band_labels),
    max_band_weekly = factor(max_band_weekly, levels = 1:7, ordered = T, labels = band_labels)
  ) %>%
  select(max_band_wd, max_band_we, max_band_weekly)

df$screen_time_22 <- factor(screen_maxband$max_band_weekly, ordered = T, levels = band_labels, labels = c("2 hours or less", "2 hours or less", "2 hours or less", "3–4 hours", "5-6 hours", "7 hours or more", "7 hours or more"))

## Number of hours per day spent reading books for pleasure on an average weekday/weekend day
read_maxband <- data %>%
  transmute(
    across(all_of(c('YPB2066', 'YPB2076')), ~ {
      x <- as.numeric(.x)
      x[x < 0] <- NA
      x
    }),
    reading_typ = case_when(
      is.na(YPB2066) | is.na(YPB2076) ~ NA,
      TRUE ~ round((5 * YPB2066 + 2 * YPB2076) / 7)
    )
  ) %>%
  mutate(
    reading_typ = factor(reading_typ, levels = 1:7, ordered = TRUE, labels = band_labels)
  )

df$reading_time_22 <- factor(read_maxband$reading_typ, ordered = T, levels = band_labels, labels = c("None", "Less than 1 hour", "1 hour or more", "1 hour or more", "1 hour or more", "1 hour or more", "1 hour or more"))

## Time spent outdoors on average
outdoor_maxband <- data %>%
  transmute(
    across(all_of(c('YPB2064','YPB2074','YPB2065','YPB2075')), ~ {
      x <- as.numeric(.x)
      x[x < 0] <- NA
      x
    }),
    # weekly typical band (1–7) within each season
    summer_typ = case_when(
      is.na(YPB2064) | is.na(YPB2074) ~ NA,
      TRUE ~ round((5 * YPB2064 + 2 * YPB2074) / 7)
      ),
    
    winter_typ = case_when(
      is.na(YPB2065) | is.na(YPB2075) ~ NA,
      TRUE ~ round((5 * YPB2065 + 2 * YPB2075) / 7)
      ),
    
    #seasonality where +ve = more outdoors in summer
    seasonality = summer_typ - winter_typ,  
    
    #overall “year-round typical” (average of seasonal typ)
    outdoors_typ = case_when(
      is.na(summer_typ) | is.na(winter_typ) ~ NA,
      TRUE ~ round((summer_typ + winter_typ) / 2)
      )
  ) %>%
  mutate(
    summer_typ  = factor(summer_typ, levels = 1:7, ordered = TRUE, labels = band_labels),
    winter_typ  = factor(winter_typ, levels = 1:7, ordered = TRUE, labels = band_labels),
    outdoors_typ = factor(outdoors_typ, levels = 1:7, ordered = TRUE, labels = band_labels)
    )

df$outdoor_time_22 <- factor(outdoor_maxband$outdoors_typ, ordered = T, levels = band_labels, labels = c("Less than 1 hour", "Less than 1 hour", "1-2 hours", "3-4 hours", "5 hours or more", "5 hours or more", "5 hours or more"))


## Number of hours YP sleeps in 24 hours
df$n_hrs_sleep_25 <- as.numeric(data$YPE7440)
df$n_hrs_sleep_25[df$n_hrs_sleep_25<0] <- NA

##Length of time YP sleeps on normal school night
sleepdf_16 <- data %>%
  mutate(
    # Convert to numeric and set ALSPAC missing codes to NA
    sleep_hours_tf3 = as.numeric(fh5440),
    sleep_mins_tf3  = as.numeric(fh5441),
    
    sleep_hours_tf3 = ifelse(sleep_hours_tf3 < 0, NA, sleep_hours_tf3),
    sleep_mins_tf3  = ifelse(sleep_mins_tf3 < 0, NA, sleep_mins_tf3),
    
    # Total sleep duration
    sleep_duration_mins_tf3 = sleep_hours_tf3 * 60 + sleep_mins_tf3,
    sleep_duration_hrs_16  = sleep_duration_mins_tf3 / 60
  ) %>%
  select(all_of(c('sleep_duration_mins_tf3','sleep_duration_hrs_16')))

df$sleep_duration_hrs_16 <- sleepdf_16$sleep_duration_hrs_16

## Frequency YP gets enough sleep
df <- df %>%
  mutate(
    enough_sleep_25  = case_when(
      data$YPE7490 %in% 0:1 ~ 1,
      data$YPE7490 == 2 ~ 2,
      data$YPE7490 %in% 3:4 ~ 3
    ),
    enough_sleep_25 = factor(enough_sleep_25,
                             levels = 1:3,
                             ordered = T,
                             labels = c('Never/Rarely', 'Sometimes', 'Usually/Always'))
  )

## Frequency YP gets enough sleep
df <- df %>%
  mutate(
    enough_sleep_16  = case_when(
      data$fh5341 %in% 5:4 ~ 1,
      data$fh5341 == 3 ~ 2,
      data$fh5341 %in% 2:1 ~ 3
    ),
    enough_sleep_16 = factor(enough_sleep_16,
                             levels = 1:3,
                             ordered = T,
                             labels = c('Never/Rarely', 'Sometimes', 'Usually/Always'))
  )

## During day time activities, YP has a problem with sleepiness
df <- df %>%
  mutate(
    daytime_tired_25  = case_when(
      data$YPE7480 == 0 ~ 1,
      data$YPE7480 == 1 ~ 2,
      data$YPE7480 %in% 2:4 ~ 3
    ),
    daytime_tired_25 = factor(daytime_tired_25,
                              levels = 1:3,
                              ordered = T,
                              labels = c('No problem', 'Little problem', 'More than a little/Big problem'))
    )

## Problem YP has with sleepiness during daytime activities
df <- df %>%
  mutate(
    daytime_tired_16  = case_when(
      data$fh5334 == 1 ~ 1,
      data$fh5334 == 2 ~ 2,
      data$fh5334 %in% 3:5 ~ 3
    ),
    daytime_tired_16 = factor(daytime_tired_16,
                              levels = 1:3,
                              ordered = T,
                              labels = c('No problem', 'Little problem', 'More than a little/Big problem'))
  )


## YP is conscious of what they are eating
df <- df %>%
  mutate(
    eat_conscious_25  = case_when(
      data$YPE8090 == 0 ~ 1,
      data$YPE8090 == 1 ~ 2,
      data$YPE8090 == 2 ~ 3,
      data$YPE8090 == 3 ~ 4
    ),
    eat_conscious_25 = factor(eat_conscious_25,
                              levels = 1:4, 
                              ordered = T,
                              labels = c('Not at all', 'Slightly', 'Moderately', 'Very much'))
  )

## YP's weight has hardly changed at all in the last 5 years
df <- df %>%
  mutate(
    weight_change_25  = case_when(
      data$YPE8043 == 0 ~ 0,
      data$YPE8043 == 1 ~ 1
      ),
    weight_change_25 = factor(weight_change_25,
                              levels = 0:1, 
                              labels = c('False', 'True'))
    )



# SUBSTANCE USE/ANTISOCIAL BEHAVIOUR

# Combined polysubstance (smoking cigarettes, vaping, cannabis use) use

## age 24
df <- df %>%
  mutate(smk_past_30_days_24  = case_when(
    data$FKSM1040 == 1 ~ 1,
    data$FKSM1040 %in% c(-3,0) ~ 0
  ))

df <- df %>%
  mutate(smk_every_day_24  = case_when(
    data$FKSM1060 == 1 ~ 1,
    data$FKSM1060 %in% c(-4,-3,0) ~ 0
  ))

df <- df %>%
  mutate(vape_current_24  = case_when(
    data$FKSM1210 == 1 ~ 1,
    data$FKSM1210 %in% c(-3,0) ~ 0
  ))
    

df <- df %>%
  mutate(cannabis_past_yr_24  = case_when(
    data$FKCA1016 %in% 1:5 ~ 1,
    data$FKCA1016 %in% c(-3,0) ~ 0
    ))

#df$smk_vape_cannabis_24 <- rowSums(df[,c('smk_every_day_24', 'vape_current_24', 'cannabis_past_yr_24')], na.rm=F)
df$smk_vape_cannabis_24 <- rowSums(df[,c('smk_past_30_days_24', 'vape_current_24', 'cannabis_past_yr_24')], na.rm=F)

df <- df %>%
  mutate(
    smk_vape_cannabis_24  = case_when(
      smk_vape_cannabis_24 == 0 ~ 1,
      smk_vape_cannabis_24 == 1 ~ 2,
      smk_vape_cannabis_24 %in% 2:3 ~ 3,
    ),
    smk_vape_cannabis_24 = factor(smk_vape_cannabis_24,
                                  levels = 1:3,
                                  ordered = T,
                                  labels = c('None', 'One', 'More than one'))
    )

## age 18
df <- df %>%
  mutate(smk_past_30_days_18  = case_when(
    data$cct5010 == 1 ~ 1,
    data$cct5010 %in% c(-2,2) ~ 0
  ))

df <- df %>%
  mutate(smk_every_day_18  = case_when(
    data$cct5012 == 1 ~ 1,
    data$cct5012 %in% c(-3,-2,2) ~ 0
  ))

df <- df %>%
  mutate(cannabis_past_yr_18  = case_when(
    data$cct5055 %in% 1:5 ~ 1,
    data$cct5055 %in% c(-2) ~ 0
  ))

#df$smk_vape_cannabis_18 <- rowSums(df[,c('smk_every_day_18', 'cannabis_past_yr_18')], na.rm=F)
df$smk_cannabis_18 <- rowSums(df[,c('smk_past_30_days_18', 'cannabis_past_yr_18')], na.rm=F)

df <- df %>%
  mutate(
    smk_cannabis_18  = case_when(
      smk_cannabis_18 == 0 ~ 1,
      smk_cannabis_18 == 1 ~ 2,
      smk_cannabis_18 == 2 ~ 3,
    ),
    smk_cannabis_18 = factor(smk_cannabis_18,
                                  levels = 1:3,
                                  ordered = T,
                                  labels = c('None', 'One', 'Both'))
  )


# During past year frequency had a drink containing alcohol

## age 24
df <- df %>%
  mutate(
    alc_freq_24  = case_when(
      data$FKAL1020 %in% c(-3,0,1) ~ 1,
      data$FKAL1020 == 2 ~ 2,
      data$FKAL1020 %in% 3:4 ~ 3
    ),
    alc_freq_24 = factor(alc_freq_24,
                         levels = 1:3,
                         ordered = T,
                         labels = c('Never/Monthly or less', '2-4 times a month', '2 or more times a week'))
  )

## age 18 - auxiliary for imputation
df <- df %>%
  mutate(
    alc_freq_18  = case_when(
      data$cct5030 %in% c(-2,1,2) ~ 1,
      data$cct5030 == 3 ~ 2,
      data$cct5030 %in% 4:5 ~ 3
    ),
    alc_freq_18 = factor(alc_freq_18,
                         levels = 1:3,
                         ordered = T,
                         labels = c('Never/Monthly or less', '2-4 times a month', '2 or more times a week'))
  )

# During past year frequency of drinking 6 or more units on one occasion

## age 24
df <- df %>%
  mutate(
    bing_drink_24  = case_when(
      data$FKAL1022 %in% c(-4,-3,0) ~ 1,
      data$FKAL1022 == 1 ~ 2,
      data$FKAL1022 == 2 ~ 3,
      data$FKAL1022 %in% 3:4 ~ 4
    ),
    bing_drink_24 = factor(bing_drink_24,
                           levels = 1:4,
                           ordered = T,
                           labels = c('Never/Not in last year', 'Less than monthly', 'Monthly', 'Weekly or more'))
  )

## age 18 - auxiliary for imputation
df <- df %>%
  mutate(
    bing_drink_18  = case_when(
      data$cct5032 %in% c(-3,1) ~ 1,
      data$cct5032 %in% c(2:3) ~ 2,
      data$cct5032 == 4 ~ 3,
      data$cct5032 %in% 5:6 ~ 4
    ),
    bing_drink_18 = factor(bing_drink_18,
                           levels = 1:4,
                           ordered = T,
                           labels = c('Never', 'Less than monthly', 'Monthly', 'Weekly or more'))
  )

# (Number of) illicit drugs ever used

## age 24
drug_vars24 <- c("FKDR1010","FKDR1020","FKDR1030","FKDR1040","FKDR1050",
                 "FKDR1060","FKDR1070","FKDR1080","FKDR1090")

data$drug_0count_24 <- rowSums(apply(data[drug_vars24], 2, \(x) x %in% 0))
data$drug_1count_24 <- rowSums(data[drug_vars24] == 1, na.rm=T)

df <- df %>%
  mutate(
    n_illict_drugs_24  = case_when(
      data$drug_0count_24 == 9 ~ 1,
      data$drug_1count_24 %in% 1:2 ~ 2,
      data$drug_1count_24 %in% 3:9 ~ 3
    ),
    n_illict_drugs_24 = factor(n_illict_drugs_24,
                               levels = 1:3,
                               ordered = T,
                               labels = c('None', 'One or two', 'Three or more'))
    )

## age 18 - auxiliary for imputation
drug_vars18 <- c("cct5100", "cct5110", "cct5120", "cct5130", "cct5140", "cct5150",
                 "cct5160", "cct5170", "cct5180")

data$drug_0count_18 <- rowSums(apply(data[drug_vars18], 2, \(x) x %in% 2))
data$drug_1count_18 <- rowSums(data[drug_vars18] == 1, na.rm=T)

df <- df %>%
  mutate(
    n_illict_drugs_18  = case_when(
      data$drug_0count_18 == 9 ~ 1,
      data$drug_1count_18 %in% 1:2 ~ 2,
      data$drug_1count_18 %in% 3:9 ~ 3
    ),
    n_illict_drugs_18 = factor(n_illict_drugs_18,
                               levels = 1:3,
                               ordered = T,
                               labels = c('None', 'One or two', 'Three or more'))
  )

# Anti-social behaviour (Frequency score for all ASB items)
df <- df %>%
  mutate(
    antisocialb_22  = case_when(
      data$YPB4492 == 0 ~ 0,
      data$YPB4492 %in% 1:12 ~ 1
    ),
    antisocialb_22 = factor(antisocialb_22,
                            levels = 0:1,
                            labels = c('No','Yes'))
  )



# FAMILY/SOCIAL CONTACTS/LIVING ARRANGEMENTS

# Respondent has any brothers and sisters (including any brothers and sisters who have passed away)
df <- df %>%
  mutate(
    sibling_23  = case_when(
      data$YPC1150 == 0 ~ 1,
      data$YPC1150 == 1 ~ 2
    ),
    sibling_23 = factor(sibling_23,
                        levels = 1:2,
                        labels = c('No', 'Yes'))
  )

# Number of brothers and sisters respondent has
df <- df %>%
  mutate(
    n_siblings_23  = case_when(
      data$YPC1150 == 0 ~ 1,
      data$YPC1160 == 1 ~ 2,
      data$YPC1160 > 1 ~ 3
    ),
    n_siblings_23 = factor(n_siblings_23,
                           levels = 1:3,
                           ordered = T,
                           labels = c('None', 'One', 'Two or more'))
    )


#Number of close friends YP has

## age 24
df <- df %>%
  mutate(
    n_friends_24  = case_when(
      data$FKFR1000 %in% 0:1 ~ 1,
      data$FKFR1000 == 2 ~ 2,
      data$FKFR1000 %in% c(3:6) ~ 3
    ),
    n_friends_24 = factor(n_friends_24,
                          levels = 1:3,
                          ordered = T,
                          labels = c('0-1', '2-4', '5+'))
  )

## age 18 - auxiliary for imputation
df <- df %>%
  mutate(
    n_friends_18  = case_when(
      data$FJPC050 %in% 1:2 ~ 1,
      data$FJPC050 == 3 ~ 2,
      data$FJPC050 %in% c(4:7) ~ 3
    ),
    n_friends_18 = factor(n_friends_18,
                          levels = 1:3,
                          ordered = T,
                          labels = c('0-1', '2-4', '5+'))
  )

#Frequency YP sees their mother
df <- df %>%
  mutate(
    freq_see_mum_25  = case_when(
      data$YPE7420 %in% 6:7 ~ 1,
      data$YPE7420 %in% 4:5 ~ 2,
      data$YPE7420 %in% 2:3 ~ 3,
      data$YPE7420 == 1 ~ 4
    ),
    freq_see_mum_25 = factor(freq_see_mum_25,
                             levels = 1:4,
                             ordered = T,
                             labels = c('No contact/passed away', 'One or two times a year or less', 'More than twice a year', 'Once a week or more'))
  )

#Frequency YP sees their father
df <- df %>%
  mutate(
    freq_see_dad_25  = case_when(
      data$YPE7430 %in% 6:7 ~ 1,
      data$YPE7430 %in% 4:5 ~ 2,
      data$YPE7430 %in% 2:3 ~ 3,
      data$YPE7430 == 1 ~ 4
    ),
    freq_see_dad_25 = factor(freq_see_dad_25,
                             levels = 1:4,
                             ordered = T,
                             labels = c('No contact/passed away', 'One or two times a year or less', 'More than twice a year', 'Once a week or more'))
  )

#Frequency YP sees parents
df <- df %>%
  mutate(
    mum_i = as.integer(freq_see_mum_25),
    dad_i = as.integer(freq_see_dad_25),
    both_obs = !is.na(mum_i) & !is.na(dad_i),
    
    freq_see_parents_25 = case_when(
      both_obs & (mum_i == 4 | dad_i == 4) ~ "At least one weekly",
      both_obs & (mum_i >= 3 | dad_i >= 3) ~ "Moderate (>= more than twice/year)",
      both_obs & (mum_i <= 2 & dad_i <= 2) ~ "Low (<= twice/year or never)",
      TRUE ~ NA
    ),
    freq_see_parents_25 = factor(
      freq_see_parents_25,
      levels = c("Low (<= twice/year or never)",
                 "Moderate (>= more than twice/year)",
                 "At least one weekly"),
      ordered = T
    )
  ) %>%
  select(-mum_i, -dad_i, -both_obs)

# How close YP feels to their parents
df <- df %>%
  mutate(
    feel_close_parents_18  = case_when(#no parents combined with not close, excludes don't know
      data$FJPC2000 %in% 3:5 ~ 1,
      data$FJPC2000 == 2 ~ 2,
      data$FJPC2000 == 1 ~ 3
    ),
    feel_close_parents_18 = factor(feel_close_parents_18,
                                   levels = 1:3,
                                   ordered = T,
                                   labels = c('No parents/Not close to either', 'Quite close to at least one', 'Very close to at least one'))
  )


#YP currently lives alone
df <- df %>%
  mutate(
    lives_alone_25  = case_when(
      data$YPE7403 == 0 ~ 0,
      data$YPE7403 == 1 ~ 1
    ),
    lives_alone_25 = factor(lives_alone_25,
                            levels = 0:1,
                            labels = c('No','Yes'))
  )


#Frequency YP visits any social media sites or apps
##Excludes don't know

#YPD9500 - YP has social media profile or account on any sites or apps - Nobody said no
df <- df %>%
  mutate(
    socmed_freq_24  = case_when(
      data$YPD9550 %in% 3:7 ~ 1,
      data$YPD9550 == 2 ~ 2,
      data$YPD9550 == 1 ~ 3
    ),
    socmed_freq_24 = factor(socmed_freq_24,
                            levels = 1:3,
                            ordered = T,
                            labels = c('Once a day or less', '2-10 times a day', '10+ times a day'))
  )


#YP is a parent (include biological, step, foster & adopted children)
df <- df %>%
  mutate(
    parent_25  = case_when(
      data$YPE0101 == 0 ~ 0,
      data$YPE0101 == 1 ~ 1
    ),
    parent_25 = factor(parent_25,
                       levels = 0:1,
                       labels = c('No', 'Yes'))
  )


#YP currently has a partner
df <- df %>%
  mutate(
    partner_25  = case_when(
      data$YPE7600 == 0 ~ 0,
      data$YPE7600 == 1 ~ 1
    ),
    partner_25 = factor(partner_25,
                        levels = 0:1,
                        labels = c('No', 'Yes'))
  )

#Partner support (Degree to which YP's partner meets their needs)
df <- df %>%
  mutate(
    partner_support_25  = case_when(
      data$YPE7601 == -2 ~ 1,
      data$YPE7601 %in% 1:4 ~ 2,
      data$YPE7601 == 5 ~ 3
    ),
    partner_support_25 = factor(partner_support_25,
                                levels = 1:3,
                                labels = c('No partner', 'Partner does not always meet needs', 'Partner always meets needs'))
  )



# EMPLOYMENT/INCOME/EDUCATION

## NS-SEC Occupational class (5 collapsed to 3 categories)
df <- df %>%
  mutate(#!doesn't include those not in work
    nssec_23  = case_when(
      data$YPC2492 == 1 ~ 3,
      data$YPC2492 %in% 2:3 ~ 2,
      data$YPC2492 %in% 4:5 ~ 1
    ),
    nssec_23 = factor(nssec_23,
                      levels = 1:3,
                      ordered = T,
                      labels = c(
                        'Lower supervisory and technical/Semi-routine and routine occupations',
                        'Intermediate occupations/Small employers and own account workers',
                        'Managerial, administrative and professional occupations'))
  )

## Employment status code
df <- df %>%
  mutate(
    employ_st_23  = case_when(
      data$YPC2493 %in% 1:3 ~ 2,
      data$YPC2493 == 4 ~ 1
    ),
    employ_st_23 = factor(employ_st_23,
                          levels = 1:2,
                          labels = c('Employee', 'Employer/Self-employed/Manager/Supervisor'))
  )

## Frequency YP attends school: Percentage
df <- df %>%
  mutate(
    school_attendance_16  = case_when( #none includes those not registered at school or college
      data$ccs1520 %in% c(0,7) ~ 1,
      data$ccs1520 %in% c(1:4) ~ 2,
      data$ccs1520 == 5 ~ 3,
      data$ccs1520 == 6 ~ 4
    ),
    school_attendance_16 = factor(school_attendance_16,
                                  levels = 1:4, ordered = T,
                                  labels = c('None/Not registered', 'About 60% or less', 'About 80%', '100%'))
    )

##  Likelihood of respondent applying to university for degree in next 5 years
df <- df %>%
  mutate(
    uni_likely_18  = case_when( #excludes don't know
      data$cct2993 == 4 ~ 1,
      data$cct2993 == 3 ~ 2,
      data$cct2993 == 2 ~ 3,
      data$cct2993 == 1 ~ 4
    ),
    uni_likely_18 = factor(uni_likely_18,
                           levels = 1:4, ordered = T,
                           labels = c('Not at all likely', 'Not very likely', 'Fairly likely', 'Very likely'))
  )

## Currently in full- or part-time education
df <- df %>%
  mutate(
    inedu_22  = case_when(
      data$YPB9080 == 3 ~ 0,
      data$YPB9080 %in% c(1,2,4) ~ 1
    ),
    inedu_22 = factor(inedu_22,
                      levels = 0:1,
                      labels = c('No', 'Yes'))
  )

## Currently doing voluntary work - Silent no's included
df <- df %>%
  mutate(
    volunteer_22  = case_when(
      data$YPB9006_imputeno == 2 ~ 0,
      data$YPB9006_imputeno == 1 ~ 1
    ),
    volunteer_22 = factor(volunteer_22,
                          levels = 0:1,
                          labels = c('No', 'Yes'))
  )

## Currently a full/part-time carer - Silent no's included
df <- df %>%
  mutate(
    carer_22  = case_when(
      data$YPB9008_imputeno == 1 ~ 1,
      data$YPB9008_imputeno == 2 ~ 0
    ),
    carer_22 = factor(carer_22,
                      levels = 0:1,
                      labels = c('No', 'Yes'))
  )

## Number of jobs that YP had since leaving school
df <- df %>%
  mutate(
    n_jobs_school_25  = case_when(
      data$YPE6030 %in% 0:1 ~ 1,
      data$YPE6030 %in% 2:3 ~ 2,
      data$YPE6030 == 4 ~ 3
    ),
    n_jobs_school_25 = factor(n_jobs_school_25,
                              levels = 1:3,
                              ordered = T,
                              labels = c('None or one', 'Two to Three', 'Four or more'))
    )

## YP's total take-home pay each month after tax & NI
df <- df %>%
  mutate(#assumes Not doing paid work lower than 1000
    income_25  = case_when(
      data$YPE6020 == 0 ~ 1,
      (data$YPE7470 == 0 & data$YPE6020 == -1) ~ 1,
      #(data$YPE6004 == 1 & data$YPE6020 == -1) ~ 1,
      #(data$YPE6005 == 1 & data$YPE6020 == -1) ~ 1,
      data$YPE6020 %in% 1:2 ~ 2,
      data$YPE6020 == 3 ~ 3,
      data$YPE6020 %in% 4:7 ~ 4
    ),
    income_25 = factor(income_25,
                       levels = 1:4,
                       ordered = T,
                       labels = c('Not doing paid work', '<1000', '1000-1499', '1500+'))
  )

# Ever been employed
df$ever_employed_20 <- factor(data$CCU4118, levels=c(2:1), labels=c('No','Yes'))




## YP has a job, job involves shift work/night shifts
df <- df %>%
  mutate(
    job_shift_nights_25 = case_when(
      data$YPE7470 == 0 ~ 0,
      (data$YPE7471 == 0 & data$YPE7472 == 0) ~ 0,
      (data$YPE7471 %in% 1:3 | data$YPE7472 %in% 1:3) ~ 1
    ),
    job_shift_nights_25 = factor(job_shift_nights_25,
                                 levels = 0:1,
                                 labels = c('No job/No shift work', 'Job involves nights/shift work'))
    )


## DV: Maximum UK (education) level attained
df <- df %>%
  mutate(
    highest_edu_26  = case_when(
      data$YPF7970 %in% 1:2 ~ 1,
      data$YPF7970 %in% 3:5 ~ 2,
      data$YPF7970 == 6 ~ 3,
      data$YPF7970 %in% 7:8 ~ 4,
    ),
    highest_edu_26 = factor(highest_edu_26,
                            levels = 1:4,
                            ordered = T,
                            labels = c('GCSE or equivalent', 'A-level/NVQ3-5/BTEC 3-5/HNC/HND', 'Degree', 'Masters/PGCE/PhD'))
  )


# ADVERSITY/(UNFAIR TREATMENT)/ACCIDENTS

#YP had big financial problems past year
df <- df %>%
  mutate(
    financ_prob_25  = case_when(
      data$YPE6710 %in% 1:4 ~ 1,
      data$YPE6710 == 0 ~ 0
    ),
    financ_prob_25 = factor(financ_prob_25,
                            levels = 0:1,
                            labels = c('No', 'Yes'))
  )

#YP was claiming any State Benefits/Tax Credits week ending this Sunday
df <- df %>%
  mutate(
    claim_benefits_25  = case_when(
      data$YPE6040 == 1 ~ 1,
      data$YPE6040 == 0 ~ 0
    ),
    claim_benefits_25 = factor(claim_benefits_25,
                               levels = 0:1,
                               labels = c('No', 'Yes'))
  )


# Bullied in last 6 months (based on Frequency respondent been Directly/Indirectly/Cyber Bullied)
df <- df %>%
  mutate(
    bullied_23 = case_when(
      (data$YPC1750 %in% 1:3 | data$YPC1770 %in% 1:3 | data$YPC1790 %in% 1:3) ~ 1,
      (data$YPC1750 == 0 & data$YPC1770 == 0 & data$YPC1790 == 0) ~ 0
    ),
    bullied_23 = factor(bullied_23,
                        levels = 0:1,
                        labels = c('No', 'Yes'))
  )

# When growing up respondent felt loved
df <- df %>%
  mutate(
    feltlov_grwup_23  = case_when(
      data$YPC1810 %in% 0:2 ~ 1,
      data$YPC1810 == 3 ~ 2,
      data$YPC1810 == 4 ~ 3,
    ),
    feltlov_grwup_23 = factor(feltlov_grwup_23,
                              levels = 1:3,
                              ordered = T,
                              labels = c('Never/Rarely/Sometimes true', 'Often true', 'Very often true'))
  )

# Respondent felt loved by someone they were in a relationship with since they were sixteen
df <- df %>%
  mutate(
    feltlov_relat_23  = case_when(
      data$YPC1820 %in% 0:2 ~ 1,
      data$YPC1820 == 3 ~ 2,
      data$YPC1820 == 4 ~ 3
    ),
    feltlov_relat_23 = factor(feltlov_relat_23,
                              levels = 1:3,
                              ordered = T,
                              labels = c('Never/Rarely/Sometimes true', 'Often true', 'Very often true'))
  )

# Composite relationship abuse/childhood maltreatment
## Someone respondent was in a relationship with deliberately hit them so hard it left them with bruises or marks since they were sixteen
## Someone respondent was in a relationship with attacked them or threatened them with a weapon (e.g. knife) or tried to choke them since they were sixteen
## Someone respondent was in a relationship with belittled them, threatened them, or stopped them from seeing friends or relatives since they were sixteen
## Someone respondent was in a relationship with sexually interfered with them, or forced them to have sex against their wishes since they were sixteen
## When growing up people in respondent's family hit them so hard that it left them with bruises or marks
## When growing up respondent felt that someone in their family hated them
## When growing up someone molested respondent (sexually)
trauma_vars <- data %>%
  transmute(
    relationship_abuse_23 = case_when(
      if_any(all_of(c("YPC1821", "YPC1822", "YPC1823", "YPC1824")), ~ .x %in% 1:4) ~ 1,
      if_all(all_of(c("YPC1821", "YPC1822", "YPC1823", "YPC1824")), ~ .x == 0) ~ 0
    ),
    relationship_abuse_23 = factor(
      relationship_abuse_23,
      levels = 0:1,
      labels = c("No", "Yes")
    ),
    
    childh_maltreat_23 = case_when(
      if_any(all_of(c("YPC1811", "YPC1812", "YPC1813")), ~ .x %in% 1:4) ~ 1,
      if_all(all_of(c("YPC1811", "YPC1812", "YPC1813")), ~ .x == 0) ~ 0
    ),
    childh_maltreat_23 = factor(
      childh_maltreat_23,
      levels = 0:1,
      labels = c("No", "Yes")
    )
  )
df <- bind_cols(df, trauma_vars)

# Respondent has ever experienced the sudden, unexpected death of someone close to them
df <- df %>%
  mutate(
    death_somclos_23  = case_when(
      data$YPC1829 == 0 ~ 0,
      data$YPC1829 == 1 ~ 1
    ),
    death_somclos_23 = factor(death_somclos_23,
                              levels = 0:1,
                              labels = c( 'No', 'Yes'))
  )

# Respondent has ever been in a serious accident or fire that they believed at the time might cause serious injury or death to them or someone else
df <- df %>%
  mutate(
    serious_accident_fire_23  = case_when(
      data$YPC1825 == 0 ~ 0,
      data$YPC1825 == 1 ~ 1
    ),
    serious_accident_fire_23 = factor(serious_accident_fire_23,
                                      levels = 0:1,
                                      labels = c( 'No', 'Yes'))
    )

# Respondent has ever experienced any other very traumatic or extremely stressful event
df <- df %>%
  mutate(
    oth_traum_stress_23  = case_when(
      data$YPC1830 == 0 ~ 0,
      data$YPC1830 == 1 ~ 1
      ),
    oth_traum_stress_23 = factor(oth_traum_stress_23,
                                 levels = 0:1,
                                 labels = c( 'No', 'Yes'))
    )

#!Better to create life events inventory score?

# (Total score for number of) bad experiences in last year
df <- df %>%
  mutate(
    bad_exps_24  = case_when(
      data$FKBE1017 > 0 ~ 1,
      data$FKBE1017 == 0 ~ 0
    ),
    bad_exps_24 = factor(bad_exps_24,
                         levels = 0:1,
                         labels = c('No', 'Yes'))
  )


# Self-harm history

## age 24
df <- df %>%
  mutate(
    selfharm_24  = case_when(
      data$FKSH1120 %in% 1:3 ~ 1,
      data$FKSH1120 == 0 ~ 0
    ),
    selfharm_24 = factor(selfharm_24,
                         levels = 0:1,
                         labels = c('No', 'Yes'))
  )

## age 20 - auxiliary for imputation
df <- df %>%
  mutate(
    selfharm_20  = case_when(
      data$CCU2040 == 1 ~ 1,
      data$CCU2040 == 2 ~ 0
    ),
    selfharm_20 = factor(selfharm_20,
                         levels = 0:1,
                         labels = c('No', 'Yes'))
  )


# BIOLOGICAL/BIOMARKERS

## C-reactive Protein mg/l

### age 24
df$crp_24 <- as.numeric(data$CRP_F24)
df$crp_24[df$crp_24<0] <- NA
#df$crp_24[df$crp_24>10] <- NA #114 possible infection - remove outliers?
df$log_crp_24 <- log(df$crp_24)

### age 18 - auxiliary for imputation
df$crp_18 <- as.numeric(data$CRP_TF4)
df$crp_18[df$crp_18<0] <- NA
df$log_crp_18 <- log(df$crp_18)

## Interleukin-6 - NPX values log2 scale
df$il6_24 <- as.numeric(data$IL6_F24)
df$il6_24[df$il6_24<0] <- NA

## White blood cell count 10^9/L
df$wbc_24 <- as.numeric(data$WBC_F24)
df$wbc_24[df$wbc_24<0] <- NA

##Neutrophil-to-Lymphocyte Ratio
df$neutrophil_24 <- as.numeric(data$Neutrophils_F24) #Neutrophils 10^9/L
df$neutrophil_24[df$neutrophil_24<0] <- NA

df$lymphocytes_24 <- as.numeric(data$Lymphocytes_F24) #Lymphocytes 10^9/L
df$lymphocytes_24[df$lymphocytes_24<0] <- NA

df$nlr_24 <- df$neutrophil_24/df$lymphocytes_24

## Triglycerides mmol/L

### age 24
df$triglycerides_24 <- as.numeric(data$Trig_F24)
df$triglycerides_24[df$triglycerides_24<0] <- NA
df$log_triglycerides_24 <- log(df$triglycerides_24)

### age 18 - auxiliary for imputation
df$triglycerides_18 <- as.numeric(data$TRIG_TF4)
df$triglycerides_18[df$triglycerides_18<0] <- NA
df$log_triglycerides_18 <- log(df$triglycerides_18)


## Insulin uU/mL

### age 24
df$insulin_24 <- as.numeric(data$Insulin_F24)
df$insulin_24[df$insulin_24<0] <- NA
df$log_insulin_24 <- log(df$insulin_24)

### age 18 - auxiliary for imputation
df$insulin_18 <- as.numeric(data$insulin_TF4)
df$insulin_18[df$insulin_18<0] <- NA
df$log_insulin_18 <- log(df$insulin_18)


## Platelet count 10^9/L
df$platelets_24 <- as.numeric(data$Platelets_F24)
df$platelets_24[df$platelets_24<0] <- NA

## Haemoglobin g/L

### age 24
df$haemoglobin_24 <- as.numeric(data$HB_F24)
df$haemoglobin_24[df$haemoglobin_24<0] <- NA

### age 18 - auxiliary for imputation
df$haemoglobin_18 <- as.numeric(data$Hb_TF4)
df$haemoglobin_18[df$haemoglobin_18<0] <- NA


## Fat mass index (FMI)
df$fat_mass_ind_24 <- as.numeric(data$FKDX1060)
df$fat_mass_ind_24[df$fat_mass_ind_24<0] <- NA
df$fat_mass_ind_24[df$fat_mass_ind_24>30] <- NA #2 outliers

## Lean mass index (LMI)
df$lean_mass_ind_24 <- as.numeric(data$FKDX1070)
df$lean_mass_ind_24[df$lean_mass_ind_24<0] <- NA
df$lean_mass_ind_24[df$lean_mass_ind_24>30] <- NA #3 outliers

## Body mass index (BMI), weight (kg) / [height (m)]² 
df$bmi_24 <- as.numeric(data$FKMS1040)
df$bmi_24[df$bmi_24<0] <- NA

## Average seated pulse rate (bpm)
df$heartrate_24 <- as.numeric(data$FKBP1032)
df$heartrate_24[df$heartrate_24<0] <- NA
#FKSP1837 - PWA: Heart Rate average (bpm)

##Peripheral Mean Pressure, mmHg - auxiliary for imputation
df$arterial_pressure_18 <- as.numeric(data$FJEL123)
df$arterial_pressure_18[df$arterial_pressure_18<0] <- NA

## FEV1:FVC Ratio (pre-salbutamol)

###age 24
df$lungfunc_24 <- as.numeric(data$FKLF1007)
df$lungfunc_24[df$lungfunc_24<0] <- NA

### age 16 - auxiliary for imputation
df$fev1_16 <- data$fh4431
df$fvc_16 <- data$fh4430
df$fev1_16[df$fev1_16<0] <- NA
df$fvc_16[df$fvc_16<0] <- NA
df$lungfunc_16 <- df$fev1_16/df$fvc_16


## PWV: Average carotid-femoral pulse wave velocity (m/s)
df$arterial_stiff_24 <- as.numeric(data$FKCV4200)
df$arterial_stiff_24[df$arterial_stiff_24<0] <- NA

## Maximum hand grip strength

### age 24
df$grip_strength_24 <- as.numeric(data$FKSG1010)
df$grip_strength_24[df$grip_strength_24<0] <- NA

### age 16 - auxiliary for imputation
df$grip_strength_18 <- as.numeric(data$FJAR033)
df$grip_strength_18[df$grip_strength_18<0] <- NA

# HEALTH-RELATED MEASURES

## YP takes any medication (think this is only referring to mental health etc)
df <- df %>%
  mutate(
    meds_24  = case_when(
      data$FKCO1001 == 0 ~ 0,
      data$FKCO1001 == 1 ~ 1
    ),
    meds_24 = factor(meds_24,
                     levels = 0:1,
                     labels = c('No', 'Yes'))
  )

## YP has any allergies
df <- df %>%
  mutate(
    allerg_24  = case_when(
      data$FKCO1002 == 0 ~ 0,
      data$FKCO1002 == 1 ~ 1
    ),
    allerg_24 = factor(allerg_24,
                       levels = 0:1,
                       labels = c('No', 'Yes'))
  )

## In past year, number of times YP has been to GP about own health
df <- df %>%
  mutate(
    gp_visits_24  = case_when(
      data$FKDQ2500 == 0 ~ 1,
      data$FKDQ2500 == 1 ~ 2,
      data$FKDQ2500 %in% 2:4 ~ 3
    ),
    gp_visits_24 = factor(gp_visits_24,
                          levels = 1:3,
                          ordered = T,
                          labels = c('None', 'Once or twice', 'Three or more'))
    )

## Length of time since YP last went to doctor about a condition
df <- df %>%
  mutate(#excludes don't know
    seen_doc_25  = case_when(
      data$YPE3000 == 1 ~ 1,
      data$YPE3000 %in% 2:3 ~ 2,
      data$YPE3000 == 0 ~ 3
    ),
    seen_doc_25 = factor(seen_doc_25,
                         levels = 1:3,
                         ordered = T,
                         labels = c('Last 6 months', 'More than 6 months', 'Never'))
  )

## YP has a long-standing illness, disability or infirmity
df <- df %>%
  mutate(
    longst_ill_disab_24  = case_when(
      data$FKDQ2520 == 1 ~ 1,
      data$FKDQ2520 == 2 ~ 0
    ),
    longst_ill_disab_24 = factor(longst_ill_disab_24,
                                 levels = 0:1,
                                 labels = c('No', 'Yes'))
  )

## YP gets a lot of headaches, stomach-aches or sickness
df <- df %>%
  mutate(
    aches_sickness_25  = case_when(
      data$YPE1152 == 2 ~ 1,
      data$YPE1152 == 1 ~ 2,
      data$YPE1152 == 0 ~ 3
    ),
    aches_sickness_25 = factor(aches_sickness_25,
                               levels = 1:3,
                               ordered = T,
                               labels = c('Certainly true', 'Somewhat true', 'Not true'))
  )



# NEIGHBOURHOOD VARIABLES

## 2001 Census urban/rural indicator

### age 24
df <- df %>%
  mutate(
    urbrur_24  = case_when(
      data$YPDur01ind %in% 2:4 ~ 0,
      data$YPDur01ind == 1 ~ 1
    ),
    urbrur_24 = factor(urbrur_24,
                       levels = 0:1,
                       labels = c('Town/Village/Hamlet/Isolated Dwelling', 'Urban (pop. >= 10k)'))
  )

### age 16 - auxiliary for imputation
df <- df %>%
  mutate(
    urbrur_16  = case_when(
      data$ccsur01ind %in% 2:4 ~ 0,
      data$ccsur01ind == 1 ~ 1
    ),
    urbrur_16 = factor(urbrur_16,
                       levels = 0:1,
                       labels = c('Town/Village/Hamlet/Isolated Dwelling', 'Urban (pop. >= 10k)'))
  )

## Townsend deprivation score, quintiles

### age 24
df <- df %>%
  mutate(
    townsendq_24  = case_when(
      data$YPDTownsendq5 == 5 ~ 1,
      data$YPDTownsendq5 == 4 ~ 2,
      data$YPDTownsendq5 == 3 ~ 3,
      data$YPDTownsendq5 == 2 ~ 4,
      data$YPDTownsendq5 == 1 ~ 5,
    ),
    townsendq_24 = factor(townsendq_24,
                          levels = 1:5,
                          ordered = T,
                          labels = c('Q5 (Most deprived)', 'Q4', 'Q3', 'Q2', 'Q1 (Least deprived)'))
    )

### age 16 - auxiliary for imputation
df <- df %>%
  mutate(
    townsendq_16  = case_when(
      data$ccsTownsendq5 == 5 ~ 1,
      data$ccsTownsendq5 == 4 ~ 2,
      data$ccsTownsendq5 == 3 ~ 3,
      data$ccsTownsendq5 == 2 ~ 4,
      data$ccsTownsendq5 == 1 ~ 5,
    ),
    townsendq_16 = factor(townsendq_16,
                          levels = 1:5,
                          ordered = T,
                          labels = c('Q5 (Most deprived)', 'Q4', 'Q3', 'Q2', 'Q1 (Least deprived)'))
  )

## IMD Score 2015, quintiles

### age 24
df <- df %>%
  mutate(
    imdq_24  = case_when(
      data$YPDimd2015q5 == 5 ~ 1,
      data$YPDimd2015q5 == 4 ~ 2,
      data$YPDimd2015q5 == 3 ~ 3,
      data$YPDimd2015q5 == 2 ~ 4,
      data$YPDimd2015q5 == 1 ~ 5,
    ),
    imdq_24 = factor(imdq_24,
                     levels = 1:5,
                     ordered = T,
                     labels = c('Q5 (Most deprived)', 'Q4', 'Q3', 'Q2', 'Q1 (Least deprived)'))
  )

### age 16 - auxiliary for imputation
df <- df %>%
  mutate(
    imdq_16  = case_when(
      data$ccsimd2015q5 == 5 ~ 1,
      data$ccsimd2015q5 == 4 ~ 2,
      data$ccsimd2015q5 == 3 ~ 3,
      data$ccsimd2015q5 == 2 ~ 4,
      data$ccsimd2015q5 == 1 ~ 5,
    ),
    imdq_16 = factor(imdq_16,
                     levels = 1:5,
                     ordered = T,
                     labels = c('Q5 (Most deprived)', 'Q4', 'Q3', 'Q2', 'Q1 (Least deprived)'))
  )


#PAIN

## Anyone thought child had ache or pain problem (78 months)
df <- df %>%
  mutate(
    pain_prob_7  = case_when(
      data$kp3130 == 2 ~ 0,
      data$kp3130 == 1 ~ 1
    ),
    pain_prob_7 = factor(pain_prob_7,
                         levels = 0:1,
                         labels = c('No', 'Yes'))
    )

## Acute pain (17.5y)
df <- df %>%
  mutate(
    pain_18  = case_when(
      data$FJPA010 == 0 ~ 0,
      data$FJPA010 %in% c(1,5) ~ 1
    ),
    pain_18 = factor(pain_18,
                     levels = 0:1,
                     labels = c('No', 'Yes'))
  )

## Chronic pain (17.5y)
df <- df %>%
  mutate(
    chronic_pain_18  = case_when(
      data$FJPA011 == 0 ~ 0,
      data$FJPA011 == 1 ~ 1
    ),
    chronic_pain_18 = factor(chronic_pain_18,
                             levels = 0:1,
                             labels = c('No', 'Yes'))
  )

## Pain status (17.5y)
df <- df %>%
  mutate(
    pain_status_18  = case_when(
      df$pain_18 == 'No' ~ 1,
      df$chronic_pain_18 == 'No' ~ 2,
      df$chronic_pain_18 == 'Yes' ~ 3
    ),
    pain_status_18 = factor(pain_status_18,
                            labels = c('No pain', 'Acute pain', 'Chronic pain'))
  ) #35 with not answered pain length question


## Acute pain (26y)
df <- df %>%
  mutate(
    pain_26  = case_when(
      data$YPF1130 == 0 ~ 0,
      data$YPF1130 == 1 ~ 1
    ),
    pain_26 = factor(pain_26,
                     levels = 0:1,
                     labels = c('No', 'Yes'))
  )

## Chronic pain (26y)
df <- df %>%
  mutate(
    chronic_pain_26  = case_when(
      data$YPF1140 == 1 ~ 0,
      data$YPF1140 == 2 ~ 1
    ),
    chronic_pain_26 = factor(chronic_pain_26,
                             levels = 0:1,
                             labels = c('No', 'Yes'))
  )

## Pain impact (26y) - Number of days that YP's pains kept YP from work/daily activities/physical activity in past 6 months
df <- df %>%
  mutate(#this is answered by everyone
    pain_impact_26  = case_when(
      (data$YPF1610 %in% c(0:1) & data$YPF1620 %in% c(0:1) & data$YPF1630 %in% c(0:1)) ~ 0,
      (data$YPF1610 %in% c(2:4) | data$YPF1620 %in% c(2:4) | data$YPF1630 %in% c(2:4)) ~ 1 #7 or more days in any domain
    ),
    pain_impact_26 = factor(pain_impact_26,
                            levels = 0:1,
                            labels = c('Low', 'High'))
    )

## Pain status (26y)
df <- df %>%
  mutate(
    pain_status_26  = case_when(
      df$pain_26 == 'No' ~ 1,
      df$chronic_pain_26 == 'No' ~ 2,
      df$chronic_pain_26 == 'Yes' & df$pain_impact_26 == 'Low' ~ 3, #10 missing impact
      df$chronic_pain_26 == 'Yes' & df$pain_impact_26 == 'High' ~ 4
    ),
    pain_status_26 = factor(pain_status_26,
                            levels = 1:4,
                            labels = c('No pain', 'Acute pain', 'Chronic pain, low impact', 'Chronic pain, high impact'))
    ) #20 participants missing (10 with acute pain missing chronicity measure and 10 with chronic pain missing impact measure)


## Acute pain (30y)
df <- df %>%
  mutate(
    pain_30  = case_when(
      data$YPL6000 == 0 ~ 0,
      data$YPL6000 == 1 ~ 1
    ),
    pain_30 = factor(pain_30,
                     levels = 0:1,
                     labels = c('No', 'Yes'))
    )

## Chronic pain (30y)
df <- df %>%
  mutate(
    chronic_pain_30  = case_when(
      data$YPL6001 == 1 ~ 0,
      data$YPL6001 == 2 ~ 1
    ),
    chronic_pain_30 = factor(chronic_pain_30,
                             levels = 0:1,
                             labels = c('No', 'Yes'))
    )

## Troubled by pain or discomfort, either all the time or on and off, that has been present for more than 3 months
df <- df %>%
  mutate(
    chronic_pain_other_30  = case_when(
      data$YPL6050 == 0 ~ 0,
      data$YPL6050 == 1 ~ 1
    ),
    chronic_pain_other_30 = factor(chronic_pain_other_30,
                                   levels = 0:1,
                                   labels = c('No', 'Yes'))
    )

## Pain impact (30y) - In past 6 months rate of pain that interfered with daily activities/changed ability to do social or family/work activities
df <- df %>%
  mutate(#this is answered by people who said 'Yes' to: Overall troubled by pain/discomfort, all time/on off, present > 3 months
    pain_impact_30  = case_when(
      (data$YPL6056 %in% c(0:4) & data$YPL6057 %in% c(0:4) & data$YPL6058 %in% c(0:4)) ~ 0,
      (data$YPL6056 %in% c(5:10) | data$YPL6057 %in% c(5:10) | data$YPL6058 %in% c(5:10)) ~ 1 #Rating 5 or more in any domain
    ),
    pain_impact_30 = factor(pain_impact_30,
                            levels = 0:1,
                            labels = c('Low', 'High'))
  )

## Pain status (30y)
df <- df %>%
  mutate(
    pain_status_30  = case_when(
      df$chronic_pain_other_30 == 'Yes' & df$pain_impact_30 == 'High' ~ 5, #5 missing impact
      df$chronic_pain_other_30 == 'Yes' & df$pain_impact_30 == 'Low' ~ 4,
      df$chronic_pain_30 == 'Yes' ~ 3, #Includes 292 who stated no to second chronicity q but yes to first, plus 5 who said yes but are missing impact 
      df$pain_30 == 'Yes' & df$chronic_pain_30 == 'No' ~ 2, #1 missing
      df$pain_30 == 'No' ~ 1
    ),
    pain_status_30 = factor(pain_status_30,
                            levels = 1:5,
                            labels = c('No pain', 'Acute pain', 'Chronicity not known', 'Chronic pain, low impact', 'Chronic pain, high impact'))
  ) #7 participants missing including those who reported acute pain but no chronicity

df <- df %>%
  mutate(
    pain_status_30  = case_when(
      df$pain_30 == 'No' & df$chronic_pain_other_30 == 'No' ~ 1,
      df$pain_30 == 'Yes' & df$chronic_pain_other_30 == 'No' ~ 2,
      df$chronic_pain_other_30 == 'Yes' & df$pain_impact_30 == 'Low' ~ 3, #5 missing impact
      df$chronic_pain_other_30 == 'Yes' & df$pain_impact_30 == 'High' ~ 4
    
    ),
    pain_status_30 = factor(pain_status_30,
                            levels = 1:4,
                            labels = c('No pain', 'Acute pain', 'Chronic pain, low impact', 'Chronic pain, high impact'))
  ) #14 participants missing including 4 missing impact and 10 missing chronicity measure 


# SAVE DATASET
alldf <- data.frame(df)
save(alldf, file = "//path/to/data/clean_data.rda")

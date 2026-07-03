# Get Slurm array task number
task_id <- Sys.getenv('SLURM_ARRAY_TASK_ID')
imp_num <- as.integer(task_id)

# Give each imputation a different seed
seed_i <- 237657 + imp_num


# Install packages
#install.packages('labelled')
#install.packages('sticky')
#install.packages('tidyverse')
#install.packages('mice')
#install.packages('ggmice')

# Load packages
library(labelled)
library(sticky)
library(tidyverse)
library(mice)
library(ggmice)

# Load data
load('clean_data.rda')

# Subset dataset to those with data on pain at ages 26 AND 30
df <- alldf[!is.na(alldf$pain_status_26) & !is.na(alldf$pain_status_30),]

# Variable labels
mainvarlabs <-  list(
  'aln'='Pregnancy identifier',
  'qlet'='Birth order (within pregnancy)',
  'sex'='Sex assigned at birth', 
  'ethnicity'='Ethnicity',
  'parent_sc_0' = 'Highest parental social class at birth',
  'pain_prob_7' = 'Pain problem in childhood (7y)',
  'parent_pain_12' = 'Parental pain (12y)',
  'sf12_sr_hlth_22'='SF-12: Self-rated health (22y)',
  'sf12_hlth_limt_mod_act_22'='SF-12: Health limits moderate activities (22y)',
  'sf12_ph_accomp_less_22'='SF-12: Freq. accomplished less due to physical health (22y)', 
  'sf12_emo_limt_wrk_act_22'='SF-12: Freq. work/activities limited by emotions (22y)',
  'sf12_calm_peace_22'='SF-12: Freq. felt calm and peaceful (22y)',
  'sf12_energy_22'='SF-12: Freq. had lot of energy (22y)',
  'sf12_limt_soc_act_22'='SF-12: Freq. health/emotions interfered with social activities (22y)',
  'wellbeing_sc_23'='Warwick-Edinburgh Mental Wellbeing Scale score (23y)', 
  'subj_happy_sc_23'='Subjective Happiness Scale score (23y)',
  'life_satis_sc_23'='Life Satisfaction Scale score (23y)',
  'meaning_life_presc_sc_23'='Meaning in Life Questionnaire: Presence score (23y)',
  'meaning_life_search_sc_23'='Meaning in Life Questionnaire: Search score (23y)',
  'basic_psych_needs_auton_sc_23'='BPNSS: Autonomy score (23y)', 
  'basic_psych_needs_compet_sc_23'='BPNSS: Competence score (23y)', 
  'basic_psych_needs_relat_sc_23'='BPNSS: Relatedness score (23y)',
  'gratitude_sc_23'='Gratitude Questionnaire score (23y)',
  'optimism_sc_23'='Life Orientation Test-Revised Optimism score (23y)',
  'smfq_sc_23'='Short Moods and Feelings Questionnaire score (23y)',
  'cisr_mild_depres_24'='CIS-R: Depression (24y)',
  'cisr_general_anxiety_24'='CIS-R: Generalised anxiety disorder (24y)',
  'scaared_total_25'='SCAARED: Total score (25y)', 
  'sapas_personality_dis_sc_24'='SAPAS: Total score (24y)',  
  'supps_neg_urg_24'='S-UPPS-P: Negative Urgency (24y)',
  'supps_pos_urg_24'='S-UPPS-P: Positive Urgency (24y)',
  'supps_lack_premed_24'='S-UPPS-P: Lack of Premeditation (24y)',
  'supps_lack_persev_24'='S-UPPS-P: Lack of Perseverance (24y)',
  'supps_sens_seek_24'='S-UPPS-P: Sensation Seeking (24y)',
  'sdq_prosocial_25'='SDQ: Prosocial behavior (25y)',
  'sdq_emotional_25'='SDQ: Emotional symptoms (25y)',
  'sdq_conduct_25'='SDQ: Conduct problems (25y)',
  'sdq_hyperact_25'='SDQ: Hyperactivity/inattention (25y)',
  'sdq_peer_25'='SDQ: Peer relationship problems (25y)',
  'aq_social_skill_sc_25'='AQ: Social Skills (25y)',
  'aq_attention_switch_sc_25'='AQ: Attention Switching (25y)',
  'aq_attention_detail_sc_25'='AQ: Attention to Detail (25y)',
  'aq_communication_sc_25'='AQ: Communication (25y)',
  'aq_imagination_sc_25'='AQ: Imagination (25y)',
  'ari_affect_react_sc_25'='ARI: Total Score (25y)',
  'atsrs_emo_support_sc_25'='ATSRS: Emotional support (25y)',
  'atsrs_prac_support_sc_25'='ATSRS: Instrumental support (25y)',
  'work_mem_24'='Working memory: N-back d prime (24y)',
  'resp_inhib_24'='Response inhibition: Stop-Signal reaction time, ms (24y)', 
  'screen_time_22'='Hours per day spent using screens on average (22y)',
  'outdoor_time_22'='Hours per day spent outdoors on average (22y)',
  'actv_jrnys_22'='Regular walking/cycling journeys (22y)',
  'vigact_freq_22'='Freq. taking part in strenuous/vigorous activity (22y)',
  'eat_conscious_25'='Conscious of what they are eating (25y)',
  'weight_change_25'='Weight hardly changed in last 5 years (25y)',
  'n_hrs_sleep_25'='Number of hours sleeps in 24 hours (25y)', 
  'enough_sleep_25'='Frequency gets enough sleep (25y)', 
  'daytime_tired_25' = 'Problem with sleepiness during day time activities (25y)',
  'smk_vape_cannabis_24'='Nicotine or cannabis use (24y)',
  'bing_drink_24'='Binge drinking (24y)',
  'n_illict_drugs_24'='Number of illicit drugs ever used (24y)', 
  'antisocialb_22'='Anti-social behaviour (22y)',
  'n_siblings_23'='Number of siblings (23y)',
  'n_friends_24'='Number of close friends (24y)',
  'freq_see_parents_25'='Frequency sees parents (25y)',
  'socmed_freq_24'='Social media use frequency (24y)',
  'parent_25'='Is a parent (25y)',
  'partner_25'='Has a partner (25y)',
  'lives_alone_25'='Lives alone (25y)',
  'volunteer_22'='Doing volunteer work (22y)',
  'inedu_22'='In part-/full-time education (22y)',
  'income_25'='Take-home pay each month (25y)',
  'job_shift_nights_25'='Has job that involves shift work or night shifts (25y)',
  'n_jobs_school_25' = 'Number of jobs had since leaving school (25y)',
  'financ_prob_25'='Had big financial problems past year (25y)',
  'bullied_23'='Bullied, past 6 months (23y)',
  'feltlov_grwup_23'='Felt loved growing up (23y)',
  'feltlov_relat_23'='Felt loved by a partner since 16 (23y)',
  'relationship_abuse_23' = 'Relationship abuse since 16 (23y)',
  'childh_maltreat_23' = 'Childhood maltreatment growing up (23y)',
  'serious_accident_fire_23'='Ever been in a serious accident or fire (23y)',
  'death_somclos_23'='Unexpected death of someone close (23y)',
  'oth_traum_stress_23' = 'Ever other very traumatic or stressful event (23y)',
  'bad_exps_24'='Bad experience in past year (24y)',
  'selfharm_24'='Self-harm history (24y)',
  'lean_mass_ind_24'='Lean mass index (24y)',
  'bmi_24'='Body Mass Index (24y)',
  'heartrate_24'='Heart Rate average, bpm (24y)',
  'grip_strength_24'='Maximum hand grip strength (24y)',
  'log_crp_24'='C-Reactive Protein, mg/l log-transformed (24y)',
  'il6_24'='Interleukin-6,NPX values log2 scale (24y)',
  'nlr_24' = 'Neutrophil-to-Lymphocyte Ratio (24y)',
  'wbc_24' = 'White blood cell count 10^9/L (24y)',
  'log_insulin_24' = 'Insulin uU/mL log-transformed (24y)',
  'log_triglycerides_24'='Triglycerides mmol/L log-transformed (24y)',
  'aches_sickness_25' = 'Regular headaches, stomach-aches or sickness (25y)',
  'gp_visits_24'='GP visits about own health, past year (24y)',
  'longst_ill_disab_24'='Long-standing illness/disability/infirmity (24y)', 
  'urbrur_24'='Urban/rural indicator (24y)',
  'imdq_24'='IMD Neighbourhood deprivation, quintiles (24y)',
  'townsendq_24'='Townsend deprivation score, quintiles (24y)',
  'pain_status_18' = 'Pain status (18y)',
  'pain_status_26'='Pain status (26y)',
  'pain_status_30'='Pain status (30y)'
)

auxvarlabs <- list(
  'sf12_hlth_limt_stairs_22'='SF-12: Health limits climbing stairs (22y)',
  'sf12_ph_limt_wrk_act_22'='SF-12: Freq. limited work/other activities - physical health (22y)',
  'sf12_emo_accomp_less_22'='SF-12: Freq. accomplished less - emotional problems (22y)',
  'sf12_pain_intf_wrk_22'='SF-12: Amount pain interfered with normal work, past 4 weeks (22y)', 
  'sf12_down_depr_22'='SF-12: Freq. felt downhearted/depressed (22y)', 
  'sdq_any_diffic_25'='SDQ: Any difficulties (25y)',
  'ons_wellb_life_satis_23'='ONS Wellbeing: Satisfied with life (23y)', 
  'ons_wellb_things_done_worthw_23'='ONS Wellbeing: Things they do are worthwhile (23y)',
  'ons_wellb_happy_yest_23'='ONS Wellbeing: Degree happy yesterday (23y)',
  'ons_wellb_anxious_yest_23'='ONS Wellbeing: Degree anxious yesterday (23y)',
  'cisr_panic_att_symp_24'='CIS-R: Panic attack symptoms total score (24y)',
  'cisr_hlth_cond_22'='CIS-R: Health conditions (24y)',
  'intellig_24'='Intelligence: Number of symbols correct in WISC (24y)',
  'nssec_23'='National Statistics Socio-economic Classification (23y)',
  'employ_st_23'='Employment status (23y)',
  'highest_edu_26'='Education level attained (26y)',
  'claim_benefits_25'='Claiming any State Benefits/Tax Credits (25y)',
  'fat_mass_ind_24'='Fat mass index (24y)', 
  'meds_24'='Takes any medication (24y)',
  'allerg_24'='Has any allergies (24y)',
  'seen_doc_25'='Length of time since went to doctor about a condition (25y)',
  'sf36_sr_hlth_18' = 'SF-12: Self-rated health (18y)',
  'sf36_hlth_limt_vig_act_18' = 'SF-12: Health limits vigorous activities (18y)',
  'sf36_hlth_limt_mod_act_18' = 'SF-12: Health limits moderate activities (18y)',
  'sf36_hlth_limt_stairs_18' = 'SF-12: Health limits climbing stairs (18y)',
  'sf36_hlth_limt_hlf_mile_18' = 'SF-12: Health limits walking half mile (18y)',
  'sf36_hlth_limt_bath_dress_18' = 'SF-12: Health limits bathing/dressing (18y)',
  'sf36_ph_accomp_less_18' = 'SF-12: Freq. accomplished less due to physical health (18y)',
  'sf36_ph_limt_wrk_act_18' = 'SF-12: Freq. limited work/other activities - physical health (18y)',
  'sf36_emo_accomp_less_18' = 'SF-12: Freq. accomplished less - emotional problems (18y)',
  'sf36_emo_wrk_less_caref_18' = 'SF-12: Freq. work/activities less careful due to emotional problems (18y)',
  'sf36_bodily_pain_18' = 'Amount of bodily pain, past 4 weeks (18y)',
  'sf36_bodpain_intf_wrk_18' = 'SF-12: Amount pain interfered with normal work, past 4 weeks (18y)',
  'sf36_full_of_life_18' = 'SF-12: Freq. felt full of life (18y)',
  'sf36_calm_peace_18' = 'SF-12: Freq. felt calm and peaceful (18y)',
  'sf36_energy_18' = 'SF-12: Freq. had lot of energy (18y)',
  'sf36_down_low_18' = 'SF-12: Freq. felt downhearted/low (18y)',
  'sf36_been_happy_18' = 'SF-12: Freq. been happy (18y)',
  'sf36_felt_tired_18' = 'SF-12: Freq. felt tired (18y)',
  'sf36_limt_soc_act_18' = 'SF-12: Freq. health/emotions interfered with social activities (18y)',
  'sf36_expect_hlth_wrs_18' = 'SF-12: Expects my health to get worse (18y)',
  'smfq_sc_18' = 'Short Moods and Feelings Questionnaire score (18y)',
  'cisr_mild_depres_18'='CIS-R: Depression (18y)',
  'cisr_general_anxiety_18'='CIS-R: Generalised anxiety disorder (18y)',
  'selfharm_20' = 'Self-harm (20y)',
  'wellbeing_sc_18' = 'Warwick-Edinburgh Mental Wellbeing Scale score (18y)',
  'dis_func_att_sc_18' = 'Dysfunctional Attitude Scale score (18y)',
  'aiss_novelty_sc_18' = 'Arnett Inventory of Sensation Seeking: Novelty score (18y)',
  'aiss_intensity_sc_18' = 'Arnett Inventory of Sensation Seeking: Intensity score (18y)',
  'work_mem_18' = 'Working memory: N-back Task signal-detection ability (18y)',
  'resp_inhib_18' = 'Response inhibition: Affective Go/No-Go Task errors (18y)',
  'exercise_18' = 'Frequency respondent exercised in past year (18y)',
  'reading_time_22'='Hours per day spent reading on average (22y)',
  'sleep_duration_hrs_16' = 'Length of time sleeps on normal school night (16y)',
  'enough_sleep_16'='Frequency gets enough sleep (16y)', 
  'daytime_tired_16' = 'Problem with sleepiness during day time (16y)',
  'smk_cannabis_18' = 'Smoking or cannabis use (18y)',
  'bing_drink_18' = 'Binge drinking (18y)',
  'n_illict_drugs_18' = 'Number of illicit drugs ever used (18y)',
  'uni_likely_18' = 'Likelihood of applying to uni in next 5 years (18y)',
  'school_attendance_16' = 'Frequency attends school (16y)',
  'carer_22'='Full/part-time carer (22y)',
  'n_friends_18' = 'Number of friends (18y)',
  'ever_employed_20' = 'Ever been employed (20y)',
  'feel_close_parents_18' = 'Feels close to parents (18y)',
  'lungfunc_24'='Lung function FEV1:FVC Ratio (24y)',
  'arterial_stiff_24'='Arterial stiffness (24y)',
  'haemoglobin_24' = 'Haemoglobin g/L (24y)',
  'platelets_24'='Platelet count 10^9/L (24y)',
  'log_crp_18' = 'C-reactive Protein, mg/l log-transformed (18y)',
  'log_triglycerides_18' = 'Triglycerides mmol/L log-transformed (18y)',
  'log_insulin_18' = 'Insulin uU/mL log-transformed (18y)',
  'haemoglobin_18' = 'Haemoglobin g/L (18y)',
  'lungfunc_16' = 'Lung function FEV1:FVC Ratio (16y)',
  'arterial_pressure_18' = 'Mean arterial pressure mmHg (18y)',
  'grip_strength_18' = 'Maximum hand grip strength (18y)',
  'urbrur_16' = 'Urban/rural indicator (16y)',
  'imdq_16' = 'IMD Neighbourhood deprivation, quintiles (16y)',
  'townsendq_16'='Townsend deprivation score, quintiles (16y)'
  )

# Combine variable labels
alllabs <- c(mainvarlabs, auxvarlabs)

# Subset dataset to only include IDs, demographics, multidomain exposure items and pain outcomes
dfkeep <- df[,names(alllabs)]

# Assign labels
var_label(dfkeep) <- alllabs[colnames(dfkeep)]

# Persist label attributes
dfkeep <- sticky_all(dfkeep)

# Get default methods - !check
init <- mice(dfkeep, maxit = 0)
imp_methods <- init$method

# Construct quickpred predictor matrix
def_predm <- quickpred(dfkeep)
predm <- def_predm

# Main analysis variables 
predm[names(mainvarlabs),names(mainvarlabs)] <- 1

# Don't impute IDs or sex
predm[c('aln','qlet','sex'),] <- 0

# Don't use IDs/diagonal to impute
predm[,c('aln','qlet')] <- 0
diag(predm) <- 0

# Don't impute pain status outcomes
predm[c('pain_status_26','pain_status_30'),] <- 0

# Run imputation with mice
imp <- mice(data = dfkeep,
            m = 1, maxit = 25, seed = seed_i, 
            method = imp_methods, predictorMatrix = predm)

# Save mice object
save(imp, file=paste0('mult_pain_imp_',imp_num,'.rda'))

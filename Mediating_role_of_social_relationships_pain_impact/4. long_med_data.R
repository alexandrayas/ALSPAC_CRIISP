# Paths
path_to_data <- "//path/to/data/"

# Get data
data <- read_dta(paste0(path_to_data,'data_irt_all.dta'))
data <- data.frame(to_factor(data, sort_levels='auto'))
data <- remove_attributes(data, 'format.stata')


# Long datasets (used for array job when running mediation models on HPC)

## No intermediate confounder (15 models)
ldat <- data.frame(
  pain_1day_mnth_18 = rep(data$pain_1day_mnth_18, 15),
  sex = rep(data$sex, 15),
  ethnicity = rep(data$ethnicity, 15),
  m_pregsmk = rep(data$m_pregsmk, 15), 
  parent_sc_0 = rep(data$parent_sc_0, 15),
  m_homown_0 = rep(data$m_homown_0, 15),
  parent_mhp_1 = rep(data$parent_mhp_1, 15),
  
  exposure = unlist(
    rep(
      list(
        data$selfc_spl_18,
        data$social_spl_18,
        data$work_spl_18
      ), times = 5),
    use.names = FALSE
  ),
  
  mediator = unlist(
    rep(
      list(
        data$social_integration_sc_grp_25,
        data$emo_supp_sc_grp_25,
        data$prac_supp_sc_grp_25,
        data$covid_inpers_cont_adult_grp_28,
        data$covid4_dist_cont_adult_grp_28
      ),
      each = 3
    ),
    use.names = FALSE
  ),
  
  outcome = unlist(
    rep(
      list(
        data$daily_spl_30,
        data$social_spl_30,
        data$work_spl_30
      ),
      times = 5
    ),
    use.names = FALSE
  ),
  
  mod_n = rep(1:15, each = nrow(data))
)

#mod n    exposure                       mediator       outcome
#1   selfc_spl_18   social_integration_sc_grp_25  daily_spl_30
#2  social_spl_18   social_integration_sc_grp_25 social_spl_30
#3    work_spl_18   social_integration_sc_grp_25   work_spl_30
#4   selfc_spl_18             emo_supp_sc_grp_25  daily_spl_30
#5  social_spl_18             emo_supp_sc_grp_25 social_spl_30
#6    work_spl_18             emo_supp_sc_grp_25   work_spl_30
#7   selfc_spl_18            prac_supp_sc_grp_25  daily_spl_30
#8  social_spl_18            prac_supp_sc_grp_25 social_spl_30
#9    work_spl_18            prac_supp_sc_grp_25   work_spl_30
#10  selfc_spl_18 covid_inpers_cont_adult_grp_28  daily_spl_30
#11 social_spl_18 covid_inpers_cont_adult_grp_28 social_spl_30
#12   work_spl_18 covid_inpers_cont_adult_grp_28   work_spl_30
#13  selfc_spl_18  covid4_dist_cont_adult_grp_28  daily_spl_30
#14 social_spl_18  covid4_dist_cont_adult_grp_28 social_spl_30
#15   work_spl_18  covid4_dist_cont_adult_grp_28   work_spl_30

## With intermediate confounder (30 models)
ldat_intconf <- data.frame(
  pain_1day_mnth_18 = rep(data$pain_1day_mnth_18, 30),
  sex = rep(data$sex, 30),
  ethnicity = rep(data$ethnicity, 30),
  m_pregsmk = rep(data$m_pregsmk, 30), 
  parent_sc_0 = rep(data$parent_sc_0, 30),
  m_homown_0 = rep(data$m_homown_0, 30),
  parent_mhp_1 = rep(data$parent_mhp_1, 30),
  
  exposure = unlist(
    rep(
      list(
        data$selfc_spl_18,
        data$social_spl_18,
        data$work_spl_18
      ), times = 10),
    use.names = FALSE
  ),
  
  intconf = unlist(
    rep(
      list(
        data$mfq_cutoff_21,
        data$gad_cutoff_21
      ),
      each = 15
    ),
    use.names = FALSE
  ),
  
  mediator = unlist(
    rep(
      list(
        data$social_integration_sc_grp_25,
        data$emo_supp_sc_grp_25,
        data$prac_supp_sc_grp_25,
        data$covid_inpers_cont_adult_grp_28,
        data$covid4_dist_cont_adult_grp_28
      ),
      each = 3, times = 2
    ),
    use.names = FALSE
  ),
  
  outcome = unlist(
    rep(
      list(
        data$daily_spl_30,
        data$social_spl_30,
        data$work_spl_30
      ),
      times = 10
    ),
    use.names = FALSE
  ),
  
  mod_n = rep(1:30, each = nrow(data))
)

#mod n    exposure                       mediator       intconf       outcome
#1   selfc_spl_18   social_integration_sc_grp_25 mfq_cutoff_21  daily_spl_30
#2  social_spl_18   social_integration_sc_grp_25 mfq_cutoff_21 social_spl_30
#3    work_spl_18   social_integration_sc_grp_25 mfq_cutoff_21   work_spl_30
#4   selfc_spl_18             emo_supp_sc_grp_25 mfq_cutoff_21  daily_spl_30
#5  social_spl_18             emo_supp_sc_grp_25 mfq_cutoff_21 social_spl_30
#6    work_spl_18             emo_supp_sc_grp_25 mfq_cutoff_21   work_spl_30
#7   selfc_spl_18            prac_supp_sc_grp_25 mfq_cutoff_21  daily_spl_30
#8  social_spl_18            prac_supp_sc_grp_25 mfq_cutoff_21 social_spl_30
#9    work_spl_18            prac_supp_sc_grp_25 mfq_cutoff_21   work_spl_30
#10  selfc_spl_18 covid_inpers_cont_adult_grp_28 mfq_cutoff_21  daily_spl_30
#11 social_spl_18 covid_inpers_cont_adult_grp_28 mfq_cutoff_21 social_spl_30
#12   work_spl_18 covid_inpers_cont_adult_grp_28 mfq_cutoff_21   work_spl_30
#13  selfc_spl_18  covid4_dist_cont_adult_grp_28 mfq_cutoff_21  daily_spl_30
#14 social_spl_18  covid4_dist_cont_adult_grp_28 mfq_cutoff_21 social_spl_30
#15   work_spl_18  covid4_dist_cont_adult_grp_28 mfq_cutoff_21   work_spl_30
#16  selfc_spl_18   social_integration_sc_grp_25 gad_cutoff_21  daily_spl_30
#17 social_spl_18   social_integration_sc_grp_25 gad_cutoff_21 social_spl_30
#18   work_spl_18   social_integration_sc_grp_25 gad_cutoff_21   work_spl_30
#19  selfc_spl_18             emo_supp_sc_grp_25 gad_cutoff_21  daily_spl_30
#20 social_spl_18             emo_supp_sc_grp_25 gad_cutoff_21 social_spl_30
#21   work_spl_18             emo_supp_sc_grp_25 gad_cutoff_21   work_spl_30
#22  selfc_spl_18            prac_supp_sc_grp_25 gad_cutoff_21  daily_spl_30
#23 social_spl_18            prac_supp_sc_grp_25 gad_cutoff_21 social_spl_30
#24   work_spl_18            prac_supp_sc_grp_25 gad_cutoff_21   work_spl_30
#25  selfc_spl_18 covid_inpers_cont_adult_grp_28 gad_cutoff_21  daily_spl_30
#26 social_spl_18 covid_inpers_cont_adult_grp_28 gad_cutoff_21 social_spl_30
#27   work_spl_18 covid_inpers_cont_adult_grp_28 gad_cutoff_21   work_spl_30
#28  selfc_spl_18  covid4_dist_cont_adult_grp_28 gad_cutoff_21  daily_spl_30
#29 social_spl_18  covid4_dist_cont_adult_grp_28 gad_cutoff_21 social_spl_30
#30   work_spl_18  covid4_dist_cont_adult_grp_28 gad_cutoff_21   work_spl_30

ldat_pain <- ldat[ldat$pain_1day_mnth_18 %in% 1,]
ldat_intconf_pain <- ldat_intconf[ldat_intconf$pain_1day_mnth_18 %in% 1,]

# Save datasets
write_dta(data=ldat,path=paste0(path_to_data,"ldat.dta"))
write_dta(data=ldat_intconf,path=paste0(path_to_data,"ldat_intconf.dta"))
write_dta(data=ldat_pain,path=paste0(path_to_data,"ldat_pain.dta"))
write_dta(data=ldat_intconf_pain,path=paste0(path_to_data,"ldat_intconf_pain.dta"))

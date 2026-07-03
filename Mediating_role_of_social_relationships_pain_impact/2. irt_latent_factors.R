# Load packages

library(tidyverse) 
library(psych)
library(mirt) 
library(devtools)
#devtools::install_github("masurp/ggmirt")
library(ggmirt) 
library(cowplot) 
library(naniar)
library(RMX)
library(haven)
library(labelled)

# Paths
path_to_data <- "//path/to/data/"

# Get data
alldata <- read_dta(paste0(path_to_data,'data.dta'))
alldata <- data.frame(to_factor(alldata, sort_levels='auto'))
alldata <- remove_attributes(alldata, 'format.stata')


# Subset to just those with acute pain data
data <- alldata[!is.na(alldata$pain_1day_mnth_18),]
nrow(data) #3936 (2100 no pain, 1836 with pain)


# Create datasets for each latent construct for IRT analysis

## self-care pain impact
data %>% dplyr::select(aln, 
                       sf36_sc_modacts_18, 
                       sf36_sc_groceries_18, 
                       sf36_sc_onestair_18, 
                       sf36_sc_kneel_18, 
                       sf36_sc_walkhlfmile_18,
                       sf36_sc_walk100m_18, 
                       sf36_sc_bathdress_18) -> tf4_selfcare #7 items, 2066 not all NA, 2048 complete cases

table(apply(is.na(tf4_selfcare[,-1]), 1, function(x) !all(x)))
table(complete.cases(tf4_selfcare))
table(rowSums(is.na(tf4_selfcare[-1])))

tf4_selfcare <- tf4_selfcare[complete.cases(tf4_selfcare), ]

## social pain impact
data %>% dplyr::select(aln,
                       sf36_s_amount_18, 
                       sf36_s_accomp_18, 
                       sf36_s_lesscareful_18,
                       sf36_s_extent_soc_18, 
                       sf36_s_amount_soc_18) -> tf4_social #5 items, 2068 not all NA, 2015 complete cases

table(apply(is.na(tf4_social[,-1]), 1, function(x) !all(x)))
table(complete.cases(tf4_social))
table(rowSums(is.na(tf4_social[-1])))

tf4_social <- tf4_social[complete.cases(tf4_social), ]

## work pain impact
data %>% dplyr::select(aln, 
                       sf36_w_cuttime_18, 
                       sf36_w_accomp_18, 
                       sf36_w_limitkind_18, 
                       sf36_w_difficulty_18) -> tf4_work #4 items, 2040 not all NA, 2037 complete cases

table(apply(is.na(tf4_work[,-1]), 1, function(x) !all(x)))
table(complete.cases(tf4_work))
table(rowSums(is.na(tf4_work[-1])))

tf4_work <- tf4_work[complete.cases(tf4_work), ]


# Model estimation. Here we indicate that we are estimating a Rasch model, and standard errors for parameters are estimated.

## self-care pain impact
sc.1pl.fit <- mirt::mirt(tf4_selfcare[,-1], 1, itemtype = "graded", verbose = F)

## social pain impact
so.1pl.fit <- mirt::mirt(tf4_social[-1], 1, itemtype = "graded", verbose = F)

## work pain impact
w.1pl.fit <- mirt::mirt(tf4_work[,-1], 1, itemtype = "graded", verbose = F)


# Extract factor loadings (discrimination parameters) and see highest variables
selfc_loadings <- mirt::coef(sc.1pl.fit, IRTpars = TRUE, simplify = TRUE)$items[, "a"]
selfc_loadings <- sort(selfc_loadings, decreasing = TRUE)
selfc_loadings #sf36_sc_walk100m_18  

soc_loadings <- mirt::coef(so.1pl.fit, IRTpars = TRUE, simplify = TRUE)$items[, "a"]
soc_loadings <- sort(soc_loadings, decreasing = TRUE)
soc_loadings #sf36_s_accomp_18  

work_loadings <- mirt::coef(w.1pl.fit, IRTpars = TRUE, simplify = TRUE)$items[, "a"]
work_loadings <- sort(work_loadings, decreasing = TRUE)
work_loadings #sf36_w_limitkind_18  


# Extract latent trait scores for each domain
irt_scores <- list(
  'aln' = data.frame('aln' = data$aln, 'qlet' = data$qlet),
  'sc_irt' = data.frame('aln' = tf4_selfcare$aln, 'selfc_irt_18' = mirt::fscores(sc.1pl.fit)[,1]),
  'so_irt' = data.frame('aln' = tf4_social$aln, 'social_irt_18' = mirt::fscores(so.1pl.fit)[,1]),
  'w_irt' = data.frame('aln' = tf4_work$aln, 'work_irt_18' = mirt::fscores(w.1pl.fit)[,1])
  )

# Combine scores in single dataframe
latent_scores <- reduce(irt_scores, ~ left_join(.x, .y, by = "aln"))
latent_scores$qlet <- NULL

# Check non-missing
sapply(latent_scores, function(x) sum(!is.na(x)))

# Dichotomise IRT variables
latent_scores$selfc_spl_18 <- ifelse(is.na(latent_scores$selfc_irt_18), NA, ifelse(latent_scores$selfc_irt_18 < 0, 0, 1))
latent_scores$social_spl_18 <- ifelse(is.na(latent_scores$social_irt_18), NA, ifelse(latent_scores$social_irt_18 < 0, 0, 1))
latent_scores$work_spl_18 <- ifelse(is.na(latent_scores$work_irt_18), NA, ifelse(latent_scores$work_irt_18 < 0, 0, 1))

# Merge all data
mergedata <- merge(x = data,
                   y = latent_scores,
                   by = 'aln')

# Check data
sapply(mergedata[,c('selfc_spl_18','social_spl_18','work_spl_18')], function(x) table(x))

# Subset to those with pain in past month at age 18
merg_dat_pain <- mergedata[mergedata$pain_1day_mnth_18 %in% 1,]
nrow(merg_dat_pain) #1836

sapply(merg_dat_pain[,c('selfc_spl_18','social_spl_18','work_spl_18')], function(x) table(x))


# Save datasets
write_dta(data=mergedata,path=paste0(path_to_data,"data_irt_all.dta"))
write_dta(data=merg_dat_pain,path=paste0(path_to_data,"data_irt.dta"))

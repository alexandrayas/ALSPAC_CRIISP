# Load packages
library(dplyr)

# Variables labels
exp_labs <- c('selfc_spl_18','social_spl_18','work_spl_18')
intc_labs <- c('mfq_cutoff_21','gad_cutoff_21')
med_labs <- c('social_integration_sc_grp_25',
              'emo_supp_sc_grp_25',
              'prac_supp_sc_grp_25',
              'covid_inpers_cont_adult_grp_28',
              'covid4_dist_cont_adult_grp_28')
outc_labs <- c('daily_spl_30', 'social_spl_30', 'work_spl_30')

# Read in log files
logpath <- "//path/to/log_files/"

log_lines <- list(
  'all' = lapply(1:15, function(i) {readLines(paste0(logpath, "gformula_all_log_", i, ".log"))}),
  'all_intc' = lapply(1:30, function(i) {readLines(paste0(logpath, "gformula_all_intc_log_", i, ".log"))}),
  'wpain' = lapply(1:15, function(i) {readLines(paste0(logpath, "gformula_wpain_log_", i, ".log"))}),
  'wpain_intc' = lapply(1:30, function(i) {readLines(paste0(logpath, "gformula_wpain_intc_log_", i, ".log"))})
)

# Pull gformula estimates from log files
get_gform_ests <- function(logout){
  
  effect_lines <- grep("r\\((se_)?(tce|cde|nie|nde|pm)\\)", logout, value = TRUE)
  effect_names <- sub("^\\s*r\\((.*?)\\)\\s*=.*", "\\1", effect_lines)
  effect_vals <- as.numeric(sub(".*=\\s*", "", effect_lines))
  named_effects <- setNames(effect_vals, effect_names)
  
  out <- data.frame(
    'effect' = grep("se_", names(named_effects), value=T, invert=T),
    'logOR' = named_effects[grep("se_", names(named_effects), invert=T)],
    'se' = named_effects[grep("se_", names(named_effects))]
  )
  
  return(out)
}

gform_ests <- lapply(log_lines, function(x) lapply(x, get_gform_ests))

# Tidy gformula results/labels
tidy_gform_ests <- function(gform_out, is_intc, n_mods, pop){
  out <- do.call('rbind',gform_out)
  out$exposure <- rep(exp_labs, times=n_mods/length(exp_labs), each=5)
  out$outcome <- rep(outc_labs, times=n_mods/length(outc_labs), each=5)
  
  if(is_intc == F){
  out$mediator <- rep(med_labs, each=n_mods)
  out$intconf <- "none"
  } else if(is_intc == T){
    out$mediator <- rep(med_labs, each=length(exp_labs)*length(med_labs), times=2)
    out$intconf <- rep(intc_labs, each=nrow(out)/length(intc_labs))
  }
  
  out$pop <- pop
  
  out <- out %>%
    mutate(
      'OR' = exp(logOR),
      'LCI' = exp(logOR - 1.96 * se),
      'UCI' = exp(logOR + 1.96 * se),
      'logLCI' = logOR - 1.96 * se,
      'logUCI' = logOR + 1.96 * se
      ) %>% 
    mutate(
      OR_LCI_UCI = ifelse(effect %in% 'pm', sprintf("%.2f%%", logOR * 100), sprintf("%.2f (%.2f–%.2f)", OR, LCI, UCI)),
      effect = factor(out$effect, levels=c('tce','cde','nde','nie','pm'), labels=c('TCE','CDE','NDE','NIE','PM'))
    )
  
  return(out)
}

gform_ests <- list(
  'all' = tidy_gform_ests(gform_ests$all, is_intc=F, n_mods=15, pop="all"),
  'all_intc' = tidy_gform_ests(gform_ests$all_intc, is_intc=T, n_mods=30, pop="all"),
  'wpain' = tidy_gform_ests(gform_ests$wpain, is_intc=F, n_mods=15, pop="wpain"),
  'wpain_intc' = tidy_gform_ests(gform_ests$wpain_intc, is_intc=T, n_mods=30, pop="wpain")
)

gform_ests <- do.call('rbind',gform_ests)

# Save mediation estimates
save(gform_ests, file="//path/to/outputs/gformula_estimates.rda")

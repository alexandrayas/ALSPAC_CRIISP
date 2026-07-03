# Load packages
library(nnet)
library(mice)
library(tidyverse)
library(gt)

# Load data
load("//path/to/data/imp_netw_z_scs.rda")

# Path to outputs
path_to_outputs <- "//path/to/outputs/"

# Domain names
cluslabs <- list(
  `clus1` = "Functioning difficulties/behaviour",
  `clus2` = "Psychological wellbeing",
  `clus3` = "Distress/dysregulation/adversity",
  `clus4` = "Social communication/peer problems",
  `clus5` = "Support/security",
  `clus6` = "Activity/resources/roles",
  `clus7` = "Body composition/metabolic"
)

# Covariates
covariates <- c('sex','ethnicity','parent_sc_0', 'pain_prob_7','parent_pain_12')

# Function to fit one model with single domain score
fit_1dom_mod_per_imp <- function(dat, exp, outc, covar = NULL, ref = ref) {
  dat[[outc]] <- relevel(factor(dat[[outc]]), ref = ref)
  terms <- c(exp, covar)
  multinom(
    reformulate(terms, response = outc), data = dat, trace=F #use trace=T for convergence messages
  )
}

# Function to fit mutually adjusted models incl all domain scores
fit_multdom_mod <- function(outc, covar = NULL, ref = ref){
  lapply(imp_netw_z_scs, function(dat){
    dat[[outc]] <- relevel(factor(dat[[outc]]), ref = ref)
    terms <- c(names(cluslabs), covar)
    multinom(reformulate(terms, response=outc), data=dat, trace=F)
  })
}

# Function to tidy effects
get_imp_effs <- function(mods_pooled, sexmod){
  
  expF <- summary(mods_pooled, conf.int=T, exponentiate=F) 
  
  expT <- summary(mods_pooled, conf.int=T, exponentiate=T) %>%
    transmute(
      `RRR`= sprintf("%.2f", estimate),
      `LCI` = sprintf("%.2f", conf.low),
      `UCI` = sprintf("%.2f", conf.high),
      `95% CI` = sprintf("%.2f, %.2f", conf.low, conf.high),
      `p-value`= if_else(p.value < 0.001, "<0.001", sprintf("%.3f", p.value))
    )
  
  out <- cbind('y.level' = mods_pooled$pooled$y.level, expF, expT)
  
  if(sexmod == F){
    out <- out %>% filter(stringr::str_detect(term, "^clus\\d+$"))
    } else if(sexmod == T){
      out <- out %>% filter(stringr::str_detect(term, "^sex"))
      }
  
  rownames(out) <- NULL
  return(out)
}

# Function to get table of effects
get_effects_tbl <- function(imp_effs){
  imp_effs %>%
    mutate(term_lab=recode(term, !!!unlist(cluslabs))) %>%
    transmute(
      `Outcome (vs reference)`=y.level,
      Exposure=term_lab,
      RRR, `95% CI`, `p-value`
    ) %>%
    gt() %>%
    tab_style(
      style=cell_text(weight="bold"),
      locations=cells_column_labels(everything())
    ) %>%
    cols_align(
      align="left",
      columns=c(`Outcome (vs reference)`, Exposure)
    ) %>%
    cols_align(
      align="center",
      columns=c(RRR, `95% CI`, `p-value`)
    )
}

# Single domain models


## Reference: Persisting low-impact pain

### Run models
singdom_mods_persistlicp_ref <- lapply(names(cluslabs), function(x) {
  lapply(
    imp_netw_z_scs,
    fit_1dom_mod_per_imp,
    exp = x,
    outc = "pain_transition_26to30",
    covar = covariates,
    ref = "Persisting, low impact"
  )
})

singdom_mods_persistlicp_ref_unadj <- lapply(names(cluslabs), function(x) {
  lapply(
    imp_netw_z_scs,
    fit_1dom_mod_per_imp,
    exp = x,
    outc = "pain_transition_26to30",
    covar = NULL,
    ref = "Persisting, low impact"
  )
})

### Pool effects
singdom_mods_persistlicp_ref <- lapply(singdom_mods_persistlicp_ref, pool)
singdom_mods_persistlicp_ref_unadj <- lapply(singdom_mods_persistlicp_ref_unadj, pool)

### Tidy effects
singdom_effs_persistlicp_ref <- singdom_mods_persistlicp_ref %>%
  purrr::map_dfr(get_imp_effs, sexmod=F) %>%
  dplyr::arrange(y.level)

singdom_effs_persistlicp_ref_unadj <- singdom_mods_persistlicp_ref_unadj %>%
  purrr::map_dfr(get_imp_effs, sexmod=F) %>%
  dplyr::arrange(y.level)

### Print table
get_effects_tbl(singdom_effs_persistlicp_ref)
get_effects_tbl(singdom_effs_persistlicp_ref_unadj)

### Save table
get_effects_tbl(singdom_effs_persistlicp_ref) %>% gtsave(filename=paste0(path_to_outputs,"singdom_effs_persistlicp_ref.docx"))
get_effects_tbl(singdom_effs_persistlicp_ref_unadj) %>% gtsave(filename=paste0(path_to_outputs,"singdom_effs_persistlicp_ref_unadj.docx"))


## Reference: Improving

### Run models
singdom_mods_improv_ref <- lapply(names(cluslabs), function(x) {
  lapply(
    imp_netw_z_scs,
    fit_1dom_mod_per_imp,
    exp = x,
    outc = "pain_transition_26to30",
    covar = covariates,
    ref = "Improving"
  )
})

singdom_mods_improv_ref_unadj <- lapply(names(cluslabs), function(x) {
  lapply(
    imp_netw_z_scs,
    fit_1dom_mod_per_imp,
    exp = x,
    outc = "pain_transition_26to30",
    covar = NULL,
    ref = "Improving"
  )
})

### Pool effects
singdom_mods_improv_ref <- lapply(singdom_mods_improv_ref, pool)
singdom_mods_improv_ref_unadj <- lapply(singdom_mods_improv_ref_unadj, pool)

### Tidy effects
singdom_effs_improv_ref <- singdom_mods_improv_ref %>%
  purrr::map_dfr(get_imp_effs, sexmod=F) %>%
  dplyr::arrange(y.level)

singdom_effs_improv_ref_unadj <- singdom_mods_improv_ref_unadj %>%
  purrr::map_dfr(get_imp_effs, sexmod=F) %>%
  dplyr::arrange(y.level)

### Print table
get_effects_tbl(singdom_effs_improv_ref)
get_effects_tbl(singdom_effs_improv_ref_unadj)

### Save table
get_effects_tbl(singdom_effs_improv_ref) %>% gtsave(filename=paste0(path_to_outputs,"singdom_effs_improv_ref.docx"))
get_effects_tbl(singdom_effs_improv_ref_unadj) %>% gtsave(filename=paste0(path_to_outputs,"singdom_effs_improv_ref_unadj.docx"))


## Reference: No pain

### Run models
singdom_mods_nopain_ref <- lapply(names(cluslabs), function(x) {
  lapply(
    imp_netw_z_scs,
    fit_1dom_mod_per_imp,
    exp = x,
    outc = "pain_transition_26to30",
    covar = covariates,
    ref = "No pain"
  )
})

singdom_mods_nopain_ref_unadj <- lapply(names(cluslabs), function(x) {
  lapply(
    imp_netw_z_scs,
    fit_1dom_mod_per_imp,
    exp = x,
    outc = "pain_transition_26to30",
    covar = NULL,
    ref = "No pain"
  )
})

### Pool effects
singdom_mods_nopain_ref <- lapply(singdom_mods_nopain_ref, pool)
singdom_mods_nopain_ref_unadj <- lapply(singdom_mods_nopain_ref_unadj, pool)

### Tidy effects
singdom_effs_nopain_ref <- singdom_mods_nopain_ref %>%
  purrr::map_dfr(get_imp_effs, sexmod=F) %>%
  dplyr::arrange(y.level)

singdom_effs_nopain_ref_unadj <- singdom_mods_nopain_ref_unadj %>%
  purrr::map_dfr(get_imp_effs, sexmod=F) %>%
  dplyr::arrange(y.level)

### Print table
get_effects_tbl(singdom_effs_nopain_ref)
get_effects_tbl(singdom_effs_nopain_ref_unadj)

### Save table
get_effects_tbl(singdom_effs_nopain_ref) %>% gtsave(filename=paste0(path_to_outputs,"singdom_effs_nopain_ref.docx"))
get_effects_tbl(singdom_effs_nopain_ref_unadj) %>% gtsave(filename=paste0(path_to_outputs,"singdom_effs_nopain_ref_unadj.docx"))





# Mutually adjusted domain models

## Reference: Persisting low-impact pain

### Run  models
multdom_mods_persistlicp_ref <- fit_multdom_mod(
  "pain_transition_26to30", 
  covar=covariates,
  ref = "Persisting, low impact"
  )

multdom_mods_persistlicp_ref_unadj <- fit_multdom_mod(
  "pain_transition_26to30", 
  covar=NULL,
  ref = "Persisting, low impact"
)

### Pool effects
multdom_mods_persistlicp_ref <- pool(multdom_mods_persistlicp_ref)
multdom_mods_persistlicp_ref_unadj <- pool(multdom_mods_persistlicp_ref_unadj)

### Tidy effects
multdom_effs_persistlicp_ref <- get_imp_effs(multdom_mods_persistlicp_ref, sexmod=F)
multdom_effs_persistlicp_ref_unadj <- get_imp_effs(multdom_mods_persistlicp_ref_unadj, sexmod=F)

### Print table
get_effects_tbl(multdom_effs_persistlicp_ref)
get_effects_tbl(multdom_effs_persistlicp_ref_unadj)

### Save table
get_effects_tbl(multdom_effs_persistlicp_ref) %>% gtsave(filename=paste0(path_to_outputs,"multdom_effs_persistlicp_ref.docx"))
get_effects_tbl(multdom_effs_persistlicp_ref_unadj) %>% gtsave(filename=paste0(path_to_outputs,"multdom_effs_persistlicp_ref_unadj.docx"))


## Reference: Improving

### Run  models
multdom_mods_improv_ref <- fit_multdom_mod(
  "pain_transition_26to30", 
  covar=covariates,
  ref = "Improving"
  )

multdom_mods_improv_ref_unadj <- fit_multdom_mod(
  "pain_transition_26to30", 
  covar=NULL,
  ref = "Improving"
)

### Pool effects
multdom_mods_improv_ref <- pool(multdom_mods_improv_ref)
multdom_mods_improv_ref_unadj <- pool(multdom_mods_improv_ref_unadj)

### Tidy effects
multdom_effs_improv_ref <- get_imp_effs(multdom_mods_improv_ref, sexmod=F)
multdom_effs_improv_ref_unadj <- get_imp_effs(multdom_mods_improv_ref_unadj, sexmod=F)

### Print table
get_effects_tbl(multdom_effs_improv_ref)
get_effects_tbl(multdom_effs_improv_ref_unadj)

### Save table
get_effects_tbl(multdom_effs_improv_ref) %>% gtsave(filename=paste0(path_to_outputs,"multdom_effs_improv_ref.docx"))
get_effects_tbl(multdom_effs_improv_ref_unadj) %>% gtsave(filename=paste0(path_to_outputs,"multdom_effs_improv_ref_unadj.docx"))


## Reference: No pain

### Run  models
multdom_mods_nopain_ref <- fit_multdom_mod(
  "pain_transition_26to30", 
  covar=covariates,
  ref = "No pain"
  )

multdom_mods_nopain_ref_unadj <- fit_multdom_mod(
  "pain_transition_26to30", 
  covar=NULL,
  ref = "No pain"
)

### Pool effects
multdom_mods_nopain_ref <- pool(multdom_mods_nopain_ref)
multdom_mods_nopain_ref_unadj <- pool(multdom_mods_nopain_ref_unadj)

### Tidy effects
multdom_effs_nopain_ref <- get_imp_effs(multdom_mods_nopain_ref, sexmod=F)
multdom_effs_nopain_ref_unadj <- get_imp_effs(multdom_mods_nopain_ref_unadj, sexmod=F)

### Print table
get_effects_tbl(multdom_effs_nopain_ref)
get_effects_tbl(multdom_effs_nopain_ref_unadj)

### Save table
get_effects_tbl(multdom_effs_nopain_ref) %>% gtsave(filename=paste0(path_to_outputs,"multdom_effs_nopain_ref.docx"))
get_effects_tbl(multdom_effs_nopain_ref_unadj) %>% gtsave(filename=paste0(path_to_outputs,"multdom_effs_nopain_ref_unadj.docx"))



# Plot regression results

## Combine all estimates into dataframe
all_effs <- list(
  singdom_effs_nopain_ref,
  singdom_effs_persistlicp_ref,
  singdom_effs_improv_ref,
  multdom_effs_nopain_ref,
  multdom_effs_persistlicp_ref,
  multdom_effs_improv_ref
)
plt_effs <- bind_rows(all_effs)
plt_effs$y.level <- factor(plt_effs$y.level, 
                           levels = c('No pain','Acute',
                                      'Persisting, low impact','Persisting, high impact',
                                      'Improving','Worsening'), 
                           labels = c('No pain','Acute',
                                      'Persisting low-impact','Persisting high-impact',
                                      'Improving','Worsening'))
plt_effs$mod_type <- factor(
  rep(c('Single domain','Mutually adjusted domains'), each=105), 
  levels=c('Single domain','Mutually adjusted domains'))
plt_effs$reference <- factor(
  rep(c('No pain','Persisting low-impact','Improving'), each=35, times=2), 
  levels=c('No pain','Persisting low-impact','Improving'))
plt_effs$ref_mod <- interaction(plt_effs$ref, plt_effs$mod_type, sep = " | ", lex.order = TRUE)

# Domain names
pltlabs <- list(
  `clus1` = "Functional-behavioural difficulties",
  `clus2` = "Positive wellbeing",
  `clus3` = "Distress and adversity",
  `clus4` = "Social communication/peer problems",
  `clus5` = "Interpersonal support/security",
  `clus6` = "Activity engagement + resources",
  `clus7` = "Bio-health indicators"
)

plt_effs$term_lab <- recode(plt_effs$term, !!!unlist(pltlabs))

# Save regression estimates
save(plt_effs, file=paste0(path_to_outputs,'regress_effs.rda'))

# Remove repeated contrasts
load(paste0(path_to_outputs,'regress_effs.rda'))

plt_effs <- plt_effs[!(plt_effs$y.level %in% 'No pain'),]
plt_effs$y.level <- droplevels(plt_effs$y.level)
plt_effs <- plt_effs[!(plt_effs$reference %in% 'Improving' & plt_effs$y.level %in% 'Persisting low-impact'),]

## Produce plot
get_regress_plt <- function(effsdf){
  pd <- position_dodge(width = 0.55)
  ggplot(effsdf, aes(x=term_lab, y=estimate, 
                     ymin=conf.low, ymax=conf.high, 
                     colour=mod_type, shape=reference)) +
    theme_minimal() +
    facet_grid(rows=vars(y.level)) +
    geom_hline(yintercept=0, linetype="dotted") +
    geom_errorbar(aes(group=ref_mod), lwd=0.5, width=0.3, position=pd) +
    geom_point(aes(group=ref_mod), size=1.5, position=pd) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
          text = element_text(size = 8),
          legend.position="bottom", 
          legend.spacing.x = unit(1, "mm"),
          legend.key.width = unit(3, "mm"),
          strip.background=element_blank(),
          strip.placement = "outside", 
          strip.text=element_text(size=6)) + #face="bold", 
    labs(x='\nNetwork-derived domain', y='log(RRR)\n',colour='Model : ',shape='Reference : ')
}
regress_plt <- get_regress_plt(plt_effs)
regress_plt

## Save plot
ggsave(plot=regress_plt, filename=paste0(path_to_outputs,"regress_plt.png"), width=174, height=214, units="mm", dpi=350)



# Sex differences in domain scores

## Fit sex -> domain score model in one imputed dataset
fit_sex_domain_per_imp <- function(dat, domain, sexvar = "sex") {
  
  dat <- dat %>%
    mutate("{sexvar}" := relevel(factor(.data[[sexvar]]), ref = "Male"))
  
  lm(reformulate(sexvar, response = domain), data = dat)
}

## Fit and pool across imputations for one domain
pool_sex_domain <- function(dat_list, domain, sexvar = "sex") {
  
  mods <- lapply(
    dat_list,
    fit_sex_domain_per_imp,
    domain = domain,
    sexvar = sexvar
  )
  
  pool(mods)
}

## Run across all domains
sex_domain_mods <- names(cluslabs) %>%
  set_names() %>%
  map(
    ~ pool_sex_domain(
      dat_list = imp_netw_z_scs,
      domain = .x,
      sexvar = "sex"
    )
  )

## Extract estimates
sex_domain_effs <- imap_dfr(
  sex_domain_mods,
  ~ summary(.x, conf.int = TRUE) %>%
    as_tibble() %>%
    filter(term == "sexFemale") %>%
    mutate(
      domain = .y,
      domain_lab = unname(cluslabs[.y]),
      .before = 1
    )
) %>%
  mutate(
    mean_diff_CI = sprintf(
      "%.2f (%.2f, %.2f)",
      estimate, conf.low, conf.high
    ),
    p_value = if_else(
      p.value < 0.001,
      "<0.001",
      sprintf("%.3f", p.value)
    )
  )

## Make gt table sex_tab <- sex_domain_effs %>%
sex_tab <- sex_domain_effs %>%
  select(
    domain_lab,
    mean_diff_CI,
    p_value
  ) %>%
  gt(rowname_col = "domain_lab") %>%
  cols_label(
    mean_diff_CI = "Mean difference (95% CI)",
    p_value = "p-value"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()
  )

sex_tab

## Save table
#sex_tab %>% gtsave(filename=paste0(path_to_outputs,"sex_tab.docx"))

# Load packages
library(haven)
library(labelled)
library(tidyverse)

# Paths
path_to_data <- "//path/to/data/"
path_to_outputs <- "//path/to/outputs/"

# Read in and remove attributes
alldf <- read_dta(paste0(path_to_data,'data_irt_all.dta'))
alldf <- data.frame(to_factor(alldf, sort_levels='auto'))
alldf <- remove_attributes(alldf, 'format.stata')

# Subset dataset to those with or without pain in past month at baseline
df_nopain <- alldf[alldf$pain_1day_mnth_18 %in% 0,]
df_wpain <- alldf[alldf$pain_1day_mnth_18 %in% 1,]

# Define exposures, intermediate confounders, mediators and outc_labs
exp_labs <- c('selfc_spl_18','social_spl_18','work_spl_18')
intc_labs <- c('mfq_cutoff_21','gad_cutoff_21')
med_labs <- c('social_integration_sc_grp_25',
              'emo_supp_sc_grp_25',
              'prac_supp_sc_grp_25',
              'covid_inpers_cont_adult_grp_28',
              'covid4_dist_cont_adult_grp_28')
outc_labs <- c('daily_spl_30', 'social_spl_30', 'work_spl_30')

# Baseline confounders (not including sex)
baseconf <- c('ethnicity', 'm_pregsmk', 'parent_sc_0', 'm_homown_0', 'parent_mhp_1')

# Model-fitting function
fit_mod <- function(y, x, xtra = NULL, data,
                    base = baseconf){
  
  terms <- c(x, xtra, base)
  
  form <- reformulate(termlabels = terms, response = y)
  
  glm(form,
      family = binomial(link = "logit"),
      data = data
  )
}

# Create model-specification dataframe
make_model_grid <- function(outcomes, exposures, model_set,
                            xtra = NULL,
                            base = baseconf,
                            adjustments = "baseline",
                            domain_adjusted = FALSE) {
  
  early_pain_by_outcome <- c(
    daily_spl_30  = "selfc_spl_18",
    social_spl_30 = "social_spl_18",
    work_spl_30   = "work_spl_18"
  )
  
  tidyr::expand_grid(
    outcome = outcomes,
    exposure = exposures
  ) |>
    mutate(
      model_set = model_set,
      xtra = if (domain_adjusted == TRUE) {
        map(outcome, ~ c(unname(early_pain_by_outcome[.x]), xtra))
      } else if (domain_adjusted == FALSE){
        rep(list(xtra), n())
      },
      base = rep(list(base), n()),
      adjustments = adjustments
    )
}

# Define all the models
model_specs <- bind_rows(
  
  # Health limitations on later measures
  make_model_grid(
    outcomes = c(intc_labs, med_labs, outc_labs),
    exposures = exp_labs,
    model_set = "hlthlim_on",
    base = c("sex",baseconf)
  ),
  
  # Mental health on social support
  imap_dfr(
    set_names(exp_labs),
    ~ make_model_grid(
      outcomes = med_labs,
      exposures = intc_labs,
      model_set = "mh_on_support",
      xtra = .x,
      base = c("sex",baseconf),
      adjustments = .y
    )
  ),
  
  # Mental health on later pain impact
  make_model_grid(
    outcomes = outc_labs,
    exposures = intc_labs,
    model_set = "mh_on_painimpact",
    base = c("sex",baseconf),
    adjustments = "domain_hlthlim",
    domain_adjusted = TRUE
  ),
  
  # Social support on later pain impact
  imap_dfr(
    list(
      no_mh = NULL,
      dep = "mfq_cutoff_21",
      anx = "gad_cutoff_21"
    ),
    ~ make_model_grid(
      outcomes = outc_labs,
      exposures = med_labs,
      model_set = "support_on_painimpact",
      xtra = .x,
      base = c("sex",baseconf),
      domain_adjusted = TRUE,
      adjustments = .y
    )
  ),
  
  # Baseline adjusted sex interaction models
  make_model_grid(
    outcomes = c(intc_labs, med_labs, outc_labs),
    exposures = paste0("sex*",exp_labs),
    model_set = "sex_interaction_adj",
  ),
  
  # Unadjusted sex interaction models
  make_model_grid(
    outcomes = c(intc_labs, med_labs, outc_labs),
    exposures = paste0("sex*",exp_labs),
    model_set = "sex_interaction_unadj",
    base = NULL,
    adjustments = "unadjusted"
  ),
  
  # Sex as exposure
  make_model_grid(
    outcomes = c(exp_labs, intc_labs, med_labs, outc_labs),
    exposures = "sex",
    model_set = "sex_as_exposure",
    base = NULL,
    adjustments = "unadjusted"
  )
)

# Fit models
get_model_fits <- function(data, pop){
  out <- model_specs |>
    mutate(
      model = pmap(
        list(outcome, exposure, xtra, base),
        \(outcome, exposure, xtra, base) {
          fit_mod(
            y = outcome,
            x = exposure,
            xtra = xtra,
            base = base,
            data = data
          )
        }
      ),
      pop = pop
    )
  return(out)
}

model_fits_all <- get_model_fits(alldf, "full_sample")
model_fits_wpain <- get_model_fits(df_wpain, "samp_with_pain")

# Tidy all estimates into one dataframe
get_event_n <- function(mod) {
  y <- model.response(model.frame(mod))
  
  if (is.factor(y)) {
    sum(y == levels(y)[2], na.rm = TRUE)
  } else {
    sum(y == 1, na.rm = TRUE)
  }
}

tidy_model <- function(mod) {
  
  term_labels <- attr(terms(mod), "term.labels")
  mm <- model.matrix(mod)
  
  term_lookup <- tibble(
    term = colnames(mm)[-1],
    model_term = term_labels[attr(mm, "assign")[-1]]
  )
  
  broom::tidy(mod) |>
    filter(term != "(Intercept)") |>
    mutate(
      conf.low = estimate - 1.96 * std.error,
      conf.high = estimate + 1.96 * std.error,
      OR = exp(estimate),
      conf.low = exp(conf.low),
      conf.high = exp(conf.high),
      n = nobs(mod),
      events = get_event_n(mod)
    ) |>
    left_join(term_lookup, by = "term")
}

# Extract estimates
results_all <- model_fits_all |>
  mutate(tidy = map(model, tidy_model)) |>
  select(-model) |>
  unnest(tidy)

results_wpain <- model_fits_wpain |>
  mutate(tidy = map(model, tidy_model)) |>
  select(-model) |>
  unnest(tidy)

results <- bind_rows(results_all,results_wpain)

# Add formatted estimates
get_clean_results <- function(result_df){
  out <- result_df |>
    mutate(
      OR_CI = sprintf(
        "%.2f (%.2f, %.2f)",
        OR, conf.low, conf.high
      ),
      p = gtsummary::style_pvalue(p.value, digits = 3),
      term_type = case_when(
        model_term == exposure ~ "exposure",
        str_detect(term, ":") | str_detect(model_term, ":") ~ "sex_interaction",
        model_term == "sex" ~ "sex",
        model_term %in% baseconf ~ "baseconf",
        TRUE ~ "covariate"
      )
    )
  return(out)
}

results_clean <- get_clean_results(results)


# Save regression results
save(results_clean, file=paste0(path_to_outputs,"regression_estimates.rda"))


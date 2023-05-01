# This script replicate the Empirical Application in Roth and Sant'Anna (2023)
# It reproduces Table 6 and Appendix Table 14.
#---------------------------------------------------------------------------------------------------------------
library(here)
library(dplyr)
library(ggplot2)
library(purrr)
#library(doAzureParallel)
library(devtools)
#devtools::install_github("jonathandroth/staggered@add_balance_tests")
library(staggered)
library(glue)
library(RColorBrewer)
library(gt)
library(suptCriticalValue)
library(webshot2)
library(tidyr)
#---------------------------------------------------------------------------------------------------------------
source(here("Code/aux_fte_theme.R"))

load(here("Data/procedural_justice_revisited/products/rdata/3_officer_level_data.RData"))
#---------------------------------------------------------------------------------------------------------------
num_fisher_permutations = 500
set.seed(11182021)
#---------------------------------------------------------------------------------------------------------------
balance_checks_for_outcome <-
  function(outcome, df){
    df <- df %>% 
      rename(t = period,
             y = !!outcome,
             g = first_trained,
             i = uid)
    all_results <- staggered::balance_checks(df = df, 
                                             estimand = "all",
                                             eventTime = 0, 
                                             compute_fisher = TRUE,
                                             num_fisher_permutations = num_fisher_permutations)$resultsDF
    
    results <- bind_rows(all_results) %>% 
      mutate(outcome = outcome)
    return(results)
  }

#---------------------------------------------------------------------------------------------------------------
## Summary parameter results for entire sample

complaints_results <- balance_checks_for_outcome(outcome = "complaints", 
                                                 df = pj_officer_level_balanced)
sustained_results <- balance_checks_for_outcome(outcome = "sustained",
                                                df = pj_officer_level_balanced)
force_results <- balance_checks_for_outcome(outcome = "force", 
                                            df = pj_officer_level_balanced)
bal_checks_results <- bind_rows(complaints_results,
                                sustained_results,
                                force_results)

saveRDS(bal_checks_results,
        here("Temp/bal_check_results.rds"))
#---------------------------------------------------------------------------------------------------------------
# Drop Special units and pilot
#---------------------------------------------------------------------------------------------------------------
assignment <- readRDS(here("Data/procedural_justice-master/products/rdata/assignment.rds"))
pj_officer_level_balanced_nospecial_nopilot <-
  pj_officer_level_balanced %>%
  left_join(assignment %>%
              select(uid,type,unit),
            by = "uid") %>%
  filter(!is.na(type) & type != "special") %>%
  filter(first_trained > 14)


## Summary parameter results without special units and pilot----
complaints_results_nospecial_nopilot <- balance_checks_for_outcome(outcome = "complaints",
                                                                   df = pj_officer_level_balanced_nospecial_nopilot)
sustained_results_nospecial_nopilot <- balance_checks_for_outcome(outcome = "sustained", 
                                                                  df = pj_officer_level_balanced_nospecial_nopilot)
force_results_nospecial_nopilot <- balance_checks_for_outcome(outcome = "force",
                                                              df = pj_officer_level_balanced_nospecial_nopilot)
bal_checks_results_nospecial_nopilot <- bind_rows(complaints_results_nospecial_nopilot,
                                                  sustained_results_nospecial_nopilot,
                                                  force_results_nospecial_nopilot)

saveRDS(bal_checks_results_nospecial_nopilot,
        here("Temp/bal_check_results_nospecial_nopilot.rds"))

#---------------------------------------------------------------------------------------------------------------
# Drop later cohorts and special/pilot units
#---------------------------------------------------------------------------------------------------------------
pj_officer_level_balanced_nospecial_nopilot_omit_late_g <- pj_officer_level_balanced_nospecial_nopilot %>%
  filter(first_trained<60)


## Summary parameter results without special units and dropping later cohorts----
complaints_results_nospecial_nopilot_omit_late_g  <- balance_checks_for_outcome(outcome = "complaints",
                                                                                df = pj_officer_level_balanced_nospecial_nopilot_omit_late_g )
sustained_results_nospecial_nopilot_omit_late_g  <- balance_checks_for_outcome(outcome = "sustained",
                                                                               df = pj_officer_level_balanced_nospecial_nopilot_omit_late_g )
force_results_nospecial_nopilot_omit_late_g  <- balance_checks_for_outcome(outcome = "force", 
                                                                           df = pj_officer_level_balanced_nospecial_nopilot_omit_late_g )
bal_checks_results_nospecial_nopilot_omit_late_g  <- bind_rows(complaints_results_nospecial_nopilot_omit_late_g , 
                                                     sustained_results_nospecial_nopilot_omit_late_g , 
                                                     force_results_nospecial_nopilot_omit_late_g )

saveRDS(bal_checks_results_nospecial_nopilot_omit_late_g,
        here("Temp/bal_check_results_nospecial_nopilot_omit_late_g.rds"))

#---------------------------------------------------------------------------------------------------------------
# Put all in a Table


## Load results here XX 

#function for relabeling estimands
relabel_estimands <- 
  function(df){
  df %>%
  mutate(estimand = replace(estimand, estimand=="all_simple", "Simple")) %>% 
    mutate(estimand = replace(estimand, estimand=="all_cohort", "Cohort")) %>% 
    mutate(estimand = replace(estimand, estimand=="all_calendar", "Calendar")) %>% 
    mutate(estimand = replace(estimand, estimand=="all_ES0", "ES0"))
}

bal_checks_results <- bal_checks_results %>% 
  mutate(sample = "Including pilot + special") %>% 
  relabel_estimands()         

bal_checks_results_nospecial_nopilot <- bal_checks_results_nospecial_nopilot %>% 
  mutate(sample = "Main estimation sample")%>% 
  relabel_estimands()


bal_checks_results_nospecial_nopilot_omit_late_g <- bal_checks_results_nospecial_nopilot_omit_late_g %>% 
  mutate(sample = "Omit later cohorts")%>% 
  relabel_estimands()

#---------------------------------------------------------------------------------------------------------------
#Table comparing balance for main estimation sample versus including all officers (Table 6)

rbind(bal_checks_results_nospecial_nopilot, bal_checks_results) %>%
  select(Xhat, t_test, pvalue_t, fisher_pval, fisher_supt_pval, outcome, estimand, sample) %>%
  tidyr::pivot_wider(id_cols = c(estimand, outcome), 
                     names_from = sample,
                     values_from = -c(estimand, outcome,sample)) %>%
  select(outcome, estimand, contains("Main"), contains("Including")) %>% #order columns
  gt() %>%
  fmt_number(contains("Xhat"), decimals = 3) %>%
  fmt_number(contains("fisher"), decimals = 2) %>%
  fmt_number(contains("t_test"), decimals = 2) %>%
  fmt_number(contains("pvalue"), decimals = 2) %>%
  tab_spanner(label = "Main Estimation Sample", contains("Main")) %>%
  tab_spanner(label = "Including pilot + special", contains("Including")) %>%
  cols_label(outcome = "Outcome", estimand = "Estimand",
             "Xhat_Including pilot + special" = "Xhat",
             "t_test_Including pilot + special" = "t-stat",
             "pvalue_t_Including pilot + special" = "p-val",
             "fisher_pval_Including pilot + special" = "p-val (FRT)",
             "fisher_supt_pval_Including pilot + special" = "Joint p-val \n(FRT)",
             "Xhat_Main estimation sample" = "Xhat",
             "t_test_Main estimation sample" = "t-stat",
             "pvalue_t_Main estimation sample"  = "p-val",
             "fisher_pval_Main estimation sample" = "p-val (FRT)",
             "fisher_supt_pval_Main estimation sample" = "Joint p-val \n(FRT)"
  ) %>%
  gtsave(here("Tables/wood-et-al-application-balance-checks-main-versus-all.png"))

#---------------------------------------------------------------------------------------------------------------
# Appendix Table 14, which is similar to Table 6 but omit late treated units

rbind(bal_checks_results_nospecial_nopilot, bal_checks_results_nospecial_nopilot_omit_late_g) %>%
  select(Xhat, t_test, pvalue_t, fisher_pval, fisher_supt_pval, outcome, estimand, sample) %>%
  tidyr::pivot_wider(id_cols = c(estimand, outcome), 
                     names_from = sample,
                     values_from = -c(estimand,outcome,sample)) %>%
  select(outcome, estimand, contains("Main"), contains("Omit")) %>% #order columns
  gt() %>%
  fmt_number(contains("Xhat"), decimals = 3) %>%
  fmt_number(contains("fisher"), decimals = 2) %>%
  fmt_number(contains("t_test"), decimals = 2) %>%
  fmt_number(contains("pvalue"), decimals = 2) %>%
  tab_spanner(label = "Main Estimation Sample", contains("Main")) %>%
  tab_spanner(label = "Omit later treated", contains("Omit")) %>%
  cols_label(outcome = "Outcome", estimand = "Estimand",
             "Xhat_Omit later cohorts" = "Xhat",
             "t_test_Omit later cohorts" = "t-stat",
             "pvalue_t_Omit later cohorts" = "p-val",
             "fisher_pval_Omit later cohorts" = "p-val (FRT)",
             "fisher_supt_pval_Omit later cohorts" = "Joint p-val \n(FRT)",
             "Xhat_Main estimation sample" = "Xhat",
             "t_test_Main estimation sample" = "t-stat",
             "pvalue_t_Main estimation sample"  = "p-val",
             "fisher_pval_Main estimation sample" = "p-val (FRT)",
             "fisher_supt_pval_Main estimation sample" = "Joint p-val \n(FRT)"
  ) %>%
  gtsave(here("Tables/wood-et-al-application-balance-checks-main-versus-omit-late.png"))


#---------------------------------------------------------------------------------------------------------------
save.image(here("Temp/wood-et-al-application/balance_checks_application.RData"))

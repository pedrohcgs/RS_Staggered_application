# This script replicate the Empirical Application in Roth and Sant'Anna (2023)
# It revisit the the Wood et al. application, but focus attention on the sample
# with no special forces and without the pilot period.
#---------------------------------------------------------------------------------------------------------------
rm(list=ls(all=TRUE))
#---------------------------------------------------------------------------------------------------------------
library(here)
library(dplyr)
library(ggplot2)
library(purrr)
#library(doAzureParallel)

library(staggered)
library(glue)
library(RColorBrewer)
library(gt)
remotes::install_github("ryanedmundkessler/suptCriticalValue")
library(suptCriticalValue)
library(webshot2)
#---------------------------------------------------------------------------------------------------------------
source(here("Code/aux_fte_theme.R"))
set.seed(20211124)
num_fisher_permutations = 5000
#---------------------------------------------------------------------------------------------------------------
# Functions to compute event study (must have more than 1 event time, otherwise supT function breaks)

compute_efficient_event_study <- function(df, firstTime =0, lastTime, beta = NULL,...){
  staggeredResults <- staggered(df = df, estimand = "eventstudy", 
                                eventTime = firstTime:lastTime,
                                beta=beta, 
                                return_full_vcv = TRUE,
                                num_fisher_permutations = num_fisher_permutations,
                                ...)   
  df <- staggeredResults$resultsDF
  vcv <- staggeredResults$vcv
  
  #Compute the sup-t critical value and save in the dataset
  #Remove rows of vcv corresponding with 0 variance ()
  df$supT <- suptCriticalValue::suptCriticalValue(vcov_matrix = vcv[which(diag(vcv)>10^(-6)),
                                                                    which(diag(vcv)>10^(-6))], 
                                                  num_sim = 10^4)
  
  # purrr::map_dfr(.x = firstTime:lastTime, 
  #              .f = ~staggered(df = df, estimand = "eventstudy", eventTime = .x, beta=beta,...)%>% mutate(eventTime = .x) )
  
  return(df)
}

compute_efficient_eventstudy_for_outcome <-
  function(outcome, df,firstTime = 0, lastTime,beta = NULL,...){
    #df <- simulate_data_fn(seed)
    df <- df %>% rename(t = period, y = !!outcome, g = first_trained, i = uid)
    
    results <- compute_efficient_event_study(df=df, firstTime = firstTime, lastTime = lastTime,beta=beta,...)
    results <- results %>% mutate(outcome = outcome)
    
    return(results)
  }

#---------------------------------------------------------------------------------------------------------------
# Load and clean data
load(here("Data/procedural_justice_revisited/products/rdata/3_officer_level_data.RData"))


assignment <- readRDS(here("Data/procedural_justice-master/products/rdata/assignment.rds"))
pj_officer_level_balanced_nospecial_nopilot <-
  pj_officer_level_balanced %>%
  left_join(assignment %>%
              select(uid,type,unit),
            by = "uid") %>%
  filter(!is.na(type) & type != "special") %>%
  filter(first_trained > 14)

#---------------------------------------------------------------------------------------------------------------
# Functions to compute the estimates

compute_efficient_estimator_for_outcome <-
  function(outcome, df,...){
    #df <- simulate_data_fn(seed)
    df <- df %>% rename(t = period, y = !!outcome, g = first_trained, i = uid)
    
    simple_results <- staggered(df = df,
                                estimand = "simple",
                                num_fisher_permutations = num_fisher_permutations,
                                ...) %>% 
      mutate(estimand = "simple")
    calendar_results <- staggered(df = df,
                                  estimand = "calendar",
                                  num_fisher_permutations = num_fisher_permutations,
                                  ...) %>% 
      mutate(estimand = "calendar")
    cohort_results <- staggered(df = df,
                                estimand = "cohort",
                                num_fisher_permutations = num_fisher_permutations,
                                ...) %>% 
      mutate(estimand = "cohort")
    eventtime_results <- staggered(df = df,
                                   estimand = "eventstudy",
                                   eventTime = 0,
                                   num_fisher_permutations = num_fisher_permutations,
                                   ...) %>% 
      mutate(estimand = "ES0")
    results <- bind_rows(simple_results, 
                         calendar_results,
                         cohort_results,
                         eventtime_results) %>% 
      mutate(outcome = outcome)
    
    return(results)
  }

compute_CS_estimator_for_outcome <- 
  function(outcome, df,...){
    #df <- simulate_data_fn(seed)
    df <- df %>% rename(t = period, y = !!outcome, g = first_trained, i = uid)
    
    simple_results <- staggered(df = df, estimand = "simple", beta =1, use_DiD_A0=TRUE,
                                num_fisher_permutations = num_fisher_permutations,...) %>% 
      mutate(estimand = "simple")
    calendar_results <- staggered(df = df, estimand = "calendar",
                                  beta =1,use_DiD_A0=TRUE,
                                  num_fisher_permutations = num_fisher_permutations,...) %>% 
      mutate(estimand = "calendar")
    cohort_results <- staggered(df = df, estimand = "cohort",
                                beta =1,use_DiD_A0=TRUE,
                                num_fisher_permutations = num_fisher_permutations,...) %>% 
      mutate(estimand = "cohort")
    eventtime_results <- staggered(df = df, estimand = "eventstudy",
                                   beta =1,eventTime = 0,use_DiD_A0=TRUE,
                                   num_fisher_permutations = num_fisher_permutations,...) %>% 
      mutate(estimand = "ES0")
    results <- bind_rows(simple_results, calendar_results, cohort_results, eventtime_results) %>% 
      mutate(outcome = outcome)
    
    return(results)
  }
#---------------------------------------------------------------------------------------------------------------
## Summary of the results ----
complaints_results <- compute_efficient_estimator_for_outcome(outcome = "complaints",
                                                              pj_officer_level_balanced_nospecial_nopilot, 
                                                              compute_fisher = TRUE)

sustained_results <- compute_efficient_estimator_for_outcome(outcome = "sustained",
                                                             pj_officer_level_balanced_nospecial_nopilot,
                                                             compute_fisher = TRUE)

force_results <- compute_efficient_estimator_for_outcome(outcome = "force", 
                                                         pj_officer_level_balanced_nospecial_nopilot,
                                                         compute_fisher = TRUE)

efficient_results <- bind_rows(complaints_results,
                               sustained_results,
                               force_results) %>% 
  mutate(estimator = "efficient")

complaints_resultsCS <- compute_CS_estimator_for_outcome(outcome = "complaints",
                                                         pj_officer_level_balanced_nospecial_nopilot, 
                                                         compute_fisher = TRUE)

sustained_resultsCS <- compute_CS_estimator_for_outcome(outcome = "sustained", 
                                                        pj_officer_level_balanced_nospecial_nopilot, 
                                                        compute_fisher = TRUE)

force_resultsCS <- compute_CS_estimator_for_outcome(outcome = "force",
                                                    pj_officer_level_balanced_nospecial_nopilot, 
                                                    compute_fisher = TRUE)

CS_results <- bind_rows(complaints_resultsCS,
                        sustained_resultsCS, 
                        force_resultsCS) %>% 
  mutate(estimator = "CS")

# Save
saveRDS(efficient_results,
        here("Temp/wood-et-al-application/efficient_results-nospecial_nopilot.rds"))
saveRDS(CS_results,
        here("Temp/wood-et-al-application/CS_results-nospecial_nopilot.rds"))
#---------------------------------------------------------------------------------------------------------------
# 
# efficient_results <- readRDS(here("Temp/wood-et-al-application/efficient_results-nospecial_nopilot.rds"))
# CS_results <- readRDS(here("Temp/wood-et-al-application/CS_results-nospecial_nopilot.rds"))
#---------------------------------------------------------------------------------------------------------------
# Export the tables - Figure 1 of the paper
long_table <- bind_rows(efficient_results,
                        CS_results)

wide_table <- long_table %>%
  tidyr::pivot_wider(id_cols = c(estimand,outcome), 
                     names_from = estimator,
                     values_from = c(estimate,se, fisher_pval))

cs_ratio_comparison <- wide_table %>%
  filter(estimand != "ES0") %>%
  mutate(se_ratio = se_CS / se_efficient, 
         t_stat_efficient = estimate_efficient/se_efficient, 
         t_stat_CS = estimate_CS/se_CS) %>%
  select(estimand,outcome,se_ratio)

cs_ratio_comparison

# Figure 1 of the paper
Fig1 <- long_table %>% 
  filter(estimand != "ES0") %>% #remove ES0 since we will have the event-study plot
  mutate(estimator = ifelse(estimator == "efficient", "PlugIn",estimator)) %>%
  mutate(ymin = estimate + 1.96*se,
         ymax = estimate - 1.96*se)%>% 
  ggplot(aes(x=estimand, y = estimate, 
             ymin = ymin, ymax = ymax, 
             color = estimator, shape = estimator)) +
  geom_pointrange(position =  position_dodge(width = 0.3), linewidth =.3) +
  facet_wrap(~outcome) + 
  fte_theme() +
  xlab("Estimand") + ylab("Estimate") +
  scale_color_brewer(name = "Estimator", palette = "Set1") +
  scale_shape_discrete(name = "Estimator") +
  geom_hline(yintercept = 0)

ggsave(here("Figures/Wood-et-al-application/summary-estimand-comparsion-nospecial_nopilot.png"),
       plot = Fig1,
       width = 8, height =4)
#---------------------------------------------------------------------------------------------------------------
## Efficient Event-study including pre-periods - Appendix Figure 1

force_ES_results <- compute_efficient_eventstudy_for_outcome(outcome = "force", 
                                                             df=pj_officer_level_balanced_nospecial_nopilot,
                                                             firstTime = -12,
                                                             lastTime = 23)

complaints_ES_results <- compute_efficient_eventstudy_for_outcome(outcome = "complaints", 
                                                                  df=pj_officer_level_balanced_nospecial_nopilot, 
                                                                  firstTime = -12, 
                                                                  lastTime = 23)
sustained_ES_results <- compute_efficient_eventstudy_for_outcome(outcome = "sustained",
                                                                 df=pj_officer_level_balanced_nospecial_nopilot, 
                                                                 firstTime = -12,
                                                                 lastTime = 23)

efficient_ES_results <- bind_rows(complaints_ES_results, 
                                  sustained_ES_results, 
                                  force_ES_results) %>% 
  mutate(estimator = "efficient")

# Appendix Figure 1
efficient_ES_results %>% 
  # mutate(ymin_bonf = estimate + qnorm(1-0.05/2/length(unique(force_ES_results$eventTime)))*se,
  #        ymax_bonf = estimate - qnorm(1-0.05/2/length(unique(force_ES_results$eventTime)))*se)%>%
  mutate(ymin_supt = estimate + supT*se,
         ymax_supt = estimate - supT*se)%>%
  mutate(ymin_ptwise = estimate + 1.96*se,
         ymax_ptwise = estimate - 1.96*se)%>%
  ggplot(aes(x=eventTime, y =estimate)) +
  geom_point() + 
  #  geom_pointrange(aes(ymin = ymin_bonf, ymax = ymax_bonf, color = "Simultaneous")) +
  geom_pointrange(aes(ymin = ymin_supt, ymax = ymax_supt, color = "Simultaneous")) +
  geom_pointrange(aes(ymin = ymin_ptwise, ymax = ymax_ptwise, color = "Pointwise")) +
  geom_hline(yintercept =0) +
  fte_theme() + xlab("Event Time") + ylab("Estimate") +
  scale_color_manual(name = "Confidence Bands", values = RColorBrewer::brewer.pal(n=8,"Blues")[c(8,6)]) +
  facet_wrap(~outcome)

ggsave(here("Figures/Wood-et-al-application/efficient-event-study-plot-nospecial_nopilot-with-pretrends.png"),
       width = 8, height =4)
#---------------------------------------------------------------------------------------------------------------
# Event Study for CS. - Appendix Figure 2
force_ES_resultsCS <- compute_efficient_eventstudy_for_outcome(outcome = "force", 
                                                               df=pj_officer_level_balanced_nospecial_nopilot, 
                                                               firstTime = -12,
                                                               lastTime = 23,
                                                               beta =1, 
                                                               use_DiD_A0=TRUE)
complaints_ES_resultsCS <- compute_efficient_eventstudy_for_outcome(outcome = "complaints", 
                                                                    df=pj_officer_level_balanced_nospecial_nopilot,
                                                                    firstTime = -12, 
                                                                    lastTime = 23, 
                                                                    beta =1, 
                                                                    use_DiD_A0=TRUE)
sustained_ES_resultsCS <- compute_efficient_eventstudy_for_outcome(outcome = "sustained", 
                                                                   df=pj_officer_level_balanced_nospecial_nopilot,
                                                                   firstTime = -12, 
                                                                   lastTime = 23, 
                                                                   beta =1, 
                                                                   use_DiD_A0=TRUE)

efficient_ES_resultsCS <- bind_rows(complaints_ES_resultsCS,
                                    sustained_ES_resultsCS, 
                                    force_ES_resultsCS) %>% 
  mutate(estimator = "efficient")


# Appendix Figure 2
efficient_ES_resultsCS %>% 
  # mutate(ymin_bonf = estimate + qnorm(1-0.05/2/length(unique(force_ES_results$eventTime)))*se,
  #        ymax_bonf = estimate - qnorm(1-0.05/2/length(unique(force_ES_results$eventTime)))*se)%>%
  mutate(ymin_supt = estimate + supT*se,
         ymax_supt = estimate - supT*se)%>%
  mutate(ymin_ptwise = estimate + 1.96*se,
         ymax_ptwise = estimate - 1.96*se)%>%
  ggplot(aes(x=eventTime, y =estimate)) +
  geom_point() + 
  #  geom_pointrange(aes(ymin = ymin_bonf, ymax = ymax_bonf, color = "Simultaneous")) +
  geom_pointrange(aes(ymin = ymin_supt, ymax = ymax_supt, color = "Simultaneous")) +
  geom_pointrange(aes(ymin = ymin_ptwise, ymax = ymax_ptwise, color = "Pointwise")) +
  geom_hline(yintercept =0) +
  fte_theme() + xlab("Event Time") + ylab("Estimate") +
  scale_color_manual(name = "Confidence Bands", values = RColorBrewer::brewer.pal(n=8,"Blues")[c(8,6)]) +
  facet_wrap(~outcome)

ggsave(here("Figures/Wood-et-al-application/CS2-event-study-plot-nospecial_nopilot-with-pretrends.png"),
       width = 8, height =4)
#---------------------------------------------------------------------------------------------------------------

## Compare to pre-treatment means ---- Table 5 of the paper
preTreatmentMeanTable <-
  pj_officer_level_balanced_nospecial_nopilot %>% 
  filter(period <= 12) %>%
  group_by() %>%
  summarise_at(c("complaints", "sustained", "force"),mean) %>%
  reshape2::melt(value.name = "pretreatmentMean")

percentage_effects_table <-
  left_join(long_table,preTreatmentMeanTable, by = c("outcome" = "variable") ) %>%
  mutate(yub = estimate + 1.96*se, ylb = estimate-1.96*se)%>%
  mutate(estimate_over_pretreatment_mean = estimate/pretreatmentMean,
         yub_over_pretreatment_mean = yub/pretreatmentMean,
         ylb_over_pretreatment_mean = ylb/pretreatmentMean) %>%
  select(estimand,outcome,estimator, pretreatmentMean, contains("pretreatment_mean"), fisher_pval)

percentage_effects_table %>% filter(estimand == "simple") %>% 
  select(estimand,outcome,estimator, contains("lb"), contains("estimate"), contains("ub"))

#Table 5 of the paper
percentage_effects_table %>% 
  select(outcome,estimand,estimator, pretreatmentMean, contains("estimate"), 
         contains("lb"), contains("ub"), fisher_pval) %>%
  filter(estimand != "ES0") %>%
  tidyr::pivot_wider(id_cols = c(estimand,outcome, pretreatmentMean), 
                     names_from = estimator, 
                     values_from = -c(estimand,outcome,pretreatmentMean,estimator)) %>%
  select(outcome, estimand, pretreatmentMean, contains("efficient"), contains("CS")) %>% #order columns
  #mutate_at(vars(contains("_mean")), ~100*.) %>% #multiply by 100 to get %s
  mutate(CI_ratio =  (yub_over_pretreatment_mean_CS - ylb_over_pretreatment_mean_CS )/ 
           (yub_over_pretreatment_mean_efficient - ylb_over_pretreatment_mean_efficient ) ) %>%
  gt() %>%
  fmt_percent(contains("_mean"),decimals = 0) %>%
  fmt_number("pretreatmentMean", decimals = 3) %>%
  fmt_number(contains("fisher"), decimals = 3) %>%
  fmt_number("CI_ratio", decimals = 1) %>%
  tab_spanner(label = "Plug-In", contains("efficient")) %>%
  tab_spanner(label = "CS", contains("CS")) %>%
  cols_label(outcome = "Outcome", estimand = "Estimand",
             pretreatmentMean = "Pre-treat Mean",
             estimate_over_pretreatment_mean_efficient = "Estimate",
             ylb_over_pretreatment_mean_efficient = "LB",
             yub_over_pretreatment_mean_efficient = "UB",
             estimate_over_pretreatment_mean_CS = "Estimate",
             ylb_over_pretreatment_mean_CS = "LB",
             yub_over_pretreatment_mean_CS = "UB",
             CI_ratio = "CI Ratio",
             fisher_pval_efficient = "p-val (FRT)",
             fisher_pval_CS = "p-val (FRT)") %>%
  gtsave(here("Tables/wood-et-al-application-percentage-effects-nospecial_nopilot.png"))
#---------------------------------------------------------------------------------------------------------------
## Robustness check to omitting later cohorts ---- Appendix Figure 4
#---------------------------------------------------------------------------------------------------------------
pj_officer_level_balanced_nospecial_nopilot_omit_late_g <- pj_officer_level_balanced_nospecial_nopilot %>%
  filter(first_trained<=60)

complaints_results_omit_late_g <- compute_efficient_estimator_for_outcome(outcome = "complaints", 
                                                                          pj_officer_level_balanced_nospecial_nopilot_omit_late_g , 
                                                                          compute_fisher = TRUE)
sustained_results_omit_late_g <- compute_efficient_estimator_for_outcome(outcome = "sustained",
                                                                         pj_officer_level_balanced_nospecial_nopilot_omit_late_g,
                                                                         compute_fisher = TRUE)
force_results_omit_late_g <- compute_efficient_estimator_for_outcome(outcome = "force",
                                                                     pj_officer_level_balanced_nospecial_nopilot_omit_late_g,
                                                                     compute_fisher = TRUE)
efficient_results_omit_late_g <- bind_rows(complaints_results_omit_late_g,
                                           sustained_results_omit_late_g, 
                                           force_results_omit_late_g) %>% 
  mutate(estimator = "efficient")

complaints_results_omit_late_gCS <- compute_CS_estimator_for_outcome(outcome = "complaints",
                                                                     pj_officer_level_balanced_nospecial_nopilot,
                                                                     compute_fisher = TRUE)
sustained_results_omit_late_gCS <- compute_CS_estimator_for_outcome(outcome = "sustained", 
                                                                    pj_officer_level_balanced_nospecial_nopilot,
                                                                    compute_fisher = TRUE)
force_results_omit_late_gCS <- compute_CS_estimator_for_outcome(outcome = "force",
                                                                pj_officer_level_balanced_nospecial_nopilot, 
                                                                compute_fisher = TRUE)
CS_results_omit_late_g <- bind_rows(complaints_results_omit_late_gCS,
                                    sustained_results_omit_late_gCS, 
                                    force_results_omit_late_gCS) %>% 
  mutate(estimator = "CS")

long_table_omit_late_g <- bind_rows(efficient_results_omit_late_g,
                                    CS_results_omit_late_g)
# Appendix Figure 4
long_table_omit_late_g %>% 
  filter(estimand != "ES0") %>% #remove ES0 since we will have the event-study plot
  mutate(estimator = ifelse(estimator == "efficient", "PlugIn",estimator)) %>%
  mutate(ymin = estimate + 1.96*se,
         ymax = estimate - 1.96*se)%>% 
  ggplot(aes(x=estimand, y = estimate, ymin = ymin, ymax = ymax, color = estimator, shape = estimator)) +
  geom_pointrange(position =  position_dodge(width = 0.3), size =.3) +
  facet_wrap(~outcome) + 
  fte_theme() +
  xlab("Estimand") + ylab("Estimate") +
  scale_color_brewer(name = "Estimator", palette = "Set1") +
  scale_shape_discrete(name = "Estimator") +
  geom_hline(yintercept = 0)

ggsave(here("Figures/Wood-et-al-application/summary-estimand-comparsion-omit-late-g-nospecial_nopilot.png"),
       width = 8, height =4)
#---------------------------------------------------------------------------------------------------------------

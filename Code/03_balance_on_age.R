# This script replicate the Empirical Application in Roth and Sant'Anna (2023)
# It reproduces Appendix Figure 3
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
library(suptCriticalValue)
library(fixest)
library(webshot2)
#---------------------------------------------------------------------------------------------------------------
source(here("Code/aux_fte_theme.R"))
num_fisher_permutations = 5000
set.seed(20211124)
#---------------------------------------------------------------------------------------------------------------

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

check_age_balance <- function(pj_officer_level_balanced_nospecial_nopilot){
  
  
  #Linear regression of birth_year on first_trained
  reg_linear <- pj_officer_level_balanced_nospecial_nopilot %>% 
    filter(period == 1) %>% 
    fixest::feols(fml = birth_year ~ first_trained, 
                  vcov = "hetero",
                  data = .)
  
  fstat_linear <- fitstat(reg_linear, type = "f", vcov = "hetero")$f$stat
  
  #Regression of birthyear on first_trained dummies
  reg_dummies <- pj_officer_level_balanced_nospecial_nopilot %>% 
    filter(period == 1) %>% 
    fixest::feols(fml = birth_year ~ factor(first_trained), 
                  vcov = "hetero",
                  data = ., ) 
  
  reg_dummies_summary <- reg_dummies %>% summary()
  max_t_dummies <- max(abs(reg_dummies_summary$coeftable[-1, "t value"]))
  
  fstat_dummies <- fitstat(reg_dummies, type = "f")$f$stat
  
  return(data.frame(fstat_linear = fstat_linear,
                    fstat_dummies = fstat_dummies,
                    max_t_dummies = max_t_dummies))
  
}
#---------------------------------------------------------------------------------------------------------------

permuteTreatment <- function(df,i_g_table, seed){
  #This function takes a data.frame with columns i and g, and permutes the values of g assigned to i
  # The input i_g_table has the unique combinations of (i,g) in df, and is calculated outside for speed improvements
  
  #Draw a random permutation of the elements of first_period_df
  set.seed(seed)
  n = NROW(i_g_table)
  randIndex <-
    sample.int(n = n,
               size = n,
               replace = FALSE)
  
  #Replace first_period_df$g with a permuted version based on randIndex
  i_g_table$g <- i_g_table$g[randIndex]
  
  #Merge the new treatment assignments back with the original
  df$g <- NULL
  df <- dplyr::left_join(df,
                         i_g_table,
                         by = c("i"))
  
  return(df)
}

#---------------------------------------------------------------------------------------------------------------
i_g_table <- pj_officer_level_balanced_nospecial_nopilot %>%
  filter(period == 1) %>%
  rename(g = first_trained, i = uid) %>%
  select(i,g)

permuteOfficerTreatment <- function(seed){
  permuteTreatment(df = pj_officer_level_balanced_nospecial_nopilot %>%
                     rename(g = first_trained, i = uid),
                   i_g_table = i_g_table, 
                   seed = seed) %>%
    rename(first_trained = g, uid = i)
}

frt_results_age <- purrr::map_df(.x = 1:num_fisher_permutations, 
                                 .f = ~check_age_balance(permuteOfficerTreatment(seed = .x)))

actual_results_age <- check_age_balance(pj_officer_level_balanced_nospecial_nopilot)

pvals <-
  colMeans(
    frt_results_age >= 
      pracma::repmat(as.matrix(actual_results_age), n = NROW(frt_results_age), m = 1) 
  )
pvals
# #Regression using actual data
# reg_linear <-  pj_officer_level_balanced_nospecial_nopilot %>% 
#   filter(period == 1) %>% 
#   fixest::feols(fml = birth_year ~ first_trained,
#                 vcov = "hetero",
#                 data = .)
# reg_linear
#---------------------------------------------------------------------------------------------------------------
# This is Appendix Figure 3
pj_officer_level_balanced_nospecial_nopilot %>% 
  filter(period == 1) %>%
  ggplot(aes(x = first_trained, y = birth_year)) +
  geom_point(alpha = 0.8) + 
  stat_summary_bin(fun='mean', bins=20,
                   color='orange', size=2, geom='point', shape ="triangle") +
  xlab("Training Month") +
  ylab("Year of Birth") +
  fte_theme()

ggsave(here("Figures/Wood-et-al-application/age-balance-scatter.png"),
         width =8, height = 4)
#---------------------------------------------------------------------------------------------------------------
save.image(here("Temp/wood-et-al-application/balance_on_age.RData"))

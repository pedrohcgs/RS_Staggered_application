# This script replicates the Empirical Application in Roth and Sant'Anna (2023)
# It revisit the the Wood et al. application
# This script run all the analyzes
#---------------------------------------------------------------------------------------------------------------
library(here)
remotes::install_github("jonathandroth/staggered@59565db")
#---------------------------------------------------------------------------------------------------------------
source(here::here("Code/01_main_wood-et-al-no-special_ops-no-pilot.R"))
source(here::here("Code/02_balance_checks_pre_tests.R"))
source(here::here("Code/03_balance_on_age.R"))

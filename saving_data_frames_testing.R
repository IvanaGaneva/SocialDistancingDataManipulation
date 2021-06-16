# Saving all data frames:

# Types of policies:
# 1) Binary (bin)
# 2) SchoolClose (cat_sch) -> DONE
# 3) Businesses Close (cat_bus)
# 4) Number (numb)
# 5) Mandatory/Recommended (cat_mand)

fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
# Removing the fileloc string
rm(fileloc)
# Setting locale
Sys.setlocale("LC_ALL", "English")
# --------------------------------------------------------------------------
# 0.2. Loading the necessary packages
library(tidyverse)                                       # for handling data
library(data.table)                                      # for handling data
library(rvest)                                           # for scraping data
# --------------------------------------------------------------------------
# 0.3. Clearing the workspace environment
rm(list = ls())
gc()


load('Environment_until_EELJ_2021-06-16.RData')

source('PolicyType.R')

source('Simple_EDA.R')
df_EDA <- simple_EDA_fun(main_df = COVID_measures_df_REVIEWED)

source('EmergDec.R')

source('aux_fun_chains_START_fill.R')
source('FILL_function.R')

county_data <- counties_df %>%
  group_by(State) %>%
  mutate(StatePopulation2019 = sum(Population2019)) %>%
  ungroup()


# --------------------------------------------------------------------------
# Binary variables, in the lines of EmergDec:

# ==========================================================================================
# CaseIsolation:

# i. county level
df_CaseIsolation <- FILL_function(data_measures = COVID_measures_df_REVIEWED,
                                 county_data = counties_df,
                                 state_name = 'Alabama',
                                 policy_measure = 'CaseIsolation',
                                 not_vec = c(0, NA))

for(i in 2:length(all_states_considered)){
  df_CaseIsolation <- bind_rows(df_CaseIsolation,
                              FILL_function(data_measures = COVID_measures_df_REVIEWED,
                                            county_data = counties_df,
                                            state_name = all_states_considered[i],
                                            policy_measure = 'CaseIsolation',
                                            not_vec = c(0, NA)))
}

# filtering/changing for mandate:
df_CaseIsolation <- df_CaseIsolation %>%
  mutate(policy_measure_var_main = ifelse(is.na(mandate) | mandate == 0,
                                          policy_type_function('CaseIsolation')[2],
                                          policy_measure_var_main)) %>%
  # NA-s means these dates occur after the last policy ended => replace with usual value
  # non-mandatory in THIS data set also translates to non-existent policy
  dplyr::select(1:4) %>%
  # selecting the relevant variables only
  rename(CaseIsolation = policy_measure_var_main)

# # saving the EmergeDec at the county lvl:
# save(df_CaseIsolation, file = 'CaseIsolation_COUNTY_lvl.RData')


# ii. state lvl
df_CaseIsolation_STATE <- df_CaseIsolation %>%
  # adding the population as of 2019 from the county df:
  left_join(county_data, by = c('State', 'County')) %>%
  # obtaining for what percentage of the population within the whole state
  # measures for public schools applied:
  mutate(county_pop_frac_of_state_pop = Population2019/StatePopulation2019)

# group by date and state now
df_CaseIsolation_STATE <- df_CaseIsolation_STATE %>%
  mutate(CaseIsolation = as.numeric(CaseIsolation)) %>%
  mutate(CaseIsolation_frac_pop = CaseIsolation*county_pop_frac_of_state_pop) %>%
  group_by(State, Date) %>%
  summarize(frac_state_pop_with_CaseIsolation = sum(CaseIsolation_frac_pop))

# save(df_CaseIsolation_STATE, file = 'CaseIsolation_STATE_lvl.RData')


# ====================================================================================
# Function to do the rest of the binary variables:
vec_bin_policies <- c('StayAtHome',
                      'BusinessMask', 'SchoolMask',
                      'Quarantine', 
                      'TravelRestrictExit', 'TravelRestrictIntra', 'TravelRestrictEntry')

make_bin_df_county_lvl <- function(policy){
  df_temp <- FILL_function(data_measures = COVID_measures_df_REVIEWED,
                                    county_data = counties_df,
                                    state_name = 'Alabama',
                                    policy_measure = policy,
                                    not_vec = c(0, NA))
  
  for(i in 2:length(all_states_considered)){
    df_temp <- bind_rows(df_temp,
                                  FILL_function(data_measures = COVID_measures_df_REVIEWED,
                                                county_data = counties_df,
                                                state_name = all_states_considered[i],
                                                policy_measure = policy,
                                                not_vec = c(0, NA)))
  }
  
  # filtering/changing for mandate:
  df_temp <- df_temp %>%
    mutate(policy_measure_var_main = ifelse(is.na(mandate) | mandate == 0,
                                            policy_type_function(policy)[2],
                                            policy_measure_var_main)) %>%
    # NA-s means these dates occur after the last policy ended => replace with usual value
    # non-mandatory in THIS data set also translates to non-existent policy
    dplyr::select(1:4)
    # selecting the relevant variables only
  
  colnames(df_temp)[4] <- policy
  
  save(df_temp, file = paste0(policy, '_COUNTY_lvl.RData'))
  
  return(df_temp)
}

# test_StayAtHome <- make_bin_df_county_lvl('StayAtHome') -> OK
# test_BusinessMask <- make_bin_df_county_lvl('BusinessMask') -> OK
# test_SchoolMask <- make_bin_df_county_lvl('SchoolMask') -> OK


list_bin_policies <- list()

for(j in 1:length(vec_bin_policies)){
  list_bin_policies[[j]] <- make_bin_df_county_lvl(vec_bin_policies[j])
}

names(list_bin_policies) <- vec_bin_policies

# --------------------------------------------------------------------------
# Now, a function to transform the county-level data to state-level data:
make_bin_df_state_lvl <- function(policy){
  df_temp <- list_bin_policies[[policy]] %>%
    # adding the population as of 2019 from the county df:
    left_join(county_data, by = c('State', 'County')) %>%
    # obtaining for what percentage of the population within the whole state
    # measures for public schools applied:
    mutate(county_pop_frac_of_state_pop = Population2019/StatePopulation2019)  
  
  colnames(df_temp)[4] <- 'POLICYNAMEWILLBEHERE'
  
  # grouping by date and state now:
  df_temp <- df_temp %>%
    mutate(POLICYNAMEWILLBEHERE = as.numeric(POLICYNAMEWILLBEHERE)) %>%
    mutate(POLICYNAMEWILLBEHERE_frac_pop = POLICYNAMEWILLBEHERE*county_pop_frac_of_state_pop) %>%
    group_by(State, Date) %>%
    summarize(fract_state_pop_with_POLICYNAMEWILLBEHERE = sum(POLICYNAMEWILLBEHERE_frac_pop))
  
  colnames(df_temp)[3] <- paste0('frac_state_pop_with_', policy)

  save(df_temp, file = paste0(policy, '_STATE_lvl.RData'))
  
  return(df_temp)
}

list_bin_policies_STATE <- list()

for(j in 1:length(vec_bin_policies)){
  list_bin_policies_STATE[[j]] <- make_bin_df_state_lvl(vec_bin_policies[j])
}

names(list_bin_policies_STATE) <- vec_bin_policies
# ==========================================================================

# Now, proceding with the cat_bus variables (4 of them)

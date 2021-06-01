# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ganeva, I.K.
# Covid 19 State Policy Data Cleaning, Exploration & Implementation of 
# Hand-Made Functions for the Construction of a Set of Local Metrics
# Feb-Mar, 2021

# __________________________________________________________________________
# Nancy Fullman, Bree Bang-Jensen, Grace Reinke, Beatrice Magistro,          
# Rachel Castellano, Megan Erickson, Rebecca Walcott, Carolyn Dapper, 
# Kenya Amano, John Wilkerson, and Christopher Adolph. 
# "State-level social distancing policies in response to COVID-19 in the US". 
# Version 1.117, February 17, 2021. http://www.covid19statepolicy.org
# __________________________________________________________________________

# ==========================================================================
# FILE TO TEST THE THINGS DONE SO FAR
# ==========================================================================

# __________________________________________________________________________
# 0. SOME PRELIMINARIES:
# --------------------------------------------------------------------------
# 0.1. Setting the working directory right 
# (automatically, as with the current script's location)
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

# __________________________________________________________________________
# 1. LOADING THE DATA FROM THE SAVED FILES DIRECTLY:
load('Environment_uptil_EELJ_31_May.Rdata')


# load('saved_data_frames.RData')
# load('saved_policy_chains_all_states.RData')

# __________________________________________________________________________
# 2. LOADING THE FUNCTION CODES:

source('EmptyDfForState.R')
# test_empty_df <- function_empty_df_for_state(state_name = 'California', long_output = F)
# makes an empty df for each possible state

source('ENDS_EXTENDS_function.R')
# test_ends_extends <- ENDS_EXTENDS_function(state_name = 'California', policy_measure = 'StayAtHome')
# makes a df for a state and a policy measure, having simplified the ending/extending rows

source('EELJ_function.R')
# test_eelj <- EELJ_function(state_name = 'California', policy_measure = 'SchoolClose')
# looks for where the policy changes are and which category these changes fall into

source('PolicyType.R')

source('EmergDec.R')
# obtains the mandatory EmergDec dummy variable value
# test_EmergDec <- EmergDec_function_for_state(state_name = 'Alabama')

source('Simple_EDA.R')
# test_EDA <- simple_EDA_fun(main_df = COVID_measures_df_REVIEWED)


# __________________________________________________________________________
# 3. TESTING THE FILL FUNCTION:

source('aux_fun_chains_START_fill.R')
source('FILL_function.R')


# testing the FILL_function:

# function(data_measures = COVID_measures_df_REVIEWED,
#          county_data = counties_df,
#          state_name,
#          policy_measure,
#          not_vec = c(0, NA)){
  
  # for construction/testing:
  # -------------------------------------------
  # data_measures <- COVID_measures_df_REVIEWED
  # county_data <- counties_df
  # state_name <- 'Alabama'
  # policy_measure <- 'SchoolClose'
  # not_vec <- c(0, NA)
  # -------------------------------------------

df_SchoolClose_test <- FILL_function(data_measures = COVID_measures_df_REVIEWED,
                                county_data = counties_df,
                                state_name = 'Alabama',
                                policy_measure = 'SchoolClose',
                                not_vec = c(0, NA))


# Saving this df into a separate file:
# save(df_SchoolClose, file = 'saved_SchoolClose_all_states.RData')



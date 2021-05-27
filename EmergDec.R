# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ganeva, I.K.
# Covid 19 State Policy Data Cleaning
# Feb-Mar, 2021

# __________________________________________________________________________
# Nancy Fullman, Bree Bang-Jensen, Grace Reinke, Beatrice Magistro,          
# Rachel Castellano, Megan Erickson, Rebecca Walcott, Carolyn Dapper, 
# Kenya Amano, John Wilkerson, and Christopher Adolph. 
# "State-level social distancing policies in response to COVID-19 in the US". 
# Version 1.117, February 17, 2021. http://www.covid19statepolicy.org
# __________________________________________________________________________

# EmergDec State Policy Measure

# DEFINITION AND POLICY ACTION VARIABLES / CORRESPONDING EXAMPLES:
#           - TAKEN FROM THE DATA DOCUMENTATION PDF -
#                   - AS OF 1 MARCH 2021 -
# Emergency declaration; currently includes State of Emergency, Public Health Emergency, 
# Public Health Disaster declarations, Civil Emergency declarations, and other permutations of 
# state‐level declarations of emergency in response to COVID‐19.
#
# States usually some kind of time limit on states of emergency (e.g., 15 days, 30 days), 
# but others appear to not have any temporal requirements (i.e., these
# emergency declarations are in effect until otherwise rescinded by the governor).
#
# Note that some states may have multiple types of emergency declarations at once 
# (e.g., state of emergency and public health emergency). We have sought to generally capture
# these separately if possible, as well as document the type of emergency declaration issued.

#  Extends: continues the status of a state’s emergency declaration; 
#            this is frequently used in this dataset.
#  Expands: increases the level of emergency declaration (e.g., extreme disaster emergency 
#            declaration in Idaho); note that this is rarely used in this dataset.
#  Eases:   reduces the level of emergency declaration (e.g., scaling down from a super disaster 
#            emergency declaration); note that this is rarely used in this dataset.
#  Ends:    the state of emergency is no longer active, and/or is actively repealed or struck down.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

EmergDec_function_for_state <- function(data_measures = COVID_measures_df_REVIEWED,
                                        state_name){
  
  df_state_var <- function_empty_df_for_state(state_name = state_name) %>%
    # this will be the data frame to fill
    mutate(EmergDec = 0)

  # NOTE: see the main code file for reasoning/proof that this measure is state-wide
  #       and why it will be applied to all counties where seen
  
  beginnings_and_ends <- function_seq_of_policies_for_state(state_name = state_name) %>%
    filter(End_Date_Obtained == 'TRUE', 
           Initial_policy == 'EmergDec')
  
  if(nrow(beginnings_and_ends) != 0){
    dates_active_policy <- setDT(beginnings_and_ends)[, .(DateActive = seq(Start_act, 
                                                                           End_act_or_exp, 
                                                                           by = 'day')),
                                                      by = Chain_start] 
    # these are the dates the policy was active
    df_state_var$EmergDec[df_state_var$Date %in% dates_active_policy$DateActive] <- 1
    # filling state-wide
  }
  
  return(df_state_var)
  # returns a data frame wchich can be transformed to wide format with a single line of code
  # EmergDec takes a value of 1 if the policy was active at the time and 0 otherwise
}
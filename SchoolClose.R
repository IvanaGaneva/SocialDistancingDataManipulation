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

# SchoolClose State Policy Measure

# DEFINITION AND POLICY ACTION VARIABLES / CORRESPONDING EXAMPLES:
#           - TAKEN FROM THE DATA DOCUMENTATION PDF -
#                   - AS OF 1 MARCH 2021 -
# Formal closing of (at minimum) public K‐12 schools. Where possible, additional
# information on types of school closings are provided in "PolicyCodingNotes".
# 
# If states explicitly close K‐12 schools and higher education institutions via
# separate executive orders, we code these as separate policy chains. We do not
# include childcare or early child learning care services as part of this policy type.
#
# SchoolRestrictLevel is used to provide broad categories of school closures or
# restrictions.

#  Extends: continues the status of the previous school closure or restriction level.
#  Expands: applies to more types of schools (private and charter schools, higher education) 
#            than the previous school closure or restriction, and/or reflects an increased level 
#            of restriction or closures for schools than the previous entry. This can also reflect a
#            shift from recommending that schools close (and thus allowing school districts and/or 
#            local authorities to determine school status) to requiring that schools close.
#  Eases:   reflects a decreasing level of restriction for schools compared to its previous level, 
#            and/or applies to fewer school types than the previous entry (e.g., allows grades 6‐12 
#            to resume in‐person instruction while K‐5 remain fully remote).
#  Ends:    state‐level executive or agency devolves school re‐opening responsibilities to districts
#            and/or local authorities; governor issues an order requiring schools to provide in‐person 
#            learning.
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

SchoolClose_function_for_state <- function(data_measures = COVID_measures_df_REVIEWED,
                                           state_name){
  # SchoolRestrictLevel:
  # NoInPerson > LimitedInPerson > InPersonAllowed

  policy_state_changes <- EELJ_function(data_measures = data_measures,
                                        state_name = state_name,
                                        policy_measure = policy_measure)
  policy_state_simplified <- ENDS_EXTENDS_function(data_measures = data_measures,
                                                   state_name = state_name,
                                                   policy_measure = policy_measure)
  
  
  
  # PUBLIC AND PRIVATE SCHOOLS:
  df_state_var <- function_empty_df_for_state(state_name = state_name) %>%
    # this will be the data frame to fill
    mutate(PublicSchoolClose = 'InPersonAllowed',
           PrivateSchoolClose = 'InPersonAllowed')
    # default value over time is that students are allowed to attend classes in person
  
  changes_policies <- EELJ_function(data_measures = data_measures,
                                    state_name = state_name,
                                    policy_measure = 'SchoolClose')
  
  state_measures <- ENDS_EXTENDS_function(data_measures = data_measures,
                                          state_name = state_name,
                                          policy_measure = 'SchoolClose')
  
  # First important scenario to note:
  # -> if the only change in a policy with easing/expansion was in the location:
  # (i.e. same level of restrictions in school applied, same mandate, etc.)
  
  state_measures <- data_measures %>%
    filter(StateName == state_name,
           StatePolicy == 'SchoolClose')
  
  state_measures <- data_measures %>%
    filter(StateName == state_name,
           StatePolicy == 'SchoolClose',
           Mandate == 1) %>%
    select(1, StateWide, SWGeo, SWPop,
           21:32, SchoolRestrictLevel) %>%
    mutate(SchoolRestrictLevel = as.factor(SchoolRestrictLevel))
    # these are the measures for the respective state and the respective dates of enactions/expiry
  colnames(state_measures)[1] <- 'PID'
    # renaming the first col for simpler use in the future
  
  policy_chains <- function_seq_of_policies_for_state(state_name = state_name) %>%
    filter(End_Date_Obtained == 'TRUE', Initial_policy == 'SchoolClose')
  
  # A brief manipulation for the Ends variable:
  for(r in 1:nrow(state_measures)){
    if(is.na(state_measures$DateExpiry[r]) & is.na(state_measures$DateEnded[r])){
      if(state_measures$PID[r] %in% state_measures$Ends){
        ending_policy_found <- which(state_measures$Ends == state_measures$PID[r])
        # changing the end date if it is not present and we can infer it from an ending policy measure
        state_measures$DateEnded[r] <- state_measures$DateEnacted[ending_policy_found]
      } else{
        state_measures$DateEnded[r] <- lubridate::ymd('2100-12-31')
      }
    }
  }
  state_measures <- state_measures %>%
    filter(is.na(Ends)) %>%
    # do not need this variable anymore
    mutate(begins = DateEnacted,
           finishes = fifelse(is.na(DateEnded), DateExpiry, DateEnded))
  
  
  
  dates_active_policy <- setDT(state_measures)[, .(DateActive = seq(DateEnacted, 
                                                                    DateExpiry, 
                                                                    by = 'day')),
                                               by = PID] 
    # these are the dates the policy was active
  
  df_state_var$EmergDec[df_state_var$Date %in% dates_active_policy$DateActive] <- 1
  #  filling state-wide
  
  return(df_state_var)
    # returns a data frame wchich can be transformed to wide format with a single line of code
    # EmergDec takes a value of 1 if the policy was active at the time and 0 otherwise
}
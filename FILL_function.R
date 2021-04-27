# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ganeva, I.K.
# Covid 19 State Policy Data Cleaning
# Feb-Apr, 2021

# __________________________________________________________________________
# Nancy Fullman, Bree Bang-Jensen, Grace Reinke, Beatrice Magistro,          
# Rachel Castellano, Megan Erickson, Rebecca Walcott, Carolyn Dapper, 
# Kenya Amano, John Wilkerson, and Christopher Adolph. 
# "State-level social distancing policies in response to COVID-19 in the US". 
# Version 1.117, February 17, 2021. http://www.covid19statepolicy.org
# __________________________________________________________________________

# FUNCTION THAT FILLS THE EMPTY DATA FRAME FOR A SPECIFIC POLICY MEASURE
# AND A PARTICULAR STATE OF INTEREST

# Crucial Notes:
# Heterogeneity arises on multiple dimensions!
# --- these can be found in the 9:25 columns (totalling 17) in the EELJ
#     large df -> the columns which start with ch_

# --- to this end, introducing auxiliary functions/chunks of code to capture them:
#     o TIME CHANGES [related to the three ch_curfew variables]
#     o LOCATION CHANGES
#     o GROUPS OF POPULATION AFFECTED CHANGES [vaccinated vs. not considered only]
#     o LIMITS OF PPL GATHERINGS
#     o MANDATORY OR NOT


# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# UNDER CONSTRUCTION:

FILL_function <- function(data_measures = COVID_measures_df_REVIEWED,
                          county_data = counties_df,
                          state_name,
                          policy_measure){
  
  # for construction/testing:
  data_measures <- COVID_measures_df_REVIEWED
  county_data <- counties_df
  state_name <- 'Alabama'
  policy_measure <- 'SchoolClose'
  # -------------------------------------------
  
  
  policy_type_usual_value <- policy_type_function(policy_measure)
  policy_type <- policy_type_usual_value[1]
  usual_value <- policy_type_usual_value[2]
  
  # rm(data_measures, county_data, state_name, policy_measure, 
  #    policy_type_usual_value, policy_type, usual_value)
  # gc()
  
  if(policy_type == 'undefined'){
    return('Not a valid policy type! Please, enter a valid one.')
  } else{
    df_to_fill <- function_empty_df_for_state(data_counties = county_data,
                                              data_measures = data_measures,
                                              state_name = state_name,
                                              long_output = T) %>%
      mutate(policy_measure_var_main = usual_value,
             policy_measure_var_sec = ifelse(policy_type == 'cat_sch',
                                             usual_value,
                                             NA)) %>%
      # filling the policy_measure_var with the 'usual' no-restrictions value
      # according to the policy_measure_type
      
      # first and second policy measure variables are needed for StatePolicy == 'SchoolClose'
      # where the policy measure carries (implicit) information about the closure of Public (main)
      # and Private schools (secondary variable)
      mutate(perc_usual_time = NA,
             only_non_vaccinated_ppl = NA,
             lim_in_general = NA, lim_out_general = NA,
             lim_in_rel = NA, lim_out_rel = NA,
             mandate = NA)
    # note that the location variable does not require a separate column/variable, but will be 
    # instead captured within the rows of the df_to_fill
    
    policy_state_changes <- EELJ_function(data_measures = data_measures,
                                          state_name = state_name,
                                          policy_measure = policy_measure)
    policy_state_simplified <- ENDS_EXTENDS_function(data_measures = data_measures,
                                                     state_name = state_name,
                                                     policy_measure = policy_measure)
    
    # Filling for the uncoded policy measures for the PublicMask variable:
    if(policy_type == 'cat_mand'){
      policy_state_simplified$PublicMaskLevel[policy_state_simplified$Mandate != 1 & 
                                                is.na(policy_state_simplified$PublicMaskLevel)] <- 'Special_Recommend'
    }
    # i.e. advised to wear masks but not in general settings, but in rather more special occasions
    #      which are uncoded under the cat_mand type of variable
    
    if(nrow(policy_state_simplified) != 0){
      policy_state_simplified <- policy_state_simplified %>%
        arrange(DateIssued)
      # Arranging by date issued.
      
      # ---------------------------- STEP I ----------------------------------
      # Starting from the first observation in the policy_state_simplified df:
      # filling the time/vaccine/lim-s/mandate/location dimensions
      #                                &
      # # ----------------------=--- STEP II ---------------------------------
      # Continuing to work with the first observation in the policies df:
      # depending on the type of policy (5 broad categories of variables)
      # [And doing the same thing for each beginning of a new policy chain.]
      starting_policies <- setdiff(policy_state_simplified$PID, policy_state_changes$PID)
        # will be running the auxiliary function for the beginnings of chains on these policy measures
      
      if(length(starting_policies) == 0){
        return('Check data inputs - no starting policies in this policy chain!')
        # Ideally, if there are no data input mistakes, this should not occur!
      } else{
        for(ps in starting_policies){
          df_to_fill <- aux_fun_chains_START_fill(starting_policy = ps)
          # updating the data frame to fill to match the starting policies
        }

        
        # ---------------------------- STEP III ---------------------------------
        # Continuing to fill for the remaining entries in the policy state df:
        rn_ps_df <- nrow(policy_state_simplified)
        # if(nrow(policy_state_simplified) != 1){
        #   for(r in 2:rn_ps_df){
        #     
        #   }
        # }
        
        
        # return(df_to_fill)
      }

    }
    
    
    
  }
  
  # rm(df_to_fill, policy_state_changes, policy_state_simplified)
  # gc()
  
  
}

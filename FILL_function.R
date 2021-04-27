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
  
  data_measures <- COVID_measures_df_REVIEWED
  county_data <- counties_df
  state_name <- 'Alabama'
  policy_measure <- 'PublicMask'
  
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
    
    if(nrow(policy_state_simplified) != 0){
      # ---------------------------- STEP I ----------------------------------
      # Starting from the first observation in the policy_state_simplified df:
      # filling the time/vaccine/lim-s/mandate/location dimensions
    
      first_policy_dates <- seq(policy_state_simplified$begins[1],
                                policy_state_simplified$finishes[1],
                                by = 'days')
      last_day_first_policy <- policy_state_simplified$finishes[1]
      first_day_first_policy <- first_policy_dates[1]
      
      df_to_fill$perc_usual_time[df_to_fill$Date < first_day_first_policy] <- 'All the time'
      df_to_fill$only_non_vaccinated_ppl[df_to_fill$Date < first_day_first_policy] <- 0
      df_to_fill[df_to_fill$Date < first_day_first_policy, 8:11] <- 'No limit'
      df_to_fill$mandate[df_to_fill$Date < first_day_first_policy] <- 1
      
      if(policy_state_simplified$SWGeo[1] == 1){
        counties_to_fill_vec <- unique(as.character(df_to_fill$County))
      } else{
        counties_to_fill_vec <- unlist(str_split(policy_state_simplified$AppliesTo[1],
                                                 ', '))
      }
      
      not_vec <- c(NA, 0)
      which_loc_date_vec <- which((df_to_fill$County %in% counties_to_fill_vec) &
                                    (df_to_fill$Date %in% first_policy_dates))
      
      # Adding the time dimension:
      if(policy_state_simplified$Curfew[1] %in% not_vec){
        df_to_fill$perc_usual_time[which_loc_date_vec] <- 'All the time'
      } else{
        df_to_fill$perc_usual_time[which_loc_date_vec] <- paste0('From ',
                                                                 policy_state_simplified$CurfewStart[1],
                                                                 ' to ',
                                                                 policy_state_simplified$CurfewEnd[1])
      }
      
      # Ading the vaccinated people dimension:
      if(policy_state_simplified$VaccineExempt[1] %in% not_vec){
        df_to_fill$only_non_vaccinated_ppl[which_loc_date_vec] <- 0
      } else{
        df_to_fill$only_non_vaccinated_ppl[which_loc_date_vec] <- 1
      }
      
      # Adding the mandatory or not dimension:
      if(policy_state_simplified$Mandate[1] %in% not_vec){
        df_to_fill$mandate[which_loc_date_vec] <- 0
      } else{
        df_to_fill$mandate[which_loc_date_vec] <- 1
      }
      
      # Adding the gathering limits dimension:
      # - for inside general gath
      if(is.na(policy_state_simplified$InGathLim[1])){
        df_to_fill$lim_in_general[which_loc_date_vec] <- 'No limit'
      } else{
        df_to_fill$lim_in_general[which_loc_date_vec] <- policy_state_simplified$InGathLim[1]
      }
      # - for outside general gath
      if(is.na(policy_state_simplified$OutGathLim[1])){
        df_to_fill$lim_out_general[which_loc_date_vec] <- 'No limit'
      } else{
        df_to_fill$lim_out_general[which_loc_date_vec] <- policy_state_simplified$OutGathLim[1]
      }
      # - for inside religious gath
      if(is.na(policy_state_simplified$InGathLimReligious[1])){
        df_to_fill$lim_in_rel[which_loc_date_vec] <- 'No limit'
      } else{
        df_to_fill$lim_in_rel[which_loc_date_vec] <- policy_state_simplified$InGathLimReligious[1]
      }
      # - for outside religious gath
      if(is.na(policy_state_simplified$OutGathLimReligious[1])){
        df_to_fill$lim_out_rel[which_loc_date_vec] <- 'No limit'
      } else{
        df_to_fill$lim_out_rel[which_loc_date_vec] <- policy_state_simplified$OutGathLimReligious[1]
      }
      
      # ---------------------------- STEP II ----------------------------------
      # Continuing to work with the first observation in the policies df:
      # depending on the type of policy and extend/ease/join/leave var-s
      if(policy_type == 'cat_sch'){
        df_to_fill$policy_measure_var_main[which_loc_date_vec] <- policy_state_simplified$SchoolRestrictLevel[1]
        if(str_detect(policy_state_simplified$PolicyCodingNotes[1], 'private')){
          df_to_fill$policy_measure_var_sec[which_loc_date_vec] <- policy_state_simplified$SchoolRestrictLevel[1]
        }
      } else{
        if(policy_type == 'bin'){
          df_to_fill$policy_measure_var_main[which_loc_date_vec] <- 1
          # this is the first policy introduced => cannot be easing of the original values
        } else{
          if(policy_type == 'cat_bus'){
            df_to_fill$policy_measure_var_main[which_loc_date_vec] <- policy_state_simplified$BusinessRestrictLevel[1]
          } else{
            if(policy_type == 'numb'){
              df_to_fill$policy_measure_var_main[which_loc_date_vec] <- 'GathRestrict: see limit var-s'
            } else{
              if(policy_type == 'cat_mand'){
                
                if(is.na(policy_state_simplified$PublicMaskLevel[1])){
                  df_to_fill$policy_measure_var_main[which_loc_date_vec] <- 'Special_Occasions'
                  # i.e. advised to wear masks but not in general settings, but in rather more special occasions
                  #      which are uncoded under the cat_mand type of variable
                } else{
                  df_to_fill$policy_measure_var_main[which_loc_date_vec] <- policy_state_simplified$PublicMaskLevel[1]
                }
              }
            }
          }
        }
      }
      # 
      # # ---------------------------- STEP III ---------------------------------
      # # Continuing to fill for the remaining entries in the policy state df:
      # rn_ps_df <- nrow(policy_state_simplified)
      # if(nrow(policy_state_simplified) != 1){
      #   
      # }
      
    }
    
    
    
  }
  
  # rm(df_to_fill, policy_state_changes, policy_state_simplified)
  # gc()
  
  
}
  
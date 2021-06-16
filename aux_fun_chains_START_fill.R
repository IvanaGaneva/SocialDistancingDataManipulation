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

# FUNCTION THAT GIVES THE df_to_fill VALUES FOR EACH NEW POLICY INTRODUCED
# (AUXILIARY CODE FOR BREVITY)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

aux_fun_chains_START_fill <- function(starting_policy,
                                      p_type = policy_type, 
                                      p_usual_vale = usual_value,
                                      df_changes = policy_state_changes, 
                                      df_simplified = policy_state_simplified,
                                      df_fill = df_to_fill,
                                      not_vec = c(0, NA)){
  # used for testing within the FILL_function:
  # -----------------
  # df_changes <- policy_state_changes
  # df_simplified <- policy_state_simplified
  # starting_policy <- policy_state_simplified$PID[1]
  # p_type <- policy_type
  # p_usual_value <- usual_value
  # df_fill <- df_to_fill
  
  row_s_df <- which(df_simplified$PID == starting_policy)
  
  # --------------------------------- STEP I ---------------------------------------
  # Starting from the first observation in the policy_state_simplified df:
  # filling the time/vaccine/lim-s/mandate/location dimensions
  # [And doing the same thing for each beginning of a new policy chain.]
  
  st_policy_dates <- seq(df_simplified$begins[row_s_df],
                         df_simplified$finishes[row_s_df],
                         by = 'days')
  last_day_st_policy <- df_simplified$finishes[row_s_df]
  first_day_st_policy <- st_policy_dates[1]
  
  if(row_s_df == 1){
    df_fill$perc_usual_time[df_fill$Date < first_day_st_policy] <- 'All the time'
    df_fill$only_non_vaccinated_ppl[df_fill$Date < first_day_st_policy] <- 0
    df_fill[df_fill$Date < first_day_st_policy, 8:11] <- 'No limit'
    df_fill$mandate[df_fill$Date < first_day_st_policy] <- 1
    # Filling for the dates before the first policy was introduced.
  }
  
  # Finding which locations to fill for:
  if(df_simplified$SWGeo[row_s_df] == 1){
    counties_to_fill_vec <- unique(as.character(df_fill$County))
    # i.e. if it is state-wide geographically, fill for all counties in the state
    #      for this policy instance
  } else{
    counties_to_fill_vec <- unlist(str_split(df_simplified$AppliesTo[row_s_df],
                                             ', '))
  }
  
  # Putting the dates-locations combinations to be filled in a single vector:
  which_loc_date_vec <- which((df_fill$County %in% counties_to_fill_vec) &
                                (df_fill$Date %in% st_policy_dates))
  
  # ===============================
  # FILLING FOR THE TIME DIMENSION:
  if(df_simplified$Curfew[row_s_df] %in% not_vec){
    df_fill$perc_usual_time[which_loc_date_vec] <- 'All the time'
  } else{
    df_fill$perc_usual_time[which_loc_date_vec] <- paste0('From ',
                                                          df_simplified$CurfewStart[row_s_df],
                                                          ' to ',
                                                          df_simplified$CurfewEnd[row_s_df])
  }
  
  # =========================================
  # FILLING FOR THE VACCINATED PPL DIMENSION:
  if(df_simplified$VaccineExempt[row_s_df] %in% not_vec){
    df_fill$only_non_vaccinated_ppl[which_loc_date_vec] <- 0
  } else{
    df_fill$only_non_vaccinated_ppl[which_loc_date_vec] <- 1
  }
  
  # ==================================
  # FILLING FOR THE MANDATE DIMENSION:
  if(df_simplified$Mandate[row_s_df] %in% not_vec){
    df_fill$mandate[which_loc_date_vec] <- 0
  } else{
    df_fill$mandate[which_loc_date_vec] <- 1
  }
  
  # =====================================
  # FILLING FOR THE GATHERINGS DIMENSION:
  # -> FOR INSIDE, GENERAL GATHERINGS
  if(is.na(df_simplified$InGathLim[row_s_df])){
    df_fill$lim_in_general[which_loc_date_vec] <- 'No limit'
  } else{
    df_fill$lim_in_general[which_loc_date_vec] <- df_simplified$InGathLim[row_s_df]
  }
  # -> FOR OUTSIDE, GENERAL GATHERINGS
  if(is.na(df_simplified$OutGathLim[row_s_df])){
    df_fill$lim_out_general[which_loc_date_vec] <- 'No limit'
  } else{
    df_fill$lim_out_general[which_loc_date_vec] <- df_simplified$OutGathLim[row_s_df]
  }
  # -> FOR INSIDE, RELIGIOUS GATHERINGS
  if(is.na(df_simplified$InGathLimReligious[row_s_df])){
    df_fill$lim_in_rel[which_loc_date_vec] <- 'No limit'
  } else{
    df_fill$lim_in_rel[which_loc_date_vec] <- df_simplified$InGathLimReligious[row_s_df]
  }
  # -> FOR OUTSIDE, RELIGIOUS GATHERINGS
  if(is.na(df_simplified$OutGathLimReligious[row_s_df])){
    df_fill$lim_out_rel[which_loc_date_vec] <- 'No limit'
  } else{
    df_fill$lim_out_rel[which_loc_date_vec] <- df_simplified$OutGathLimReligious[row_s_df]
  }
  
  # --------------------------------------------------------------------------------
  
  # --------------------------------- STEP II --------------------------------------
  # Continuing to work with the first observation in the policies df:
  # depending on the type of policy (5 broad categories of variables)
  # [And doing the same thing for each beginning of a new policy chain.]
  
  if(p_type == 'cat_sch'){
    # =========== THIS IS FOR THE SchoolClose VARIABLE
    df_fill$policy_measure_var_main[which_loc_date_vec] <- df_simplified$SchoolRestrictLevel[row_s_df]
    if(str_detect(df_simplified$PolicyCodingNotes[row_s_df], 'private|Private')){
      df_fill$policy_measure_var_sec[which_loc_date_vec] <- df_simplified$SchoolRestrictLevel[row_s_df]
    }
  } else{
    if(p_type == 'bin'){
      # ========= THIS IS FOR THE EmergDec, CaseIsolation, StayAtHome, BusinessMask, SchoolMask,
      #                           Quarantine, & the three TravelRestrict VARIABLES
      df_fill$policy_measure_var_main[which_loc_date_vec] <- 1
        # since this is for the first policy in each policy chain on this variable
        # (and so, it cannot start with an easing of the 'normal' values)
    } else{
      if(p_type == 'cat_bus'){
        # ======= THIS IS FOR THE BarRestrict, RestaurantRestrict, OtherBusinessClose and the
        #                         NEBusinessClose VARIABLES
        df_fill$policy_measure_var_main[which_loc_date_vec] <- df_simplified$BusinessRestrictLevel[row_s_df]
      } else{
        if(p_type == 'numb'){
          # ======= THIS IS FOR THE GathRestrict VARIABLE
          df_fill$policy_measure_var_main[which_loc_date_vec] <- 'GathRestrict: see limit var-s'
        } else{
          if(p_type == 'cat_mand'){
            # ===== THIS IS FOR THE PublicMask VARIABLE
            df_fill$policy_measure_var_main[which_loc_date_vec] <- df_simplified$PublicMaskLevel[row_s_df]
          }
        }
      } 
    }
  }
  
  # --------------------------------------------------------------------------------
  
  return(df_fill)
  # --------------------------- END OF AUXILIARY FUNCTION --------------------------
}

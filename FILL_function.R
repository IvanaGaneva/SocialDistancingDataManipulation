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

FILL_function <- function(data_measures = COVID_measures_df_REVIEWED,
                          county_data = counties_df,
                          state_name,
                          policy_measure,
                          not_vec = c(0, NA)){
  
  # for construction/testing:
  # -------------------------------------------
  # data_measures <- COVID_measures_df_REVIEWED
  # county_data <- counties_df
  # state_name <- 'California'
  # policy_measure <- 'OtherBusinessClose'
  # not_vec <- c(0, NA)
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
      
      # if(length(starting_policies) == 0){
      #   # Before, this resulted in a simple warning:
      #   
      #   # print(paste0('Check data inputs - no starting policies in this policy chain!',
      #   #              ' - CASE OF ', state_name))
      #   # return(df_to_fill)
      #   
      #   # However, for OtherBusinessClose variable and the case of 27 states,
      #   # starting policies are missing!
      #   
      #   extract_first_non_missing_row <- policy_state_simplified %>%
      #     arrange(DateIssued) %>%
      #     filter(!(is.na(Expands)) | !(is.na(Eases)) |
      #              !(is.na(Joins)) | !(is.na(Leaves)))
      #   extract_first_non_missing_row <- extract_first_non_missing_row[1,]
      #   
      #   first_row_policy_link <- c(extract_first_non_missing_row$Expands,
      #                              extract_first_non_missing_row$Joins,
      #                              extract_first_non_missing_row$Eases,
      #                              extract_first_non_missing_row$Leaves)
      #   first_row_policy_link <- first_row_policy_link[!is.na(first_row_policy_link)]
      #   
      #   if(length(first_row_policy_link) != 0){
      #     first_row_policy_link <- first_row_policy_link[1]
      #     colnames(data_measures)[1] <- 'PID'
      #     
      #     row_to_add <- filter(data_measures, 
      #                          PID == first_row_policy_link)
      #     policy_state_simplified <- policy_state_simplified %>%
      #       bind_rows(row_to_add) %>%
      #       arrange(DateIssued)
      #     
      #     policy_state_simplified$begins[1] <- max(policy_state_simplified$DateEnacted[1],
      #                                              policy_state_simplified$DateIssued[1], na.rm = T)
      #     policy_state_simplified$finishes[1] <- min(policy_state_simplified$DateExpiry[1],
      #                                                policy_state_simplified$DateEnded[1],
      #                                                as.Date('2021-12-31'),
      #                                                na.rm = T)
      #     
      #     starting_policies <- setdiff(policy_state_simplified$PID, policy_state_changes$PID)
      #   }
      # 
      # } 
      
      # ------------------------------------------------------------------------------------
      # Checking again if there are still no starting policies after the above modification:
      if(length(starting_policies) == 0){
        # i.e. if no policy from another chain is present:
          print(paste0('Check data inputs - no starting policies in this policy chain!',
                        ' - CASE OF ', state_name))
          return(df_to_fill)
      } else{
        for(ps in starting_policies){
          df_to_fill <- aux_fun_chains_START_fill(starting_policy = ps,
                                                  p_type = policy_type, 
                                                  p_usual_vale = usual_value,
                                                  df_changes = policy_state_changes, 
                                                  df_simplified = policy_state_simplified,
                                                  df_fill = df_to_fill,
                                                  not_vec = c(0, NA))
          # updating the data frame to fill to match the starting policies
        }

        
        # ---------------------------- STEP III ---------------------------------
        # Continuing to fill for the remaining entries in the policy state df:
        changing_policies <- intersect(policy_state_changes$PID, 
                                       policy_state_simplified$PID)
        
        if(length(changing_policies) != 0){
          for(pc in changing_policies){
            row_pc_df <- which(policy_state_simplified$PID == pc)
            private_school_indicator <- F
            
            # -------------------------------------------------------------------------------------
            # Obtaining the dates for which this measure holds:
            if(policy_state_simplified$begins[row_pc_df] > 
               policy_state_simplified$finishes[row_pc_df]){
              # WARNING: THIS SHOULD NOT HAPPEN, BUT THERE APPEARS TO BE MISTAKE IN THE ENTRIES OF
              #          A FEW POLICY - THE TWO DATES ARE INTERCHANGED
              #          TAKE FOR EXAMPLE POLICY # MI0168
              pc_policy_dates <- seq(policy_state_simplified$finishes[row_pc_df],
                                     policy_state_simplified$begins[row_pc_df],
                                     by = 'days')
            } else{
              # the normal input scenario
              pc_policy_dates <- seq(policy_state_simplified$begins[row_pc_df],
                                     policy_state_simplified$finishes[row_pc_df],
                                     by = 'days')
            }
            
            last_day_pc_policy <- pc_policy_dates[length(pc_policy_dates)]
            first_day_pc_policy <- pc_policy_dates[1]
            
            # -------------------------------------------------------------------------------------
            # Obtaining the locations for which this measure holds:
            if(policy_state_simplified$SWGeo[row_pc_df] == 1){
              counties_to_fill_vec <- unique(as.character(df_to_fill$County))
              # i.e. if it is state-wide geographically, fill for all counties in the state
              #      for this policy instance
            } else{
              if(!(is.na(policy_state_simplified$Joins[row_pc_df]))|
                 !(is.na(policy_state_simplified$Expands[row_pc_df]))){
                # i.e. if this policy joins a previous one, and locations are specified
                # joins_id_row <- which(policy_state_simplified$PID == policy_state_simplified$Joins[row_pc_df])
                  # this is the past policy it joins
                joins_id_row <- c(policy_state_simplified$Joins[row_pc_df],
                                  policy_state_simplified$Expands[row_pc_df]) %>%
                  na.omit()
                joins_id_row <- joins_id_row[1]
                joins_id_row <- which(policy_state_simplified$PID == joins_id_row)
                
                if(length(joins_id_row) == 0){
                  joins_id_row <- max(which(changing_policies == pc), 1)
                }
                
                if(policy_state_simplified$SWGeo[joins_id_row] == 1){
                  if(policy_type != 'cat_sch'){
                    joins_id_locations <- unique(as.character(df_to_fill$County))
                    # ideally, this case should not occur with other variables than SchoolClose
                    # cannot join a policy which was initially SWGeo
                  } else{
                    private_school_indicator <- T
                    # This means that the variable will be filling ONLY for the private schools
                    # (secondary) variable
                    joins_id_locations <- unlist(str_split(policy_state_simplified$AppliesTo[row_pc_df],
                                                           ', '))
                  }
                } else{
                  joins_id_locations <- unlist(str_split(policy_state_simplified$AppliesTo[joins_id_row],
                                                         ', '))
                }
                  # and these are the locations it joins
                
                # Here, the counties for which the policy applies to will be simply:
                counties_to_fill_vec <- union(unlist(str_split(policy_state_simplified$AppliesTo[row_pc_df],
                                                                 ', ')),
                                                # these are the newly-added locations
                                                joins_id_locations)
                  # and these are the old ones
                
                rm(joins_id_row, joins_id_locations)
              } else{
                if(!(is.na(policy_state_simplified$Leaves[row_pc_df]))|
                   !(is.na(policy_state_simplified$Eases[row_pc_df]))){
                  # i.e. if this policy LEAVES a previous one, and locations are specified
                  # [this case is simpler, see data documentation for reference]  

                  counties_to_fill_vec <- unlist(str_split(policy_state_simplified$AppliesTo[row_pc_df],
                                                           ', '))
                }
              }
            }
            
            # Now that we have the locations and the dates, we can obtain their combinations:
            # [similary to what has been done in the aux_fun_chains_START_fill function]
            
            # Putting the dates-locations combinations to be filled in a single vector:
            which_loc_date_vec <- which((df_to_fill$County %in% counties_to_fill_vec) &
                                          (df_to_fill$Date %in% pc_policy_dates))
            
            # -------------------------------------------------------------------------------------
            # Now, actually filling the instances in the data frame:
            if(policy_type == 'cat_sch'){
              # =========== THIS IS FOR THE SchoolClose VARIABLE
              if(is.na(policy_state_simplified$SchoolRestrictLevel[row_pc_df])){
              # That is, if level is not present
                if(!(is.na(policy_state_simplified$Expands[row_pc_df]))|
                   !(is.na(policy_state_simplified$Joins[row_pc_df]))){
                  # i.e. if level is not present and policy is expansive w.r.t previous one
                  related_policy_level <- policy_state_simplified$SchoolRestrictLevel[policy_state_simplified$PID %in%
                                                                                        c(policy_state_simplified$Expands[row_pc_df],
                                                                                          policy_state_simplified$Joins[row_pc_df])]
                  related_policy_level <- related_policy_level[!(is.na(related_policy_level))]
                  related_policy_level <- related_policy_level[1]
                  
                  if(related_policy_level %in% c('NoInPerson', 'LimitedInPerson')){
                    policy_level_to_fill_with <- 'NoInPerson'
                  } else{
                    if(related_policy_level == 'InPersonAllowed'){
                      policy_level_to_fill_with <- 'LimitedInPerson'
                    }
                  } 
                } else{
                  if(!(is.na(policy_state_simplified$Eases[row_pc_df]))|
                     !(is.na(policy_state_simplified$Leaves[row_pc_df]))){
                  # i.e. if level is not present and policy is easing w.r.t previous one
                    related_policy_level <- policy_state_simplified$SchoolRestrictLevel[policy_state_simplified$PID %in%
                                                                                          c(policy_state_simplified$Eases[row_pc_df],
                                                                                            policy_state_simplified$Leaves[row_pc_df])]
                    related_policy_level <- related_policy_level[!(is.na(related_policy_level))]
                    related_policy_level <- related_policy_level[1]
                    
                    if(related_policy_level %in% c('InPersonAllowed', 'LimitedInPerson')){
                      policy_level_to_fill_with <- 'InPersonAllowed'
                    } else{
                      if(related_policy_level == 'NoInPerson'){
                        policy_level_to_fill_with <- 'LimitedInPerson'
                      }
                    }
                  }
                }
              } else{
                # That is, if level IS present
                policy_level_to_fill_with <- policy_state_simplified$SchoolRestrictLevel[row_pc_df]
              }
              
              # Now, looking at whether we should fill for public/private schools only, 
              # or should we fill for both!
              
              if(private_school_indicator){
                # i.e. if need to fill only for private schools:
                df_to_fill$policy_measure_var_sec[which_loc_date_vec] <- policy_level_to_fill_with
              } else{
                df_to_fill$policy_measure_var_main[which_loc_date_vec] <- policy_level_to_fill_with
                
                if(str_detect(policy_state_simplified$PolicyCodingNotes[row_pc_df], 'private|Private')){
                  # i.e. if need to fill for both
                  df_to_fill$policy_measure_var_sec[which_loc_date_vec] <- policy_level_to_fill_with
                }
              }
              # ============================ END OF SCHOOLCLOSE VARIABLE (most complex case)
            } else{
              if(policy_type == 'bin'){
                # ========= THIS IS FOR THE EmergDec, CaseIsolation, StayAtHome, BusinessMask, SchoolMask,
                #                           Quarantine, & the three TravelRestrict VARIABLES
                df_to_fill$policy_measure_var_main[which_loc_date_vec] <- 1
              } else{
                if(policy_type == 'cat_bus'){
                  # ======= THIS IS FOR THE BarRestrict, RestaurantRestrict, OtherBusinessClose and the
                  #                         NEBusinessClose VARIABLES
                  if(is.na(policy_state_simplified$BusinessRestrictLevel[row_pc_df])){
                    # That is, if level is not present
                    if(!(is.na(policy_state_simplified$Expands[row_pc_df]))|
                       !(is.na(policy_state_simplified$Joins[row_pc_df]))){
                      # i.e. if level is not present and policy is expansive w.r.t previous one
                      related_policy_level <- policy_state_simplified$BusinessRestrictLevel[policy_state_simplified$PID %in%
                                                                                              c(policy_state_simplified$Expands[row_pc_df],
                                                                                                policy_state_simplified$Joins[row_pc_df])]
                      related_policy_level <- related_policy_level[!(is.na(related_policy_level))]
                      related_policy_level <- related_policy_level[1]
                      
                      if(related_policy_level %in% c('FullClose', 'TakeawayOnly')){
                        policy_level_to_fill_with <- 'FullClose'
                      } else{
                        if(related_policy_level == 'OutdoorOnly'){
                          policy_level_to_fill_with <- 'TakeawayOnly'
                        } else{
                          if(related_policy_level == 'IndoorAllowed'){
                            policy_level_to_fill_with <- 'OutdoorOnly'
                          }
                        }
                      }
                    } else{
                      if(!(is.na(policy_state_simplified$Eases[row_pc_df]))|
                         !(is.na(policy_state_simplified$Leaves[row_pc_df]))){
                        # i.e. if level is not present and policy is easing w.r.t previous one
                        related_policy_level <- policy_state_simplified$BusinessRestrictLevel[policy_state_simplified$PID %in%
                                                                                              c(policy_state_simplified$Eases[row_pc_df],
                                                                                                policy_state_simplified$Leaves[row_pc_df])]
                        related_policy_level <- related_policy_level[!(is.na(related_policy_level))]
                        related_policy_level <- related_policy_level[1]
                        
                        if(related_policy_level %in% c('IndoorAllowed', 'OutdoorOnly')){
                          policy_level_to_fill_with <- 'IndoorAllowed'
                        } else{
                          if(related_policy_level == 'TakeawayOnly'){
                            policy_level_to_fill_with <- 'OutdoorOnly'
                          } else{
                            if(related_policy_level == 'FullClose'){
                              policy_level_to_fill_with <- 'TakeawayOnly'
                            }
                          }
                        }
                      }
                    }
                  } else{
                    # That is, if the level IS present:
                    policy_level_to_fill_with <- policy_state_simplified$BusinessRestrictLevel[row_pc_df]
                  }
                  
                  # Actually filling now:
                  df_to_fill$policy_measure_var_main[which_loc_date_vec] <- policy_level_to_fill_with
                } else{
                  if(policy_type == 'numb'){
                    # ======= THIS IS FOR THE GathRestrict VARIABLE
                    df_to_fill$policy_measure_var_main[whcih_loc_date_vec] <- 'GathRestrict: see limit var-s'
                  } else{
                    if(policy_type == 'cat_mand'){
                      # ===== THIS IS FOR THE PublicMask VARIABLE
                      df_to_fill$policy_measure_var_main[which_loc_date_vec] <- policy_state_simplified$PublicMaskLevel[row_pc_df]
                    }
                  }
                }
              }
            }
            # -------------------------------------------------------------------------------------
            
            # Lastly, filling for the perc_usual_time, only_non_vaccinated_ppl, lim_in_general, 
            # lim_out_general, lim_in_rel, lim_out_rel, & the mandate columns:
          
            # ===============================
            # FILLING FOR THE TIME DIMENSION:
            if(policy_state_simplified$Curfew[row_pc_df] %in% not_vec){
              df_to_fill$perc_usual_time[which_loc_date_vec] <- 'All the time'
            } else{
              df_to_fill$perc_usual_time[which_loc_date_vec] <- paste0('From ',
                                                                       policy_state_simplified$CurfewStart[row_pc_df],
                                                                       ' to ',
                                                                       policy_state_simplified$CurfewEnd[row_pc_df])
            }
            
            # =========================================
            # FILLING FOR THE VACCINATED PPL DIMENSION:
            if(policy_state_simplified$VaccineExempt[row_pc_df] %in% not_vec){
              df_to_fill$only_non_vaccinated_ppl[which_loc_date_vec] <- 0
            } else{
              df_to_fill$only_non_vaccinated_ppl[which_loc_date_vec] <- 1
            }
            
            # ==================================
            # FILLING FOR THE MANDATE DIMENSION:
            if(policy_state_simplified$Mandate[row_pc_df] %in% not_vec){
              df_to_fill$mandate[which_loc_date_vec] <- 0
            } else{
              df_to_fill$mandate[which_loc_date_vec] <- 1
            }
            
            # =====================================
            # FILLING FOR THE GATHERINGS DIMENSION:
            # -> FOR INSIDE, GENERAL GATHERINGS
            if(is.na(policy_state_simplified$InGathLim[row_pc_df])){
              df_to_fill$lim_in_general[which_loc_date_vec] <- 'No limit'
            } else{
              df_to_fill$lim_in_general[which_loc_date_vec] <- policy_state_simplified$InGathLim[row_pc_df]
            }
            # -> FOR OUTSIDE, GENERAL GATHERINGS
            if(is.na(policy_state_simplified$OutGathLim[row_pc_df])){
              df_to_fill$lim_out_general[which_loc_date_vec] <- 'No limit'
            } else{
              df_to_fill$lim_out_general[which_loc_date_vec] <- policy_state_simplified$OutGathLim[row_pc_df]
            }
            # -> FOR INSIDE, RELIGIOUS GATHERINGS
            if(is.na(policy_state_simplified$InGathLimReligious[row_pc_df])){
              df_to_fill$lim_in_rel[which_loc_date_vec] <- 'No limit'
            } else{
              df_to_fill$lim_in_rel[which_loc_date_vec] <- policy_state_simplified$InGathLimReligious[row_pc_df]
            }
            # -> FOR OUTSIDE, RELIGIOUS GATHERINGS
            if(is.na(policy_state_simplified$OutGathLimReligious[row_pc_df])){
              df_to_fill$lim_out_rel[which_loc_date_vec] <- 'No limit'
            } else{
              df_to_fill$lim_out_rel[which_loc_date_vec] <- policy_state_simplified$OutGathLimReligious[row_pc_df]
            }
            
            
            # -------------------- END OF FILLING PART ----------------------------------
          }
        }
      }
    }
  }
  
  return(df_to_fill)
  # --------------------------------------- END OF FUNCTION -----------------------------
}

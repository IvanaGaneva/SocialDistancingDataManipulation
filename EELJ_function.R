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

# FUNCTION THAT DETECTS THE SOURCE(S) OF HETEROGENEITY IN AN INSTANCE OF 
# EXPANDS/EASES/LEAVES/JOINS POLICY

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

EELJ_function <- function(data_measures = COVID_measures_df_REVIEWED,
                          state_name,
                          policy_measure){
  # input: the policy measure and state of interest
  # output: a data frame with the information on expand/ease/leave/join
  #         policies and the heterogeneities mechanically found

  state_measures <- ENDS_EXTENDS_function(state_name = state_name,
                                          policy_measure = policy_measure)
    # applying the ends-extends-function here so as to simplify entries
  
  output_df <- state_measures %>%
    mutate(Eases_ind = ifelse(is.na(Eases), 0, 1),
           Expands_ind = ifelse(is.na(Expands), 0, 1),
           Joins_ind = ifelse(is.na(Joins), 0, 1),
           Leaves_ind = ifelse(is.na(Leaves), 0, 1))
    # indicator variables which will be useful in a bit
    # initializing the output data frame here
  output_df <- output_df %>% 
    mutate(sum_ind = output_df %>% select(ends_with('_ind')) %>% rowSums()) %>%
    filter(sum_ind != 0) %>%
    # interested not in initial policies here but rather in policies that either expand
    # or shrink a previous policy OR change the location of it
    select(PID, StateName, StatePolicy, 
           # these are key variables
           ends_with('_ind')) %>%
           # will now add some important variables on the heterogeneity
    mutate(more_strict = ifelse(Expands_ind + Joins_ind >= 1, 
                                1, 
                                ifelse(Eases_ind + Leaves_ind >= 1, 1,NA)),
           # this could never be NA in fact due to the filtering on sum_ind from above
           ch_mandate = NA,
           # if it changes mandate of the policy
           ch_SW = NA,
           ch_SWGeo = NA,
           ch_location = NA,
           ch_SWPop = NA,
           # if there is a change to the state-wide measures in a way
           ch_Curfew = NA,
           ch_CurfewStart = NA,
           ch_CurfewEnd = NA,
           # if there is a change to the curfew measures in a way
           ch_InGathLim = NA,
           ch_OutGathLim = NA,
           ch_InGathLimRel = NA, 
           ch_OutGathLimRel = NA,
           # if there is a change in the gatherings limits of persons
           ch_BusinessRestrict_lvl = NA,
           ch_SchoolRestrict_lvl = NA,
           ch_PublicMask_lvl = NA
           # if there are some changes in the above three levels
           )
  
  for(r in 1:nrow(output_df)){
    row_in_sm_df <- which(state_measures$PID == output_df$PID[r])
    related_policies_vec <- paste(state_measures$Expands[row_in_sm_df],
                                  state_measures$Eases[row_in_sm_df],
                                  state_measures$Joins[row_in_sm_df],
                                  state_measures$Leaves[row_in_sm_df],
                                  sep = ';') %>%
      str_remove_all(';NA') %>%
      strsplit(';') %>%
      unlist()
      # these are the past policies which are changed by the underlying output_df$PID[r] policy measure
    
    results_ch_df <- state_measures[0,] %>%
      mutate(across(everything(), as.character))
    for(w in 1:length(related_policies_vec)){
      temp_df <- state_measures %>%
        filter(PID == related_policies_vec[w] | PID == output_df$PID[r])
      temp_df[is.na(temp_df)] <- ''
      comparing_policies <- as.character(temp_df[1,] == temp_df[2,])
      results_ch_df[w,] <- comparing_policies
      # this shows whether there have been changes to the related_policies_vec[w] policy measure in 
      # all of the aspects we are interested in
    }
    
    results_vec_final <- c()
    for(o in 1:ncol(results_ch_df)){
      results_vec_final <- c(results_vec_final, 
                             ifelse(sum(as.logical(results_ch_df[,o])) == nrow(results_ch_df),
                                  # i.e. if all are true and there are no changes anywhere
                                  0,
                                  1))
    }
    names(results_vec_final) <- colnames(results_ch_df)
    
    # Now, we are ready to fill the output_df:
    output_df$ch_mandate[r] <- as.numeric(results_vec_final['Mandate'])
      # for the mandatory lvl
    output_df$ch_SW[r] <- as.numeric(results_vec_final['StateWide'])
    output_df$ch_SWGeo[r] <- as.numeric(results_vec_final['SWGeo'])
    output_df$ch_SWPop[r] <- as.numeric(results_vec_final['SWPop'])
    output_df$ch_location[r] <- as.numeric(results_vec_final['AppliesTo'])
      # if there is a change to the state-wide measures in a way
    output_df$ch_Curfew[r] <- as.numeric(results_vec_final['Curfew'])
    output_df$ch_CurfewStart[r] <- as.numeric(results_vec_final['CurfewStart'])
    output_df$ch_CurfewEnd[r] <- as.numeric(results_vec_final['CurfewEnd'])
      # if there is a change to the curfew measures in a way
    output_df$ch_InGathLim[r] <- as.numeric(results_vec_final['InGathLim'])
    output_df$ch_OutGathLim[r] <- as.numeric(results_vec_final['OutGathLim'])
    output_df$ch_InGathLimRel[r] <- as.numeric(results_vec_final['InGathLimReligious'])
    output_df$ch_OutGathLimRel[r] <- as.numeric(results_vec_final['OutGathLimReligious'])
      # if there is a change in the gatherings limits of persons
    output_df$ch_BusinessRestrict_lvl[r] <- as.numeric(results_vec_final['BusinessRestrictLevel'])
    output_df$ch_SchoolRestrict_lvl[r] <- as.numeric(results_vec_final['SchoolRestrictLevel'])
    output_df$ch_PublicMask_lvl[r] <- as.numeric(results_vec_final['PublicMaskLevel'])
      # if there are some changes in the above three levels
  }
          
  output_df <- output_df %>%
    mutate(sum_changes = output_df %>% select(starts_with('ch_')) %>% rowSums()) %>%
    mutate(ch_uncoded = ifelse(sum_changes == 0, 1, 0)) %>%
    # if the changes are not coded within the previous categories and one needs to have a look
    # at the respective policy measure description within original data
    select(-starts_with('sum_'))

  return(output_df)
}

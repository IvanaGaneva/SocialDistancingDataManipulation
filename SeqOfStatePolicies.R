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

# CREATING A FUNCTION THAT MAKES AN THAT MAKES A SEQUENCE OF TIME-RELATED 
# POLICIES WITHIN A SINGLE STATE

# NOTE: This is probably the second most important function for this set of codes

# NOTE, mid-April: bug fixed following the introduction of new variables to the data set
#                  by the authors

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function_seq_of_policies_for_state <- function(data_measures = COVID_measures_df_REVIEWED,
                                               state_name){
  # This function creates a seq of the policies within each state.
  # Special attention paid to the cases where a policy eases or expands a previous measure.
  state_measures <- data_measures %>%
    filter(StateName == state_name)
    # filtering out data for this specific state only
  colnames(state_measures)[1] <- 'PID'
  
  vec_PIDs <- state_measures$PID
  # this is a vector of all the PIDs implemented for the state
  
  cols_indicating_change <- which(colnames(state_measures) %in% c('Extends', 'Expands',
                                                                  'Eases', 'Ends',
                                                                  'Joins', 'Leaves'))
  state_measures$First_Policy <- apply(state_measures[, cols_indicating_change], 
                                       1, 
                                       function(x) all(is.na(x)))
  # checking if the policy does not relate to previous policies
  # (i.e. if it is the first policy in the chain)
  
  state_measures <- state_measures %>%
    mutate(First_Policy_YN = ifelse(First_Policy == T, 'Y', NA)) %>%
    select(-First_Policy)
    # will now gather the data for easier manipulation / brevity
  cols_not_gather <- which(!(1:ncol(state_measures) %in% cols_indicating_change))
  cols_not_gather <- cols_not_gather[-length(cols_not_gather)]
    # these are the cols not participating in the gather -> the ones we are interested in this point
    # these are the complement of cols indicating change / indicating First Policy
  state_measures <- state_measures %>%
    gather(type_of_link, links_to_policy, -all_of(cols_not_gather)) %>%
    select(PID, type_of_link, links_to_policy, everything())
    # re-ordering the variables for brevity
  # Some of the variables here are unneccessary for this part of the data manipulation
  vars_to_keep_in_sm <- c(1:3,
                          which(colnames(state_measures) %in% c('StateName', 'StatePolicy',
                                                                'Mandate', 'StateWide',
                                                                'SWGeo', 'SWPop',
                                                                'DateIssued', 'DateEnacted',
                                                                'DateExpiry', 'DateEnded',
                                                                'AppliesTo', 'PolicyCodingNotes')))
  # NOTE: Adhering to names of cols instead of simply using their respective numbers due to an issue
  #       which arised after the authors of the data decided to add intermediate columns and change
  #       some variables.
  state_measures <- state_measures %>%
    select(all_of(vars_to_keep_in_sm)) %>%
    filter(!(is.na(links_to_policy)))
  # all in all, folding data for simpler use

  policy_chain_df <- state_measures %>%
    filter(links_to_policy == 'Y') %>%
    select(Chain_start = PID,
           Initial_policy = StatePolicy,
           Start_act = DateEnacted
           # when the policy was enacted
    ) %>%
    mutate(Chain_of_policies = Chain_start,
           End_act_or_exp = Start_act,
           # when the policy ended / or when it is expected to end (when not yet ended))
           Reached_end_of_chain = 'N',
           # to check whether the end of the policy chain has been reached within the loop below
           # N = No; Y = Yes
           Includes_policies = Initial_policy,
           # this is to check consistency of data (whether there are chains regarding more than 1
           # type of policy - which should not happen ideally)
           End_Date_Obtained = NA
           # this ill be an indicator to show whether the end date has been correcly obtained
           # or whether user should manually calculate it
           # ideally, all of the values in this column will always be TRUE
           # however, this allows to see when data is incomplete/missing or there are some faulty 
           # or worrying inputs somewhere along the chain
    )
  # this will be the data frame to fill
  
  for(id in policy_chain_df$Chain_start){
    # looping over the policy id-s possible
    row_chain_start <- which(policy_chain_df$Chain_start == id)
    
    old_ids_to_check <- id
    temp_df <- filter(state_measures, links_to_policy == old_ids_to_check)
    
    while(nrow(temp_df) != 0){
      new_ids_to_check <- temp_df$PID
      if(length(new_ids_to_check) == 1){
        policy_chain_df$Chain_of_policies[row_chain_start] <- paste0(policy_chain_df$Chain_of_policies[row_chain_start],
                                                                     ', ',
                                                                     new_ids_to_check)
        policy_chain_df$Includes_policies[row_chain_start] <- paste0(policy_chain_df$Includes_policies[row_chain_start],
                                                                     ', ',
                                                                     temp_df$StatePolicy)
      } else{
        for(l in 1:length(new_ids_to_check)){
          policy_chain_df$Chain_of_policies[row_chain_start] <- paste0(policy_chain_df$Chain_of_policies[row_chain_start],
                                                                       ', ',
                                                                       new_ids_to_check[l])
          policy_chain_df$Includes_policies[row_chain_start] <- paste0(policy_chain_df$Includes_policies[row_chain_start],
                                                                       ', ',
                                                                       temp_df$StatePolicy[l])
        }
      }
      old_ids_to_check <- new_ids_to_check
      temp_df <- filter(state_measures, links_to_policy %in% old_ids_to_check)
    }
    
    # the last policy of the chain is stored in the old_ids (and the new_ids to check)
    
    policy_chain_df$Reached_end_of_chain[row_chain_start] <- 'Y'
  }
  
  policy_chain_df <- policy_chain_df %>%
    mutate(indicator_MTP = str_replace_all(Includes_policies, paste0(Initial_policy, ', '), '')) %>%
    mutate(indicator_MTP = str_replace_all(indicator_MTP, Initial_policy, '')) %>%
    mutate(ind_MTP = ifelse(indicator_MTP == '', F, T)) %>%
    select(-indicator_MTP)
  # indicator which shows whether there are Multiple Types of Policies (MTP) in this policy chain
  # (this should be F if there are no faulty entries in the data, ideally)
  
  # LAST STEP HERE (IMPORTANT):
  # - obtaining the end date (real/expected where no real one yet) of a policy chain
  for(id in policy_chain_df$Chain_start){
    # looping over the policy id-s possible
    row_chain_start <- which(policy_chain_df$Chain_start == id)
    
    if(policy_chain_df$ind_MTP[row_chain_start] == T){
      policy_chain_df$End_Date_Obtained[row_chain_start] <- 'ATTENTION NEEDED: Multiple Policies in Chain'
    } else{
      # If only one StatePolicy type is present within the policy chain:
      policies_vec_of_chain <- str_replace_all(policy_chain_df$Chain_of_policies[row_chain_start],
                                               ' ', '') %>%
        strsplit(split = ',') %>%
        unlist()
      specific_chain_temp_df <- state_measures %>%
        filter(PID %in% policies_vec_of_chain) %>%
        arrange(DateIssued)
      if(specific_chain_temp_df$links_to_policy[1] == 'Y'){
        if(!('Ends' %in% specific_chain_temp_df$type_of_link)){
          # If there are no policies of the 'End' type:
          specific_chain_temp_df <- specific_chain_temp_df %>%
            mutate(END_date = fifelse(is.na(DateEnded),
                                     fifelse(is.na(DateExpiry),
                                            lubridate::ymd('2100-12-31'),
                                            # i.e. if both dates for expiry/ending are missing,
                                            # we naturallly assume that the policy is still in place
                                            # at the current moment AND we set a date in the far
                                            # future so as to distinguish such cases
                                            DateExpiry),
                                     min(DateExpiry, DateEnded))) %>%
            arrange(desc(END_date))

          policy_chain_df$End_act_or_exp[row_chain_start] <- specific_chain_temp_df$END_date[1]
          policy_chain_df$End_Date_Obtained[row_chain_start] <- 'TRUE'
        } else{
          # If there are policies of the 'End' type:
          specific_chain_temp_df <- specific_chain_temp_df %>%
            filter(type_of_link == 'Ends')
          if(nrow(specific_chain_temp_df == 1)){
            policy_chain_df$End_act_or_exp[row_chain_start] <- specific_chain_temp_df$DateEnacted[1]
            policy_chain_df$End_Date_Obtained[row_chain_start] <- 'TRUE'
          } else{
            policy_chain_df$End_Date_Obtained[row_chain_start] <- 'ATTENTION NEEDED: Multiple End Dates'
          }
        }
      } else{
        policy_chain_df$End_Date_Obtained[row_chain_start] <- 'ATTENTION NEEDED: First Policy In Chain was Not Issued First'
      }
    }
  }
  return(policy_chain_df)
}

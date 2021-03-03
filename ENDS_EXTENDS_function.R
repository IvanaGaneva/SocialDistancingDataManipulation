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

# FUNCTION TO SIMPLIFY THE DATA FRAME BY TRANSFORMING THE EXTENDING/ENDING
# POLICIES TO BE INCORPORATED IN THE TIMING OF THE PREVIOUS POLICY THEY ARE
# ARE RELATED TO

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

ENDS_EXTENDS_function <- function(data_measures = COVID_measures_df_REVIEWED,
                                  state_name,
                                  policy_measure){
  # input: the policy measure and state of interest
  # output: a data frame with the instances of the policy measure on the state
  #         of interest to NOT include the ending/extending entries separately
  #         (i.e. a simpler data frame with fewer rows)
  
  # reasoning: extends and ends are always time-related, as stated in the data
  #           documentation file
  
  state_measures <- data_measures %>%
    filter(StateName == state_name,
           StatePolicy == 'SchoolClose')
  colnames(state_measures)[1] <- 'PID'
  
  # A brief manipulation for the Ends variable:
  for(r in 1:nrow(state_measures)){
    if(is.na(state_measures$DateExpiry[r]) & is.na(state_measures$DateEnded[r])){
      # if neither an expiry date nor an ending date is included originally in some entry
      if(state_measures$PID[r] %in% state_measures$Ends){
        # but there is an ending policy to include this entry's end date
        ending_policy_found <- which(state_measures$Ends == state_measures$PID[r])
        # we can in this case infer it from an ending policy measure
        state_measures$DateEnded[r] <- state_measures$DateEnacted[ending_policy_found]
      } else{
        # if there is no ending policy to include this entry's end date,
        # setting a date in the far future (so as to distinguish from originally-stated
        # ending/expiration dates)
        state_measures$DateEnded[r] <- lubridate::ymd('2100-12-31')
      }
    }
  }
  state_measures <- state_measures %>%
    filter(is.na(Ends)) %>%
    select(-Ends) %>%
    # do not need this variable anymore as it carries no extra information
    arrange(desc(DateIssued))
    # ordering in this way so as to remove recursively backwards the Extending instances
  
  # A brief manipulation for the Extends variable:
  extending_PIDs <- state_measures$PID[!(is.na(state_measures$Extends))]
    # these are the policy IDs of extending policies
  l <- length(extending_PIDs)
  if(l != 0){
    # i.e. if there are indeed some extending policies
    for(i in 1:l){
      prev_policy_relation <- state_measures$Extends[state_measures$PID == extending_PIDs[i]]
      new_expiry_date <- state_measures$DateExpiry[state_measures$PID == extending_PIDs[i]]
      new_ending_date <- state_measures$DateEnded[state_measures$PID == extending_PIDs[i]]
      
      state_measures$DateExpiry[state_measures$PID == prev_policy_relation] <- new_expiry_date
      state_measures$DateEnded[state_measures$PID == prev_policy_relation] <- new_ending_date
      
      state_measures <- state_measures %>%
        filter(PID != extending_PIDs[i])
      
      state_measures_dt <- as.data.table(state_measures)
      coords <- c(25:29)
      state_measures_dt[, (coords) := replace(.SD, 
                                              .SD == extending_PIDs[i], 
                                              prev_policy_relation), .SDcols = coords]
      state_measures <- as.data.frame(state_measures_dt)
        # ideally, there should not be any such entries to edit out since we have made the ordering
        # in a descending manner by date issued, but since there can be multiple policies issued at
        # one day, etc., this row is needed.
    }
    state_measures <- state_measures %>%
      arrange(DateIssued) %>%
      mutate(begins = DateEnacted,
             finishes = fifelse(is.na(DateEnded), DateExpiry, DateEnded)) %>%
      select(-Extends)
    return(state_measures)
  }
}

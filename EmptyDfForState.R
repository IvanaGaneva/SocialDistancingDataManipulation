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

# CREATING A FUNCTION THAT MAKES AN EMPTY DATA FRAME FOR EACH STATE
#   [AS DISCUSSED VIA DISCORD: A COLUMN FOR EACH DATE AND A ROW FOR EACH 
#                              COUNTY IN THE STATE; THERE WILL BE A DATA 
#                              FRAME OF THIS KIND FOR EACH POSSIBLE MEASURE]

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

function_empty_df_for_state <- function(data_counties = counties_df,
                                        data_measures = COVID_measures_df_REVIEWED,
                                        state_name,
                                        long_output = T){
  # long_output = T argument means that the output will be in long format:
  #  -> nrow(state_counties) * length(dates_state) rows
  #  -> 2 cols
  # long_output = F means that the output data frame will have 
  #  -> nrow(state_counties) rows
  #  -> 2 + length(date_state) cols
  
  state_measures <- data_measures %>%
    filter(StateName == state_name)
  # filtering out data for this specific state only
  first_date_state <- min(state_measures$DateIssued, na.rm = T)
  last_date_state <- max(state_measures$DateExpiry, na.rm = T)
  # obtaining the first and last date for policy data on this specific state
  dates_state <- seq(first_date_state, last_date_state, by = 'day')
  # obtaining the sequence of dates between the above two dates
  
  state_counties <- data_counties %>%
    filter(State == state_name)
  # filtering out the counties' names for the state of interest
  
  blank_df <- expand.grid(state_counties$County, dates_state) %>%
    # this gives a data frame of nrow(state_counties) * length(dates_state) rows
    rename(County = Var1, Date = Var2) %>%
    mutate(State = state_name) %>%
    select(State, County, Date) %>%
    arrange(County, Date)
  
  if(long_output == F){
    blank_df <- blank_df %>%
      mutate(Value_Policy = NA) %>%
      spread(Date, Value_Policy)
  }
  
  return(blank_df)
}

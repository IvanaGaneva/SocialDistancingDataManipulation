# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ganeva, I.K.
# Covid 19 State Policy Data Cleaning, Exploration & Implementation of 
# Hand-Made Functions for the Construction of a Set of Local Metrics
# Feb-Mar, 2021

# __________________________________________________________________________
# Nancy Fullman, Bree Bang-Jensen, Grace Reinke, Beatrice Magistro,          
# Rachel Castellano, Megan Erickson, Rebecca Walcott, Carolyn Dapper, 
# Kenya Amano, John Wilkerson, and Christopher Adolph. 
# "State-level social distancing policies in response to COVID-19 in the US". 
# Version 1.117, February 17, 2021. http://www.covid19statepolicy.org
# __________________________________________________________________________

# A simple EDA function to establish whether a policy is:
# -- always/sometimes/never mandatory
# -- always/sometimes/never state-wide
# -- how many of the non-state-wide cases have information on the locality

# Inputs:
# -- the COVID_measures_REVIEWED data frame

# Output:
# -- a data frame to contain information on the above three dimensions for each of
# -- the distinct policy measures considered in the COVID_measures_REVIEWED df

simple_EDA_fun <- function(main_df){
  output_df <- data.frame(StatePolicy = unique(main_df$StatePolicy),
                          Mandatory_perc = NA,
                          SW_perc = NA,
                          Non_SW_present = NA,
                          AppliesTo_present_perc = NA)
  for(sp in output_df$StatePolicy){
    temp_df <- main_df %>%
      filter(StatePolicy == sp)
    
    # Percentage of mandatory instances where Mandate is not NA:
    Mandatory_perc <- 100*nrow(filter(temp_df, Mandate == 1))/nrow(filter(temp_df, !(is.na(Mandate))))
    
    # Percentage of state-wide instances where StateWide is not NA:
    SW_perc <- 100*nrow(filter(temp_df, StateWide == 1))/nrow(filter(temp_df, !(is.na(StateWide))))
    
    # Presence of non-state wide instances:
    Non_SW_present <- ifelse(SW_perc == 100, 0, 1)
    
    # Percentage of the non-state-wide cases with info on the locality of the measure:
    AppliesTo_present <- ifelse(Non_SW_present == 0,
                                NA,
                                100*nrow(filter(temp_df, StateWide == 0, !is.na(AppliesTo)))/nrow(filter(temp_df, StateWide == 0)))
    
    # Filling the output_df:
    
  }
}
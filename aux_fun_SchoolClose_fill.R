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
# WITHIN THE SchoolClose VARIABLE 
# (AUXILIARY CODE FOR BREVITY)

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# UNDER CONSTRUCTION:
aux_fun_SchoolClose_fill <- function()

if(policy_type == 'cat_sch'){
  # ======== THIS IS FOR THE SchoolClose VARIABLE
  df_to_fill$policy_measure_var_main[which_loc_date_vec] <- policy_state_simplified$SchoolRestrictLevel[1]
  if(str_detect(policy_state_simplified$PolicyCodingNotes[1], 'private')){
    df_to_fill$policy_measure_var_sec[which_loc_date_vec] <- policy_state_simplified$SchoolRestrictLevel[1]
  }
}
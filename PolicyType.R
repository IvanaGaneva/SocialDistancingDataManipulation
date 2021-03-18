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

# FUNCTION THAT TELLS THE POLICY TYPE
# LAST UPDATE: 9 MARCH 2021

# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

policy_type_function <- function(policy_measure){
  
  policy_types_df <- data.frame('StatePolicy' = rep(NA, 16),
                                'PolicyType' = rep(NA, 16))
  
  policy_types_df[,1] <- c('EmergDec', 'SchoolClose', 'BarRestrict', 'GathRestrict',
                           'RestaurantRestrict', 'OtherBusinessClose', 'CaseIsolation', 'StayAtHome',
                           'PublicMask', 'BusinessMask', 'SchoolMask', 'Quarantine',
                           'TravelRestrictExit', 'NEBusinessClose', 'TravelRestrictIntra',
                           'TravelRestrictEntry')
  policy_types_df[,2] <- c('bin', 'cat_sch', 'cat_bus', 'numb',
                           'cat_bus', 'cat_bus', 'bin', 'bin_rec',
                           'cat_mand', rep('bin', 4),
                           'cat_bus', rep('bin', 2))
  
  if(policy_measure %in% policy_types_df$StatePolicy){
    type_of_this_measure <- policy_types_df$PolicyType[policy_types_df$StatePolicy == policy_measure]
    
    usual_no_restr_value_df <- data.frame(PolicyType = c('bin', 'cat_sch', 'cat_bus',
                                                         'numb', 'bin_rec', 'cat_mand'),
                                          UsualValue = c('0', 'InPersonAllowed', 'IndoorAllowed',
                                                         '999999', 'NotMentioned', 'NotMentioned'))
    
    usual_value <- usual_no_restr_value_df$UsualValue[usual_no_restr_value_df$PolicyType == type_of_this_measure]
  } else{
    type_of_this_measure <- 'undefined'
    usual_value <- 'undefined'
  }
  
  vec_results <- c(type_of_this_measure, usual_value)
    
  return(vec_results)
  
}

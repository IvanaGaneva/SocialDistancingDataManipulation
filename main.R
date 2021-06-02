# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Ganeva, I.K.
# Covid 19 State Policy Data Cleaning, Exploration & Implementation of 
# Hand-Made Functions for the Construction of a Set of Local Metrics
# Feb-Apr, 2021

# __________________________________________________________________________
# Nancy Fullman, Bree Bang-Jensen, Grace Reinke, Beatrice Magistro,          
# Rachel Castellano, Megan Erickson, Rebecca Walcott, Carolyn Dapper, 
# Kenya Amano, John Wilkerson, and Christopher Adolph. 
# "State-level social distancing policies in response to COVID-19 in the US". 
# Version 1.117, February 17, 2021. http://www.covid19statepolicy.org
# __________________________________________________________________________

# __________________________________________________________________________
# 0. SOME PRELIMINARIES:
# --------------------------------------------------------------------------
# 0.1. Setting the working directory right 
    # (automatically, as with the current script's location)
fileloc <- dirname(rstudioapi::getSourceEditorContext()$path)
setwd(fileloc)
    # Removing the fileloc string
rm(fileloc)
    # Setting locale
Sys.setlocale("LC_ALL", "English")
# --------------------------------------------------------------------------
# 0.2. Loading the necessary packages
library(tidyverse)                                       # for handling data
library(data.table)                                      # for handling data
library(rvest)                                           # for scraping data
# --------------------------------------------------------------------------
# 0.3. Clearing the workspace environment
rm(list = ls())
gc()
# quicker loading of the environment - up until section 6. included
# ----------------------------------
# load('Environment_uptil_EELJ_27_May.RData')

# __________________________________________________________________________
# 1. LOADING THE DATA:
# --------------------------------------------------------------------------
# 1.1. Extracting the data
file_name <- 'https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/USstatesCov19distancingpolicyBETA.csv'
    # Obtaining the file directly from the parent directory (so as to be as up to date as possible)
    # There are 14,388 observations as of Apr, 14
COVID_measures_df <- read.csv(url(file_name))
    # Removing the file location
rm(file_name)
# --------------------------------------------------------------------------
# 1.2. Obtaining the names of the counties in the U.S. by state
    # (as of 24/02/2021, there are 3,143 counties/county equivalents in the U.S. according to Wikipedia)
html_counties <- 'https://en.wikipedia.org/wiki/List_of_United_States_counties_and_county_equivalents'
    # Obtaining all tables from this wiki link
tables_html <- html_counties %>%
  read_html() %>%
  html_nodes('table')
    # After inspection, we observe that the first element in this list is what we are interested in

counties_df <- as.data.frame(html_table(tables_html[1], fill = T)) %>%
  select(2,1,3) %>%
  rename(State = State.or.equivalent,
         County = County.or.equivalent,
         Population2019 = Population..2019.estimate.)
    # The latter object gives us the counties by state
    # (and conveniently, the Wiki reports of their population according to the US Census Bureau)

# NOTICE: The state of Hawaii is written as Hawaiʻi, renaming it to avoid issues in the future:
counties_df$State[counties_df$State == 'Hawaiʻi'] <- 'Hawaii'

# --------------------------------------------------------------------------
# NEW CHANGES:
# Also removing the [d] and [e], etc. superscripts:
counties_df <- counties_df %>%
  mutate(County = str_replace_all(County, '\\[[:alpha:]+\\]', '')) %>%
  mutate(County = str_replace_all(County, '\\([:alpha:]+.*\\)', '')) %>%
  mutate(County = str_replace_all(County, '\\[[:digit:]+\\]', '')) %>%
  # mutate(County = str_replace_all(County, '\\,[:space:]+[:alpha:]+', '')) %>%
  mutate(County = str_replace_all(County, '\\,.*', '')) %>%
  mutate(County = str_replace_all(County, "[^'[:^punct:]]", ' ')) %>%
  mutate(County = str_squish(County))
  

# counties_with_punctuation <- str_subset(counties_df$County, '[:punct:]')
# # all good - only apostrophes left, and no multiple spaces!

# ---------------------------------------------------------------------------
# test <- counties_df %>%
#   distinct(State, County, .keep_all = T)
# 
# View(counties_df %>% filter(!(Population2019 %in% test$Population2019)))
# ---------------------------------------------------------------------------
# (observing which are the duplpicates to fix this)

counties_df <- counties_df %>%
  mutate(Population2019 = str_replace_all(Population2019, '\\,', '')) %>%
  mutate(Population2019 = as.numeric(Population2019)) %>%
  # transforming the population into number!
  group_by(State, County) %>%
  mutate(Population2019_new = sum(Population2019)) %>%
  ungroup() %>%
  dplyr::select(-Population2019) %>%
  rename(Population2019 = Population2019_new) %>%
  distinct()

# counties_df %>% 
#   filter(Population2019 != Population2019_new) %>%
#   View()
# # all good

# NOTE: Have to make similar changes to the AppliesTo variable in the final COVID reviewed df!

rm(html_counties, tables_html)
gc()
    # Cleaning the workspace from the auxiliary objects
# --------------------------------------------------------------------------
# 1.3. Obtaining the names of the states considered in the COVID-19 Policy data
file_name <- 'https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/00_archive/state_id.csv'
    # Obtaining the file directly from the parent directory (so as to be as up to date as possible)
states_df <- read.csv(url(file_name))
    # Removing the file location
rm(file_name)


# __________________________________________________________________________
# 2. SOME DATA CLEANING OF THE MEASURES DATA FRAME:
# --------------------------------------------------------------------------
# 2.1. Fixing the date variables
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# 2.1.1. Initial works
COVID_measures_df_n <- COVID_measures_df %>%
  mutate(DateIssued = lubridate::ymd(DateIssued),
         DateEnacted = lubridate::ymd(DateEnacted),
         DateExpiry = lubridate::ymd(DateExpiry),
         DateEnded = lubridate::ymd(DateEnded),
         LastUpdated = lubridate::ymd(LastUpdated))
  # warning: one failed to parse in the DateExpiry column
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# 2.1.2. Performing check-ups to identify which entry causes the trouble
COVID_measures_df_checkup <- COVID_measures_df %>%
  filter(!is.na(DateExpiry)) %>%
  mutate(DateExpiry = lubridate::ymd(DateExpiry)) %>%
  arrange(DateExpiry)
tail(select(COVID_measures_df_checkup, c(DateExpiry, PolicySourceID)))
  # it is the Washington0086 policy
COVID_measures_df$DateExpiry[COVID_measures_df$PolicySourceID == 'Washington0086']
  # INDEED! the expiry date there is coded as 29th of Feb, 2021 - non-existent date
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# 2.1.3. Changing the troubling entry to the 28/02/2021
COVID_measures_df$DateExpiry[COVID_measures_df$PolicySourceID == 'Washington0086'] <- '20210228'
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# 2.1.4. Final modification of the date variables in the original COVID_measures data frame
COVID_measures_df <- COVID_measures_df %>%
  mutate(DateIssued = lubridate::ymd(DateIssued),
         DateEnacted = lubridate::ymd(DateEnacted),
         DateExpiry = lubridate::ymd(DateExpiry),
         DateEnded = lubridate::ymd(DateEnded),
         LastUpdated = lubridate::ymd(LastUpdated))
# --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --
# 2.1.5. Removing the auxiliary data frames
rm(COVID_measures_df_n, COVID_measures_df_checkup)
gc()
# --------------------------------------------------------------------------
# 2.2. Replacing '' by NA in the measures data frame
COVID_measures_df[COVID_measures_df == ''] <- NA
# --------------------------------------------------------------------------
# 2.3. Exploring and fixing the StatePolicies variable
unique(COVID_measures_df$StatePolicy)
  # there are 22 'distinct' values of this variable
  # however, four sets of the values coincide:
  # 'TravelRestrictEntry' and 'TravelRestrictEntry ' ;
  # 'TravelRestrictIntra' and 'TravelRestrictIntra ' ;
  # 'SchoolHealthSafety' and 'SchoolHealthSafety ' ;
  # 'BusinessMask' and 'BusinessMask ' .
  # will truncate these with the following simple mutation:
COVID_measures_df <- COVID_measures_df %>%
  mutate(StatePolicy = gsub(' ', '', StatePolicy, fixed = T))
length(unique(COVID_measures_df$StatePolicy)) 
  # this leaves us with 18 unique policy measures 
  # (as indicated in the data documentation pdf)
# --------------------------------------------------------------------------
# 2.4. Selecting only data which is available at the county-level
# # [as of 14/04/2021]
COVID_measures_df %>%
  filter(!(is.na(AppliesTo))) %>%
  nrow()
  # There are 5,542 rows (out of the initial 14,388) to include explicit information for counties
  # However, there are measures that apply to all counties (state-wide geo measures, SWGeo == 1)!
COVID_measures_df %>%
  filter(!(is.na(AppliesTo)) | SWGeo == 1) %>%
  nrow()
  # Allowing for policy measures that apply to all counties, too, we have 14,143 entries from the 
  # original data set
COVID_measures_df %>%
  filter(ReReviewed == 1) %>%
  nrow()
# There are 13,334 rows (out of the initial 14,388) to have been re-reviewed
COVID_measures_df %>%
  filter(ReReviewed == 1 & (!(is.na(AppliesTo)) | SWGeo == 1)) %>%
  nrow()
  # From the re-reviewed data, there are 13,315 out of the 13,334 rows to include some information 
  # on the county-level.
# View(filter(COVID_measures_df, ReReviewed == 1 & is.na(AppliesTo) & SWGeo != 1))
  # There are 19 entries for which a review of data has been made, but neither SWGeo holds,
  # nor is AppliesTo present -> yet these measures all appear to affect the whole state population
  # so they also convey info on the state-level!
COVID_measures_df_REVIEWED <- COVID_measures_df %>%
  filter(ReReviewed == 1)
  # filtering out non-reviewed entries to make work cleaner and cohesive
  # this leaves us with 14,388 observations of 40 variables
  # * * *
  # also, there are only 16 state policy variables considered there as discussed in the
  # data documentation pdf
  # to see this, run:
  # length(unique(COVID_measures_df_REVIEWED$StatePolicy))
# --------------------------------------------------------------------------
# 2.5. Making a couple of further small manipulations to the data:
# Ends and extends typically refer to changes in the value of the policy measure
# and not in any other dimensions (like location). However, this does not hold for the
# case where a policy ends for specific locations only! Will change this to facilitate the
# coding of the Ends-Extends function.


# EXTRA STEP: Normalizing the input for county names (similarly to the counties_df manipulation)
vec_counties_mentioned <- unlist(strsplit(unlist(COVID_measures_df_REVIEWED$AppliesTo)[!(is.na(unlist(COVID_measures_df_REVIEWED$AppliesTo)))],
                           ', ')) %>%
  unique() %>%
  str_subset('[:punct:]')
  # these are the counties' names to contain punctuation

vec_counties_mentioned_changed <- vec_counties_mentioned %>%
  str_replace_all('\\([:alpha:]+.*\\)', '') %>%
  # removes text in brackets along with the brackets themselves
  str_replace_all("[^'[:^punct:]]", ' ') %>%
  str_squish()
  
# vec_counties_mentioned_changed
# # all good

for(i in vec_counties_mentioned){
  loc_temp <- which(vec_counties_mentioned == i)
  COVID_measures_df_REVIEWED$AppliesTo <- str_replace_all(COVID_measures_df_REVIEWED$AppliesTo, 
                                   i, 
                                   vec_counties_mentioned_changed[loc_temp])
  COVID_measures_df_REVIEWED$AppliesTo <- str_replace_all(COVID_measures_df_REVIEWED$AppliesTo, 
                                   '\\([:alpha:]+.*\\)',
                                   '')
  rm(loc_temp)
}
COVID_measures_df_REVIEWED$AppliesTo <- str_squish(COVID_measures_df_REVIEWED$AppliesTo)
rm(i); gc()


# unlist(strsplit(unlist(COVID_measures_df_REVIEWED$AppliesTo)[!(is.na(unlist(COVID_measures_df_REVIEWED$AppliesTo)))],
#                                           ', ')) %>%
#   unique() %>%
#   str_subset('[:punct:]')
# # these are the counties' names to contain punctuation LEFT: NONE (as desired)

rm(vec_counties_mentioned, vec_counties_mentioned_changed); gc()


# --------------------------------------------------------------------------
# 2.6. Saving these two files into the working directory for simpler loading in the future
save(counties_df, states_df, COVID_measures_df, COVID_measures_df_REVIEWED,
     file = 'saved_data_frames.RData')
# --------------------------------------------------------------------------
# 2.7. Loading the data directly
# WARNING: this does not guarantee up-to-date information, one needs to check the date of the file
#          beforehand and if it falls before an update noted on the GitHub source folder,
#          better to run the above codes instead to ensure completeness of data at this point of time

# UNCOMMENT IF DO NOT WISH TO RUN THE ABOVE CODES BUT SIMPLY WISH TO LOAD PRE-SAVED DATA:
# rm(list = ls())
# gc()
# load('saved_data_frames.RData')


# __________________________________________________________________________
# 3. LOADING THE FUNCTION THAT MAKES AN EMPTY DATA FRAME FOR EACH STATE:
# --------------------------------------------------------------------------
source('EmptyDfForState.R')
# testing if it works: (it does as of Apr, 14)
# test_empty_df <- function_empty_df_for_state(state_name = 'California', long = F)
# --------------------------------------------------------------------------


# __________________________________________________________________________
# 4. LOADING THE FUNCTION THAT MAKES A SEQUENCE OF RELATED POLICIES WITHIN A
#    SINGLE STATE:
# 
# /NOTE: this is needed since in the next few steps, we will manipulate out
#        the cases of extending/ending policies, and the sequences of
#        original policies saved in this step would serve as a proof-check
#        source of PID-s whenever needed./
# --------------------------------------------------------------------------
source('SeqOfStatePolicies.R')
# NOTICE: Specific attention on the Ends-type of policy and the DateEnded
nrow(filter(COVID_measures_df_REVIEWED, !(is.na(DateEnded))))
  # there are 4372 instances of policies where the date ended is present
nrow(filter(COVID_measures_df_REVIEWED, !(is.na(Ends))))
  # and only 787 instances of policies where there is an explicit policy
  # that ends a previous one

all_states_considered <- unique(COVID_measures_df_REVIEWED$StateName)
  # these are the 51 'states': 50 states + the Fed. District of Columbia
POLICY_CHAINS_all_states_df <- function_seq_of_policies_for_state(state_name = all_states_considered[1])
  # this is for Alabama only
for(i in 2:length(all_states_considered)){
  POLICY_CHAINS_all_states_df <- rbind(POLICY_CHAINS_all_states_df,
                                       function_seq_of_policies_for_state(state_name = all_states_considered[i]))
}
POLICY_CHAINS_all_states_df <- POLICY_CHAINS_all_states_df %>%
  mutate(StatePostal = str_sub(Chain_start, start = 1L, end = 2L)) %>%
  select(StatePostal, everything())
length(unique(POLICY_CHAINS_all_states_df$StatePostal))
  # 51 states, as expected!
  # as opposed to the 43 states present in the data in March
save(POLICY_CHAINS_all_states_df, file = 'saved_policy_chains_all_states.RData')
  # saving for future use / faster extraction
# load('saved_policy_chains_all_states.RData')
# --------------------------------------------------------------------------


# __________________________________________________________________________
# 5. LOADING THE FUNCTION THAT CLEANS THE DATA ON STATES FOR A SPECIFIC
#    POLICY BY REMOVING SAFELY THE ENDS/EXTENDS VARIABLES:
# --------------------------------------------------------------------------
# input: the policy measure and state of interest
# output: a data frame with the instances of the policy measure on the state
#         of interest to NOT include the ending/extending entries separately
#         (i.e. a simpler data frame with fewer rows)

# reasoning: extends and ends are always time-related, as stated in the data
#           documentation file
source('ENDS_EXTENDS_function.R')
# --------------------------------------------------------------------------


# SIDE NOTE: Sources of heterogeneity now could be divided into several
#            groups, which can in turn be categorized into the two broader 
#            categories of 'changes_in_locality' and 'expansion_or_shrink'


# __________________________________________________________________________
# 6. LOADING THE FUNCTION THAT DETECTS THE SOURCE(S) OF HETEROGENEITY IN AN
#    INSTANCE OF EXPANDS/EASES/LEAVES/JOINS POLICY:
# --------------------------------------------------------------------------
# input: the policy measure and state of interest
# output: a data frame with the information on expand/ease/leave/join
#         policies and the heterogeneities mechanically found
# types of heterogeneities included in the output data frame:
#    o more_strict: 1 if it is an expansion/joins policy measure
#                   0 if it is an easing/leave policy measure
#    o changes in mandate / state-wide measures / curfew hours / 
source('EELJ_function.R')

# creating a large df with the result from applying this function on all 
# possible states/measures separately (then rbind):
# --- last done 27/05/2021
# --- uncomment lines below to re-run:

# first_state_df <- COVID_measures_df_REVIEWED$StateName[1]
# first_policy_df <- COVID_measures_df_REVIEWED$StatePolicy[1]
# 
# EELJ_all_states_policies_df <- EELJ_function(state_name = first_state_df,
#                                              policy_measure = first_policy_df)
# 
# for(p in unique(COVID_measures_df_REVIEWED$StatePolicy)[-1]){
#   temp_eelj_df <- EELJ_function(state_name = first_state_df,
#                                 policy_measure = p)
#   if((nrow(EELJ_all_states_policies_df) != 0) & nrow(temp_eelj_df) != 0){
#     EELJ_all_states_policies_df <- rbind(EELJ_all_states_policies_df,
#                                            temp_eelj_df)
#   } else{
#       if(nrow(temp_eelj_df) != 0){
#         EELJ_all_states_policies_df <- temp_eelj_df
#       }
#     }
#   rm(temp_eelj_df)
# }
# # this gives the df for Alabama and all possible policy measures
# # as of mid-April, there are 25 observations on 25 var-s
# 
# # Adding the remaining states/policies combinations:
# for(i in unique(COVID_measures_df_REVIEWED$StateName)[-1]){
#   for(j in unique(COVID_measures_df_REVIEWED$StatePolicy)){
#     temp_eelj_df <- EELJ_function(state_name = i,
#                                   policy_measure = j)
#     if(nrow(temp_eelj_df) != 0){
#       EELJ_all_states_policies_df <- rbind(EELJ_all_states_policies_df,
#                                            temp_eelj_df)
#     }
#     rm(temp_eelj_df)
#   }
# }
# 
# rm(first_state_df, first_policy_df, i, j, p)
# gc()
# 
# -------------------------------------------------
# # Comparing if consistent with previous data saved:
# new <- EELJ_all_states_policies_df
# load('saved_EELJ_all_states_policies.RData')
# old <- EELJ_all_states_policies_df
# dplyr::all_equal(old, new)
# -------------------------------------------------
# # Making choices based on that output:
# EELJ_all_states_policies_df <- new
# rm(old, new); gc()
# 
# # Saving the data frame for future use:
# save(EELJ_all_states_policies_df,
#      file = 'saved_EELJ_all_states_policies.RData')
# 
# # environment saved up until here:
# # save.image(file = 'Environment_uptil_EELJ_27_May.RData')
# # save.image(file = 'Environment_uptil_EELJ_31_May.Rdata')
# # save.image(file = 'Environment_uptil_EELJ_1_June.RData')


# Some EDA for the EELJ df which captures heterogeneity across policy measures:
EELJ_all_states_policies_df %>%
  filter(ch_uncoded == 1) %>%
  nrow()
  # there are 729 out of all 4,743 rows in the EELJ df which capture some uncoded
  # changes -> need to be looked at manually one-by-one depending on policy coding
  #            notes (potentially)

# 85% of data could be manipulated automatically using the FILL_function sourced
# and introduced below.


# --------------------------------------------------------------------------


# __________________________________________________________________________
# 7. ADDING THE POLICY TYPE FUNCTION/CHUNK OF CODE:
# --------------------------------------------------------------------------
source('PolicyType.R')
# works for the 16 policy measure variables as of mid-May, 2021

# --------------------------------------------------------------------------

# __________________________________________________________________________
# 8. SOME EDA FOR THE HETEROGENEITY IN THE ABOVE POLICY MEASURES:
# --------------------------------------------------------------------------
source('Simple_EDA.R')
df_EDA <- simple_EDA_fun(main_df = COVID_measures_df_REVIEWED)
# works for the 16 policy measure variables as of mid-May, 2021
# shows the percentage of instances for each policy, where
#   o the instances are mandatory
#   o the instances are applied to state-wide
#   o % of the non-state-wide cases with info on the locality of the measure

# One can notice that in 100% of the cases for the EmergDec variable,
# the policy applies to the whole state, and is always mandatory.
#   o As expected, no instances of this policy at the county level;
#   o Further, no cases of easing this policy:
COVID_measures_df_REVIEWED %>%
  filter(StatePolicy == 'EmergDec', !(is.na(Eases))) %>%
  nrow()
#   o Two cases of expansion of the policy in the data set:
#     these represent extreme/disaster emergency declarations
COVID_measures_df_REVIEWED %>%
  filter(StatePolicy == 'EmergDec', !(is.na(Expands))) %>%
  nrow()

# Following the above EDA on the EmergDec policy measure, a simple function 
# has been designated for it to obtain the final resp. data frame. 
# [see next section]
# --------------------------------------------------------------------------


# __________________________________________________________________________
# 9. CREATING THE DUMMY VARIABLES FOR EACH MANDATORY STATE POLICY OVER TIME 
#    BY COUNTIES:
# --------------------------------------------------------------------------
# 8.1. EmergDec
#      [Loading the function which generates the data frames for each state]
source('EmergDec.R')
  # loading the function that obtains the mandatory EmergDec dummy variable 
  # value
df_EmergDec <- EmergDec_function_for_state(state_name = 'Alabama')
for(i in 2:length(all_states_considered)){
  df_EmergDec <- bind_rows(df_EmergDec, 
                           EmergDec_function_for_state(state_name = all_states_considered[i]))
}

# new <- df_EmergDec
# load('saved_EmergDec_all_states.RData')
# old <- df_EmergDec
# dplyr::all_equal(new, old)
#
# df_EmergDec <- new
# rm(new, old); gc()

# Saving this df into a separate file:
save(df_EmergDec, file = 'saved_EmergDec_all_states.RData')



source('aux_fun_chains_START_fill.R')
source('FILL_function.R')

# ------------------------------------------------------------------------------------
# 8.2. SchoolClose (per request)

# Generating the School_Close data frame as requested:
# (FOR ALL STATES/COUNTIES TOGETHER)

# 1. At the County level for all states together

# A couple of notes:
# - initial issue with Pennsylvania -> need to fix -> DONE
# - function now runs for all states: check with < length(unique(df_SchoolClose$State)) >
#   after the loop below
# - some missing observations for Utah, however -> need to fix -> DONE


df_SchoolClose <- FILL_function(data_measures = COVID_measures_df_REVIEWED,
                                county_data = counties_df,
                                state_name = 'Alabama',
                                policy_measure = 'SchoolClose',
                                not_vec = c(0, NA))

for(i in 2:length(all_states_considered)){
  df_SchoolClose <- bind_rows(df_SchoolClose,
                              FILL_function(data_measures = COVID_measures_df_REVIEWED,
                                            county_data = counties_df,
                                            state_name = all_states_considered[i],
                                            policy_measure = 'SchoolClose',
                                            not_vec = c(0, NA)))
}

# Saving into an RData object:
# save(df_SchoolClose, file = 'saved_SchoolClose_all_states_1_June.Rdata')

# -------------------------------------------------------------------------------
# Subsetting for mandatory only:

# table(df_SchoolClose$mandate)
# # there are some non-mandatory measures

# table(df_SchoolClose$only_non_vaccinated_ppl)
# # no heterogeneity across this variable
# summary(df_SchoolClose)
# # and also across many others (apart from the 'important ones')

df_SchoolClose_mandate <- df_SchoolClose %>%
  mutate(policy_measure_var_main = ifelse(is.na(mandate) | mandate == 0,
                                          policy_type_function('SchoolClose')[2],
                                          policy_measure_var_main),
         policy_measure_var_sec = ifelse(is.na(mandate) | mandate == 0,
                                         policy_type_function('SchoolClose')[2],
                                         policy_measure_var_sec)) %>%
  # NA-s means these dates occur after the last policy ended => replace with usual value
  # non-mandatory in THIS data set also translates to non-existent policy
  dplyr::select(1:5) %>%
  # selecting the relevant variables only
  rename(Public_Schools = policy_measure_var_main,
         Private_Schools = policy_measure_var_sec)

table(df_SchoolClose_mandate$Public_Schools)

# save(df_SchoolClose_mandate, file = 'SchoolClose_COUNTY_lvl.Rdata')


# save(df_SchoolClose_County_lvl_Mandatory_only, 
#      file = 'SchoolClose_County_Mandate_PRELIM.RData')

# load('SchoolClose_County_Mandate_PRELIM.RData')
# for past version of the file

df_SchoolClose_mandate_STATE <- df_SchoolClose_mandate %>%
  gather(schools_type, value, -c(1:3)) %>%
  mutate(value = factor(value,
                        levels = c('InPersonAllowed',
                                      'LimitedInPerson',
                                      'NoInPerson'),
                        labels = c('0', '0.5', '1'))) %>%
  spread(schools_type, value)

county_data <- county_data %>%
  group_by(State) %>%
  mutate(StatePopulation2019 = sum(Population2019)) %>%
  ungroup()

df_SchoolClose_mandate_STATE <- df_SchoolClose_mandate_STATE %>%
  # adding the population as of 2019 from the county df:
  left_join(county_data, by = c('State', 'County')) %>%
  # obtaining for what percentage of the population within the whole state
  # measures for public schools applied:
  mutate(county_pop_frac_of_state_pop = Population2019/StatePopulation2019)

# group by date and state now
df_SchoolClose_mandate_STATE <- df_SchoolClose_mandate_STATE %>%
  mutate(Public_Schools = as.numeric(as.character(Public_Schools))) %>%
  mutate(Private_Schools = as.numeric(as.character(Private_Schools))) %>%
  # transforming to numeric to allow for multiplication
  mutate(private_sch_frac_pop = Private_Schools*county_pop_frac_of_state_pop,
         public_sch_frac_pop = Public_Schools*county_pop_frac_of_state_pop) %>%
  group_by(State, Date) %>%
  summarize(frac_state_pop_with_closed_private_sch = sum(private_sch_frac_pop),
         frac_state_pop_with_closed_public_sch = sum(public_sch_frac_pop))

# save(df_SchoolClose_mandate_STATE, file = 'SchoolClose_STATE_lvl.Rdata')


#                                   END OF MAIN PART OF CODES
# -------------------------------------------------------------------------------------------------  
# =================================================================================================
# -------------------------------------------------------------------------------------------------  

# NOTE: For the remaining 15 state policy measures, will rely on the generalized 
#       FILL_function.
#      - in fact, it can also be used with EmergDec, but it has been considered
#        a separate case (seen above) for brevity

EELJ_all_states_policies_df %>%
  filter(ch_SWPop == 1) %>%
  nrow()
  # there are only 16 rows where changes in population occur
COVID_measures_df_REVIEWED %>%
  filter(SWPop == 0) %>%
  nrow()
  # 256 rows from the 13,334 observations of the original table

# ---> these need to be checked manually as the changes are coded within the policy coding notes

# IMPORTANT NOTE:
# Heterogeneity arises on multiple dimensions!
# --- these can be found in the 9:25 columns (totalling 17) in the EELJ
#     large df -> the columns which start with ch_

# --- THESE WILL BE GROUPED INTO 5 CATEGORIES FOR BREVITY:
#     o TIME CHANGES [related to the three ch_curfew variables]
#     o LOCATION CHANGES
#     o GROUPS OF POPULATION AFFECTED CHANGES 
#       [vaccinated vs. not considered only - see argument above]
#     o LIMITS OF PPL GATHERINGS
#     o MANDATORY OR NOT

# For these 5 groups of heterogeneity considered, loading the 5 auxiliary functions:


# --------------------------------------------------------------------------
# sourcing/testing other important codes:

# test_empty_df <- function_empty_df_for_state(state_name = 'California', long_output = F)
# makes an empty df for each possible state - California tested here

# test_ends_extends <- ENDS_EXTENDS_function(state_name = 'California', policy_measure = 'StayAtHome')
# makes a df for a state and a policy measure, having simplified the ending/extending rows

# test_eelj <- EELJ_function(state_name = 'California', policy_measure = 'SchoolClose')
# looks for where the policy changes are and which category these changes fall into

# test_EmergDec <- EmergDec_function_for_state(state_name = 'Alabama')
# obtains the mandatory EmergDec dummy variable value

# --------------------------------------------------------------------------




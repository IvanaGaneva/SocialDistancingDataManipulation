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

# __________________________________________________________________________
# 1. LOADING THE DATA:
# --------------------------------------------------------------------------
# 1.1. Extracting the data
file_name <- 'https://raw.githubusercontent.com/COVID19StatePolicy/SocialDistancing/master/data/USstatesCov19distancingpolicyBETA.csv'
    # Obtaining the file directly from the parent directory (so as to be as up to date as possible)
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
# # [as of 01/03/2021]
COVID_measures_df %>%
  filter(!(is.na(AppliesTo))) %>%
  nrow()
  # There are 4,142 rows (out of the initial 11,892) to include explicit information for counties
  # However, there are measures that apply to all counties (state-wide geo measures, SWGeo == 1)!
COVID_measures_df %>%
  filter(!(is.na(AppliesTo)) | SWGeo == 1) %>%
  nrow()
  # Allowing for policy measures that apply to all counties, too, we have 11,478 entries from the 
  # original data set
COVID_measures_df %>%
  filter(ReReviewed == 1) %>%
  nrow()
# There are 9,773 rows (out of the initial 11,892) to have been re-reviewed
COVID_measures_df %>%
  filter(ReReviewed == 1 & (!(is.na(AppliesTo)) | SWGeo == 1)) %>%
  nrow()
  # From the re-reviewed data, there are 9,758 out of the 9,773 rows to include some information 
  # on the county-level.
View(filter(COVID_measures_df,
            ReReviewed == 1 & is.na(AppliesTo) & SWGeo != 1))
  # There are 15 entries for which a review of data has been made, but neither SWGeo holds,
  # nor is AppliesTo present -> yet these measures all appear to affect the whole state population
  # so they also convey info on the state-level!
COVID_measures_df_REVIEWED <- COVID_measures_df %>%
  filter(ReReviewed == 1)
  # filtering out non-reviewed entries to make work cleaner and cohesive
  # this leaves us with 9,773 observations of 39 variables
  # * * * 
  # also, there are only 16 state policy variables considered there as discussed in the
  # data documentation pdf
# --------------------------------------------------------------------------
# 2.5. Saving these two files into the working directory for simpler loading in the future
save(counties_df, states_df, COVID_measures_df, COVID_measures_df_REVIEWED,
     file = 'saved_data_frames.RData')
# --------------------------------------------------------------------------
# 2.6. Loading the data directly
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
  # there are 985 instances of policies where the date ended is present
nrow(filter(COVID_measures_df_REVIEWED, !(is.na(Ends))))
  # and only 356 instances of policies where there is an explicit policy
  # that ends a previous one

all_states_considered <- unique(COVID_measures_df_REVIEWED$StateName)
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
  # 43 states, as expected!
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
# --------------------------------------------------------------------------


# __________________________________________________________________________
# 7. CREATING THE DUMMY VARIABLES FOR EACH MANDATORY STATE POLICY OVER TIME 
#    BY COUNTIES:
# --------------------------------------------------------------------------
# 7.1. EmergDec
COVID_measures_df_REVIEWED %>%
  filter(StatePolicy == 'EmergDec', !(is.na(AppliesTo))) %>%
  nrow()
  # as discussed in the data documentation file, this is a state-wide policy
  # measure and there are no cases where the policy applies to specific
  # counties only (as expected)
COVID_measures_df_REVIEWED %>%
  filter(StatePolicy == 'EmergDec', StateWide != 1) %>%
  nrow()
  # further proof on the above statement
COVID_measures_df_REVIEWED %>%
  filter(StatePolicy == 'EmergDec', !(is.na(Eases))) %>%
  nrow()
  # zero eases of this policy in the data set (as of March, 1st)
COVID_measures_df_REVIEWED %>%
  filter(StatePolicy == 'EmergDec', !(is.na(Expands))) %>%
  nrow()
  # two cases of expansion of the policy in the data set -> these are extreme
  # or disaster & emergency declarations
source('EmergDec.R')
  # loading the function that obtains the mandatory EmergDec dummy variable 
  # value
# --------------------------------------------------------------------------



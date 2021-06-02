# SocialDistancingDataManipulation
 
## Covid 19 State Policy Data Cleaning, Exploration & Construction of a Set of Metrics at the County Level

### Feb-Mar, 2021
### Ivana Ganeva

#### Data Source:
Nancy Fullman, Bree Bang-Jensen, Grace Reinke, Beatrice Magistro, Rachel Castellano, Megan Erickson, Rebecca Walcott, Carolyn Dapper, Kenya Amano, John Wilkerson, and Christopher Adolph. "State-level social distancing policies in response to COVID-19 in the US". Version 1.117, February 17, 2021. http://www.covid19statepolicy.org

#### Updates (April onwards):

##### 26/04
o Adjusted the codes to work with the additional information on the District of Columbia which was added in the last data set updates. 
o Fixed minor issues and improved the policy_chains function.
o Began working on introducing the VaccineExempt dimensionality to the data (which was added this month by the authors).
o Improvements made on the FILL_function.
o Need to complete first the aux_fun_chains_START_fill.
o Saved all relevant data sets (up-to-date) in the Environment_uptil_EELJ_26_April.RData file for faster loading.

##### 30/04
o Completed the aux_fun_chains_START_fill function. It fills the final data frame output (per state, per policy type) with the information on all starting policies.
o Improvements made on the FILL_function.
o Need to fix the issue with the primary and the secondary variable on the SchoolClose measure.
o Made an exhaustive list of heterogeneity sources.
o Improvements made on the EDA function.
o Need to adjust output for policy-state combinations so that there are the 'normal-world' values for dates before the policy chain commencements in the respective measures.

##### 05/05
o Found an issue with the encoding of the state of Hawaii: discrepancy between Wiki/original data and the one in the data set (fixed).
o Improved and finalized the ENDS_EXTENDS function.
o Improved and finalized the EELJ function.
o Ran the functions on all state-policies combinations and saved the outputs in merged data frames.
o Saved all relevant data sets (up-to-date) in the Environment_uptil_EELJ_05_May.RData file for faster loading.

##### 11/05
o Completed the time dimensionality in the FILL_function.
o Need to add remaining cases of heterogeneity, following the example set in the aux_fun_chains_START_fill function.
o After the completion of the FILL_function, probably need to split it into smaller chunks for brevity/clarity.
o Completed the introduction of the VaccineExempt dimensionality.

##### 16/05
o Completed the FILL_function.
o Fixed minor issues with the functions of aux_fun_chains_START_fill, and the functions of EELJ.
o Re-ordered parts of the code of empty_df function in order to avoid potential issues in the future.
o A couple of notes on filtering in the future: potentially need to filter on end_date_obtained, and issues in the policy_chains data frame for all policies and states.
o Addressed the issue of uncoded policy changes, began initial work on text-mining in order to assimilate these instances: at most 15% of data is uncoded.

##### 27/05
o Improvements made on the SchoolClose policy measure (needs to be completed), text mining, analyze and structure all possible cases.
o Working on the uncoded policy changes cases - improvements made where other variables give necessary information. (=> <15% uncoded data)
o Began structuring the functions within a single R project.
o Improvements made on the plotting function (maps/timeline of counties vs. measures over time).
o Found a minor issue with the coding/input of some counties in the data set (e.g. 'St Clair' vs. 'St. Clair'): need to fix this.
o Improvements made on the leaves & joins incidences (simultaneous): this needs to be completed.
o Began working on the aggregation part (simpler than the county level, equivalent to filtering for SWGeo == 1 in the *transformed* output).
o Saved all relevant data sets (up-to-date) in the Environment_uptil_EELJ_27_May.RData file for faster loading.

##### 31/05
o Completed the text mining process for the School Close variable.
o Fixed the issues with the counties' punctuation; added a couple of text-mining chunks of codes.
o Fixed minor issues with the main FILL_function.
o Saved the SchoolClose data set at the county level within the SchoolClose_COUNTY_lvl.RData file, as per request.

##### 01/06
o Added aggregation of the SchoolClose variable at the State level, based on what fraction of the population is affected by the measures implemented at the county-level.
o Saved the SchoolClose data set at the state level within the SchoolClose_STATE_lvl.RData file, as per request.
o Some important notes for the transferring between categorical to numeric values: 
   - adopting the notion that 'NoInPerson' translates to 1 (policy being strictly implemented);
   - likewise 'LimitedInPerson' is set to equal 0.5 (this could be optimized with further text mining, somewhat arbitrary atm);
   - lastly, 'InPersonAllowed' means that the restrictive policy measure is not implemented at the time, i.e. giving it a value of 0;
   - using the official Wiki data for the counties' population as of 2019 (automatically downloaded).

#### Generalized list of the things which need improvement/attention:
_____________________________________________________________________________________________

Task/Issue | STATUS
| :--- | ---:
District of Columbia, counties? | DONE
VaccineExempt dimensionality | DONE
Fill for dates prior to first measure in a policy set being introduced | DONE
Encoding for Hawaii: it is seen as Hawai'i in the county_df originally | DONE
Finalize the aux_fun_chains_START_fill | DONE
Improve the FILL_function | DONE
Complete v.1 of FILL_function | DONE
SchoolClose: Public vs. Private, basic text mining | DONE
Fix minor issues with the policy_chains function | DONE
Finalize the EDA function | DONE
Ends/extends cases removed completely (assimilated) | DONE
Uncoded policy changes, text mining | IN PROGRESS
Alternatively, filtering for uncoded policy changes (15%) | IN PROGRESS
Filtering for end_date_obtained, or for others in the policy chains df | IN PROGRESS
(Interactive) plotting function: maps and timelines of policy changes | IN PROGRESS
Structuring the codes within a single project | IN PROGRESS
Simplifying/Splitting the FILL_function into smaller parts | IN PROGRESS
If necessary, add some rm(), gc() lines after that, memory-wise improvement | TBD
Bars/Restaurants/BusinessesClose: usual working hours, function file | TBD
Coding of St. Clair, Alabama: at some points it is with a dot | TBD
Check for similar issues, basic text mining (Jaccard Similarity?) | TBD
Cases of Leaves & Joins, should add these to uncoded changes porbably | TBD
Aggregation/Adding a filter for the state level instead | DONE
_____________________________________________________________________________________________

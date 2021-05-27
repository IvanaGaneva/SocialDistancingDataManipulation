# ========================================================================================

# List of things that need improvement/attention:

# -> Filtering for end_date_obtained ---------------------------------------------- TBD
# -> Filtering for uncoded changes (15% data) ------------------------------------- TBD
# -> Fill for dates prior to first measure in a policy set being introduced ------- DONE
# -> Encoding for Hawaii: it is seen as Hawai'i in the county_df originally ------- DONE
# -> District of Columbia, counties? ---------------------------------------------- DONE
# -> VaccineExempt dimensionality ------------------------------------------------- DONE
# -> Create a function with info of up-to-date variables/categories --------------- TBD
# -> SchoolClose: Public vs. Private, basic text mining --------------------------- ALMOST 
# --------> (see FILL_function codes)
# -> Bars/Restaurants/BusinessesClose: usual working hours, function file --------- TBD
# -> Coding of St. Clair, Alabama: at some points it is with a dot ---------------- TBD
# -> Check for similar issues, basic text mining (Jaccard Similarity?) ------------ TBD
# -> Case of Ends/Extends --------------------------------------------------------- DONE
# -> Cases of Leaves & Joins, should add these to uncoded changes porbably -------- TBD
# --------> e.g. run View(filter(COVID_measures_df_REVIEWED, `ï..PID` == 'AK0008'))
# -> Simplify the FILL_function code: split into separate chunks for clarity ------ TBD
# -> If necessary, add some rm(), gc() lines after that, memory-wise improvement -- TBD
# -> Generate the plotting function code 00---------------------------------------- TBD
# -> Aggregate/add filter for the state-level ------------------------------------- TBD
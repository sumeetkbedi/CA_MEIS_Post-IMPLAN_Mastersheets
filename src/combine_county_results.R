## Code for reading in county IMPLAN data and combining it into a single Excel file ##

## ECONOMIC INDICATORS BY IMPACT ##
# Read in a list of all these files based on their directory
econ_indic_files <- list.files(file.path(implan_res_c, econ_indic_path), xlsx_pat)

# Create a list of county names as well
counties <- gsub(paste(year, ".+"), "", econ_indic_files)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- grep(inv, econ_indic_files)
reg_ind <- (1:length(econ_indic_files))[-inv_ind]

# Define null variables as empty dataframes to write data into from the for loop
inv_temp <- NULL
reg_temp <- NULL

# Run result_loop function to generate "regular" and "inverse" dataframes for economic indicators
reg_temp <- result_loop(reg_ind, reg_temp, counties, implan_res_c, econ_indic_path, econ_indic_files)
inv_temp <- result_loop(inv_ind, inv_temp, counties, implan_res_c, econ_indic_path, econ_indic_files)

# Change the "NAs" in the dataframes to be "Total"
reg_temp$Impact[is.na(reg_temp$Impact)] <- "Total"
inv_temp$Impact[is.na(inv_temp$Impact)] <- "Total"

# Merge the 2 dataframes into 1, and turn NA values into 0's
econ_indic_counties <- merge(reg_temp, inv_temp, by = c("geo", "Impact"), all = TRUE) 
econ_indic_counties[is.na(econ_indic_counties)] <- 0

# Rename existing columns, and add in new ones
colnames(econ_indic_counties) <- c("county", "impact", "employment", "labor_income", "value_added", "output",
                                   "in_employment", "in_labor_income", "in_value_added", "in_output")
econ_indic_counties <- econ_indic_counties %>%
  mutate(total_employment = employment + in_employment, total_labor_income = labor_income + in_labor_income,
         total_value_added = value_added + in_value_added, total_output = output + in_output)

# Write into an Excel file - ALL DONE!
write.xlsx(econ_indic_counties, file.path(temp_path, paste0(year, "_econ_indicators_by_county.xlsx")))



## TAX RESULTS ##
# Read in a list of all these files based on their directory
tax_res_files <- list.files(file.path(implan_res_c, tax_res_path), xlsx_pat)

# Create a list of county names as well
counties <- gsub(paste(year, ".+"), "", tax_res_files)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- grep(inv, tax_res_files)
reg_ind <- (1:length(tax_res_files))[-inv_ind]

# Define null variables as empty dataframes to write data into from the for loop
inv_temp2 <- NULL
reg_temp2 <- NULL

# Run result_loop function to generate "regular" and "inverse" dataframes for tax results
reg_temp2 <- result_loop(reg_ind, reg_temp2, counties, implan_res_c, tax_res_path, tax_res_files)
inv_temp2 <- result_loop(inv_ind, inv_temp2, counties, implan_res_c, tax_res_path, tax_res_files)

# Change the "NAs" in the dataframes to be "Total"
reg_temp2$Impact[is.na(reg_temp2$Impact)] <- "Total"
inv_temp2$Impact[is.na(inv_temp2$Impact)] <- "Total"

# Merge the 2 dataframes into 1, and turn NA values into 0's
tax_res_counties <- merge(reg_temp2, inv_temp2, by = c("geo", "Impact"), all = TRUE) 
tax_res_counties[is.na(tax_res_counties)] <- 0

# Rename existing columns, and add in new ones
colnames(tax_res_counties) <- c("county", "impact", "sub_county_general", "sub_county_special_district", "county_rev", 
                                "state", "federal", "reg_total", "in_sub_county_general", "in_sub_county_special_district", 
                                "in_county_rev", "in_state", "in_federal", "in_total")
tax_res_counties <- tax_res_counties %>%
  mutate(total_sub_county_general = sub_county_general + in_sub_county_general, 
         total_sub_county_special_district = sub_county_special_district + in_sub_county_special_district,
         total_county_rev = county_rev + in_county_rev,
         total_state = state + in_state,
         total_federal = federal + in_federal,
         total = reg_total + in_total)

# Write into an Excel file - ALL DONE!
write.xlsx(tax_res_counties, file.path(temp_path, paste0(year, "_tax_results_by_county.xlsx")))



## INDUSTRIES BY IMPACT ##
# Read in a list of all these files based on their directory
output_indus_files <- list.files(file.path(implan_res_c, output_indus_path), xlsx_pat)

# Create a list of county names as well
counties <- gsub(paste(year, ".+"), "", output_indus_files)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- grep(inv, output_indus_files)
reg_ind <- (1:length(output_indus_files))[-inv_ind]

# Define null variables as empty dataframes to write data into from the for loop
inv_temp3 <- NULL
reg_temp3 <- NULL

# Run result_loop function to generate "regular" and "inverse" dataframes for output by industry
reg_temp3 <- result_loop(reg_ind, reg_temp3, counties, implan_res_c, output_indus_path, output_indus_files)
inv_temp3 <- result_loop(inv_ind, inv_temp3, counties, implan_res_c, output_indus_path, output_indus_files)

# Remove extra column and blank rows where impact is "industry display" from each dataframe
reg_temp3 <- reg_temp3 %>%
  select(-(...1)) %>%
  filter(!(Impact == ind_disp))
inv_temp3 <- inv_temp3 %>%
  select(-(...1)) %>%
  filter(!(Impact == ind_disp))

# Merge the 2 dataframes into 1
output_indus_counties <- merge(reg_temp3, inv_temp3, by = c("geo", "Impact"), all = TRUE) 

# Change column names, and make all the data as type numeric
colnames(output_indus_counties) <- c("county", "impact", "direct", "indirect", "induced",
                                     "reg_total", "in_indirect", "in_induced", "in_total")
output_indus_counties <- output_indus_counties %>%
  mutate_at(c("direct", "indirect", "induced", "reg_total", "in_indirect", "in_induced", "in_total"), as.numeric)

# Add in new columns
output_indus_counties <- output_indus_counties %>%
  mutate(total_direct = direct, 
         total_indirect = indirect + in_indirect,
         total_induced = induced + in_induced, 
         total = reg_total + in_total)

# Write into an Excel file - ALL DONE!
write.xlsx(output_indus_counties, file.path(temp_path, paste0(year, "_industry_output_by_county.xlsx")))



## EMPLOYMENT INDUSTRIES BY IMPACT ##
# Read in a list of all these files based on their directory
emp_indus_files <- list.files(file.path(implan_res_c, emp_indus_path), xlsx_pat)

# Create a list of county names as well
counties <- gsub(paste(year, ".+"), "", emp_indus_files)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- grep(inv, emp_indus_files)
reg_ind <- (1:length(emp_indus_files))[-inv_ind]

# Define null variables as empty dataframes to write data into from the for loop
inv_temp4 <- NULL
reg_temp4 <- NULL

# Run result_loop function to generate "regular" and "inverse" dataframes for economic indicators
reg_temp4 <- result_loop(reg_ind, reg_temp4, counties, implan_res_c, emp_indus_path, emp_indus_files)
inv_temp4 <- result_loop(inv_ind, inv_temp4, counties, implan_res_c, emp_indus_path, emp_indus_files)

# Remove extra column and blank rows where impact is "industry display" from each dataframe
reg_temp4 <- reg_temp4 %>%
  select(-(...1)) %>%
  filter(!(Impact == ind_disp))
inv_temp4 <- inv_temp4 %>%
  select(-(...1)) %>%
  filter(!(Impact == ind_disp))

# Merge the 2 dataframes into 1
emp_indus_counties <- merge(reg_temp4, inv_temp4, by = c("geo", "Impact"), all = TRUE) 

# Change column names, and make all the data as type numeric
colnames(emp_indus_counties) <- c("county", "impact", "direct", "indirect", "induced",
                                     "reg_total", "in_indirect", "in_induced", "in_total")
emp_indus_counties <- emp_indus_counties %>%
  mutate_at(c("direct", "indirect", "induced", "reg_total", "in_indirect", "in_induced", "in_total"), as.numeric)

# Add in new columns
emp_indus_counties <- emp_indus_counties %>%
  mutate(total_direct = direct, 
         total_indirect = indirect + in_indirect,
         total_induced = induced + in_induced, 
         total = reg_total + in_total)

# Write into an Excel file - ALL DONE!
write.xlsx(emp_indus_counties, file.path(temp_path, paste0(year, "_industry_employment_by_county.xlsx")))
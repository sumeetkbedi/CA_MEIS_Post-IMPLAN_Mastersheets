## Code for reading in county IMPLAN data and combining it into a single Excel file ##

## ECONOMIC INDICATORS BY IMPACT ##
# Read in a list of all these files based on their directory
econ_indic_files <- list.files(file.path(implan_res_c, econ_indic_path), csv_pat)

# Create a list of county names as well
counties <- gsub(year, "", 
                 gsub(econ_ind_csv, "",
                      gsub(inv, "", econ_indic_files), ignore.case = T))
counties <- str_trim(counties)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- grep(inv, econ_indic_files)
reg_ind <- (1:length(econ_indic_files))[-inv_ind]

# Define null variables as empty dataframes to write data into from the for loop
temp1_inv <- NULL
temp1_reg <- NULL

# Run result_loop function to generate "regular" and "inverse" dataframes for economic indicators
temp1_reg <- result_loop(reg_ind, temp1_reg, counties, implan_res_c, econ_indic_path, econ_indic_files)
temp1_inv <- result_loop(inv_ind, temp1_inv, counties, implan_res_c, econ_indic_path, econ_indic_files)

# Change the blanks in the dataframes to be "Total"
temp1_reg$Impact[temp1_reg$Impact == ""] <- "Total"
temp1_inv$Impact[temp1_inv$Impact == ""] <- "Total"

# Merge the 2 dataframes into 1, change columns to be numeric, and turn NA values into 0's
econ_indic_counties <- merge(temp1_reg, temp1_inv, by = c("geo", "Impact"), all = TRUE) 
econ_indic_counties[is.na(econ_indic_counties)] <- 0

# Rename existing columns, and run gsub loop code. Then, change columns to be numeric, and add in total columns
colnames(econ_indic_counties) <- c("county", "impact", "employment", "labor_income", "value_added", "output",
                                   "in_employment", "in_labor_income", "in_value_added", "in_output")
econ_indic_counties <- gsub_loop(econ_indic_counties, 3:10)

econ_indic_counties <- econ_indic_counties %>%
  mutate_at(3:10, ~ as.numeric(.)) %>%
  mutate(total_employment = employment + in_employment, total_labor_income = labor_income + in_labor_income,
         total_value_added = value_added + in_value_added, total_output = output + in_output)

# Write into an Excel file - ALL DONE!
write.xlsx(econ_indic_counties, file.path(temp_path, paste0(year, econ_c_file)))


## TAX RESULTS ##
# Read in a list of all these files based on their directory
tax_res_files <- list.files(file.path(implan_res_c, tax_res_path), csv_pat)

# Create a list of county names as well
counties <- gsub(year, "", 
                 gsub(tax_res_csv, "",
                      gsub(inv, "", tax_res_files), ignore.case = T))
counties <- str_trim(counties)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- grep(inv, tax_res_files)
reg_ind <- (1:length(tax_res_files))[-inv_ind]

# Define null variables as empty dataframes to write data into from the for loop
temp2_inv <- NULL
temp2_reg <- NULL

# Run result_loop function to generate "regular" and "inverse" dataframes for tax results
temp2_reg <- result_loop(reg_ind, temp2_reg, counties, implan_res_c, tax_res_path, tax_res_files)
temp2_inv <- result_loop(inv_ind, temp2_inv, counties, implan_res_c, tax_res_path, tax_res_files)

# Change the "NAs" in the dataframes to be "Total"
temp2_reg$Impact[temp2_reg$Impact == ""] <- "Total"
temp2_inv$Impact[temp2_inv$Impact == ""] <- "Total"

# Merge the 2 dataframes into 1, and turn NA values into 0's
tax_res_counties <- merge(temp2_reg, temp2_inv, by = c("geo", "Impact"), all = TRUE)
tax_res_counties[is.na(tax_res_counties)] <- 0

# Rename existing columns, and run gsub loop code. Then, change columns to be numeric, and add in total columns
colnames(tax_res_counties) <- c("county", "impact", "sub_county_general", "sub_county_special_district", "county_rev", 
                                "state", "federal", "reg_total", "in_sub_county_general", "in_sub_county_special_district", 
                                "in_county_rev", "in_state", "in_federal", "in_total")
tax_res_counties <- gsub_loop(tax_res_counties, 3:14)

tax_res_counties <- tax_res_counties %>%
  mutate_at(3:14, ~ as.numeric(.)) %>%
  mutate(total_sub_county_general = sub_county_general + in_sub_county_general, 
         total_sub_county_special_district = sub_county_special_district + in_sub_county_special_district,
         total_county_rev = county_rev + in_county_rev,
         total_state = state + in_state,
         total_federal = federal + in_federal,
         total = reg_total + in_total)

# Write into an Excel file - ALL DONE!
write.xlsx(tax_res_counties, file.path(temp_path, paste0(year, tax_c_file)))


## INDUSTRIES BY IMPACT ##
# Read in a list of all these files based on their directory
ind_output_files <- list.files(file.path(implan_res_c, ind_output_path), csv_pat)

# Create a list of county names as well
counties <- gsub(year, "", 
                 gsub(out_ind_csv, "",
                      gsub(inv, "", ind_output_files), ignore.case = T))
counties <- str_trim(counties)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- grep(inv, ind_output_files)
reg_ind <- (1:length(ind_output_files))[-inv_ind]

# Define null variables as empty dataframes to write data into from the for loop
temp3_inv <- NULL
temp3_reg <- NULL

# Run result_loop function to generate "regular" and "inverse" dataframes for output by industry
temp3_reg <- result_loop(reg_ind, temp3_reg, counties, implan_res_c, ind_output_path, ind_output_files)
temp3_inv <- result_loop(inv_ind, temp3_inv, counties, implan_res_c, ind_output_path, ind_output_files)

# Remove extra column and blank rows where impact is "industry display" from each dataframe
temp3_reg <- temp3_reg %>%
  select(-(X)) %>%
  filter(!(Impact == ind_disp))
temp3_inv <- temp3_inv %>%
  select(-(X)) %>%
  filter(!(Impact == ind_disp))

# Merge the 2 dataframes into 1, and turn NA values into 0's
ind_output_counties <- merge(temp3_reg, temp3_inv, by = c("geo", "Impact"), all = TRUE)
ind_output_counties$Impact[ind_output_counties$Impact == ""] <- "Total"
ind_output_counties[ind_output_counties == ""] <- 0

# Rename existing columns, and run gsub loop code. Then, change columns to be numeric, and add in total columns
colnames(ind_output_counties) <- c("county", "impact", "direct", "indirect", "induced",
                                     "reg_total", "in_indirect", "in_induced", "in_total")
ind_output_counties <- gsub_loop(ind_output_counties, 3:9)

ind_output_counties <- ind_output_counties %>%
  mutate_at(3:9, ~ as.numeric(.)) %>%
  mutate(total_direct = direct,
         total_indirect = indirect + in_indirect,
         total_induced = induced + in_induced,
         total = reg_total + in_total)

# Write into an Excel file - ALL DONE!
write.xlsx(ind_output_counties, file.path(temp_path, paste0(year, out_c_file)))


## EMPLOYMENT INDUSTRIES BY IMPACT ##
# Read in a list of all these files based on their directory
ind_emp_files <- list.files(file.path(implan_res_c, ind_emp_path), csv_pat)

# Create a list of county names as well
counties <- gsub(year, "", 
                 gsub(emp_ind_csv, "",
                      gsub(inv, "", ind_emp_files), ignore.case = T))
counties <- str_trim(counties)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- grep(inv, ind_emp_files)
reg_ind <- (1:length(ind_emp_files))[-inv_ind]

# Define null variables as empty dataframes to write data into from the for loop
temp4_inv <- NULL
temp4_reg <- NULL

# Run result_loop function to generate "regular" and "inverse" dataframes for employment by industry
temp4_reg <- result_loop(reg_ind, temp4_reg, counties, implan_res_c, ind_emp_path, ind_emp_files)
temp4_inv <- result_loop(inv_ind, temp4_inv, counties, implan_res_c, ind_emp_path, ind_emp_files)

# Remove extra column from each dataframe
temp4_reg <- temp4_reg %>%
  select(-(X))
temp4_inv <- temp4_inv %>%
  select(-(c(X, Impact.Employment..1...Direct.)))

# Merge the 2 dataframes into 1, and turn NA values into 0's
ind_emp_counties <- merge(temp4_reg, temp4_inv, by = c("geo", "Industry.Display"), all = TRUE)
ind_emp_counties$Industry.Display[ind_emp_counties$Industry.Display == ""] <- "Total"
ind_emp_counties[is.na(ind_emp_counties)] <- 0

# Rename existing columns, and run gsub loop code. Then, change columns to be numeric, and add in total columns
colnames(ind_emp_counties) <- c("county", "impact", "direct", "indirect", "induced",
                                     "reg_total", "in_indirect", "in_induced", "in_total")
ind_emp_counties <- gsub_loop(ind_emp_counties, 3:9)

ind_emp_counties <- ind_emp_counties %>%
  mutate_at(3:9, ~ as.numeric(.)) %>%
  mutate(total_direct = direct,
         total_indirect = indirect + in_indirect,
         total_induced = induced + in_induced,
         total = reg_total + in_total)

# Write into an Excel file, and remove variables from environment - ALL DONE!
write.xlsx(ind_emp_counties, file.path(temp_path, paste0(year, emp_c_file)))

rm(econ_indic_counties, ind_emp_counties, ind_output_counties, tax_res_counties, temp1_inv, temp1_reg,
   temp2_inv, temp2_reg, temp3_inv, temp3_reg, temp4_inv, temp4_reg)
## Code for reading in county IMPLAN data and combining it into a single Excel file ##

## ECONOMIC INDICATORS BY IMPACT ##
# Read in a list of all these files based on their directory
econ_indic_files <- f_list(file.path(implan_res_c, econ_indic_path), excel_pattern)

# Create a list of county names as well
counties <- geo_list(paste(year, ".+"), "", econ_indic_files)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- f_index(inv, econ_indic_files)
reg_ind <- (1:length(econ_indic_files))[-inv_ind]

# Define null variables to use in for loop

inv_temp <- NULL
reg_temp <- NULL

# Run result_loop function to generate "regular" and "inverse" dataframes for economic indicators
result_loop(reg_ind, reg_temp, county, counties, file.path(implan_res_c, econ_indic_files))






## TAX RESULTS ##
# Read in a list of all these files based on their directory
tax_res_files <- f_list(file.path(implan_res_c, tax_res_path), excel_pattern)

# Create a list of county names as well
counties <- geo_list(paste(year, ".+"), "", tax_res_files)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- f_index(inv, tax_res_files)
reg_ind <- (1:length(tax_res_files))[-inv_ind]





## INDUSTRIES BY IMPACT ##
# Read in a list of all these files based on their directory
output_indus_files <- f_list(file.path(implan_res_c, output_indus_path), excel_pattern)

# Create a list of county names as well
counties <- geo_list(paste(year, ".+"), "", output_indus_files)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- f_index(inv, output_indus_files)
reg_ind <- (1:length(output_indus_files))[-inv_ind]






## EMPLOYMENT INDUSTRIES BY IMPACT ##
# Read in a list of all these files based on their directory
emp_indus_files <- f_list(file.path(implan_res_c, emp_indus_path), excel_pattern)

# Create a list of county names as well
counties <- geo_list(paste(year, ".+"), "", emp_indus_files)

# Define indices to find the "inverse" and "regular" model data sheets
inv_ind <- f_index(inv, emp_indus_files)
reg_ind <- (1:length(emp_indus_files))[-inv_ind]


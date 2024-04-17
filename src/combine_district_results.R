## Code for reading in district IMPLAN data and combining it into a single Excel file ##

## ECONOMIC INDICATORS BY IMPACT ##
# Read in a list of all these files based on their directory
econ_indic_files <- list.files(file.path(implan_res_d, econ_indic_path), csv_pat)

# Create a list of districts as well
districts <- gsub(year, "", 
                 gsub(econ_ind_csv, "",
                      gsub(inv, "", econ_indic_files)))
districts <- str_trim(districts)

# Define index to find the "regular" model data sheet
reg_ind <- (1:length(econ_indic_files))

# Define null variables as empty dataframe to write data into from the for loop
econ_indic_districts <- NULL

# Run result_loop function to generate "regular" dataframe for economic indicators
econ_indic_districts <- result_loop(reg_ind, econ_indic_districts, districts, implan_res_d, econ_indic_path, econ_indic_files)

# Change the "NAs" in the dataframe to be "Total", and change any "NA" values in the other columns to 0
econ_indic_districts$Impact[econ_indic_districts$Impact == ""] <- "Total"
econ_indic_districts[is.na(econ_indic_districts)] <- 0

# Rename existing columns, run gsub loop code, and change columns to be numeric
colnames(econ_indic_districts) <- c("district", "impact", "employment", "labor_income", "value_added", "output")

econ_indic_districts <- gsub_loop(econ_indic_districts, 3:6)

# Write into an Excel file - ALL DONE!
write.xlsx(econ_indic_districts, file.path(temp_path, paste0(year, "_econ_indicators_by_district.xlsx")))
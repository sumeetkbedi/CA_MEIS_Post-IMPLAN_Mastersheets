## CODE FOR CREATING COUNTY INPUT-OUTPUT MULTI-SHEET MASTER EXCEL FILE ##

## PART 1: INPUT DATA ##
# 1.1: Start with USAspending - read in cleaned contracts and grants data #
contracts <- read.csv(file.path(input_path, paste0(f_year, c_file))) %>%
  filter(awarding_agency_name != DOE)

grants <- read.csv(file.path(input_path, paste0(f_year, g_file))) %>%
  filter(awarding_agency_name != DOE)

# Aggregate the dataframes by county, rename columns, and then bring the dataframes together. Keep the usaspending total
contracts <- aggregate(contracts$spending, by = list(contracts$recipient_county_name), FUN = sum)
grants <- aggregate(grants$spending, by = list(grants$recipient_county_name), FUN = sum)

colnames(contracts) <- c("county", "contract_spending")
colnames(grants) <- c("county", "grant_spending")

usaspending <- merge(contracts, grants, by = "county", all = T)
usaspending[is.na(usaspending)] <- 0

usaspending <- usaspending %>%
  add_row(county = "ALPINE", contract_spending = 0, grant_spending = 0) %>%
  add_row(county = "SIERRA", contract_spending = 0, grant_spending = 0) %>%
  mutate(event_value_spending = contract_spending + grant_spending) %>%
  select(county, event_value_spending)

# 1.2: Now grab the SmartPay data by county #
smartpay <- read.xlsx(file.path(input_path, paste0(s_file, f_year, xlsx_pat)), sheet = 1)

# Rename and select the needed columns. Then modify county columns names and drop the total row at the end
smartpay <- smartpay[,c(1,5)]
smartpay <- smartpay %>% 
  rename(county = County, smartpay_spending = Total) %>%
  select(county, smartpay_spending)

smartpay$county = toupper(smartpay$county)
smartpay <- smartpay[!(smartpay$county=="TOTAL"),]

# 1.3: Read in and wrangle VA direct payments data #
va_benefits <- read.csv(file.path(input_path, paste0(f_year, va_file)))
va_benefits <- aggregate(va_benefits$spending, by = list(va_benefits$recipient_county_name), FUN = sum) %>%
  rename(county = Group.1, household_spending = x)

# 1.4: Aggregate all 3 spending data sources by county to get total input spending per county #
input_spend <- Reduce(function(x,y) merge(x = x, y = y, by = "county", all = T),
                         list(usaspending, va_benefits, smartpay))
input_spend <- input_spend %>% 
  mutate(input_spending = event_value_spending + household_spending + smartpay_spending)

# 1.5: Read in and wrangle employment data, should have this already aggregated by county from master analysis run through #
input_emp <- read.xlsx(file.path(input_path, paste0(f_year, emp_file)), sheet = 1)

# Select the needed columns - county, civilian employees, military employees, and total employees
input_emp <- input_emp %>%
  rename(input_emp = total_emp) %>%
  select(county, mili_emp, civil_emp, input_emp)

# 1.6: Merge employment and spending data together to get TOTAL input spending and employment for counties. remove previous variables from environment #
CountyInputs <- merge(input_spend, input_emp, by = "county", all = T)
rm(contracts, grants, usaspending, smartpay, va_benefits, input_spend, input_emp)


## PART 2: OUTPUT DATA ##

# Read in the County Economic Indicators and County Tax Results Excel sheets
county_econ_indicators <- read.xlsx(file.path(temp_path, paste0(year, econ_c_file)))
county_tax_results <- read.xlsx(file.path(temp_path, paste0(year, tax_c_file)))

# 2.1: County Output AND Employment - use "county_econ_indicators" and select the county, output/employment impact type, output, and employment columns #
EconOutput <- county_econ_indicators %>% 
  select(county, impact, total_output) %>%
  rename(total_econ_output = total_output)

OutputEmployment <- county_econ_indicators %>% 
  select(county, impact, total_employment) %>%
  rename(total_fte = total_employment)

# 2.2: County Tax Results - use "county_tax_results", add a column for total local tax revenue #
# select the county, impact, and total tax revenue by local, state, and federal columns. Then filter to only include the totals by impact.
county_tax_results <- county_tax_results %>% 
  mutate(local_tax = total_sub_county_general + total_sub_county_special_district + total_county_rev) %>%
  select(county, impact, local_tax, total_state, total_federal, total) %>% 
  rename(state_tax = total_state, federal_tax = total_federal, total_tax = total)

# 2.3: Combine all of the output data frames into one #
CountyOutputs <- Reduce(function(x,y,z) merge(x = x, y = y, z = z, c("county", "impact")), 
                        list(EconOutput, OutputEmployment, county_tax_results))

# Clean output data frames (remove unnecessary numbers from Impacts column)
CountyOutputs <- CountyOutputs %>%
  mutate(impact = case_when(impact == "1 - Direct" ~ "direct",
                            impact == "2 - Indirect" ~ "indirect",
                            impact == "3 - Induced" ~ "induced",
                            T ~ "total"))

# Convert data to vertical, uppercase county names. Reorder and rename columns
CountyOutputs <- CountyOutputs %>% 
  gather(variable, value, -(county:impact)) %>%
  unite(temp, impact, variable) %>%
  spread(temp, value)

CountyOutputs$county <- toupper(CountyOutputs$county)

CountyOutputs <- CountyOutputs[,c(1, 5, 11, 17, 23, 6, 12, 18, 24, 3, 9, 15, 21, 4, 10, 16, 22, 2, 8, 14, 20, 7, 13, 19, 25)]

CountyOutputs <- CountyOutputs %>%
  rename(direct_output = direct_total_econ_output, indirect_output = indirect_total_econ_output, 
         induced_output = induced_total_econ_output, total_output = total_total_econ_output, 
         direct_fte = direct_total_fte, indirect_fte = indirect_total_fte, 
         induced_fte = induced_total_fte, total_fte = total_total_fte)

# Combine the county input and output data into 1 dataframe. Remove previous variables from environment
CountyIOData <- merge(CountyInputs, CountyOutputs, by = "county", all = T)
rm(county_econ_indicators, county_tax_results, EconOutput, OutputEmployment, CountyInputs, CountyOutputs)


## REGIONALIZE DATA ##
regions_cw <- read.xlsx(file.path(input_path, reg_file), sheet = 1) #upload regions crosswalk

# Merge crosswalk to get regions for every county. Relocate the regions column next to the county column
CountyIOData <- merge(CountyIOData, regions_cw, by = ("county"), all = T)
CountyIOData <- CountyIOData %>%
  relocate(region, .after = county)

# Create new DF with data aggregated by region
Regionsag <- CountyIOData %>% 
  select(-(county)) %>% #get rid of County column (it breaks the code below)
  group_by(region) %>% #group by region
  summarise_all(list(sum)) %>% #sum the columns
  mutate(county = "REGION") %>% #make county name "region"
  relocate(county, .before = region) #move the region column

# Vertically merge data aggregated by region with CountyIOData
CountyIOData <- rbind(CountyIOData, Regionsag)


## ADD % OF EMPLOYMENT PER COUNTY ## - percentage of total jobs in a county supported by national security activity. Use Sept. FY data file from EDD
lf_county <- read.xlsx(file.path(input_path, paste0(f_year, lf_file)))

names(lf_county) <- lapply(lf_county[1,], as.character)
lf_county <- lf_county[-1,]

lf_county <- lf_county %>% #select only needed columns
  select(COUNTY, EMPLOYMENT)  %>%
  rename(county = COUNTY, employment = EMPLOYMENT)
lf_county <- lf_county[2:59,]

# Merge region crosswalk with labor force data. Get regional totals
lf_county <- merge(lf_county, regions_cw, by = ("county"), all = T)
lf_county[,2] <- as.numeric(lf_county[,2])

regions_lf_data <- lf_county %>%
  select(-(county)) %>% #get rid of County column (it breaks the code below)
  group_by(region) %>% #group by region
  summarise_all(list(sum)) %>% #sum the columns
  mutate(county = "REGION") %>% #make county name "region"
  relocate(county, .before = region) #move the region column

# Vertically merge regionalized data with labor force data. Then merge with county input-output data
lf_county <- rbind(lf_county, regions_lf_data)

CountyIOData <- merge(CountyIOData, lf_county, by = (c("county", "region")), all = T)

# Calculate percentage of total county employment made up by national security-generated jobs
CountyIOData <- CountyIOData %>%
  mutate(percent_total_county_emp = total_fte/employment) %>%
  select(-(employment)) %>%
  relocate(percent_total_county_emp, .after = total_fte)


## ADD COUNTY POPULATION COLUMN ##
county_pop <- read.xlsx(file.path(input_path, paste0(f_year, pop_file)), sheet = 2)

names(county_pop) <- lapply(county_pop[1,], as.character)
county_pop <- county_pop[-1,]

county_pop <- county_pop %>%
  select(Geography, f_year) %>%
  rename(county = Geography, population = f_year)
#county_pop <- county_pop[-60,]
#county_pop$county <- gsub(" County","",as.character(county_pop$county))
#county_pop$county <- toupper(county_pop$county)
county_pop <- county_pop[2:59,]
county_pop$county <- str_trim(county_pop$county)

# Merge region crosswalk with county population data. Get regional totals
county_pop <- merge(county_pop, regions_cw, by = ("county"))
county_pop[,2] <- as.numeric(county_pop[,2])

regions_county_pop <- county_pop %>%
  select(-(county)) %>% #get rid of County column (it breaks the code below)
  group_by(region) %>% #group by region
  summarise_all(list(sum)) %>% #sum the columns
  mutate(county = "REGION") %>% #make county name "region"
  relocate(county, .before = region) #move the region column

# Vertically merge regionalized data with population data. Then merge with county input/output data
county_pop <- rbind(county_pop, regions_county_pop)

CountyIOData <- merge(CountyIOData, county_pop, by = (c("county", "region")), all = T)

# Add a column that convert county names to title case. Relocate population and county title case columns to front of data
CountyIOData$county_title <- str_to_title(CountyIOData$county)

CountyIOData <- CountyIOData %>%
  relocate(population, .before = event_value_spending) %>%
  relocate(county_title, .before = region)


## Add data per 100,000 residents ##
CountyIOData <- CountyIOData %>%
  mutate(input_spending_per_100k = ((input_spending/population)*100000), 
         input_emp_per_100k = ((input_emp/population)*100000),
         econ_output_per_100k = ((total_output/population)*100000),
         fte_per_100k = ((total_fte/population)*100000))

CountyIOData <- CountyIOData %>%
  relocate(input_spending_per_100k, .after = input_spending) %>%
  relocate(input_emp_per_100k, .after = input_emp) %>%
  relocate(econ_output_per_100k, .after = total_output) %>%
  relocate(fte_per_100k, .after = total_fte)

# Remove unneeded variables
rm(Regionsag, lf_county, regions_lf_data, county_pop, regions_county_pop)

## THIS COMPLETES SHEET 1 ##


## SHEET 2: Industries' Output ##

# Import industries output spreadsheet and crosswalk, format them appropriately, and merge together. Sum the data by county and rollup
industry_output <- read_xlsx(file.path(temp_path, paste0(year, out_c_file)))
industry_output <- industry_output %>% 
  separate(impact, c("industry_display", "description"), sep = "( - )", extra = "merge", fill = "right")

industry_cw <- read.csv(file.path(input_path, paste0(ind_imp_file)))
industry_cw <- industry_cw %>%
  select(industry_display, rollup)

industry_output <- merge(industry_cw, industry_output, by = ("industry_display"))
industry_output <- industry_output %>% 
  select(-(c(industry_display, description))) %>%
  group_by(county, rollup) %>%
  summarise_all(list(sum))
industry_output$county <- toupper(industry_output$county)

# Regionalize the data by merging it with the regions crosswalk
industry_output <- merge(industry_output, regions_cw, by = ("county"), all = T)
industry_output <- industry_output %>%
  relocate(region, .after = county)

# Create a separate df with industry info aggregated by region
ind_out_Regionsag <- industry_output %>% 
  select(-(county)) %>% #get rid of impacts column (it breaks the code below)
  group_by(region, rollup) %>% #group by region and rollup
  summarise_all(list(sum)) %>% #sum the columns
  mutate(county = "REGION") %>% #re-add county name but set them all to Region
  relocate(county, .before = region) #move columns around for consistency

# Vertically merge regionalized data with industries by impact data and remove unneeded variables
industry_output <- rbind(industry_output, ind_out_Regionsag)
rm(ind_out_Regionsag)


## SHEET 3: Industries' Employment ##

# Import industries employment spreadsheet, and run same process as you did for industries' output
industry_employment <- read_xlsx(file.path(temp_path, paste0(year, emp_c_file)))
industry_employment <- industry_employment %>% 
  separate(impact, c("industry_display","description"), sep = "( - )", extra = "merge", fill = "right")

industry_employment <- merge(industry_cw, industry_employment, by = ("industry_display"))

industry_employment  <- industry_employment %>% 
  select(-(c(industry_display, description))) %>%
  group_by(county, rollup) %>%
  summarise_all(list(sum))
industry_employment$county <- toupper(industry_employment$county)

# Regionalize the data by merging it with the regions crosswalk
industry_employment  <- merge(industry_employment, regions_cw, by = ("county"), all = T) #apply crosswalk to counties spreadsheet
industry_employment <- industry_employment %>%
  relocate(region, .after = county)

# Create a separate df with industry info aggregated by region
ind_emp_Regionsag <- industry_employment %>% 
  select(-(county)) %>% #get rid of impacts column (it breaks the code below)
  group_by(region, rollup) %>% #group by region and rollup
  summarise_all(list(sum)) %>% #sum the columns
  mutate(county = "REGION") %>% #re-add county name but set them all to Region
  relocate(county, .before = region) #move columns around for consistency

# Vertically merge regionalized data with industries by impact data and remove unneeded variables
industry_employment <- rbind(industry_employment, ind_emp_Regionsag)
rm(regions_cw, industry_cw, ind_emp_Regionsag)


## FINAL STEPS: create list with needed dataframes, and write list into file. All done! ##
multisheetlist <- list(InputOutput = CountyIOData, IndustryOutput = industry_output, IndustryEmployment = industry_employment)
write.xlsx(multisheetlist, file.path(output_path, paste0(year, "_county", final_file)))
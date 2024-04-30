## CODE FOR CREATING COUNTY INPUT-OUTPUT MULTI-SHEET MASTER EXCEL FILE ##

## PART 1: INPUT DATA ##
# 1.1: Start with USAspending - read in cleaned contracts and grants data #
contracts <- read.csv(file.path(input_path, paste0(f_year, c_file))) %>%
  filter(awarding_agency_name != "DEPARTMENT OF ENERGY (DOE)")
grants <- read.csv(file.path(input_path, paste0(f_year, g_file))) %>%
  filter(awarding_agency_name != "DEPARTMENT OF ENERGY (DOE)")

# Aggregate the dataframes by county, rename columns, and then bring the dataframes together. Keep the usaspending total
contracts <- aggregate(contracts$spending, by = list(contracts$recipient_county_name), FUN = sum)
grants <- aggregate(grants$spending, by = list(grants$recipient_county_name), FUN = sum)

colnames(contracts) <- c("county", "contract_spending")
colnames(grants) <- c("county", "grant_spending")

usaspending <- merge(contracts, grants, by = "county", all = TRUE)
usaspending <- usaspending %>%
  add_row(county = "ALPINE", contract_spending = 0, grant_spending = 0) %>%
  add_row(county = "SIERRA", contract_spending = 0, grant_spending = 0) %>%
  mutate(event_value_spending = contract_spending + grant_spending) %>%
  select(county, event_value_spending)
usaspending$event_value_spending[is.na(usaspending$event_value_spending)] <- 0

# 1.2: Now grab the SmartPay data by county #
smartpay <- read.xlsx(file.path(input_path, paste0("SmartPay_FY_2022.xlsx")), sheet = 1)

# Rename and select the needed columns. Then modify county columns names and drop the total row at the end
smartpay <- smartpay %>% 
  rename(county = X1, smartpay_spending = Total) %>%
  select(county, smartpay_spending)

smartpay$county = toupper(smartpay$county)
smartpay <- smartpay[!(smartpay$county=="TOTAL"),]

# 1.3: Read in and wrangle VA direct payments data #
va_benefits <- read.csv(file.path(input_path, paste0(f_year, "_cleaned_va_benefits.csv")))
va_benefits <- aggregate(va_benefits$spending, by = list(va_benefits$recipient_county_name), FUN = sum) %>%
  rename(county = Group.1, household_spending = x)

# 1.4: Aggregate all 3 spending data sources by county to get total input spending per county #
input_spend <- Reduce(function(x,y) merge(x = x, y = y, by = "county", all = TRUE),
                         list(usaspending, va_benefits, smartpay))
input_spend <- input_spend %>% 
  mutate(input_spending = event_value_spending + household_spending + smartpay_spending)

# 1.5: Read in and wrangle employment data, should have this already aggregated by county from master analysis run through #
input_emp <- read.xlsx(file.path(input_path, paste0(year, "_direct_employment.xlsx")), sheet = 1)

# Select the needed columns - county, civilian employees, military employees, and total employees
input_emp <- input_emp %>%
  rename(input_emp = total_emp) %>%
  select(county, mili_emp, civil_emp, input_emp)

# 1.6: Merge employment and spending data together to get TOTAL input spending and employment for counties. remove previous variables from environment #
CountyInputs <- merge(input_spend, input_emp, by = "county", all = TRUE)
rm(contracts, grants, usaspending, smartpay, va_benefits)


## PART 2: OUTPUT DATA ##

# Read in the County Economic Indicators and County Tax Results Excel sheets
county_econ_indicators <- read.xlsx(file.path(temp_path, paste0(year, "_econ_indicators_by_county.xlsx")))
county_tax_results <- read.xlsx(file.path(temp_path, paste0(year, "_tax_results_by_county.xlsx")))

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
                            TRUE ~ "total"))

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

# Combine the county input and output data into 1 dataframe
CountyIOData <- merge(CountyInputs, CountyOutputs, by = "county", all = TRUE)


## REGIONALIZE DATA ##
regions_crosswalk <- read.csv(file.path(input_path, paste0("County_Regions.csv")), fileEncoding = "UTF-8-BOM") #upload regions crosswalk

# Merge crosswalk to get regions for every county. Relocate the regions column next to the county column
CountyIOData <- merge(CountyIOData, regions_crosswalk, by = ("county"), all = TRUE)
CountyIOData <- CountyIOData %>%
  relocate(region, .after = county)

# Create new DF with data aggregated by region
Regionsag <- CountyIOData %>% 
  select(-(county)) %>% #get rid of County column (it breaks the code below)
  group_by(region) %>% #group by region
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(county = "REGION") %>% #make county name "region"
  relocate(county, .before = region) #move the region column

# Vertically merge data aggregated by region with CountyIOData
CountyIOData <- rbind(CountyIOData, Regionsag)


## ADD % OF EMPLOYMENT PER COUNTY ## - this is percentage of total jobs in a county that are supported by national security spending
labor_force_county <- read.xlsx(file.path(input_path, paste0("2022 Labor Force by County EDD.xlsx")))

labor_force_county <- labor_force_county %>% #select only needed columns
  select(COUNTY, EMPLOYMENT)  %>%
  rename(county = COUNTY, employment = EMPLOYMENT)

# Merge region crosswalk with labor force data. Get regional totals
labor_force_county <- merge(labor_force_county, regions_crosswalk, by = ("county"), all = TRUE) 

regions_labor_force_data <- labor_force_county %>%
  select(-(county)) %>% #get rid of County column (it breaks the code below)
  group_by(region) %>% #group by region
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(county = "REGION") %>% #make county name "region"
  relocate(county, .before = region) #move the region column

# Vertically merge regionalized data with labor force data. Then merge with county input-output data
labor_force_county <- rbind(labor_force_county, regions_labor_force_data) 

CountyIOData <- merge(CountyIOData, labor_force_county, by = (c("county", "region")), all = TRUE)

# Calculate percentage of total county employment made up by national security-generated jobs
CountyIOData <- CountyIOData %>%
  mutate(percent_total_county_employment = total_fte/employment) %>%
  select(-(employment)) %>%
  relocate(percent_total_county_employment, .after = total_fte)


## ADD COUNTY POPULATION COLUMN ##
county_pop <- read.xlsx(file.path(input_path, paste0("2022 CA Population by County.xlsx")), sheet = 2)
county_pop <- county_pop %>%
  select(Geography, "2022") %>%
  rename(county = Geography, population = "2022")
county_pop <- county_pop[-60,]
county_pop$county <- gsub(" County","",as.character(county_pop$county))
county_pop$county <- toupper(county_pop$county)

county_pop <- merge(county_pop, regions_crosswalk, by = ("county"))

# Regionalize county population data
regions_county_pop <- county_pop %>%
  select(-(county)) %>% #get rid of County column (it breaks the code below)
  group_by(region) %>% #group by region
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(county = "REGION") %>% #make county name "region"
  relocate(county, .before = region) #move the region column

# Vertically merge regionalized data with population data. Then merge with county input/output data
county_pop <- rbind(county_pop, regions_county_pop)

CountyIOData <- merge(CountyIOData, county_pop, by = (c("county", "region")), all = TRUE)

# Add a column that convert county names to title case. Relocate population and county title case columns to front of data
CountyIOData$county_title <- str_to_title(CountyIOData$county)

CountyIOData <- CountyIOData %>%
  relocate(population, .before = event_value_spending) %>%
  relocate(county_title, .before = region)


## Add data per 100,000 residents ##
CountyIOData <- CountyIOData %>%
  mutate(input_spending_per_100000 = ((input_spending/population)*100000), 
         input_emp_per_100000 = ((input_emp/population)*100000),
         econ_output_per_100000 = ((total_output/population)*100000),
         fte_per_100000 = ((total_fte/population)*100000))

CountyIOData <- CountyIOData %>%
  relocate(input_spending_per_100000, .after = input_spending) %>%
  relocate(input_emp_per_100000, .after = input_emp) %>%
  relocate(econ_output_per_100000, .after = total_output) %>%
  relocate(fte_per_100000, .after = total_fte)

## THIS COMPLETES SHEET 1 ##


## SHEET 2: Industries' Output ##

# Import industries output spreadsheet and crosswalk, format them appropriately, and merge together. Sum the data by county and rollup
industry_output <- read_xlsx(file.path(temp_path, paste0(year, "_industry_output_by_county.xlsx")))
industry_output <- industry_output %>% 
  separate(impact, c("industry_display","description"), sep = "( - )", extra = "merge", fill = "right")

industry_crosswalk <- read_xlsx(file.path(raw_path, paste0("Industries by Impact Groupings.xlsx")))
industry_crosswalk <- industry_crosswalk %>%
  select(industry_display, rollup)

industry_output <- merge(industry_crosswalk, industry_output, by = ("industry_display"))
industry_output <- industry_output %>% 
  select(-(c(industry_display, description))) %>%
  group_by(county, rollup) %>%
  summarise_each(funs(sum))
industry_output$county <- toupper(industry_output$county)

# Regionalize the data by merging it with the regions crosswalk
industry_output <- merge(industry_output, regions_crosswalk, by = ("county"), all = TRUE)
industry_output <- industry_output %>%
  relocate(region, .after = county)

# Create a separate df with industry info aggregated by region
industry_Regionsag <- industry_output %>% 
  select(-(county)) %>% #get rid of impacts column (it breaks the code below)
  group_by(region, rollup) %>% #group by region and rollup
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(county = "REGION") %>% #re-add county name but set them all to Region
  relocate(county, .before = region) #move columns around for consistency

# Vertically merge regionalized data with industries by impact data
industry_output <- rbind(industry_output, industry_Regionsag)


## SHEET 3: Industries' Employment ##

# Import industries employment spreadsheet, and run same process as you did for industries' output
industry_employment <- read_xlsx(file.path(temp_path, paste0(year, "_industry_employment_by_county.xlsx")))
industry_employment <- industry_employment %>% 
  separate(impact, c("industry_display","description"), sep = "( - )", extra = "merge", fill = "right")

industry_employment <- merge(industry_crosswalk, industry_employment, by = ("industry_display"))

industry_employment  <- industry_employment %>% 
  select(-(c(industry_display, description))) %>%
  group_by(county, rollup) %>%
  summarise_each(funs(sum))
industry_employment$county <- toupper(industry_employment$county)

# Regionalize the data by merging it with the regions crosswalk
industry_employment  <- merge(industry_employment, regions_crosswalk, by = ("county"), all = TRUE) #apply crosswalk to counties spreadsheet
industry_employment <- industry_employment %>%
  relocate(region, .after = county)

# Create a separate df with industry info aggregated by region
employment_Regionsag <- industry_employment %>% 
  select(-(county)) %>% #get rid of impacts column (it breaks the code below)
  group_by(region, rollup) %>% #group by region and rollup
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(county = "REGION") %>% #re-add county name but set them all to Region
  relocate(county, .before = region) #move columns around for consistency

# Vertically merge regionalized data with industries by imact data
industry_employment <- rbind(industry_employment, employment_Regionsag)


## FINAL STEPS: create list with needed dataframes, and write list into file. All done! ##
multisheetlist <- list(InputOutput = CountyIOData, IndustryOutput = industry_output, IndustryEmployment = industry_employment)
write.xlsx(multisheetlist, file.path(output_path, paste0(year, "_county_input_output_data.xlsx")))

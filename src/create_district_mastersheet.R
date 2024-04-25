## CODE FOR CREATING DISTRICT INPUT-OUTPUT MASTER EXCEL FILE ##

## PART 1: INPUT DATA ##
# 1.1: Start with USAspending - read in cleaned contracts and grants data #
contracts <- read.csv(file.path(input_path, paste0(f_year, c_file))) %>%
  filter(awarding_agency_name != "DEPARTMENT OF ENERGY (DOE)")
grants <- read.csv(file.path(input_path, paste0(f_year, g_file))) %>%
  filter(awarding_agency_name != "DEPARTMENT OF ENERGY (DOE)")

# Aggregate the dataframes by district, rename columns, and then bring the dataframes together. Keep the usaspending total
contracts <- aggregate(contracts$spending, by = list(contracts$recipient_congressional_district), FUN = sum)
grants <- aggregate(grants$spending, by = list(grants$recipient_congressional_district), FUN = sum)

colnames(contracts) <- c("district", "contract_spending")
colnames(grants) <- c("district", "grant_spending")

usaspending <- merge(contracts, grants, by = "district", all = TRUE)
usaspending[is.na(usaspending)] <- 0
usaspending <- usaspending %>%
  mutate(event_value_spending = contract_spending + grant_spending) %>%
  select(district, event_value_spending)

# 1.2: Now grab the SmartPay data by district #
smartpay <- read_xlsx(file.path(input_path, paste0("SmartPay_FY_2022.xlsx")), sheet = 2)

# Rename and select the needed columns. Then modify district columns names and drop unneeded rows
smartpay <- smartpay %>% 
  rename(district = ...9, smartpay_spending = ...13) %>%
  select(district, smartpay_spending)

smartpay <- smartpay[!(smartpay$district == "District"| smartpay$district == "Total" | is.na(smartpay$district)),]
smartpay$district <- as.integer(smartpay$district)
smartpay$smartpay_spending <- as.numeric(smartpay$smartpay_spending)

# 1.3: Read in and wrangle VA direct payments data #
va_benefits <- read.csv(file.path(input_path, paste0(f_year, "_cleaned_va_benefits.csv")))
va_benefits <- aggregate(va_benefits$spending, by = list(va_benefits$congressional_district), FUN = sum) %>%
  rename(district = Group.1, household_spending = x)

# 1.4: Aggregate all 3 spending data sources by district to get total input spending per district #
input_spend <- Reduce(function(x,y) merge(x = x, y = y, by = "district", all = TRUE),
                      list(usaspending, va_benefits, smartpay))
input_spend <- input_spend %>% 
  mutate(input_spending = event_value_spending + household_spending + smartpay_spending)

# 1.5: Read in and wrangle employment data, should have this already aggregated by district from master analysis run through #
input_emp <- read_xlsx(file.path(input_path, paste0(year, "_direct_employment.xlsx")), sheet = 2)

#Select the needed columns - district, civilian employees, military employees, and total employees
input_emp <- input_emp %>%
  rename(input_emp = total_emp) %>%
  select(district, mili_emp, civil_emp, input_emp)

# 1.6: Merge employment and spending data together to get TOTAL input spending and employment for districts. remove previous variables from environment #
DistrictInputs <- merge(input_spend, input_emp, by = "district", all = TRUE)
rm(contracts, grants, usaspending, smartpay, va_benefits)


## PART 2: OUTPUT DATA ##

# Read in the District Economic Indicators Excel sheet for direct output and employment 
district_econ_indicators <- read.xlsx(file.path(temp_path, paste0(year, "_econ_indicators_by_district.xlsx")))

# 2.1: Direct Output AND Employment - use "district_econ_indicators" and select the district, direct impact, output, and employment columns #
DirectDistrictOutput <- district_econ_indicators %>% 
  select ("district", "impact", "output") %>%
  mutate(impact = replace(impact, impact == "1 - Direct", "Direct")) %>%
  filter(impact == "Direct") %>%
  rename(direct_district_output = output) %>%
  select("district", "direct_district_output")
DirectDistrictOutput$district <- as.numeric(gsub("[a-zA-Z ]", "",
                                                 gsub("-", "", DirectDistrictOutput$district)))

DirectDistrictEmployment <- district_econ_indicators %>% 
  select ("district", "impact", "employment") %>%
  mutate(impact = replace(impact, impact == "1 - Direct", "Direct")) %>%
  filter(impact == "Direct") %>%
  rename(direct_district_fte = employment) %>%
  select("district", "direct_district_fte")
DirectDistrictEmployment$district <- as.numeric(gsub("[a-zA-Z ]", "",
                                                     gsub("-", "", DirectDistrictEmployment$district)))

DirectDistrict <- merge(DirectDistrictOutput, DirectDistrictEmployment, by = "district") %>%
  mutate_at(c("direct_district_fte", "direct_district_output"), as.numeric)

# Now read in the needed files for calculating the districts' indirect and induced output and employment
county_econ_indicators <- read.xlsx(file.path(temp_path, paste0(year, "_econ_indicators_by_county.xlsx")))
cd_proportion <- read.csv(file.path(input_path, "county_to_52_districts_pop_crosswalk.csv")) %>%
  rename(county = geography)
cd_proportion$county = tolower(cd_proportion$county)

# 2.2: Induced Output and Employment - grab the induced data from the County Econ Indicators file and merge them #
InducedCountyOutput <- county_econ_indicators %>% 
  select ("county", "impact", "total_output") %>%
  mutate(impact = replace(impact, impact == "3 - Induced", "Induced")) %>%
  filter(impact == "Induced") %>%
  rename(induced_county_output = total_output) %>%
  select("county", "induced_county_output")

InducedCountyEmp <- county_econ_indicators %>% 
  select ("county", "impact", "total_employment") %>%
  mutate(impact = replace(impact, impact == "3 - Induced", "Induced")) %>%
  filter(impact == "Induced") %>%
  rename(induced_county_employment = total_employment) %>%
  select("county", "induced_county_employment")

InducedCounty <- merge(InducedCountyEmp, InducedCountyOutput)

# Use the cd_proportion to distribute induced county output and employment to the districts, and aggregate the results to the respective districts
InducedDistrict <- merge(InducedCounty, cd_proportion, by = "county")

InducedDistrict <- InducedDistrict %>%
  mutate(induced_district_fte = InducedDistrict$induced_county_employment*InducedDistrict$percentage,
         induced_district_output = InducedDistrict$induced_county_output*InducedDistrict$percentage) %>%
  select("district", "induced_district_fte", "induced_district_output")
InducedDistrict <- InducedDistrict %>%
  aggregate(by = list(InducedDistrict$district), FUN = sum) %>%
  select("Group.1", "induced_district_output", "induced_district_fte") %>%
  rename(district = Group.1)

# 2.3: Indirect Output and Employment - grab the indirect data from the County Econ Indicators file and merge them #
IndirectCountyOutput <- county_econ_indicators %>% 
  select ("county", "impact", "total_output") %>%
  mutate(impact = replace(impact, impact == "2 - Indirect", "Indirect")) %>%
  filter(impact == "Indirect") %>%
  rename(indirect_county_output = total_output) %>%
  select("county", "indirect_county_output")

IndirectCountyEmp <- county_econ_indicators %>% 
  select ("county", "impact", "total_employment") %>%
  mutate(impact = replace(impact, impact == "2 - Indirect", "Indirect")) %>%
  filter(impact == "Indirect") %>%
  rename(indirect_county_employment = total_employment) %>%
  select("county", "indirect_county_employment")

IndirectCounty <- merge(IndirectCountyEmp, IndirectCountyOutput)

# Merge in districts' direct output and employment data with counties' indirect results to calculate districts' indirect results
IndirectCountyProp <- merge(IndirectCounty, cd_proportion, by = "county")

DistrictD_CountyI <- merge(DirectDistrict, IndirectCountyProp, by = "district")
DistrictD_CountyI <- DistrictD_CountyI %>%
  relocate(county, .before = district) %>%
  relocate(percentage, .after = district) %>%
  relocate(population, .after= percentage) %>%
  rename(countypop2020 = population) %>%
  mutate_at(c("direct_district_fte", "direct_district_output"), as.numeric)

# Calculate the districts' 2010 population, which is used to apportion the district's share in each county, and multiply this share by the districts' direct output and employment
DistrictPop2020 <- aggregate(DistrictD_CountyI$countypop2020, by = list(DistrictD_CountyI$district), FUN = sum) %>%
  rename(district = Group.1, districtpop2020 = x)

DistrictD_CountyI <- merge(DistrictD_CountyI, DistrictPop2020, by = "district") %>%
  relocate(districtpop2020, .after= countypop2020)
DistrictD_CountyI <- DistrictD_CountyI %>%
  mutate(CountyDistrictShare = DistrictD_CountyI$countypop2020 / DistrictD_CountyI$districtpop2020)
DistrictD_CountyI <- DistrictD_CountyI %>%
  mutate(CountyDistrictDirectOutput = DistrictD_CountyI$direct_district_output * DistrictD_CountyI$CountyDistrictShare,
         CountyDistrictDirectEmployment = DistrictD_CountyI$direct_district_fte * DistrictD_CountyI$CountyDistrictShare)

# Create 2 new data frames that aggregate the CountyDistrict output and employment, and merge back into the original data frame DistrictD_CountyI
CountyDistrictDirectOutputSum <- aggregate(DistrictD_CountyI$CountyDistrictDirectOutput, by = list(DistrictD_CountyI$county), FUN = sum) %>%
  rename(county = Group.1, CountyDistrictDirectOutputSum = x)

CountyDistrictDirectEmploymentSum <- aggregate(DistrictD_CountyI$CountyDistrictDirectEmployment, by = list(DistrictD_CountyI$county), FUN = sum) %>%
  rename(county = Group.1, CountyDistrictDirectEmploymentSum = x)

DistrictD_CountyI <- Reduce(function(x,y) merge(x = x, y = y, by = "county"), list(DistrictD_CountyI, CountyDistrictDirectEmploymentSum, CountyDistrictDirectOutputSum)) %>%
  relocate(CountyDistrictDirectOutputSum, .after= CountyDistrictDirectOutput)

# Calculate the countydistricts' direct output and employment share, and then multiply the share by the counties' indirect output and employment to determine the counties' share of indirect output and employment
DistrictD_CountyI <- DistrictD_CountyI %>%
  mutate(CountyDistrictDirectOutputShare = CountyDistrictDirectOutput / CountyDistrictDirectOutputSum,
         CountyDistrictDirectEmploymentShare = CountyDistrictDirectEmployment / CountyDistrictDirectEmploymentSum,
         CountyShareOutput = CountyDistrictDirectOutputShare * indirect_county_output,
         CountyShareEmployment = CountyDistrictDirectEmploymentShare * indirect_county_employment)

# Aggregate the county's shared output and employment by district to get each district's indirect output and employment
IndirectDistrictOutput <- aggregate(DistrictD_CountyI$CountyShareOutput, by = list(DistrictD_CountyI$district), FUN = sum) %>%
  rename(district = Group.1, indirect_district_output = x)

IndirectDistrictEmployment <- aggregate(DistrictD_CountyI$CountyShareEmployment, by = list(DistrictD_CountyI$district), FUN = sum) %>%
  rename(district = Group.1, indirect_district_fte = x)

IndirectDistrict <- merge(IndirectDistrictOutput, IndirectDistrictEmployment)

# 2.4: Combine all data frames into one #
DistrictOutputs <- Reduce(function(x,y) merge(x = x, y = y, by = "district"),
                          list(DirectDistrict, IndirectDistrict, InducedDistrict))
DistrictOutputs <- DistrictOutputs %>%
  relocate(indirect_district_output, .after = direct_district_output) %>%
  relocate(induced_district_output, .after = indirect_district_output) %>%
  relocate(indirect_district_fte, .after = direct_district_fte) %>%
  relocate(induced_district_fte, .after = indirect_district_fte)
DistrictOutputs <- DistrictOutputs %>%
  mutate(total_district_output = direct_district_output + indirect_district_output + induced_district_output,
         total_district_fte = direct_district_fte + indirect_district_fte + induced_district_fte) %>%
  relocate(total_district_output, .before = direct_district_fte)

# Combine the county input and output data into 1 dataframe
DistrictIOData <- merge(DistrictInputs, DistrictOutputs, by = "district", all = TRUE)


## REGIONALIZE DATA ##
regions_crosswalk <- read_excel("Regions.xlsx") #upload regions crosswalk
colnames(regions_crosswalk) <- c("County", "Region") #change col names so they match dfs

# Merge crosswalk to get regions for every county
CountyIOData <- merge(CountyIOData, regions_crosswalk, by = ("County"), all = TRUE)

# Reorder columns
CountyIOData <- CountyIOData %>%
  select(c("County", "Region", "event_value_spending", "household_spending", "smartpay_c", "input_spending", "Civilian_Personnel", 
           "Military_Personnel", "input_employment", "Direct_total_econ_output", "Indirect_total_econ_output", 
           "Induced_total_econ_output", "Total_total_econ_output", "Direct_output_employment", "Indirect_output_employment", 
           "Induced_output_employment", "Total_output_employment")) %>%
  rename(Direct_econ_output = Direct_total_econ_output, Indirect_econ_output = Indirect_total_econ_output, 
         Induced_econ_output = Induced_total_econ_output, Total_econ_output = Total_total_econ_output, 
         Direct_output_FTE = Direct_output_employment, Indirect_output_FTE = Indirect_output_employment, 
         Induced_output_FTE = Induced_output_employment, Total_output_FTE = Total_output_employment)

# Create new DF with data aggregated by region
Regionsag <- CountyIOData %>% 
  select(-(County)) %>% #get rid of County column (it breaks the code below)
  group_by(Region) %>% #group by region
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(County = "REGION") %>% #make county name "region"
  relocate(County, .before = Region) #move the region column

# Vertically merge data aggregated by region with CountyIOData
CountyIOData <- rbind(CountyIOData, Regionsag)

## ADD % OF EMPLOYMENT PER COUNTY ## - this is percentage of total jobs in a county that are supported by national security spending
labor_force_county <- read_xlsx("2020 Labor Force by County EDD.xlsx")

labor_force_county <- labor_force_county %>% #select only needed columns
  select(COUNTY, EMPLOYMENT)  %>%
  rename(County = COUNTY, Employment = EMPLOYMENT)

# merge region crosswalk with labor force dta
labor_force_county <- merge(labor_force_county, regions_crosswalk, by = ("County"), all = TRUE) 

# get region totals
regions_labor_force_data <- labor_force_county %>%
  select(-(County)) %>% #get rid of County column (it breaks the code below)
  group_by(Region) %>% #group by region
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(County = "REGION") %>% #make county name "region"
  relocate(County, .before = Region) #move the region column

# vertically merge regionalized data with labor force data
labor_force_county <- rbind(labor_force_county, regions_labor_force_data) 

# Merge with county input/output data
CountyIOData <- merge(CountyIOData, labor_force_county, by = (c("County", "Region")), all = TRUE)

# Calculate percentage of total county employment made up by national security-generated jobs
CountyIOData <- CountyIOData %>%
  mutate(percent_total_county_employment = Total_output_FTE/Employment) %>%
  select(-(Employment)) %>%
  relocate(percent_total_county_employment, .after = Total_output_FTE)

## ADD COUNTY POPULATION COLUMN ##
County_pop <- read_xlsx("2020 CA Population by County.xlsx", sheet = 2)
County_pop <- County_pop %>%
  select(Geography, "2020") %>%
  rename(County = Geography, Population = "2020")
County_pop$County <-gsub(" County","",as.character(County_pop$County))
County_pop$County <-toupper(County_pop$County)

County_pop <- merge(County_pop, regions_crosswalk, by = ("County"))

#regionalize county population data
regions_County_pop <- County_pop %>%
  select(-(County)) %>% #get rid of County column (it breaks the code below)
  group_by(Region) %>% #group by region
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(County = "REGION") %>% #make county name "region"
  relocate(County, .before = Region) #move the region column

# vertically merge regionalized data with labor force data
County_pop <- rbind(County_pop, regions_County_pop) 

# Merge with county input/output data
CountyIOData <- merge(CountyIOData, County_pop, by = (c("County", "Region")), all = TRUE)

# relocate to top of data
CountyIOData <- CountyIOData %>%
  relocate(Population, .before = event_value_spending)

# relocate to top of data
CountyIOData <- CountyIOData %>%
  relocate(County_Title, .before = Region)

## Add data per 100,000 residents ##
CountyIOData <- CountyIOData %>%
  mutate(input_spending_per_100000 = ((input_spending/Population)*100000), 
         input_employment_per_100000 = ((input_employment/Population)*100000),
         economic_output_per_100000 = ((Total_econ_output/Population)*100000),
         FTE_per_100000 = ((Total_output_FTE/Population)*100000))

CountyIOData <- CountyIOData %>%
  relocate(input_spending_per_100000, .after = input_spending) %>%
  relocate(input_employment_per_100000, .after = input_employment) %>%
  relocate(economic_output_per_100000, .after = Total_econ_output) %>%
  relocate(FTE_per_100000, .after = Total_output_FTE)



## FINAL STEP: write data into file. All done! ##
write.xlsx(DistrictIOData, file.path(temp_path, paste0(year, "_district_input_output_data.xlsx")))

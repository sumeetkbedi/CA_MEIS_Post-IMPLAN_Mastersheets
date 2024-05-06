## CODE FOR CREATING DISTRICT INPUT-OUTPUT MASTER EXCEL FILE ##

## PART 1: INPUT DATA ##
# 1.1: Start with USAspending - read in cleaned contracts and grants data #
contracts <- read.csv(file.path(input_path, paste0(f_year, c_file))) %>%
  filter(awarding_agency_name != "Department of Energy")

grants <- read.csv(file.path(input_path, paste0(f_year, g_file))) %>%
  filter(awarding_agency_name != "Department of Energy")

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

# Select the needed columns - district, civilian employees, military employees, and total employees
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

# Combine the county input and output data into 1 dataframe. Rename columns to simpler terms
DistrictIOData <- merge(DistrictInputs, DistrictOutputs, by = "district", all = TRUE)
colnames(DistrictIOData)[9:16] <- c("direct_output", "indirect_output", "induced_output", "total_output",
                                     "direct_fte", "indirect_fte", "induced_fte", "total_fte")


## REGIONALIZE DATA ##
regions_crosswalk <- read.csv(file.path(input_path, paste0("District_Regions.csv")), fileEncoding = "UTF-8-BOM") #upload regions crosswalk

# Merge crosswalk to get regions for every district
DistrictIOData <- merge(DistrictIOData, regions_crosswalk, by = ("district"), all = TRUE)

# Relocate the region column to be next to the district column
DistrictIOData <- DistrictIOData %>%
  relocate(region, .after = district)

# Create new DF with data aggregated by region
Regionsag <- DistrictIOData %>% 
  select(-(district)) %>% #get rid of district column (it breaks the code below)
  group_by(region) %>% #group by region
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(district = "REGION") %>% #make district name "region"
  relocate(district, .before = region) #move the region column

# Vertically merge data aggregated by region with the districts IO data
DistrictIOData <- rbind(DistrictIOData, Regionsag)


## UTILIZE CENSUS DATA TO GET EACH DISTRICT'S AND REGION'S EMPLOYMENT AND POPULATION NUMBERs ##

# Read in variables needed to do census API call
Sys.setenv(CENSUS_KEY = c_key)

# Reload .Renviron
readRenviron("~/.Renviron")

# Check to see that the expected key is output in your R console
Sys.getenv("CENSUS_KEY")

state_call = paste0("state:", state_fips) 
regionin_call = paste0(state_call, "+congressional district:", dist_list)

# Run API calls to get population and employment data from Census ACS. Clean up data as needed
# DISTRICTS' POPULATION #
districts_pop_22 <- getCensus(
  name = "acs/acs5",
  vintage = c_year, 
  vars = c("NAME", "B01003_001E"), 
  region = "county (or part):*", 
  regionin = regionin_call)

districts_pop_22 <- aggregate(districts_pop_22$B01003_001E, by = list(districts_pop_22$congressional_district), FUN = sum) %>%
  rename(district = Group.1, pop = x) %>%
  mutate_if(is.character, as.numeric)

# DISTRICTS' EMPLOYMENT #
districts_emp_22 <- getCensus(
  name = "acs/acs5",
  vintage = c_year, 
  vars = c("NAME", "B23025_004E", "B23025_006E"), 
  region = "county (or part):*", 
  regionin = regionin_call)

districts_emp_22 <- districts_emp_22 %>%
  mutate(emp = B23025_004E + B23025_006E)

districts_emp_22 <- aggregate(districts_emp_22$emp, by = list(districts_emp_22$congressional_district), FUN = sum) %>%
  rename(district = Group.1, emp = x) %>%
  mutate_if(is.character, as.numeric)

# Merge the population and employment dataframes with the regional crosswalk
dist_reg_pop_emp <- Reduce(function(x,y) merge(x = x, y = y, by = "district"),
                                       list(districts_pop_22, districts_emp_22, regions_crosswalk))

# Regionalize the data and vertically merge back to the data frame with regions in it
RegionsPopEmpLandag <- dist_reg_pop_emp %>% 
  select(-(district)) %>%
  group_by(region) %>% #group by rollup, then district
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(district = "REGION") %>%
  relocate(district, .before = region)

dist_reg_pop_emp <- rbind(dist_reg_pop_emp, RegionsPopEmpLandag)
dist_reg_pop_emp <- dist_reg_pop_emp %>%
  relocate(region, .after = district) %>%
  rename(population = pop)

# Merge this data frame into a new, final IO data frame to have it all in one
DistrictIOData <- full_join(DistrictIOData, dist_reg_pop_emp)

# Calculate spending per 100k, output per 100k, employment per 100k (both input and output), % district employment, and % population
DistrictIOData <- DistrictIOData %>%
  mutate(input_spending_per_100k = (input_spending / population) * 100000,
         input_emp_per_100k = (input_emp / population) * 100000,
         econ_output_per_100k = (total_output / population) * 100000,
         fte_per_100k = (total_fte / population) * 100000,
         percent_district_emp = total_fte / emp)

DistrictIOData <- DistrictIOData %>%
  relocate(population, .after = region) %>%
  relocate(input_spending_per_100k, .after = input_spending) %>%
  relocate(input_emp_per_100k, .after = input_emp) %>%
  relocate(econ_output_per_100k, .after = total_output) %>%
  relocate(fte_per_100k, .after = total_fte) %>%
  select(-(c(emp)))


## FINAL STEP: write data into file. All done! ##
write.xlsx(DistrictIOData, file.path(output_path, paste0(year, "_district_input_output_data.xlsx")))

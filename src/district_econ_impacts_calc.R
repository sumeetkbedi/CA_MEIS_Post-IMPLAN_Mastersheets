## Code for doing district-county approximations to get final numbers on district economic output and FTEs ##
# Read in the District Economic Indicators Excel sheet for direct output and employment 
district_econ_indicators <- read.xlsx(file.path(temp_path, paste0(year, "_econ_indicators_by_district.xlsx")))

## Step 1: Direct Output AND Employment - use "district_econ_indicators" and select the district, direct impact, output, and employment columns
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
  rename(direct_district_FTE = employment) %>%
  select("district", "direct_district_FTE")
DirectDistrictEmployment$district <- as.numeric(gsub("[a-zA-Z ]", "",
                                                 gsub("-", "", DirectDistrictEmployment$district)))

DirectDistrict <- merge(DirectDistrictOutput, DirectDistrictEmployment, by = "district")

# Now read in the needed files for calculating the districts' indirect and induced output and employment
county_econ_indicators <- read.xlsx(file.path(temp_path, paste0(year, "_econ_indicators_by_county.xlsx")))
cd_proportion <- read.xlsx(file.path(raw_path, "county_district_proportion.xlsx")) %>%
  rename(county = County, district = District)
cd_proportion$county = tolower(cd_proportion$county)


## Step #2: Induced Output and Employment - grab the induced data from the County Econ Indicators file and merge them
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
  mutate(induced_district_FTE = InducedDistrict$induced_county_employment*InducedDistrict$`%`,
         induced_district_output = InducedDistrict$induced_county_output*InducedDistrict$`%`) %>%
  select("district", "induced_district_FTE", "induced_district_output")
InducedDistrict <- InducedDistrict %>%
  aggregate(by = list(InducedDistrict$district), FUN = sum) %>%
  select("Group.1", "induced_district_FTE", "induced_district_output") %>%
  rename(district = Group.1)


## Step 3: Indirect Output and Employment - grab the indirect data from the County Econ Indicators file and merge them
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

DistrictD_CountyI <- Reduce(function(x,y) merge(x = x, y = y, by = "district"),
                            list(DirectDistrictEmployment, DirectDistrictOutput, IndirectCountyProp))
DistrictD_CountyI <- DistrictD_CountyI %>%
  relocate(county, .before = district) %>%
  relocate('%', .after = district) %>%
  relocate(POP, .after= '%') %>%
  rename(countypop2010 = POP)

# Calculate the districts' 2010 population, which is used to apportion the district's share in each county, and multiply this share by the districts' direct output and employment
DistrictPop2010 <- aggregate(DistrictD_CountyI$countypop2010, by = list(DistrictD_CountyI$district), FUN = sum) %>%
  rename(district = Group.1, districtpop2010 = x)

DistrictD_CountyI <- merge(DistrictD_CountyI, DistrictPop2010, by = "district") %>%
  relocate(districtpop2010, .after= countypop2010)
DistrictD_CountyI <- DistrictD_CountyI %>%
  mutate(CountyDistrictShare = DistrictD_CountyI$countypop2010 / DistrictD_CountyI$districtpop2010)
DistrictD_CountyI <- DistrictD_CountyI %>%
  mutate(CountyDistrictDirectOutput = DistrictD_CountyI$direct_district_output * DistrictD_CountyI$CountyDistrictShare,
         CountyDistrictDirectEmployment = DistrictD_CountyI$direct_district_FTE * DistrictD_CountyI$CountyDistrictShare)

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
  rename(district = Group.1, indirect_district_FTE = x)

IndirectDistrict <- merge(IndirectDistrictOutput, IndirectDistrictEmployment)


## Step 4: Combine all data frames into one
DistrictOutputs <- Reduce(function(x,y) merge(x = x, y = y, by = "district"),
                          list(DirectDistrict, IndirectDistrict, InducedDistrict))
DistrictOutputs <- DistrictOutputs %>%
  relocate(indirect_district_output, .after = direct_district_output) %>%
  relocate(induced_district_output, .after = indirect_district_output) %>%
  relocate(indirect_district_FTE, .after = direct_district_FTE) %>%
  relocate(induced_district_FTE, .after = indirect_district_FTE)
DistrictOutputs <- DistrictOutputs %>%
  mutate(total_district_output = direct_district_output + indirect_district_output + induced_district_output,
         total_district_FTE = direct_district_FTE + indirect_district_FTE + induced_district_FTE) %>%
  relocate(total_district_output, .before = direct_district_FTE)

## Write into file - all done!
write.xlsx(DistrictOutputs, file.path(temp_path, paste0(year, "_district_outputs.xlsx")))
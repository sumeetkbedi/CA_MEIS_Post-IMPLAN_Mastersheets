## CODE FOR CREATING COUNTY INPUT-OUTPUT MASTERSHEET ##

## PART 1: INPUT DATA ##
# 1.1: Start with USAspending - read in cleaned contracts and grants data #
contracts <- read.csv(file.path(input_path, paste0(f_year, c_file)))
grants <- read.csv(file.path(input_path, paste0(f_year, g_file)))

# Aggregate the dataframes by county, rename columns, and then bring the dataframes together. Keep the usaspending total
contracts <- aggregate(contracts$federal_action_obligation, by = list(contracts$recipient_county_name), FUN = sum)
grants <- aggregate(grants$federal_action_obligation, by = list(grants$recipient_county_name), FUN = sum)

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
smartpay <- read_xlsx(file.path(input_path, paste0("SmartPay_FY_2021.xlsx")), sheet = 2)

# Rename and select the needed columns. Then modify county columns names and drop the total row at the end
smartpay <- smartpay %>% 
  rename(county = ...1, smartpay_spending = Total) %>%
  select(county, smartpay_spending)

smartpay$county = toupper(smartpay$county)
smartpay <- smartpay[!(smartpay$county=="TOTAL"),]


# 1.3: Read in and wrangle VA direct payments data. should have this already aggregated by county from master analysis run through #
va_benefits <- read.csv(file.path(input_path, paste0(f_year, va_direct_pay_c)))


# 1.4: Aggregate all 3 spending data sources by county to get total input spending per county #
input_spend <- Reduce(function(x,y) merge(x = x, y = y, by = "county", all = TRUE),
                         list(usaspending, va_benefits, smartpay))
input_spend <- input_spend %>% 
  mutate("input_spending" = event_value_spending + household_spending + smartpay_spending)


# 1.5: Read in and wrangle employment data, should have this already aggregated by county from master analysis run through #
input_emp <- read.xlsx("2021_employment_totals.xlsx", sheet = 1)

#reorganize employment sheet - NOTE!!! - the .1825 calculation reverts the employment number from FTEs to total jobs
## in other words, these employment numbers represent TOTAl jobs, NOT FTEs.
input_emp <- input_emp %>% 
  rename(County = county, Military_Personnel = mili_emp_notFTEs, Civilian_Personnel = implan_546, input_employment = total_input_emp) %>%
  select("County", "Civilian_Personnel", "Military_Personnel", "input_employment")


## 1.6: Merge employment and spending data together to get TOTAL input spending and employment for counties. remove previous variables from environment
county_inputs <- merge(input_spend, input_emp, by = "county", all = TRUE)
rm(contracts, grants, usaspending, smartpay, va_benefits)


## PART 2: OUTPUT DATA ##

# Read in the County Economic Indicators and County Tax Results Excel sheets
county_econ_indicators <- read.xlsx("Economic Indicators Counties 2021.xlsx")
county_tax_results <- read.xlsx("Tax Results Counties 2021.xlsx")

# Step 1: County Output AND Employment - use "county_econ_indicators" and select the county, output/employment impact type, output, and employment columns
EconOutput <- county_econ_indicators %>% 
  select ("County", "Impact", "total_Output") %>%
  rename(total_econ_output = total_Output)

OutputEmployment <- county_econ_indicators %>% 
  select ("County", "Impact", "total_employment") %>%
  rename(output_employment = total_employment)

# Step 2: County Tax Results - use "county_tax_results", add a column for total local tax revenue, 
#select the county, impact, and total tax revenue by local, state, and federal columns. 
#Then filter to only include the totals of each by total impact.

county_tax_results <- county_tax_results %>% 
  mutate("total_Local" = total_Sub_County_General + total_Sub_County_Special_District + total_County) %>%
  select("County", "Impact", "total_Local", "total_State", "total_Federal", "total") %>% 
  rename(tax_Local = total_Local, tax_State = total_State, tax_Federal = total_Federal, tax_total = total)

# Step 3: Combine all of the output data frames into one
CountyOutputs <- Reduce(function(x,y,z) merge(x = x, y = y, z = z, c("County", "Impact")), 
                        list(EconOutput, county_tax_results, OutputEmployment))

# Clean output data frames (remove unnecessary numbers from Impacts column)
CountyOutputs <- CountyOutputs %>%
  mutate(Impact = replace(Impact, Impact == "1 - Direct", "Direct")) %>%
  mutate(Impact = replace(Impact, Impact == "2 - Indirect", "Indirect")) %>%
  mutate(Impact = replace(Impact, Impact == "3 - Induced", "Induced"))

# Convert data to vertical
CountyOutputs <- CountyOutputs %>% 
  gather(variable, value, -(County:Impact)) %>%
  unite(temp, Impact, variable) %>%
  spread(temp, value)

## FINAL STEPS: Combine input and output data, and reorder column names
CountyIOData <- merge(CountyInputs, CountyOutputs, by = "County", all = TRUE)

## REGIONALIZE DATA
regions_crosswalk <- read_excel("Regions.xlsx") #upload regions crosswalk
colnames(regions_crosswalk) <- c("County", "Region") #change col names so they match dfs

# Merge crosswalk to get regions for every county
CountyIOData <- merge(CountyIOData, regions_crosswalk, by = ("County"), all = TRUE)

# Reorder columns
CountyIOData <- CountyIOData %>%
  select(c("County", "Region", "event_value_spending", "household_spending", "smartpay_c", "input_spending", "Civilian_Personnel", 
           "Military_Personnel", "input_employment", "Direct_total_econ_output", "Indirect_total_econ_output", 
           "Induced_total_econ_output", "Total_total_econ_output", "Direct_output_employment", "Indirect_output_employment", 
           "Induced_output_employment", "Total_output_employment", "Direct_tax_Local", "Indirect_tax_Local", 
           "Induced_tax_Local", "Total_tax_Local", "Direct_tax_State", "Indirect_tax_State", 
           "Induced_tax_State", "Total_tax_State","Direct_tax_Federal", "Indirect_tax_Federal", 
           "Induced_tax_Federal", "Total_tax_Federal", "Direct_tax_total", "Indirect_tax_total", "Induced_tax_total",
           "Total_tax_total")) %>%
  rename(Direct_econ_output = Direct_total_econ_output, Indirect_econ_output = Indirect_total_econ_output, 
         Induced_econ_output = Induced_total_econ_output, Total_econ_output = Total_total_econ_output, 
         Direct_output_FTE = Direct_output_employment, Indirect_output_FTE = Indirect_output_employment, 
         Induced_output_FTE = Induced_output_employment, Total_output_FTE = Total_output_employment, 
         Total_combined_tax = Total_tax_total)

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

#Add column that converts county names to title case (Del Norte instead of DEL NORTE) - this is for ease of use in making graphs
CountyIOData$County_Title <- str_to_title(CountyIOData$County)

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

## Industries by Impact (will be a separate sheet in the same spreadsheet)

# Import industries by impact master spreadsheet and crosswalk
industries_by_impact <- read_excel("Industries by Impact 2021.xlsx") # upload industries by impact for counties
ind_crosswalk <- read_excel("Industries by Impact Groupings.xlsx") # upload crosswalk

industries_by_impact <- industries_by_impact %>% 
  separate(Impact, c("industry_display","description"), sep = "( - )")

ind_crosswalk <- ind_crosswalk %>%
  select(industry_display, rollup) #get rid of un-needed columns

industries_by_impact <- merge(ind_crosswalk, industries_by_impact, by = ("industry_display")) #merge by industry_display
industries_by_impact <- industries_by_impact %>% 
  select(-(c(industry_display, description))) %>% #get rid of extra column (it breaks the code below)
  group_by(County, rollup) %>% #group by rollup, then county
  summarise_each(funs(sum)) #sum the columns

industries_by_impact <- merge(industries_by_impact, regions_crosswalk, by = ("County"), all = TRUE) #apply crosswalk to counties spreadsheet

#Move columns around so they match the other df 
industries_by_impact <- industries_by_impact %>%
  relocate(Region, .after = County)

#Regionalize data - create a separate df with industry info aggregated by region
industry_Regionsag <- industries_by_impact %>% 
  select(-(County)) %>% #get rid of impacts column (it breaks the code below)
  group_by(Region, rollup) %>% #group by region and rollup
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(County = "REGION") %>% #re-add county name but set them all to Region
  relocate(County, .before = Region) #move columns around for consistency

#Vertically merge regionalized data with industries by impact data
industries_by_impact <- rbind(industries_by_impact, industry_Regionsag)

#get rid of unnecessary columns, rename columns
industries_by_impact <- industries_by_impact %>%
  select(County, Region, rollup, total_Direct, total_Indirect, total_Induced, total) %>%
  rename(Direct = total_Direct, Indirect = total_Indirect, Induced = total_Induced, Total = total)

## EMPLOYMENT BY INDUSTRY ## (will be a separate sheet in the same spreadsheet)

# Import employment by industrymaster spreadsheet
employment_by_industry <- read_excel("Employment Industries by Impact 2021.xlsx") # upload industries by impact for counties

employment_by_industry <- employment_by_industry %>% 
  separate(Impact, c("industry_display","description"), sep = "( - )")

employment_by_industry <- merge(ind_crosswalk, employment_by_industry, by = ("industry_display")) #merge by industry_display

employment_by_industry  <- employment_by_industry %>% 
  select(-(c(industry_display, description))) %>% #get rid of extra columns (it breaks the code below)
  group_by(County, rollup) %>% #group by rollup, then county
  summarise_each(funs(sum)) #sum the columns

employment_by_industry  <- merge(employment_by_industry, regions_crosswalk, by = ("County"), all = TRUE) #apply crosswalk to counties spreadsheet

#Move columns around so they match the other df 
employment_by_industry <- employment_by_industry %>%
  relocate(Region, .after = County)

#Regionalize data - create a separate df with industry info aggregated by region
employment_Regionsag <- employment_by_industry %>% 
  select(-(County)) %>% #get rid of impacts column (it breaks the code below)
  group_by(Region, rollup) %>% #group by region and rollup
  summarise_each(funs(sum)) %>% #sum the columns
  mutate(County = "REGION") %>% #re-add county name but set them all to Region
  relocate(County, .before = Region) #move columns around for consistency

#Vertically merge regionalized data with industries by imact data
employment_by_industry <- rbind(employment_by_industry, employment_Regionsag)

#get rid of unnecessary columns, rename columns
employment_by_industry <- employment_by_industry %>%
  select(County, Region, rollup, total_Direct, total_Indirect, total_Induced, total) %>%
  rename(Direct = total_Direct, Indirect = total_Indirect, Induced = "total_Induced", Total = total)

#create list with dataframes
multisheetlist <- list(InputOutput = CountyIOData, Industry = industries_by_impact, EmploymentIndustry = employment_by_industry)

#write into multi-sheet excel file
write.xlsx(multisheetlist, paste0("County Input-Output Data detailed 2021.xlsx"))

# ALL DONE - This gives you a detailed spreadsheet of all the counties' input and output data for us to use for the factsheets, and a seperate sheet for the industry groupings

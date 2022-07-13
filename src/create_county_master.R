## CODE FOR CREATING COUNTY INPUT-OUTPUT MASTERSHEET ##

## PART 1: INPUT DATA ##
# 1.1: Start with USAspending - read in cleaned contracts and grants data #
contracts <- read.csv(file.path(temp_path, paste0(f_year, "_cleaned_contracts.csv")))
grants <- read.csv(file.path(temp_path, paste0(f_year, "_cleaned_grants.csv")))

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

## 1.2: Now grab the SmartPay data by county #
SmartPay <- read_xlsx("SmartPay_FY_2020.xlsx", sheet = 2) %>% 
  rename(County = ...1, smartpay_c = Total) %>%
  select("County", "smartpay_c")

SmartPay$County = toupper(SmartPay$County)
SmartPay<-SmartPay[!(SmartPay$County=="TOTAL"),]





## Step 3: Read in and wrangle VA Payments (for Household Spending data), selecting only the county and household spending by county. Then aggregate the spending by county, and rename the columns to its respective names.
HouseholdSpending <- read.csv("VA_Direct_Payments_Recipient_USA_CA.csv") %>%
  filter(primary_place_of_performance_state_name == "CALIFORNIA") %>%
  select("recipient_county_name", "total_obligated_amount") %>% #select needed columns
  rename(County = "recipient_county_name") 

HouseholdSpending <- aggregate(HouseholdSpending$total_obligated_amount, by = list(HouseholdSpending$County), FUN = sum) %>%
  rename(County = Group.1, household_spending = x)





## Step 4: Aggregate all 3 spending data sources by county to get total input spending per county
InputSpending <- Reduce(function(x,y) merge(x = x, y = y, by = "County", all = TRUE), list(Event_Value_Spending, HouseholdSpending, SmartPay))

InputSpending <- InputSpending %>% 
  mutate("input_spending" = event_value_spending + household_spending + smartpay_c)

## Step 5: Read in and wrangle 2021_employment_totals.xlsx
InputEmployment <- read.xlsx("2021_employment_totals.xlsx", sheet = 1)



#reorganize employment sheet - NOTE!!! - the .1825 calculation reverts the employment number from FTEs to total jobs
## in other words, these employment numbers represent TOTAl jobs, NOT FTEs.
InputEmployment <- InputEmployment %>% 
  rename(County = county, Military_Personnel = mili_emp_notFTEs, Civilian_Personnel = implan_546, input_employment = total_input_emp) %>%
  select("County", "Civilian_Personnel", "Military_Personnel", "input_employment")

## Step 6: Merge employment and spending data together to get TOTAL input spending and employment for counties
CountyInputs <- merge(InputSpending, InputEmployment, by = "County", all = TRUE)
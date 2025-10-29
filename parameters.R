## PARAMETERS FOR RUNNING THE MASTER CODE ##

# general variables and folder paths #
f_year = "2024"
year = "2025"
state = "CALIFORNIA"

output_path = file.path("output")
temp_path = file.path("data", "temp")
raw_path = file.path("data", "raw") 

# combine_results variables #
implan_res_c = file.path("data", "raw", "implan_results", "counties")
implan_res_d = file.path("data", "raw", "implan_results", "districts")

econ_indic_path = file.path("econ_indicators")
tax_res_path = file.path("tax_results")
ind_output_path = file.path("industry_output")
ind_emp_path = file.path("industry_emp")

econ_ind_csv = "econ indicators.csv"
econ_c_file = "_econ_indicators_by_county.xlsx"
econ_d_file = "_econ_indicators_by_district.xlsx"

tax_res_csv = "tax results.csv"
tax_c_file = "_tax_results_by_county.xlsx"

out_ind_csv = "industry output.csv"
out_c_file = "_industry_output_by_county.xlsx"

emp_ind_csv = "industry employment.csv"
emp_c_file = "_industry_employment_by_county.xlsx"

csv_pat = "*.csv"
xlsx_pat = ".xlsx"
inv = "inv"

ind_disp = "Industry Display"

# create_mastersheets variables #
input_path = file.path("data", "raw", "inputs")
c_file = "_cleaned_contracts.csv"
g_file = "_cleaned_grants.csv"
DOE = "Department of Energy"

s_file <- "SmartPay_FY_"

va_file = "_cleaned_va_benefits.csv"

emp_file <- "_direct_employment.xlsx"

cd_pop_file <- "county_to_52_districts_pop_crosswalk.csv"

reg_file = "regions.xlsx"

lf_file = "_EDD_labor_force_by_county.xlsx"

pop_file = "_DOF_population_by_county.xlsx"

ind_imp_file = "Industries by Impact Groupings.csv"

final_file = "_input_output_data.xlsx"

c_key = "8df2ad0b1d54d9a4a19c1d97bed2e94e44995571"
state_fips = "06" 
dist_list = "01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52" 
c_year = 2023
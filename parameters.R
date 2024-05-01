## PARAMETERS FOR RUNNING THE MASTER CODE ##

# general variables and folder paths #
f_year = "2022"
year = "2023"
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
tax_res_csv = "tax results.csv"
out_ind_csv = "industry output.csv"
emp_ind_csv = "industry employment.csv"

csv_pat = "*.csv"
inv = "inv"

ind_disp = "Industry Display"

# create_mastersheets variables #
input_path = file.path("data", "raw", "inputs")
c_file = "_cleaned_contracts.csv"
g_file = "_cleaned_grants.csv"

va_direct_pay_c = "_va_benefits_by_county.csv"
va_direct_pay_d = "_va_benefits_by_district.csv"

c_key = "8df2ad0b1d54d9a4a19c1d97bed2e94e44995571"
state_fips = "06" 
dist_list = "01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52" 
c_year = 2022
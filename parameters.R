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
output_indus_path = file.path("industry_output")
emp_indus_path = file.path("industry_employment")

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
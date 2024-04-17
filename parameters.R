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
output_indus_path = file.path("output_industries")
emp_indus_path = file.path("emp_industries")

econ_ind_xl = "econ indicators.xlsx"
tax_res_xl = "tax results.xlsx"
out_ind_xl = "industry output.xlsx"
emp_ind_xl = "industry employment.xlsx"

xlsx_pat = "*.xlsx"
inv = "inv"

ind_disp = "Industry Display"

# create_mastersheets variables #
input_path = file.path("data", "raw", "inputs")
c_file = "_cleaned_contracts.csv"
g_file = "_cleaned_grants.csv"

va_direct_pay_c = "_va_benefits_by_county.csv"
va_direct_pay_d = "_va_benefits_by_district.csv"
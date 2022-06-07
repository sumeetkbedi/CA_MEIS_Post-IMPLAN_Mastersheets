## PARAMETERS FOR RUNNING THE MASTER CODE ##

# general variables and folder paths #
f_year = "2020"
year = "2021"
state = "CALIFORNIA"

output_path = file.path("output")
temp_path = file.path("data", "temp")
raw_path = file.path("data", "raw") 

# combine_results variables #
implan_res_c = file.path("data", "raw", "implan_results", "counties")
implan_res_d = file.path("data", "raw", "implan_results", "districts")

econ_indic_path = file.path("econ_indicators")
emp_indus_path = file.path("emp_industries")
output_indus_path = file.path("output_industries")
tax_res_path = file.path("tax_results")

xlsx_pat = "*.xlsx"
inv = "Inverse"

* This program  sets up ONET data for use in occupational switching project
*
* au : David Wiczer
*
*

use "/home/wicze006/Desktop/onetstata/skills.dta", clear
*extract occupation codes
gen occ_2d = regexs(0) if(regexm(o_net_soc_cod, "[0-9][0-9]"))
destring occ_2d, replace
* need to do things to map this to census occupatiosn 2d?
egen skill_id = group(element_id),label
drop element_id
rename data_value skill_val
bysort occ_2d skill_id scale_id: egen skill_val_o2d = mean(skill_val)

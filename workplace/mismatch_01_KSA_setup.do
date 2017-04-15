/* Setup O*NET data for use with ASVAB tests
*
*  au : Satoshi Tanaka & David Wiczer
*  mdy: 3-15-14
*  mod: 7-26-14
*
*
* description: This file reads in the txt files for ability, knowledge and skills from the ONET website
* it then stacks them together and keeps only the 13 descriptors identified in the 'expert_mapping' to 
* relate to the ASVAB scores.  We apply the expert mapping, which aggregates the descriptors to ASVAB test categories.  
* We normalize so that each ASVAB category gets a weighted sum of the O*NET descriptors. 
* It then computes the mean importance score within each occ1990 code, a simple average over the SOC codes that map there.
* Now it is computing the average of the ranks, but should change it to the rank of the average.
* 
*/

/*------------------------------------------------------------------------------------*/

clear all

/*
* Create the cross walk from ONET to occ2000
*/

use  $onet/soc2000_2_coc2000.dta, clear

merge 1:m soc2000 using $onet/onet2000_2_soc2000.dta, generate (_merge_onet2soc)
replace coc2000 = 13  if soc2000 == "11-3040"
replace coc2000 = 121 if soc2000 == "15-3011"
replace coc2000 = 161 if soc2000 == "19-1020"
replace coc2000 = 161 if soc2000 == "19-1099"
replace coc2000 = 234 if soc2000 == "25-9099"
replace coc2000 = 402 if soc2000 == "35-2019"
replace coc2000 = 443 if soc2000 == "39-3099"
replace coc2000 = 570 if soc2000 == "43-6014"
replace coc2000 = 605 if soc2000 == "45-2031"
rename coc2000 occ2000

merge m:1 occ2000 using $onet/occ2000_2_occ90.dta, generate (_merge_occ2000_2_occ90)

drop if missing(O_NET_SOC_Code)
gen soc2d = regexs(0) if(regexm(soc2000, "[0-9][0-9]"))
replace occ90 = 905 if soc2d == "55" /* military guys */
replace occ90 = 417 if occ2000 == 374 | occ2000 == 375  /* fire fighters */


gen o_net_stem = regexs(0) if(regexm(O_NET_SOC_Code, "[0-9][0-9]-[0-9][0-9][0-9][0-9]"))
save $onet/ONET_2_occ.dta, replace

* this is replacing occupation codes for which we do not have ONET data
duplicates drop o_net_stem, force

save $onet/ONETstem_2_occ.dta, replace

/*------------------------------------------------------------------------------------*/

/*
* Reads in data from the *.TXT files ability, knowledge and skills from the O*NET
*/

insheet using $onet/Ability.TXT, clear

* Take the full ONET SOC code and also the first 2 digits as the stem
rename onetsoccode O_NET_SOC_Code
gen o_net_stem = regexs(0) if(regexm(O_NET_SOC_Code, "[0-9][0-9]-[0-9][0-9][0-9][0-9]"))
merge m:1 o_net_stem using $onet/ONETstem_2_occ.dta, generate(_merge_occ_A)

save $onet/ability.dta, replace

insheet using $onet/Knowledge.TXT, clear

rename onetsoccode O_NET_SOC_Code
gen o_net_stem = regexs(0) if(regexm(O_NET_SOC_Code, "[0-9][0-9]-[0-9][0-9][0-9][0-9]"))
merge m:1 o_net_stem using $onet/ONETstem_2_occ.dta, generate(_merge_occ_K)

save $onet/knowledge.dta, replace

insheet using $onet/Skills.TXT,clear

rename onetsoccode O_NET_SOC_Code
gen o_net_stem = regexs(0) if(regexm(O_NET_SOC_Code, "[0-9][0-9]-[0-9][0-9][0-9][0-9]"))
merge m:1 o_net_stem using $onet/ONETstem_2_occ.dta, generate(_merge_occ_S)

save $onet/skills.dta, replace

** Stack on top of eachother skills, ability and knowledge
append using $onet/ability.dta
append using $onet/knowledge.dta

save $onet/ONET_KSA.dta, replace

* keep the elements identified by the expert_mapping:

replace elementname = "Mathematics Knowledge" if elementid == "2.C.4.a"
replace elementname = "Mathematics SKill" if elementid == "2.A.1.e"

local mapping_group /*
*/      elementname == "Inductive Reasoning"	| elementname == "Written Comprehension" 	| /*
*/	elementname == "Oral Comprehension"	| elementname == "English Language"		| /*
*/	elementname == "Reading Comprehension"	| elementname == "Deductive Reasoning"		| /*
*/	elementname == "Mathematics Skill"	| elementname == "Technology Design"		| /*
*/	elementname == "Number Facility"	| elementname == "Mathematical Reasoning"	| /*
*/	elementname == "Mathematics Knowledge"	| elementname == "Information Ordering"		| /*
*/	elementname == "Science"		| elementname == "Biology"			| /*
*/	elementname == "Chemistry"		| elementname == "Computers and Electronics"	| /*
*/	elementname == "Physics"		| elementname == "Engineering and Technology"	| /*
*/	elementname == "Installation"		| elementname == "Building and Construction"	| /*
*/	elementname == "Troubleshooting"	| elementname == "Operation and Control"	| /*
*/	elementname == "Repairing"		| elementname == "Equipment Maintenance"	| /*
*/	elementname == "Mechanical"		| elementname == "Equipment Selection"          | /*
*/      elementname == "Mathematics Knowledge"  | elementname == "Mathematics SKill"

drop if scaleid == "LV" /* use only importance, as suggested by ASVAB tech doc below */
replace datavalue = datavalue/5.0 /* rescale to 0-1, not 0-5 */

egen KSA_num = group(elementname) if `mapping_group'
sort elementname
gen onetsort_n = _n

/* This matrix comes from http://www.asvabprogram.com/downloads/Technical_Chapter_2010.pdf, Table 10 */
matrix expert_mapping = ( /*
*/1.11	,	1.13	,	1.33	,	1.22	,	5.71	,	1.07	,	1	\ /*
*/1.22	,	1.22	,	1.11	,	1.22	,	1.29	,	4.07	,	1.71	\ /*
*/1.22	,	1.33	,	1.11	,	1.11	,	5.29	,	1.43	,	1.29	\ /*
*/1.78	,	2	,	1.56	,	1.56	,	1.79	,	2.57	,	5.29	\ /*
*/4.56	,	4.22	,	2.67	,	3.44	,	5.07	,	4.21	,	3.93	\ /*
*/1.67	,	2.11	,	1.22	,	1.56	,	3.08	,	5.07	,	3.79	\ /*
*/2.22	,	2.89	,	5.67	,	5.78	,	1.79	,	1.57	,	1.43	\ /*
*/1	,	1.33	,	1.11	,	1.22	,	1.43	,	3.43	,	3	\ /*
*/1.56	,	1.22	,	1.33	,	1.22	,	2.14	,	4.5	,	3.36	\ /*
*/3.67	,	4.33	,	3.11	,	4.89	,	4.57	,	3.71	,	3.14	\ /*
*/4.22	,	4.56	,	3.56	,	3.78	,	3.5	,	2.86	,	3.14	\ /*
*/1.22	,	1.11	,	1.44	,	1.22	,	1.64	,	4.71	,	4.29	\ /*
*/5.78	,	5.89	,	1.89	,	2.22	,	3.71	,	3.64	,	2.79	\ /*
*/5.89	,	6	,	1.67	,	1.67	,	3.71	,	3.36	,	2.93	\ /*
*/6	,	6	,	2	,	1.78	,	3.86	,	3.57	,	3.29	\ /*
*/1.11	,	1.67	,	1.11	,	1.11	,	2.43	,	5.57	,	2.5	\ /*
*/5.67	,	5.56	,	1.67	,	2.22	,	3.93	,	3.43	,	2.86	\ /*
*/1.11	,	1.11	,	1.11	,	1.11	,	3.93	,	4.29	,	3.71	\ /*
*/3	,	3.11	,	4.22	,	4.44	,	3.5	,	2.57	,	2.5	\ /*
*/2	,	2.11	,	1.33	,	1.22	,	5.86	,	4.79	,	3.43	\ /*
*/2.44	,	2.56	,	5.78	,	5.67	,	3.07	,	2.85	,	2.71	\ /*
*/1.11	,	1.11	,	1.11	,	1.11	,	1.14	,	3.57	,	3	\ /*
*/3.78	,	4.22	,	1.78	,	2.67	,	5.71	,	4.07	,	3.29	\ /*
*/1.44	,	1.67	,	1.11	,	1.11	,	2.29	,	4.71	,	3.93	\ /*
*/1.56	,	1.44	,	1.56	,	1.44	,	2.5	,	4.36	,	3.93	\ /*
*/4.11	,	3.44	,	6	,	5.89	,	4.21	,	3.07	,	3.29	)
matrix expert_mapping = expert_mapping/6.0  /*I'm not sure why the ASVAB guys make it out of 6 */

/* normalize by rowsum so each ASVAB has equal total weight */
matrix un = J(1,26,1.0)
matrix row_sum = un*expert_mapping
matrix list row_sum
forvalues t=1/7{
	forvalues s=1/26{
		matrix expert_mapping[`s',`t'] = expert_mapping[`s',`t']/row_sum[1,`t']
	}
	quietly gen ONET_ASVAB_`t' = .
}

forvalues s = 1/26{
	sum onetsort_n if KSA_num == `s', meanonly
	local s0 = r(min)
	local sT = r(max)
	forvalues t = 1/7{
		quietly replace ONET_ASVAB_`t' = expert_mapping[`s',`t']*datavalue in `s0'/`sT'
	}
	qui gen ONET_expert`s' = datavalue in `s0'/`sT'
}
sort O_NET_SOC_Code
forvalues t = 1/7{
	quietly by O_NET_SOC_Code: egen tmp_sum = total(ONET_ASVAB_`t')
	quietly replace ONET_ASVAB_`t' = tmp_sum
	drop tmp_sum
}

drop KSA_num onetsort_n

/*------------------------------------------------------------------------------------*/
/* generating onet scores */

sort elementid
gen onetsort_n = _n
egen KSA_num = group(elementid)
qui forvalues s = 1/120 {
        sum onetsort_n if KSA_num == `s'
	local s0 = r(min)
	local sT = r(max)	
	gen ONET_`s' = datavalue in `s0'/`sT'
}

sort occ90 elementid
qui forvalues s = 1/120 {
        replace ONET_`s' = ONET_`s'[_n-1] if occ90 == occ90[_n-1] & ONET_`s'[_n-1] != .
}

gsort +occ90 -elementid
qui forvalues s = 1/120 {
        replace ONET_`s' = ONET_`s'[_n-1] if occ90 == occ90[_n-1] & ONET_`s'[_n-1] != .
}

/*------------------------------------------------------------------------------------*/

/* Save now have ONET merged with ASVAB and ranks */
save $onet/ONET_ASVAB.dta, replace

/*------------------------------------------------------------------------------------*/

/* 
* Create the average within an occupation code (coarser than ONET)
*/
sort occ90
forvalues t = 1/7{
	by occ90 : egen mean90 = mean(ONET_ASVAB_`t')
	quietly replace ONET_ASVAB_`t'= mean90
	drop mean90
	/* Take the rank of the average, just for show: will do this in mismatch_04
 	sort ONET_ASVAB_`t'
	qui egen tmp_n = group(occ90)
	sum tmp_n, meanonly
	qui replace ONET_rnk_`t' = tmp_n/r(max)
	drop tmp_n */
}

forvalues s = 1/26{
	by occ90 : egen mean90 = mean(ONET_expert`s')
	quietly replace ONET_expert`s'= mean90
	drop mean90
}

forvalues s = 1/120{
	by occ90 : egen mean90 = mean(ONET_`s')
	quietly replace ONET_`s'= mean90
	drop mean90
}

**** replace occ90 with occ2000 if we are using occ2000 codessort 
duplicates drop occ90, force

keep ONET* occ90

/*------------------------------------------------------------------------------------*/

save $onet/ONET_ASVAB_occ90.dta, replace


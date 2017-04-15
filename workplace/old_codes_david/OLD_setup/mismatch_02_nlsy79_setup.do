/*--------------------------------------------------------------------------------------
* name: mismatch_01_setup_do
* made by: satoshi tanaka
* date: 08/21/2010
*       06/05/2012
* description: this code is for the project 'occupation skill mismatch'
--------------------------------------------------------------------------------------*/

* this code is to read the nlsy79 from the dictionary, name and label variables

/*------------------------------------------------------------------------------------*/

/* run dictionary for demographics data */
infile using $download/demographics/demographics.dct

/* put lavels */
run $download/demographics/demographics-value-labels

/* rename variables */
run $download/demographics/demographics-rename.do

/* create a variable list for later use*/
#delimit ;
local vlist_demo
AGEATINT_
C_SAMPWEIGHT_
ENROLLMTREV_
FAMSIZE_
HGC_
HGCREV_
MARSTAT_KEY_
SAMPWEIGHT_
Q3_8_
Q3_10B_
Q3_8A_
;
#delimit cr

/* Reshape original wide data from NLSY79 to long data */ 
reshape long `vlist_demo', i(CASEID) j(year)

/* rename variables for convenience */
rename	CASEID	id
rename	SAMPLE_RACE	race
rename	SAMPLE_ID	sample_id
rename	SAMPLE_SEX	sex
rename	AGEATINT_	age
rename	ASVAB_1	asvab_check_env
rename	ASVAB_2	asvab_check_hs
rename	ASVAB_3	asvab_sec01
rename	ASVAB_4	asvab_sec02
rename	ASVAB_5	asvab_sec03
rename	ASVAB_6	asvab_sec04
rename	ASVAB_7	asvab_sec05
rename	ASVAB_8	asvab_sec06
rename	ASVAB_9	asvab_sec07
rename	ASVAB_10	asvab_sec08
rename	ASVAB_11	asvab_sec09
rename	ASVAB_12	asvab_sec10
rename	ASVAB_13	asvab_sec01_scale
rename	ASVAB_14	asvab_sec01_stderr
rename	ASVAB_15	asvab_sec02_scale
rename	ASVAB_16	asvab_sec02_stderr
rename	ASVAB_17	asvab_sec03_scale
rename	ASVAB_18	asvab_sec03_stderr
rename	ASVAB_19	asvab_sec04_scale
rename	ASVAB_20	asvab_sec04_stderr
rename	ASVAB_21	asvab_sec05_scale
rename	ASVAB_22	asvab_sec05_stderr
rename	ASVAB_23	asvab_sec06_scale
rename	ASVAB_24	asvab_sec06_stderr
rename	ASVAB_25	asvab_sec07_scale
rename	ASVAB_26	asvab_sec07_stderr
rename	ASVAB_27	asvab_sec08_scale
rename	ASVAB_28	asvab_sec08_stderr
rename	ASVAB_29	asvab_sec09_scale
rename	ASVAB_30	asvab_sec09_stderr
rename	ASVAB_31	asvab_sec10_scale
rename	ASVAB_32	asvab_sec10_stderr
rename	ASVAB_33	asvab_sec01_std
rename	ASVAB_34	asvab_sec02_std
rename	ASVAB_35	asvab_sec03_std
rename	ASVAB_36	asvab_sec04_std
rename	ASVAB_37	asvab_sec05_std
rename	ASVAB_38	asvab_sec06_std
rename	ASVAB_39	asvab_sec07_std
rename	ASVAB_40	asvab_sec08_std
rename	ASVAB_41	asvab_sec09_std
rename	ASVAB_42	asvab_sec10_std
rename	ASVAB_43	asvab_verb_std
rename	C_SAMPWEIGHT_	csweight
rename	ENROLLMTREV_	enrollmt
rename	FAMSIZE_	famsize
rename	HGC_	grade
rename	HGCREV_	grade_rev
rename	MARSTAT_KEY_	marst
rename	SAMPWEIGHT_	sweight
rename	Q3_8_	hsdiploma
rename	Q3_10B_	degree
rename	Q3_8A_	ged

rename year intvw_num
replace intvw_num = intvw_num - 1978 if intvw_num <= 1994
replace intvw_num = (intvw_num - 1994)/2 + 16 if intvw_num > 1994
/* saving demographics data */
save $result/demographics_01.dta, replace

/*------------------------------------------------------------------------------------*/

clear
/* run dictionary for employment data */
infile using $download/employment/employment.dct

/* put lavels */
run $download/employment/employment-value-labels

/* rename variables */
run $download/employment/employment-rename.do

/* create a variable list for later use*/
#delimit ;
local vlist_emp
COWALL_EMP1_
COWALL_EMP2_
COWALL_EMP3_
COWALL_EMP4_
COWALL_EMP5_
Q6_56_
QES1_52_
QES2_52_
QES3_52_
QES4_52_
QES5_52_
HRP1_
HRP2_
HRP3_
HRP4_
HRP5_
Q5_26D_
QES1_52A_
QES2_52A_
QES3_52A_
QES4_52A_
QES5_52A_
OCCALL_EMP1_
OCCALL_EMP2_
OCCALL_EMP3_
OCCALL_EMP4_
OCCALL_EMP5_
QES1_55I_
QES2_55I_
QES3_55I_
QES4_55I_
QES5_55I_
Q6_53_
PREV_EMP1_
PREV_EMP2_
PREV_EMP3_
PREV_EMP4_
PREV_EMP5_
INDALL_EMP1_
INDALL_EMP2_
INDALL_EMP3_
INDALL_EMP4_
INDALL_EMP5_
QES1_55F_
QES2_55F_
QES3_55F_
QES4_55F_
QES5_55F_
Q6_52_
QES1_56KB_
QES2_56KB_
QES3_56KB_
QES4_56KB_
QES5_56KB_
QES1_56KA_
QES2_56KA_
QES3_56KA_
QES4_56KA_
QES5_56KA_
CPSHRP_
;
#delimit cr

/* Reshape original wide data from NLSY79 to long data */ 
reshape long `vlist_emp', i(CASEID) j(year)

/* rename variables for convenience */
rename	CASEID	id
rename	SAMPLE_ID_1979 sample_id
rename	COWALL_EMP1_	cow_emp1
rename	COWALL_EMP2_	cow_emp2
rename	COWALL_EMP3_	cow_emp3
rename	COWALL_EMP4_	cow_emp4
rename	COWALL_EMP5_	cow_emp5
rename	Q6_56_	cow_cps
rename	QES1_52_	check_cps_emp1
rename	QES2_52_	check_cps_emp2
rename	QES3_52_	check_cps_emp3
rename	QES4_52_	check_cps_emp4
rename	QES5_52_	check_cps_emp5
rename	HRP1_	wage_emp1
rename	HRP2_	wage_emp2
rename	HRP3_	wage_emp3
rename	HRP4_	wage_emp4
rename	HRP5_	wage_emp5
rename	Q5_26D_	hrwkd_cps
rename	QES1_52A_	hrwkd_emp1
rename	QES2_52A_	hrwkd_emp2
rename	QES3_52A_	hrwkd_emp3
rename	QES4_52A_	hrwkd_emp4
rename	QES5_52A_	hrwkd_emp5
rename	OCCALL_EMP1_	occ_all_emp1
rename	OCCALL_EMP2_	occ_all_emp2
rename	OCCALL_EMP3_	occ_all_emp3
rename	OCCALL_EMP4_	occ_all_emp4
rename	OCCALL_EMP5_	occ_all_emp5
rename	QES1_55I_	occ_emp1
rename	QES2_55I_	occ_emp2
rename	QES3_55I_	occ_emp3
rename	QES4_55I_	occ_emp4
rename	QES5_55I_	occ_emp5
rename	Q6_53_	occ_cps
rename	PREV_EMP1_	prev_emp1
rename	PREV_EMP2_	prev_emp2
rename	PREV_EMP3_	prev_emp3
rename	PREV_EMP4_	prev_emp4
rename	PREV_EMP5_	prev_emp5
rename	INDALL_EMP1_	ind_all_emp1
rename	INDALL_EMP2_	ind_all_emp2
rename	INDALL_EMP3_	ind_all_emp3
rename	INDALL_EMP4_	ind_all_emp4
rename	INDALL_EMP5_	ind_all_emp5
rename	QES1_55F_	ind_emp1
rename	QES2_55F_	ind_emp2
rename	QES3_55F_	ind_emp3
rename	QES4_55F_	ind_emp4
rename	QES5_55F_	ind_emp5
rename	Q6_52_	ind_cps
rename	QES1_56KB	occ_last_emp1
rename	QES2_56KB	occ_last_emp2
rename	QES3_56KB	occ_last_emp3
rename	QES4_56KB	occ_last_emp4
rename	QES5_56KB	occ_last_emp5
rename	QES1_56KA	ind_last_emp1
rename	QES2_56KA	ind_last_emp2
rename	QES3_56KA	ind_last_emp3
rename	QES4_56KA	ind_last_emp4
rename	QES5_56KA	ind_last_emp5
rename	CPSHRP	wage_cps

rename year intvw_num
replace intvw_num = intvw_num - 1978 if intvw_num <= 1994
replace intvw_num = (intvw_num - 1994)/2 + 16 if intvw_num > 1994
/* saving demographics data */
save $result/employment_01.dta, replace

/*------------------------------------------------------------------------------------*/

clear
/* run dictionary for weekly work history data */
infile using $download/weekly/weekly.dct

/* put lavels */
run $download/weekly/weekly-value-labels

/* rename variables */
run $download/weekly/weekly-rename.do

/* create a variable list for later use*/
#delimit ;
local vlist_weekly
dual_emp1_
dual_emp2_
dual_emp3_
dual_emp4_
lab_status_
;
#delimit cr

/* Reshape original wide data from NLSY79 to long data */ 
reshape long `vlist_weekly', i(CASEID) j(time)

/* rename variables for convenience */
rename	CASEID	id
rename	dual_emp1_	dual_emp1
rename	dual_emp2_	dual_emp2
rename	dual_emp3_	dual_emp3
rename	dual_emp4_	dual_emp4
rename	lab_status_	lab_status

/* saving demographics data */
save $result/weekly_01.dta, replace


/*-----------------------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
/*---------------------Setup health data------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
/*-----------------------------------------------------------------------------------*/
clear all
infile using $download/health/health.dct, clear

ssc install tolower

do $download/health/health-value-labels.do


rename caseid_1979 id
rename sample_race_1979 race
rename sample_sex_1979 sex
rename h50sf12_4b_xrnd hlth_limwk_x
rename h50fl_2_000001_xrnd hlth_runmile
rename h50fl_2_000002_xrnd hlth_walksevblock
rename h50fl_2_000003_xrnd hlth_walk1block
rename h50fl_2_000004_xrnd hlth_sit
rename h50fl_2_000005_xrnd hlth_situp
rename h50fl_2_000006_xrnd hlth_sevstairs
rename h50fl_2_000007_xrnd hlth_1stairs
rename h50fl_2_000008_xrnd hlth_liftcarry
rename h50fl_2_000009_xrnd hlth_kneel
rename h50fl_2_000010_xrnd hlth_pickup
rename h50fl_2_000011_xrnd hlth_raisearms
rename h50fl_2_000012_xrnd hlth_pullpush

rename   h40_sf12_pcs_score_xrnd hlth_composite

reshape long q11_4_ q11_5_ q11_9_ q11_10_a_ q11_10_b_ health_height_ , i(id) j(year)

gen height = health_height_ if year == 1985 | year == 1982
replace height = round(health_height_/100)*12 + health_height_ - round(health_height_/100)*100 if year ==1981
replace height = q11_10_b_ + q11_10_a_*12 if q11_10_b_<=12
replace height = q11_10_b_ if q11_10_b_>12 & q11_10_b_<.
replace height = . if height<50 | height> 90 /* there're some people who answered crazy and also throw out height<0 */

xtset id year
by id: egen tmp_height = max(height)
replace height = tmp_height if height>=. & year>1985
drop tmp_height
/* first fill in non survey years when they were growing , 1981-1985 */
by id: ipolate height year if year<=1985, gen(tmp_height)
replace height = tmp_height if height>=. & tmp_height<.
/* now replace non-response with interpolated height */
drop tmp_height
by id: ipolate height year if year<=1986, gen(tmp_height)
replace height = tmp_height if height>=. & tmp_height<.
/* still leaves non-responses, give them adult height */
drop tmp_height
by id: egen tmp_height = max(height)
replace height = tmp_height if height>=. & tmp_height<.

rename q11_9_ weight
rename q11_5_ hlth_limamtwk
rename q11_4_ hlth_limkndwk

gen BMI = weight/(height^2)*703

label var hlth_limamtwk "Health Limits Amount of Work?"
label var hlth_limkndwk "Health Limits Kind of Work?"

drop race sex

save $result/health_nlsy79.dta, replace 

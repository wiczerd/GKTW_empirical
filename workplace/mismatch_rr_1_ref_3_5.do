/*--------------------------------------------------------------------------------------
* name: mismatch_04_regression.do
* made by: david wiczer, moidfied by: satoshi tanaka
* date: 08/21/2010
*       04/03/2015
* description: this code is for the project 'occupation skill mismatch'
--------------------------------------------------------------------------------------*/

global diminitls "vms_rr_1_ref_3_5"

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* sample selection */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

set more off
use $data/yearly_02.dta, clear

/*------------------------------------------------------------------------------------*/

/* choose cross-sectional male sample */

keep if sample_id >= 1 & sample_id <= 4

/*------------------------------------------------------------------------------------*/

/* this says how many people and how many obs we have */
xtset id year
xtdes
count

/*------------------------------------------------------------------------------------*/

drop if (occ <= 0 | occ == . | ind <= 0 | ind == .)

/*------------------------------------------------------------------------------------*/

/* this says how many people and how many obs we have */
xtset id year
xtdes
count

/*------------------------------------------------------------------------------------*/

/* replace age and race if missing */
sort id year
by id: replace age = age[_n-1] + year - year[_n-1] if age == . & age[_n-1] != .
by id: replace race = race[_n-1] if race == . & race[_n-1] != .
gsort +id -year
by id: replace age = age[_n-1] - year + year[_n-1] if age == . & age[_n-1] != .
by id: replace race = race[_n-1] if race == . & race[_n-1] != .

drop if age == .
drop if age < 16

/*------------------------------------------------------------------------------------*/

/* this says how many people and how many obs we have */
xtset id year
xtdes
count

/*------------------------------------------------------------------------------------*/

/* replace grade if missing */
sort id year
by id: replace grade = grade[_n-1] if grade == . & grade[_n-1] != .
gsort +id -year
by id: replace grade = grade[_n-1] if grade == . & grade[_n-1] != .

/* drop if the person is still in school */
gen flag_1 = (age - grade - 6) < 0 
gsort id -year
replace flag_1 = 1 if flag_[_n-1] == 1 & id == id[_n-1]
tab flag_1

drop if grade == .
drop if flag_1 == 1
drop flag_1

/*------------------------------------------------------------------------------------*/

/* this says how many people and how many obs we have */
xtset id year
xtdes
count

/*------------------------------------------------------------------------------------*/

/* drop if no valid asvab scores */
sort id year
foreach i in 01 02 03 04 05 06 07 08 09 10 {
replace asvab_sec`i' = asvab_sec`i'[_n-1] if asvab_sec`i'[_n-1] != . & asvab_sec`i' == . & id == id[_n-1]
}
gsort +id -year
foreach i in 01 02 03 04 05 06 07 08 09 10 {
replace asvab_sec`i' = asvab_sec`i'[_n-1] if asvab_sec`i'[_n-1] != . & asvab_sec`i' == . & id == id[_n-1]
}

drop if (asvab_sec01 == . | asvab_sec01 < 0)

/*------------------------------------------------------------------------------------*/

/* this says how many people and how many obs we have */
xtset id year
xtdes
count

/*------------------------------------------------------------------------------------*/

/* create employer switch */
gen switch_emp = 0
replace switch_emp = 1 if id == id[_n-1] & job_number != job_number[_n-1]

/* create occupation switch */
sort id year
gen switch_occ = 0        
replace switch_occ = 1 if id == id[_n-1] & occ != occ[_n-1]

/* create industry switch */
gen switch_ind = 0        
replace switch_ind = 1 if id == id[_n-1] & ind != ind[_n-1]

/*------------------------------------------------------------------------------------*/

/* create tenure variable */
sort id year
                                                                                                                             /* check later */
gen tenure_emp = 1 /* (fulltime >= 1200) */
replace tenure_emp = tenure_emp + tenure_emp[_n-1] if id == id[_n-1] & switch_emp != 1
gen tenure_occ = 1 /* (fulltime >= 1200) */
replace tenure_occ = tenure_occ + tenure_occ[_n-1] if id == id[_n-1] & switch_occ != 1
gen tenure_ind = 1 /* (fulltime >= 1200) */
replace tenure_ind = tenure_ind + tenure_ind[_n-1] if id == id[_n-1] & switch_ind != 1
gen exp = 1 /* (fulltime >= 1200) */
replace exp = exp + exp[_n-1] if id == id[_n-1]

/*------------------------------------------------------------------------------------*/

/* take out time effects: index wages to average wage */
merge m:1 year using $base_folder/cpi/PCE_deflator.dta
drop if _merge==2
sum PCE if year==2000, meanonly
local PCE_2000 = r(mean)
replace PCE = PCE/`PCE_2000'
gen rwage = wage/PCE
drop _merge

/* generate logged valued of wages and earnings */
gen lwage = log(rwage)

/* selection if wage is not negative */
drop if wage < 0

/*------------------------------------------------------------------------------------*/

/* this says how many people and how many obs we have */
xtset id year
xtdes
count

/*------------------------------------------------------------------------------------*/

/* selection by real wage, if in top 0.1% or in bottom 0.1% */
forvalues i = 1/24 {
_pctile wage if intvw_num == `i', p(0.1, 99.9)
drop if wage <= r(r1) & intvw_num == `i'
drop if wage >= r(r2) & intvw_num == `i'
}

/*------------------------------------------------------------------------------------*/

/* this says how many people and how many obs we have */
xtset id year
xtdes
count

save $data/yearly_03.dta, replace

/*------------------------------------------------------------------------------------*/

/* descriptive statistics: all sample */
sort id year
xtset id year

by id: egen max_grade = max(grade) 

xtdes
count
su age
xttab max_grade
tab max_grade
xttab race
su switch_occ
univar tenure_occ
su switch_emp
univar tenure_emp
su fulltime

drop max_grade

/*------------------------------------------------------------------------------------*/

/* descriptive statistics: <= high school */

use $data/yearly_03.dta, clear

sort id year
xtset id year

by id: egen max_grade = max(grade)
keep if max_grade <= 12

xtdes
count
su age
xttab max_grade
tab max_grade
xttab race
su switch_occ
univar tenure_occ
su switch_emp
univar tenure_emp
su fulltime

drop max_grade

/*------------------------------------------------------------------------------------*/

/* descriptive statistics: > high school */

use $data/yearly_03.dta, clear

sort id year
xtset id year

by id: egen max_grade = max(grade)
keep if max_grade > 12

xtdes
count
su age
xttab max_grade
tab max_grade
xttab race
su switch_occ
univar tenure_occ
su switch_emp
univar tenure_emp
su fulltime

drop max_grade


/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* preparation for regression */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

use $data/yearly_03.dta, clear

/* generate some  other covariates */

gen univ = (grade>=16)
gen hs = (grade>=12)
gen lths = (grade<12)

gen hispanic = (race == 1)
gen black = (race == 2)
gen yob = year - age

label var lwage	"ln(Wage)"
label var univ	"4-Year College"
label var hs	"High School"
label var lths	"< High School"

label var hispanic	"Hispanic"
label var black	"Black"
label var yob "Year of Birth"

/*------------------------------------------------------------------------------------*/

/* create weights */
sort id year
by id: gen ind_indic = (_n==1)
gen ones = 1
egen swt = total(ones), by(id)
replace swt = 1/swt

sort occ
by occ: gen occ_indic = (_n==1)
by occ: egen occ_wt = total(ones)

/*------------------------------------------------------------------------------------*/

/* define higher order terms for tenure variables */

sort id year
gen ten_emp2 = tenure_emp^2/100
gen ten_emp3 = tenure_emp^3/100
gen ten_occ2 = tenure_occ^2/100
gen ten_occ3 = tenure_occ^3/100
gen exp2 = exp^2/100
gen exp3 = exp^3/100
gen age2 = age^2/100
gen oj = (tenure_emp > 1)

gen job_part = (oj == 0)
replace job_part = sum(job_part)
sort job_part	
by job_part: egen ten_emp_iv = mean(tenure_emp)
replace ten_emp_iv = tenure_emp - ten_emp_iv
by job_part: egen ten_emp2_iv = mean(ten_emp2)
replace ten_emp2_iv = ten_emp2 - ten_emp2_iv
by job_part: egen oj_iv = mean(oj)
replace oj_iv = oj - oj_iv
	
gen occ_part = (tenure_occ == 1 | id != id[_n-1])
replace occ_part = sum(occ_part)
sort occ_part
by occ_part: egen ten_occ_iv = mean(tenure_occ)
replace ten_occ_iv = tenure_occ - ten_occ_iv
by occ_part: egen ten_occ2_iv = mean(ten_occ2)
replace ten_occ2_iv = ten_occ2 - ten_occ2_iv
by occ_part: egen ten_occ3_iv = mean(ten_occ3)
replace ten_occ3_iv = ten_occ3 - ten_occ3_iv

by occ_part: egen lwage_occ_iv = mean(lwage)
replace lwage_occ_iv = lwage - lwage_occ_iv 
by occ_part: egen lwage_occ2_iv = mean(lwage^2)
replace lwage_occ2_iv = lwage^2 - lwage_occ2_iv 

sort id exp
by id: egen exp_iv = mean(exp)
replace exp_iv = exp - exp_iv
by id: egen exp2_iv = mean(exp2)
replace exp2_iv = exp2 - exp2_iv
by id: egen exp3_iv = mean(exp3)
replace exp3_iv = exp3 - exp3_iv

label var tenure_emp	"Emp Tenure"
label var ten_emp2	"Emp Tenure$^2\times$ 100"
label var tenure_occ	"Occ Tenure"
label var ten_occ2	"Occ Tenure$^2\times$ 100"
label var ten_occ3	"Occ Tenure$^3\times$ 100"
label var exp	"Experience"
label var exp2	"Experience$^2\times$ 100"
label var exp3	"Experience$^3\times$ 100"
label var oj	"Old Job"
label var age	"Age"
label var age2	"Age$^2$"

/*------------------------------------------------------------------------------------*/
/* combining ASVAB */

/* create AFQT (1980 basis) scores: */
gen AFQT = asvab_sec02 + asvab_sec03 + asvab_sec04 + asvab_sec05/2
replace AFQT = log(AFQT)

/* drop asvab scores not in use */
drop asvab_sec05-asvab_sec07 asvab_sec05_std-asvab_sec07_std

/* renumber them */
rename asvab_sec01 asvab_sec05
rename asvab_sec02 asvab_sec01
rename asvab_sec08 asvab_sec02
rename asvab_sec09 asvab_sec06
rename asvab_sec10 asvab_sec07

/* renumber them */
rename asvab_sec01_std asvab_sec05_std
rename asvab_sec02_std asvab_sec01_std
rename asvab_sec08_std asvab_sec02_std
rename asvab_sec09_std asvab_sec06_std
rename asvab_sec10_std asvab_sec07_std

drop asvab_sec??_scale asvab_sec??_stderr

rename asvab_sec01 asvab_sec1
rename asvab_sec02 asvab_sec2
rename asvab_sec03 asvab_sec3
rename asvab_sec04 asvab_sec4
rename asvab_sec05 asvab_sec5
rename asvab_sec06 asvab_sec6
rename asvab_sec07 asvab_sec7

/* clean the age effects from tests by means */
gen tmp = age -(year-1980) 
bysort id: egen tmp2 = min(tmp)
qui tab tmp2, gen(I_enterage)

/* make the standard deviation the same across birth cohorts */
qui forvalues i=1/7{
	sum asvab_sec`i' if tmp2 == 20 & ind_indic == 1
	local sd20 = r(sd)
	forvalues ai=16/24{
		sum asvab_sec`i' if tmp2 ==`ai' & ind_indic == 1
		replace asvab_sec`i' = asvab_sec`i'/r(sd)*`sd20' if tmp2 ==`ai' & ind_indic == 1
	}
}

/* take out the cohort effects on mean from asvab scores*/
sort id year
qui forvalues i=1/7{
	reg asvab_sec`i' I_enterage1-I_enterage8 if ind_indic == 1
	predict asvab_res`i', residual
	replace asvab_res`i' = asvab_res`i'[_n-1] if id == id[_n-1]
}

qui reg AFQT I_enterage1-I_enterage8 if ind_indic == 1
qui predict AFQT_res, residual
sort id year
replace AFQT_res = AFQT_res[_n-1] if id == id[_n-1]

/* take out the cohort effects on mean from social ability scores */
qui reg rotter_score I_enterage1-I_enterage8 if ind_indic == 1
predict social_res1, residual
replace social_res1 = -social_res1
sort id year
replace social_res1 = social_res1[_n-1] if id == id[_n-1]

qui reg rosenberg_score I_enterage1-I_enterage8 if ind_indic == 1
predict social_res2, residual
sort id year
replace social_res2 = social_res2[_n-1] if id == id[_n-1]

/*------------------------------------------------------------------------------------*/
/* normalize everything to have standard deviation of 1 */

/* asvab */
qui forvalues i=1/7{
	sum asvab_res`i'
	gen asvab_std`i' = (asvab_res`i'-r(min))/r(sd)
}
/* afqt */ 
qui sum AFQT_res
qui gen AFQT_std = (AFQT_res-r(min))/r(sd)
label var AFQT_std "AFQT"

/* onet */
qui forvalues s = 1/7 {
	qui sum ONET_ASVAB_`s'
	replace ONET_ASVAB_`s' = (ONET_ASVAB_`s'-r(min))/r(sd)
}

qui forvalues s = 1/120 {
	qui sum ONET_`s'
	replace ONET_`s' = (ONET_`s'-r(min))/r(sd)
}

/* social ability */
qui forvalues i=1/2{
	qui sum social_res1
	gen social_std`i' = (social_res`i'-r(min))/r(sd)
}

/*------------------------------------------------------------------------------------*/
                                                                                                                             /* check later */
/* verbal skills */
pca ONET_ASVAB_3-ONET_ASVAB_4, components(1)
*pca ONET_1-ONET_4 ONET_53-ONET_56 ONET_112 ONET_113, components(1)
predict skill_v, score
sum skill_v if skill_v != .
replace skill_v = (skill_v - r(min))/r(sd)  if skill_v != .

/* math skills */
pca ONET_ASVAB_1-ONET_ASVAB_2, components(1)
*pca ONET_12 ONET_13 ONET_57 ONET_58 ONET_102 ONET_103, components(1)
predict skill_m, score
sum skill_m if skill_m != .
replace skill_m = (skill_m - r(min))/r(sd)  if skill_m != .

/* social skill */
pca ONET_63-ONET_68, components(1)
predict skill_s, score
sum skill_s if skill_s != .
replace skill_s = (skill_s - r(min))/r(sd)  if skill_s != .

/*------------------------------------------------------------------------------------*/

/* verbal ability */
pca asvab_std3-asvab_std4 if ind_indic == 1, components(1)
predict ability_v, score
sum ability_v if ind_indic == 1
replace ability_v = (ability_v - r(min))/r(sd) if ind_indic == 1
sort id year
replace ability_v = ability_v[_n-1] if id == id[_n-1]

/* math ability */
pca asvab_std1-asvab_std2 if ind_indic == 1, components(1)
predict ability_m, score
sum ability_m if ind_indic == 1
replace ability_m = (ability_m - r(min))/r(sd) if ind_indic == 1
sort id year
replace ability_m = ability_m[_n-1] if id == id[_n-1]

/* social ability */
pca social_std1 social_std2 if ind_indic == 1, components(1)
predict ability_s, score
sum ability_s if ind_indic == 1
replace ability_s = (ability_s - r(min))/r(sd) if ind_indic == 1
sort id year
replace ability_s = ability_s[_n-1] if id == id[_n-1]

corr asvab_std3-asvab_std4 
corr asvab_std1-asvab_std2
corr social_std1 social_std2

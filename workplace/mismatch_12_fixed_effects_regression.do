/*--------------------------------------------------------------------------------------
* name: mismatch_04_regression.do
* made by: david wiczer, moidfied by: satoshi tanaka
* date: 08/21/2010
*       04/03/2015
* description: this code is for the project 'occupation skill mismatch'
--------------------------------------------------------------------------------------*/

global diminitls "vms_fe"

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

/*------------------------------------------------------------------------------------*/

/* export the correlation matrix between health and cog */
corr ability_v ability_m ability_s skill_v skill_m skill_s
matrix corr_vmp = r(C)
matrix colnames corr_vmp = "W_Verb" "W_Math" "W_Soc" "O_Verb" "O_Math" "O_Soc" 
matrix rownames corr_vmp = "Worker_Verb" "Worker_Math" "Worker_Soc" "Occ_Verb" "Occ_Math" "Occ_Soc"
putexcel A1=matrix(corr_vmp, names) using ${result}/table_${diminitls}_corr.xls, replace

/*------------------------------------------------------------------------------------*/
                                                                                                                             /* check later */
global zlist
*global zlist lths univ hispanic black
*global zlist univ hispanic black AFQT_std

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* generate mismatch measure */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

foreach i in "v" "m" "s" {
	cumul ability_`i' if ind_indic == 1, gen(ability_rnk_tmp) equal
	bysort id: egen ability_rnk_`i' = max(ability_rnk_tmp)
	drop ability_rnk_tmp
	cumul skill_`i', gen(skill_rnk_`i') equal
	gen mm_`i' = ability_rnk_`i' - skill_rnk_`i'
	gen absmm_`i' = abs(mm_`i')
	replace ability_`i' = ability_rnk_`i'
	replace skill_`i' = skill_rnk_`i'
}

/*------------------------------------------------------------------------------------*/

gen mm = 0
gen mm_sgn = 0
gen mm_neg = 0
gen mm_pos = 0

gen absmm_aa = absmm_v
gen absmm_bb = absmm_m
gen absmm_cc = absmm_s
gen mm_aa = mm_v
gen mm_bb = mm_m
gen mm_cc = mm_s

/*------------------------------------------------------------------------------------*/
/* mismatch */
                                                                                                                             /* check later */
local nlist "aa bb cc"

pca absmm_aa absmm_bb absmm_cc
estat loadings, cnorm(unit)
matrix L = e(L)
local Li = 1
local Lwt = 0
foreach i of local nlist {
	gen mm_`i'_neg = 0
	gen mm_`i'_pos = 0
}
foreach i of local nlist {
	local Lhere = abs(L[`Li',1])
	local Lwt = `Lwt' +  `Lhere'
	replace mm = mm + absmm_`i'*`Lhere'
	replace mm_sgn = mm_sgn + mm_`i'*`Lhere'
	replace mm_neg = mm_neg + mm_`i'*`Lhere' if mm_`i' <0
	replace mm_pos = mm_pos + mm_`i'*`Lhere' if mm_`i' >0
	replace mm_`i'_neg = mm_`i'_neg + mm_`i'*`Lhere' if mm_`i' <0
	replace mm_`i'_pos = mm_`i'_pos + mm_`i'*`Lhere' if mm_`i' >0
	local Li = `Li' + 1
}

replace mm = mm/`Lwt'
replace mm_sgn = mm_sgn/`Lwt'
replace mm_neg = mm_neg/`Lwt'
replace mm_pos = mm_pos/`Lwt'

matrix check = L/`Lwt'
matrix list check

/*------------------------------------------------------------------------------------*/

/* normalize aggregate dimensions */

qui{	
        sum mm
	replace mm = (mm-r(min))/r(sd)
	sum mm_sgn
	replace mm_sgn = mm_sgn/r(sd)
	sum mm_neg
	replace mm_neg = mm_neg/r(sd)
	sum mm_pos
	replace mm_pos = mm_pos/r(sd)
}

/* re-normalize dimensions */

local nlist "aa bb cc"
qui foreach i of local nlist{
	sum absmm_`i'
	replace absmm_`i' = (absmm_`i'-r(min))/r(sd)
	sum mm_`i'_neg
	replace mm_`i'_neg = mm_`i'_neg/r(sd)
	sum mm_`i'_pos
	replace mm_`i'_pos = mm_`i'_pos/r(sd)
}

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* creating iteraction terms */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

* w/o tenure iv
gen mm_ten_occ = mm*tenure_occ
gen mm_ten_occ2 = mm*ten_occ2
gen mm_neg_ten_occ = mm_neg*tenure_occ
gen mm_pos_ten_occ = mm_pos*tenure_occ
local nlist "aa bb cc"
qui foreach i of local nlist {
	gen mm_`i'_neg_ten_occ = mm_`i'_neg*tenure_occ
	gen mm_`i'_pos_ten_occ = mm_`i'_pos*tenure_occ
	gen mm_`i'_ten_occ = mm_`i'*tenure_occ
	gen absmm_`i'_ten_occ = absmm_`i'*tenure_occ
}

* w/ tenure iv
gen mm_ten_occ_iv = mm*ten_occ_iv
gen mm_ten_occ2_iv = mm*ten_occ2_iv
gen mm_neg_ten_occ_iv = mm_neg*ten_occ_iv
gen mm_pos_ten_occ_iv = mm_pos*ten_occ_iv
local nlist "aa bb cc"
foreach i of local nlist {
	gen mm_`i'_neg_ten_occ_iv = mm_`i'_pos*ten_occ_iv
	gen mm_`i'_pos_ten_occ_iv = mm_`i'_pos*ten_occ_iv
	gen mm_`i'_ten_occ_iv = mm_`i'*ten_occ_iv
	gen absmm_`i'_ten_occ_iv = absmm_`i'*ten_occ_iv
}

/*------------------------------------------------------------------------------------*/
/* creating other terms */

gen ability_aa = ability_v
gen ability_bb = ability_m
gen ability_cc = ability_s
gen skill_aa = skill_v
gen skill_bb = skill_m
gen skill_cc = skill_s

gen ability_lev = (ability_aa + ability_bb + ability_cc)/3
cumul ability_lev, gen(ability_mean) equal
gen ability_mean_ten_occ = ability_mean*tenure_occ
gen ability_mean_ten_occ2 = ability_mean*ten_occ2
gen ability_mean_ten_occ_iv = ability_mean*ten_occ_iv
gen ability_mean_ten_occ2_iv = ability_mean*ten_occ2_iv

gen skill_lev  = (skill_aa + skill_bb + skill_cc)/3
cumul skill_lev, gen(skill_mean) equal
gen skill_mean_ten_occ = skill_mean*tenure_occ
gen skill_mean_ten_occ2 = skill_mean*ten_occ2
gen skill_mean_ten_occ_iv = skill_mean*ten_occ_iv
gen skill_mean_ten_occ2_iv = skill_mean*ten_occ2_iv

gen  ability_exp = ability_mean*exp

local nlist "aa bb cc"
foreach i of local nlist{
	gen ability_`i'_ten_occ = ability_`i'*tenure_occ
	gen ability_`i'_exp = ability_`i'*exp
	gen ability_`i'_ten_occ_iv = ability_`i'*ten_occ_iv
	gen skill_`i'_ten_occ = skill_`i'*tenure_occ
	gen skill_`i'_ten_occ_iv = skill_`i'*ten_occ_iv
}

/*------------------------------------------------------------------------------------*/
/* labeling */

label var mm "Mismatch"
label var mm_ten_occ "Mismatch $\times$ Occ Tenure"
label var mm_neg "Negative Mismatch"
label var mm_pos "Positive Mismatch"
label var mm_neg_ten_occ "Pos. Mismatch $\times$ Occ Tenure"
label var mm_pos_ten_occ "Neg. Mismatch $\times$ Occ Tenure"

label var ability_mean "Worker Ability (Mean)"
label var skill_mean "Occ Reqs (Mean)"
label var ability_mean_ten_occ "Worker Ability $\times$ Occ Tenure"
label var ability_mean_ten_occ2 "Worker Ability $\times$ Occ Tenure$^2 \times$ 100"
label var skill_mean_ten_occ "Occ Reqs $\times$ Occ Tenure"
label var skill_mean_ten_occ2 "Occ Reqs $\times$ Occ Tenure$^2 \times$ 100"

/*------------------------------------------------------------------------------------*/

local nlist "aa bb cc"
local llist "Verbal Math Social"
local ll = 1
capture foreach i of local nlist{
	local l: word `ll' of `llist'
	label var ability_`i' "`l' Ability"
	label var ability_`i'_ten_occ "`l' Ability $\times$ Occ Tenure"
	label var ability_`i'_exp "`l' Ability $\times$ Experience"
	label var skill_`i' "Occ Reqs `l'"
	label var skill_`i'_ten_occ "Occ Reqs `l' $\times$ Occ Tenure"		
	label var absmm_`i' "Mismatch `l'"
	label var absmm_`i'_ten_occ "Mismatch `l' $\times$ Occ Tenure"
	label var mm_`i'_neg "Negative Mismatch `l'"
	label var mm_`i'_pos "Positive Mismatch `l'"
	label var mm_`i'_neg_ten_occ "Neg Mismatch `l' $\times$ Occ Tenure"
	label var mm_`i'_pos_ten_occ "Pos Mismatch `l' $\times$ Occ Tenure"
	local ll = `ll' + 1
}

/*------------------------------------------------------------------------------------*/
/* summary statistics for mismatch */
su mm
forvalues i = 1/12 {
	su mm if ind_1 == `i'
}
su mm if grade < 12
su mm if grade == 12
su mm if grade > 12
forvalues i = 1/3 {
	su mm if race == `i'
}

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* create cumulative measures */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

egen idocc = group(id occ)

xtset id year
by id: egen nswitch = total(switch_occ)
by id: gen switch_count = sum(switch_occ)

/* define last mismatch */
sort id year
gen lmm = mm[_n-1] if switch_occ == 1
replace lmm = lmm[_n-1] if switch_occ == 0 & id == id[_n-1]

gen lten_occ = tenure_occ[_n-1] if switch_occ == 1
replace lten_occ = lten_occ[_n-1] if switch_occ == 0 & id == id[_n-1]

local nlist "aa bb cc"
foreach i of local nlist{
	gen lmm_`i' = absmm_`i'[_n-1] if switch_occ == 1
	replace lmm_`i' = lmm_`i'[_n-1] if switch_occ == 0 & id == id[_n-1]
	gen lmm_`i'_neg = mm_`i'_neg[_n-1] if switch_occ == 1
	replace lmm_`i'_neg = lmm_`i'_neg[_n-1] if switch_occ == 0 & id == id[_n-1]
	gen lmm_`i'_pos = mm_`i'_pos[_n-1] if switch_occ == 1
	replace lmm_`i'_pos = lmm_`i'_pos[_n-1] if switch_occ == 0 & id == id[_n-1]
}

gen lmm_neg = mm_neg[_n-1] if switch_occ == 1
replace lmm_neg = lmm_neg[_n-1] if switch_occ == 0 & id == id[_n-1]
gen lmm_pos = mm_pos[_n-1] if switch_occ == 1
replace lmm_pos = lmm_pos[_n-1] if switch_occ == 0 & id == id[_n-1]

label var lmm "Last Mismatch"
label var lmm_pos "Last Mismatch Positive"
label var lmm_neg "Last Mismatch Negative"

/* create cumulative mismatch */
gsort +id -switch_occ +year

by id: gen cmm = sum(lmm*lten_occ)  if switch_occ==1  & lmm<. & lten_occ<.
by id: gen cmm_neg = sum(lmm_neg*lten_occ)  if switch_occ==1 
by id: gen cmm_pos = sum(lmm_pos*lten_occ)  if switch_occ==1 
by id: gen totexp = sum(lten_occ) if switch_occ==1
replace cmm = cmm/totexp if switch_occ==1
replace cmm_neg = cmm_neg/totexp if switch_occ==1 
replace cmm_pos = cmm_pos/totexp if switch_occ==1

local nlist "aa bb cc"
foreach i of local nlist{
	by id: gen cmm_`i' = sum(lmm_`i'*lten_occ)  if switch_occ==1  & lmm_`i'<. & lten_occ<.
	replace cmm_`i' = cmm_`i'/totexp if switch_occ==1
	by id: gen cmm_neg_`i' = sum(lmm_`i'_neg*lten_occ)/totexp  if switch_occ==1 
	by id: gen cmm_pos_`i' = sum(lmm_`i'_pos*lten_occ)/totexp  if switch_occ==1 
}

sort id year
replace cmm = cmm[_n-1] if switch_occ == 0 & id == id[_n-1]
replace cmm_neg = cmm_neg[_n-1] if switch_occ == 0 & id == id[_n-1]
replace cmm_pos = cmm_pos[_n-1] if switch_occ == 0 & id == id[_n-1]

local nlist "aa bb cc"
foreach i of local nlist{
	replace cmm_`i' = cmm_`i'[_n-1] if switch_occ == 0 & id == id[_n-1]
	replace cmm_neg_`i' = cmm_neg_`i'[_n-1] if switch_occ == 0 & id == id[_n-1]
	replace cmm_pos_`i' = cmm_pos_`i'[_n-1] if switch_occ == 0 & id == id[_n-1]
}

/*------------------------------------------------------------------------------------*/

/* normalize aggregate dimensions */

qui{	
        sum cmm
	replace cmm = (cmm-r(min))/r(sd)
	sum cmm_neg
	replace cmm_neg = cmm_neg/r(sd)
	sum cmm_pos
	replace cmm_pos = cmm_pos/r(sd)
}

/* re-normalize dimensions */

local nlist "aa bb cc"
foreach i of local nlist{
	sum cmm_`i'
	replace cmm_`i' = (cmm_`i'-r(min))/r(sd)
	sum cmm_neg_`i'
	replace cmm_neg_`i' = cmm_neg_`i'/r(sd)
	sum cmm_pos_`i'
	replace cmm_pos_`i' = cmm_pos_`i'/r(sd)
}

/*------------------------------------------------------------------------------------*/

label var cmm "Cumul Mismatch"
label var cmm_pos "Cumul Positive Mismatch"
label var cmm_neg "Cumul Negative Mismatch"

local nlist "aa bb cc"
local llist "Verbal Math Social"
local ll = 1
capture foreach i of local nlist{
	local l: word `ll' of `llist'
	label var lmm_`i' "Last Mismatch `l'"
	label var lmm_`i'_neg "Last Neg. Mismatch, `l'"
	label var lmm_`i'_pos "Last Pos. Mismatch, `l'"
	label var cmm_`i' "Cumul Mismatch `l'"
	label var cmm_neg_`i' "Cumul Neg. Mismatch, `l'"
	label var cmm_pos_`i' "Cumul Pos. Mismatch, `l'"	
	local ll = `ll' + 1
}

save $data/yearly_03.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* wage estimation */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

use $data/yearly_03.dta, clear

sort id year
by id: gen counter = _n
egen max_counter = max(counter), by(id)
gen multobs_01 = (max_counter != 1)
drop counter max_counter

gen one = (cmm != .)
by id: gen counter = sum(one)
egen max_counter = max(counter), by(id)
gen multobs_02 = (max_counter != 1)
drop counter max_counter

/*------------------------------------------------------------------------------------*/

global xlist_0 tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist_0 ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv

/*------------------------------------------------------------------------------------*/
/* benchmark */

/* ols regression */
xtset id year
global xlist $xlist_0
xi: xtivreg2 lwage $xlist $zlist i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/bench_ols.ster, replace
sort id year

/* iv regression (Altonji and Shakotko) */
xtset id year
global xlist $xlist_0
global ivlist $ivlist_0
xi: xtivreg2 lwage ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d if multobs_01 == 1, fe  bw(2) robust
estimate save ${result}/bench_iv.ster, replace

/*------------------------------------------------------------------------------------*/
/* mismatch */

global xlist  ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: xtivreg2 lwage mm $xlist $zlist skill_mean i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/ols_mm_means.ster, replace

global xlist  ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xi: xtivreg2 lwage mm ($xlist = $ivlist) $zlist skill_mean i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/iv_mm_means.ster, replace

/*------------------------------------------------------------------------------------*/
/* mismatch with tenure */

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: xtivreg2 lwage mm $xlist $zlist skill_mean i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/ols_mm_ten_means.ster, replace

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xi: xtivreg2 lwage mm ($xlist = $ivlist) $zlist skill_mean i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/iv_mm_ten_means.ster, replace

/*------------------------------------------------------------------------------------*/
/* cumulative mismatch */

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: xtivreg2 lwage mm cmm $xlist $zlist skill_mean i.ind_1d i.occ_1d if multobs_02 == 1, fe bw(2) robust
estimate save ${result}/ols_cmm_mm_means.ster, replace

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xi: xtivreg2 lwage mm cmm ($xlist = $ivlist) $zlist skill_mean i.ind_1d i.occ_1d if multobs_02 == 1, fe bw(2) robust
estimate save ${result}/iv_cmm_mm_means.ster, replace

/*------------------------------------------------------------------------------------*/
/* mismatch with positive & negative components */

global xlist  $xlist_0
xi: xtivreg2 lwage mm_pos mm_neg $xlist $zlist i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/ols_mm_means_pos_neg.ster, replace

global xlist  $xlist_0
global ivlist $ivlist_0
xi: xtivreg2 lwage mm_pos mm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/iv_mm_means_pos_neg.ster, replace

/*------------------------------------------------------------------------------------*/
/* mismatch with positive & negative components with tenure */

global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0
xi: xtivreg2 lwage mm_pos mm_neg $xlist $zlist i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/ols_mm_ten_means_pos_neg.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0
global ivlist mm_pos_ten_occ_iv mm_neg_ten_occ_iv $ivlist_0
xi: xtivreg2 lwage mm_pos mm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/iv_mm_ten_means_pos_neg.ster, replace

/*------------------------------------------------------------------------------------*/
/* cumulative mismatch with positive & negative components */

global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0
xi: xtivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg $xlist $zlist i.ind_1d i.occ_1d if multobs_02 == 1, fe bw(2) robust
estimate save ${result}/ols_cmm_mm_means_pos_neg.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0
global ivlist mm_pos_ten_occ_iv mm_neg_ten_occ_iv $ivlist_0
xi: xtivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d if multobs_02 == 1, fe bw(2) robust
estimate save ${result}/iv_cmm_mm_means_pos_neg.ster, replace

/*------------------------------------------------------------------------------------*/
/* individual component mismatch */

global xlist  ability_??_ten_occ skill_??_ten_occ $xlist_0
xi: xtivreg2 lwage absmm_?? $xlist $zlist skill_?? i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/ols_ind_mm_means.ster, replace

global xlist  ability_??_ten_occ skill_??_ten_occ $xlist_0
global ivlist ability_??_ten_occ_iv skill_??_ten_occ_iv $ivlist_0
xi: xtivreg2 lwage absmm_?? ($xlist = $ivlist) $zlist skill_?? i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/iv_ind_mm_means.ster, replace

/*------------------------------------------------------------------------------------*/
/* individual component mismatch with tenure */

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0
xi: xtivreg2 lwage absmm_?? $xlist $zlist skill_?? i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/ols_ind_mm_ten_means.ster, replace

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0
global ivlist absmm_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv $ivlist_0
xi: xtivreg2 lwage absmm_?? ($xlist = $ivlist) $zlist skill_?? i.ind_1d i.occ_1d if multobs_01 == 1, fe bw(2) robust
estimate save ${result}/iv_ind_mm_ten_means.ster, replace

/*------------------------------------------------------------------------------------*/
/* individual component cumulative mismatch */

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0 
xi: xtivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? $xlist $zlist skill_?? i.ind_1d i.occ_1d if multobs_02 == 1, fe bw(2) robust
estimate save ${result}/ols_ind_cmm_mm_means.ster, replace

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0 
global ivlist absmm_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv $ivlist_0
xi: xtivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) $zlist skill_?? i.ind_1d i.occ_1d if multobs_02 == 1, fe bw(2) robust
estimate save ${result}/iv_ind_cmm_mm_means.ster, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* creating tables and figures */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* mismatch */

estimate clear
estimate use ${result}/ols_mm_means.ster
estimate store ols_mm_means
estimate use ${result}/ols_mm_ten_means.ster
estimate store ols_mm_ten_means
estimate use ${result}/ols_cmm_mm_means.ster
estimate store ols_cmm_mm_means

estimate use ${result}/iv_mm_means.ster
estimate store iv_mm_means
estimate use ${result}/iv_mm_ten_means.ster
estimate store iv_mm_ten_means
estimate use ${result}/iv_cmm_mm_means.ster
estimate store iv_cmm_mm_means

/* tex */		   
esttab iv_mm_means iv_mm_ten_means iv_cmm_mm_means ols_mm_means ols_mm_ten_means ols_cmm_mm_means ///
                   using ${result}/table_${diminitls}_fe.tex, b(4) ///
                   r2 nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "Robust standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV" "IV" "IV" "OLS" "OLS" "OLS") ///
		   order(mm mm_ten_occ cmm ability_mean_ten_occ skill_mean skill_mean_ten_occ) ///
                   star(* 0.10 ** 0.05 *** 0.01) replace
		   
esttab iv_mm_means iv_mm_ten_means iv_cmm_mm_means ols_mm_means ols_mm_ten_means ols_cmm_mm_means ///
                   using ${result}/table_apx_${diminitls}_fe.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\hline  " ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   title("Wage Regression with Mismatch (Full Results)") ///
		   drop(_I*) ///
		   mtitles("IV" "IV" "IV" "OLS" "OLS" "OLS") ///
		   order(mm mm_ten_occ cmm ability_mean_ten_occ skill_mean skill_mean_ten_occ ten* exp* oj $zlist _cons) ///
                   star(* 0.10 ** 0.05 *** 0.01) replace
  
  
  
/*------------------------------------------------------------------------------------*/
/* pos-neg mismatch */

estimate clear
estimate use ${result}/ols_mm_means_pos_neg.ster
estimate store ols_mm_means_pos_neg
estimate use ${result}/ols_mm_ten_means_pos_neg.ster
estimate store ols_mm_ten_means_pos_neg
estimate use ${result}/ols_cmm_mm_means_pos_neg.ster
estimate store ols_cmm_mm_means_pos_neg

estimate use ${result}/iv_mm_means_pos_neg.ster
estimate store iv_mm_means_pos_neg
estimate use ${result}/iv_mm_ten_means_pos_neg.ster
estimate store iv_mm_ten_means_pos_neg
estimate use ${result}/iv_cmm_mm_means_pos_neg.ster
estimate store iv_cmm_mm_means_pos_neg

/* tex */		   
esttab iv_mm_means_pos_neg iv_mm_ten_means_pos_neg iv_cmm_mm_means_pos_neg ols_mm_means_pos_neg ols_mm_ten_means_pos_neg ols_cmm_mm_means_pos_neg ///
                   using ${result}/table_${diminitls}_pos_neg_fe.tex, b(4) ///
                   r2 nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "Robust standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV" "IV" "IV" "OLS" "OLS" "OLS") ///
		   order(mm_??? mm_???_ten_occ cmm_??? )  ///
                   star(* 0.10 ** 0.05 *** 0.01) replace
		   
esttab iv_mm_means_pos_neg iv_mm_ten_means_pos_neg iv_cmm_mm_means_pos_neg ols_mm_means_pos_neg ols_mm_ten_means_pos_neg ols_cmm_mm_means_pos_neg ///
                   using ${result}/table_apx_${diminitls}_pos_neg_fe.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\hline  " ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   title("Wage Regression with Mismatch (Full Results)") ///
		   drop(_I*) ///
		   mtitles("IV" "IV" "IV" "OLS" "OLS" "OLS") ///
		   order(mm_??? mm_???_ten_occ cmm_???  ten* exp* oj $zlist _cons) ///
                   star(* 0.10 ** 0.05 *** 0.01) replace
  
  
/*------------------------------------------------------------------------------------*/
/* individual component mismatch */

estimate clear
estimate use ${result}/ols_ind_mm_means.ster
estimate store ols_ind_mm_means
estimate use ${result}/ols_ind_mm_ten_means.ster
estimate store ols_ind_mm_ten_means
estimate use ${result}/ols_ind_cmm_mm_means.ster
estimate store ols_ind_cmm_mm_means

estimate use ${result}/iv_ind_mm_means.ster
estimate store iv_ind_mm_means
estimate use ${result}/iv_ind_mm_ten_means.ster
estimate store iv_ind_mm_ten_means
estimate use ${result}/iv_ind_cmm_mm_means.ster
estimate store iv_ind_cmm_mm_means

/* tex */
esttab iv_ind_mm_means iv_ind_mm_ten_means iv_ind_cmm_mm_means ols_ind_mm_means ols_ind_mm_ten_means ols_ind_cmm_mm_means ///
                   using ${result}/table_${diminitls}_ind_fe.tex, b(4) ///
                   r2 nodepvars gaps label not nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "Robust standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV" "IV" "IV" "OLS" "OLS" "OLS") ///
		   order(absmm_aa absmm_bb absmm_cc absmm_aa_t* absmm_bb_t* absmm_cc_t* cmm_aa cmm_bb cmm_cc ability_??_* skill_?? skill_??_*) ///
                   star(* 0.10 ** 0.05 *** 0.01) replace
		   
esttab iv_ind_mm_means iv_ind_mm_ten_means iv_ind_cmm_mm_means ols_ind_mm_means ols_ind_mm_ten_means ols_ind_cmm_mm_means ///
                   using ${result}/table_apx_${diminitls}_ind_fe.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\hline  " ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I*) ///
		   mtitles("IV" "IV" "IV" "OLS" "OLS" "OLS") ///
		   title("Wage Regression with Mismatch by Components (Full Results)") ///
		   order(absmm_aa absmm_bb absmm_cc absmm_aa_t* absmm_bb_t* absmm_cc_t* cmm_aa cmm_bb cmm_cc ability_??_* skill_?? skill_??_* ten* exp* oj $zlist _cons) ///
                   star(* 0.10 ** 0.05 *** 0.01) replace

/*------------------------------------------------------------------------------------*/		   
/* predicted effect of mismatch on wages */

estimate clear
estimate use ${result}/iv_cmm_mm_means.ster
matrix pred_mm_pt = J(5,4,0.0)
matrix pred_mm_sd = J(5,4,0.0)
matrix eV = e(V)
* should replace this with name-based indexing
local mi = 13
local mti = 1

_pctile mm, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)
scalar check = r(r1)

forvalues tt = 1/3 {
	matrix pred_mm_pt[1,`tt'] = _b["mm"]*`p90'+_b["mm_ten_occ"]*`p90'*`tt'*5
	matrix pred_mm_pt[2,`tt'] = _b["mm"]*`p70'+_b["mm_ten_occ"]*`p70'*`tt'*5
	matrix pred_mm_pt[3,`tt'] = _b["mm"]*`p50'+_b["mm_ten_occ"]*`p50'*`tt'*5
	matrix pred_mm_pt[4,`tt'] = _b["mm"]*`p30'+_b["mm_ten_occ"]*`p30'*`tt'*5
	matrix pred_mm_pt[5,`tt'] = _b["mm"]*`p10'+_b["mm_ten_occ"]*`p10'*`tt'*5

	matrix pred_mm_sd[1,`tt'] = (eV[`mi',`mi']*`p90'^2+ eV[`mti',`mti']*(`p90'*`tt'*5)^2 + 2*eV[`mi',`mti']*`p90'*(`p90'*`tt'*5))^.5 
	matrix pred_mm_sd[2,`tt'] = (eV[`mi',`mi']*`p70'^2+ eV[`mti',`mti']*(`p70'*`tt'*5)^2 + 2*eV[`mi',`mti']*`p70'*(`p70'*`tt'*5))^.5 
	matrix pred_mm_sd[3,`tt'] = (eV[`mi',`mi']*`p50'^2+ eV[`mti',`mti']*(`p50'*`tt'*5)^2 + 2*eV[`mi',`mti']*`p50'*(`p50'*`tt'*5))^.5 
	matrix pred_mm_sd[4,`tt'] = (eV[`mi',`mi']*`p30'^2+ eV[`mti',`mti']*(`p30'*`tt'*5)^2 + 2*eV[`mi',`mti']*`p30'*(`p30'*`tt'*5))^.5 
	matrix pred_mm_sd[5,`tt'] = (eV[`mi',`mi']*`p10'^2+ eV[`mti',`mti']*(`p10'*`tt'*5)^2 + 2*eV[`mi',`mti']*`p10'*(`p10'*`tt'*5))^.5
}

_pctile cmm, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)

matrix pred_mm_pt[1,4] = _b["cmm"]*`p90'
matrix pred_mm_pt[2,4] = _b["cmm"]*`p70'
matrix pred_mm_pt[3,4] = _b["cmm"]*`p50'
matrix pred_mm_pt[4,4] = _b["cmm"]*`p30'
matrix pred_mm_pt[5,4] = _b["cmm"]*`p10'

matrix pred_mm_sd[1,4] = _se["cmm"]*`p90'
matrix pred_mm_sd[2,4] = _se["cmm"]*`p70'
matrix pred_mm_sd[3,4] = _se["cmm"]*`p50'
matrix pred_mm_sd[4,4] = _se["cmm"]*`p30'
matrix pred_mm_sd[5,4] = _se["cmm"]*`p10'

matrix pred_mm_pt_sd = J(10,4,0.0)
forvalues pi = 1/5{
	forvalues tt = 1/4{
		local ri = (`pi'-1)*2+1
		matrix pred_mm_pt_sd[`ri',`tt'] = pred_mm_pt[`pi',`tt']
		local ri = (`pi')*2
		matrix pred_mm_pt_sd[`ri',`tt'] = pred_mm_sd[`pi',`tt']
	}
}

matrix colnames pred_mm_pt_sd = "MM_5_Years" "MM_10_Years" "MM_15_Years" "Cumul_MM"
matrix rownames pred_mm_pt_sd = "90_Percentile" "90_Percentile" "70_Percentile" "70_Percentile" "50_Percentile" "50_Percentile" "30_Percentile" "30_Percentile" "10_Percentile" "10_Percentile"
putexcel A1=matrix(pred_mm_pt_sd, names) using ${result}/table_${diminitls}_pred_mm.xls, replace

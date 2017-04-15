/*--------------------------------------------------------------------------------------
* name: mismatch_04_regression.do
* made by: david wiczer, moidfied by: satoshi tanaka
* date: 08/21/2010
*       04/03/2015
* description: this code is for the project 'occupation skill mismatch'
--------------------------------------------------------------------------------------*/

global diminitls "vms"
global varcor    "cid" /* may be cluster,id or rbst*/
global varmeth   "asymp"/* how to compute variance, may be boot or asymp */

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
use ${result}/yearly_02.dta, clear

/*------------------------------------------------------------------------------------*/

/* choose cross-sectional male sample */

keep if sample_id >= 1 & sample_id <= 4

/*------------------------------------------------------------------------------------*/

/* this says how many people and how many obs we have */
xtset id year
*xtdes
*count

/*------------------------------------------------------------------------------------*/

drop if (occ <= 0 | occ == . | ind <= 0 | ind == .)

/*------------------------------------------------------------------------------------*/

/* this says how many people and how many obs we have */
xtset id year
*xtdes
*count

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
*xtdes
*count

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
*xtdes
*count

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

save $result/yearly_03.dta, replace

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

use $result/yearly_03.dta, clear

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

use $result/yearly_03.dta, clear

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

use $result/yearly_03.dta, clear

/* generate some  other covariates */

gen hs = (grade>=12)
gen lths = (grade<12)
gen univ = (grade>=16)
gen hispanic = (race == 1)
gen black = (race == 2)
gen yob = year - age

label var lwage	"ln(Wage)"
label var hs	"High School"
label var lths	"< High School"
label var univ	"4-Year College"
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
global zlist_0 lths univ hispanic black
global zlist $zlist_0
*global zlist lths univ hispanic black AFQT_std

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
	gen mm_norm_`i' = ability_`i' - skill_`i'
	gen absmm_`i' = abs(mm_`i')
	replace ability_`i' = ability_rnk_`i'
	replace skill_`i' = skill_rnk_`i'
}

/*------------------------------------------------------------------------------------*/

gen mm = 0
gen mm_sgn = 0
gen mm_neg = 0
gen mm_pos = 0
gen mm_2nm = 0
*gen skill_pca =0
*gen ability_pca = 0

gen absmm_aa = absmm_v
gen absmm_bb = absmm_m
gen absmm_cc = absmm_s
gen mm_aa = mm_v
gen mm_bb = mm_m
gen mm_cc = mm_s
gen mm_norm_aa = mm_norm_v
gen mm_norm_bb = mm_norm_m
gen mm_norm_cc = mm_norm_s

gen ability_aa = ability_v
gen ability_bb = ability_m
gen ability_cc = ability_s
gen skill_aa = skill_v
gen skill_bb = skill_m
gen skill_cc = skill_s

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
	replace mm_2nm = (mm_norm_`i'^2)*`Lhere'
*	replace skill_pca = skill_pca + skill_`i'*`Lhere'
*	replace ability_pca = ability_pca + ability_`i'*`Lhere'
	replace mm_sgn = mm_sgn + mm_`i'*`Lhere'
	replace mm_neg = mm_neg + mm_`i'*`Lhere' if mm_`i' <0
	replace mm_pos = mm_pos + mm_`i'*`Lhere' if mm_`i' >0
	replace mm_`i'_neg = mm_`i'_neg + mm_`i'*`Lhere' if mm_`i' <0
	replace mm_`i'_pos = mm_`i'_pos + mm_`i'*`Lhere' if mm_`i' >0
	local Li = `Li' + 1
}

replace mm = mm/`Lwt'
*replace skill_pca = skill_pca /`Lwt'
*replace ability_pca = ability_pca /`Lwt'
replace mm_sgn = mm_sgn/`Lwt'
replace mm_neg = mm_neg/`Lwt'
replace mm_pos = mm_pos/`Lwt'

*cumul skill_pca, gen(skill_pca_mean)
*cumul ability_pca, gen(ability_pca_mean)
*gen mm_1d = abs(ability_pca_mean - skill_pca_mean)

matrix check = L/`Lwt'
matrix list check

/*------------------------------------------------------------------------------------*/

/* normalize aggregate dimensions */

qui{	
        sum mm
	replace mm = (mm-r(min))/r(sd)
	sum mm_2nm
	replace mm_2nm = (mm_2nm-r(min))/r(sd)
	sum mm_sgn
	replace mm_sgn = mm_sgn/r(sd)
	sum mm_neg
	replace mm_neg = mm_neg/r(sd)
	sum mm_pos
	replace mm_pos = mm_pos/r(sd)
	cumul mm, gen(mm_rnk) equal
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
gen mm_2nm_ten_occ = mm_2nm*tenure_occ
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
gen mm_2nm_ten_occ_iv = mm_2nm*ten_occ_iv
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

gen ability_lev = (ability_aa + ability_bb + ability_cc)/3
cumul ability_lev, gen(ability_mean) 
pca ability_aa ability_bb ability_cc, components(1)
predict ability_pca
cumul ability_pca, gen(ability_pca_mean) 
gen ability_mean_ten_occ = ability_mean*tenure_occ
gen ability_mean_ten_occ2 = ability_mean*ten_occ2
gen ability_mean_ten_occ_iv = ability_mean*ten_occ_iv
gen ability_mean_ten_occ2_iv = ability_mean*ten_occ2_iv

gen skill_lev  = (skill_aa + skill_bb + skill_cc)/3
cumul skill_lev, gen(skill_mean) 
pca skill_aa skill_bb skill_cc , components(1)
predict skill_pca
cumul skill_pca, gen(skill_pca_mean) 
gen skill_mean_ten_occ = skill_mean*tenure_occ
gen skill_mean_ten_occ2 = skill_mean*ten_occ2
gen skill_mean_ten_occ_iv = skill_mean*ten_occ_iv
gen skill_mean_ten_occ2_iv = skill_mean*ten_occ2_iv

gen  mm_1d = abs(ability_pca_mean - skill_pca_mean)
qui sum mm_1d
replace mm_1d = (mm_1d-r(min))/r(sd)
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
label var mm_2nm "Quadratic Mismatch"
label var mm_ten_occ "Mismatch $\times$ Occ Tenure"
label var mm_neg "Negative Mismatch"
label var mm_pos "Positive Mismatch"
label var mm_neg_ten_occ "Neg. Mismatch $\times$ Occ Tenure"
label var mm_pos_ten_occ "Pos. Mismatch $\times$ Occ Tenure"
label var mm_2nm_ten_occ "Quad. Mismatch $\times$ Occ Tenure"


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
gen lmm_rnk = mm_rnk[_n-1] if switch_occ == 1
replace lmm_rnk = lmm_rnk[_n-1] if switch_occ == 0 & id == id[_n-1]
gen lmm_1d = mm_1d[_n-1] if switch_occ == 1
replace lmm_1d = lmm_1d[_n-1] if switch_occ == 0 & id == id[_n-1]

/* last requirement */
gen lskill = skill_pca_mean[_n-1] if switch_occ==1
replace lskill = lskill[_n-1] if switch_occ == 0 & id == id[_n-1]

gen delmm_rnk = mm_rnk - lmm_rnk
gen delmm = mm-lmm

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
gen lmm_2nm = mm_2nm[_n-1] if switch_occ == 1
replace lmm_2nm = lmm_2nm[_n-1] if switch_occ == 0 & id == id[_n-1]

label var lmm "Last Mismatch"
label var lmm_pos "Last Mismatch Positive"
label var lmm_neg "Last Mismatch Negative"
label var lmm_2nm "Last Quadratic Mismatch"


/* create cumulative mismatch */
gsort +id -switch_occ +year

by id: gen cmm = sum(lmm*lten_occ)  if switch_occ==1  & lmm<. & lten_occ<.
by id: gen cmm_2nm = sum(lmm_2nm*lten_occ)  if switch_occ==1  & lmm_2nm<. & lten_occ<.
by id: gen cmm_neg = sum(lmm_neg*lten_occ)  if switch_occ==1 
by id: gen cmm_pos = sum(lmm_pos*lten_occ)  if switch_occ==1 
by id: gen cskill= sum(lskill*lten_occ) if switch_occ==1 & lskill<. & lten_occ<.
by id: gen cmm_1d = sum(lmm_1d*lten_occ)  if switch_occ==1  & lmm_1d<. & lten_occ<.


by id: gen totexp = sum(lten_occ) if switch_occ==1
replace cmm = cmm/totexp if switch_occ==1
replace cmm_2nm = cmm_2nm/totexp if switch_occ==1
replace cmm_1d = cmm_1d/totexp if switch_occ==1
replace cmm_neg = cmm_neg/totexp if switch_occ==1 
replace cmm_pos = cmm_pos/totexp if switch_occ==1
replace cskill = cskill/totexp if switch_occ==1

local nlist "aa bb cc"
foreach i of local nlist{
	by id: gen cmm_`i' = sum(lmm_`i'*lten_occ)  if switch_occ==1  & lmm_`i'<. & lten_occ<.
	replace cmm_`i' = cmm_`i'/totexp if switch_occ==1
	by id: gen cmm_neg_`i' = sum(lmm_`i'_neg*lten_occ)/totexp  if switch_occ==1 
	by id: gen cmm_pos_`i' = sum(lmm_`i'_pos*lten_occ)/totexp  if switch_occ==1 
}

sort id year
replace cmm = cmm[_n-1] if switch_occ == 0 & id == id[_n-1]
replace cmm_2nm = cmm_2nm[_n-1] if switch_occ == 0 & id == id[_n-1]
replace cmm_1d = cmm_1d[_n-1] if switch_occ == 0 & id == id[_n-1]
replace cmm_neg = cmm_neg[_n-1] if switch_occ == 0 & id == id[_n-1]
replace cmm_pos = cmm_pos[_n-1] if switch_occ == 0 & id == id[_n-1]
replace cskill = cskill[_n-1] if switch_occ == 0 & id == id[_n-1]


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
	sum cmm_2nm
	replace cmm_2nm = (cmm_2nm-r(min))/r(sd)	
	sum cmm_1d
	replace cmm_1d = (cmm_1d-r(min))/r(sd)
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
label var cmm_2nm "Cumul Quadratic Mismatch"
label var cmm_1d "Cumul Mismatch, 1D"
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

save ${result}/yearly_03.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* mismatch summary statistics */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

merge m:1 year using ${download}/unrate.dta, gen(_merge_unrate)
drop if _merge_unrate <3

reg mm i.age $zlist_0
predict mm_ageres, residual
preserve 
collapse mm_ageres mm, by(year)
twoway (scatter mm_ageres year) (lpoly mm_ageres year, lwidth(thick)) , ///
ytitle("Mismatch Residual", size(medlarge)) xtitle("Year", size(medlarge)) ///
title("Mismatch by year, age dummies and demographics") ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(, grid gstyle(dot)) ///
legend(off) /// yscale(range(0.15 0.24) noextend)
saving(${result}/mm_${diminitls}_ageres_year, replace)
graph export ${result}/mm_${diminitls}_ageres_year.eps, replace 
*graph export ${result}/mm_${diminitls}_ageres_year.png, replace 
restore

reg mm unrate year i.age $zlist_0
gen rec = unrate >=7

reg mm year i.age $zlist_0
predict mm_yrageres, residual
preserve 
collapse mm_yrageres mm unrate, by(year)
twoway (scatter mm_yrageres year ) (lpoly mm_yrageres year,lwidth(thick)), ///
ytitle("Mismatch Residual", size(medlarge)) xtitle("Year", size(medlarge)) ///
title("Mismatch by year, year trend, age and demographics") ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(, grid gstyle(dot)) ///
legend(off) /// yscale(range(0.15 0.24) noextend)
saving(${result}/mm_${diminitls}_ageres_year, replace)
graph export ${result}/mm_${diminitls}_yrageres_year.eps, replace 
*graph export ${result}/mm_${diminitls}_yrageres_year.png, replace 


twoway (scatter mm_yrageres year, yaxis(1) ) (lpoly mm_yrageres year, yaxis(1) lwidth(thick)) ///
(line unrate year, yaxis(2) lwidth(thick)) , ///
ytitle("Mismatch Residual", size(medlarge) axis(1)) ytitle("Unemployment Rate", size(medlarge) axis(2)) ///
 xtitle("Year", size(medlarge)) ///
title("Mismatch by year, year trend, age and demographics") ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(, grid gstyle(dot)) ///
legend(off) /// yscale(range(0.15 0.24) noextend)
saving(${result}/mm_${diminitls}_ageres_unrate_year, replace)
graph export ${result}/mm_${diminitls}_yrageres_unrate_year.eps, replace 
*graph export ${result}/mm_${diminitls}_yrageres_unrate_year.png, replace 
restore

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

xtset id year
gen fswitch_occ = f.switch_occ

local min_exp = 1
local max_exp = 20
local exp_range (exp >= `min_exp' & exp <= `max_exp')
/*
sort id year
gen check_exp = 0
bysort id: replace check_exp = 1 if _n == 1
bysort id: replace check_exp = check_exp[_n-1] + 1 if _n != 1
gen ind_exp = (exp == check_exp & exp == `max_exp')
egen flag_exp = max(ind_exp), by(id)
drop check_exp ind_exp
replace flag_exp = 1
*/
bysort exp: egen medmm_rnk_exp = median(mm_rnk)
bysort exp: egen meanmm_rnk_exp = mean(mm_rnk)

qui {
reg medmm_rnk_exp exp exp2 exp3 if `exp_range'
predict medmm_rnk_exp_pred if `exp_range'
reg meanmm_rnk_exp exp exp2 exp3 if `exp_range'
predict meanmm_rnk_exp_pred if `exp_range'
}

twoway (line medmm_rnk_exp exp,lcolor(eltblue) lpattern(dash) lwidth(thick)) ///
       (line medmm_rnk_exp_pred exp, lcolor(blue) lpattern(solid) lwidth(thick)) if `exp_range', ///
ytitle("Median Rank of Mismatch", size(large) color(dknavy)) ///
xtitle("Experience", size(large) color(dknavy)) ///
title("Mismatch By Experience", size(large)) ///
legend(off) ///
graphregion(color(white)) xlabel(0(5)`max_exp',grid gstyle(default)) ylabel(, grid gstyle(default))
graph export ${result}/fig_mm_median_${diminitls}_rnk_exp.eps, replace

twoway (line meanmm_rnk_exp exp,lcolor(eltblue) lpattern(dash) lwidth(thick)) ///
       (line meanmm_rnk_exp_pred exp, lcolor(blue) lpattern(solid) lwidth(thick)) if `exp_range', ///
ytitle("Mean Rank of Mismatch", size(large) color(dknavy)) ///
xtitle("Experience", size(large) color(dknavy)) ///
title("Mismatch By Experience", size(large)) ///
legend(off) ///
graphregion(color(white)) xlabel(0(5)`max_exp',grid gstyle(default)) ylabel(, grid gstyle(default))
graph export ${result}/fig_mm_mean_${diminitls}_rnk_exp.eps, replace

/*------------------------------------------------------------------------------------*/

/*
sort id year
matrix mm_by_nswith = J(6,2,0.0)
su lmm_rnk if switch_occ == 1, meanonly
matrix mm_by_nswith[1,1] = r(mean)
su mm_rnk if switch_occ == 1, meanonly
matrix mm_by_nswith[1,2] = r(mean)

forvalues t = 1/4{
	su lmm_rnk if switch_occ == 1 & nswitch == `t', meanonly
	matrix mm_by_nswith[`t'+1,1] = r(mean)
	su mm_rnk if switch_occ == 1 & nswitch == `t', meanonly
	matrix mm_by_nswith[`t'+1,2] = r(mean)
}

su lmm_rnk if switch_occ == 1 & nswitch >= 5, meanonly
matrix mm_by_nswith[6,1] = r(mean)
su mm_rnk if switch_occ == 1 & nswitch >= 5, meanonly
matrix mm_by_nswith[6,2] = r(mean)

matrix colnames mm_by_nswith = "Before" "After"
matrix rownames mm_by_nswith = "All" "1st Switch" "2nd Switch" "3rd Switch" "4th Switch" ">=5th Switch"
putexcel A1=matrix(mm_by_nswith, names) using ${result}/table_${diminitls}_mm_by_nswith.xls, replace
*/

/*------------------------------------------------------------------------------------*/
/* return migration */
/*------------------------------------------------------------------------------------*/
duplicates tag occ id if switch_occ==1, generate(dup_switch_occ)
bysort id occ: egen yr1 = min(year)
gen ret_switch_occ = (dup_switch_occ>0 & year != yr1) if switch_occ==1
drop yr1 dup_switch_occ
duplicates tag occ_1d id if switch_occ==1, generate(dup_switch_occ)
bysort id occ_1d: egen yr1 = min(year)
gen ret_switch_occ_1d = (dup_switch_occ>0 & year != yr1) if switch_occ==1
drop yr1 dup_switch_occ

xtset id year
by id: gen switch_occ_ctr = sum(switch_occ)




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

global xlist_0 tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist_0 ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv

if("$varcor" == "cid"){
	egen id_job = group(id job)
	egen id_occ = group(id occ)
	global vcetxt = "cluster id"
	global xtvcetxt = "cluster(id)"
}
else if("$varcor"=="rbst"){
	global vcetxt = "robust"
	global xtvcetxt = "robust"
}
if("$varmeth" =="asymp"){
	xtset id year
}
gen obs1 = id !=id[_n-1]

/*------------------------------------------------------------------------------------*/
/* benchmark */

/* ols regression */

global xlist $xlist_0
xi: reg lwage $xlist $zlist i.ind_1d i.occ_1d, vce(${vcetxt})
estimate save ${result}/bench_ols.ster, replace

/* iv regression (Altonji and Shakotko) */
global xlist $xlist_0
global ivlist $ivlist_0
if("$varmeth" == "boot" & "$varcor" == "cid"){
	xtset, clear
	bootstrap, cluster(id) seed(9487): ivregress 2sls lwage ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d 
	estimate save ${result}/bench_iv_boot.ster, replace
	xtset id year
}
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(${vcetxt})
estimate save ${result}/bench_iv.ster, replace

/*------------------------------------------------------------------------------------*/
/* mismatch */

global xlist  ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(${vcetxt})
estimate save ${result}/ols_mm.ster, replace

global xlist  ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
if("$varmeth" == "boot" & "$varcor" == "cid"){
	xtset,clear
	bootstrap, cluster(id) seed(9487): ivregress 2sls lwage mm $zlist ability_mean skill_mean i.ind_1d i.occ_1d ($xlist = $ivlist)
	estimate save ${result}/iv_mm_boot.ster, replace
	xtset id year
} 
xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(${vcetxt})
estimate save ${result}/iv_mm.ster, replace

/*------------------------------------------------------------------------------------*/
/* mismatch with tenure */

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(${vcetxt})
estimate save ${result}/ols_mm_ten.ster, replace

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
if("$varmeth" == "boot" & "$varcor" == "cid"){
	xtset, clear
	bootstrap, cluster(id) seed(9487): ivregress 2sls lwage mm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d
	estimate save ${result}/iv_mm_ten_boot.ster, replace
	xtset id year
	
} 
xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(${vcetxt})
estimate save ${result}/iv_mm_ten.ster, replace

/*------------------------------------------------------------------------------------*/
/* cumulative mismatch */

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm cmm $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(${vcetxt})
estimate save ${result}/ols_cmm_mm.ster, replace

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
if("$varmeth" == "boot" & "$varcor" == "cid"){
	xtset, clear
	bootstrap, cluster(id)  seed(9487): ivregress 2sls lwage mm cmm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d
	estimate save ${result}/iv_cmm_mm_boot.ster, replace
	xtset id year
} 
xi: ivregress 2sls lwage mm cmm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(${vcetxt})
estimate save ${result}/iv_cmm_mm.ster, replace

*FGLS attempt
ivregress 2sls lwage mm cmm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d
predict uhat, residuals
reg uhat l.uhat /*, fe  */
global rhohat = _b["L.uhat"]
drop uhat
forvalues iter=1/15{
	qui foreach zv of varlist mm cmm $zlist ability_mean skill_mean $xlist $ivlist lwage{
		gen `zv'_R =`zv'
		replace `zv'= `zv'_R  - ${rhohat}*l.`zv'_R 
		replace `zv' = `zv'_R*(1-${rhohat}^2)^0.5 if obs1==1 & `zv'==.
		_crcslbl `zv'_R `zv'
	}
	xi: ivregress 2sls lwage mm cmm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d /*[aw=inv_omega] */
	estimate save ${result}/iv_cmm_mm_fgls.ster, replace
	predict uhat, residuals
	reg uhat l.uhat /*, fe  */
	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist mm cmm $zlist ability_mean skill_mean $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}



*with robust errors for baseline
xi: ivregress 2sls lwage mm cmm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_cmm_mm_rbst.ster, replace

/*------------------------------------------------------------------------------------*/
/* cumulative mismatch with fixed effects */

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xtset id year
xi: xtreg lwage mm cmm $xlist  skill_mean i.ind_1d i.occ_1d, fe nonest vce(${vcetxt})
estimate save ${result}/ols_cmm_mm_fe.ster, replace

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xtivreg2 lwage mm cmm ($xlist = $ivlist)  skill_mean _Iind_1d_* _Iocc_1d_* , ${xtvcetxt} fe


estimate save ${result}/iv_cmm_mm_fe.ster, replace
if("${varcor}" == "cid" & "$varmeth" =="boot"){
	xtset, clear
	bootstrap, cluster(id) seed(9487): xtivreg2 lwage mm cmm skill_mean _Iind_1d_* _Iocc_1d_*  ($xlist = $ivlist), fe i(id)
	estimate save ${result}/iv_cmm_mm_fe_boot.ster, replace
	xtset id year
} 

*FGLS attempt
xtivreg2 lwage mm cmm  skill_mean _Iind_1d_* _Iocc_1d_* ($xlist = $ivlist) , fe
predict uhat, e
reg uhat l.uhat 
global rhohat = _b["L.uhat"]
drop uhat
forvalues iter=1/15{
	qui foreach zv of varlist mm cmm  skill_mean  $xlist $ivlist lwage{
		gen `zv'_R =`zv'
		replace `zv'= `zv'_R  - ${rhohat}*l.`zv'_R 
		replace `zv' = `zv'_R*(1-${rhohat}^2)^0.5 if obs1==1 & `zv'==.
		_crcslbl `zv'_R `zv'
	}
	xtivreg2 lwage mm cmm  skill_mean _Iind_1d_* _Iocc_1d_* ($xlist = $ivlist), fe /*[aw=inv_omega] */
	estimate save ${result}/iv_cmm_mm_fe_fgls.ster, replace
	predict uhat, e
	reg uhat l.uhat /*, fe  */
	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist mm cmm  skill_mean  $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}

*with robust errors for baseline
xtivreg2 lwage mm cmm  skill_mean _Iind_1d_* _Iocc_1d_* ($xlist = $ivlist) , robust fe
estimate save ${result}/iv_cmm_mm_fe_rbst.ster, replace


/*------------------------------------------------------------------------------------*/
/* cumulative with quadratic mismatch */

global xlist  mm_2nm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm_2nm cmm_2nm $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(${vcetxt})
estimate save ${result}/ols_cmm_mm_2nm.ster, replace

global xlist  mm_2nm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_2nm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
if("$varmeth" == "boot" & "$varcor" == "cid"){
	xtset,clear
	bootstrap, cluster(id_job)  seed(9487): ivregress 2sls lwage mm_2nm cmm_2nm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d
	xtset id year
} 
else{
	xi: ivregress 2sls lwage mm_2nm cmm_2nm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(${vcetxt})
} 
estimate save ${result}/iv_cmm_mm_2nm.ster, replace

/*------------------------------------------------------------------------------------*/
/* mismatch with positive & negative components */

global xlist  $xlist_0
xi: reg lwage mm_pos mm_neg $xlist $zlist i.ind_1d i.occ_1d, vce(${vcetxt})
estimate save ${result}/ols_mm_pos_neg.ster, replace

global xlist  $xlist_0
global ivlist $ivlist_0
if("$varmeth" == "boot" & "$varcor" == "cid"){
	xtset, clear
	bootstrap, cluster(id_job) seed(9487): ivregress 2sls lwage mm_pos mm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d 
	estimate save ${result}/iv_mm_pos_neg_boot.ster, replace
	xtset id year
} 
xi: ivregress 2sls lwage mm_pos mm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(${vcetxt})
estimate save ${result}/iv_mm_pos_neg.ster, replace

/*------------------------------------------------------------------------------------*/
/* mismatch with positive & negative components with tenure */

global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0
xi: reg lwage mm_pos mm_neg $xlist $zlist i.ind_1d i.occ_1d, vce(${vcetxt})
estimate save ${result}/ols_mm_ten_pos_neg.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0
global ivlist mm_pos_ten_occ_iv mm_neg_ten_occ_iv $ivlist_0
if("$varmeth" == "boot" & "$varcor" == "cid"){
	xtset, clear
	bootstrap, cluster(id_job)  seed(9487): ivregress 2sls lwage mm_pos mm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d 
	estimate save ${result}/iv_mm_ten_pos_neg_boot.ster, replace
	xtset id year

} 
xi: ivregress 2sls lwage mm_pos mm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(${vcetxt})
estimate save ${result}/iv_mm_ten_pos_neg.ster, replace


/*------------------------------------------------------------------------------------*/
/* cumulative mismatch with positive & negative components */

global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0
xi: reg lwage mm_pos mm_neg cmm_pos cmm_neg $xlist $zlist i.ind_1d i.occ_1d, vce(${vcetxt})
estimate save ${result}/ols_cmm_mm_pos_neg.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0
global ivlist mm_pos_ten_occ_iv mm_neg_ten_occ_iv $ivlist_0
if("$varmeth" == "boot" & "$varcor" == "cid"){
	xtset, clear
	bootstrap, cluster(id_job)  seed(9487): ivregress 2sls lwage mm_pos mm_neg cmm_pos cmm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d
	estimate save ${result}/iv_cmm_mm_pos_neg_boot.ster, replace
	xtset id year

} 
xi: ivregress 2sls lwage mm_pos mm_neg cmm_pos cmm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(${vcetxt})
estimate save ${result}/iv_cmm_mm_pos_neg.ster, replace

*FGLS attempt
xi: ivregress 2sls lwage mm_pos mm_neg cmm_pos cmm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d
predict uhat, residuals
reg uhat l.uhat 
global rhohat = _b["L.uhat"]
drop uhat
forvalues iter=1/15{
	qui foreach zv of varlist mm_pos mm_neg cmm_pos cmm_neg $zlist $xlist $ivlist lwage{
		gen `zv'_R =`zv'
		replace `zv'= `zv'_R  - ${rhohat}*l.`zv'_R 
		replace `zv' = `zv'_R*(1-${rhohat}^2)^0.5 if obs1==1 & `zv'==.
		_crcslbl `zv'_R `zv'
	}
	xi: ivregress 2sls lwage mm_pos mm_neg cmm_pos cmm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d /*[aw=inv_omega] */
	estimate save ${result}/iv_cmm_mm_pos_neg_fgls.ster, replace
	predict uhat, residuals
	reg uhat l.uhat /*, fe  */
	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist mm_pos mm_neg cmm_pos cmm_neg $zlist $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}


*with robust errors for baseline
xi: ivregress 2sls lwage mm_pos mm_neg cmm_pos cmm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_cmm_mm_pos_neg_rbst.ster, replace


/*------------------------------------------------------------------------------------*/
/* cumulative mismatch with positive & negative components and fixed effects*/

global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0
xtset id year
xtreg lwage mm_pos mm_neg cmm_pos cmm_neg $xlist  _Iind_1d* _Iocc_1d*, nonest vce(${vcetxt}) fe
estimate save ${result}/ols_cmm_mm_pos_neg_fe.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0
global ivlist mm_pos_ten_occ_iv mm_neg_ten_occ_iv $ivlist_0
xtivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg _Iind_1d* _Iocc_1d* ($xlist = $ivlist), ${xtvcetxt} fe
estimate save ${result}/iv_cmm_mm_pos_neg_fe.ster, replace

if("${varcor}" == "cid" & "$varmeth" == "boot"){
	xtset, clear
	bootstrap, cluster(id)  seed(9487): xtivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg _Iind_1d* _Iocc_1d* ($xlist = $ivlist), fe  i(id)
	estimate save ${result}/iv_cmm_mm_pos_neg_fe_boot.ster, replace
	xtset id year
} 
*FGLS attempt
xtivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg _Iind_1d* _Iocc_1d* ($xlist = $ivlist), fe
predict uhat, e
reg uhat l.uhat 
global rhohat = _b["L.uhat"]
drop uhat
forvalues iter=1/15{
	qui foreach zv of varlist mm_pos mm_neg cmm_pos cmm_neg $xlist $ivlist lwage{		
		gen `zv'_R =`zv'
		replace `zv'= `zv'_R  - ${rhohat}*l.`zv'_R 
		replace `zv' = `zv'_R*(1-${rhohat}^2)^0.5 if obs1==1 & `zv'==.
		_crcslbl `zv'_R `zv'
	}
	xtivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg _Iind_1d* _Iocc_1d* ($xlist = $ivlist) , fe /*[aw=inv_omega] */
	estimate save ${result}/iv_cmm_mm_pos_neg_fe_fgls.ster, replace
	predict uhat, e
	reg uhat l.uhat /*, fe  */
	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist mm_pos mm_neg cmm_pos cmm_neg $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}




*with robust errors for baseline
xtivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg _Iind_1d* _Iocc_1d* ($xlist = $ivlist), robust fe
estimate save ${result}/iv_cmm_mm_pos_neg_fe_rbst.ster, replace


/*------------------------------------------------------------------------------------*/
/* individual component mismatch */

global xlist  ability_??_ten_occ skill_??_ten_occ $xlist_0
xi: reg lwage absmm_?? $xlist $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(${vcetxt})
estimate save ${result}/ols_ind_mm.ster, replace

global xlist  ability_??_ten_occ skill_??_ten_occ $xlist_0
global ivlist ability_??_ten_occ_iv skill_??_ten_occ_iv $ivlist_0
if("$varmeth" == "boot" & "$varcor" == "cid"){
	xtset,clear
	bootstrap, cluster(id_job)  seed(9487): ivregress 2sls lwage absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d 
	estimate save ${result}/iv_ind_mm_boot.ster, replace
	xtset id year
}
else{
	xi: ivregress 2sls lwage absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(${vcetxt})
} 
estimate save ${result}/iv_ind_mm.ster, replace

/*------------------------------------------------------------------------------------*/
/* individual component mismatch with tenure */

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0
xi: reg lwage absmm_?? $xlist $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(${vcetxt})
estimate save ${result}/ols_ind_mm_ten.ster, replace

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0
global ivlist absmm_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv $ivlist_0
if("$varmeth" == "boot" & "$varcor" == "cid"){
	xtset, clear
	bootstrap, cluster(id_job)  seed(9487): ivregress 2sls  lwage absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d 
	estimate save ${result}/iv_ind_mm_ten_boot.ster, replace
	xtset id year
} 
xi: ivregress 2sls lwage absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(${vcetxt})
estimate save ${result}/iv_ind_mm_ten.ster, replace

/*------------------------------------------------------------------------------------*/
/* individual component cumulative mismatch */

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0 
xi: reg lwage cmm_aa cmm_bb cmm_cc absmm_?? $xlist $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(${vcetxt})
estimate save ${result}/ols_ind_cmm_mm.ster, replace

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0 
global ivlist absmm_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv $ivlist_0
if("$varmeth" == "boot" & "$varcor" == "cid"){
	xtset,clear
	bootstrap, cluster(id)  seed(9487): ivregress 2sls  lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d
	estimate save ${result}/iv_ind_cmm_mm_boot.ster, replace
	xtset id year
} 
xi: ivregress 2sls lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(${vcetxt})
estimate save ${result}/iv_ind_cmm_mm.ster, replace

*FGLS attempt
xi: ivregress 2sls lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d
predict uhat, residuals
reg uhat l.uhat /*, fe  */
global rhohat = _b["L.uhat"]
drop uhat
forvalues iter=1/15{
	qui foreach zv of varlist cmm_aa cmm_bb cmm_cc absmm_?? $zlist ability_?? skill_??  $xlist $ivlist lwage{
		gen `zv'_R =`zv'
		replace `zv'= `zv'_R  - ${rhohat}*l.`zv'_R 
		replace `zv' = `zv'_R*(1-${rhohat}^2)^0.5 if obs1==1 & `zv'==.
		_crcslbl `zv'_R `zv'
	}
	xi: ivregress 2sls lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d 
	estimate save ${result}/iv_ind_cmm_mm_fgls.ster, replace
	predict uhat, residuals
	reg uhat l.uhat /*, fe  */
	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist cmm_aa cmm_bb cmm_cc absmm_?? $zlist ability_?? skill_??  $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}


*with robust errors for baseline
xi: ivregress 2sls lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmm_mm_rbst.ster, replace


/*------------------------------------------------------------------------------------*/
/* individual component cumulative mismatch , fixed effects*/

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0 
xtset id year
xtreg lwage cmm_aa cmm_bb cmm_cc absmm_?? $xlist   skill_?? _Iind_1d* _Iocc_1d*, nonest vce(${vcetxt}) fe
estimate save ${result}/ols_ind_cmm_mm_fe.ster, replace

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0 
global ivlist absmm_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv $ivlist_0
xtivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) skill_?? _Iind_1d* _Iocc_1d* , ${xtvcetxt} fe
estimate save ${result}/iv_ind_cmm_mm_fe.ster, replace

if("$varmeth" == "boot" & "${varcor}"=="cid"){
	xtset, clear
	bootstrap, cluster(id_job)  seed(9487): xtivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) skill_?? _Iind_1d* _Iocc_1d*, i(id) fe
	estimate save ${result}/iv_ind_cmm_mm_fe_boot.ster, replace
	xtset id year
} 
*FGLS
xtreg lwage cmm_aa cmm_bb cmm_cc absmm_?? $xlist   skill_?? _Iind_1d* _Iocc_1d*, fe
predict uhat, e
reg uhat l.uhat /* mm cmm  $zlist ability_mean skill_mean i.ind_1d i.occ_1d ${xlist} */
global rhohat = _b["L.uhat"]
drop uhat
forvalues iter=1/15{
	qui foreach zv of varlist cmm_aa cmm_bb cmm_cc absmm_?? skill_?? $xlist $ivlist lwage{
		gen `zv'_R =`zv'
		replace `zv' = `zv'_R  - ${rhohat}*l.`zv'_R 
		replace `zv' = `zv'_R*(1-${rhohat}^2)^0.5 if obs1==1 & `zv'==.
		_crcslbl `zv'_R `zv'
	}
	xtivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist)  skill_?? _Iind_1d* _Iocc_1d*, fe /*[aw = inv_omega]*/
	estimate save ${result}/iv_ind_cmm_mm_fe_fgls.ster, replace
	predict uhat, e
	reg uhat l.uhat /*, fe  */
	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist cmm_aa cmm_bb cmm_cc absmm_?? skill_?? $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}


*with robust errors for baseline
xtivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist)  skill_?? _Iind_1d* _Iocc_1d*, robust fe
estimate save ${result}/iv_ind_cmm_mm_fe_rbst.ster, replace


/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* creating tables and figures */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
if("${varcor}" == "cid"){
	global labtxt = "Clustered"
} 
else if("${varcor}" == "rbst"){
	global labtxt = "Robust"
} 

/* mismatch */

estimate clear
estimate use ${result}/ols_mm.ster
estimate store ols_mm
estimate use ${result}/ols_mm_ten.ster
estimate store ols_mm_ten
estimate use ${result}/ols_cmm_mm.ster
estimate store ols_cmm_mm
estimate use ${result}/ols_cmm_mm_fe.ster
estimate store ols_cmm_mm_fe

estimate use ${result}/iv_mm.ster
estimate store iv_mm
estimate use ${result}/iv_mm_ten.ster
estimate store iv_mm_ten
estimate use ${result}/iv_cmm_mm.ster
estimate store iv_cmm_mm
estimate use ${result}/iv_cmm_mm_fe.ster
estimate store iv_cmm_mm_fe

estimate use ${result}/iv_cmm_mm_fe_rbst.ster
estimate store iv_cmm_mm_fe_rbst
estimate use ${result}/iv_cmm_mm_rbst.ster
estimate store iv_cmm_mm_rbst
estimate use ${result}/iv_cmm_mm_fe_fgls.ster
estimate store iv_cmm_mm_fe_fgls
estimate use ${result}/iv_cmm_mm_fgls.ster
estimate store iv_cmm_mm_fgls

estimate use ${result}/iv_cmm_mm_boot.ster
estimate store iv_cmm_mm_boot
estimate use ${result}/iv_cmm_mm_fe_boot.ster
estimate store iv_cmm_mm_fe_boot



/* tex */		   
esttab iv_mm iv_mm_ten iv_cmm_mm iv_cmm_mm_fe ols_cmm_mm ols_cmm_mm_fe ///
                   using ${result}/table_${diminitls}_${varcor}_${varmeth}.tex, b(4) ///
                   r2 nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "${labtxt} standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV" "IV" "IV" "IV-FE" "OLS" "OLS-FE") ///
		   order(mm mm_ten_occ cmm ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ) ///
                   star(\sym{*} 0.10 \sym{**} 0.05 \sym{***} 0.01) replace
		   
esttab iv_mm iv_mm_ten iv_cmm_mm iv_cmm_mm_fe ols_cmm_mm ols_cmm_mm_fe ///
                   using ${result}/table_apx_${diminitls}_${varcor}_${varmeth}.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\hline  " ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "${labtxt} standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   title("Wage Regression with Mismatch (Full Results)") ///
		   drop(_I*) ///
		   mtitles("IV" "IV" "IV" "IV-FE" "OLS" "OLS-FE") ///
		   order(mm mm_ten_occ cmm ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ ten* exp* oj $zlist _cons) ///
                   star(\sym{*} 0.10 \sym{**} 0.05 \sym{***} 0.01) replace
*FGLS
esttab iv_cmm_mm_rbst iv_cmm_mm_fe_rbst iv_cmm_mm iv_cmm_mm_fe iv_cmm_mm_fgls iv_cmm_mm_fe_fgls ///
                   using ${result}/table_${diminitls}_${varcor}_${varmeth}_fgls.tex, b(4) ///
                   r2 nodepvars gaps label not nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "${labtxt} standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(*occ_1* *ind_1* ten* exp* oj $zlist _cons) ///
		   mtitles("IV-RBST" "IV-FE-RBST" "IV-CLU" "IV-FE-CLU" "IV-FGLS" "IV-FE-FGLS") ///
		   order(mm mm_ten_occ cmm ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ ten* exp* oj $zlist _cons) ///
                   star(\sym{*} 0.10 \sym{**} 0.05 \sym{***} 0.01) replace

/*------------------------------------------------------------------------------------*/
/* positive & negative mismatch */

estimate clear
estimate use ${result}/ols_mm_pos_neg.ster
estimate store ols_mm_pos_neg
estimate use ${result}/iv_mm_pos_neg.ster
estimate store iv_mm_pos_neg

estimate use ${result}/ols_mm_ten_pos_neg.ster
estimate store ols_mm_ten_pos_neg
estimate use ${result}/iv_mm_ten_pos_neg.ster
estimate store iv_mm_ten_pos_neg

estimate use ${result}/ols_cmm_mm_pos_neg.ster
estimate store ols_cmm_mm_pos_neg
estimate use ${result}/iv_cmm_mm_pos_neg.ster
estimate store iv_cmm_mm_pos_neg
estimate use ${result}/iv_cmm_mm_pos_neg_fe.ster
estimate store ols_cmm_mm_pos_neg_fe
estimate use ${result}/iv_cmm_mm_pos_neg_fe.ster
estimate store iv_cmm_mm_pos_neg_fe

estimate use ${result}/iv_cmm_mm_pos_neg_boot.ster
estimate store iv_cmm_mm_pos_neg_boot
estimate use ${result}/iv_cmm_mm_pos_neg_fe_boot.ster
estimate store iv_cmm_mm_pos_neg_fe_boot
estimate use ${result}/iv_cmm_mm_pos_neg_rbst.ster
estimate store iv_cmm_mm_pos_neg_rbst
estimate use ${result}/iv_cmm_mm_pos_neg_fe_rbst.ster
estimate store iv_cmm_mm_pos_neg_fe_rbst
estimate use ${result}/iv_cmm_mm_pos_neg_fgls.ster
estimate store iv_cmm_mm_pos_neg_fgls
estimate use ${result}/iv_cmm_mm_pos_neg_fe_fgls.ster
estimate store iv_cmm_mm_pos_neg_fe_fgls

/* tex */
esttab iv_mm_pos_neg iv_mm_ten_pos_neg iv_cmm_mm_pos_neg iv_cmm_mm_pos_neg_fe ols_cmm_mm_pos_neg ols_cmm_mm_pos_neg_fe ///
                   using ${result}/table_${diminitls}_${varcor}_${varmeth}_pos_neg.tex, b(4) ///
                   r2 nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "${labtxt} standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV" "IV" "IV" "IV-FE" "OLS" "OLS-FE") ///
		   order(mm_??? mm_???_ten_occ cmm_??? ) ///
                   star(\sym{*} 0.10 \sym{**} 0.05 \sym{***} 0.01) replace

esttab iv_mm_pos_neg iv_mm_ten_pos_neg iv_cmm_mm_pos_neg iv_cmm_mm_pos_neg_fe ols_cmm_mm_pos_neg ols_cmm_mm_pos_neg_fe ///
                   using ${result}/table_apx_${diminitls}_${varcor}_${varmeth}_pos_neg.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\hline  " ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "${labtxt} standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   title("Wage Regression with Positive and Negative Mismatch (Full Results)") ///
		   drop(_I*) ///
		   mtitles("IV" "IV" "IV" "IV-FE" "OLS" "OLS-FE") ///
		   order(mm_??? mm_???_ten_occ cmm_???) ///
                   star(\sym{*} 0.10 \sym{**} 0.05 \sym{***} 0.01) replace

*FGLS
esttab iv_cmm_mm_pos_neg_rbst iv_cmm_mm_pos_neg_fe_rbst iv_cmm_mm_pos_neg iv_cmm_mm_pos_neg_fe iv_cmm_mm_pos_neg_fgls iv_cmm_mm_pos_neg_fe_fgls ///
                   using ${result}/table_${diminitls}_${varcor}_${varmeth}_pos_neg_fgls.tex, b(4) ///
                   r2 nodepvars gaps label not nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "${labtxt} standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(*occ_1* *ind_1* ten* exp* oj $zlist _cons) ///
		   mtitles("IV-RBST" "IV-FE-RBST" "IV-CLU" "IV-FE-CLU" "IV-FGLS" "IV-FE-FGLS") ///
		   order(mm_??? mm_???_ten_occ cmm_??? ) ///
                   star(\sym{*} 0.10 \sym{**} 0.05 \sym{***} 0.01) replace

  
/*------------------------------------------------------------------------------------*/
/* individual component mismatch */

estimate clear
estimate use ${result}/ols_ind_mm.ster
estimate store ols_ind_mm
estimate use ${result}/ols_ind_mm_ten.ster
estimate store ols_ind_mm_ten
estimate use ${result}/ols_ind_cmm_mm.ster
estimate store ols_ind_cmm_mm
estimate use ${result}/ols_ind_cmm_mm_fe.ster
estimate store ols_ind_cmm_mm_fe

estimate use ${result}/iv_ind_mm.ster
estimate store iv_ind_mm
estimate use ${result}/iv_ind_mm_ten.ster
estimate store iv_ind_mm_ten
estimate use ${result}/iv_ind_cmm_mm.ster
estimate store iv_ind_cmm_mm
estimate use ${result}/iv_ind_cmm_mm_fe.ster
estimate store iv_ind_cmm_mm_fe


estimate use ${result}/iv_ind_cmm_mm_boot.ster
estimate store iv_ind_cmm_mm_boot
estimate use ${result}/iv_ind_cmm_mm_fe_boot.ster
estimate store iv_ind_cmm_mm_fe_boot
estimate use ${result}/iv_ind_cmm_mm_fgls.ster
estimate store iv_ind_cmm_mm_fgls
estimate use ${result}/iv_ind_cmm_mm_fe_fgls.ster
estimate store iv_ind_cmm_mm_fe_fgls
estimate use ${result}/iv_ind_cmm_mm_rbst.ster
estimate store iv_ind_cmm_mm_rbst
estimate use ${result}/iv_ind_cmm_mm_fe_rbst.ster
estimate store iv_ind_cmm_mm_fe_rbst


/* tex */
esttab iv_ind_mm iv_ind_mm_ten iv_ind_cmm_mm iv_ind_cmm_mm_fe ols_ind_cmm_mm ols_ind_cmm_mm_fe ///
                   using ${result}/table_${diminitls}_${varcor}_${varmeth}_ind.tex, b(4) ///
                   r2 nodepvars gaps label not nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "${labtxt} standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV" "IV" "IV" "IV-FE" "OLS" "OLS-FE") ///
		   order(absmm_aa absmm_bb absmm_cc absmm_aa_t* absmm_bb_t* absmm_cc_t* cmm_aa cmm_bb cmm_cc ability_?? ability_??_* skill_?? skill_??_*) ///
                   star(\sym{*} 0.10 \sym{**} 0.05 \sym{***} 0.01) replace
		   
esttab iv_ind_mm iv_ind_mm_ten iv_ind_cmm_mm iv_ind_cmm_mm_fe ols_ind_cmm_mm ols_ind_cmm_mm_fe ///
                   using ${result}/table_apx_${diminitls}_${varcor}_${varmeth}_ind.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\hline  " ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "${labtxt} standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I*) ///
		   mtitles("IV" "IV" "IV" "IV-FE" "OLS" "OLS-FE") ///
		   title("Wage Regression with Mismatch by Components (Full Results)") ///
		   order(absmm_aa absmm_bb absmm_cc absmm_aa_t* absmm_bb_t* absmm_cc_t* cmm_aa cmm_bb cmm_cc ability_?? ability_??_* skill_?? skill_??_* ten* exp* oj $zlist _cons) ///
                   star(\sym{*} 0.10 \sym{**} 0.05 \sym{***} 0.01) replace

*FGLS
esttab iv_ind_cmm_mm_rbst iv_ind_cmm_mm_fe_rbst iv_ind_cmm_mm iv_ind_cmm_mm_fe iv_ind_cmm_mm_fgls iv_ind_cmm_mm_fe_fgls ///
                   using ${result}/table_${diminitls}_${varcor}_${varmeth}_ind_fgls.tex, b(4) ///
                   r2 nodepvars gaps label not nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "${labtxt} standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(*occ_1* *ind_1* ten* exp* oj $zlist _cons) ///
		   mtitles("IV-RBST" "IV-FE-RBST" "IV-CLU" "IV-FE-CLU" "IV-FGLS" "IV-FE-FGLS") ///
		   order(absmm_aa absmm_bb absmm_cc absmm_aa_t* absmm_bb_t* absmm_cc_t* cmm_aa cmm_bb cmm_cc ability_?? ability_??_* skill_?? skill_??_*) ///
                   star(\sym{*} 0.10 \sym{**} 0.05 \sym{***} 0.01) replace
		   
esttab iv_ind_mm iv_ind_mm_ten iv_ind_cmm_mm iv_ind_cmm_mm_fe iv_ind_cmm_mm_fgls iv_ind_cmm_mm_fe_fgls ///
                   using ${result}/table_apx_${diminitls}_${varcor}_${varmeth}_ind_fgls.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\hline  " ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "${labtxt} standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I*) ///
		   mtitles("IV" "IV" "IV" "IV-FE" "IV-FGLS" "IV-FE-FGLS") ///
		   title("Wage Regression with Mismatch by Components (Full Results)") ///
		   order(absmm_aa absmm_bb absmm_cc absmm_aa_t* absmm_bb_t* absmm_cc_t* cmm_aa cmm_bb cmm_cc ability_?? ability_??_* skill_?? skill_??_* ten* exp* oj $zlist _cons) ///
                   star(\sym{*} 0.10 \sym{**} 0.05 \sym{***} 0.01) replace


/*------------------------------------------------------------------------------------*/		   
/* predicted effect of mismatch on wages */

estimate clear
estimate use ${result}/iv_cmm_mm.ster
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

/*matrix colnames pred_mm_pt = "MM_5_Years" "MM_10_Years" "MM_15_Years" "Cumul_MM"
matrix rownames pred_mm_pt = "10_Percentile" "30_Percentile" "50_Percentile" "70_Percentile" "90_Percentile"
putexcel A1=matrix(pred_mm_pt, names) using ${result}/table_${diminitls}_pred_mm_pt.xls, replace

matrix colnames pred_mm_sd = "MM_5_Years" "MM_10_Years" "MM_15_Years" "Cumul_MM"
matrix rownames pred_mm_sd = "10_Percentile" "30_Percentile" "50_Percentile" "70_Percentile" "90_Percentile"
putexcel A1=matrix(pred_mm_sd, names) using ${result}/table_${diminitls}_pred_mm_sd.xls, replace
*/

matrix colnames pred_mm_pt_sd = "MM_5_Years" "MM_10_Years" "MM_15_Years" "Cumul_MM"
matrix rownames pred_mm_pt_sd = "90_Percentile" "90_Percentile" "70_Percentile" "70_Percentile" "50_Percentile" "50_Percentile" "30_Percentile" "30_Percentile" "10_Percentile" "10_Percentile"
putexcel A1=matrix(pred_mm_pt_sd, names) using ${result}/table_${diminitls}_pred_mm.xls, replace


/*------------------------------------------------------------------------------------*/
/* Figures for mismatch predicted effect on wages */
/*------------------------------------------------------------------------------------*/
 
use ${result}/yearly_03.dta,clear
 
***********************************************
* parial correlation between wage and mismatch:
qui ivregress 2sls lwage ($xlist_0 = $ivlist_0) $zlist_0 i.ind_1d i.occ_1d
predict lwage_Mxlist 
qui ivregress 2sls mm ($xlist_0 = $ivlist_0) $zlist_0 i.ind_1d i.occ_1d
predict mm_Mxlist

lpoly lwage_Mxlist mm_Mxlist, nograph gen(lw_M_x lw_M_y) se(slw_M_y)
gen ch_M_y = lw_M_y + slw_M_y*1.645
gen cl_M_y = lw_M_y - slw_M_y*1.645
twoway (rarea ch_M_y cl_M_y lw_M_x, fcolor(gs10) lwidth(none) lcolor(gs10)) ///
(line lw_M_y lw_M_x, clcolor(black) clpattern(solid) clwidth(thick)) , ///
ytitle("Log Wage, Residuals", size(medlarge)) ///
xtitle("Mismatch, Residuals", size(medlarge)) ///
title("Log Wage by Mismatch, Partial Correlation", size(large)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(, grid gstyle(dot)) ///
 legend(off) /// yscale(range(0.15 0.24) noextend)
saving(${result}/lwage_${diminitls}_mm_M, replace)
graph export ${result}/lwage_${diminitls}_mm_M.eps, replace

drop c?_M_y *Mxlist

**********************************
* Quasi param, residual wage trajectory by cmm


global xlist $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv

qui ivregress 2sls lwage ($xlist = $ivlist ) $zlist_0 ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
predict lwage_Mxlist, residual

qui ivregress 2sls lwage ($xlist_0 = $ivlist_0 ) $zlist_0 i.ind_1d i.occ_1d, vce(robust)
predict lwage_Mxlist_0, residual



cumul mm if age>=20 & age<=30, gen(mm_20rnk) equal
cumul cmm if age>=30 & age<=40, gen(cmm_30srnk) equal
cumul cmm if age==30 , gen(cmm_30rnk) equal

bysort id: egen cmm_rnk_tmp = mean(cmm_30rnk)
replace cmm_30rnk = cmm_rnk_tmp 
bysort id: egen mm_rnk_tmp = mean(mm_20rnk)
replace mm_20rnk = mm_rnk_tmp 
drop *mm_rnk_tmp

gen t_a30 = (age==30)
bysort id: egen exp_a30_tmp = mean(exp) if (age==30)
bysort id: egen exp_a30_0 = max(exp_a30_tmp) 
gen exp_a30 = exp - exp_a30_0

* cum mismatch in 30s
twoway (lpoly lwage_Mxlist exp_a30 if cmm_30rnk >=.0 & cmm_30rnk <=.1 , lwidth(thick)) ///
(lpoly lwage_Mxlist exp_a30 if cmm_30rnk >=.9 & cmm_30rnk <=1., lwidth(thick) lpattern(-)) if exp_a30<=15  & exp_a30>0, ///
xtitle("Experience since age 30", size(medlarge)) ///
ytitle("Log Wage (residuals)", size(medlarge)) /// 
/// title("Quasi-parametric effect of mismatch through age 30", size(large)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) yscale(range(-.15 .15)) ///
legend(lab(1 "Best matched decile") lab(2 "Worst matched decile") ///
 ring(0) position(8) rows(2) ) ///
saving(${result}/nonparam_${diminitls}_cmm, replace) 
graph export ${result}/nonparam_${diminitls}_cmm.eps, replace

local ea_i = 1
matrix quasiparam_difs = J(2,5,0.0)
forvalues ea = 5(5)20{
	sum lwage_Mxlist if (exp_a30>=`ea'-1 & exp_a30<=`ea'+1) & (cmm_30rnk >=0.00 & cmm_30rnk<=0.1), meanonly
	matrix quasiparam_difs[1,`ea_i'] = r(mean)
	sum lwage_Mxlist if (exp_a30>=`ea'-1 & exp_a30<=`ea'+1) & (cmm_30rnk >=0.9 & cmm_30rnk<=1.0), meanonly
	matrix quasiparam_difs[2,`ea_i'] = r(mean)
	local ea_i = `ea_i'+1
}

* compare to 1 d
*capture drop cmm_1d
*gen cmm_1d = abs(ability_pca_mean - cskill)
qui sum cmm_1d
replace cmm_1d = cmm_1d/r(sd)
cumul cmm_1d if age==30, gen(cmm1d_30rnk) 
bysort id: egen tmp30 = mean(cmm1d_30rnk )
replace cmm1d_30rnk = tmp30
drop tmp30

sum lwage_Mxlist if exp_a30>=0 & exp_a30<=15 & cmm_30rnk>=0.0 & cmm_30rnk<=0.1
sum lwage_Mxlist if exp_a30>=0 & exp_a30<=15 & cmm_30rnk>=0.9 & cmm_30rnk<=1.0
sum lwage_Mxlist if exp_a30>=0 & exp_a30<=15 & cmm1d_30rnk>=0.0 & cmm1d_30rnk<=0.1
sum lwage_Mxlist if exp_a30>=0 & exp_a30<=15 & cmm1d_30rnk>=0.9 & cmm1d_30rnk<=1.0

* cum mismatch in 30s
twoway (lpoly lwage_Mxlist exp_a30 if cmm_30rnk >=.0 & cmm_30rnk <=.1 , lwidth(thick)) ///
(lpoly lwage_Mxlist exp_a30 if cmm_30rnk >=.9 & cmm_30rnk <=1., lwidth(thick) lpattern(-)) ///
(lpoly lwage_Mxlist exp_a30 if cmm1d_30rnk >=.0 & cmm1d_30rnk <=.1 , lwidth(thick) lcolor(eltblue) lpattern(_-)) ///
(lpoly lwage_Mxlist exp_a30 if cmm1d_30rnk >=.9 & cmm1d_30rnk <=1., lwidth(thick)  lcolor(erose) lpattern(shortdash_dot)) if exp_a30<=15  & exp_a30>=0, ///
xtitle("Experience since age 30", size(medlarge)) ///
ytitle("Log Wage (residuals)", size(medlarge)) /// 
/// title("Quasi-parametric effect of mismatch through age 30", size(large)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) yscale(range(-.15 .15)) ///
legend(lab(1 "Best matched decile") lab(2 "Worst matched decile") lab(3 "Best matched decile, 1D") lab(4 "Worst matched decile, 1D") ///
 ring(0) position(8) rows(2) ) ///
saving(${result}/nonparam_${diminitls}_cmm_compare1d, replace) 
graph export ${result}/nonparam_${diminitls}_cmm_compare1d.eps, replace
graph export ${result}/nonparam_${diminitls}_cmm_compare1d.png, replace



*compute wage loss of average cmm_30rnk.
*bysort exp_a30: egen avgwg = mean(lwage)
*by exp_a30 : egen avghr = mean(hrs)
sum lwage if exp_a30<=15 & exp_a30>0, meanonly
gen earn_Mxlist = exp(lwage_Mxlist+ r(mean))/100*hrs*52 
bysort id: egen cumearn_Mxlist = total(earn_Mxlist ) if exp_a30<=15 & exp_a30>=0
sum cumearn_Mxlist if (cmm_30rnk >=0.01 & cmm_30rnk<=0.1) & exp_a30==15, meanonly
matrix quasiparam_difs[1,`ea_i'] = r(mean)
sum cumearn_Mxlist if (cmm_30rnk >=0.9 & cmm_30rnk<=1.) & exp_a30==15, meanonly
matrix quasiparam_difs[2,`ea_i'] = r(mean)

matrix colnames quasiparam_difs = "5_Years" "10_Years" "15_Years" "20_Years" "Accum Difference"
matrix rownames quasiparam_difs = "0-10_Percentile" "90-100_Percentile" 
putexcel A1=matrix(quasiparam_difs, names) using ${result}/table_${diminitls}_quasiparam_cmm.xls, replace

*mismatch in 20s
twoway (lpoly lwage_Mxlist exp_a30 if mm_20rnk >=.05 & mm_20rnk <=.15 ) (lpoly lwage_Mxlist exp_a30 if mm_20rnk >=.85 & mm_20rnk <=.95) if exp_a30<=20  & exp_a30>0, ///
xtitle("Experience since age 30", size(medlarge)) ///
ytitle("Log Wage (residuals)", size(medlarge)) ///
title("Quasi-parametric effect of mismatch in one's 20s", size(large)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(lab(1 "10th percentile, best matched") lab(2 "90th percentile, worse matched") ///
 ring(0) position(8) rows(2) ) ///
saving(${result}/nonparam_${diminitls}_mm, replace) 
graph export ${result}/nonparam_${diminitls}_mm.eps, replace


/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* Some examples of switching*/

bysort id: egen nswitch_occ = total(switch_occ)
preserve
drop if nswitch_occ < 1 | nswitch_occ> 8
qui sum delmm, detail

gen exampleid_tmp = (delmm< r(p25))
by id: egen exampleid = max(exampleid_tmp)
xtset id year

gen lwage100 = log(exp(lwage)/100)
gen mswitch_wage = lwage100 if switch_occ == 1 | switch_occ[_n+1] == 1
gen mswitch_mm = mm if switch_occ == 1 | switch_occ[_n+1] == 1

twoway (line lwage100 year, yaxis(1) lwidth(thick)) (line mm year, yaxis(2) lwidth(thick)) ///
(scatter mswitch_wage year, yaxis(1) msymbol(0) msize(large) mcolor(gs8)) (scatter mswitch_mm year, yaxis(2) msymbol(0) msize(large)  mcolor(gs8)) ///
if id == 223, ///
///xline(1986, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xtitle("Year", size(medlarge)) ///
ytitle("Log Wage", axis(1) size(medlarge)) ytitle("Mismatch", axis(2) size(medlarge)) ///
///title("Mr 223, from sales clerk to chef", size(large)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(order(1 "Log Wages" 3 "Mismatch") ///
 ring(0) position(3) rows(2)) ///
saving(${result}/mr223, replace) 
graph export ${result}/mr223.eps, replace
graph export ${result}/mr223.png, replace

/*twoway (line lwage100 year, yaxis(1) lwidth(thick)) (line mm year, yaxis(2) lwidth(thick)) if id == 120, ///
xline(1985, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xline(1986, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xtitle("Year", size(medlarge)) ///
ytitle("Log Wage", axis(1) size(medlarge)) ytitle("Mismatch", axis(2) size(medlarge)) ///
title("Mr 120, from bar tender to salesperson to realator", size(large)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(lab(1 "Log Wages") lab(2 "Mismatch") ///
 ring(0) position(3) rows(2) ) ///
saving(${result}/mr120, replace) 
graph export ${result}/mr120.eps, replace
graph export ${result}/mr120.png, replace

twoway (line lwage100 year, yaxis(1) lwidth(thick)) (line mm year, yaxis(2) lwidth(thick)) if id == 195 & year<2009, ///
xline(1993, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xline(1998, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xtitle("Year", size(medlarge)) ///
ytitle("Log Wage", axis(1) size(medlarge)) ytitle("Mismatch", axis(2) size(medlarge)) ///
title("Mr 195, from salesperson to administrator to executive", size(large)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(lab(1 "Log Wages") lab(2 "Mismatch") ///
 ring(0) position(3) rows(2) ) ///
saving(${result}/mr195, replace) 
graph export ${result}/mr195.eps, replace
graph export ${result}/mr195.png, replace

twoway (line lwage100 year, yaxis(1) lwidth(thick)) (line mm year, yaxis(2) lwidth(thick)) if id == 484 & year<2009, ///
xline(1993, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xline(1998, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xtitle("Year", size(medlarge)) ///
ytitle("Log Wage", axis(1) size(medlarge)) ytitle("Mismatch", axis(2) size(medlarge)) ///
title("Mr 484, from salesperson to administrator to executive", size(large)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(lab(1 "Log Wages") lab(2 "Mismatch") ///
 ring(0) position(3) rows(2) ) ///
saving(${result}/mr484, replace) 
graph export ${result}/mr484.eps, replace
graph export ${result}/mr484.png, replace
*/
twoway (line lwage100 year, yaxis(1) lwidth(thick)) (line mm year, yaxis(2) lwidth(thick)) ///
(scatter mswitch_wage year, yaxis(1) msymbol(0) msize(large) mcolor(gs8)) (scatter mswitch_mm year, yaxis(2) msymbol(0) msize(large)  mcolor(gs8)) ///
 if id == 817 & year~=1989, ///
/// xline(1987, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
/// xline(1990, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
 xtitle("Year", size(medlarge)) ///
 ytitle("Log Wage", axis(1) size(medlarge)) ytitle("Mismatch", axis(2) size(medlarge)) ///
///title("Mr 817, from groundskeeper to manager to precision production foreman", size(medium)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(order(1 "Log Wages" 3 "Mismatch") ///
 ring(0) position(3) rows(2) ) ///
saving(${result}/mr817, replace) 
graph export ${result}/mr817.eps, replace
graph export ${result}/mr817.png, replace

twoway (line lwage100 year, yaxis(1) lwidth(thick)) (line mm year, yaxis(2) lwidth(thick)) /// 
(scatter mswitch_wage year, yaxis(1) msymbol(0) msize(large) mcolor(gs8)) (scatter mswitch_mm year, yaxis(2) msymbol(0) msize(large)  mcolor(gs8)) ///
if id == 3757, ///
/// xline(1985, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
/// xline(1987, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xtitle("Year", size(medlarge)) ///
ytitle("Log Wage", axis(1) size(medlarge)) ytitle("Mismatch", axis(2) size(medlarge)) ///
///title("Mr 3757, from agr worker to manager to civil engineer", size(medium)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(order(1 "Log Wages" 3 "Mismatch") ///
 ring(0) position(3) rows(2) ) ///
saving(${result}/mr3757, replace) 
graph export ${result}/mr3757.eps, replace
graph export ${result}/mr3757.png, replace

/*
twoway (line lwage100 year, yaxis(1) lwidth(thick)) (line mm year, yaxis(2) lwidth(thick)) if id == 41, ///
xline(1986, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xline(1988, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xline(1996, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xtitle("Year", size(medlarge)) ///
ytitle("Log Wage", axis(1) size(medlarge)) ytitle("Mismatch", axis(2) size(medlarge)) ///
title("Mr 41, from guard to realator to admin to sales supervisor", size(medium)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(lab(1 "Log Wages") lab(2 "Mismatch") ///
 ring(0) position(3) rows(2) ) ///
saving(${result}/mr41, replace) 
graph export ${result}/mr41.eps, replace
graph export ${result}/mr41.png, replace

twoway (line lwage100 year, yaxis(1) lwidth(thick)) (line mm year, yaxis(2) lwidth(thick)) if id == 4106, ///
xline(1989, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xline(1993, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xline(2007, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xtitle("Year", size(medlarge)) ///
ytitle("Log Wage", axis(1) size(medlarge)) ytitle("Mismatch", axis(2) size(medlarge)) ///
title("Mr 4106, from technician to admin support to software developer to cartographer", size(medium)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(lab(1 "Log Wages") lab(2 "Mismatch") ///
 ring(0) position(3) rows(2) ) ///
saving(${result}/mr4106, replace) 
graph export ${result}/mr4106.eps, replace
graph export ${result}/mr4106.png, replace

twoway (line lwage100 year, yaxis(1) lwidth(thick)) (line mm year, yaxis(2) lwidth(thick)) if id == 12116 & year<2008, ///
xline(1999, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xtitle("Year", size(medlarge)) ///
ytitle("Log Wage", axis(1) size(medlarge)) ytitle("Mismatch", axis(2) size(medlarge)) ///
title("Mr 12116, from insurance sales to financial specialist", size(medium)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(lab(1 "Log Wages") lab(2 "Mismatch") ///
 ring(0) position(3) rows(2) ) ///
saving(${result}/mr12116, replace) 
graph export ${result}/mr12116.eps, replace
graph export ${result}/mr12116.png, replace
*/
twoway (line lwage100 year, yaxis(1) lwidth(thick)) (line mm year, yaxis(2) lwidth(thick)) ///
(scatter mswitch_wage year, yaxis(1) msymbol(0) msize(large) mcolor(gs8)) (scatter mswitch_mm year, yaxis(2) msymbol(0) msize(large)  mcolor(gs8)) ///
if id == 5504, ///
///xline(1987, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
///xline(1993, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xtitle("Year", size(medlarge)) ///
ytitle("Log Wage", axis(1) size(medlarge)) ytitle("Mismatch", axis(2) size(medlarge)) ///
///title("Mr 5504, from admin to telecom repair to software developer", size(medium)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(order(1 "Log Wages" 3 "Mismatch") ///
 ring(0) position(3) rows(2) ) ///
saving(${result}/mr5504, replace) 
graph export ${result}/mr5504.eps, replace
graph export ${result}/mr5504.png, replace

/*
twoway (line lwage100 year, yaxis(1) lwidth(thick)) (line mm year, yaxis(2) lwidth(thick)) if id == 5398 & year>= 1980, ///
xline(1987, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xline(2008, lcolor(gs8) lwidth(thick) lpattern(dash))  ///
xtitle("Year", size(medlarge)) ///
ytitle("Log Wage", axis(1) size(medlarge)) ytitle("Mismatch", axis(2) size(medlarge)) ///
title("Mr 5398, from agr worker to financial sales to manager", size(medium)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(lab(1 "Log Wages") lab(2 "Mismatch") ///
 ring(0) position(3) rows(2) ) ///
saving(${result}/mr5398, replace) 
graph export ${result}/mr5398.eps, replace
graph export ${result}/mr5398.png, replace
*/


restore

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* switching probability regression */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

global prswlist $zlist ability_mean skill_mean i.occ_1d i.ind_1d tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj skill_mean_ten_occ ability_mean_ten_occ 
global prswlist_x tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj skill_mean_ten_occ ability_mean_ten_occ 
global prswlist_z $zlist ability_mean skill_mean i.occ_1d i.ind_1d
global prswlist_iv ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv skill_mean_ten_occ_iv ability_mean_ten_occ_iv

xtset id year
gen fswitch_occ = f.switch_occ

/*------------------------------------------------------------------------------------*/
/* linear probability regressions */

xi: reg fswitch_occ mm $prswlist, vce(robust)
estimate save ${result}/lpm_${diminitls}_mm.ster, replace

xi: ivregress 2sls fswitch_occ (${prswlist_x} = ${prswlist_iv}) mm $prswlist_z, vce(robust)
estimate save ${result}/iv_lpm_${diminitls}_mm.ster, replace

/*------------------------------------------------------------------------------------*/
/* lpm by components */

xi: reg fswitch_occ absmm_?? $prswlist, vce(robust)
estimate save ${result}/lpm_${diminitls}_mm_ind.ster, replace

xi: ivregress 2sls fswitch_occ absmm_aa absmm_bb absmm_cc ($prswlist_x = ${prswlist_iv}) $prswlist_z, vce(robust)
estimate save ${result}/iv_lpm_${diminitls}_mm_ind.ster, replace

/*------------------------------------------------------------------------------------*/
/* lpm with positive and negative mistmatch */

global prswlist_pos_neg $zlist i.occ_1d i.ind_1d tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global prswlist_x_pos_neg tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global prswlist_z_pos_neg $zlist i.occ_1d i.ind_1d
global prswlist_iv_pos_neg ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv

xi: reg fswitch_occ mm_pos mm_neg $prswlist_pos_neg, vce(robust)
estimate save ${result}/lpm_${diminitls}_mm_pos_neg.ster, replace

xi: ivregress 2sls fswitch_occ (${prswlist_x_pos_neg} = ${prswlist_iv_pos_neg}) mm_pos mm_neg $prswlist_z_pos_neg, vce(robust)
estimate save ${result}/iv_lpm_${diminitls}_mm_pos_neg.ster, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* creating tables and figures */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* switching probability regression table */

estimate clear
estimate use ${result}/lpm_${diminitls}_mm.ster
estimate store lpm_${diminitls}_mm
estimate use ${result}/iv_lpm_${diminitls}_mm.ster
estimate store iv_lpm_${diminitls}_mm

estimate use ${result}/lpm_${diminitls}_mm_ind.ster
estimate store lpm_${diminitls}_mm_ind
estimate use ${result}/iv_lpm_${diminitls}_mm_ind.ster
estimate store iv_lpm_${diminitls}_mm_ind

estimate use ${result}/lpm_${diminitls}_mm_pos_neg.ster
estimate store lpm_${diminitls}_mm_pos_neg
estimate use ${result}/iv_lpm_${diminitls}_mm_pos_neg.ster
estimate store iv_lpm_${diminitls}_mm_pos_neg

/* tex */
esttab iv_lpm_${diminitls}_mm iv_lpm_${diminitls}_mm_ind iv_lpm_${diminitls}_mm_pos_neg lpm_${diminitls}_mm lpm_${diminitls}_mm_ind lpm_${diminitls}_mm_pos_neg ///
                   using ${result}/table_${diminitls}_switch.tex, b(4) ///
                   nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "Robust standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   mtitles("LPM-IV" "LPM-IV" "LPM-IV" "LPM" "LPM" "LPM") ///
		   order(mm absmm_aa absmm_bb absmm_cc mm_pos mm_neg ability_mean ability_mean* skill_mean skill_mean*) drop(ten* exp* oj $zlist _cons _I*) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace

esttab iv_lpm_${diminitls}_mm iv_lpm_${diminitls}_mm_ind iv_lpm_${diminitls}_mm_pos_neg lpm_${diminitls}_mm lpm_${diminitls}_mm_ind lpm_${diminitls}_mm_pos_neg ///
                   using ${result}/table_apx_${diminitls}_switch.tex, b(4) se(4) ///
                   nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\hline  " ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   mtitles("LPM-IV" "LPM-IV" "LPM-IV" "LPM" "LPM" "LPM") ///
		   title("Regressions for Probability of Occupational Switch (Full Results)") ///
		   order(mm absmm_aa absmm_bb absmm_cc mm_pos mm_neg ability_mean ability_mean* skill_mean skill_mean* ten* exp* oj $zlist) drop(_I*) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace

/*------------------------------------------------------------------------------------*/
/* predicted effect of mismatch on switching probability */

estimate clear
estimate use ${result}/iv_lpm_${diminitls}_mm.ster
matrix pred_switch_pt = J(5,4,0.0)
matrix pred_switch_sd = J(5,4,0.0)

_pctile mm, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)

matrix pred_switch_pt[1,1] = _b["mm"]*`p90'
matrix pred_switch_pt[2,1] = _b["mm"]*`p70'
matrix pred_switch_pt[3,1] = _b["mm"]*`p50'
matrix pred_switch_pt[4,1] = _b["mm"]*`p30'
matrix pred_switch_pt[5,1] = _b["mm"]*`p10'

matrix pred_switch_sd[1,1] = _se["mm"]*`p90'
matrix pred_switch_sd[2,1] = _se["mm"]*`p70'
matrix pred_switch_sd[3,1] = _se["mm"]*`p50'
matrix pred_switch_sd[4,1] = _se["mm"]*`p30'
matrix pred_switch_sd[5,1] = _se["mm"]*`p10'

estimate clear
estimate use ${result}/iv_lpm_${diminitls}_mm_ind.ster
local ll = 2
foreach i of local nlist{

	_pctile absmm_`i', p(10 30 50 70 90)
	local p90 = r(r5)
	local p70 = r(r4)
	local p50 = r(r3)
	local p30 = r(r2)
	local p10 = r(r1)

	matrix pred_switch_pt[1,`ll'] = _b["absmm_`i'"]*`p90'
	matrix pred_switch_pt[2,`ll'] = _b["absmm_`i'"]*`p70'
	matrix pred_switch_pt[3,`ll'] = _b["absmm_`i'"]*`p50'
	matrix pred_switch_pt[4,`ll'] = _b["absmm_`i'"]*`p30'
	matrix pred_switch_pt[5,`ll'] = _b["absmm_`i'"]*`p10'
	
	matrix pred_switch_sd[1,`ll'] = _se["absmm_`i'"]*`p90'
	matrix pred_switch_sd[2,`ll'] = _se["absmm_`i'"]*`p70'
	matrix pred_switch_sd[3,`ll'] = _se["absmm_`i'"]*`p50'
	matrix pred_switch_sd[4,`ll'] = _se["absmm_`i'"]*`p30'
	matrix pred_switch_sd[5,`ll'] = _se["absmm_`i'"]*`p10'

	local ll = `ll' + 1
}

matrix pred_switch_pt_sd = J(10,4,0.0)
forvalues pi = 1/5{
	forvalues tt = 1/4{
		local ri = (`pi'-1)*2+1
		matrix pred_switch_pt_sd[`ri',`tt'] = pred_switch_pt[`pi',`tt']
		local ri = (`pi')*2
		matrix pred_switch_pt_sd[`ri',`tt'] = pred_switch_sd[`pi',`tt']
	}
}

matrix colnames pred_switch_pt_sd = "Mismatch" "Verbal" "Math" "Social"
matrix rownames pred_switch_pt_sd = "90_Percentile" "90_Percentile" "70_Percentile" "70_Percentile" "50_Percentile" "50_Percentile" "30_Percentile" "30_Percentile" "10_Percentile" "10_Percentile"
putexcel A1=matrix(pred_switch_pt_sd, names) using ${result}/table_${diminitls}_pred_switch.xls, replace

/* ------------------------------------------- */
/* switching probability figures */
/* ------------------------------------------- */

twoway (lpoly fswitch_occ absmm_aa , clcolor(navy) clpattern(solid) clwidth(thick)) ///
       (lpoly fswitch_occ absmm_bb, clcolor(cranberry) clpattern(solid) clwidth(thick)) ///
	(lpoly fswitch_occ absmm_cc, clcolor(dkgreen) clpattern(solid) clwidth(thick)), ///
ytitle("Probability of Switching Occupation", size(medlarge)) ///
xtitle("Last Mismatch", size(medlarge)) ///
title("Probability of Switching by Mismatch Dimension", size(large)) ///
graphregion(color(white)) xlabel(0(1)5, grid gstyle(dot)) ylabel(0.15(.03).24,grid gstyle(dot)) ///
legend(lab(1 "Verbal Mismatch") lab(2 "Math Mismatch") lab(3 "Social Mismatch") ///
ring(0) pos(11) cols(1)) ///
saving(${result}/pswitch_${diminitls}_mm_ind, replace)
graph export ${result}/pswitch_${diminitls}_mm_ind.eps, replace
 

lpoly fswitch_occ mm_pos if mm_pos>0, gen(mp_x mp_y) se(sp_y) nograph
lpoly fswitch_occ mm_neg if mm_neg<0, gen(mn_x mn_y) se(sn_y) nograph
gen chn_y = mn_y+1.645*sn_y
gen cln_y = mn_y-1.645*sn_y
gen chp_y = mp_y+1.645*sp_y
gen clp_y = mp_y-1.645*sp_y
qui sum mp_y, meanonly
local mp_max = r(max)
qui sum mn_y, meanonly
local mn_max = r(max)
/* to prevent the figure from getting a very high y axis */
qui replace chp_y = `mp_max' if chp_y>`mp_max' /* to prevent the figure from getting a very high y axis */

twoway (rarea chp_y clp_y mp_x , fcolor(midblue) fintensity(40) lwidth(none)) ///
	(rarea chn_y cln_y mn_x , fcolor(red) fintensity(40) lwidth(none)) ///
	(line mp_y mp_x, clcolor(midblue) clpattern(solid) clwidth(thick)) ///
	(line mn_y mn_x, clcolor(red) clpattern(solid) clwidth(thick)), ///
ytitle("Probability of Switching Occupation", size(medlarge)) ///
xtitle("Last Mismatch", size(medlarge)) ///
title("Probability of Switching by Positive and Negative Mismatch", size(large)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(off) yscale(range(0.15 `mp_max') noextend) ///
saving(${result}/pswitch_${diminitls}_mm_pos_neg, replace)
graph export ${result}/pswitch_${diminitls}_mm_pos_neg.eps, replace

lpoly fswitch mm, nograph gen(m_x m_y) se(s_y)
gen ch_y = m_y + s_y*1.645
gen cl_y = m_y - s_y*1.645
replace ch_y = 0.24 if ch_y > 0.24 /* prevent extending y axis up */
twoway (rarea ch_y cl_y m_x, fcolor(gs10) lwidth(none) lcolor(gs10)) ///
(line m_y m_x, clcolor(black) clpattern(solid) clwidth(thick)) , ///
ytitle("Probability of Switching Occupation", size(medlarge)) ///
xtitle("Last Mismatch", size(medlarge)) ///
title("Probability of Switching by Mismatch", size(large)) ///
graphregion(color(white)) xlabel(0(1)5, grid gstyle(dot)) ylabel(0.15(.03).24,grid gstyle(dot)) ///
yscale(range(0.15 0.24) noextend) legend(off) ///
saving(${result}/pswitch_${diminitls}_mm, replace)
graph export ${result}/pswitch_${diminitls}_mm.eps, replace


qui ivregress 2sls fswitch_occ (${prswlist_x} = ${prswlist_iv}) $prswlist_z
predict fswitch_Mprswlist
qui ivregress 2sls mm  (${prswlist_x} = ${prswlist_iv}) $prswlist_z
predict mm_Mprswlist

lpoly fswitch_Mprswlist mm_Mprswlist, nograph gen(m_M_x m_M_y) se(s_M_y)
gen ch_M_y = m_M_y + s_M_y*1.645
gen cl_M_y = m_M_y - s_M_y*1.645
*replace ch_M_y = 0.24 if ch_M_y > 0.24 /* prevent extending y axis up */
replace cl_M_y = 0.0 if cl_M_y < 0. /* prevent extending y axis down */
twoway (rarea ch_M_y cl_M_y m_M_x, fcolor(gs10) lwidth(none) lcolor(gs10)) ///
(line m_M_y m_M_x, clcolor(black) clpattern(solid) clwidth(thick)) , ///
ytitle("Probability of Switching Occupation, Residuals", size(medlarge)) ///
xtitle("Last Mismatch, Residuals", size(medlarge)) ///
title("Partial Correlations, Probability of Switching by Mismatch", size(large)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(, grid gstyle(dot)) ///
 legend(off) /// yscale(range(0.15 0.24) noextend)
saving(${result}/pswitch_${diminitls}_mm_M, replace)
graph export ${result}/pswitch_${diminitls}_mm_M.eps, replace

capture drop fswitch_Mprswlist
qui ivregress 2sls fswitch_occ mm_pos (${prswlist_x_pos_neg} = ${prswlist_iv_pos_neg}) $prswlist_z_pos_neg
predict fswitch_Mprswlist_neg
qui ivregress 2sls fswitch_occ mm_neg (${prswlist_x_pos_neg} = ${prswlist_iv_pos_neg}) $prswlist_z_pos_neg
predict fswitch_Mprswlist_pos
qui ivregress 2sls mm_neg mm_pos (${prswlist_x_pos_neg} = ${prswlist_iv_pos_neg}) $prswlist_z_pos_neg
predict mm_neg_Mprswlist
qui ivregress 2sls mm_pos mm_neg (${prswlist_x_pos_neg} = ${prswlist_iv_pos_neg}) $prswlist_z_pos_neg
predict mm_pos_Mprswlist
lpoly fswitch_Mprswlist_pos mm_pos_Mprswlist if mm_pos>0, gen(mp_M_x mp_M_y) se(sp_M_y) nograph
lpoly fswitch_Mprswlist_neg mm_neg_Mprswlist if mm_neg<0, gen(mn_M_x mn_M_y) se(sn_M_y) nograph
gen chn_M_y = mn_M_y+1.645*sn_M_y
gen cln_M_y = mn_M_y-1.645*sn_M_y
gen chp_M_y = mp_M_y+1.645*sp_M_y
gen clp_M_y = mp_M_y-1.645*sp_M_y
qui sum mp_M_y, meanonly
local mp_max = r(max)
qui sum mn_M_y, meanonly
local mn_max = r(max)
/* to prevent the figure from getting a very high y axis */
qui replace chp_M_y = `mp_max' if chp_M_y>`mp_max' /* to prevent the figure from getting a very high y axis */

twoway (rarea chp_M_y clp_M_y mp_M_x , fcolor(midblue) fintensity(40) lwidth(none)) ///
	(rarea chn_M_y cln_M_y mn_M_x , fcolor(red) fintensity(40) lwidth(none)) ///
	(line mp_M_y mp_M_x, clcolor(midblue) clpattern(solid) clwidth(thick)) ///
	(line mn_M_y mn_M_x, clcolor(red) clpattern(solid) clwidth(thick)), ///
ytitle("Probability of Switching Occupation", size(medlarge)) ///
xtitle("Last Mismatch", size(medlarge)) ///
title("Probability of Switching by Positive and Negative Mismatch", size(large)) ///
graphregion(color(white)) xlabel(, grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
legend(off) yscale(range(0.15 `mp_max') noextend) ///
saving(${result}/pswitch_${diminitls}_mm_M_pos_neg, replace)
graph export ${result}/pswitch_${diminitls}_mm_M_pos_neg.eps, replace


/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* switching direction */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

xtset id year
gen lexp = l.exp
label var lexp "Last Experience"
gen lexp2 = l.exp2
label var lexp2 "Last Experience$^2 \times$ 100"
gen lexp3 = l.exp3
label var lexp3 "Last Experience$^3 \times$ 100"
gen ltenure_occ = l.tenure_occ
label var ltenure_occ "Last Occupational Tenure"
gen lten_occ2 = l.ten_occ2 
label var lten_occ2 "Last Occupational Tenure$^2 \times$ 100"
gen lten_occ3 = l.ten_occ3
label var lten_occ3 "Last Occupational Tenure$^3 \times$ 100"
gen ltenure_emp = l.tenure_emp
label var ltenure_emp "Last Employer Tenure"
gen lten_emp2 = l.ten_emp2 
label var lten_emp2 "Last Employer Tenure$^2 \times$ 100"
gen loj = l.oj
label var loj "Last Old Job"

local nlist "aa bb cc"
local llist "Verbal Math Social"
local ll =1
foreach i of local nlist {
	local l: word `ll' of `llist'
	
	gen chng_skill_`i' = skill_`i' - l.skill_`i'
	label var chng_skill_`i' "Skill Change `l'"
	gen ability_`i'_lten = ability_`i'*l.tenure_occ
	label var ability_`i'_lten "`l' Ability $\times$  Tenure"

	global swlist $zlist ltenure_emp lten_emp2 ltenure_occ lten_occ2 lten_occ3 lexp lexp2 lexp3 loj i.occ_1d i.ind_1d
	xi: reg chng_skill_`i' lmm_??_neg lmm_??_pos $swlist if switch_occ==1 & lmm_neg <. & lmm_pos <.
	estimate save ${result}/mm_${diminitls}_where_`i'_pos_neg.ster, replace

	local ll =`ll'+1
}

gen ability_mean_lten = ability_mean*ltenure_occ
label var ability_mean_lten "Worker Ability $\times$ Occ Tenure"
egen chng_skill = rowmean(chng_skill_??)
label var chng_skill "Change in Skill"

global swlist $zlist ltenure_emp lten_emp2 ltenure_occ lten_occ2 lten_occ3 lexp lexp2 lexp3 loj i.occ_1d i.ind_1d
xi: reg chng_skill lmm_pos lmm_neg $swlist if switch_occ==1
estimate save ${result}/mm_${diminitls}_where_pos_neg.ster, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* creating tables and figures */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* swithching direction regression table */

estimate clear
estimate use ${result}/mm_${diminitls}_where_pos_neg.ster
estimate store mm_${diminitls}_where_pos_neg
estimate use ${result}/mm_${diminitls}_where_aa_pos_neg.ster
estimate store mm_${diminitls}_where_aa_pos_neg
estimate use ${result}/mm_${diminitls}_where_bb_pos_neg.ster
estimate store mm_${diminitls}_where_bb_pos_neg
estimate use ${result}/mm_${diminitls}_where_cc_pos_neg.ster
estimate store mm_${diminitls}_where_cc_pos_neg

/* tex */	   
esttab mm_${diminitls}_where_aa_pos_neg mm_${diminitls}_where_bb_pos_neg mm_${diminitls}_where_cc_pos_neg mm_${diminitls}_where_pos_neg ///
                   using ${result}/table_${diminitls}_where.tex, b(4) ///
                   r2 nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "Robust standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   mtitles("Verbal" "Math" "Social" "All Average") ///
		   order(lmm_aa_pos lmm_aa_neg lmm_bb_pos lmm_bb_neg lmm_cc_pos lmm_cc_neg lmm_pos lmm_neg) drop(lten* lexp* loj $zlist _cons _I*) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace

esttab mm_${diminitls}_where_aa_pos_neg mm_${diminitls}_where_bb_pos_neg mm_${diminitls}_where_cc_pos_neg mm_${diminitls}_where_pos_neg ///
                   using ${result}/table_apx_${diminitls}_where.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\hline  " ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   mtitles("Verbal" "Math" "Social" "All Average" ) ///
		   title("Regressions for Direction of Occupational Switch (Full Results)") ///
		   order(lmm_aa_pos lmm_aa_neg lmm_bb_pos lmm_bb_neg lmm_cc_pos lmm_cc_neg lmm_pos lmm_neg lten* lexp* loj $zlist) drop(_I*) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
/*------------------------------------------------------------------------------------*/
/* summary statitics of changes in skills */

local nlist "aa bb cc"
matrix mat_chng_skill = J(4,6,0.0)

foreach i of local nlist{
gen index_pos_`i' = (chng_skill_`i' > 0) if switch_occ == 1
}

/* all */
local ll = 1
foreach i of local nlist{
	qui su index_pos_`i'  if switch_occ == 1, meanonly
	matrix mat_chng_skill[1,`ll'] = r(mean)
        qui su chng_skill_`i' if switch_occ == 1, meanonly
	matrix mat_chng_skill[1,`ll'+3] = r(mean)*100
	local ll = `ll' + 1
}

/* less than high school */
local ll = 1
foreach i of local nlist{
	qui su index_pos_`i'  if switch_occ == 1 & grade < 12, meanonly
	matrix mat_chng_skill[2,`ll'] = r(mean)
        qui su chng_skill_`i' if switch_occ == 1 & grade < 12, meanonly
	matrix mat_chng_skill[2,`ll'+3] = r(mean)*100
	local ll = `ll' + 1
}

/* high school */
local ll = 1
foreach i of local nlist{
	qui su index_pos_`i'  if switch_occ == 1 & grade == 12, meanonly
	matrix mat_chng_skill[3,`ll'] = r(mean)
        qui su chng_skill_`i' if switch_occ == 1 & grade == 12, meanonly
	matrix mat_chng_skill[3,`ll'+3] = r(mean)*100
	local ll = `ll' + 1
}

/* some college */
local ll = 1
foreach i of local nlist{
	qui su index_pos_`i'  if switch_occ == 1 & grade > 12, meanonly
	matrix mat_chng_skill[4,`ll'] = r(mean)
        qui su chng_skill_`i' if switch_occ == 1 & grade > 12, meanonly
	matrix mat_chng_skill[4,`ll'+3] = r(mean)*100
	local ll = `ll' + 1
}

matrix colnames mat_chng_skill = "Frac Pos Verbal" "Frac Pos Math" "Frac Pos Social" "Change in Verbal" "Change in Math" "Change in Social"
matrix rownames mat_chng_skill = "All" "Less_than_High_School" "High_School" "Some_College"
putexcel A1=matrix(mat_chng_skill, names) using ${result}/table_${diminitls}_chng_skill.xls, replace

/*------------------------------------------------------------------------------------*/
/* predicted direction of switch */

estimate clear
estimate use ${result}/mm_${diminitls}_where_pos_neg.ster
matrix pred_where_pt = J(6,3,0.0)
matrix pred_where_sd = J(6,3,0.0)
matrix pred_where_pt_sd = J(12,3,0.0)

/* loop over skills */
local ll = 1
foreach i of local nlist{
	estimate clear
	estimate use ${result}/mm_${diminitls}_where_`i'_pos_neg.ster

	_pctile lmm_`i'_pos if switch_occ == 1 & lmm_`i'_pos != 0, p(10 50 90)
	local p90 = r(r3)
	local p50 = r(r2)
	local p10 = r(r1)

	matrix pred_where_pt[1,`ll'] = _b["lmm_`i'_pos"]*`p90'*100
	matrix pred_where_pt[2,`ll'] = _b["lmm_`i'_pos"]*`p50'*100
	matrix pred_where_pt[3,`ll'] = _b["lmm_`i'_pos"]*`p10'*100
	matrix pred_where_sd[1,`ll'] = _se["lmm_`i'_pos"]*`p90'*100
	matrix pred_where_sd[2,`ll'] = _se["lmm_`i'_pos"]*`p50'*100
	matrix pred_where_sd[3,`ll'] = _se["lmm_`i'_pos"]*`p10'*100

	_pctile lmm_`i'_neg if switch_occ == 1 & lmm_`i'_neg != 0, p(10 50 90)
	local p90 = r(r3)
	local p50 = r(r2)
	local p10 = r(r1)

	matrix pred_where_pt[4,`ll'] = _b["lmm_`i'_neg"]*`p90'*100
	matrix pred_where_pt[5,`ll'] = _b["lmm_`i'_neg"]*`p50'*100
	matrix pred_where_pt[6,`ll'] = _b["lmm_`i'_neg"]*`p10'*100
	matrix pred_where_sd[4,`ll'] = _se["lmm_`i'_neg"]*`p90'*100
	matrix pred_where_sd[5,`ll'] = _se["lmm_`i'_neg"]*`p50'*100
	matrix pred_where_sd[6,`ll'] = _se["lmm_`i'_neg"]*`p10'*100
	
	local ll = `ll' + 1
}

forvalues pi = 1/6{
	forvalues tt = 1/3{
		local ri = (`pi'-1)*2+1
		matrix pred_where_pt_sd[`ri',`tt'] = pred_where_pt[`pi',`tt']
		local ri = (`pi')*2
		matrix pred_where_pt_sd[`ri',`tt'] = pred_where_sd[`pi',`tt']
	}
}

matrix colnames pred_where_pt_sd = "Verbal" "Math" "Social"
matrix rownames pred_where_pt_sd = "90_Pos" "90_Pos" "50_Pos" "50_Pos" "10_Pos" "10_Pos" "90_Neg" "90_Neg" "50_Neg" "50_Neg" "10_Neg" "10_Neg"
putexcel A1=matrix(pred_where_pt_sd, names) using ${result}/table_${diminitls}_pred_where.xls, replace

/*------------------------------------------------------------------------------------*/
/* figure of the change in the direction of skills */

graph twoway (scatter chng_skill lmm_pos if switch_occ==1 & lmm_pos > 0, msymbol(x) mlcolor(eltblue) mlwidth(vvthin)) ///
             (scatter chng_skill lmm_neg if switch_occ==1 & lmm_neg < 0, msymbol(x) mlcolor(eltblue) mlwidth(vvthin)) ///
             (lpolyci chng_skill lmm_pos if switch_occ==1 & lmm_pos > 0, clcolor(blue) clpattern(solid) clwidth(thick) fcolor(gs11)) ///
             (lpolyci chng_skill lmm_neg if switch_occ==1 & lmm_neg < 0, clcolor(blue) clpattern(solid) clwidth(thick) fcolor(gs11)), ///
ytitle("Average Change in Skill", size(large) color(dknavy)) ///
xtitle("Last Positive or Negative Mismatch", size(large) color(dknavy)) ///
title("(d) Direction of Switch, All Average", size(large)) ///
legend(off) ///
graphregion(color(white)) xlabel(-4(2)4,grid gstyle(default)) ylabel(-1.0(0.5)1.0, grid gstyle(default)) ///
xline(0, lcolor(navy) lpattern(solid) lwidth(medium))
graph export ${result}/fig_where_${diminitls}_scat_mm.eps, replace


lpoly chng_skill lmm_pos if switch_occ==1 & lmm_pos > 0, nograph gen(lmp_i csp_i)
lpoly chng_skill lmm_neg if switch_occ==1 & lmm_neg < 0, nograph gen(lmn_i csn_i)

sum lmm_neg, meanonly
local lmm_min = r(min)
sum lmm_pos, meanonly
local lmm_max = r(max)
graph twoway 	(line csp_i lmp_i, clcolor(blue) clpattern(solid) clwidth(thick) fcolor(gs11)) ///
		(line csn_i lmn_i, clcolor(red) clpattern(solid) clwidth(thick) fcolor(gs11)) ///
		(lfit csp_i lmp_i , range(`lmm_min' `lmm_max') clwidth(medthick) clcolor(eltblue) clpattern(dash)) ///
		(lfit csn_i lmn_i, range(`lmm_min' `lmm_max') clwidth(medthick) clcolor(erose) clpattern(dash) ) ///
	     , ///
ytitle("Average Change in Skill", size(large) color(dknavy)) ///
xtitle("Last Positive or Negative Mismatch", size(large) color(dknavy)) ///
title("(d) Direction of Switch, All Average", size(large)) ///
legend(off) ///
graphregion(color(white)) xlabel(-4(2)4,grid gstyle(default)) ylabel(-1.0(0.5)1.0, grid gstyle(default)) ///
xline(0. , lcolor(navy) lpattern(solid) lwidth(medium))
graph export ${result}/fig_where_${diminitls}_mm.eps, replace
drop lmp_ csp_i lmn_i csn_i

local llist "Verbal Math Social"
local nlist "aa bb cc"
local cllist_pos "eltblue eltblue eltblue"
local cllist_neg "erose erose erose"
local tllist "(a) (b) (c)"
local ll =1
local cll = 1
local tll = 1
foreach i of local nlist {
	qui sum lmm_`i'_neg, meanonly
	local lmm_min = r(min)
	qui sum lmm_`i'_pos, meanonly
	local lmm_max = r(max)
	
	lpoly chng_skill_`i' lmm_`i'_pos if switch_occ==1 & lmm_`i'_pos > 0, gen(lmp_i csp_i) nograph
	lpoly chng_skill_`i' lmm_`i'_neg if switch_occ==1 & lmm_`i'_neg < 0, gen(lmn_i csn_i) nograph
	
	local l: word `ll' of `llist'
	local cp: word `cll' of `cllist_pos'
	local cn: word `cll' of `cllist_neg'
	local t: word `tll' of `tllist'
	graph twoway 	(line csp_i lmp_i , clcolor(blue) clpattern(solid) clwidth(thick) fcolor(gs11)) ///
			(line csn_i lmn_i , clcolor(red) clpattern(solid) clwidth(thick) fcolor(gs11)) ///
			(lfit csp_i lmp_i , range(`lmm_min' `lmm_max') clcolor("`cp'") clwidth(medthick) clpattern(dash)) ///
			(lfit csn_i lmn_i , range(`lmm_min' `lmm_max') clcolor("`cn'") clwidth(medthick) clpattern(dash) ) ///
	             , ///
	ytitle("Change in Skill, `l'", size(large) color(dknavy)) ///
	xtitle("Last Positive or Negative Mismatch in `l'", size(large) color(dknavy)) ///
	title("`t' Direction of  Switch, `l'", size(large)) ///
	legend(off) ///
	graphregion(color(white)) xlabel(-4(2)4,grid gstyle(default)) ylabel(-1.0(0.5)1.0, grid gstyle(default)) ///
	xline(0, lcolor(navy) lpattern(solid) lwidth(medium))
	graph export ${result}/fig_where_${diminitls}_mm_`i'.eps, replace
	drop csp_i lmp_i csn_i lmn_i
	
	graph twoway (scatter chng_skill_`i' lmm_`i'_pos if switch_occ==1 & lmm_`i'_pos > 0, msymbol(x) mlcolor("`cp'") mlwidth(vvthin)) ///
                     (scatter chng_skill_`i' lmm_`i'_neg if switch_occ==1 & lmm_`i'_neg < 0, msymbol(x) mlcolor("`cp'") mlwidth(vvthin)) ///
	             (lpolyci chng_skill_`i' lmm_`i'_pos if switch_occ==1 & lmm_`i'_pos > 0, clcolor(blue) clpattern(solid) clwidth(thick) fcolor(gs11)) ///
                     (lpolyci chng_skill_`i' lmm_`i'_neg if switch_occ==1 & lmm_`i'_neg < 0, clcolor(blue) clpattern(solid) clwidth(thick) fcolor(gs11)), ///
	ytitle("Change in Skill, `l'", size(large) color(dknavy)) ///
	xtitle("Last Positive or Negative Mismatch in `l'", size(large) color(dknavy)) ///
	title("`t' Direction of  Switch, `l'", size(large)) ///
	legend(off) ///
	graphregion(color(white)) xlabel(-4(2)4,grid gstyle(default)) ylabel(-1.0(0.5)1.0, grid gstyle(default)) ///
	xline(0, lcolor(navy) lpattern(solid) lwidth(medium))
	graph export ${result}/fig_where_${diminitls}_mm_scat_`i'.eps, replace
		
	local ll = `ll'+1
	local cll = `cll'+ 1
	local tll = `tll'+ 1
}

/*--------------------------------------------------------------------------------------
* name: mismatch_04_regression.do
* made by: david wiczer, moidfied by: satoshi tanaka
* date: 08/21/2010
*       04/03/2015
* description: this code is for the project 'occupation skill mismatch'
--------------------------------------------------------------------------------------*/

global diminitls "vms"
global varcor    "rbst" /* may be cluster,id (cid) or rbst*/
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
use ${data}/yearly_02.dta, clear

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

use ${result}/yearly_03.dta, clear

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

/*-!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!-*/		   
/* for referees */
forvalues i = 1/7{
	cumul asvab_std`i' if ind_indic == 1, gen(ability_rnk_tmp) equal
	bysort id: egen asvab_std_rnk_tmp = max(ability_rnk_tmp)
	cumul ONET_ASVAB_`i', gen(ONET_ASVAB_rnk_tmp) equal
	gen mm_d_`i' = abs(asvab_std_rnk_tmp-ONET_ASVAB_rnk_tmp)
	drop asvab_std_rnk_tmp ability_rnk_tmp ONET_ASVAB_rnk_tmp
}
forvalues i=1/2{
	cumul social_std`i' if ind_indic == 1, gen(ability_rnk_tmp) equal
	bysort id: egen asvab_std_rnk_tmp = max(ability_rnk_tmp)
	local j = `i'+7
	gen mm_d_`j' = abs(asvab_std_rnk_tmp - skill_s)
	drop asvab_std_rnk_tmp ability_rnk_tmp
}

gen mm_dim = 0
pca mm_d_*
estat loadings, cnorm(unit)
matrix L = e(L)
local Lwt= 0
forvalues i=1/9{
	local Lhere = abs(L[`i',1])
	local Lwt = `Lwt' +  `Lhere'
	replace mm_dim = mm_dim + mm_d_`i'*`Lhere'
}
replace mm_dim = mm_dim/`Lwt'

/*------------------------------------------------------------------------------------*/

/* normalize aggregate dimensions */

qui{	
        sum mm
	replace mm = (mm-r(min))/r(sd)
	sum mm_2nm
	replace mm_2nm = (mm_2nm-r(min))/r(sd)
	sum mm_dim
	replace mm_dim = (mm_dim-r(min))/r(sd)
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
gen mm_dim_ten_occ = mm_dim*tenure_occ
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
gen mm_dim_ten_occ_iv = mm_dim*ten_occ_iv
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
pca ability_aa ability_bb ability_cc, components(1)
predict ability_pca
cumul ability_pca, gen(ability_pca_mean) equal
gen ability_mean_ten_occ = ability_mean*tenure_occ
gen ability_mean_ten_occ2 = ability_mean*ten_occ2
gen ability_mean_ten_occ_iv = ability_mean*ten_occ_iv
gen ability_mean_ten_occ2_iv = ability_mean*ten_occ2_iv

gen skill_lev  = (skill_aa + skill_bb + skill_cc)/3
cumul skill_lev, gen(skill_mean) equal
pca skill_aa skill_bb skill_cc , components(1)
predict skill_pca
cumul skill_pca, gen(skill_pca_mean)  equal
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
label var mm_dim "All-Dimension Mismatch"
label var mm_neg "Negative Mismatch"
label var mm_pos "Positive Mismatch"
label var mm_ten_occ "Mismatch $\times$ Occ Tenure"
label var mm_neg_ten_occ "Neg. Mismatch $\times$ Occ Tenure"
label var mm_pos_ten_occ "Pos. Mismatch $\times$ Occ Tenure"
label var mm_2nm_ten_occ "Quad. Mismatch $\times$ Occ Tenure"
label var mm_dim_ten_occ "All-Dim Mismatch $\times$ Occ Tenure"

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
gen lmm_dim = mm_dim[_n-1] if switch_occ == 1
replace lmm_dim = lmm_dim[_n-1] if switch_occ == 0 & id == id[_n-1]

/* last requirement */
gen lskill = skill_pca_mean[_n-1] if switch_occ==1
replace lskill = lskill[_n-1] if switch_occ == 0 & id == id[_n-1]

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
label var lmm_dim "Last All-dim. Mismatch"


gen delmm_rnk = mm_rnk - lmm_rnk
gen delmm = mm-lmm

gen lten_occ = tenure_occ[_n-1] if switch_occ == 1
replace lten_occ = lten_occ[_n-1] if switch_occ == 0 & id == id[_n-1]

/* create cumulative mismatch */
gsort +id -switch_occ +year

by id: gen cmm = sum(lmm*lten_occ)  if switch_occ==1  & lmm<. & lten_occ<.
by id: gen cmm_2nm = sum(lmm_2nm*lten_occ)  if switch_occ==1  & lmm_2nm<. & lten_occ<.
by id: gen cmm_dim = sum(lmm_dim*lten_occ)  if switch_occ==1  & lmm_dim<. & lten_occ<.
by id: gen cmm_neg = sum(lmm_neg*lten_occ)  if switch_occ==1 
by id: gen cmm_pos = sum(lmm_pos*lten_occ)  if switch_occ==1 
by id: gen cskill= sum(lskill*lten_occ) if switch_occ==1 & lskill<. & lten_occ<.
by id: gen cmm_1d = sum(lmm_1d*lten_occ)  if switch_occ==1  & lmm_1d<. & lten_occ<.


by id: gen totexp = sum(lten_occ) if switch_occ==1
replace cmm = cmm/totexp if switch_occ==1
replace cmm_2nm = cmm_2nm/totexp if switch_occ==1
replace cmm_1d  = cmm_1d /totexp if switch_occ==1
replace cmm_dim = cmm_dim/totexp if switch_occ==1
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
replace cmm_dim = cmm_dim[_n-1] if switch_occ == 0 & id == id[_n-1]
replace cmm_1d  = cmm_1d[_n-1]  if switch_occ == 0 & id == id[_n-1]
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
	sum cmm_dim
	replace cmm_dim = (cmm_dim-r(min))/r(sd)	
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
label var cmm_dim "Cumul All-Dim Mismatch"
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

save ${data}/yearly_03.dta, replace


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
reg uhat l.uhat, noc /*, fe  */
global rhohat = _b["L.uhat"]
drop uhat
qui forvalues iter=1/50{
	qui foreach zv of varlist mm cmm $zlist ability_mean skill_mean $xlist $ivlist lwage{
		gen `zv'_R =`zv'
		replace `zv'= `zv'_R  - ${rhohat}*l.`zv'_R 
		_crcslbl `zv'_R `zv'
	}
	xi: ivregress 2sls lwage mm cmm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d /*[aw=inv_omega] */
	estimate save ${result}/iv_cmm_mm_fgls.ster, replace
	predict uhat, residuals
	reg uhat l.uhat, noc /*, fe  */
	if( abs( _b["L.uhat"] - ${rhohat})<0.01 ){
		qui foreach zv of varlist mm cmm $zlist ability_mean skill_mean $xlist $ivlist lwage{
			replace `zv'= `zv'_R
		}
		drop *_R uhat
		continue, break
	}
	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist mm cmm $zlist ability_mean skill_mean $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}



*with robust errors for baseline
xi: ivregress 2sls lwage mm cmm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_cmm_mm_rbst.ster, replace

*with clustered standard errors for comparison
xi: ivregress 2sls lwage mm cmm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(cluster id)
estimate save ${result}/iv_cmm_mm_clu.ster, replace


*with HAC AR(1) standard errors for comparison
xi: ivreg2 lwage mm cmm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, bw(2) robust
estimate save ${result}/iv_cmm_mm_hac.ster, replace


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
reg uhat l.uhat , noc
global rhohat = _b["L.uhat"]
drop uhat
qui forvalues iter=1/50{
	qui foreach zv of varlist mm cmm  skill_mean  $xlist $ivlist lwage{
		gen `zv'_R =`zv'
		replace `zv'= `zv'_R  - ${rhohat}*l.`zv'_R 
		_crcslbl `zv'_R `zv'
	}
	xtivreg2 lwage mm cmm  skill_mean _Iind_1d_* _Iocc_1d_* ($xlist = $ivlist), fe /*[aw=inv_omega] */
	estimate save ${result}/iv_cmm_mm_fe_fgls.ster, replace
	predict uhat, e
	reg uhat l.uhat, noc 
	if( abs( _b["L.uhat"] - ${rhohat})<0.01 ){
		qui foreach zv of varlist mm cmm  skill_mean  $xlist $ivlist lwage{
			replace `zv'= `zv'_R
		}
		drop *_R uhat
		continue, break
	}
	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist mm cmm  skill_mean  $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}

*with robust errors for comparison
xtivreg2 lwage mm cmm  skill_mean _Iind_1d_* _Iocc_1d_* ($xlist = $ivlist) , robust fe
estimate save ${result}/iv_cmm_mm_fe_rbst.ster, replace


*with cluster errors for comparison
xtivreg2 lwage mm cmm  skill_mean _Iind_1d_* _Iocc_1d_* ($xlist = $ivlist) , cluster(id) fe
estimate save ${result}/iv_cmm_mm_fe_clu.ster, replace


*with HAC errors for baseline
xtivreg2 lwage mm cmm  skill_mean _Iind_1d_* _Iocc_1d_* ($xlist = $ivlist) , bw(2) robust fe
estimate save ${result}/iv_cmm_mm_fe_hac.ster, replace



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
reg uhat l.uhat , noc
global rhohat = _b["L.uhat"]
drop uhat
qui forvalues iter=1/50{
	qui foreach zv of varlist mm_pos mm_neg cmm_pos cmm_neg $zlist $xlist $ivlist lwage{
		gen `zv'_R =`zv'
		replace `zv'= `zv'_R  - ${rhohat}*l.`zv'_R 
		_crcslbl `zv'_R `zv'
	}
	xi: ivregress 2sls lwage mm_pos mm_neg cmm_pos cmm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d /*[aw=inv_omega] */
	estimate save ${result}/iv_cmm_mm_pos_neg_fgls.ster, replace
	predict uhat, residuals
	reg uhat l.uhat , noc
	if( abs( _b["L.uhat"] - ${rhohat})<0.01 ){
		qui foreach zv of varlist mm_pos mm_neg cmm_pos cmm_neg $zlist $xlist $ivlist lwage{
			replace `zv'= `zv'_R
		}
		drop *_R uhat
		continue, break
	}
	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist mm_pos mm_neg cmm_pos cmm_neg $zlist $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}


*with robust errors for baseline
xi: ivregress 2sls lwage mm_pos mm_neg cmm_pos cmm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_cmm_mm_pos_neg_rbst.ster, replace


xi: ivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, bw(2) robust
estimate save ${result}/iv_cmm_mm_pos_neg_hac.ster, replace

xi: ivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, cluster(id)
estimate save ${result}/iv_cmm_mm_pos_neg_clu.ster, replace



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
reg uhat l.uhat , noc
global rhohat = _b["L.uhat"]
drop uhat
qui forvalues iter=1/50{
	qui foreach zv of varlist mm_pos mm_neg cmm_pos cmm_neg $xlist $ivlist lwage{		
		gen `zv'_R =`zv'
		replace `zv'= `zv'_R  - ${rhohat}*l.`zv'_R 
		_crcslbl `zv'_R `zv'
	}
	xtivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg _Iind_1d* _Iocc_1d* ($xlist = $ivlist) , fe /*[aw=inv_omega] */
	estimate save ${result}/iv_cmm_mm_pos_neg_fe_fgls.ster, replace
	predict uhat, e
	reg uhat l.uhat , noc
	if( abs( _b["L.uhat"] - ${rhohat})<0.01 ){
		qui foreach zv of varlist mm_pos mm_neg cmm_pos cmm_neg $xlist $ivlist lwage{
			replace `zv'= `zv'_R
		}
		drop *_R uhat
		continue, break
	}
	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist mm_pos mm_neg cmm_pos cmm_neg $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}




*with hac errors for baseline
xtivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg _Iind_1d* _Iocc_1d* ($xlist = $ivlist), bw(2) robust fe
estimate save ${result}/iv_cmm_mm_pos_neg_fe_rbst.ster, replace


*with robust errors for comparison
xtivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg _Iind_1d* _Iocc_1d* ($xlist = $ivlist), robust fe
estimate save ${result}/iv_cmm_mm_pos_neg_fe_rbst.ster, replace

*with cluster errors for comparison
xtivreg2 lwage mm_pos mm_neg cmm_pos cmm_neg _Iind_1d* _Iocc_1d* ($xlist = $ivlist), cluster(id) fe
estimate save ${result}/iv_cmm_mm_pos_neg_fe_clu.ster, replace

/*------------------------------------------------------------------------------------*/
/* individual component cumulative mismatch */

*FGLS attempt
xi: ivregress 2sls lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d
predict uhat, residuals
reg uhat l.uhat , noc
global rhohat = _b["L.uhat"]
drop uhat
qui forvalues iter=1/50{
	qui foreach zv of varlist cmm_aa cmm_bb cmm_cc absmm_?? $zlist ability_?? skill_??  $xlist $ivlist lwage{
		gen `zv'_R =`zv'
		replace `zv'= `zv'_R  - ${rhohat}*l.`zv'_R 
		_crcslbl `zv'_R `zv'
	}
	xi: ivregress 2sls lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d 
	estimate save ${result}/iv_ind_cmm_mm_fgls.ster, replace
	predict uhat, residuals
	reg uhat l.uhat , noc
	if( abs( _b["L.uhat"] - ${rhohat})<0.01 ){
		qui foreach zv of varlist cmm_aa cmm_bb cmm_cc absmm_?? $zlist ability_?? skill_??  $xlist $ivlist lwage{
			replace `zv'= `zv'_R
		}
		drop *_R uhat
		continue, break
	}
	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist cmm_aa cmm_bb cmm_cc absmm_?? $zlist ability_?? skill_??  $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}


*with robust errors for comparison
xi: ivregress 2sls lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmm_mm_rbst.ster, replace

xi: ivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d, bw(2) robust
estimate save ${result}/iv_ind_cmm_mm_hac.ster, replace


xi: ivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d, cluster(id)
estimate save ${result}/iv_ind_cmm_mm_clu.ster, replace



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
reg uhat l.uhat , noc
global rhohat = _b["L.uhat"]
drop uhat
qui forvalues iter=1/15{
	qui foreach zv of varlist cmm_aa cmm_bb cmm_cc absmm_?? skill_?? $xlist $ivlist lwage{
		gen `zv'_R =`zv'
		replace `zv' = `zv'_R  - ${rhohat}*l.`zv'_R 
		_crcslbl `zv'_R `zv'
	}
	xtivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist)  skill_?? _Iind_1d* _Iocc_1d*, fe /*[aw = inv_omega]*/
	estimate save ${result}/iv_ind_cmm_mm_fe_fgls.ster, replace
	predict uhat, e
	reg uhat l.uhat , noc
	if( abs( _b["L.uhat"] - ${rhohat})<0.01 ){
		qui foreach zv of varlist cmm_aa cmm_bb cmm_cc absmm_?? skill_?? $xlist $ivlist lwage{
			replace `zv'= `zv'_R
		}
		drop *_R uhat
		continue, break
	}

	global rhohat = _b["L.uhat"]*0.1 + 0.9*${rhohat}
	
	qui foreach zv of varlist cmm_aa cmm_bb cmm_cc absmm_?? skill_?? $xlist $ivlist lwage{
		replace `zv'= `zv'_R
	}
	drop *_R uhat
}


*with HAC errors for baseline
xtivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist)  skill_?? _Iind_1d* _Iocc_1d*, bw(2) robust fe
estimate save ${result}/iv_ind_cmm_mm_fe_hac.ster, replace

*with robust errors for comparison
xtivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist)  skill_?? _Iind_1d* _Iocc_1d*, robust fe
estimate save ${result}/iv_ind_cmm_mm_fe_rbst.ster, replace

*with cluster errors for comparison
xtivreg2 lwage cmm_aa cmm_bb cmm_cc absmm_?? ($xlist = $ivlist)  skill_?? _Iind_1d* _Iocc_1d*, cluster(id) fe
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


estimate use ${result}/iv_cmm_mm_clu.ster
estimate store iv_cmm_mm_clu
estimate use ${result}/iv_cmm_mm_fe_clu.ster
estimate store iv_cmm_mm_fe_clu

estimate use ${result}/iv_cmm_mm_rbst.ster
estimate store iv_cmm_mm_rbst
estimate use ${result}/iv_cmm_mm_fe_rbst.ster
estimate store iv_cmm_mm_fe_rbst

estimate use ${result}/iv_cmm_mm_fe_hac.ster
estimate store iv_cmm_mm_fe_hac
estimate use ${result}/iv_cmm_mm_hac.ster
estimate store iv_cmm_mm_hac
estimate use ${result}/iv_cmm_mm_fe_fgls.ster
estimate store iv_cmm_mm_fe_fgls
estimate use ${result}/iv_cmm_mm_fgls.ster
estimate store iv_cmm_mm_fgls


esttab iv_cmm_mm_hac iv_cmm_mm_clu iv_cmm_mm_fgls iv_cmm_mm_fe_hac iv_cmm_mm_fe_clu ///
                   using ${result}/table_${diminitls}_${varcor}_${varmeth}_r12compare.tex, b(4) ///
                   r2 nodepvars gaps label not nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "${labtxt} standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(*occ_1* *ind_1* ten* exp* oj $zlist _cons) ///
		   mtitles("IV-HAC" "IV-CLU" "IV-FGLS" "IV-FE-HAC" "IV-FE-CLU") ///
		   order(mm mm_ten_occ cmm ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ ten* exp* oj $zlist _cons) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace

*Table in response to referees
matrix coef_se_compare = J(12,2,0.)
matrix colnames coef_se_compare = "Coefficient" "Std Err"
matrix rownames coef_se_compare = "Mismatch" "Mismatch \times Occ Tenure" "Cumul Mismatch" "Mismatch" "Mismatch \times Occ Tenure" "Cumul Mismatch" "Mismatch" "Mismatch \times Occ Tenure" "Cumul Mismatch" "Mismatch" "Mismatch \times Occ Tenure" "Cumul Mismatch" 

forvalues ei = 1/4{
	if(`ei' == 1){
		estimates restore iv_cmm_mm_rbst
	}
	if(`ei' == 2){
		estimates restore iv_cmm_mm_clu
	}
	if(`ei' == 3){
		estimates restore iv_cmm_mm_fgls
	}
	if(`ei' == 4){
		estimates restore iv_cmm_mm_hac
	}

	matrix coef_se_compare[1+(`ei'-1)*3,1] = _b[mm]
	matrix coef_se_compare[1+(`ei'-1)*3,2] = _se[mm]
	matrix coef_se_compare[2+(`ei'-1)*3,1] = _b[mm_ten_occ]
	matrix coef_se_compare[2+(`ei'-1)*3,2] = _se[mm_ten_occ]
	matrix coef_se_compare[3+(`ei'-1)*3,1] = _b[cmm]
	matrix coef_se_compare[3+(`ei'-1)*3,2] = _se[cmm]
}

putexcel A1=matrix(coef_se_compare, names) using ${result}/table_${diminitls}_ref_1_2.xls, replace

/*------------------------------------------------------------------------------------*/
/* positive & negative mismatch */

estimate clear

estimate use ${result}/iv_cmm_mm_pos_neg.ster
estimate store iv_cmm_mm_pos_neg
estimate use ${result}/iv_cmm_mm_pos_neg_fe.ster
estimate store ols_cmm_mm_pos_neg_fe
estimate use ${result}/iv_cmm_mm_pos_neg_fe.ster
estimate store iv_cmm_mm_pos_neg_fe

estimate use ${result}/iv_cmm_mm_pos_neg_rbst.ster
estimate store iv_cmm_mm_pos_neg_rbst
estimate use ${result}/iv_cmm_mm_pos_neg_fe_rbst.ster
estimate store iv_cmm_mm_pos_neg_fe_rbst
estimate use ${result}/iv_cmm_mm_pos_neg_fgls.ster
estimate store iv_cmm_mm_pos_neg_fgls
estimate use ${result}/iv_cmm_mm_pos_neg_fe_fgls.ster
estimate store iv_cmm_mm_pos_neg_fe_fgls

esttab iv_cmm_mm_pos_neg_rbst iv_cmm_mm_pos_neg_fe_rbst iv_cmm_mm_pos_neg iv_cmm_mm_pos_neg_fe iv_cmm_mm_pos_neg_fgls iv_cmm_mm_pos_neg_fe_fgls ///
                   using ${result}/table_${diminitls}_${varcor}_${varmeth}_pos_neg_fgls.tex, b(4) ///
                   r2 nodepvars gaps label not nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "${labtxt} standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(*occ_1* *ind_1* ten* exp* oj $zlist _cons) ///
		   mtitles("IV-HAC" "IV-FE-HAV" "IV-CLU" "IV-FE-CLU" "IV-FGLS" "IV-FE-FGLS") ///
		   order(mm_??? mm_???_ten_occ cmm_??? ) ///
                   star(\sym{*} 0.10 \sym{**} 0.05 \sym{***} 0.01) replace

  
/*------------------------------------------------------------------------------------*/
/* individual component mismatch */

estimate clear


estimate use ${result}/iv_ind_cmm_mm_clu.ster
estimate store iv_ind_cmm_mm_clu
estimate use ${result}/iv_ind_cmm_mm_fe_clu.ster
estimate store iv_ind_cmm_mm_fe_clu
estimate use ${result}/iv_ind_cmm_mm_hac.ster
estimate store iv_ind_cmm_mm_hac
estimate use ${result}/iv_ind_cmm_mm_fe_hac.ster
estimate store iv_ind_cmm_mm_fe_hac
estimate use ${result}/iv_ind_cmm_mm_fgls.ster
estimate store iv_ind_cmm_mm_fgls
estimate use ${result}/iv_ind_cmm_mm_fe_fgls.ster
estimate store iv_ind_cmm_mm_fe_fgls
estimate use ${result}/iv_ind_cmm_mm_rbst.ster
estimate store iv_ind_cmm_mm_rbst
estimate use ${result}/iv_ind_cmm_mm_fe_rbst.ster
estimate store iv_ind_cmm_mm_fe_rbst
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
		   


/*--------------------------------------------------------------------------------------
* name: mismatch_11_xdimphysical.do
* made by: david wiczer, moidfied by: satoshi tanaka
* date: 08/21/2010
*       03/25/2015
* description: this code is for the project 'occupation skill mismatch'
--------------------------------------------------------------------------------------*/

/* this code runs the regressions */ 

use ${result}/yearly_02.dta, clear
set more off

/*------------------------------------------------------------------------------------*/

global diminitls "vmp"

/*------------------------------------------------------------------------------------*/

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

bysort id: egen grade_m = max(grade) if age<=30
bysort id: egen grade_30 = max(grade_m)
drop grade_m

/*------------------------------------------------------------------------------------*/

/* create weights */
sort id
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

/* calculate the difference in wages */
xtset id year
gen delwage = lwage - l.lwage

/* defining residual wages */
gen reswage = .
sum year, meanonly
local y0=r(min)
local yY=r(max)
qui forvalues yi=`y0'/`yY'{
	reg lwage lths univ exp exp2 black hispanic oj i.ind_1d i.occ_1d if year == `yi'
	predict reswage_t, residual 
	replace reswage = reswage_t if year == `yi'
	drop reswage_t
}

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

/* make the standard deviation the same */
qui forvalues i=1/7{
	sum asvab_sec`i' if tmp2== 20
	local sd20 = r(sd)
	forvalues ai=16/24{
		sum asvab_sec`i' if tmp2 ==`ai'
		replace asvab_sec`i' = asvab_sec`i'/r(sd)*`sd20' if tmp2 ==`ai'
	}
}

/* take out the mean */
qui forvalues i=1/7{
	reg asvab_sec`i' I_enterage1-I_enterage8
	predict asvab_res`i', residual 
	replace asvab_res`i' = asvab_res`i' +_b["_cons"]
}
qui reg AFQT I_enterage1-I_enterage8
qui predict AFQT_res, residual 
qui replace AFQT_res = AFQT_res +_b["_cons"]

/* normalize everything to have standard deviation of 1 */
qui forvalues i=1/7{
	sum asvab_res`i'
	gen asvab_std`i' = asvab_res`i'/r(sd)
}
qui sum AFQT_res
qui gen AFQT_std = AFQT_res/r(sd)
label var AFQT_std "AFQT"

/*------------------------------------------------------------------------------------*/

/* normalize everything to have standard deviation of 1 */
forvalues s = 1/7 {
	qui sum ONET_ASVAB_`s'
	replace ONET_ASVAB_`s' = ONET_ASVAB_`s'/r(sd)
}

/*------------------------------------------------------------------------------------*/

/* verbal skills */
pca ONET_ASVAB_3-ONET_ASVAB_4, components(1)
predict skill_v, score
sum skill_v if skill_v != .
replace skill_v = (skill_v - r(min))/r(sd)  if skill_v != .

/* math skills */
pca ONET_ASVAB_1-ONET_ASVAB_2, components(1)
predict skill_m, score
sum skill_m if skill_m != .
replace skill_m = (skill_m - r(min))/r(sd)  if skill_m != .

/* cognitive skill */
pca ONET_ASVAB_1-ONET_ASVAB_4, components(1)
predict skill_c, score
sum skill_c if skill_c != .
replace skill_c = (skill_c - r(min))/r(sd)  if skill_c != .

/* mechanic skill */
pca ONET_ASVAB_6-ONET_ASVAB_7, components(1)
predict skill_e, score
sum skill_e if skill_e != .
replace skill_e = (skill_e - r(min))/r(sd)  if skill_e != .

/* health skill */
forvalues s = 22/40 {
	qui sum ONET_`s'
	replace ONET_`s' = ONET_`s'/r(sd)
}

pca ONET_22-ONET_40, components(1)
predict skill_h, score
sum skill_h if skill_h != .
replace skill_h = (skill_h - r(min))/r(sd)  if skill_h != .
///////////////////
/////health iv/////
///////////////////
/* health skill iv */
gen skill_h_iv = skill_h

/*------------------------------------------------------------------------------------*/

/* verbal ability */
pca asvab_std3-asvab_std4 if ind_indic == 1, components(1)
predict ability_v if ind_indic == 1, score
sum ability_v if ability_v != . & ind_indic == 1
replace ability_v = (ability_v - r(min))/r(sd)  if ability_v != . & ind_indic == 1
bysort id: egen ability_tmp = max(ability_v)
replace ability_v = ability_tmp
drop ability_tmp

/* math ability */
pca asvab_std1-asvab_std2 if ind_indic == 1, components(1)
predict ability_m if ind_indic == 1, score
sum ability_m if ability_m != . & ind_indic == 1
replace ability_m = (ability_m - r(min))/r(sd)  if ability_m != . & ind_indic == 1
bysort id: egen ability_tmp = max(ability_m)
replace ability_m = ability_tmp
drop ability_tmp

/* cognitive ability */
pca asvab_std1-asvab_std4 if ind_indic == 1, components(1)
predict ability_c if ind_indic == 1, score
sum ability_c if ability_c != . & ind_indic == 1
replace ability_c = (ability_c - r(min))/r(sd)  if ability_c != . & ind_indic == 1
bysort id: egen ability_tmp = max(ability_c)
replace ability_c = ability_tmp
drop ability_tmp

/* mechanic ability */
pca asvab_std6-asvab_std7 if ind_indic == 1, components(1)
predict ability_e if ind_indic == 1, score
sum ability_e if ability_e != . & ind_indic == 1
replace ability_e = (ability_e - r(min))/r(sd)  if ability_e != . & ind_indic == 1
bysort id: egen ability_tmp = max(ability_e)
replace ability_e = ability_tmp
drop ability_tmp

/* physical ability */
sum hlth_composite
gen ability_h = (hlth_composite - r(min))/r(sd)

/* physical ability iv */
xtset id year
replace bmi = . if bmi<15 | bmi > 40
bysort id: egen age_bmi_init_tmp =min(age) if bmi<.
bysort id: egen age_bmi_init = min(age_bmi_init_tmp)
gen bmi_init_tmp1 = bmi if age == age_bmi_init
bysort id: egen bmi_init =min(bmi_init_tmp1)

drop *_tmp

gen age_bmi_init2 = age_bmi_init^2
by id: egen bmi_init_tmp = max(bmi_init)
replace bmi_init = bmi_init_tmp
reg bmi_init age_bmi_init age_bmi_init2
predict bmi_init_resid, residuals
qui sum bmi_init if age ==24, meanonly
replace bmi_init = bmi_init_resid + r(mean)

gen bmi_init2 = bmi_init^2
gen bmi_init_p25 = bmi_init - 25 if bmi_init>25
replace bmi_init_p25 = 0 if bmi_init<25
gen bmi_init_m25 = bmi_init - 25 if bmi_init<25
replace bmi_init_m25 = 0 if bmi_init>25

label var bmi_init  "Initial BMI"
label var bmi_init2  "BMI$^2$"
label var bmi_init_m25 "BMI-25 | BMI<25"
label var bmi_init_p25 "BMI-25 | BMI>25"

global zlist lths univ hispanic black AFQT_std
global hlth_list bmi_init_p25 bmi_init_m25 bmi_init2 height yob bdad_alive bdad_death bmom_alive bmom_death

reg hlth_composite $hlth_list $zlist, vce(robust)
esttab using ${result}/table_hlth_comp.tex, r2 nodepvars label wide se nonumbers gap mtitles("Health Index") ///
       substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" "Standard" "Robust standard") ///
       drop(_cons) order(bmi_init* height yob bdad_alive bdad_death bmom_alive bmom_death $zlist) replace 

predict hlth_fit
sum hlth_fit
///////////////////
/////health iv/////
///////////////////
gen ability_h_iv = (hlth_fit - r(min))/r(sd)

/*------------------------------------------------------------------------------------*/

/* export the correlation matrix between health and cog */
corr ability_v ability_m ability_h skill_v skill_m skill_h
matrix corr_vmp = r(C)
matrix colnames corr_vmp = "W_Verb" "W_Math" "W_Phys" "O_Verb" "O_Math" "O_Phys" 
matrix rownames corr_vmp = "Worker_Verb" "Worker_Math" "Worker_Phys" "Occ_Verb" "Occ_Math" "Occ_Phys"
outtable using ${result}/table_${diminitls}_corr, replace mat(corr_vmp) nobox f(%9.2f) center caption("Correlations between Skill Dimensions")

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* generate mismatch measure */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

foreach i in "v" "m" "c" "e" "h" {
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
///////////////////
/////health iv/////
///////////////////
foreach i in "h_iv" {
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
gen mm_iv = 0
gen mm_sgn_iv = 0
gen mm_neg_iv = 0
gen mm_pos_iv = 0

gen absmm_aa = absmm_v
gen absmm_bb = absmm_m
gen absmm_cc = absmm_h
gen mm_aa = mm_v
gen mm_bb = mm_m
gen mm_cc = mm_h

/*------------------------------------------------------------------------------------*/
/* mismatch */

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

/*------------------------------------------------------------------------------------*/
///////////////////
/////health iv/////
///////////////////
/* mistmach for health iv */

local nlist "aa bb h_iv"
pca absmm_aa absmm_bb absmm_h_iv
estat loadings, cnorm(unit)
matrix L = e(L)
local Li = 1
local Lwt = 0
foreach i of local nlist {
	gen mm_`i'_neg_iv = 0
	gen mm_`i'_pos_iv = 0
}
foreach i of local nlist {
	local Lhere = abs(L[`Li',1])
	local Lwt = `Lwt' +  `Lhere'
	replace mm_iv = mm_iv + absmm_`i'*`Lhere'
	replace mm_sgn_iv = mm_sgn_iv + mm_`i'*`Lhere'
	replace mm_neg_iv = mm_neg_iv + mm_`i'*`Lhere' if mm_`i' <0
	replace mm_pos_iv = mm_pos_iv + mm_`i'*`Lhere' if mm_`i' >0
	replace mm_`i'_neg_iv = mm_`i'_neg_iv + mm_`i'*`Lhere' if mm_`i' <0
	replace mm_`i'_pos_iv = mm_`i'_pos_iv + mm_`i'*`Lhere' if mm_`i' >0
	local Li = `Li' + 1
}
replace mm_iv = mm_iv/`Lwt'
replace mm_neg_iv = mm_neg_iv/`Lwt'
replace mm_pos_iv = mm_pos_iv/`Lwt'

/*------------------------------------------------------------------------------------*/
/* re-normalize dimensions */

local nlist "aa bb cc"
qui foreach i of local nlist{
	gen absmm_`i'_nostd = absmm_`i'
	sum absmm_`i'_nostd
	replace absmm_`i' =  (absmm_`i'-r(min) )/(r(max) - r(min)) /* back to 0-1 range */
	gen mm_`i'_neg_nostd = mm_`i'_neg
	sum mm_neg, meanonly
	local n0 = r(min)
	local nM = r(max)
	sum mm_`i'_neg, meanonly
	replace mm_`i'_neg =  (mm_`i'_neg )/(r(max) - r(min))*(`nM'- `n0') /* range of aggregate neg mismatch */
	sum mm_pos, meanonly
	local p0 = r(min)
	local pM = r(max)
	sum mm_`i'_pos , meanonly
	replace mm_`i'_pos =  (mm_`i'_pos )/(r(max) - r(min))*(`pM'- `p0') /* range of aggregate neg mismatch */
}

/*------------------------------------------------------------------------------------*/
///////////////////
/////health iv/////
///////////////////
/* re-normalize dimensions for iv */

local nlist "h_iv"
qui foreach i of local nlist{
	gen absmm_`i'_nostd = absmm_`i'
	sum absmm_`i'_nostd
	replace absmm_`i' =  (absmm_`i'-r(min) )/(r(max) - r(min)) /* back to 0-1 range */
}

/*------------------------------------------------------------------------------------*/
/* creating iteraction terms */

gen mm_ten_occ = mm*tenure_occ
gen mm_ten_occ2 = mm*ten_occ2
gen mm_neg_ten_occ = mm_neg*tenure_occ
gen mm_pos_ten_occ = mm_pos*tenure_occ
gen mm_ten_occ_iv = mm*ten_occ_iv
gen mm_ten_occ2_iv = mm*ten_occ2_iv
gen mm_neg_ten_occ_iv = mm_neg*ten_occ_iv
gen mm_pos_ten_occ_iv = mm_pos*ten_occ_iv

local nlist "aa bb cc"
foreach i of local nlist {
	gen mm_`i'_neg_ten_occ = mm_`i'_neg*tenure_occ
	gen mm_`i'_neg_ten_occ_iv = mm_`i'_neg*ten_occ_iv
	gen mm_`i'_pos_ten_occ = mm_`i'_pos*tenure_occ
	gen mm_`i'_pos_ten_occ_iv = mm_`i'_pos*ten_occ_iv
}

/*------------------------------------------------------------------------------------*/
///////////////////
/////health iv/////
///////////////////
/* creating interaction terms for iv */

gen mm_iv_ten_occ = mm_iv*tenure_occ
gen mm_iv_ten_occ2 = mm_iv*ten_occ2
gen mm_neg_iv_ten_occ = mm_neg_iv*tenure_occ
gen mm_pos_iv_ten_occ = mm_pos_iv*tenure_occ
gen mm_iv_ten_occ_iv = mm_iv*ten_occ_iv
gen mm_iv_ten_occ2_iv = mm_iv*ten_occ2_iv
gen mm_neg_iv_ten_occ_iv = mm_neg_iv*ten_occ_iv
gen mm_pos_iv_ten_occ_iv = mm_pos_iv*ten_occ_iv

/*------------------------------------------------------------------------------------*/
/* creating other terms */

gen ability_aa = ability_v
gen ability_bb = ability_m
gen ability_cc = ability_h
gen skill_aa = skill_v
gen skill_bb = skill_m
gen skill_cc = skill_h

gen ability_mean = (ability_aa + ability_bb + ability_cc)/3
gen skill_mean  = (skill_aa + skill_bb + skill_cc)/3
egen ability_stdev = rowsd(ability_aa ability_bb ability_cc)
egen skill_stdev = rowsd(skill_aa skill_bb skill_cc)

gen ability_skill_mean = ability_mean*skill_mean
gen ability_skill_mean_ten_occ = ability_skill_mean*tenure_occ/100
gen ability_skill_mean_ten_occ2 = ability_skill_mean*ten_occ2
gen ability_skill_mean_ten_occ_iv = ability_skill_mean*ten_occ_iv/100
gen ability_skill_mean_ten_occ2_iv = ability_skill_mean*ten_occ2_iv

gen ability_mean_ten_occ = ability_mean*tenure_occ
gen ability_mean_ten_occ2 = ability_mean*ten_occ2
gen ability_mean_ten_occ_iv = ability_mean*ten_occ_iv
gen ability_mean_ten_occ2_iv = ability_mean*ten_occ2_iv
gen skill_mean_ten_occ = skill_mean*tenure_occ
gen skill_mean_ten_occ2 = skill_mean*ten_occ2
gen skill_mean_ten_occ_iv = skill_mean*ten_occ_iv
gen skill_mean_ten_occ2_iv = skill_mean*ten_occ2_iv

local nlist "aa bb cc"

foreach i of local nlist{
	gen ability_`i'_ten_occ = ability_`i'*tenure_occ
	gen ability_`i'_ten_occ_iv = ability_`i'*ten_occ_iv
	gen skill_`i'_ten_occ = skill_`i'*tenure_occ
	gen skill_`i'_ten_occ_iv = skill_`i'*ten_occ_iv
	gen absmm_`i'_ten_occ = absmm_`i'*tenure_occ
	gen absmm_`i'_ten_occ_iv = absmm_`i'*ten_occ_iv
}
gen absmm_h_iv_ten_occ = absmm_h_iv*tenure_occ
gen absmm_h_iv_ten_occ_iv = absmm_h_iv*ten_occ_iv

/*------------------------------------------------------------------------------------*/
/* labeling */

label var mm "Mismatch"
label var mm_ten_occ "Mismatch $\times$ Occ Tenure"
label var mm_neg "Mismatch Negative Components"
label var mm_pos "Mismatch Positive Components"
label var mm_neg_ten_occ "Mismatch Negative $\times$ Occ Tenure"
label var mm_pos_ten_occ "Mismatch Positive $\times$ Occ Tenure"

label var ability_skill_mean "Worker Ability $\times$ Occ Reqs $\times$ 100"
label var ability_skill_mean_ten_occ "Worker Ability $\times$ Occ Reqs $\times$ Occ Tenure"
label var ability_skill_mean_ten_occ2 "Worker Ability $\times$ Occ Reqs $\times$ Occ Tenure$^2 \times$ 100"

label var ability_mean "Worker Ability (Mean)"
label var skill_mean "Occ Reqs (Mean)"
label var ability_mean_ten_occ "Worker Ability $\times$ Occ Tenure"
label var ability_mean_ten_occ2 "Worker Ability $\times$ Occ Tenure$^2 \times$ 100"
label var skill_mean_ten_occ "Occ Reqs $\times$ Occ Tenure"
label var skill_mean_ten_occ2 "Occ Reqs $\times$ Occ Tenure$^2 \times$ 100"

/*------------------------------------------------------------------------------------*/

local nlist "aa bb cc"
local llist "Verbal Math Phys"
local ll = 1
foreach i of local nlist{
	local l: word `ll' of `llist'
	label var ability_`i' "`l' Ability"
	label var ability_`i'_ten_occ "`l' Ability $\times$ Occ Tenure"
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

gen lmm = l.mm if switch_occ==1
replace lmm = l2.mm if switch_occ==1 & lmm ==. /* go back 2 periods if a missing obs */
replace lmm = l.lmm if switch_occ==0 & lmm ==.

gen lmm_iv = l.mm_iv if switch_occ==1
replace lmm_iv = l2.mm_iv if switch_occ==1 & lmm_iv ==. /* go back 2 periods if a missing obs */
replace lmm_iv = l.lmm_iv if switch_occ==0 & lmm_iv ==.


gen lten_occ = l.tenure_occ if id == l.id & switch_occ==1
replace lten_occ  = l2.tenure_occ  if switch_occ==1 & lten_occ ==.
replace lten_occ = l.lten_occ if switch_occ==0 /*BIG CHANGE HERE, CHECK IF IT MESSED THINGS UP!! */

gen loccwage = l.lwage if switch_occ==1
replace loccwage = l2.lwage if switch_occ==1 & loccwage ==. /* go back 2 periods if a missing obs */
replace loccwage = l.loccwage  if switch_occ==0 & loccwage ==.

gen loccten = l.tenure_occ if switch_occ==1
replace loccten = l2.tenure_occ if switch_occ==1 & loccten ==. /* go back 2 periods if a missing obs */
replace loccten = l.loccten if switch_occ==0 & loccten ==.

local nlist "aa bb cc h_iv"

foreach i of local nlist{
	gen lmm_`i' = l.absmm_`i' if id == l.id & switch_occ==1
	replace lmm_`i' = l2.absmm_`i' if switch_occ==1 & lmm_`i'==.
	replace lmm_`i' = l.lmm_`i' if switch_occ==0
	gen lmm_`i'_neg = l.mm_`i'_neg if id == l.id & switch_occ==1
	replace lmm_`i'_neg = l2.mm_`i'_neg if switch_occ==1 & lmm_`i'==.
	replace lmm_`i'_neg = l.lmm_`i'_neg if switch_occ==0	
	gen lmm_`i'_pos = l.mm_`i'_pos if id == l.id & switch_occ==1
	replace lmm_`i'_pos = l2.mm_`i'_pos if switch_occ==1 & lmm_`i'==.
	replace lmm_`i'_pos = l.lmm_`i'_pos if switch_occ==0	
}

gen lmm_neg = l.mm_neg if id == l.id & switch_occ==1
replace lmm_neg = l.lmm_neg if switch_occ==0
gen lmm_pos = l.mm_pos if id == l.id & switch_occ==1
replace lmm_pos = l.lmm_pos if switch_occ==0

gen lmm_neg_iv = l.mm_neg_iv if id == l.id & switch_occ==1
replace lmm_neg_iv = l.lmm_neg_iv if switch_occ==0
gen lmm_pos_iv = l.mm_pos_iv if id == l.id & switch_occ==1
replace lmm_pos_iv = l.lmm_pos_iv if switch_occ==0

bysort idocc: egen lmm_gapfill = min(lmm)
replace lmm = lmm_gapfill if lmm==. & lmm_gapfill <. & switch_count>0
by idocc: egen lmm_neg_gapfill = min(lmm_neg)
replace lmm_neg = lmm_neg_gapfill if lmm_neg==. & lmm_neg_gapfill <. & switch_count>0
by idocc: egen lmm_pos_gapfill = min(lmm_pos)
replace lmm_pos = lmm_pos_gapfill if lmm_pos==. & lmm_pos_gapfill <. & switch_count>0

by idocc: egen lmm_iv_gapfill = min(lmm_iv)
replace lmm_iv = lmm_iv_gapfill if lmm_iv==. & lmm_iv_gapfill <. & switch_count>0
by idocc: egen lmm_neg_iv_gapfill = min(lmm_neg_iv)
replace lmm_neg_iv = lmm_neg_iv_gapfill if lmm_neg_iv==. & lmm_neg_iv_gapfill <. & switch_count>0
by idocc: egen lmm_pos_iv_gapfill = min(lmm_pos)
replace lmm_pos_iv = lmm_pos_iv_gapfill if lmm_pos_iv==. & lmm_pos_iv_gapfill <. & switch_count>0


by idocc: egen lten_gapfill = min(lten_occ)
replace lten_occ = lten_gapfill if lten_occ==. & lten_gapfill <. & switch_count>0
foreach i of local nlist{
	by idocc: egen lmm_`i'_gapfill = min(lmm_`i')
	replace lmm_`i' = lmm_`i'_gapfill if lmm_`i'==. & lmm_`i'_gapfill <. & switch_count>0
	by idocc: egen lmm_`i'_pos_gapfill = min(lmm_`i'_pos)
	replace lmm_`i'_pos = lmm_`i'_pos_gapfill if lmm_`i'_pos==. & lmm_`i'_pos_gapfill <. & switch_count>0
	by idocc: egen lmm_`i'_neg_gapfill = min(lmm_`i'_neg)
	replace lmm_`i'_neg = lmm_`i'_neg_gapfill if lmm_`i'_neg==. & lmm_`i'_neg_gapfill <. & switch_count>0
}
drop l*_gapfill

label var lmm "Last Mismatch"
label var lmm_pos "Last Mismatch, Pos"
label var lmm_neg "Last Mismatch, Neg"

gsort +id -switch_occ +year
by id: gen cmm_ave = sum(lmm*lten_occ)  if switch_occ==1  & lmm<. & lten_occ<.

by id: gen cmm_neg_ave = sum(lmm_neg*lten_occ)  if switch_occ==1 
by id: gen cmm_pos_ave = sum(lmm_pos*lten_occ)  if switch_occ==1 

by id: gen totexp = sum(lten_occ) if switch_occ==1
replace cmm_ave = cmm_ave/totexp if switch_occ==1 
replace cmm_neg_ave = cmm_neg_ave/totexp if switch_occ==1 
replace cmm_pos_ave = cmm_pos_ave/totexp if switch_occ==1 


by id: gen cmm_iv_ave = sum(lmm_iv*lten_occ)  if switch_occ==1  & lmm_iv<. & lten_occ<.

by id: gen cmm_neg_iv_ave = sum(lmm_neg_iv*lten_occ)  if switch_occ==1 
by id: gen cmm_pos_iv_ave = sum(lmm_pos_iv*lten_occ)  if switch_occ==1 

replace cmm_iv_ave = cmm_iv_ave/totexp if switch_occ==1 
replace cmm_neg_iv_ave = cmm_neg_iv_ave/totexp if switch_occ==1 
replace cmm_pos_iv_ave = cmm_pos_iv_ave/totexp if switch_occ==1 

foreach i of local nlist{
	by id: gen cmm_ave_`i' = sum(lmm_`i'*lten_occ)  if switch_occ==1  & lmm_`i'<. & lten_occ<.
	replace cmm_ave_`i' = cmm_ave_`i'/totexp if switch_occ==1 
		
	by id: gen cmm_neg_ave_`i' = sum(lmm_`i'_neg*lten_occ)  if switch_occ==1 
	by id: gen cmm_pos_ave_`i' = sum(lmm_`i'_pos*lten_occ)  if switch_occ==1 
}

xtset id year
replace cmm_ave = l.cmm_ave if switch_occ==0 & mm<.

replace cmm_neg_ave = l.cmm_neg_ave if switch_occ==0 & mm<.
replace cmm_pos_ave = l.cmm_pos_ave if switch_occ==0 & mm<.

replace cmm_iv_ave = l.cmm_iv_ave if switch_occ==0 & mm_iv<.
replace cmm_neg_iv_ave = l.cmm_neg_iv_ave if switch_occ==0 & mm_iv<.
replace cmm_pos_iv_ave = l.cmm_pos_iv_ave if switch_occ==0 & mm_iv<.

foreach i of local nlist{
	replace cmm_ave_`i' = l.cmm_ave_`i' if switch_occ==0 & mm<.
	replace cmm_neg_ave_`i' = l.cmm_neg_ave_`i' if switch_occ==0 & mm<.
	replace cmm_pos_ave_`i' = l.cmm_pos_ave_`i' if switch_occ==0 & mm<.
	label var cmm_ave_`i' "Cumul Mismatch `i'"	
}

label var cmm_ave "Cumul Mismatch"
label var cmm_neg_ave "Cumul Mismatch, Neg"
label var cmm_pos_ave "Cumul Mismatch, Pos"


*now fill in gaps w/in an occ stint
bysort idocc: egen cmm_gapfill = min(cmm_ave)
replace cmm_ave = cmm_gapfill if cmm_ave ==. & cmm_gapfill <. & switch_count>0
bysort idocc: egen cmm_neg_gapfill = min(cmm_neg_ave)
replace cmm_neg_ave = cmm_neg_gapfill if cmm_neg_ave==. & cmm_neg_gapfill <. & switch_count>0
bysort idocc: egen cmm_pos_gapfill = min(cmm_pos_ave)
replace cmm_pos_ave = cmm_pos_gapfill if cmm_pos_ave==. & cmm_pos_gapfill <. & switch_count>0

by idocc: egen cmm_iv_gapfill = min(cmm_iv_ave)
replace cmm_iv_ave = cmm_iv_gapfill if cmm_iv_ave ==. & cmm_iv_gapfill <. & switch_count>0
by idocc: egen cmm_neg_iv_gapfill = min(cmm_neg_iv_ave)
replace cmm_neg_iv_ave = cmm_neg_iv_gapfill if cmm_neg_iv_ave==. & cmm_neg_iv_gapfill <. & switch_count>0
by idocc: egen cmm_pos_iv_gapfill = min(cmm_pos_iv_ave)
replace cmm_pos_iv_ave = cmm_pos_iv_gapfill if cmm_pos_iv_ave==. & cmm_pos_iv_gapfill <. & switch_count>0

foreach i of local nlist{
	by idocc: egen cmm_`i'_gapfill = min(cmm_ave_`i')
	replace cmm_ave_`i' = cmm_`i'_gapfill if cmm_ave_`i'==. & cmm_`i'_gapfill <. & switch_count>0
}
drop c*_gapfill

local nlist "aa bb cc"
local llist "Verbal Math Phys"

local ll = 1
foreach i of local nlist{
	local l: word `ll' of `llist'
	label var lmm_`i' "Last Mismatch, `l'"
	label var lmm_`i'_neg "Last Mismatch Neg, `l'"
	label var lmm_`i'_pos "Last Mismatch Pos, `l'"
	label var cmm_ave_`i' "Cumul Mismatch, `l'"
	label var cmm_neg_ave_`i' "Cumul Mismatch Neg, `l'"
	label var cmm_pos_ave_`i' "Cumul Mismatch Pos, `l'"	
	local ll = `ll' + 1
}

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

/*------------------------------------------------------------------------------------*/

/* benchmark */

/* ols regression */

xtset id year
global xlist $xlist_0
xi: reg lwage $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/bench_ols.ster, replace

/* iv regression (Altonji and Shakotko) */

xtset id year
global xlist $xlist_0
global ivlist $ivlist_0
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/bench_iv.ster, replace

/*------------------------------------------------------------------------------------*/

/* mismatch */
/*
global xlist mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_mm_means.ster, replace

global xlist mm_pos_ten_occ mm_neg_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_pos_ten_occ_iv mm_neg_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage mm_pos mm_neg ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_mm_means_pos_neg.ster, replace
*/

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/ols_mm_means.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm_pos mm_neg $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_mm_means_pos_neg.ster, replace

global xlist  mm mm_ten_occ
global ivlist mm_iv mm_iv_ten_occ
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist $xlist_0 ability_mean skill_mean ability_mean_ten_occ skill_mean_ten_occ i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/ivh_mm_means.ster, replace

global xlist  mm_pos mm_neg mm_pos_ten_occ mm_neg_ten_occ
global ivlist mm_pos_iv mm_neg_iv mm_pos_iv_ten_occ mm_neg_iv_ten_occ
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist $xlist_0 ability_mean skill_mean ability_mean_ten_occ skill_mean_ten_occ i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/ivh_mm_means_pos_neg.ster, replace

global xlist  mm mm_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist mm_iv mm_iv_ten_occ_iv $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv 
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_mm_means.ster, replace

global xlist  mm_pos mm_neg mm_pos_ten_occ mm_neg_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist mm_pos_iv mm_neg_iv mm_pos_iv_ten_occ_iv mm_neg_iv_ten_occ_iv $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv 
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_mm_means_pos_neg.ster, replace

/*------------------------------------------------------------------------------------*/

/* cumulative mismatch */
/*
global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm cmm_ave $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_cmm_mm_means.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm_neg mm_pos cmm_pos_ave cmm_neg_ave $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_cmm_mm_means_pos_neg.ster, replace

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage mm cmm_ave ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_cmm_mm_means.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_pos_ten_occ_iv mm_neg_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage mm_neg mm_pos cmm_pos_ave cmm_neg_ave ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_cmm_mm_means_pos_neg.ster, replace
*/

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm cmm_ave $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_cmm_mm_means.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm_neg mm_pos cmm_pos_ave cmm_neg_ave $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_cmm_mm_means_pos_neg.ster, replace

global xlist  mm cmm_ave mm_ten_occ
global ivlist mm_iv cmm_iv_ave mm_iv_ten_occ
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist $xlist_0 ability_mean skill_mean ability_mean_ten_occ skill_mean_ten_occ i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ivh_cmm_mm_means.ster, replace

global xlist  mm_neg mm_pos cmm_pos_ave cmm_neg_ave mm_pos_ten_occ mm_neg_ten_occ
global ivlist mm_neg_iv mm_pos_iv cmm_pos_iv_ave cmm_neg_iv_ave mm_pos_iv_ten_occ mm_neg_iv_ten_occ
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist $xlist_0 ability_mean skill_mean ability_mean_ten_occ skill_mean_ten_occ i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ivh_cmm_mm_means_pos_neg.ster, replace

global xlist  mm cmm_ave mm_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist mm_iv cmm_iv_ave mm_iv_ten_occ_iv $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_cmm_mm_means.ster, replace

global xlist  mm_neg mm_pos cmm_pos_ave cmm_neg_ave mm_pos_ten_occ mm_neg_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist mm_neg_iv mm_pos_iv cmm_pos_iv_ave cmm_neg_iv_ave mm_pos_iv_ten_occ_iv mm_neg_iv_ten_occ_iv $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_cmm_mm_means_pos_neg.ster, replace

/*------------------------------------------------------------------------------------*/

/* individual component mismatch */
/*
global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0
xi: reg lwage absmm_?? $xlist $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/ols_ind_mm_means.ster, replace

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0
global ivlist absmm_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_mm_means.ster, replace
*/

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0
xi: reg lwage absmm_?? $xlist $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/ols_ind_mm_means.ster, replace

global xlist absmm_cc absmm_cc_ten_occ
global ivlist absmm_h_iv absmm_h_iv_ten_occ
xi: ivregress 2sls lwage absmm_aa absmm_bb absmm_aa_ten_occ absmm_bb_ten_occ ($xlist = $ivlist) $zlist $xlist_0 ability_?? skill_?? ability_??_ten_occ skill_??_ten_occ i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/ivh_ind_mm_means.ster, replace

global xlist absmm_cc absmm_??_ten_occ $xlist_0 ability_??_ten_occ skill_??_ten_occ
global ivlist absmm_h_iv absmm_aa_ten_occ_iv absmm_bb_ten_occ_iv absmm_h_iv_ten_occ_iv $ivlist_0 ability_??_ten_occ_iv skill_??_ten_occ_iv 
xi: ivregress 2sls lwage absmm_aa absmm_bb ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_mm_means.ster, replace

/*------------------------------------------------------------------------------------*/

/* individual component cumulative mismatch */
/*
global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0 
xi: reg lwage cmm_ave_?? absmm_?? $xlist $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_ind_cmm_mm_means.ster, replace

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0 
global ivlist absmm_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage cmm_ave_?? absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmm_mm_means.ster, replace
*/

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0 
xi: reg lwage cmm_ave_?? absmm_?? $xlist $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_ind_cmm_mm_means.ster, replace

global xlist  cmm_ave_cc absmm_cc absmm_cc_ten_occ
global ivlist cmm_ave_h_iv absmm_h_iv absmm_h_iv_ten_occ
xi: ivregress 2sls lwage cmm_ave_aa absmm_aa cmm_ave_bb absmm_bb absmm_aa_ten_occ absmm_bb_ten_occ ($xlist = $ivlist) $zlist $xlist_0 ability_?? skill_?? ability_??_ten_occ skill_??_ten_occ i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ivh_ind_cmm_mm_means.ster, replace

global xlist  cmm_ave_cc absmm_cc absmm_??_ten_occ $xlist_0 ability_??_ten_occ skill_??_ten_occ
global ivlist cmm_ave_h_iv absmm_h_iv absmm_h_iv_ten_occ_iv absmm_aa_ten_occ_iv absmm_bb_ten_occ_iv $ivlist_0 ability_??_ten_occ_iv skill_??_ten_occ_iv
xi: ivregress 2sls lwage cmm_ave_aa absmm_aa cmm_ave_bb absmm_bb ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmm_mm_means.ster, replace

/*------------------------------------------------------------------------------------*/
/* ceating tables */

estimate clear
estimate use ${result}/ols_mm_means.ster
estimate store ols_mm_means
estimate use ${result}/ols_cmm_mm_means.ster
estimate store ols_cmm_mm_means
estimate use ${result}/ols_cmm_mm_means_pos_neg.ster
estimate store ols_cmm_mm_means_pos_neg

estimate use ${result}/ivh_mm_means.ster
estimate store ivh_mm_means
estimate use ${result}/ivh_cmm_mm_means.ster
estimate store ivh_cmm_mm_means
estimate use ${result}/ivh_cmm_mm_means_pos_neg.ster
estimate store ivh_cmm_mm_means_pos_neg

estimate use ${result}/iv_mm_means.ster
estimate store iv_mm_means
estimate use ${result}/iv_cmm_mm_means.ster
estimate store iv_cmm_mm_means
estimate use ${result}/iv_cmm_mm_means_pos_neg.ster
estimate store iv_cmm_mm_means_pos_neg

esttab iv_mm_means iv_cmm_mm_means ivh_mm_means ivh_cmm_mm_means ols_mm_means ols_cmm_mm_means ///
                   using ${result}/table_${diminitls}.tex, b(4) ///
                   r2 nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" "Standard" "Robust standard") ///
		   drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV: TH" "IV: TH" "IV: H" "IV: H" "OLS" "OLS") ///
		   order(mm mm_ten_occ cmm_ave ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
esttab iv_mm_means iv_cmm_mm_means ivh_mm_means ivh_cmm_mm_means ols_mm_means ols_cmm_mm_means ///
                   using ${result}/table_apx_${diminitls}.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\).") ///
		   title("Wage Regression with Mismatch (Full Results)") ///
		   drop(_I*) ///
		   mtitles("IV: TH" "IV: TH" "IV: H" "IV: H" "OLS" "OLS") ///
		   order(mm mm_ten_occ cmm_ave ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ ten* exp* oj $zlist _cons) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace

/*------------------------------------------------------------------------------------*/

estimate clear
estimate use ${result}/ols_ind_mm_means.ster
estimate store ols_ind_mm_means
estimate use ${result}/ols_ind_cmm_mm_means.ster
estimate store ols_ind_cmm_mm_means

estimate use ${result}/ivh_ind_mm_means.ster
estimate store ivh_ind_mm_means
estimate use ${result}/ivh_ind_cmm_mm_means.ster
estimate store ivh_ind_cmm_mm_means

estimate use ${result}/iv_ind_mm_means.ster
estimate store iv_ind_mm_means
estimate use ${result}/iv_ind_cmm_mm_means.ster
estimate store iv_ind_cmm_mm_means

esttab iv_ind_mm_means iv_ind_cmm_mm_means ivh_ind_mm_means ivh_ind_cmm_mm_means ols_ind_mm_means ols_ind_cmm_mm_means ///
                   using ${result}/table_${diminitls}_ind.tex, b(4) ///
                   r2 nodepvars gaps label not nonotes substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" "Standard" "Robust standard") ///
		   drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV: TH" "IV: TH" "IV: H" "IV: H" "OLS" "OLS") ///
		   order(absmm_aa absmm_bb absmm_cc absmm_aa_t* absmm_bb_t* absmm_cc_t* cmm_ave_aa cmm_ave_bb cmm_ave_cc ability_?? ability_??_* skill_?? skill_??_*) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
esttab iv_ind_mm_means iv_ind_cmm_mm_means ivh_ind_mm_means ivh_ind_cmm_mm_means ols_ind_mm_means ols_ind_cmm_mm_means ///
                   using ${result}/table_apx_${diminitls}_ind.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\).") ///
		   drop(_I* _cons) ///
		   mtitles("IV: TH" "IV: TH" "IV: H" "IV: H" "OLS" "OLS") ///
		   title("Wage Regression with Mismatch by Components (Full Results)") ///
		   order(absmm_aa absmm_bb absmm_cc absmm_aa_t* absmm_bb_t* absmm_cc_t* cmm_ave_aa cmm_ave_bb cmm_ave_cc ability_?? ability_??_* skill_?? skill_??_* ten* exp* oj $zlist) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace

/*------------------------------------------------------------------------------------*/
/* predicted profile of mismatch */

estimate use ${result}/iv_cmm_mm_means.ster
matrix pred_mm = J(5,4,0.0)

_pctile mm, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)

forvalues tt = 1/3 {
	matrix pred_mm[1,`tt'] = _b["mm"]*`p90'+_b["mm_ten_occ"]*`p90'*`tt'*5
	matrix pred_mm[2,`tt'] = _b["mm"]*`p70'+_b["mm_ten_occ"]*`p70'*`tt'*5
	matrix pred_mm[3,`tt'] = _b["mm"]*`p50'+_b["mm_ten_occ"]*`p50'*`tt'*5
	matrix pred_mm[4,`tt'] = _b["mm"]*`p30'+_b["mm_ten_occ"]*`p30'*`tt'*5
	matrix pred_mm[5,`tt'] = _b["mm"]*`p10'+_b["mm_ten_occ"]*`p10'*`tt'*5
}

_pctile cmm_ave, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)

matrix pred_mm[1,4] = _b["cmm_ave"]*`p90'
matrix pred_mm[2,4] = _b["cmm_ave"]*`p70'
matrix pred_mm[3,4] = _b["cmm_ave"]*`p50'
matrix pred_mm[4,4] = _b["cmm_ave"]*`p30'
matrix pred_mm[5,4] = _b["cmm_ave"]*`p10'

matrix colnames pred_mm = "MM_5_Years" "MM_10_Years" "MM_15_Years" "Cumul_MM"
matrix rownames pred_mm = "90_Percentile" "70_Percentile" "50_Percentile" "30_Percentile" "10_Percentile"
outtable using ${result}/table_${diminitls}_pred_mm, replace mat(pred_mm) nobox f(%9.3f)  center caption("Wage Losses from Mismatch or Cumul. Mismatch")

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* switching probability regressions */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

global prswlist $zlist ability_mean skill_mean i.occ_1d  i.ind_1d oj exp exp2 tenure_occ ten_occ2 tenure_emp ten_emp2 skill_mean_ten_occ ability_mean_ten_occ 
global prswlist_x tenure_occ ten_occ2 tenure_emp ten_emp2 skill_mean_ten_occ ability_mean_ten_occ oj
global prswlist_z $zlist ability_mean skill_mean i.occ_1d i.ind_1d exp exp2
global prswlist_iv ten_occ_iv ten_occ2_iv ten_emp_iv ten_emp2_iv skill_mean_ten_occ_iv ability_mean_ten_occ_iv oj_iv

xtset id year
gen fswitch_occ = f.switch_occ

/*------------------------------------------------------------------------------------*/

/* probit regressions */
/*
xi: probit fswitch_occ mm $prswlist 
matrix matV_mm = e(V)
margins, dydx(*) post
estimate save ${result}/pr_${diminitls}_mm.ster, replace

xi: probit fswitch_occ absmm_?? $prswlist 
matrix matV_mm_ind = e(V)
margins, dydx(*) post
estimate save ${result}/pr_${diminitls}_mm_ind.ster, replace
*/

/*------------------------------------------------------------------------------------*/

/* linear probability regressions */

xi: reg fswitch_occ mm $prswlist 
estimate save ${result}/lpm_${diminitls}_mm.ster, replace

xi: reg fswitch_occ absmm_?? $prswlist 
estimate save ${result}/lpm_${diminitls}_mm_ind.ster, replace

xi: ivregress 2sls fswitch_occ (mm = mm_iv) $prswlist, vce(robust)
estimate save ${result}/ivh_lpm_${diminitls}_mm.ster, replace

xi: ivregress 2sls fswitch_occ absmm_aa absmm_bb (absmm_cc  = absmm_h_iv) $prswlist, vce(robust)
estimate save ${result}/ivh_lpm_${diminitls}_mm_ind.ster, replace

xi: ivregress 2sls fswitch_occ (${prswlist_x}  mm = ${prswlist_iv} mm_iv) $prswlist_z, vce(robust)
estimate save ${result}/iv_lpm_${diminitls}_mm.ster, replace

xi: ivregress 2sls fswitch_occ absmm_aa absmm_bb ($prswlist_x absmm_cc  = ${prswlist_iv} absmm_h_iv) $prswlist_z, vce(robust)
estimate save ${result}/iv_lpm_${diminitls}_mm_ind.ster, replace

/*------------------------------------------------------------------------------------*/
/* creating tables */

estimate clear

estimate use ${result}/lpm_${diminitls}_mm.ster
estimate store lpm_${diminitls}_mm
estimate use ${result}/lpm_${diminitls}_mm_ind.ster
estimate store lpm_${diminitls}_mm_ind
estimate use ${result}/iv_lpm_${diminitls}_mm.ster
estimate store iv_lpm_${diminitls}_mm
estimate use ${result}/iv_lpm_${diminitls}_mm_ind.ster
estimate store iv_lpm_${diminitls}_mm_ind
estimate use ${result}/ivh_lpm_${diminitls}_mm.ster
estimate store ivh_lpm_${diminitls}_mm
estimate use ${result}/ivh_lpm_${diminitls}_mm_ind.ster
estimate store ivh_lpm_${diminitls}_mm_ind
/*
estimate use ${result}/pr_${diminitls}_mm.ster
estimate store pr_${diminitls}_mm
estimate use ${result}/pr_${diminitls}_mm_ind.ster
estimate store pr_${diminitls}_mm_ind
*/
esttab iv_lpm_${diminitls}_mm iv_lpm_${diminitls}_mm_ind ivh_lpm_${diminitls}_mm ivh_lpm_${diminitls}_mm_ind lpm_${diminitls}_mm lpm_${diminitls}_mm_ind ///
                   using ${result}/table_${diminitls}_switch_lpm.tex, b(4) ///
                   nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" "Standard" "Robust standard") ///
		   mtitles("LPM: TH" "LPM: TH" "LPM: H" "LPM: H" "LPM" "LPM") ///
		   order(mm absmm_aa absmm_bb absmm_cc ability_mean ability_mean* skill_mean skill_mean*) drop(tenure_occ ten_occ2 tenure_emp ten_emp2 exp exp2 oj $zlist _cons _I* ) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
esttab iv_lpm_${diminitls}_mm iv_lpm_${diminitls}_mm_ind ivh_lpm_${diminitls}_mm ivh_lpm_${diminitls}_mm_ind lpm_${diminitls}_mm lpm_${diminitls}_mm_ind ///
                   using ${result}/table_apx_${diminitls}_switch_lpm.tex, b(4) se(4) ///
                   nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\).") ///
		   mtitles("LPM: TH" "LPM: TH" "LPM: H" "LPM: H" "LPM" "LPM") ///
		   title("Regressions for Probability of Occupational Switch (Full Results)") ///
		   order(mm absmm_aa absmm_bb absmm_cc ability_mean ability_mean* skill_mean skill_mean* tenure_occ ten_occ2 tenure_emp ten_emp2 exp exp2 oj $zlist) drop(_cons _I*) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
/*------------------------------------------------------------------------------------*/
/* table for the effect of mismatch */

estimate clear
estimate use ${result}/iv_lpm_${diminitls}_mm.ster
matrix pred_switch = J(5,4,0.0)

_pctile mm, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)

matrix pred_switch[1,1] = _b["mm"]*`p90'
matrix pred_switch[2,1] = _b["mm"]*`p70'
matrix pred_switch[3,1] = _b["mm"]*`p50'
matrix pred_switch[4,1] = _b["mm"]*`p30'
matrix pred_switch[5,1] = _b["mm"]*`p10'

local ll = 2
foreach i of local nlist{
	estimate clear
	estimate use ${result}/iv_lpm_${diminitls}_mm_ind.ster
	
	_pctile mm, p(10 30 50 70 90)
	local p90 = r(r5)
	local p70 = r(r4)
	local p50 = r(r3)
	local p30 = r(r2)
	local p10 = r(r1)

	matrix pred_switch[1,`ll'] = _b["absmm_`i'"]*`p90'
	matrix pred_switch[2,`ll'] = _b["absmm_`i'"]*`p70'
	matrix pred_switch[3,`ll'] = _b["absmm_`i'"]*`p50'
	matrix pred_switch[4,`ll'] = _b["absmm_`i'"]*`p30'
	matrix pred_switch[5,`ll'] = _b["absmm_`i'"]*`p10'
	
	local ll = `ll' + 1
}

matrix colnames pred_switch = "Mismatch" "Verbal" "Math" "Physical"
matrix rownames pred_switch = "90_Percentile" "70_Percentile" "50_Percentile" "30_Percentile" "10_Percentile"
outtable using ${result}/table_${diminitls}_pred_switch, replace mat(pred_switch) nobox f(%9.3f)  center caption("Effect of Mismatch on Probability of Switch")


/*------------------------------------------------------------------------------------*/
/* plots!!*/

/* switch rate by mismatch */
twoway (lpolyci fswitch_occ mm, clcolor(blue) clwidth(thick)), ///
ytitle("Probability of Occupational Switch", size(medlarge)) ///
xtitle("Mismatch", size(medlarge)) ///
title("Probability of Occupational Switch", size(large)) ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(0.14(0.04)0.26, grid gstyle(dot)) ///
saving(${result}/fig_pswitch_${diminitls}_mm, replace)
graph export ${result}/fig_pswitch_${diminitls}_mm.eps, replace

/* switch rate by mismatch aa and bb cc*/
twoway (lpoly fswitch_occ absmm_aa, lcolor(blue) lpattern(solid) lwidth(thick)) || ///
       (lpoly fswitch_occ absmm_bb, lcolor(red) lpattern(longdash) lwidth(thick)) || ///
       (lpoly fswitch_occ absmm_cc, lcolor(green) lpattern(dash) lwidth(thick)), ///
ytitle("Probability of Occupational Switch", size(medlarge)) ///
xtitle("Mismatch", size(medlarge)) ///
title("Probability of Occupational Switch", size(large)) ///
legend(lab(1 "Verbal Mismatch") lab(2 "Math Mismatch") lab(3 "Physical  Mismatch") ring(0) pos(12) cols(1)) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(0.14(0.04)0.26,grid gstyle(dot)) ///
saving(${result}/fig_pswitch_${diminitls}_mm_aa_mm_bb_mm_cc, replace)
graph export ${result}/fig_pswitch_${diminitls}_mm_aa_mm_bb_mm_cc.eps, replace

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
/* regressions for direction */

xtset id year
gen lmm_sgn = l.mm_sgn if switch_occ == 1
gen lmm_sgn_iv = l.mm_sgn_iv if switch_occ == 1
label var lmm_sgn "Last Signed Mismatch"
gen lexp = l.exp
label var lexp "Experience"
gen lexp2 = l.exp2
label var lexp2 "Experience$^2 \times$ 100"
gen ltenure_occ = l.tenure_occ
label var ltenure_occ "Occupational Tenure"
gen lten_occ2 = l.ten_occ2 
label var lten_occ2 "Occupational Tenure$^2 \times$ 100"
gen ltenure_emp = l.tenure_emp
label var ltenure_emp "Employer Tenure"
gen lten_emp2 = l.ten_emp2 
label var lten_emp2 "Employer Tenure$^2 \times$ 100"
gen loj = l.oj
label var loj "Old Job"

gen lmm_h_iv_sgn = l.mm_h_iv

local llist "Verbal Math Physical"
local ll =1
foreach i of local nlist {
	local l: word `ll' of `llist'
	
	gen chng_skill_`i' = skill_`i' - l.skill_`i'
	label var chng_skill_`i' "Skill Change `l'"
	gen lmm_`i'_sgn = l.mm_`i'
	label var lmm_`i'_sgn "Last Signed Mismatch `l'"
	gen ability_`i'_lten = ability_`i'*l.tenure_occ
	label var ability_`i'_lten "`l' Ability $\times$  Tenure"

	global swlist $zlist ltenure_occ lten_occ2 ltenure_emp lten_emp2 lexp lexp2 loj i.occ_1d i.ind_1d
	if("`i'"=="aa" | "`i'"=="bb") {
	xi: reg chng_skill_`i' lmm_`i'_neg lmm_`i'_pos $swlist if switch_occ==1 & lmm_neg <. & lmm_pos<.
	estimate save ${result}/mm_${diminitls}_where_`i'_pos_neg.ster, replace
        xi: reg chng_skill_`i' lmm_`i'_sgn $swlist if switch_occ==1
        estimate save ${result}/mm_${diminitls}_where_`i'.ster, replace 
	}
	if("`i'"=="cc") {
        /*
	xi: ivregress 2sls chng_skill_`i' (lmm_`i'_neg lmm_`i'_pos = lmm_h_iv_neg lmm_h_iv_pos) $swlist if switch_occ==1 & lmm_neg <. & lmm_pos<.
	estimate save ${result}/mm_${diminitls}_where_`i'_pos_neg.ster, replace
	xi: ivregress 2sls chng_skill_`i' (lmm_`i'_sgn = lmm_h_iv_sgn) $sgnlist if switch_occ==1
        estimate save ${result}/mm_${diminitls}_where_`i'.ster, replace 
	*/
	xi: reg chng_skill_`i' lmm_`i'_neg lmm_`i'_pos $swlist if switch_occ==1 & lmm_neg <. & lmm_pos<.
	estimate save ${result}/mm_${diminitls}_where_`i'_pos_neg.ster, replace
	xi: reg chng_skill_`i' lmm_`i'_sgn $swlist if switch_occ==1
        estimate save ${result}/mm_${diminitls}_where_`i'.ster, replace
	}
	
	local ll =`ll'+1
}

gen ability_mean_lten = ability_mean*ltenure_occ
label var ability_mean_lten "Worker Ability $\times$ Occ Tenure"
egen chng_skill = rowmean(chng_skill_??)
label var chng_skill "Change in Skill"
/*
global swlist ability_mean ability_mean_lten $zlist ltenure_occ lten_occ2 ltenure_emp lten_emp2 lexp lexp2 loj i.occ_1d i.ind_1d
xi: ivregress 2sls chng_skill (lmm_neg lmm_pos = lmm_neg_iv lmm_pos_iv) $swlist if switch_occ==1
estimate save ${result}/mm_${diminitls}_where_pos_neg.ster, replace
*/
gen lmm_sgn_pos = 0
gen lmm_sgn_neg = 0
replace lmm_sgn_pos = lmm_sgn if lmm_sgn >= 0
replace lmm_sgn_neg = lmm_sgn if lmm_sgn < 0
label var lmm_sgn_pos "Last Mismatch Pos."
label var lmm_sgn_neg "Last Mismatch Neg."
global swlist $zlist ltenure_occ lten_occ2 ltenure_emp lten_emp2 lexp lexp2 loj i.occ_1d i.ind_1d
xi: reg chng_skill lmm_sgn_pos lmm_sgn_neg $swlist if switch_occ==1
estimate save ${result}/mm_${diminitls}_where_pos_neg.ster, replace
xi: reg chng_skill lmm_sgn $swlist if switch_occ==1
estimate save ${result}/mm_${diminitls}_where.ster, replace

/*------------------------------------------------------------------------------------*/
/* creating tables */

estimate clear
estimate use ${result}/mm_${diminitls}_where_pos_neg.ster
estimate store mm_${diminitls}_where_pos_neg
estimate use ${result}/mm_${diminitls}_where_aa_pos_neg.ster
estimate store mm_${diminitls}_where_aa_pos_neg
estimate use ${result}/mm_${diminitls}_where_bb_pos_neg.ster
estimate store mm_${diminitls}_where_bb_pos_neg
estimate use ${result}/mm_${diminitls}_where_cc_pos_neg.ster
estimate store mm_${diminitls}_where_cc_pos_neg

esttab mm_${diminitls}_where_pos_neg mm_${diminitls}_where_aa_pos_neg mm_${diminitls}_where_bb_pos_neg mm_${diminitls}_where_cc_pos_neg ///
                   using ${result}/table_${diminitls}_mm_where.tex, b(4) ///
                   r2 nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" "Standard" "Robust standard") ///
		   mtitles("Average of All" "Verbal" "Math" "Physical") ///
		   order(lmm_sgn_pos lmm_sgn_neg lmm_aa_pos lmm_aa_neg lmm_bb_pos lmm_bb_neg lmm_cc_pos lmm_cc_neg) drop(lten* lexp* loj $zlist _cons _I*) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace

esttab mm_${diminitls}_where_pos_neg mm_${diminitls}_where_aa_pos_neg mm_${diminitls}_where_bb_pos_neg mm_${diminitls}_where_cc_pos_neg ///
                   using ${result}/table_apx_${diminitls}_mm_where.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\).") ///
		   mtitles("Average of All" "Verbal" "Math" "Physical") ///
		   title("Regressions for Direction of Occupational Switch (Full Results)") ///
		   order(lmm_sgn_pos lmm_sgn_neg lmm_aa_pos lmm_aa_neg lmm_bb_pos lmm_bb_neg lmm_cc_pos lmm_cc_neg lten* lexp* loj $zlist) drop(_cons _I*) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
/*------------------------------------------------------------------------------------*/
/* summary statitics for change in skills */

local nlist "aa bb cc"
matrix mat_chng_skill = J(4,3,0.0)

/* all */
local ll = 1
foreach i of local nlist{
        qui su chng_skill_`i' if switch_occ == 1, meanonly
	matrix mat_chng_skill[1,`ll'] = r(mean)*100
	local ll = `ll' + 1
}

/* less than high school */
local ll = 1
foreach i of local nlist{
        qui su chng_skill_`i' if switch_occ == 1 & grade < 12, meanonly
	matrix mat_chng_skill[2,`ll'] = r(mean)*100
	local ll = `ll' + 1
}

/* high school */
local ll = 1
foreach i of local nlist{
        qui su chng_skill_`i' if switch_occ == 1 & grade == 12, meanonly
	matrix mat_chng_skill[3,`ll'] = r(mean)*100
	local ll = `ll' + 1
}

/* some college */
local ll = 1
foreach i of local nlist{
        qui su chng_skill_`i' if switch_occ == 1 & grade > 12, meanonly
	matrix mat_chng_skill[4,`ll'] = r(mean)*100
	local ll = `ll' + 1
}

matrix colnames mat_chng_skill = "Verbal" "Math" "Physical"
matrix rownames mat_chng_skill = "All" "Less_than_High_School" "High_School" "Some_College"
outtable using ${result}/table_${diminitls}_chng_skill, replace mat(mat_chng_skill) nobox f(%9.1f) center caption("Change in Skills when Switching Occupations")

/*------------------------------------------------------------------------------------*/

estimate clear
estimate use ${result}/mm_${diminitls}_where_pos_neg.ster
matrix pred_where = J(6,4,0.0)

_pctile lmm_sgn_pos if switch_occ == 1 & lmm_sgn_pos != 0, p(10 50 90)
local p90 = r(r3)
local p50 = r(r2)
local p10 = r(r1)

matrix pred_where[1,1] = _b["lmm_sgn_pos"]*`p90'*100
matrix pred_where[2,1] = _b["lmm_sgn_pos"]*`p50'*100
matrix pred_where[3,1] = _b["lmm_sgn_pos"]*`p10'*100

_pctile lmm_sgn_neg if switch_occ == 1 & lmm_sgn_neg != 0, p(10 50 90)
local p90 = r(r3)
local p50 = r(r2)
local p10 = r(r1)

matrix pred_where[4,1] = _b["lmm_sgn_neg"]*`p90'*100
matrix pred_where[5,1] = _b["lmm_sgn_neg"]*`p50'*100
matrix pred_where[6,1] = _b["lmm_sgn_neg"]*`p10'*100

/* loop over skills */
local ll = 2
foreach i of local nlist{
	estimate clear
	estimate use ${result}/mm_${diminitls}_where_`i'_pos_neg.ster

	_pctile lmm_`i'_pos if switch_occ == 1 & lmm_`i'_pos != 0, p(10 50 90)
	local p90 = r(r3)
	local p50 = r(r2)
	local p10 = r(r1)

	matrix pred_where[1,`ll'] = _b["lmm_`i'_pos"]*`p90'*100
	matrix pred_where[2,`ll'] = _b["lmm_`i'_pos"]*`p50'*100
	matrix pred_where[3,`ll'] = _b["lmm_`i'_pos"]*`p10'*100

	_pctile lmm_`i'_neg if switch_occ == 1 & lmm_`i'_neg != 0, p(10 50 90)
	local p90 = r(r3)
	local p50 = r(r2)
	local p10 = r(r1)

	matrix pred_where[4,`ll'] = _b["lmm_`i'_neg"]*`p90'*100
	matrix pred_where[5,`ll'] = _b["lmm_`i'_neg"]*`p50'*100
	matrix pred_where[6,`ll'] = _b["lmm_`i'_neg"]*`p10'*100
	
	local ll = `ll' + 1
}

matrix colnames pred_where = "All_Average" "Verbal" "Math" "Physical"
matrix rownames pred_where = "90_Pos" "50_Pos" "10_Pos" "90_Neg" "50_Neg" "10_Neg"
outtable using ${result}/table_${diminitls}_pred_where, replace mat(pred_where) nobox f(%9.1f)  center caption("The Effect of Mismatch on Direction of Switch")

/*------------------------------------------------------------------------------------*/
/* 9lot the change in skills */

graph twoway (lpolyci chng_skill lmm_sgn if switch_occ==1, clcolor(blue) clpattern(solid) clwidth(thick)), ///
ytitle("Average Change in Skill", size(medlarge)) ///
xtitle("Last Signed Mismatch", size(medlarge)) ///
title("Direction of Switch, All Average", size(large)) ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(-0.3(0.15)0.3, grid gstyle(dot)) ///
xline(0, lcolor(red) lpattern(solid)) ///
saving(${result}/fig_where_${diminitls}_mm, replace)
graph export ${result}/fig_where_${diminitls}_mm.eps, replace

local llist "Verbal Math Physical"
local ll =1
foreach i of local nlist {
	local l: word `ll' of `llist'
	graph twoway (lpolyci chng_skill lmm_`i'_sgn if switch_occ==1, clcolor(blue) clpattern(solid) clwidth(thick)), ///
	ytitle("Change in Skill, `l'", size(medlarge)) ///
	xtitle("Last Signed Mismatch in `l'", size(medlarge)) ///
	title("Direction of  Switch, `l'", size(large)) ///
	legend(off) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(-0.3(0.15)0.3, grid gstyle(dot)) ///
	xline(0, lcolor(red) lpattern(solid)) ///
	saving(${result}/fig_where_${diminitls}_mm, replace)
	graph export ${result}/fig_where_${diminitls}_mm_`i'.eps, replace
	local ll = `ll'+1
}

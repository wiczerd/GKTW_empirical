/*--------------------------------------------------------------------------------------
* name: mismatch_04_regression.do
* made by: david wiczer, moidfied by: satoshi tanaka
* date: 08/21/2010
*       04/03/2015
* description: this code is for the project 'occupation skill mismatch'
--------------------------------------------------------------------------------------*/

use ${result}/yearly_02.dta, clear
set more off

/*------------------------------------------------------------------------------------*/

global diminitls "vmsp"

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
	sum asvab_sec`i' if tmp2== 20
	local sd20 = r(sd)
	forvalues ai=16/24{
		sum asvab_sec`i' if tmp2 ==`ai'
		replace asvab_sec`i' = asvab_sec`i'/r(sd)*`sd20' if tmp2 ==`ai'
	}
}

/* take out the cohort effects on mean from asvab scores*/
qui forvalues i=1/7{
	reg asvab_sec`i' I_enterage1-I_enterage8
	predict asvab_res`i', residual 
	replace asvab_res`i' = asvab_res`i'
}
qui reg AFQT I_enterage1-I_enterage8
qui predict AFQT_res, residual 
qui replace AFQT_res = AFQT_res

/* take out the cohort effects on mean from social ability scores */
qui reg rotter_score I_enterage1-I_enterage8
predict social_res1, residual 
replace social_res1 = -social_res1

qui reg rosenberg_score I_enterage1-I_enterage8
predict social_res2, residual

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

/* health skill */
forvalues s = 22/40 {
	qui sum ONET_`s'
	replace ONET_`s' = ONET_`s'/r(sd)
}

pca ONET_22-ONET_40, components(1)
predict skill_h, score
sum skill_h if skill_h != .
replace skill_h = (skill_h - r(min))/r(sd)  if skill_h != .
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

/* social ability */
pca social_std1 social_std2 if ind_indic == 1, components(1)
predict ability_s if ind_indic == 1, score
sum ability_s if ability_s != . & ind_indic == 1
replace ability_s = (ability_s - r(min))/r(sd)  if ability_s != . & ind_indic == 1
bysort id: egen ability_tmp = max(ability_s)
replace ability_s = ability_tmp
drop ability_tmp

/* physical ability */
reg hlth_composite I_enterage1-I_enterage8 lths univ hispanic black
qui predict hlth_std, resid
qui sum hlth_std
gen ability_h = (hlth_std - r(min))/r(sd)

/*------------------------------------------------------------------------------------*/

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

global hlth_list bmi_init_p25 bmi_init_m25 bmi_init2 height bdad_alive bdad_death bmom_alive bmom_death
foreach hv of varlist height bmi_init2 bmi_init{
	foreach hvv of varlist $hlth_list{
		gen `hv'X`hvv' = `hv'*`hvv'
		global hlth_list2 $hlth_list2 `hv'X`hvv'
	}
}
reg ability_h ${hlth_list} ${hlth_list2}, vce(robust)

/* csv */
esttab using ${result}/table_hlth_comp.csv, r2 nodepvars label wide se nonumbers nogap mtitles("Health Index") ///
       drop(_cons) order(bmi_init* height yob bdad_alive bdad_death bmom_alive bmom_death $zlist) replace
/* tex */
esttab using ${result}/table_hlth_comp.tex, r2 nodepvars label wide se nonumbers gap mtitles("Health Index") ///
       substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" "Standard" "Robust standard" ///
       "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
       drop(_cons) order(bmi_init* height yob bdad_alive bdad_death bmom_alive bmom_death $zlist) ///
       star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace 

predict hlth_fit, xb
sum hlth_fit
gen ability_h_iv = (hlth_fit - r(min))/r(sd)

/*------------------------------------------------------------------------------------*/

/* export the correlation matrix between health and cog */
corr ability_v ability_m ability_s ability_h_iv skill_v skill_m skill_s skill_h_iv
matrix corr_vmp = r(C)
matrix colnames corr_vmp = "W_Verb" "W_Math" "W_Soc" "W_Phys" "O_Verb" "O_Math" "O_Soc" "O_Phys"
matrix rownames corr_vmp = "Worker_Verb" "Worker_Math" "Worker_Soc" "Worker_Phys" "Occ_Verb" "Occ_Math" "Occ_Soc" "Occ_Phys"
putexcel A1=matrix(corr_vmp, names) using ${result}/table_${diminitls}_corr.xls, replace

/*------------------------------------------------------------------------------------*/

global zlist lths univ hispanic black AFQT_std

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* generate mismatch measure */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

foreach i in "v" "m" "s" "h" "h_iv" {
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
gen absmm_dd = absmm_h_iv
gen mm_aa = mm_v
gen mm_bb = mm_m
gen mm_cc = mm_s
gen mm_dd = mm_h_iv

/*------------------------------------------------------------------------------------*/
/* mismatch */

local nlist "aa bb cc dd"

pca absmm_aa absmm_bb absmm_cc absmm_dd
estat loadings, cnorm(unit)
matrix L = e(L)
local Li = 1
local Lwt = 0

foreach i of local nlist {
	local Lhere = abs(L[`Li',1])
	local Lwt = `Lwt' +  `Lhere'
	replace mm = mm + absmm_`i'*`Lhere'
	replace mm_sgn = mm_sgn + mm_`i'*`Lhere'
	replace mm_neg = mm_neg + mm_`i'*`Lhere' if mm_`i' <0
	replace mm_pos = mm_pos + mm_`i'*`Lhere' if mm_`i' >0
	
	local Li = `Li' + 1
}
replace mm = mm/`Lwt'
replace mm_sgn = mm_sgn/`Lwt'
replace mm_neg = mm_neg/`Lwt'
replace mm_pos = mm_pos/`Lwt'

/*------------------------------------------------------------------------------------*/
/* re-normalize dimensions */

local nlist "aa bb cc dd"
qui foreach i of local nlist{
	gen mm_`i'_neg =0
	replace mm_`i'_neg = mm_`i'_neg + mm_`i' if mm_`i' <0
	gen mm_`i'_pos =0
	replace mm_`i'_pos = mm_`i'_pos + mm_`i' if mm_`i' >0
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

*/

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* creating iteraction terms */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

* w/o tenure iv
gen mm_ten_occ = mm*tenure_occ
gen mm_ten_occ2 = mm*ten_occ2
gen mm_neg_ten_occ = mm_neg*tenure_occ
gen mm_pos_ten_occ = mm_pos*tenure_occ
local nlist "aa bb cc dd"
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
local nlist "aa bb cc dd"
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
gen ability_dd = ability_h_iv
gen skill_aa = skill_v
gen skill_bb = skill_m
gen skill_cc = skill_s
gen skill_dd = skill_h

gen ability_mean = (ability_aa + ability_bb + ability_cc)/3
gen ability_mean_ten_occ = ability_mean*tenure_occ
gen ability_mean_ten_occ2 = ability_mean*ten_occ2
gen ability_mean_ten_occ_iv = ability_mean*ten_occ_iv
gen ability_mean_ten_occ2_iv = ability_mean*ten_occ2_iv

gen skill_mean = (skill_aa + skill_bb + skill_cc)/3
gen skill_mean_ten_occ = skill_mean*tenure_occ
gen skill_mean_ten_occ2 = skill_mean*ten_occ2
gen skill_mean_ten_occ_iv = skill_mean*ten_occ_iv
gen skill_mean_ten_occ2_iv = skill_mean*ten_occ2_iv

gen  ability_exp = ability_mean*exp

local nlist "aa bb cc dd"
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
label var mm_neg "Mismatch Negative Components"
label var mm_pos "Mismatch Positive Components"
label var mm_neg_ten_occ "Mismatch Negative $\times$ Occ Tenure"
label var mm_pos_ten_occ "Mismatch Positive $\times$ Occ Tenure"

label var ability_mean "Worker Ability (Mean)"
label var skill_mean "Occ Reqs (Mean)"
label var ability_mean_ten_occ "Worker Ability $\times$ Occ Tenure"
label var ability_mean_ten_occ2 "Worker Ability $\times$ Occ Tenure$^2 \times$ 100"
label var skill_mean_ten_occ "Occ Reqs $\times$ Occ Tenure"
label var skill_mean_ten_occ2 "Occ Reqs $\times$ Occ Tenure$^2 \times$ 100"

/*------------------------------------------------------------------------------------*/

local nlist "aa bb cc dd"
local llist "Verbal Math Social Phys"
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

local nlist "aa bb cc dd"
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

local nlist "aa bb cc dd"
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

local nlist "aa bb cc dd"
foreach i of local nlist{
	replace cmm_`i' = cmm_`i'[_n-1] if switch_occ == 0 & id == id[_n-1]
	replace cmm_neg_`i' = cmm_neg_`i'[_n-1] if switch_occ == 0 & id == id[_n-1]
	replace cmm_pos_`i' = cmm_pos_`i'[_n-1] if switch_occ == 0 & id == id[_n-1]
}

label var cmm "Cumul Mismatch"
label var cmm_pos "Cumul Mismatch Positive"
label var cmm_neg "Cumul Mismatch Negative"

local nlist "aa bb cc dd"
local llist "Verbal Math Social Phys"
local ll = 1
capture foreach i of local nlist{
	local l: word `ll' of `llist'
	label var lmm_`i' "Last Mismatch `l'"
	label var lmm_`i'_neg "Last Mismatch Neg. `l'"
	label var lmm_`i'_pos "Last Mismatch Pos. `l'"
	label var cmm_`i' "Cumul Mismatch `l'"
	label var cmm_neg_`i' "Cumul Mismatch Neg. `l'"
	label var cmm_pos_`i' "Cumul Mismatch Pos. `l'"	
	local ll = `ll' + 1
}

save ${result}/yearly_03.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* estimation */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
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

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/ols_mm_means.ster, replace

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_mm_means.ster, replace

/*------------------------------------------------------------------------------------*/
/* mismatch with positive and negative components */

global xlist  mm_pos_ten_occ mm_neg_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm_pos mm_neg $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_mm_means_pos_neg.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_pos_ten_occ_iv mm_neg_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage mm_pos mm_neg ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_mm_means_pos_neg.ster, replace

/*------------------------------------------------------------------------------------*/
/* cumulative mismatch */

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm cmm $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_cmm_mm_means.ster, replace

global xlist  mm_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage mm cmm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_cmm_mm_means.ster, replace

/*------------------------------------------------------------------------------------*/
/* cumulative mismatch with positive and negative components */

global xlist  mm_pos_ten_occ mm_neg_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
xi: reg lwage mm_neg mm_pos cmm_pos cmm_neg $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_cmm_mm_means_pos_neg.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ ability_mean_ten_occ skill_mean_ten_occ $xlist_0
global ivlist mm_pos_ten_occ_iv mm_neg_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage mm_neg mm_pos cmm_pos cmm_neg ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_cmm_mm_means_pos_neg.ster, replace

/*------------------------------------------------------------------------------------*/
/* individual component mismatch */

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0
xi: reg lwage absmm_?? $xlist $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/ols_ind_mm_means.ster, replace

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0
global ivlist absmm_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_mm_means.ster, replace

/*------------------------------------------------------------------------------------*/
/* individual component cumulative mismatch */

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0 
xi: reg lwage cmm_aa cmm_bb cmm_cc cmm_dd absmm_?? $xlist $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_ind_cmm_mm_means.ster, replace

global xlist  absmm_??_ten_occ ability_??_ten_occ skill_??_ten_occ $xlist_0 
global ivlist absmm_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage cmm_aa cmm_bb cmm_cc cmm_dd absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmm_mm_means.ster, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* switching probability regression */
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
/* linear probability regressions */

xi: reg fswitch_occ mm $prswlist, vce(robust)
estimate save ${result}/lpm_${diminitls}_mm.ster, replace

xi: ivregress 2sls fswitch_occ (${prswlist_x} = ${prswlist_iv}) mm $prswlist_z, vce(robust)
estimate save ${result}/iv_lpm_${diminitls}_mm.ster, replace

/*------------------------------------------------------------------------------------*/
/* lpm with positive and negative mistmatch */

xi: reg fswitch_occ mm_neg mm_pos $prswlist, vce(robust)
estimate save ${result}/lpm_${diminitls}_mm_pos_neg.ster, replace

xi: ivregress 2sls fswitch_occ (${prswlist_x} = ${prswlist_iv}) mm_neg mm_pos $prswlist_z, vce(robust)
estimate save ${result}/iv_lpm_${diminitls}_mm_pos_neg.ster, replace

/*------------------------------------------------------------------------------------*/
/* lpm by components */

xi: reg fswitch_occ absmm_?? $prswlist, vce(robust)
estimate save ${result}/lpm_${diminitls}_mm_ind.ster, replace

xi: ivregress 2sls fswitch_occ absmm_?? ($prswlist_x = ${prswlist_iv}) $prswlist_z, vce(robust)
estimate save ${result}/iv_lpm_${diminitls}_mm_ind.ster, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* switching direction */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

xtset id year
gen lmm_sgn = l.mm_sgn if switch_occ == 1
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

local llist "Verbal Math Social"
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
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* creating tables */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* mismatch */

estimate clear
estimate use ${result}/ols_mm_means.ster
estimate store ols_mm_means
estimate use ${result}/ols_cmm_mm_means.ster
estimate store ols_cmm_mm_means

estimate use ${result}/iv_mm_means.ster
estimate store iv_mm_means
estimate use ${result}/iv_cmm_mm_means.ster
estimate store iv_cmm_mm_means

/* csv */
esttab iv_mm_means iv_cmm_mm_means ols_mm_means ols_cmm_mm_means ///
                   using ${result}/table_${diminitls}.csv, b(4) ///
                   r2 nodepvars nogaps not label nonotes drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV" "IV" "OLS" "OLS") ///
		   order(mm mm_ten_occ cmm ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ) replace
  
esttab iv_mm_means iv_cmm_mm_means ols_mm_means ols_cmm_mm_means ///
                   using ${result}/table_apx_${diminitls}.csv, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable drop(_I*) ///
		   mtitles("IV" "IV" "OLS" "OLS") ///
		   order(mm mm_ten_occ cmm ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ ten* exp* oj $zlist _cons) replace
/* tex */		   
esttab iv_mm_means iv_cmm_mm_means ols_mm_means ols_cmm_mm_means ///
                   using ${result}/table_${diminitls}.tex, b(4) ///
                   r2 nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "Robust standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV" "IV" "OLS" "OLS") ///
		   order(mm mm_ten_occ cmm ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
esttab iv_mm_means iv_cmm_mm_means ols_mm_means ols_cmm_mm_means ///
                   using ${result}/table_apx_${diminitls}.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\hline  " ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   title("Wage Regression with Mismatch (Full Results)") ///
		   drop(_I*) ///
		   mtitles("IV" "IV" "OLS" "OLS") ///
		   order(mm mm_ten_occ cmm ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ ten* exp* oj $zlist _cons) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
/*------------------------------------------------------------------------------------*/
/* positive and negative mismatch */

estimate clear
estimate use ${result}/ols_mm_means_pos_neg.ster
estimate store ols_mm_means_pos_neg
estimate use ${result}/iv_mm_means_pos_neg.ster
estimate store iv_mm_means_pos_neg

estimate use ${result}/ols_cmm_mm_means_pos_neg.ster
estimate store ols_cmm_mm_means_pos_neg
estimate use ${result}/iv_cmm_mm_means_pos_neg.ster
estimate store iv_cmm_mm_means_pos_neg

esttab iv_mm_means_pos_neg iv_cmm_mm_means_pos_neg ols_mm_means_pos_neg ols_cmm_mm_means_pos_neg ///
                   using ${result}/table_${diminitls}_pos_neg.tex, b(4) ///
                   r2 nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\hline  " "Standard" "Robust standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV" "IV" "OLS" "OLS") ///
		   order(mm_pos mm_neg mm_pos*_ten_occ mm_neg*_ten_occ cmm_pos cmm_neg ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace

esttab iv_mm_means_pos_neg iv_cmm_mm_means_pos_neg ols_mm_means_pos_neg ols_cmm_mm_means_pos_neg ///
                   using ${result}/table_apx_${diminitls}_pos_neg.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   title("Wage Regression with Positive and Negative Mismatch (Full Results)") ///
		   drop(_I*) ///
		   mtitles("IV" "IV" "OLS" "OLS") ///
		   order(mm_pos mm_neg mm_pos*_ten_occ mm_neg*_ten_occ cmm_pos cmm_neg ability_mean ability_mean_ten_occ skill_mean skill_mean_ten_occ ten* exp* oj $zlist _cons) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
/*------------------------------------------------------------------------------------*/
/* individual component mismatch */

estimate clear
estimate use ${result}/ols_ind_mm_means.ster
estimate store ols_ind_mm_means
estimate use ${result}/ols_ind_cmm_mm_means.ster
estimate store ols_ind_cmm_mm_means

estimate use ${result}/iv_ind_mm_means.ster
estimate store iv_ind_mm_means
estimate use ${result}/iv_ind_cmm_mm_means.ster
estimate store iv_ind_cmm_mm_means

/* csv */
esttab iv_ind_mm_means iv_ind_cmm_mm_means ols_ind_mm_means ols_ind_cmm_mm_means ///
                   using ${result}/table_${diminitls}_ind.csv, b(4) ///
                   r2 nodepvars nogaps label not nonotes drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV" "IV" "OLS" "OLS") ///
		   order(absmm_?? absmm_??_t* cmm_?? ability_?? ability_??_* skill_?? skill_??_*) replace
	   
esttab iv_ind_mm_means iv_ind_cmm_mm_means ols_ind_mm_means ols_ind_cmm_mm_means ///
                   using ${result}/table_apx_${diminitls}_ind.csv, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable drop(_I* _cons) ///
		   mtitles("IV" "IV" "OLS" "OLS") ///
		   title("Wage Regression with Mismatch by Components (Full Results)") ///
		   order(absmm_?? absmm_??_t* cmm_?? ability_?? ability_??_* skill_?? skill_??_* ten* exp* oj $zlist) replace

/* tex */
esttab iv_ind_mm_means iv_ind_cmm_mm_means ols_ind_mm_means ols_ind_cmm_mm_means ///
                   using ${result}/table_${diminitls}_ind.tex, b(4) ///
                   r2 nodepvars gaps label not nonotes substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" "Standard" "Robust standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I* ten* exp* oj $zlist _cons) ///
		   mtitles("IV" "IV" "OLS" "OLS") ///
		   order(absmm_?? absmm_??_t* cmm_?? ability_?? ability_??_* skill_?? skill_??_*) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
esttab iv_ind_mm_means iv_ind_cmm_mm_means ols_ind_mm_means ols_ind_cmm_mm_means ///
                   using ${result}/table_apx_${diminitls}_ind.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\hline  " ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   drop(_I* _cons) ///
		   mtitles("IV" "IV" "OLS" "OLS") ///
		   title("Wage Regression with Mismatch by Components (Full Results)") ///
		   order(absmm_?? absmm_??_t* cmm_?? ability_?? ability_??_* skill_?? skill_??_* ten* exp* oj $zlist) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
/*------------------------------------------------------------------------------------*/
/* switching probability */

estimate clear
estimate use ${result}/lpm_${diminitls}_mm.ster
estimate store lpm_${diminitls}_mm
estimate use ${result}/iv_lpm_${diminitls}_mm.ster
estimate store iv_lpm_${diminitls}_mm

estimate use ${result}/lpm_${diminitls}_mm_pos_neg.ster
estimate store lpm_${diminitls}_mm_pos_neg
estimate use ${result}/iv_lpm_${diminitls}_mm_pos_neg.ster
estimate store iv_lpm_${diminitls}_mm_pos_neg

estimate use ${result}/lpm_${diminitls}_mm_ind.ster
estimate store lpm_${diminitls}_mm_ind
estimate use ${result}/iv_lpm_${diminitls}_mm_ind.ster
estimate store iv_lpm_${diminitls}_mm_ind

/* csv */
esttab iv_lpm_${diminitls}_mm iv_lpm_${diminitls}_mm_ind lpm_${diminitls}_mm lpm_${diminitls}_mm_ind ///
                   using ${result}/table_${diminitls}_switch_lpm.csv, b(4) ///
                   nodepvars nogaps not label nonotes ///
		   mtitles("LPM-IV" "LPM-IV" "LPM" "LPM") ///
		   order(mm absmm_aa absmm_bb absmm_cc ability_mean ability_mean* skill_mean skill_mean*) drop(tenure_occ ten_occ2 tenure_emp ten_emp2 exp exp2 oj $zlist _cons _I* ) replace
		   
esttab iv_lpm_${diminitls}_mm iv_lpm_${diminitls}_mm_ind lpm_${diminitls}_mm lpm_${diminitls}_mm_ind ///
                   using ${result}/table_apx_${diminitls}_switch_lpm.csv, b(4) se(4) ///
                   nodepvars nogaps label longtable ///
		   mtitles("LPM-IV" "LPM-IV" "LPM" "LPM") ///
		   title("Regressions for Probability of Occupational Switch (Full Results)") ///
		   order(mm absmm_aa absmm_bb absmm_cc ability_mean ability_mean* skill_mean skill_mean* tenure_occ ten_occ2 tenure_emp ten_emp2 exp exp2 oj $zlist) drop(_cons _I*) replace
/* tex */
esttab iv_lpm_${diminitls}_mm iv_lpm_${diminitls}_mm_ind lpm_${diminitls}_mm lpm_${diminitls}_mm_ind ///
                   using ${result}/table_${diminitls}_switch_lpm.tex, b(4) ///
                   nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" "Standard" "Robust standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   mtitles("LPM-IV" "LPM-IV" "LPM" "LPM") ///
		   order(mm absmm_?? ability_mean ability_mean* skill_mean skill_mean*) drop(tenure_occ ten_occ2 tenure_emp ten_emp2 exp exp2 oj $zlist _cons _I* ) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
esttab iv_lpm_${diminitls}_mm iv_lpm_${diminitls}_mm_ind lpm_${diminitls}_mm lpm_${diminitls}_mm_ind ///
                   using ${result}/table_apx_${diminitls}_switch_lpm.tex, b(4) se(4) ///
                   nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   mtitles("LPM-IV" "LPM-IV" "LPM" "LPM") ///
		   title("Regressions for Probability of Occupational Switch (Full Results)") ///
		   order(mm absmm_?? ability_mean ability_mean* skill_mean skill_mean* tenure_occ ten_occ2 tenure_emp ten_emp2 exp exp2 oj $zlist) drop(_cons _I*) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace
		   
esttab iv_lpm_${diminitls}_mm_pos_neg lpm_${diminitls}_mm_pos_neg ///
                   using ${result}/table_apx_${diminitls}_switch_lpm_sgn.tex, b(4) se(4) ///
                   nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   mtitles("LPM-IV" "LPM") ///
		   title("Regressions for Probability of Occupational Switch with Signed Mismatch (Full Results)") ///
		   order(mm_pos mm_neg ability_mean ability_mean* skill_mean skill_mean* tenure_occ ten_occ2 tenure_emp ten_emp2 exp exp2 oj $zlist) drop(_cons _I*) ///
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
	matrix pred_mm[1,`tt'] = _b["mm"]*`p10'+_b["mm_ten_occ"]*`p10'*`tt'*5
	matrix pred_mm[2,`tt'] = _b["mm"]*`p30'+_b["mm_ten_occ"]*`p30'*`tt'*5
	matrix pred_mm[3,`tt'] = _b["mm"]*`p50'+_b["mm_ten_occ"]*`p50'*`tt'*5
	matrix pred_mm[4,`tt'] = _b["mm"]*`p70'+_b["mm_ten_occ"]*`p70'*`tt'*5
	matrix pred_mm[5,`tt'] = _b["mm"]*`p90'+_b["mm_ten_occ"]*`p90'*`tt'*5
}

_pctile cmm, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)

matrix pred_mm[1,4] = _b["cmm"]*`p10'
matrix pred_mm[2,4] = _b["cmm"]*`p30'
matrix pred_mm[3,4] = _b["cmm"]*`p50'
matrix pred_mm[4,4] = _b["cmm"]*`p70'
matrix pred_mm[5,4] = _b["cmm"]*`p90'

matrix colnames pred_mm = "MM_5_Years" "MM_10_Years" "MM_15_Years" "Cumul_MM"
matrix rownames pred_mm = "10_Percentile" "30_Percentile" "50_Percentile" "70_Percentile" "90_Percentile"
putexcel A1=matrix(pred_mm, names) using ${result}/table_${diminitls}_pred_mm.xls, replace

/*------------------------------------------------------------------------------------*/
/* table for the effect of mismatch */

estimate clear
estimate use ${result}/iv_lpm_${diminitls}_mm.ster
matrix pred_switch = J(5,5,0.0)

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

matrix colnames pred_switch = "Mismatch" "Verbal" "Math" "Social"
matrix rownames pred_switch = "90_Percentile" "70_Percentile" "50_Percentile" "30_Percentile" "10_Percentile"
putexcel A1=matrix(pred_switch, names) using ${result}/table_${diminitls}_pred_switch.xls, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* plots!!*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* switch rate by mismatch */
twoway (lpolyci fswitch_occ mm_sgn, clcolor(blue) clwidth(thick)), ///
ytitle("Probability of Occupational Switch", size(medlarge)) ///
xtitle("Last Signed Mismatch", size(medlarge)) ///
title("Probability of Occupational Switch", size(large)) ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(0.14(0.04)0.26, grid gstyle(dot)) ///
xline(0, lcolor(red) lpattern(solid)) ///
saving(${result}/fig_pswitch_${diminitls}_mm, replace)
graph export ${result}/fig_pswitch_${diminitls}_mm.eps, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* switching direction */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

estimate clear
estimate use ${result}/mm_${diminitls}_where_pos_neg.ster
estimate store mm_${diminitls}_where_pos_neg
estimate use ${result}/mm_${diminitls}_where_aa_pos_neg.ster
estimate store mm_${diminitls}_where_aa_pos_neg
estimate use ${result}/mm_${diminitls}_where_bb_pos_neg.ster
estimate store mm_${diminitls}_where_bb_pos_neg
estimate use ${result}/mm_${diminitls}_where_cc_pos_neg.ster
estimate store mm_${diminitls}_where_cc_pos_neg
/* csv */
esttab mm_${diminitls}_where_pos_neg mm_${diminitls}_where_aa_pos_neg mm_${diminitls}_where_bb_pos_neg mm_${diminitls}_where_cc_pos_neg ///
                   using ${result}/table_${diminitls}_mm_where.csv, b(4) ///
                   r2 nodepvars nogaps not label nonotes ///
		   mtitles("Average of All" "Verbal" "Math" "Social") ///
		   order(lmm_sgn_pos lmm_sgn_neg lmm_aa_pos lmm_aa_neg lmm_bb_pos lmm_bb_neg lmm_cc_pos lmm_cc_neg) drop(lten* lexp* loj $zlist _cons _I*) replace

esttab mm_${diminitls}_where_pos_neg mm_${diminitls}_where_aa_pos_neg mm_${diminitls}_where_bb_pos_neg mm_${diminitls}_where_cc_pos_neg ///
                   using ${result}/table_apx_${diminitls}_mm_where.csv, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable ///
		   mtitles("Average of All" "Verbal" "Math" "Social") ///
		   title("Regressions for Direction of Occupational Switch (Full Results)") ///
		   order(lmm_sgn_pos lmm_sgn_neg lmm_aa_pos lmm_aa_neg lmm_bb_pos lmm_bb_neg lmm_cc_pos lmm_cc_neg lten* lexp* loj $zlist) drop(_cons _I*) replace
/* tex */	   
esttab mm_${diminitls}_where_pos_neg mm_${diminitls}_where_aa_pos_neg mm_${diminitls}_where_bb_pos_neg mm_${diminitls}_where_cc_pos_neg ///
                   using ${result}/table_${diminitls}_mm_where.tex, b(4) ///
                   r2 nodepvars gaps not label nonotes substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" "Standard" "Robust standard" ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   mtitles("Average of All" "Verbal" "Math" "Social") ///
		   order(lmm_sgn_pos lmm_sgn_neg lmm_aa_pos lmm_aa_neg lmm_bb_pos lmm_bb_neg lmm_cc_pos lmm_cc_neg) drop(lten* lexp* loj $zlist _cons _I*) ///
                   star(\sym{\dagger} 0.10 \sym{*} 0.05 \sym{**} 0.01) replace

esttab mm_${diminitls}_where_pos_neg mm_${diminitls}_where_aa_pos_neg mm_${diminitls}_where_bb_pos_neg mm_${diminitls}_where_cc_pos_neg ///
                   using ${result}/table_apx_${diminitls}_mm_where.tex, b(4) se(4) ///
                   r2 nodepvars nogaps label longtable substitute(\hline\hline \hline \hline "\noalign{\vskip 1mm} \hline \noalign{\vskip 1mm}" ///
		   "Standard errors in parentheses" "All regressions include occupation and industry dummies." ///
		   "\sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)" "Robust standard errors in parentheses. \sym{\sym{\dagger}} \(p<0.10\), \sym{\sym{*}} \(p<0.05\), \sym{\sym{**}} \(p<0.01\)." ///
                   "\sym{\sym{\dagger}}" "$^{\dagger}$" "\sym{\sym{*}}" "$^{*}$" "\sym{\sym{**}}" "$^{**}$") ///
		   mtitles("Average of All" "Verbal" "Math" "Social") ///
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

matrix colnames mat_chng_skill = "Verbal" "Math" "Social"
matrix rownames mat_chng_skill = "All" "Less_than_High_School" "High_School" "Some_College"
putexcel A1=matrix(mat_chng_skill, names) using ${result}/table_${diminitls}_chng_skill.xls, replace

/*------------------------------------------------------------------------------------*/

estimate clear
estimate use ${result}/mm_${diminitls}_where_pos_neg.ster
matrix pred_where = J(6,5,0.0)

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

matrix colnames pred_where = "All_Average" "Verbal" "Math" "Social"
matrix rownames pred_where = "90_Pos" "50_Pos" "10_Pos" "90_Neg" "50_Neg" "10_Neg"
putexcel A1=matrix(pred_where, names) using ${result}/table_${diminitls}_pred_where.xls, replace

/*------------------------------------------------------------------------------------*/
/* plot the change in skills */

graph twoway (lpolyci chng_skill lmm_sgn if switch_occ==1, clcolor(blue) clpattern(solid) clwidth(thick)), ///
ytitle("Average Change in Skill", size(medlarge)) ///
xtitle("Last Signed Mismatch", size(medlarge)) ///
title("Direction of Switch, All Average", size(large)) ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(-0.3(0.15)0.3, grid gstyle(dot)) ///
xline(0, lcolor(red) lpattern(solid)) ///
saving(${result}/fig_where_${diminitls}_mm, replace)
graph export ${result}/fig_where_${diminitls}_mm.eps, replace

local llist "Verbal Math Social"
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

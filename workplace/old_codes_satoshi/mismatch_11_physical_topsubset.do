/*--------------------------------------------------------------------------------------
* name: mismatch_07_corr_regression.do
* made by: david wiczer, moidfied by: satoshi tanaka
* date: 08/21/2010
*       03/18/2014
* description: this code is for the project 'occupation skill mismatch'
--------------------------------------------------------------------------------------*/
/* this code runs the regressions */ 

/*------------------------------------------------------------------------------------*/
*global result = "H:\David\Occupation_mismatch"
use $result/yearly_02.dta, clear
set more off
stop
/*--------------------------*/
* DANIEL: subset here on the second pass
/*--------------------------*/
//keep if age < 35
//keep if age >= 35

global sample_select =.
global hlth_comp_use = 1
global match_centered = 0
global two_skills = 1
/*
*  stdize = 1 makes the min 0 and standard deviation 1
*  stdize = 2 uses ranks
*/
global stdize = 2

/* generate some  other covariates */

gen hs = (grade>=12)
gen lths = (grade<12)
gen univ = (grade>=16)
gen hispanic = (race == 1)
gen black = (race == 2)

//keep if univ
//keep if !univ

label var hs	"High School"
label var univ	"4-Year College"
label var hispanic	"Hispanic"
label var black	"Black"

bysort id: egen grade_m = max(grade) if age<=30
bysort id: egen grade_30 = max(grade_m)
drop grade_m

/*
gen residwage = .
// adjusted because there are no observations for age < 35 past 1998
//forvalues yr = 1980/2010{
forvalues yr = 1980/1998 {
	qui reg lwage univ lths hispanic black i.ind_1d i.occ_1d if year ==`yr'
	predict tmp
	replace residwage=tmp if year == `yr'
	drop tmp
}
*/
/*------------------------------------------------------------------------------------*/

/* define higher order terms for tunure variables */

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
label var oj	 "Old Job"

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

/* clean the age effects from tests by means: */
gen tmp = age -(year-1980) 
bysort id: egen tmp2 = min(tmp)
qui tab tmp2, gen(I_enterage)

/* make the standard deviation the same?*/
qui forvalues i=1/7{
	sum asvab_sec`i' if tmp2== 20
	local sd20 = r(sd)
	forvalues ai=16/24{
		sum asvab_sec`i' if tmp2 ==`ai'
		replace asvab_sec`i' = asvab_sec`i'/r(sd)*`sd20' if tmp2 ==`ai'
	}
}

/* take out the mean */

forvalues i=1/7{
	qui reg asvab_sec`i' I_enterage1-I_enterage8
	qui predict asvab_res`i', residual 
	qui replace asvab_res`i' =asvab_res`i' +_b["_cons"]
}

/* normalize everything to have standard deviation of 1 */
forvalues i=1/7{
	qui sum asvab_res`i'
	qui gen asvab_std`i' = asvab_res`i' /r(sd)
}

/*------------------------------------------------------------------------------------*/

/* create weights */
sort id
gen ones = 1
egen swt = total(ones), by(id)
replace swt = 1/swt

/*------------------------------------------------------------------------------------*/

/* cognitive skill */

forvalues s = 1/7 {
	qui sum ONET_ASVAB_`s'
	replace ONET_ASVAB_`s' = ONET_ASVAB_`s'/r(sd)
}

pca ONET_ASVAB_1-ONET_ASVAB_4, components(1)
predict skill_1, score
sum skill_1 if skill_1 != .
replace skill_1 = (skill_1 - r(min))/r(sd)  if skill_1 != . != .

/* mechanic skill */

pca ONET_ASVAB_6-ONET_ASVAB_7, components(1)
predict skill_2, score
sum skill_2 if skill_2 != .
replace skill_2 = (skill_2 - r(min))/r(sd)  if skill_2 != . != .

/* health skill */

forvalues s = 22/40 {
	qui sum ONET_`s'
	replace ONET_`s' = ONET_`s'/r(sd)
}

pca ONET_22-ONET_40, components(1)
predict skill_3, score
sum skill_3 if skill_3 != .
replace skill_3 = (skill_3 - r(min))/r(sd)  if skill_3 != . != .

/*------------------------------------------------------------------------------------*/

/* cognitive ability */

pca asvab_sec1-asvab_sec4, components(1)
predict ability_1, score
sum ability_1 if ability_1 != .
replace ability_1 = (ability_1 - r(min))/r(sd)  if ability_1 != . != .

/* mechanic ability */

pca asvab_sec6-asvab_sec7, components(1)
predict ability_2, score
sum ability_2 if ability_2 != .
replace ability_2 = (ability_2 - r(min))/r(sd)  if ability_2 != . != .

/* physical ability */

forvalues i=1/9 {
	replace hlth_score_0`i'=. if hlth_score_0`i'<0
	replace hlth_score_0`i'= 5 - hlth_score_0`i'
	qui sum hlth_score_0`i'
	replace hlth_score_0`i' = hlth_score_0`i'/r(sd)
}
forvalues i=10/12 {
	replace hlth_score_`i'=. if hlth_score_`i'<0
	replace hlth_score_`i'= 5 - hlth_score_`i'
	qui sum hlth_score_`i'
	replace hlth_score_`i' = hlth_score_`i'/r(sd)
}

pca hlth_score_*, components(1)
predict ability_3, score
sum ability_3 if ability_3 != .
replace ability_3 = (ability_3 - r(min))/r(sd)  if ability_3 != .

if($hlth_comp_use ==1){
	replace hlth_composite = . if hlth_composite < 0
	sum hlth_composite if hlth_composite != .
	replace ability_3 = (hlth_composite - r(min))/r(sd)  if hlth_composite != .
}



/*------------------------------------------------------------------------------------*/
/* generate mismatch measure */
/*------------------------------------------------------------------------------------*/

bysort occ: gen occ_indic=_n==1
by occ	: egen occ_wt = total(ones)

forvalues i=1/3{
	cumul ability_`i' [aw = swt], gen(ability_rnk_`i') equal
	cumul skill_`i' if occ_indic ==1 [aw = occ_wt], gen(skill_rnk_i) equal
	bysort occ: egen skill_rnk_`i' = max(skill_rnk_i)
	drop skill_rnk_i
}
forvalues i=1/3{
	gen mm_`i' = ability_rnk_`i' - skill_rnk_`i'
	gen absmm_`i' = abs(mm_`i')
}

pca absmm_?
estat loadings, cnorm(unit)
matrix L = e(L)
gen mm = 0
/* The negative and positive components separated */
gen mm_neg = 0
gen mm_pos = 0

local Lwt = 0
forvalues i=1/3{
	local Lwt = `Lwt' +  L[`i',1]
	replace mm = mm + absmm_`i'*L[`i',1]
	replace mm_neg = mm_neg + mm_`i'*L[`i',1] if mm_`i' <0
	replace mm_pos = mm_pos + mm_`i'*L[`i',1] if mm_`i' >0
}
/* Split between people with average negative or positive mismatch*/
gen mm_aveneg = mm_neg if abs(mm_neg) > mm_pos
replace mm_aveneg = 0 if abs(mm_neg) < mm_pos
gen mm_avepos = mm_pos if abs(mm_neg) < mm_pos
replace mm_avepos = 0 if abs(mm_neg) > mm_pos

replace mm = mm/`Lwt'
replace mm_neg = mm_neg/`Lwt'
replace mm_pos = mm_pos/`Lwt'
replace mm_aveneg = mm_aveneg/`Lwt'
replace mm_avepos = mm_avepos/`Lwt'


* Do the 2-dimensional
pca absmm_1 absmm_2
estat loadings, cnorm(unit)
matrix L = e(L)
gen mm2 = 0
gen mm2_neg = 0
gen mm2_pos = 0
local Lwt = 0
forvalues i=1/2{
	local Lwt = `Lwt' + L[`i',1]
	replace mm2 = mm2+ absmm_`i'*L[`i',1]
	replace mm2_neg = mm2_neg + mm_`i'*L[`i',1] if mm_`i' <0
	replace mm2_pos = mm2_pos + mm_`i'*L[`i',1] if mm_`i' >0
}
replace mm2 = mm2/`Lwt'
replace mm2_neg = mm2_neg/`Lwt'
replace mm2_pos = mm2_pos/`Lwt'

gen mm2_aveneg = mm2_neg if abs(mm2_neg) > mm2_pos
replace mm2_aveneg = 0 if abs(mm2_neg) < mm2_pos
gen mm2_avepos = mm2_pos if abs(mm2_neg) < mm2_pos
replace mm2_avepos = 0 if abs(mm2_neg) > mm2_pos
if($two_skills ==1){
	rename mm mm3
	rename mm_neg mm3_neg
	rename mm_pos mm3_pos
	rename mm_aveneg mm3_aveneg
	rename mm_avepos mm3_avepos

	rename mm2 mm
	rename mm2_neg mm_neg
	rename mm2_pos mm_pos
	rename mm2_aveneg mm_aveneg
	rename mm2_avepos mm_avepos
}



gen mm_ten_occ = mm*tenure_occ
gen mm_ten_occ2 = mm*ten_occ2
gen mm_ten_occ_iv = mm*ten_occ_iv
gen mm_ten_occ2_iv = mm*ten_occ2_iv
forvalues i=1/3{
	gen mm_`i'_ten_occ = mm_`i'*tenure_occ
	gen mm_`i'_ten_occ_iv = mm_`i'*ten_occ_iv
	label var mm_`i' "Mismatch `i'"	
	label var mm_`i'_ten_occ "Mismatch `i' $\times$ Occ Tenure"
}
gen mm_neg_ten_occ = mm_neg*tenure_occ
gen mm_neg_ten_occ_iv = mm_neg*ten_occ_iv
label var mm_neg "Mismatch - Negative Components"	
label var mm_neg_ten_occ "Mismatch Negative $\times$ Occ Tenure"
gen mm_pos_ten_occ = mm_pos*tenure_occ
gen mm_pos_ten_occ_iv = mm_pos*ten_occ_iv
label var mm_pos "Mismatch Positive Components"	
label var mm_pos_ten_occ "Mismatch Positive $\times$ Occ Tenure"

gen mm_aveneg_ten_occ = mm_aveneg*tenure_occ
gen mm_aveneg_ten_occ_iv = mm_aveneg*ten_occ_iv
label var mm_aveneg "Mismatch Average Negative "	
label var mm_aveneg_ten_occ "Mismatch Negative $\times$ Occ Tenure"
gen mm_avepos_ten_occ = mm_avepos*tenure_occ
gen mm_avepos_ten_occ_iv = mm_avepos*ten_occ_iv
label var mm_avepos "Mismatch Average Positive"	
label var mm_avepos_ten_occ "Mismatch Positive $\times$ Occ Tenure"


* drop the top and bottom 1%
* replace mm = . if mm>0.816 | mm<0.0343

/*------------------------------------------------------------------------------------*/
/* creating a correlation measure */
/*------------------------------------------------------------------------------------*/
if( $stdize==2 ){
	forvalues i=1/3{
		rename	ability_`i' ability_std_`i'
		rename	skill_`i' skill_std_`i'
	}
	forvalues i=1/3{
		rename	ability_rnk_`i' ability_`i'
		rename	skill_rnk_`i' skill_`i'
	}
}


gen ability_m = (ability_1 + ability_2 + ability_3)/3
gen skill_m  = (skill_1 + skill_2 + skill_3)/3
egen ability_sd = rowsd(ability_1 ability_2 ability_3)
egen skill_sd = rowsd(skill_1 skill_2 skill_3)
gen ability_m2 = (ability_1 + ability_2)/2
gen skill_m2  = (skill_1 + skill_2)/2
egen ability_sd2 = rowsd(ability_1 ability_2)
egen skill_sd2 = rowsd(skill_1 skill_2)
gen match = 0
forvalues i = 1/3 {
	if($match_centered == 1){
		gen match_`i' = (ability_`i' - ability_m)*(skill_`i' - skill_m)
		replace match = match + 1/3*match_`i'
		
	}
	else{
		gen match_`i' = ability_`i'*skill_`i'
		replace match = match + 1/3*match_`i'
	}
}

*replace match = match/ability_sd/skill_sd


if($match_centered==1){
	gen match2 = ( (ability_1 - ability_m2)*(skill_1 - skill_m2) + (ability_2 - ability_m2)*(skill_2 - skill_m2) )/2
}
else{
	gen match2 = ( ability_1*skill_1 + ability_2*skill_2 )/2
}
*replace match2 = match2/asvab_sd2/ONET_sd2

if($two_skills ==1){
	rename match match3
	rename match2 match
}


sort match
quietly gen rsum = sum(swt)
quietly sum rsum, meanonly
quietly gen match_rnk = rsum/r(max)
*replace match = match_rnk

drop rsum match_rnk

gen ability_skill_m = ability_m*skill_m
gen ability_skill_m2 = ability_m2*skill_m2

if($two_skills==1){
	rename ability_m ability_m3
	rename ability_m2 ability_m
	rename ability_skill_m ability_skill_m3
	rename ability_skill_m2 ability_skill_m
}

/*------------------------------------------------------------------------------------*/
/* create cumulative measures */
/*------------------------------------------------------------------------------------*/


xtset id year
gen lmm = l.mm if id == l.id & switch_occ==1
replace lmm = l.lmm if switch_occ==0
gen lmatch = l.match if id == l.id & switch_occ==1
replace lmatch  = l.lmatch if switch_occ==0

gen lmm_neg = l.mm_neg if id == l.id & switch_occ==1
replace lmm_neg = l.lmm_neg if switch_occ==0
gen lmm_pos = l.mm_pos if id == l.id & switch_occ==1
replace lmm_pos = l.lmm_pos if switch_occ==0
gen lmm_aveneg = l.mm_aveneg if id == l.id & switch_occ==1
replace lmm_aveneg = l.lmm_aveneg if switch_occ==0
gen lmm_avepos = l.mm_avepos if id == l.id & switch_occ==1
replace lmm_avepos = l.lmm_avepos if switch_occ==0



gen lexp = l.tenure_occ if id == l.id & switch_occ==1
replace lexp = l.exp if switch_occ==0

gsort +id -switch_occ +year
by id: gen cmatch_exp = sum(lmatch*lexp) if switch_occ==1  
by id: gen cmm_exp = sum(lmm*lexp)  if switch_occ==1 

by id: gen cmm_neg_exp = sum(lmm_neg*lexp)  if switch_occ==1 
by id: gen cmm_pos_exp = sum(lmm_pos*lexp)  if switch_occ==1 
by id: gen cmm_aveneg_exp = sum(lmm_aveneg*lexp)  if switch_occ==1 
by id: gen cmm_avepos_exp = sum(lmm_avepos*lexp)  if switch_occ==1 

by id: gen totexp = sum(lexp) if switch_occ==1
replace cmatch_exp = cmatch_exp/totexp if switch_occ==1 
replace cmm_exp = cmm_exp/totexp if switch_occ==1 
replace cmm_neg_exp = cmm_neg_exp/totexp if switch_occ==1 
replace cmm_pos_exp = cmm_pos_exp/totexp if switch_occ==1 
replace cmm_aveneg_exp = cmm_aveneg_exp/totexp if switch_occ==1 
replace cmm_avepos_exp = cmm_avepos_exp/totexp if switch_occ==1 


xtset id year
replace cmatch_exp = l.cmatch_exp if switch_occ==0 & match<.
replace cmm_exp = l.cmm_exp if switch_occ==0 & mm<.

replace cmm_neg_exp = l.cmm_neg_exp if switch_occ==0 & match<.
replace cmm_pos_exp = l.cmm_pos_exp if switch_occ==0 & mm<.
replace cmm_aveneg_exp = l.cmm_aveneg_exp if switch_occ==0 & match<.
replace cmm_avepos_exp = l.cmm_avepos_exp if switch_occ==0 & mm<.


/* residual mismatch and match*/
reg match lths univ hispanic black ability_m skill_m ability_skill_m [aw= swt]
predict match_resid, resid
reg mm lths univ hispanic black ability_m skill_m ability_skill_m [aw= swt]
predict mm_resid, resid


/* asvab and onet interaction */

gen ability_skill_m_ten_occ = ability_skill_m*tenure_occ/100
gen ability_skill_m_ten_occ2 = ability_skill_m*ten_occ2
gen ability_skill_m_ten_occ_iv = ability_skill_m*ten_occ_iv/100
gen ability_skill_m_ten_occ2_iv = ability_skill_m*ten_occ2_iv

label var ability_skill_m "ASVAB $\times$ ONET $\times$ 100"
label var ability_skill_m_ten_occ "ASVAB $\times$ ONET $\times$ Occ Tenure"
label var ability_skill_m_ten_occ2 "ASVAB $\times$ ONET $\times$ Occ Tenure$^2 \times$ 100"

gen ability_m_ten_occ = ability_m*tenure_occ
gen ability_m_ten_occ2 = ability_m*ten_occ2
gen ability_m_ten_occ_iv = ability_m*ten_occ_iv
gen ability_m_ten_occ2_iv = ability_m*ten_occ2_iv
gen skill_m_ten_occ = skill_m*tenure_occ
gen skill_m_ten_occ2 = skill_m*ten_occ2
gen skill_m_ten_occ_iv = skill_m*ten_occ_iv
gen skill_m_ten_occ2_iv = skill_m*ten_occ2_iv

label var ability_m "Mean ASVAB"
label var skill_m "Mean ONET"
label var ability_m_ten_occ "ASVAB $\times$ Occ Tenure"
label var ability_m_ten_occ2 "ASVAB $\times$ Occ Tenure$^2 \times$ 100"
label var skill_m_ten_occ "ONET $\times$ Occ Tenure"
label var skill_m_ten_occ2 "ONET $\times$ Occ Tenure$^2 \times$ 100"


gen match_ten_occ = match*tenure_occ
gen match_ten_occ2 = match*ten_occ2
gen match_ten_occ_iv = match*ten_occ_iv
gen match_ten_occ2_iv = match*ten_occ2_iv
forvalues i=1/3{
	gen match_`i'_ten_occ = match_`i'*tenure_occ
	gen match_`i'_ten_occ_iv = match_`i'*ten_occ_iv
}



save $result/yearly_03_phys.dta, replace

/*--------------------------*/
* DANIEL: subset here on the first pass
/*--------------------------*/


/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* estimation */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/



global zlist hs univ hispanic black 
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv

/* benchmark ols regression */
log using $result/regressions, replace
use $result/yearly_03_phys.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
display "********** REGRESSSION 1 **********"
xi: reg lwage $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/bench_ols.ster, replace
*outreg, bdec(3) tex ctitle("", "Benchmark") drop(_I* $zlist) varlabels se replace fragment
outreg2 using $result/apxtable_corrols.tex, bdec(3) tex(fragment) ctitle("", "Benchmark") drop(_I* ) label se replace sortvar( $xlist)

/*------------------------------------------------------------------------------------*/
/* Match quality regressions */
/*------------------------------------------------------------------------------------*/

/* ols with cum match + match + (asvab +onet + asvab_X_onet)(1+tenure)  */

use $result/yearly_03_phys.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
display "********** REGRESSION 2 **********"
xi: reg lwage match match_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_cq_q_asvab_onet.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(1)") fragment drop(_I* $zlist)  varlabels se replace
outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons  replace
outreg2 using $result/apxtable_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se replace

// order: coefficient, t-stat
// match, match_ten_occ
matrix reg2 = (_b[match], _b[match]/_se[match], _b[match_ten_occ], _b[match_ten_occ]/_se[match_ten_occ])

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* benchmarnk iv regression (Altonji and Shakotko) */

use $result/yearly_03_phys.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv
display "********** REGRESSION 3 **********"
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)

/*------------------------------------------------------------------------------------*/
/* Match quality IV reqressions*/
/*------------------------------------------------------------------------------------*/


/* IV with match + cum match + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */
display "********** REGRESSION 4 **********"
use $result/yearly_03_phys.dta, clear
keep if $sample_select
xtset id year
global xlist match_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj ability_m_ten_occ skill_m_ten_occ ability_skill_m_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match_ten_occ_iv ability_m_ten_occ_iv skill_m_ten_occ_iv ability_skill_m_ten_occ_iv
xi: ivregress 2sls lwage match cmatch_exp ($xlist = $ivlist) $zlist ability_m skill_m ability_skill_m i.ind_1d i.occ_1d, vce(robust)

// match, match_ten_occ, cmatch_exp, ability_m, skill_m, ability_m_ten_occ, skill_m_ten_occ
matrix reg4 = (	_b[match], _b[match]/_se[match], _b[match_ten_occ], _b[match_ten_occ]/_se[match_ten_occ], _b[cmatch_exp], _b[cmatch_exp]/_se[cmatch_exp], ///
				_b[ability_m], _b[ability_m]/_se[ability_m], _b[skill_m], _b[skill_m]/_se[skill_m], _b[ability_m_ten_occ], _b[ability_m_ten_occ]/_se[ability_m_ten_occ], ///
				_b[skill_m_ten_occ], _b[skill_m_ten_occ]/_se[skill_m_ten_occ])

xtset id year
global xlist mm_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj  ability_m_ten_occ skill_m_ten_occ ability_skill_m_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_ten_occ_iv ability_m_ten_occ_iv skill_m_ten_occ_iv ability_skill_m_ten_occ_iv
display "********** REGRESSION 5 **********"
xi: ivregress 2sls lwage mm cmm_exp ($xlist = $ivlist) $zlist ability_m skill_m ability_skill_m  i.ind_1d i.occ_1d, vce(robust)

// mm, cmm_exp, mm_ten_occ, ability_m, skill_m, ability_m_ten_occ, skill_m_ten_occ
matrix reg5 = ( _b[mm], _b[mm]/_se[mm], _b[cmm_exp], _b[cmm_exp]/_se[cmm_exp], _b[mm_ten_occ], _b[mm_ten_occ]/_se[mm_ten_occ], _b[ability_m], _b[ability_m]/_se[ability_m], ///
				_b[skill_m], _b[skill_m]/_se[skill_m], _b[ability_m_ten_occ], _b[ability_m_ten_occ]/_se[ability_m_ten_occ], ///
				_b[skill_m_ten_occ], _se[skill_m_ten_occ])

xtset id year
global xlist match_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj ability_m_ten_occ skill_m_ten_occ ability_skill_m_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match_ten_occ_iv ability_m_ten_occ_iv skill_m_ten_occ_iv ability_skill_m_ten_occ_iv
display "********** REGRESSION 6 **********"
xi: ivregress 2sls lwage match ($xlist = $ivlist) $zlist ability_m skill_m ability_skill_m i.ind_1d i.occ_1d, vce(robust)

// match, match_ten_occ, ability_m, skill_m, ability_m_ten_occ, skill_m_ten_occ
matrix reg6 = (	_b[match], _b[match]/_se[match], _b[match_ten_occ], _b[match_ten_occ]/_se[match_ten_occ], ///
				_b[ability_m], _b[ability_m]/_se[ability_m], _b[skill_m], _b[skill_m]/_se[skill_m], _b[ability_m_ten_occ], _b[ability_m_ten_occ]/_se[ability_m_ten_occ], ///
				_b[skill_m_ten_occ], _b[skill_m_ten_occ]/_se[skill_m_ten_occ])

xtset id year
global xlist mm_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj  ability_m_ten_occ skill_m_ten_occ ability_skill_m_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_ten_occ_iv ability_m_ten_occ_iv skill_m_ten_occ_iv ability_skill_m_ten_occ_iv
display "********** REGRESSION 7 **********"
xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist ability_m skill_m ability_skill_m  i.ind_1d i.occ_1d, vce(robust)

// mm, mm_ten_occ, ability_m, skill_m, ability_m_ten_occ, skill_m_ten_occ
matrix reg7 = ( _b[mm], _b[mm]/_se[mm], _b[mm_ten_occ], _b[mm_ten_occ]/_se[mm_ten_occ], _b[ability_m], _b[ability_m]/_se[ability_m], ///
				_b[skill_m], _b[skill_m]/_se[skill_m], _b[ability_m_ten_occ], _b[ability_m_ten_occ]/_se[ability_m_ten_occ], ///
				_b[skill_m_ten_occ], _se[skill_m_ten_occ])

xtset id year
global xlist match_?_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj ability_m_ten_occ skill_m_ten_occ ability_skill_m_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match_?_ten_occ_iv ability_m_ten_occ_iv skill_m_ten_occ_iv ability_skill_m_ten_occ_iv
display "********** REGRESSION 8 **********"
xi: ivregress 2sls lwage match_? ($xlist = $ivlist) $zlist ability_m skill_m ability_skill_m i.ind_1d i.occ_1d, vce(robust)

// ability_m, skill_m, ability_m_ten_occ, skill_m_ten_occ
matrix reg8 = ( _b[ability_m], _b[ability_m]/_se[ability_m], ///
				_b[skill_m], _b[skill_m]/_se[skill_m], _b[ability_m_ten_occ], _b[ability_m_ten_occ]/_se[ability_m_ten_occ], ///
				_b[skill_m_ten_occ], _se[skill_m_ten_occ])

xtset id year
global xlist mm_?_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj  ability_m_ten_occ skill_m_ten_occ ability_skill_m_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_?_ten_occ_iv ability_m_ten_occ_iv skill_m_ten_occ_iv ability_skill_m_ten_occ_iv
display "********** REGRESSION 9 **********"
xi: ivregress 2sls lwage absmm_? ($xlist = $ivlist) $zlist ability_m skill_m ability_skill_m  i.ind_1d i.occ_1d, vce(robust)

// absmm_1, absmm_2, absmm_3, ability_m, skill_m, ability_m_ten_occ, skill_m_ten_occ
matrix reg9 = ( _b[absmm_1], _b[absmm_1]/_se[absmm_1], _b[absmm_2], _b[absmm_2]/_se[absmm_2], _b[absmm_3], _b[absmm_3]/_se[absmm_3], ///
				_b[ability_m], _b[ability_m]/_se[ability_m], _b[skill_m], _b[skill_m]/_se[skill_m], _b[ability_m_ten_occ], _b[ability_m_ten_occ]/_se[ability_m_ten_occ], ///
				_b[skill_m_ten_occ], _se[skill_m_ten_occ])
				
/* These "signed" mismatch measures don't work in a wage regression
xtset id year
global xlist mm_aveneg_ten_occ mm_avepos_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj  ability_m_ten_occ skill_m_ten_occ ability_skill_m_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_aveneg_ten_occ_iv mm_avepos_ten_occ_iv ability_m_ten_occ_iv skill_m_ten_occ_iv ability_skill_m_ten_occ_iv
xi: ivregress 2sls lwage mm_aveneg mm_avepos cmm_aveneg_exp cmm_avepos_exp ($xlist = $ivlist) $zlist ability_m skill_m ability_skill_m  i.ind_1d i.occ_1d, vce(robust)

xtset id year
global xlist mm_neg_ten_occ mm_pos_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj  ability_m_ten_occ skill_m_ten_occ ability_skill_m_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_neg_ten_occ_iv mm_pos_ten_occ_iv ability_m_ten_occ_iv skill_m_ten_occ_iv ability_skill_m_ten_occ_iv
xi: ivregress 2sls lwage mm_neg mm_pos cmm_neg_exp cmm_pos_exp ($xlist = $ivlist) $zlist ability_m skill_m ability_skill_m  i.ind_1d i.occ_1d, vce(robust)


xtset id year
global xlist mm_aveneg_ten_occ mm_avepos_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj  ability_m_ten_occ skill_m_ten_occ ability_skill_m_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_aveneg_ten_occ_iv mm_avepos_ten_occ_iv ability_m_ten_occ_iv skill_m_ten_occ_iv ability_skill_m_ten_occ_iv
xi: xtivreg lwage mm_aveneg mm_avepos cmm_aveneg_exp cmm_avepos_exp ($xlist = $ivlist) $zlist ability_m skill_m ability_skill_m  i.ind_1d i.occ_1d

xtset id year
global xlist mm_neg_ten_occ mm_pos_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj  ability_m_ten_occ skill_m_ten_occ ability_skill_m_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_neg_ten_occ_iv mm_pos_ten_occ_iv ability_m_ten_occ_iv skill_m_ten_occ_iv ability_skill_m_ten_occ_iv
xi: xtivreg lwage mm_neg mm_pos cmm_neg_exp cmm_pos_exp ($xlist = $ivlist) $zlist ability_m skill_m ability_skill_m  i.ind_1d i.occ_1d, fe
*/

display "********** REGRESSION 10 **********"
probit switch_occ mm_neg mm_pos ability_m age age2 $zlist i.occ_1d  i.ind_1d

// ability_m, mm_neg, mm_pos
matrix reg10 = ( _b[ability_m], _b[ability_m]/_se[ability_m], _b[mm_neg], _b[mm_neg]/_se[mm_neg], _b[mm_pos], _b[mm_pos]/_se[mm_pos])

display "********** REGRESSION 11 **********"
probit switch_occ mm ability_m age age2 $zlist i.occ_1d  i.ind_1d

// mm, ability_m
matrix reg11 = ( _b[mm], _b[mm]/_se[mm], _b[ability_m], _b[ability_m]/_se[ability_m])

display "********** REGRESSION 12 **********"
probit switch_occ match ability_m skill_m age age2 $zlist i.occ_1d  i.ind_1d

// match, abiltiy_m, skill_m
matrix reg12 = ( _b[match], _b[match]/_se[match], _b[ability_m], _b[ability_m]/_se[ability_m], _b[skill_m], _b[skill_m]/_se[skill_m])

display "********** XTPROBIT REGRESSIONS **********"
display "********** REGRESSION 13 **********"
xtprobit switch_occ mm_neg mm_pos ability_m age age2 $zlist i.occ_1d  i.ind_1d

// mm_neg, mm_pos, ability_m
matrix reg13 = ( _b[mm_neg], _b[mm_neg]/_se[mm_neg], _b[mm_pos], _b[mm_pos]/_se[mm_pos], _b[ability_m], _b[ability_m]/_se[ability_m])

display "********** REGRESSION 14 **********" 
xtprobit switch_occ mm ability_m age age2 $zlist i.occ_1d  i.ind_1d

// mm, ability_m
matrix reg14 = ( _b[mm], _b[mm]/_se[mm], _b[ability_m], _b[ability_m]/_se[ability_m])

display "********** REGRESSION 15 **********"
xtprobit switch_occ match ability_m skill_m age age2 $zlist i.occ_1d  i.ind_1d

// match, ability_m, skill_m
matrix reg15 = ( _b[match], _b[match]/_se[match], _b[ability_m], _b[ability_m]/_se[ability_m], _b[skill_m], _b[skill_m]/_se[skill_m])

log close

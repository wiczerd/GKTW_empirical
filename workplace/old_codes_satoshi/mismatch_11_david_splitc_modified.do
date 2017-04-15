/*--------------------------------------------------------------------------------------
* name: mismatch_07_corr_regression.do
* made by: david wiczer, moidfied by: satoshi tanaka
* date: 08/21/2010
*       03/18/2014
* description: this code is for the project 'occupation skill mismatch'
--------------------------------------------------------------------------------------*/

/* this code runs the regressions */ 

/*------------------------------------------------------------------------------------*/

use $result/yearly_02.dta, clear

/*--------------------------*/
 * DANIEL: subset here on the second pass
/*--------------------------*/

global sample_select =.
global hlth_comp_use = 1
global match_centered = 0
/* put a 1 in only one of the global variables */
/* two-dimensional measures */
global cogmech	= 0
global cogphys	= 0
global totphys	= 0
global verbmath	= 1
/* three-dimensional measures */
global verbmathphys= 0
global cogmechphys = 0

if($cogmech == 1 | $cogphys == 1 | $totphys == 1 | $verbmath == 1){
	global twodim = 1
	global threedim = 0
}
else{
	global twodim = 0
	global threedim = 1
}

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

label var hs	"High School"
label var univ	"4-Year College"
label var hispanic	"Hispanic"
label var black	"Black"

bysort id: egen grade_m = max(grade) if age<=30
bysort id: egen grade_30 = max(grade_m)
drop grade_m

gen residwage = .
forvalues yr = 1980/2010{
	qui reg lwage univ lths hispanic black i.ind_1d i.occ_1d if year ==`yr'
	predict tmp
	replace residwage=tmp if year == `yr'
	drop tmp
}

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


/* verbal skills */
pca ONET_ASVAB_3-ONET_ASVAB_4, components(1)
predict skill_v, score
sum skill_v if skill_v != .
replace skill_v = (skill_v - r(min))/r(sd)  if skill_v != . != .
/* math skills */
pca ONET_ASVAB_1-ONET_ASVAB_2, components(1)
predict skill_m, score
sum skill_m if skill_m != .
replace skill_m = (skill_m - r(min))/r(sd)  if skill_m != . != .

/* total skills */
pca ONET_ASVAB_1-ONET_ASVAB_7, components(1)
predict skill_0, score
sum skill_0 if skill_0 != .
replace skill_0 = (skill_0 - r(min))/r(sd)  if skill_0 != . != .

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

/* verbal ability */
pca asvab_std3-asvab_std4, components(1)
predict ability_v, score
sum ability_v if ability_v != .
replace ability_v = (ability_v - r(min))/r(sd)  if ability_v != . != .
/* math ability */
pca asvab_std1-asvab_std2, components(1)
predict ability_m, score
sum ability_m if ability_m != .
replace ability_m = (ability_m - r(min))/r(sd)  if ability_m != . != .


/* total ability */
pca asvab_std1-asvab_std7, components(1)
predict ability_0, score
sum ability_0 if ability_0 != .
replace ability_0 = (ability_0 - r(min))/r(sd)  if ability_0 != . != .

/* cognitive ability */
pca asvab_std1-asvab_std4, components(1)
predict ability_1, score
sum ability_1 if ability_1 != .
replace ability_1 = (ability_1 - r(min))/r(sd)  if ability_1 != . != .

/* mechanic ability */

pca asvab_std6-asvab_std7, components(1)
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

forvalues i=0/3{
	cumul ability_`i' [aw = swt], gen(ability_rnk_`i') equal
	cumul skill_`i' if occ_indic ==1 [aw = occ_wt], gen(skill_rnk_i) equal
	bysort occ: egen skill_rnk_`i' = max(skill_rnk_i)
	drop skill_rnk_i
	gen mm_`i' = ability_rnk_`i' - skill_rnk_`i'
	gen absmm_`i' = abs(mm_`i')	
}


foreach i in "v" "m"{
	cumul ability_`i' [aw = swt], gen(ability_rnk_`i') equal
	cumul skill_`i' if occ_indic ==1 [aw = occ_wt], gen(skill_rnk_i) equal
	bysort occ: egen skill_rnk_`i' = max(skill_rnk_i)
	drop skill_rnk_i
	gen mm_`i' = ability_rnk_`i' - skill_rnk_`i'
	gen absmm_`i' = abs(mm_`i')
}


/* Use the components we need, given the dimensions we are analyzing */

/*three dimensions: */
if($verbmathphys == 1){
	gen absmm_aa = absmm_v
	gen absmm_bb = absmm_m
	gen absmm_cc = absmm_3
	gen mm_aa = mm_v
	gen mm_bb = mm_m
	gen mm_cc = mm_3
}
if($cogmechphys == 1){ 
	gen absmm_aa = absmm_1
	gen absmm_bb = absmm_2
	gen absmm_cc = absmm_3
	gen mm_aa = mm_1
	gen mm_bb = mm_2
	gen mm_cc = mm_3
}


* Do the 2-dimensional
if($cogphys == 1){
	gen absmm_aa = absmm_1 
	gen absmm_bb = absmm_3
	gen mm_aa = mm_1 
	gen mm_bb = mm_3
}
if($cogmech == 1){
	gen absmm_aa = absmm_1 
	gen absmm_bb = absmm_2
	gen mm_aa = mm_1 
	gen mm_bb = mm_2

}
if($totphys == 1){
	gen absmm_aa = absmm_0 
	gen absmm_bb = absmm_3
	gen mm_aa = mm_0 
	gen mm_bb = mm_3	
}
if($verbmath == 1){
	gen absmm_aa = absmm_v 
	gen absmm_bb = absmm_m
	gen mm_aa = mm_v 
	gen mm_bb = mm_m	
}

gen mm = 0
/* The negative and positive components separated */
gen mm_neg = 0
gen mm_pos = 0

local Lwt = 0

pca absmm_??
if($threedim == 1){
	*pca absmm_aa absmm_bb absmm_cc
	estat loadings, cnorm(unit)
	matrix L = e(L)
	local Li = 1
	foreach i in  "aa" "bb" "cc"{
		local Lwt = `Lwt' +  L[`Li',1]
		replace mm = mm + absmm_`i'*L[`Li',1]
		replace mm_neg = mm_neg + mm_`i'*L[`Li',1] if mm_`i' <0
		replace mm_pos = mm_pos + mm_`i'*L[`Li',1] if mm_`i' >0
		local Li = `Li' + 1
	}
}
else{
	*pca absmm_aa absmm_bb
	estat loadings, cnorm(unit)
	matrix L = e(L)
	local Li = 1
	foreach i in  "aa" "bb"{
		local Lwt = `Lwt' +  L[`Li',1]
		replace mm = mm + absmm_`i'*L[`Li',1]
		replace mm_neg = mm_neg + mm_`i'*L[`Li',1] if mm_`i' <0
		replace mm_pos = mm_pos + mm_`i'*L[`Li',1] if mm_`i' >0
		local Li = `Li' + 1
	}

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

gen mm_ten_occ = mm*tenure_occ
gen mm_ten_occ2 = mm*ten_occ2
gen mm_ten_occ_iv = mm*ten_occ_iv
gen mm_ten_occ2_iv = mm*ten_occ2_iv

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
	forvalues i=0/3{
		rename	ability_`i' ability_std_`i'
		rename	skill_`i' skill_std_`i'
	}
	forvalues i=0/3{
		rename	ability_rnk_`i' ability_`i'
		rename	skill_rnk_`i' skill_`i'
	}
	foreach i in "v" "m"{
		rename	ability_`i' ability_std_`i'
		rename	skill_`i' skill_std_`i'
	}
	foreach i in "v" "m"{
		rename	ability_rnk_`i' ability_`i'
		rename	skill_rnk_`i' skill_`i'
	}
	
}

if($cogmechphys == 1){
	gen ability_aa = ability_1
	gen ability_bb = ability_2
	gen ability_cc = ability_3
	gen skill_aa = skill_1
	gen skill_bb = skill_2
	gen skill_cc = skill_3	
}
if($verbmathphys == 1){
	gen ability_aa = ability_v
	gen ability_bb = ability_m
	gen ability_cc = ability_3
	gen skill_aa = skill_v
	gen skill_bb = skill_m
	gen skill_cc = skill_3

}
if($verbmath == 1){
	gen ability_aa = ability_v
	gen ability_bb = ability_m
	gen skill_aa = skill_v
	gen skill_bb = skill_m	
}
if($cogphys == 1){
	gen ability_aa = ability_1
	gen ability_bb = ability_3
	gen skill_aa = skill_1
	gen skill_bb = skill_3	
}
if($cogmech == 1){
	gen ability_aa = ability_1
	gen ability_bb = ability_2
	gen skill_aa = skill_1
	gen skill_bb = skill_2
}
if($totphys == 1){
	gen ability_aa = ability_0
	gen ability_bb = ability_3
	gen skill_aa = skill_0
	gen skill_bb = skill_3
}



if($twodim == 1){
	gen ability_mean = (ability_aa + ability_bb )/3
	gen skill_mean  = (skill_aa + skill_bb )/3
	egen ability_stdev = rowsd(ability_aa ability_bb )
	egen skill_stdev = rowsd(skill_aa skill_bb )
}
else{
	gen ability_mean = (ability_aa + ability_bb + ability_cc)/3
	gen skill_mean  = (skill_aa + skill_bb + skill_cc)/3
	egen ability_stdev = rowsd(ability_aa ability_bb ability_cc)
	egen skill_stdev = rowsd(skill_aa skill_bb skill_cc)
}



gen match = 0

if( $threedim == 1 ){
	foreach i in "aa" "bb" "cc" {
		if($match_centered == 1){
			gen match_`i' = (ability_`i' - ability_mean)*(skill_`i' - skill_mean)
			replace match = match + 1/3*match_`i'
			
		}
		else{
			gen match_`i' = ability_`i'*skill_`i'
			replace match = match + 1/3*match_`i'
		}
	}
}
*replace match = match/ability_stdev/skill_stdev

if( $twodim ==1 ){
	foreach i in "aa" "bb" {
		if($match_centered == 1){
			gen match_`i' = (ability_`i' - ability_mean)*(skill_`i' - skill_mean)
			replace match = match + 1/3*match_`i'
			
		}
		else{
			gen match_`i' = ability_`i'*skill_`i'
			replace match = match + 1/3*match_`i'
		}
	}
}

sort match
quietly gen rsum = sum(swt)
quietly sum rsum, meanonly
quietly gen match_rnk = rsum/r(max)
*replace match = match_rnk

drop rsum match_rnk

gen ability_skill_mean = ability_mean*skill_mean
gen ability_skill_mean2 = ability_mean*skill_mean


/*------------------------------------------------------------------------------------*/
/* create cumulative measures */
/*------------------------------------------------------------------------------------*/


sort id year
gen cmatch_cur = 0
gen cmm_cur = 0
replace cmatch_cur = match if id != id[_n-1]
replace cmm_cur = mm if id != id[_n-1]
replace cmatch_cur = cmatch_cur[_n-1] + match if id == id[_n-1]
replace cmm_cur = cmm_cur[_n-1] + mm if id == id[_n-1]

replace cmatch_cur = cmatch_cur/exp
replace cmm_cur = cmm_cur/exp


/* residual mismatch and match*/
reg match lths univ hispanic black ability_mean skill_mean ability_skill_mean [aw= swt]
predict match_resid, resid
reg mm lths univ hispanic black ability_mean skill_mean ability_skill_mean [aw= swt]
predict mm_resid, resid


/* asvab and onet interaction */

gen ability_skill_mean_ten_occ = ability_skill_mean*tenure_occ/100
gen ability_skill_mean_ten_occ2 = ability_skill_mean*ten_occ2
gen ability_skill_mean_ten_occ_iv = ability_skill_mean*ten_occ_iv/100
gen ability_skill_mean_ten_occ2_iv = ability_skill_mean*ten_occ2_iv

label var ability_skill_mean "ASVAB $\times$ ONET $\times$ 100"
label var ability_skill_mean_ten_occ "ASVAB $\times$ ONET $\times$ Occ Tenure"
label var ability_skill_mean_ten_occ2 "ASVAB $\times$ ONET $\times$ Occ Tenure$^2 \times$ 100"

gen ability_mean_ten_occ = ability_mean*tenure_occ
gen ability_mean_ten_occ2 = ability_mean*ten_occ2
gen ability_mean_ten_occ_iv = ability_mean*ten_occ_iv
gen ability_mean_ten_occ2_iv = ability_mean*ten_occ2_iv
gen skill_mean_ten_occ = skill_mean*tenure_occ
gen skill_mean_ten_occ2 = skill_mean*ten_occ2
gen skill_mean_ten_occ_iv = skill_mean*ten_occ_iv
gen skill_mean_ten_occ2_iv = skill_mean*ten_occ2_iv

label var ability_mean "Mean ASVAB"
label var skill_mean "Mean ONET"
label var ability_mean_ten_occ "ASVAB $\times$ Occ Tenure"
label var ability_mean_ten_occ2 "ASVAB $\times$ Occ Tenure$^2 \times$ 100"
label var skill_mean_ten_occ "ONET $\times$ Occ Tenure"
label var skill_mean_ten_occ2 "ONET $\times$ Occ Tenure$^2 \times$ 100"

gen match_ten_occ = match*tenure_occ
gen match_ten_occ2 = match*ten_occ2
gen match_ten_occ_iv = match*ten_occ_iv
gen match_ten_occ2_iv = match*ten_occ2_iv

gen cmatch_cur_exp = cmatch_cur*exp
gen cmm_cur_exp = cmm_cur*exp
gen cmatch_cur_exp_iv = cmatch_cur*exp_iv
gen cmm_cur_exp_iv = cmm_cur*exp_iv

if($twodim == 1){
	foreach i in "aa" "bb"{
		gen match_`i'_ten_occ = match_`i'*tenure_occ
		gen match_`i'_ten_occ_iv = match_`i'*ten_occ_iv
		gen ability_`i'_ten_occ = ability_`i'*tenure_occ
		gen ability_`i'_ten_occ_iv = ability_`i'*ten_occ_iv
		gen skill_`i'_ten_occ = skill_`i'*tenure_occ
		gen skill_`i'_ten_occ_iv = skill_`i'*ten_occ_iv

		gen absmm_`i'_ten_occ = absmm_`i'*tenure_occ
		gen absmm_`i'_ten_occ_iv = absmm_`i'*ten_occ_iv
	}
}
else{
	foreach i in "aa" "bb" "cc"{
		gen match_`i'_ten_occ = match_`i'*tenure_occ
		gen match_`i'_ten_occ_iv = match_`i'*ten_occ_iv
		gen ability_`i'_ten_occ = ability_`i'*tenure_occ
		gen ability_`i'_ten_occ_iv = ability_`i'*ten_occ_iv
		gen skill_`i'_ten_occ = skill_`i'*tenure_occ
		gen skill_`i'_ten_occ_iv = skill_`i'*ten_occ_iv	
		gen absmm_`i'_ten_occ = absmm_`i'*tenure_occ
		gen absmm_`i'_ten_occ_iv = absmm_`i'*ten_occ_iv
	}
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

use $result/yearly_03_phys.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/bench_ols.ster, replace
*outreg, bdec(3) tex ctitle("", "Benchmark") drop(_I* $zlist) varlabels se replace fragment
outreg2 using $result/apxtable_corrols.tex, bdec(3) tex(fragment) ctitle("", "Benchmark") drop(_I* ) label se replace sortvar( $xlist)

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* benchmarnk iv regression (Altonji and Shakotko) */

use $result/yearly_03_phys.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)

/*------------------------------------------------------------------------------------*/
/* Match quality IV reqressions*/
/*------------------------------------------------------------------------------------*/


/* IV with match + cum match + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_phys.dta, clear
keep if $sample_select
xtset id year
global xlist match_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj ability_mean_ten_occ skill_mean_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv
xi: ivregress 2sls lwage match ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)

xtset id year
global xlist mm_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj  ability_mean_ten_occ skill_mean_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv
xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)

xtset id year
global xlist match_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj ability_mean_ten_occ skill_mean_ten_occ ability_skill_mean_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage match ($xlist = $ivlist) $zlist ability_mean skill_mean ability_skill_mean i.ind_1d i.occ_1d , vce(robust)

xtset id year
global xlist mm_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj  ability_mean_ten_occ skill_mean_ten_occ ability_skill_mean_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist ability_mean skill_mean ability_skill_mean  i.ind_1d i.occ_1d, vce(robust)

xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj ability_mean_ten_occ skill_mean_ten_occ cmatch_cur_exp
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv cmatch_cur_exp_iv
xi: ivregress 2sls lwage cmatch_cur ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)

xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj ability_mean_ten_occ skill_mean_ten_occ cmm_cur_exp
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv cmm_cur_exp_iv
xi: ivregress 2sls lwage cmm_cur ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)

xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj ability_mean_ten_occ skill_mean_ten_occ ability_skill_mean_ten_occ cmatch_cur_exp
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv ability_skill_mean_ten_occ_iv cmatch_cur_exp_iv
xi: ivregress 2sls lwage cmatch_cur ($xlist = $ivlist) $zlist ability_mean skill_mean ability_skill_mean i.ind_1d i.occ_1d , vce(robust)

xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj ability_mean_ten_occ skill_mean_ten_occ ability_skill_mean_ten_occ cmm_cur_exp
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv ability_skill_mean_ten_occ_iv cmm_cur_exp_iv
xi: ivregress 2sls lwage cmm_cur ($xlist = $ivlist) $zlist ability_mean skill_mean ability_skill_mean  i.ind_1d i.occ_1d, vce(robust)

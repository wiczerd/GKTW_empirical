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

global sample_select =.

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
	qui reg lwage univ lths hispanic black i.ind i.occ if year ==`yr'
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

/* machanic ability */

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

pca hlth_score*, components(1)
predict ability_3, score
sum ability_3 if ability_3 != .
replace ability_3 = (ability_3 - r(min))/r(sd)  if ability_3 != .

/*------------------------------------------------------------------------------------*/
/* creating a correlation measure */

gen ability_m = (ability_1 + ability_2 + ability_3)/3
gen skill_m  = (skill_1 + skill_2 + skill_3)/3
egen ability_sd = rowsd(ability_1 ability_2 ability_3)
egen skill_sd = rowsd(skill_1 skill_2 skill_3)
*gen ability_m = (ability_1 + ability_2)/2
*gen skill_m  = (skill_1 + skill_2)/2
*egen ability_sd = rowsd(ability_1 ability_2)
*egen skill_sd = rowsd(skill_1 skill_2)
gen match = 0
forvalues i = 1/3 {
replace match = match + 1/3*(ability_`i' - ability_m)*(skill_`i' - skill_m)
*replace match = match + 1/3*ability_`i'*skill_`i'
}
replace match = log(match)
*replace match = match/ability_sd/skill_sd

sort match
quietly gen rsum = sum(swt)
quietly sum rsum, meanonly
quietly gen match_rnk = rsum/r(max)
*replace match = match_rnk

drop rsum match_rnk

gen ability_skill_m = ability_m*skill_m

/*
/* asvab and onet interaction */

gen ASVAB_ONET_X = asvab_m*ONET_m

gen ASVAB_ONET_X_ten_occ = ASVAB_ONET_X*tenure_occ/100
gen ASVAB_ONET_X_ten_occ2 = ASVAB_ONET_X*ten_occ2
gen ASVAB_ONET_X_ten_occ_iv = ASVAB_ONET_X*ten_occ_iv/100
gen ASVAB_ONET_X_ten_occ2_iv = ASVAB_ONET_X*ten_occ2_iv

label var ASVAB_ONET_X "ASVAB $\times$ ONET $\times$ 100"
label var ASVAB_ONET_X_ten_occ "ASVAB $\times$ ONET $\times$ Occ Tenure"
label var ASVAB_ONET_X_ten_occ2 "ASVAB $\times$ ONET $\times$ Occ Tenure$^2 \times$ 100"

gen asvab_m_ten_occ = asvab_m*tenure_occ
gen asvab_m_ten_occ2 = asvab_m*ten_occ2
gen asvab_m_ten_occ_iv = asvab_m*ten_occ_iv
gen asvab_m_ten_occ2_iv = asvab_m*ten_occ2_iv
gen ONET_m_ten_occ = ONET_m*tenure_occ
gen ONET_m_ten_occ2 = ONET_m*ten_occ2
gen ONET_m_ten_occ_iv = ONET_m*ten_occ_iv
gen ONET_m_ten_occ2_iv = ONET_m*ten_occ2_iv

label var asvab_m "Mean ASVAB"
label var ONET_m "Mean ONET"
label var asvab_m_ten_occ "ASVAB $\times$ Occ Tenure"
label var asvab_m_ten_occ2 "ASVAB $\times$ Occ Tenure$^2 \times$ 100"
label var ONET_m_ten_occ "ONET $\times$ Occ Tenure"
label var ONET_m_ten_occ2 "ONET $\times$ Occ Tenure$^2 \times$ 100"
*/

gen match_ten_occ = match*tenure_occ
gen match_ten_occ2 = match*ten_occ2
gen match_ten_occ_iv = match*ten_occ_iv
gen match_ten_occ2_iv = match*ten_occ2_iv

save $result/yearly_03_corr.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* estimation */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

global zlist hs univ hispanic black AFQT
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv

/* benchmark ols regression */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/bench_ols.ster, replace
*outreg, bdec(3) tex ctitle("", "Benchmark") drop(_I* $zlist) varlabels se replace fragment
outreg2 using $result/apxtable_corrols.tex, bdec(3) tex(fragment) ctitle("", "Benchmark") drop(_I* ) label se replace sortvar( $xlist)

/*------------------------------------------------------------------------------------*/
/* Match quality regressions */
/*------------------------------------------------------------------------------------*/

/* ols with cum match + match + (asvab +onet + asvab_X_onet)(1+tenure)  */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage match match_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_cq_q_asvab_onet.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(1)") fragment drop(_I* $zlist)  varlabels se replace
outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons  replace
outreg2 using $result/apxtable_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* benchmarnk iv regression (Altonji and Shakotko) */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv

xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save $result/bench_iv.ster, replace
*outreg, tex fragment bdec(3) ctitle("", "Benchmark") drop(_I* $zlist )  varlabels se
outreg2 using $result/apxtable_corriv.tex, bdec(3) tex(fragment) ctitle("", "Benchmark") ///
	drop(_I* ) label se replace sortvar( $xlist)

/*------------------------------------------------------------------------------------*/
/* Match quality IV reqressions*/
/*------------------------------------------------------------------------------------*/


/* IV with match + cum match + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist match_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match_ten_occ_iv
xi: ivregress 2sls lwage match ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_cq_q_asvab_onet.ster, replace
*outreg , tex merge bdec(3) ctitle("", "(3)") drop(_I* $zlist ) fragment varlabels se replace
outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ten* exp* oj $zlist) label se nocons replace
outreg2 using $result/apxtable_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ) label se replace

/*--------------------------------------------------------------------------------------
* name: mismatch_07_corr_regression.do
* made by: david wiczer, moidfied by: satoshi tanaka
* date: 08/21/2010
*       07/26/2014
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
global zlist hs univ hispanic black

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

global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv

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

* create AFQT (1980 basis) scores:
gen AFQT = asvab_sec02 + asvab_sec03 + asvab_sec04 + asvab_sec05/2

rename asvab_sec01 asvab_sec1
rename asvab_sec02 asvab_sec2
rename asvab_sec03 asvab_sec3
rename asvab_sec04 asvab_sec4
rename asvab_sec05 asvab_sec5
rename asvab_sec06 asvab_sec6
rename asvab_sec07 asvab_sec7
rename asvab_sec08 asvab_sec8
rename asvab_sec09 asvab_sec9

/* clean the age effects from tests by means: */
gen tmp = age -(year-1980) 
bysort id: egen tmp2 = min(tmp)
qui tab tmp2, gen(I_enterage)

forvalues i=1/10{
	qui reg asvab_sec`i' I_enterage1-I_enterage8
	qui predict asvab_res`i', residual 
	qui replace asvab_res`i' =asvab_res`i' +_b["_cons"]
}

/* make the standard deviation the same?*/
qui forvalues i=1/10{
	sum asvab_res`i' if tmp2== 20
	local sd20 = r(sd)
	forvalues ai=16/24{
		sum asvab_res`i' if tmp2 ==`ai'
		replace asvab_res`i' = asvab_res`i'/r(sd)*`sd20' if tmp2 ==`ai'
	}
}
* normalize everything to have standard deviation of 1
forvalues i=1/10{
	qui sum asvab_res`i'
	qui gen asvab_std`i' = asvab_res`i' /r(sd)
}


/*------------------------------------------------------------------------------------*/

sort id
gen ones = 1
egen swt = total(ones), by(id)
replace swt = 1/swt

forvalues s = 1/120 {
	qui sum ONET_`s'
	replace ONET_`s' = ONET_`s'/r(sd)
}

pca ONET_1-ONET_21, components(1)
predict onet_brain, score
sort onet_brain
gen rsum = sum(ones) if onet_brain != .
sum rsum if onet_brain != ., meanonly
gen onet_brain_rnk = rsum/r(max)  if onet_brain != .
drop rsum

pca ONET_22-ONET_40, components(1)
predict onet_brawn, score
sort onet_brawn
gen rsum = sum(ones)  if onet_brawn != .
sum rsum if onet_brawn != ., meanonly
gen onet_brawn_rnk = rsum/r(max)  if onet_brain != .
drop rsum

pca asvab_std1-asvab_std10, components(1)
predict asvab_brain, score
sort asvab_brain
gen rsum = sum(swt)  if asvab_brain != .
sum rsum if asvab_brain != ., meanonly
gen asvab_brain_rnk = rsum/r(max)  if asvab_brain != .
drop rsum

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
predict asvab_brawn, score
replace hlth_composite = . if hlth_composite<.
*gen asvab_brawn = hlth_composite
sum asvab_brawn
replace asvab_brawn = asvab_brawn/r(sd)
sort asvab_brawn
gen rsum = sum(swt) if asvab_brawn != .
sum rsum if asvab_brawn != ., meanonly
gen asvab_brawn_rnk = rsum/r(max)  if asvab_brawn != .
drop rsum

/*------------------------------------------------------------------------------------*/
/* creating a correlation measure */

gen asvab_m = (asvab_brain + asvab_brawn)/2
*gen asvab_m = asvab_brawn
gen ONET_m  = (onet_brain + onet_brawn)/2
*gen ONET_m  = onet_brawn
egen asvab_sd = rowsd(asvab_brain asvab_brawn)
egen ONET_sd = rowsd(onet_brain onet_brawn)
gen match = 1/2*(asvab_brain - asvab_m)*(onet_brain - ONET_m) + 1/2*(asvab_brawn - asvab_m)*(onet_brawn - ONET_m)
gen mm = 1/2*abs(asvab_brain_rnk - onet_brain_rnk) + 1/2*abs(asvab_brawn_rnk - onet_brawn_rnk)
gen match1 = (asvab_brain)*(onet_brain)
gen match2 = (asvab_brawn)*(onet_brawn)
qui replace match = match/asvab_sd/ONET_sd

sort match1
quietly gen rsum = sum(swt)
quietly sum rsum, meanonly
quietly gen match1_rnk = rsum/r(max)
*replace match1 = match1_rnk

drop rsum match1_rnk

sort match2
quietly gen rsum = sum(swt)
quietly sum rsum, meanonly
quietly gen match2_rnk = rsum/r(max)
*replace match2 = match2_rnk

drop rsum match2_rnk


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

gen match1_ten_occ = match1*tenure_occ
gen match1_ten_occ2 = match1*ten_occ2
gen match1_ten_occ_iv = match1*ten_occ_iv
gen match1_ten_occ2_iv = match1*ten_occ2_iv
gen match2_ten_occ = match2*tenure_occ
gen match2_ten_occ2 = match2*ten_occ2
gen match2_ten_occ_iv = match2*ten_occ_iv
gen match2_ten_occ2_iv = match2*ten_occ2_iv

gen match_ten_occ = match*tenure_occ
gen match_ten_occ2 = match*ten_occ2
gen match_ten_occ_iv = match*ten_occ_iv
gen match_ten_occ2_iv = match*ten_occ2_iv
gen mm_ten_occ = mm*tenure_occ
gen mm_ten_occ2 = mm*ten_occ2
gen mm_ten_occ_iv = mm*ten_occ_iv
gen mm_ten_occ2_iv = mm*ten_occ2_iv

save $result/yearly_03_corr.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* estimation */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

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
xi: reg lwage match1 match1_ten_occ match2 match2_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
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
global xlist match1_ten_occ match2_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match1_ten_occ_iv match2_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage match1 match2 asvab_m ONET_m ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_cq_q_asvab_onet.ster, replace
*outreg , tex merge bdec(3) ctitle("", "(3)") drop(_I* $zlist ) fragment varlabels se replace
outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ten* exp* oj $zlist) label se nocons replace
outreg2 using $result/apxtable_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ) label se replace

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

global sample_select .==.


global log_match = 0


/*stdize_match Determines how to transform the scores: 
1: Raw scores 
2: Log of elements
3: Rank scores
4: Transformed ONET & ASVAB to AFQT scale
5: Transformed ASVAB to AFQT scale
6: Unit standard deviation
7: Unit std dev of log
8: transformed to 0-1 scale (divided by max value)
*/

global stdize_match = 3


/* take out age effects:
1: remove age-specific mean
2: remove age-specific mean & standard-dev
3: each age has exactly the same distribution (matching quantiles)
*/

global rm_age = 2

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* setup variables */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* Merge in health data*/
merge 1:1 id year using $result/health_nlsy79.dta , gen(_merge_health)
drop if _merge_health ==2


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
label var oj	"Old Job"

/*------------------------------------------------------------------------------------*/
/* combining ASVAB */

* create AFQT (1980 basis) scores:
gen AFQT = asvab_sec02 + asvab_sec03 + asvab_sec04 + asvab_sec05/2

* these are not mapped to ONET scores
drop asvab_sec05 asvab_sec06 asvab_sec07

* renumber them
rename asvab_sec01 asvab_sec05
rename asvab_sec02 asvab_sec01
rename asvab_sec08 asvab_sec02
rename asvab_sec09 asvab_sec06
rename asvab_sec10 asvab_sec07

/* clean the age effects from tests by means: */
gen tmp = age -(year-1980) 
bysort id: egen tmp2 = min(tmp)
qui tab tmp2, gen(I_enterage)

if $rm_age == 2{
	/* make the standard deviation the same?*/
	qui forvalues i=1/7{
		sum asvab_sec0`i' if tmp2== 20
		local sd20 = r(sd)
		forvalues ai=16/24{
			sum asvab_sec0`i' if tmp2 ==`ai'
			replace asvab_sec0`i' = asvab_sec0`i'/r(sd)*`sd20' if tmp2 ==`ai'
		}
	}


}
/* take out the mean */

forvalues i=1/7{
	qui reg asvab_sec0`i' I_enterage1-I_enterage8 /* I_enterage1-I_enterage8 */
	qui predict asvab_res0`i', residual 
	qui replace asvab_res0`i' =asvab_res0`i' +_b["_cons"]
}

/* create tests' percentile scores */
sort id
gen ones = 1
egen swt = total(ones), by(id)
drop ones
replace swt = 1/swt
quietly forvalues i = 1/7 {
	/* the built-in for the CDF/ranks
	cumul asvab_res0`i' [aw=swt] , gen(asvab_rnk`i') equal
	*/
	sort asvab_res0`i' id
	gen rsum = sum(swt)
	sum rsum, meanonly
	gen asvab_rnk`i' = rsum/r(max)
	gsort -rsum
	replace asvab_rnk`i' = asvab_rnk`i'[_n-1] if asvab_res0`i' == asvab_res0`i'[_n-1]
	drop rsum
}

drop tmp tmp2

qui reg AFQT I_enterage1-I_enterage8
qui predict AFQT_res, residual
qui sum AFQT, meanonly
qui replace AFQT_res = AFQT_res + r(mean)


gen sum_asvab = 0
forvalues i = 1/7 {
	qui sum asvab_res0`i', meanonly
        quietly replace sum_asvab = sum_asvab + asvab_res0`i'/r(max)
}

sort sum_asvab id
quietly gen rsum = sum(swt)
quietly sum rsum, meanonly
quietly gen sum_asvab_rnk = rsum/r(max)
quietly gsort -rsum
quietly replace sum_asvab_rnk = sum_asvab_rnk[_n-1] if sum_asvab == sum_asvab[_n-1]
drop rsum

/* create ONET_sum and _rnk */
gen ones = 1
bysort occ: gen occ_indic=_n==1
by occ	: egen occ_wt = total(ones)
forvalues i=1/7{
	qui sum ONET_ASVAB_`i'
	gen ONET_std`i' = (ONET_ASVAB_`i')/r(sd)
	gen ONET_maxscl`i' = ONET_ASVAB_`i'/r(max)
	gen ONET_log`i' = log(ONET_ASVAB_`i'*6)
	qui sum ONET_log`i'
	gen ONET_stdlog`i' = ONET_log`i'/r(sd)
/*	this is the built-in to make ranks
	cumul ONET_ASVAB_`i' [aw=occ_wt], gen(ONET_rnk_`i') equal */
	gsort -occ_indic +ONET_ASVAB_`i'
*	qui gen rsum = sum(ones) if occ_indic==1
	qui gen rsum = sum(occ_wt) if occ_indic==1
	qui sum rsum if occ_indic==1, meanonly
	gen ONET_rnk_`i' = rsum/r(max) if occ_indic==1
	bysort occ: egen occ_rnk = max(ONET_rnk_`i')
	qui replace ONET_rnk_`i' = occ_rnk
	drop rsum occ_rnk
}

gen sum_ONET = 0
forvalues i=1/7{
        *qui replace sum_ONET = sum_ONET + ONET_rnk_`i'
	qui sum ONET_ASVAB_`i', meanonly
	qui replace sum_ONET = sum_ONET + ONET_ASVAB_`i'/r(max)
}

gsort -occ_indic +sum_ONET
*quietly gen rsum = sum(ones) if occ_indic==1
quietly gen rsum = sum(occ_wt) if occ_indic==1
quietly sum rsum if occ_indic==1, meanonly
quietly gen sum_ONET_rnk = rsum/r(max)  if occ_indic==1
bysort occ: egen occ_rnk = max(sum_ONET_rnk)
qui replace sum_ONET_rnk = occ_rnk
drop rsum occ_rnk ones


gen asvab_ten_occ = sum_asvab_rnk*tenure_occ
gen asvab_ten_occ2 = sum_asvab_rnk*ten_occ2
gen ONET_ten_occ = sum_ONET_rnk*tenure_occ
gen ONET_ten_occ2 = sum_ONET_rnk*ten_occ2
gen asvab_ten_occ_iv = sum_asvab_rnk*ten_occ_iv
gen asvab_ten_occ2_iv = sum_asvab_rnk*ten_occ2_iv
gen ONET_ten_occ_iv = sum_ONET_rnk*ten_occ_iv
gen ONET_ten_occ2_iv = sum_ONET_rnk*ten_occ2_iv

label var sum_asvab_rnk	"ASVAB Score"
label var asvab_ten_occ "ASVAB $\times$ Occ Tenure"
label var sum_ONET_rnk "ONET Score"
label var ONET_ten_occ "ONET $\times$ Occ Tenure"
label var asvab_ten_occ2 "ASVAB $\times$ Occ Tenure$^2 \times$ 100"
label var ONET_ten_occ2 "ONET $\times$ Occ Tenure$^2 \times$ 100"

/* summary stats for ASVAB and ONET */
matrix sum_ASVAB_ONET = J(4,2,0.0)
qui sum sum_asvab [aw= swt], detail
matrix sum_ASVAB_ONET[3,1] = (r(p90)-2*r(p50)+r(p10))/(r(p90)-r(p10))
matrix sum_ASVAB_ONET[1,1] = r(mean)
matrix sum_ASVAB_ONET[2,1] = r(sd)
qui sum sum_ONET [aw= swt], detail
matrix sum_ASVAB_ONET[3,2] = (r(p90)-2*r(p50)+r(p10))/(r(p90)-r(p10))
matrix sum_ASVAB_ONET[1,2] = r(mean)
matrix sum_ASVAB_ONET[2,2] = r(sd)
pca asvab_res0?
pca ONET_ASVAB_?


/*------------------------------------------------------------------------------------*/
/* creating a correlation measure */


/*  USE RANK SCORES HERE: 	*/
egen asvab_m = rowmean(asvab_rnk?)
egen ONET_m = rowmean(ONET_rnk_?)
egen asvab_sd = rowsd(asvab_rnk?)
egen ONET_sd = rowsd(ONET_rnk_?)
gen match = 0
forvalues i = 1/7 {
	quietly replace match = match + (asvab_rnk`i' - asvab_m)*(ONET_rnk_`i'-ONET_m)/7.
}

qui replace match = match

/* use a log transform on the right? */
if($log_match ==1){
	qui replace match = log(match)
	qui replace asvab_m = log(asvab_m )
	qui replace ONET_m = log(ONET_m )
}

sort match
quietly gen rsum = sum(swt)
quietly sum rsum, meanonly
quietly gen match_rnk = rsum/r(max)

/* standardize ONET_m, ASVAB_m */
qui sum match
local sdm = r(sd)
qui sum ONET_m
replace ONET_m = ONET_m/r(sd)*`sdm'
qui sum asvab_m
replace asvab_m = asvab_m/r(sd)*`sdm'

/* Some summary stats */
sum match [aw= swt], detail
disp "Kelley's Measure"
disp (r(p90)-2*r(p50)+r(p10))/(r(p90)-r(p10))
disp "Mean"
disp r(mean)
disp "Standard Deivation"
disp r(sd)

kdensity match [aw=swt], ///
ytitle("Density") ///
xtitle("Match Quality Score") ///
title("Density of the Match Quality Score") ///
saving($result/q_corrmatch_density, replace)
graph export $result/q_corrmatch_density.eps, replace

*replace match = match_rnk
drop rsum match_rnk

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

gen match_ten_occ = match*tenure_occ
gen match_ten_occ2 = match*ten_occ2
gen match_ten_occ_iv = match*ten_occ_iv
gen match_ten_occ2_iv = match*ten_occ2_iv

label var match "Match"
label var match_ten_occ "Match $\times$ Occ Tenure"
label var match_ten_occ2 "Match $\times$ Occ Tenure$^2 \times$ 100"

/*------------------------------------------------------------------------------------*/
/* Create the PCA-based mismatch measure */
/*------------------------------------------------------------------------------------*/


/* distance in rank asvab and ONET */
forvalues i=1/7{
	quietly gen rnk_mismatch`i' =    (asvab_rnk`i'- ONET_rnk_`i')
	quietly gen abs_mismatch`i' = abs(asvab_rnk`i'- ONET_rnk_`i')
}

/* Generate mismatch (signed and abs) as PCA 1 */
pca rnk_mismatch? [aw = swt], comp(3) tol(1e-7)
order rnk_mismatch*, before(abs_mismatch1) sequential
estat loadings, cnorm(unit)
matrix L = e(L)

/*now L has the weights from this PCA. We will apply them to the absolute difference in each dimension */
quietly forvalues i = 1/3{
	gen PCA_mm`i' = 0
	gen PCA_mm_neg`i' = 0
	gen PCA_mm_pos`i' = 0
	local Lwt = 0
	forvalues k=1/7{
		replace PCA_mm`i' = L[`k',`i']*abs_mismatch`k' +PCA_mm`i'
		local Lwt = `Lwt' +  L[`k',`i']
		replace PCA_mm_neg`i' = L[`k',`i']*rnk_mismatch`k' +PCA_mm_neg`i' if rnk_mismatch`k'<0
		replace PCA_mm_pos`i' = L[`k',`i']*rnk_mismatch`k' +PCA_mm_pos`i' if rnk_mismatch`k'>0
	}
	replace PCA_mm`i' = PCA_mm`i'/`Lwt'
	replace PCA_mm_neg`i' = PCA_mm_neg`i'/`Lwt'
	replace PCA_mm_pos`i' = PCA_mm_pos`i'/`Lwt'
}

*pca abs_mismatch? [aw=swt], comp(1) tol(1e-7)
*predict PCA_mm1, score

forvalues i=1/3{
	sort PCA_mm`i'
	gen mm_rnk = sum(swt)
	quietly sum mm_rnk, meanonly
	gen mm_rnk`i' = mm_rnk/r(max)
	gen mm_log`i' = log(PCA_mm`i')
	drop mm_rnk
}
*gen mm = mm_rnk1
gen mm = PCA_mm1
gen mm_ten_occ = mm*tenure_occ
gen mm_ten_occ2 = mm*ten_occ2
gen mm_ten_occ_iv = mm*ten_occ_iv
gen mm_ten_occ2_iv = mm*ten_occ2_iv
label var mm "Mismatch"
label var mm_ten_occ "Mismatch $\times$ Occ Tenure"
label var mm_ten_occ2 "Mismatch $\times$ Occ Tenure$^2 \times$ 100"

gen mm_neg = PCA_mm_neg1
gen mm_neg_ten_occ = mm_neg*tenure_occ
gen mm_neg_ten_occ2 = mm_neg*ten_occ2
gen mm_neg_ten_occ_iv = mm_neg*ten_occ_iv
gen mm_neg_ten_occ2_iv = mm_neg*ten_occ2_iv
label var mm_neg "Mismatch negative"
label var mm_neg_ten_occ "Mismatch neg $\times$ Occ Tenure"
label var mm_neg_ten_occ2 "Mismatch neg $\times$ Occ Tenure$^2 \times$ 100"

gen mm_pos = PCA_mm_pos1
gen mm_pos_ten_occ = mm_pos*tenure_occ
gen mm_pos_ten_occ2 = mm_pos*ten_occ2
gen mm_pos_ten_occ_iv = mm_pos*ten_occ_iv
gen mm_pos_ten_occ2_iv = mm_pos*ten_occ2_iv
label var mm_pos "Mismatch positive"
label var mm_pos_ten_occ "Mismatch pos $\times$ Occ Tenure"
label var mm_pos_ten_occ2 "Mismatch pos $\times$ Occ Tenure$^2 \times$ 100"

/* Generate cumulative and lagged measures of mismatch/match quality */

xtset id year
gen lmm = l.mm if id == l.id & switch_occ==1
replace lmm = l.lmm if switch_occ==0
gen lmm_pos = l.mm_pos if id == l.id & switch_occ==1
replace lmm_pos = l.lmm_pos if switch_occ==0
gen lmm_neg = l.mm_neg if id == l.id & switch_occ==1
replace lmm_neg = l.lmm_neg if switch_occ==0

gen lmatch = l.match if id == l.id & switch_occ==1
replace lmatch = l.lmatch if switch_occ==0

* experience for the previous match(es)	
gen lexp = l.tenure_occ if id == l.id & switch_occ==1
replace lexp = l.exp if switch_occ==0

gsort +id -switch_occ +year
by id: gen cmatch = sum(lmatch) if switch_occ==1
by id: gen cmm    = sum(lmm) if switch_occ==1  
by id: gen occnum = sum(switch_occ) if switch_occ==1 
replace cmm = cmm/occnum
replace cmatch = cmatch/occnum

by id: gen cmatch_exp = sum(lmatch*lexp) if switch_occ==1  
by id: gen cmm_exp = sum(lmm*lexp)  if switch_occ==1 
by id: gen totexp = sum(lexp) if switch_occ==1
replace cmatch_exp = cmatch_exp/totexp if switch_occ==1 
replace cmm_exp = cmm_exp/totexp if switch_occ==1 

/* signed cum match */
by id: gen cmm_pos_exp = sum(lmm_pos*lexp)/totexp if switch_occ==1
by id: gen cmm_neg_exp = sum(lmm_neg*lexp)/totexp if switch_occ==1

xtset id year
replace cmatch = l.cmatch if switch_occ==0 & match<.
replace cmm = l.cmm if switch_occ==0 & mm<.
replace cmatch_exp = l.cmatch_exp if switch_occ==0 & cmatch<.
replace cmm_exp = l.cmm_exp if switch_occ==0 & cmm<.
replace cmm_neg_exp = l.cmm_neg_exp if switch_occ==0 & cmm<.
replace cmm_pos_exp = l.cmm_pos_exp if switch_occ==0 & cmm<.

gen lmm_exp = lmm*lexp
gen lmatch_exp = lmatch*lexp

label var lmm "Last Mismatch"
label var cmm "Cumul Mismatch"
label var lmm_exp "Average Last Mismatch "
label var cmm_exp "Average Cumul Mismatch"
label var cmm_neg_exp "Average Cumul Negative Mismatch"
label var cmm_pos_exp "Average Cumul Positive Mismatch"

label var lmatch "Last Match Quality"
label var cmatch "Cumul Match Quality"
label var lmatch_exp "Average Last Match Quality"
label var cmatch_exp "Average Cumul Match Quality"

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

/*------------------------------------------------------------------------------------*/
/* Match quality regressions */
/*------------------------------------------------------------------------------------*/

/* ols with match + (asvab +onet + asvab_X_onet)(1+tenure)  */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage match match_ten_occ ASVAB_ONET_X   ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)

*------------------------------------------------------------------------------------*/
/* Match quality IV reqressions*/
/*------------------------------------------------------------------------------------*/


/* IV with match + cum match + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist match_ten_occ  ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  match_ten_occ_iv  ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
*global xlist match_ten_occ  tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
*global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  match_ten_occ_iv  
xi: ivregress 2sls lwage match  ASVAB_ONET_X asvab_m ONET_m  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)


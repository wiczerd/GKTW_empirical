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

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* setup variables */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*
/* Merge in health data*/
merge 1:1 id year using $result/health_nlsy79.dta , gen(_merge_health)
drop if _merge_health ==2
*/

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

* these are not mapped to ONET scores
drop asvab_sec05 asvab_sec06 asvab_sec07

* renumber them
rename asvab_sec01 asvab_sec05
rename asvab_sec02 asvab_sec01
rename asvab_sec08 asvab_sec02
rename asvab_sec09 asvab_sec06
rename asvab_sec10 asvab_sec07

/* clean the age effects from tests: */
gen tmp = age -(year-1980) 
bysort id: egen tmp2 = min(tmp)
qui tab tmp2, gen(I_enterage)
drop tmp tmp2

forvalues i=1/7{
	qui reg asvab_sec0`i' I_enterage1-I_enterage8 /*I_enterage1-I_enterage4 I_enterage6-I_enterage9 */
	qui predict asvab_res0`i', residual 
	qui replace asvab_res0`i' =asvab_res0`i' +_b["_cons"]
	qui sum asvab_res0`i'
	qui gen asvab_std0`i' = asvab_res0`i' /r(sd)
	qui gen asvab_maxscl0`i' = asvab_res0`i' /r(max)
	qui gen asvab_log0`i' = log(asvab_res0`i')
	qui sum asvab_log0`i'
	qui gen asvab_stdlog0`i' = asvab_log0`i' /r(sd)

}
qui reg AFQT I_enterage1-I_enterage4 I_enterage6-I_enterage9
qui predict AFQT_res, residual
qui sum AFQT, meanonly
qui replace AFQT_res = AFQT_res + r(mean)

/* convert tests to percentile scores */
sort id
gen ones = 1
egen swt = total(ones), by(id)
drop ones
replace swt = 1/swt
forvalues i = 1/7 {
	sort asvab_res0`i' id
	quietly gen rsum = sum(swt)
	quietly sum rsum, meanonly
	quietly gen asvab_rnk`i' = rsum/r(max)
	quietly gsort -rsum
	quietly replace asvab_rnk`i' = asvab_rnk`i'[_n-1] if asvab_res0`i' == asvab_res0`i'[_n-1]
	drop rsum
}

/* convert tests to AFQT equivalent scores using ranks*/
/* REQUIRES AKDENSITY: ssc install akdensity */
local NKDE = 75
akdensity AFQT_res,  cdf(AFQT_pctl) gen(AFQT_x AFQT_den) nograph n(`NKDE ')
forvalues i =1/7{
	sort AFQT_x 
	gen asvab_tmp = .
	forvalues j = 1/`NKDE '{
		qui gen dist_tmp = abs(asvab_rnk`i' - AFQT_pctl[`j'])
		qui replace asvab_tmp = AFQT_x[`j'] if dist_tmp < 1/`NKDE'*1/10
		drop dist_tmp
	}
	sort asvab_rnk`i'
	**syntax: ipolate yvar xvar , generate(newvar) epolate 
	ipolate asvab_tmp asvab_rnk`i', gen(asvab_AFQT`i') epolate
	drop asvab_tmp
}

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

/* create ONET_AFQT, ONET scores normalized to AFQT distribution */
forvalues i =1/7{
	sort AFQT_x 
	gen ONET_tmp = .
	forvalues j = 1/`NKDE '{
		qui gen dist_tmp = abs(ONET_rnk_`i' - AFQT_pctl[`j'])
		qui replace ONET_tmp = AFQT_x[`j'] if dist_tmp < 1/`NKDE'*1/10
		drop dist_tmp
	}
	sort asvab_rnk`i'
	**syntax: ipolate yvar xvar , generate(newvar) epolate 
	ipolate ONET_tmp ONET_rnk_`i', gen(ONET_AFQT`i') epolate
	drop ONET_tmp
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

if($stdize_match ==1){
	/* USE THIS FOR THE RAW SCORES */
	egen asvab_m = rowmean(asvab_res0?)
	egen ONET_m = rowmean(ONET_ASVAB_?)
	egen asvab_sd = rowsd(asvab_res0?)
	egen ONET_sd = rowsd(ONET_ASVAB_?)
	gen match = 0
	forvalues i = 1/7 {
		quietly replace match = match + (asvab_res0`i' - asvab_m)*(ONET_ASVAB_`i'-ONET_m)/7.
	}
}
else if($stdize_match ==2){
	/* USE LOG SCORES HERE */
	egen asvab_m = rowmean(asvab_log0?)
	egen ONET_m = rowmean(ONET_log?)
	egen asvab_sd = rowsd(asvab_log0?)
	egen ONET_sd = rowsd(ONET_log?)
	gen match = 0
	forvalues i = 1/7 {
		quietly replace match = match + (asvab_log0`i' - asvab_m)*(ONET_log`i'-ONET_m)/7.
	}
}
else if($stdize_match ==3){
	/*  USE RANK SCORES HERE: 	*/
	egen asvab_m = rowmean(asvab_rnk?)
	egen ONET_m = rowmean(ONET_rnk_?)
	egen asvab_sd = rowsd(asvab_rnk?)
	egen ONET_sd = rowsd(ONET_rnk_?)
	gen match = 0
	forvalues i = 1/7 {
		quietly replace match = match + (asvab_rnk`i' - asvab_m)*(ONET_rnk_`i'-ONET_m)/7.
	}
}
else if($stdize_match ==4){
	/*  USE AFQT equalized SCORES HERE:	 */
	egen asvab_m = rowmean(asvab_AFQT?)
	egen ONET_m = rowmean(ONET_AFQT?)
	egen asvab_sd = rowsd(asvab_AFQT?)
	egen ONET_sd = rowsd(ONET_AFQT?)
	gen match = 0
	forvalues i = 1/7 {
		quietly replace match = match + (asvab_AFQT`i' - asvab_m)*(ONET_AFQT`i'-ONET_m)/7.
	}
}
else if($stdize_match ==5){
	/*  USE AFQT equalized SCORES HERE:	 */
	egen asvab_m = rowmean(asvab_AFQT?)
	egen ONET_m = rowmean(ONET_ASVAB_?)
	egen asvab_sd = rowsd(asvab_AFQT?)
	egen ONET_sd = rowsd(ONET_ASVAB_?)
	gen match = 0
	forvalues i = 1/7 {
		quietly replace match = match + (asvab_AFQT`i' - asvab_m)*(ONET_ASVAB_`i'-ONET_m)/7.
	}
}
else if($stdize_match ==6){
	/*  USE SD STANDARDIZED SCORES HERE:	 */
	egen asvab_m = rowmean(asvab_std0?)
	egen ONET_m = rowmean(ONET_std?)
	egen asvab_sd = rowsd(asvab_std0?)
	egen ONET_sd = rowsd(ONET_std?)
	gen match = 0
	forvalues i = 1/7 {
		quietly replace match = match + (asvab_std0`i' - asvab_m)*(ONET_std`i'-ONET_m)/7.
	}
}
else if($stdize_match ==7){
	/*  USE SD STANDARDIZED LOG SCORES HERE:	 */
	egen asvab_m = rowmean(asvab_stdlog0?)
	egen ONET_m = rowmean(ONET_stdlog?)
	egen asvab_sd = rowsd(asvab_stdlog0?)
	egen ONET_sd = rowsd(ONET_stdlog?)
	gen match = 0
	forvalues i = 1/7 {
		quietly replace match = match + (asvab_stdlog0`i' - asvab_m)*(ONET_stdlog`i'-ONET_m)/7.
	}
}
else if($stdize_match ==8){
	/*  USE max-scaled scores HERE:	 */
	egen asvab_m = rowmean(asvab_maxscl0?)
	egen ONET_m = rowmean(ONET_maxscl?)
	egen asvab_sd = rowsd(asvab_maxscl0?)
	egen ONET_sd = rowsd(ONET_maxscl?)
	gen match = 0
	forvalues i = 1/7 {
		quietly replace match = match + (asvab_maxscl0`i' - asvab_m)*(ONET_maxscl`i'-ONET_m)/7.
	}
}
qui replace match = match/asvab_sd/ONET_sd

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
if($log_match == 1){
	gen ASVAB_ONET_X = log(exp(asvab_m)*exp(ONET_m)/asvab_sd/ONET_sd)
}
else{
	gen ASVAB_ONET_X = asvab_m*ONET_m/asvab_sd/ONET_sd
}
gen ASVAB_ONET_X_ten_occ = ASVAB_ONET_X*tenure_occ
gen ASVAB_ONET_X_ten_occ2 = ASVAB_ONET_X*ten_occ2
gen ASVAB_ONET_X_ten_occ_iv = ASVAB_ONET_X*ten_occ_iv
gen ASVAB_ONET_X_ten_occ2_iv = ASVAB_ONET_X*ten_occ2_iv



label var ASVAB_ONET_X "ASVAB $\times$ ONET"
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


xtset id year
gen count_occ_sw = 0
replace count_occ_sw = switch_occ + l.count_occ_sw if id==l.id /*[_n-1] if id==id[_n-1]*/
gen num_switch = count_occ_sw if switch_occ == 1

xtset id year
gen nswitch = 0
*replace nswitch = nswitch[_n-1] + switch_occ if id == id[_n-1]
replace nswitch = l.nswitch + switch_occ if id == l.id
ta nswitch
gen nswitch2 = nswitch^2

gen nocc_20s = 1
replace nocc_20s = l.nocc_20s + switch_occ if id == l.id & age<30
by id: egen tmp= max(nocc_20s)
replace nocc_20s = tmp
gen nocc_40s = 1
replace nocc_40s = l.nocc_40s + switch_occ if id == l.id & age>=40 & age<50
by id: egen tmp2= max(nocc_40s)
replace nocc_40s = tmp2
drop tmp tmp2

* for the median number of occupations in age group:
*sum nocc_20s if age>=20 & age<30, detail
*sum nocc_40s if age>=40 & age<50, detail

/* creating distance measure */


xtset id year
gen dsw_ONET_rnk = 0 if switch_occ == 1
forvalues i=1/7{
	replace dsw_ONET_rnk =1/7*abs(ONET_rnk_`i' - l.ONET_rnk_`i') + dsw_ONET_rnk if switch_occ == 1
}

/* calculating wage and match quality rank by occupation */
sort occ lwage
gen sumswt = swt if occ != occ[_n-1]
replace sumswt = sumswt[_n-1] + swt if occ == occ[_n-1]
quietly levelsof occ, local(occ_levs)
gen wage_rnk = .
foreach k of local occ_levs {
	quietly sum sumswt if occ == `k', meanonly
	quietly replace wage_rnk = sumswt/r(max) if occ == `k'
	}
drop sumswt

sort occ match
gen sumswt = swt if occ != occ[_n-1]
replace sumswt = sumswt[_n-1] + swt if occ == occ[_n-1]
quietly levelsof occ, local(occ_levs)
gen match_rnk = .
foreach k of local occ_levs {
	quietly sum sumswt if occ == `k', meanonly
	quietly replace match_rnk = sumswt/r(max) if occ == `k'
	}
drop sumswt

gen match_ten_occ = match*tenure_occ
gen match_ten_occ2 = match*ten_occ2
gen match_ten_occ_iv = match*ten_occ_iv
gen match_ten_occ2_iv = match*ten_occ2_iv
label var match "Match"
label var match_ten_occ "Match $\times$ Occ Tenure"
label var match_ten_occ2 "Match $\times$ Occ Tenure$^2 \times$ 100"
label var nswitch "Number of switches"
label var nswitch2 "Number of switches$^2$"

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
forvalues i = 1/3{
	quietly gen PCA_mm`i' = 0
	local Lwt = 0
	forvalues k=1/7{
		quietly replace PCA_mm`i' = L[`k',`i']*abs_mismatch`k' +PCA_mm`i'
		local Lwt = `Lwt' +  L[`k',`i']
	}
	quietly replace PCA_mm`i' = PCA_mm`i'/`Lwt'
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

/* Generate cumulative and lagged measures of mismatch/match quality */

xtset id year
gen lmm = l.mm if id == l.id & switch_occ==1
replace lmm = l.lmm if switch_occ==0
*gen cmm = mm+l.mm if id == l.id & switch_occ==1
*replace cmm = l.cmm if switch_occ==0

gen lmatch = l.match if id == l.id & switch_occ==1
replace lmatch = l.lmatch if switch_occ==0
*gen cmatch = match+l.match if id == l.id & switch_occ==1
*replace cmatch = l.cmatch if switch_occ==0

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

xtset id year
replace cmatch = l.cmatch if switch_occ==0 & match<.
replace cmm = l.cmm if switch_occ==0 & mm<.
replace cmatch_exp = l.cmatch_exp if switch_occ==0 & cmatch<.
replace cmm_exp = l.cmm_exp if switch_occ==0 & cmm<.

gen lmm_exp = lmm*lexp
gen lmatch_exp = lmatch*lexp

label var lmm "Last Mismatch"
label var cmm "Cumul Mismatch"
label var lmm_exp "Last Mismatch $\times$ Past Exp"
label var cmm_exp "Cumul Mismatch $\times$ Past Exp"

label var lmatch "Last Match Quality"
label var cmatch "Cumul Match Quality"
label var lmatch_exp "Last Match Quality $\times$ Past Exp"
label var cmatch_exp "Cumul Match Quality $\times$ Past Exp"
/*
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* Health stat stuff*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

*replace hlth_limkndwk = . if hlth_limkndwk <0
*replace hlth_limamtwk = . if hlth_limamtwk <0
foreach hlthv of varlist hlth_*{
	replace `hlthv' = . if `hlthv'<0
}
egen id_yr = group(id year)
egen hlth_any_t = rowmax(hlth_runmile- hlth_pullpush)
by id: egen hlth_any = max(hlth_any_t)
pca hlth_runmile- hlth_pullpush, components(1)
predict hlth_pca, score

by id: egen hlth_comp_i = mean(hlth_composite)
sum hlth_comp_i
replace hlth_comp_i  = (hlth_comp_i -r(mean))/r(sd)*10+50

pca ONET_phys*, components(1)
predict ONET_phys_pca, score
sum ONET_phys_pca
replace ONET_phys_pca = ((ONET_phys_pca-r(mean) )/r(sd)*10 + 50)

/* This was using population means for the "correlation"
sum ONET_phys_pca
local ONET_m = r(mean)
local ONET_s = r(sd)
sum hlth_comp_i
local hlth_m = r(mean)
local hlth_s = r(sd)
gen match_phys = (ONET_phys_pca-`ONET_m')*(hlth_composite - `hlth_m')/`hlth_s '/`ONET_s' 
*/
reg ONET_phys_pca $zlist exp exp2 exp3 year
predict ONET_phys_m, residuals
reg hlth_comp_i $zlist 
predict hlth_phys_m, residuals
qui sum ONET_phys_m
local ONET_phys_sd = r(sd)
qui sum hlth_phys_m
local hlth_phys_sd = r(sd)
gen match_phys = ONET_phys_m*hlth_phys_m/`hlth_phys_sd'/`ONET_phys_sd'
gen match_phys_ten_occ = match_phys * tenure_occ
gen match_phys_ten_occ_iv= match_phys * ten_occ_iv


sort ONET_phys_pca
gen ONET_phys_rnk = sum(swt)
quietly sum ONET_phys_rnk, meanonly
replace ONET_phys_rnk= ONET_phys_rnk/r(max)

sort hlth_comp_i
gen hlth_rnk = sum(swt)
quietly sum hlth_rnk, meanonly
replace hlth_rnk= hlth_rnk/r(max)
gen mm_phys = abs(ONET_phys_rnk- hlth_rnk)
gen mm_phys_ten_occ = mm_phys * tenure_occ
gen mm_phys_ten_occ_iv = mm_phys * ten_occ_iv

xtset id year
gen lmm_phys = l.mm_phys if id == l.id & switch_occ==1
replace lmm_phys = l.lmm_phys if switch_occ==0

gen lmatch_phys = l.match_phys if id == l.id & switch_occ==1
replace lmatch_phys = l.lmatch_phys if switch_occ==0

gsort +id -switch_occ +year
by id: gen cmatch_phys = sum(lmatch_phys) if switch_occ==1
by id: gen cmm_phys    = sum(lmm_phys) if switch_occ==1  
replace cmm_phys = cmm_phys/occnum
replace cmatch_phys = cmatch_phys/occnum

by id: gen cmatch_phys_exp = sum(lmatch_phys*lexp) if switch_occ==1  
by id: gen cmm_phys_exp = sum(lmm_phys*lexp)  if switch_occ==1 
replace cmatch_phys_exp = cmatch_phys_exp/totexp if switch_occ==1 
replace cmm_phys_exp = cmm_phys_exp/totexp if switch_occ==1 

xtset id year
replace cmatch_phys = l.cmatch_phys if switch_occ==0 & match_phys<.
replace cmm_phys = l.cmm_phys if switch_occ==0 & mm_phys<.
replace cmatch_phys_exp = l.cmatch_phys_exp if switch_occ==0 & cmatch_phys<.
replace cmm_phys_exp = l.cmm_phys_exp if switch_occ==0 & cmm_phys<.

gen lmm_phys_exp = lmm_phys*lexp
gen lmatch_phys_exp = lmatch_phys*lexp

label var hlth_comp_i "NLSY Health"
label var ONET_phys_pca "ONET Physical"

label var mm_phys "Physical Mismatch"
label var match_phys "Physical Match Quality"
label var mm_phys_ten_occ "Physical Mismatch $\times$ Occ Tenure"
label var match_phys_ten_occ "Physical Match Quality $\times$ Occ Tenure"

label var lmm_phys "Last Physical Mismatch"
label var cmm_phys "Cumul Physical Mismatch"
label var lmm_phys_exp "Last Physical Mismatch $\times$ Past Exp"
label var cmm_phys_exp "Cumul Physical Mismatch $\times$ Past Exp"

label var lmatch_phys "Last Physical Match Quality"
label var cmatch_phys "Cumul Physical Match Quality"
label var lmatch_phys_exp "Last Physical Match Quality $\times$ Past Exp"
label var cmatch_phys_exp "Cumul Physical Match Quality $\times$ Past Exp"
*/
/*------------------------------------------------------------------------------------*/
/* Generate some summary stats for the measure */
/*
sum PCA_mm1 [aw= swt], detail
disp "Kelley's Measure"
disp (r(p90)-2*r(p50)+r(p10))/(r(p90)-r(p10))
disp "Mean"
disp r(mean)
disp "Standard Deivation"
disp r(sd)

kdensity PCA_mm1 [aw=swt], ///
ytitle("Density") ///
xtitle("Distance Score") ///
title("Density of the Mismatch Distance Score") ///
saving($result/PCAmm_density, replace)
graph export $result/PCAmm_density.eps, replace
*/

/*------------------------------------------------------------------------------------*/
/* Export the correlation matrics */
corr ONET_rnk_?
matrix corrONET = r(C)
matrix colnames corrONET = AR  MK   WK   PC   GS   MC
matrix rownames corrONET = "Arith~Reasoning" "Math~Knowledge" "Word~Knowledge" "Paragraph~Comp" "General~Sci" "Mech~Comp" "Elec~Info"
outtable using $result/corrONET, mat(corrONET) nobox replace f(%9.2f) center caption("Occupations' Correlations between Scores (O{*}NET)") clabel(tab:corrONET)

corr asvab_rnk?
matrix corrASVAB = r(C)
matrix colnames corrASVAB = AR  MK   WK   PC   GS   MC
matrix rownames corrASVAB = "Arith~Reasoning" "Math~Knowledge" "Word~Knowledge" "Paragraph~Comp" "General~Sci" "Mech~Comp" "Elec~Info"
outtable using $result/corrASVAB, mat(corrASVAB) nobox replace f(%9.2f) center caption("Individuals' Correlations between Scores") clabel(tab:corrASVAB)


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
xi: reg lwage cmatch_exp match match_ten_occ ASVAB_ONET_X   ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_cq_q_asvab_onet.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(1)") fragment drop(_I* $zlist)  varlabels se replace
outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons  replace ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ )
outreg2 using $result/apxtable_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se replace ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist)


/* ols with match + (asvab +onet + asvab_X_onet)(1+tenure)  */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage match match_ten_occ ASVAB_ONET_X   ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_q_asvab_onet.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(1)") fragment drop(_I* $zlist)  varlabels se replace
outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ )
outreg2 using $result/apxtable_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist)

/* ols with cum match + (asvab +onet + asvab_X_onet)(1+tenure)  */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage cmatch_exp ASVAB_ONET_X   ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_cq_asvab_onet.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(1)") fragment drop(_I* $zlist)  varlabels se replace
outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ )
outreg2 using $result/apxtable_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist)


/* OLS with cum match */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage cmatch_exp  $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_cq.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(1)") fragment drop(_I* $zlist)  varlabels se replace
outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se replace nocons
outreg2 using $result/apxtable_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ) sortvar(cmatch_exp $xlist $zlist) label se append

 
/* OLS with match */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage match match_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_q.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(1)") fragment drop(_I* $zlist)  varlabels se replace
outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons append ///
	sortvar(match_ten_occ match)
outreg2 using $result/apxtable_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ) label se append ///
	sortvar(match_ten_occ match $xlist $zlist) 


/* ols with match + cum match */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage cmatch_exp match match_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_cq_q.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(1)") fragment drop(_I* $zlist)  varlabels se replace
outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons ///
	sortvar(cmatch_exp  match_ten_occ match)
outreg2 using $result/apxtable_corrols.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(cmatch_exp  match_ten_occ match $xlist $zlist)

stop

/*------------------------------------------------------------------------------------*/
/* Mismatch regressions */
/*------------------------------------------------------------------------------------*/

estimate use $result/bench_ols.ster
*outreg , tex bdec(3) ctitle("", "Benchmark") drop(_I* $zlist ) varlabels fragment se replace
outreg2 using $result/apxtable_ols_mm.tex, bdec(3) tex(fragment) ctitle("", "Benchmark") drop(_I* ) label se replace sortvar( $xlist) 


/* ols with cum mismatch + mismatch + (asvab +onet + asvab_X_onet)(1+tenure) */
use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
qui xi: reg lwage cmm_exp  mm mm_ten_occ ASVAB_ONET_X   ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/ols_cmm_mm_asvab_onet.ster, replace
*outreg , merge tex bdec(3) ctitle("", "(1)") drop(_I* $zlist )  fragment varlabels se replace
outreg2 using $result/table_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons replace ///
	sortvar(cmm_exp  mm_ten_occ mm ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ )
outreg2 using $result/apxtable_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se replace ///
	sortvar(cmm_exp  mm_ten_occ mm ASVAB_ONET_X   ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist)

/* ols with mismatch + (asvab +onet + asvab_X_onet)(1+tenure) */
use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
qui xi: reg lwage cmm_exp  mm mm_ten_occ ASVAB_ONET_X   ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/ols_mm_asvab_onet.ster, replace
*outreg , merge tex bdec(3) ctitle("", "(1)") drop(_I* $zlist )  fragment varlabels se replace
outreg2 using $result/table_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons ///
	sortvar(cmm_exp  mm_ten_occ mm ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ )
outreg2 using $result/apxtable_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(cmm_exp  mm_ten_occ mm ASVAB_ONET_X   ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist)

/* ols with cum mismatch + (asvab +onet + asvab_X_onet)(1+tenure) */
use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
qui xi: reg lwage cmm_exp  ASVAB_ONET_X   ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/ols_cmm_asvab_onet.ster, replace
*outreg , merge tex bdec(3) ctitle("", "(1)") drop(_I* $zlist )  fragment varlabels se replace
outreg2 using $result/table_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons ///
	sortvar(cmm_exp  mm_ten_occ mm ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ )
outreg2 using $result/apxtable_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(cmm_exp  mm_ten_occ mm ASVAB_ONET_X   ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist)


/* ols with cum mismatch 
use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage cmm_exp $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/ols_cmm.ster, replace
*outreg , merge tex bdec(3) ctitle("", "(1)") drop(_I* $zlist )  fragment varlabels se replace
outreg2 using $result/table_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se replace nocons ///
	sortvar(cmm_exp )
outreg2 using $result/apxtable_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(cmm_exp  $xlist $zlist)
*/

/* ols with mismatch
use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
qui xi: reg lwage mm mm_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/ols_mm.ster, replace
*outreg , merge tex bdec(3) ctitle("", "(1)") drop(_I* $zlist )  fragment varlabels se replace
outreg2 using $result/table_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons ///
	sortvar(mm_ten_occ mm)
outreg2 using $result/apxtable_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(mm_ten_occ mm $xlist $zlist)
 */

/* ols with cum mismatch + mismatch */
use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
qui xi: reg lwage cmm_exp  mm mm_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/ols_cmm_mm.ster, replace
*outreg , merge tex bdec(3) ctitle("", "(1)") drop(_I* $zlist )  fragment varlabels se replace
outreg2 using $result/table_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons ///
	sortvar(cmm_exp  mm_ten_occ mm)
outreg2 using $result/apxtable_ols_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(cmm_exp  mm_ten_occ mm $xlist $zlist)


/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* benchmarnk iv regression (Altonji and Shakotko) */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv

qui xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
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
global xlist match_ten_occ  ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  match_ten_occ_iv  ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
*global xlist match_ten_occ  tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
*global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  match_ten_occ_iv  
xi: ivregress 2sls lwage cmatch_exp match  ASVAB_ONET_X asvab_m ONET_m  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_cq_q_asvab_onet.ster, replace
*outreg , tex merge bdec(3) ctitle("", "(3)") drop(_I* $zlist ) fragment varlabels se replace
outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ten* exp* oj $zlist) label se nocons replace ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ)
outreg2 using $result/apxtable_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ) label se replace ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ ten* exp* oj $zlist)


/* IV with match + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist match_ten_occ  ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  match_ten_occ_iv  ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
*global xlist match_ten_occ  tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
*global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  match_ten_occ_iv  
xi: ivregress 2sls lwage match  ASVAB_ONET_X asvab_m ONET_m  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_q_asvab_onet.ster, replace
outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ten* exp* oj $zlist) label se nocons ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ)
outreg2 using $result/apxtable_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ) label se ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ ten* exp* oj $zlist)


/* IV with cum match + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
*global xlist match_ten_occ  tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
*global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  match_ten_occ_iv  
xi: ivregress 2sls lwage cmatch_exp ASVAB_ONET_X asvab_m ONET_m  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_cq_asvab_onet.ster, replace
*outreg , tex merge bdec(3) ctitle("", "(3)") drop(_I* $zlist ) fragment varlabels se replace
outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ten* exp* oj $zlist) label se nocons ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ)
outreg2 using $result/apxtable_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ) label se ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ ten* exp* oj $zlist)

/* iv regression with cum match */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv
xi: ivregress 2sls lwage cmatch_exp ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_cq.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(1)") drop(_I* $zlist ) fragment varlabels se
outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $xlist $zlist) label se nocons ///
	sortvar(cmatch_exp) replace
outreg2 using $result/apxtable_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(cmatch_exp $xlist $zlist)


/* iv regression with match */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj match_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match_ten_occ_iv
xi: ivregress 2sls lwage match ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_q.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(1)") drop(_I* $zlist ) fragment varlabels se
outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ten* exp* oj $zlist) label se nocons ///
	sortvar(match_ten_occ match)
outreg2 using $result/apxtable_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(match_ten_occ match ten* exp* oj $zlist)

 
/* iv regression with match + cum match*/

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj match_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match_ten_occ_iv
xi: ivregress 2sls lwage cmatch_exp match ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_cq_q.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(1)") drop(_I* $zlist ) fragment varlabels se
outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ten* exp* oj $zlist) label se nocons ///
	sortvar(cmatch_exp match_ten_occ match)
outreg2 using $result/apxtable_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(cmatch_exp match_ten_occ match ten* exp* oj $zlist)


/* IV with sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
*global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
*global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  match_ten_occ_iv  
xi: ivregress 2sls lwage ASVAB_ONET_X asvab_m ONET_m  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_asvab_onet.ster, replace
outreg2 using $result/apxtable_corriv.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* ) label se ///
	sortvar(cmatch_exp match_ten_occ match ASVAB_ONET_X ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ ten* exp* oj $zlist)


/*------------------------------------------------------------------------------------*/
/*Mismatch IV reqressions*/
/*------------------------------------------------------------------------------------*/

estimate use $result/bench_iv.ster
*outreg, tex bdec(3) ctitle("", "Benchmark") drop(_I* $zlist )  varlabels se
outreg2 using $result/apxtable_iv_mm.tex, bdec(3) tex(fragment) ctitle("", "Benchmark")  ///
 drop(_I*) label se replace sortvar( $xlist  $zlist) 

/* IV with mismatch + cum mismatch+ sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist mm_ten_occ  ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist mm_ten_occ_iv ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage cmm_exp mm asvab_m ONET_m ASVAB_ONET_X ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/iv_cmm_mm_asvab_onet.ster, replace
*outreg , tex merge bdec(3) ctitle("", "(3)") drop(_I* $zlist ) fragment varlabels se replace
outreg2 using $result/table_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ")  label se nocons replace ///
	sortvar(cmm_exp mm_ten_occ mm ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ ) ///
	drop(_I* $zlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj)
outreg2 using $result/apxtable_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ")  label se drop(_I*) replace ///
	sortvar(cmm_exp mm_ten_occ mm ASVAB_ONET_X ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ ///
	tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj $zlist) 


/* IV with mismatch + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist mm_ten_occ  ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist mm_ten_occ_iv ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage mm asvab_m ONET_m ASVAB_ONET_X ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/iv_mm_asvab_onet.ster, replace
outreg2 using $result/table_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ")  label se nocons ///
	sortvar(cmm_exp mm_ten_occ mm ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ ) ///
	drop(_I* $zlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj)
outreg2 using $result/apxtable_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ")  label se drop(_I*) ///
	sortvar(cmm_exp mm_ten_occ mm ASVAB_ONET_X ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ ///
	tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj $zlist) 


/* IV with cum mismatch+ sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage cmm_exp asvab_m ONET_m ASVAB_ONET_X ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/iv_cmm_asvab_onet.ster, replace
*outreg , tex merge bdec(3) ctitle("", "(3)") drop(_I* $zlist ) fragment varlabels se replace
outreg2 using $result/table_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ")  label se nocons ///
	sortvar(cmm_exp mm_ten_occ mm ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ ) ///
	drop(_I* $zlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj)
outreg2 using $result/apxtable_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ")  label se drop(_I*) ///
	sortvar(cmm_exp mm_ten_occ mm ASVAB_ONET_X ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ ///
	tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj $zlist) 


/* iv regression with cum mismatch

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist  tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv
xi: ivregress 2sls lwage cmm_exp ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/iv_cmm.ster, replace
*outreg, merge tex bdec(3) ctitle("", "(1)") drop(_I* $zlist ) fragment varlabels se
outreg2 using $result/table_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist $xlist) label se ///
	sortvar(cmm_exp ) nocons replace
outreg2 using $result/apxtable_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(cmm_exp ten* exp* oj $zlist)
 */

/* iv regression with mismatch

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist  tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj mm_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_ten_occ_iv
xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/iv_mm.ster, replace
*outreg, merge tex bdec(3) ctitle("", "(1)") drop(_I* $zlist ) fragment varlabels se
outreg2 using $result/table_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	sortvar(mm_ten_occ mm) ///
	drop(_I* $zlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj)
outreg2 using $result/apxtable_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(mm_ten_occ mm ten* exp* oj $zlist)
 */

/* iv regression with cum mismatch + mismatch */
use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist  tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj mm_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_ten_occ_iv
xi: ivregress 2sls lwage cmm_exp mm ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/iv_cmm_mm.ster, replace
*outreg, merge tex bdec(3) ctitle("", "(1)") drop(_I* $zlist ) fragment varlabels se
outreg2 using $result/table_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ") label se nocons ///
	sortvar(cmm_exp mm_ten_occ mm) ///
	drop(_I* $zlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj)
outreg2 using $result/apxtable_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I*) label se ///
	sortvar(cmm_exp mm_ten_occ mm ten* exp* oj $zlist)



/* IV with sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage asvab_m ONET_m ASVAB_ONET_X ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/iv_asvab_onet.ster, replace
*outreg , tex merge bdec(3) ctitle("", "(3)") drop(_I* $zlist ) fragment varlabels se replace
outreg2 using $result/apxtable_iv_mm.tex, bdec(3) tex(fragment) ctitle("", " ")  label se drop(_I*) ///
	sortvar(cmm_exp mm_ten_occ mm ASVAB_ONET_X ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ ///
	tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj $zlist) 

/* look for non-monotone ONET */
gen asvab_m2 = asvab_m*asvab_m
gen ONET_m2 = ONET_m*ONET_m
matrix pred_ONET_asvab_profile = J(9,3,0.0)
xi: ivregress 2sls lwage asvab_m asvab_m2 ONET_m ONET_m2 ASVAB_ONET_X ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
local b_onet = _b["ONET_m"]
local b_onet2 = _b["ONET_m2"]
local b_asvab = _b["asvab_m"]
local b_asvab2 = _b["asvab_m2"]
_pctile ONET_m, p(10 20 30 40 50 60 70 80 90)
forvalues qi = 1/9{
	matrix pred_ONET_asvab_profile[`qi',1] = `qi'*10
	matrix pred_ONET_asvab_profile[`qi',2] = `b_onet'*r(r`qi') + `b_onet2'*r(r`qi')
}
_pctile asvab_m, p(10 20 30 40 50 60 70 80 90)
forvalues qi = 1/9{
	matrix pred_ONET_asvab_profile[`qi',3] = `b_asvab'*r(r`qi') + `b_asvab2'*r(r`qi')
}
clear
svmat pred_ONET_asvab_profile
twoway line pred_ONET_asvab_profile2-pred_ONET_asvab_profile3 pred_ONET_asvab_profile1, ///
xtitle(Percentile) ytitle(Predicted Effect) title("Predicted effect of O*NET and ASVAB")
/*
/*------------------------------------------------------------------------------------*/

clear
set obs 8
gen match = 0
gen sum_asvab_rnk = 0
gen asvab_ten_occ = 0
gen sum_ONET_rnk = 0
gen ONET_ten_occ = 0
forvalues j = 2/15 {
gen _Iind_1d_`j' = 0
}
forvalues j = 2/14 {
gen _Iocc_1d_`j' = 0
}
foreach x of global zlist {
gen `x' = 0
}
input n tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 match_ten_occ match_ten_occ2 oj
1	5	0.25	0	0	0	0	0	0	0	0	1
2	10	1	0	0	0	0	0	0	0	0	1
3	0	0	5	0.25	1.25	0	0	0	0	0	1
4	0	0	10	1	10	0	0	0	0	0	1
5	0	0	0	0	0	5	0.25	1.25	0	0	1
6	0	0	0	0	0	10	1	10	0	0	1
7	0	0	0	0	0	0	0	0	2.5	0.125	1
8	0	0	0	0	0	0	0	0	5	0.5	1
save $result/temp_01.dta, replace

use $result/temp_01.dta, clear
local reg_list bench_ols bench_iv
foreach reg_spec of local reg_list {
estimate use $result/`reg_spec'.ster
predict coeff
quietly replace coeff = coeff - _b[_cons]
predict std, stdp
sort n
drop if n == 7 | n == 8
list coeff std
drop coeff std
}

use $result/temp_01.dta, clear
local reg_list ols ols_asvab iv iv_asvab
foreach reg_spec of local reg_list {
estimate use $result/`reg_spec'.ster
predict coeff
quietly replace coeff = coeff - _b[_cons]
predict std, stdp
sort n
list coeff std
drop coeff std
}

/*------------------------------------------------------------------------------------	*/
/*  Marginal effect plots								*/
/*------------------------------------------------------------------------------------	*/

clear
estimate use $result/corrols_q.ster
matrix eb=e(b)
matrix marg_match = J(16,4,0.0)
forvalues tt=0/15{
	matrix marg_match[`tt'+1,1] = `tt'
*	matrix marg_match[`tt'+1,2] = _b["match"]+_se["match"] + (_b["match_ten_occ"]+_se["match_ten_occ"])*`tt' +(_b["match_ten_occ2"]+_se["match_ten_occ2"])*`tt'^2/100
*	matrix marg_match[`tt'+1,3] = _b["match"]+_b["match_ten_occ"]*`tt' +_b["match_ten_occ2"]*`tt'^2/100
*	matrix marg_match[`tt'+1,4] = _b["match"]-_se["match"] + (_b["match_ten_occ"]-_se["match_ten_occ"])*`tt' +(_b["match_ten_occ2"]-_se["match_ten_occ2"])*`tt'^2/100
	matrix marg_match[`tt'+1,2] = _b["match"]+_se["match"] + (_b["match_ten_occ"]+_se["match_ten_occ"])*`tt' 
	matrix marg_match[`tt'+1,3] = _b["match"]+_b["match_ten_occ"]*`tt' 
	matrix marg_match[`tt'+1,4] = _b["match"]-_se["match"] + (_b["match_ten_occ"]-_se["match_ten_occ"])*`tt' 
}
svmat marg_match
twoway line marg_match2-marg_match4 marg_match1, ///
ytitle("Marginal Effect") ///
xtitle("Occupational Tenure") ///
title("Marginal Effect of Match: OLS") ///
lpattern("_" "-" "_-" ) lcolor("red" "black" "blue") ///
legend(label(1 "+1 Standard Dev") label(2 "Mean") label(3 "-1 Standard Deviation") ) ///
saving($result/marg_effect_ols, replace)
graph export $result/marg_effect_ols.eps, replace
drop marg_match*

estimate use $result/corriv_q.ster
matrix eb=e(b)
matrix marg_match = J(16,4,0.0)
forvalues tt=0/15{
	matrix marg_match[`tt'+1,1] = `tt'
*	matrix marg_match[`tt'+1,2] = _b["match"]+_se["match"] + (_b["match_ten_occ"]+_se["match_ten_occ"])*`tt' +(_b["match_ten_occ2"]+_se["match_ten_occ2"])*`tt'^2/100
*	matrix marg_match[`tt'+1,3] = _b["match"]+_b["match_ten_occ"]*`tt' +_b["match_ten_occ2"]*`tt'^2/100
*	matrix marg_match[`tt'+1,4] = _b["match"]-_se["match"] + (_b["match_ten_occ"]-_se["match_ten_occ"])*`tt' +(_b["match_ten_occ2"]-_se["match_ten_occ2"])*`tt'^2/100
	matrix marg_match[`tt'+1,2] = _b["match"]+_se["match"] + (_b["match_ten_occ"]+_se["match_ten_occ"])*`tt' 
	matrix marg_match[`tt'+1,3] = _b["match"]+_b["match_ten_occ"]*`tt' 
	matrix marg_match[`tt'+1,4] = _b["match"]-_se["match"] + (_b["match_ten_occ"]-_se["match_ten_occ"])*`tt' 
}
svmat marg_match
twoway line marg_match2-marg_match4 marg_match1, ///
ytitle("Marginal Effect") ///
xtitle("Occupational Tenure") ///
title("Marginal Effect of Match: IV") ///
lpattern("_" "-" "_-" ) lcolor("red" "black" "blue") ///
legend(label(1 "+1 Standard Dev") label(2 "Mean") label(3 "-1 Standard Deviation") ) ///
saving($result/marg_effect_iv, replace)
graph export $result/marg_effect_iv.eps, replace
drop marg_match*

graph combine $result/marg_effect_ols.gph $result/marg_effect_iv.gph, ycommon
graph export $result/marg_effect_ols_iv.eps, replace

clear
estimate use $result/ols_cmm_mm_asvab_onet.ster
matrix eb=e(b)
matrix marg_match = J(16,4,0.0)
forvalues tt=0/15{
	matrix marg_match[`tt'+1,1] = `tt'
	matrix marg_match[`tt'+1,2] = _b["mm"]+_se["mm"] + (_b["mm_ten_occ"]+_se["mm_ten_occ"])*`tt' 
	matrix marg_match[`tt'+1,3] = _b["mm"]+_b["mm_ten_occ"]*`tt' 
	matrix marg_match[`tt'+1,4] = _b["mm"]-_se["mm"] + (_b["mm_ten_occ"]-_se["mm_ten_occ"])*`tt' 
}
svmat marg_match
twoway line marg_match2-marg_match4 marg_match1, ///
ytitle("Marginal Effect") ///
xtitle("Occupational Tenure") ///
title("Marginal Effect of Mismatch: OLS") ///
lpattern("_" "-" "_-" ) lcolor("red" "black" "blue") ///
legend(label(1 "+1 Standard Dev") label(2 "Mean") label(3 "-1 Standard Deviation") ) ///
saving($result/marg_effect_ols_mm, replace)
graph export $result/marg_effect_ols_mm.eps, replace
drop marg_match*

estimate use $result/iv_cmm_mm_asvab_onet.ster
matrix eb=e(b)
matrix marg_match = J(16,4,0.0)
forvalues tt=0/15{
	matrix marg_match[`tt'+1,1] = `tt'
	matrix marg_match[`tt'+1,2] = _b["mm"]+_se["mm"] + (_b["mm_ten_occ"]+_se["mm_ten_occ"])*`tt'
	matrix marg_match[`tt'+1,3] = _b["mm"]+_b["mm_ten_occ"]*`tt'
	matrix marg_match[`tt'+1,4] = _b["mm"]-_se["mm"] + (_b["mm_ten_occ"]-_se["mm_ten_occ"])*`tt'
}
svmat marg_match
twoway line marg_match2-marg_match4 marg_match1, ///
ytitle("Marginal Effect") ///
xtitle("Occupational Tenure") ///
title("Marginal Effect of Mismatch: IV") ///
lpattern("_" "-" "_-" ) lcolor("red" "black" "blue") ///
legend(label(1 "+1 Standard Dev") label(2 "Mean") label(3 "-1 Standard Deviation") ) ///
saving($result/marg_effect_iv_mm, replace)
graph export $result/marg_effect_iv_mm.eps, replace
drop marg_match*

graph combine $result/marg_effect_ols_mm.gph $result/marg_effect_iv_mm.gph, ycommon
graph export $result/marg_effect_ols_iv_mm.eps, replace


/*------------------------------------------------------------------------------------*/
/* Predicted profiles*/
/*------------------------------------------------------------------------------------*/

use $result/yearly_03_corr.dta, clear
_pctile match, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)
clear

estimate use $result/corrols_q_asvab_onet.ster
matrix eb=e(b)
matrix pred_match = J(16,4,0.0)
forvalues tt=0/15{
	matrix pred_match[`tt'+1,1] = `tt'
	matrix pred_match[`tt'+1,2] = _b["match"]*`p90'+_b["match_ten_occ"]*`p90'*`tt' 
	matrix pred_match[`tt'+1,3] = _b["match"]*`p50'+_b["match_ten_occ"]*`p50'*`tt' 
	matrix pred_match[`tt'+1,4] = _b["match"]*`p10'+_b["match_ten_occ"]*`p10'*`tt' 
}
svmat pred_match
twoway line pred_match2-pred_match4 pred_match1, ///
ytitle("Predicted Return") ///
xtitle("Occupational Tenure") ///
title("Predicted Profile by Match: OLS") ///
lpattern("l" "l" "l") lcolor("red" "black" "blue") ///
legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") c(1)) ///
saving($result/pred_effect_ols, replace)
graph export $result/pred_effect_ols.eps, replace
drop pred_match*

estimate use $result/corriv_q_asvab_onet.ster
matrix eb=e(b)
matrix pred_match = J(16,4,0.0)
forvalues tt=0/15{
	matrix pred_match[`tt'+1,1] = `tt'
	matrix pred_match[`tt'+1,2] = _b["match"]*`p90'+_b["match_ten_occ"]*`p90'*`tt' 
	matrix pred_match[`tt'+1,3] = _b["match"]*`p50'+_b["match_ten_occ"]*`p50'*`tt' 
	matrix pred_match[`tt'+1,4] = _b["match"]*`p10'+_b["match_ten_occ"]*`p10'*`tt' 	

}
svmat pred_match
twoway line pred_match2-pred_match4 pred_match1, ///
ytitle("Predicted Return") ///
xtitle("Occupational Tenure") ///
title("Predicted Profile by Match Quality: IV") ///
lpattern("l" "l" "l") lcolor("red" "black" "blue") ///
legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") c(1)) ///
saving($result/pred_effect_iv, replace)
graph export $result/pred_effect_iv.eps, replace
drop pred_match*

graph combine $result/pred_effect_ols.gph $result/pred_effect_iv.gph, ycommon
graph export $result/pred_effect_ols_iv.eps, replace

use $result/yearly_03_corr.dta, clear
_pctile mm, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)
disp `p90'
disp `p70'
disp `p50'
disp `p30'
disp `p10'
clear

estimate use $result/ols_mm_asvab_onet.ster
matrix eb=e(b)
matrix pred_mm= J(16,4,0.0)
forvalues tt=0/15{
	matrix pred_mm[`tt'+1,1] = `tt'
	matrix pred_mm[`tt'+1,2] = _b["mm"]*`p90'+_b["mm_ten_occ"]*`p90'*`tt' 
	matrix pred_mm[`tt'+1,3] = _b["mm"]*`p50'+_b["mm_ten_occ"]*`p50'*`tt' 
	matrix pred_mm[`tt'+1,4] = _b["mm"]*`p10'+_b["mm_ten_occ"]*`p10'*`tt'
}
svmat pred_mm
twoway line pred_mm2-pred_mm4 pred_mm1, ///
ytitle("Predicted Return") ///
xtitle("Occupational Tenure") ///
title("Predicted Profile By Mismatch: OLS") ///
lpattern("l" "l" "l") lcolor("red" "black" "blue") ///
legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") c(1)) ///
saving($result/pred_effect_ols_mm, replace)
graph export $result/pred_effect_ols_mm.eps, replace
drop pred_mm*

estimate use $result/iv_mm_asvab_onet.ster
matrix eb=e(b)
matrix pred_mm = J(16,4,0.0)
forvalues tt=0/15{
	matrix pred_mm[`tt'+1,1] = `tt'
	matrix pred_mm[`tt'+1,2] = _b["mm"]*`p90'+_b["mm_ten_occ"]*`p90'*`tt' 
	matrix pred_mm[`tt'+1,3] = _b["mm"]*`p50'+_b["mm_ten_occ"]*`p50'*`tt' 
	matrix pred_mm[`tt'+1,4] = _b["mm"]*`p10'+_b["mm_ten_occ"]*`p10'*`tt'
}
svmat pred_mm
twoway line pred_mm2-pred_mm4 pred_mm1, ///
ytitle("Predicted Return") ///
xtitle("Occupational Tenure") ///
title("Predicted Profile By Mismatch: IV") ///
lpattern("l" "l" "l") lcolor("red" "black" "blue") ///
legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") c(1)) ///
saving($result/pred_effect_iv_mm, replace)
graph export $result/pred_effect_iv_mm.eps, replace
drop pred_mm*

graph combine $result/pred_effect_ols_mm.gph $result/pred_effect_iv_mm.gph, ycommon
graph export $result/pred_effect_ols_iv_mm.eps, replace

/*------------------------------------------------------------------------------------*/
/*-------------Predicted profiles and distribution of cmm, cmatch effect--------------*/

use $result/yearly_03_corr.dta, clear
estimate use $result/corriv_cq_q_asvab_onet.ster
gen cmatch_effect = _b["cmatch_exp"]*cmatch_exp
kdensity cmatch_effect, xtitle("(Coefficient) X (Cumulative match quality)") ///
title("The wage effect from cumulative match quality") ///
saving($result/dist_cmatch_effect, replace)
graph export $result/dist_cmatch_effect.eps, replace

_pctile cmatch_effect, p(10 50 90)
disp r(r1) - r(r3)

matrix pred_cmatch = J(16,4,0.0)
forvalues tt=0/15{
	matrix pred_cmatch[`tt'+1,1] = `tt'

	matrix pred_cmatch[`tt'+1,4] = r(r1)
	matrix pred_cmatch[`tt'+1,3] = r(r2)
	matrix pred_cmatch[`tt'+1,2] = r(r3)
}
svmat pred_cmatch
_pctile match, p(10 50 90)
local p90 = r(r3)
local p50 = r(r2)
local p10 = r(r1)
matrix eb=e(b)
matrix pred_match = J(16,4,0.0)
forvalues tt=0/15{
	matrix pred_match[`tt'+1,1] = `tt'
	matrix pred_match[`tt'+1,2] = _b["match"]*`p90'+_b["match_ten_occ"]*`p90'*`tt' 
	matrix pred_match[`tt'+1,3] = _b["match"]*`p50'+_b["match_ten_occ"]*`p50'*`tt' 
	matrix pred_match[`tt'+1,4] = _b["match"]*`p10'+_b["match_ten_occ"]*`p10'*`tt' 	

}
svmat pred_match

twoway (line pred_cmatch2-pred_cmatch4 pred_cmatch1, lpattern("--" "--" "--") lcolor("red" "black" "blue") ) ///
(line pred_match2-pred_match4 pred_match1, lpattern("l" "l" "l") lcolor("red" "black" "blue") ), ///
ytitle("Predicted Return") ///
xtitle("Occupational Tenure") ///
title("Predicted Profile By Match Quality: IV") ///
legend(label(1 "90 pctl, cumulative") label(2 "50 pctl") label(3 "10 pctl") ///
label(4 "90 pctl, current") label(5 "50 pctl") label(6 "10 pctl") c(2) order(1 4 2 5 3 6)) ///
saving($result/pred_effect_iv_cmatch, replace)
graph export $result/pred_effect_iv_cmatch.eps, replace

drop pred_cmatch*  pred_match*


use $result/yearly_03_corr.dta, clear
estimate use $result/iv_cmm_mm_asvab_onet.ster
gen cmm_effect = _b["cmm_exp"]*cmm_exp
kdensity cmm_effect, xtitle("(Coefficient) X (Cumulative mismatch)") ///
title("The wage effect from cumulative mismatch") ///
saving($result/dist_cmm_effect, replace)
graph export $result/dist_cmm_effect.eps, replace

_pctile cmm_effect, p(10 50 90)
disp r(r1) - r(r3)

matrix pred_cmm = J(16,4,0.0)
forvalues tt=0/15{
	matrix pred_cmm[`tt'+1,1] = `tt'

	matrix pred_cmm[`tt'+1,2] = r(r1)
	matrix pred_cmm[`tt'+1,3] = r(r2)
	matrix pred_cmm[`tt'+1,4] = r(r3)
}
svmat pred_cmm
svmat pred_mm

twoway (line pred_cmm2-pred_cmm4 pred_cmm1, lpattern("--" "--" "--") lcolor("red" "black" "blue") ) ///
(line pred_mm2-pred_mm4 pred_mm1, lpattern("l" "l" "l") lcolor("red" "black" "blue") ), ///
ytitle("Predicted Return") ///
xtitle("Occupational Tenure") ///
title("Predicted Profile By Mismatch: IV") ///
legend(label(1 "90 pctl, cumulative") label(2 "50 pctl") label(3 "10 pctl") ///
label(4 "90 pctl, current") label(5 "50 pctl") label(6 "10 pctl")  c(2) order(1 4 2 5 3 6)) ///
saving($result/pred_effect_iv_cmm, replace)
graph export $result/pred_effect_iv_cmm.eps, replace

drop pred_cmm*  pred_mm*


graph combine $result/pred_effect_iv_cmm.gph $result/pred_effect_iv_mm.gph, ycommon

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* how does match quality vary with time and switches*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

*Match quality
use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 oj 
*xi: xtreg match nswitch $xlist $zlist sum_ONET_rnk sum_asvab_rnk i.ind_1d i.occ_1d if nswitch>0, vce(robust)
xi: xtreg match nswitch l.match $zlist i.ind_1d i.occ_1d if nswitch>0 & nswitch<6 , vce(robust)
estimate save $result/xt_nsw.ster, replace
outreg , tex bdec(3) ctitle("", "(1)") drop(_I*) varlabels se replace fragment

*xi: reg match nswitch $xlist $zlist sum_ONET_rnk sum_asvab_rnk i.ind_1d i.occ_1d  if nswitch>0, vce(robust)
xi: reg match nswitch l.match $zlist i.ind_1d i.occ_1d  if nswitch>0 & nswitch<6 , vce(robust)
estimate save $result/ols_nsw.ster, replace
outreg , tex merge bdec(3) ctitle("", "(2)") drop(_I*) varlabels se replace  fragment

gen delmatch = match - l.match
xi: xtreg delmatch  nswitch $zlist i.ind_1d i.occ_1d  if nswitch>0 & nswitch<6 , vce(robust)
estimate save $result/del_nsw_mm.ster, replace
outreg, tex merge bdec(3) ctitle("", "(3)") drop(_I*) varlabels se replace fragment

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist exp exp2 oj
global ivlist exp_iv exp2_iv oj_iv
xi: xtivreg  match ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d 
estimate save $result/iv_exp.ster, replace
outreg, tex merge bdec(3) ctitle("", "(4)") drop(_I* ) varlabels se replace fragment

*global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 oj ONET_ten_occ
global xlist exp exp2 oj 
xi: xtreg match $xlist  $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save $result/xt_exp.ster, replace
outreg , tex merge bdec(3) ctitle("", "(5)") drop(_I* ) varlabels se replace fragment

xi: reg match  $xlist  $zlist i.ind_1d i.occ_1d if exp<=15
estimate save $result/ols_exp.ster, replace
outreg using $result/table_match.tex, tex merge bdec(3) ctitle("", "(6)") drop(_I* ) varlabels se replace fragment


* Mismatch
use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 oj 
*xi: xtreg mm nswitch $xlist $zlist i.ind_1d i.occ_1d if nswitch>0, vce(robust)
xi: xtreg mm nswitch l.mm $zlist i.ind_1d i.occ_1d if nswitch>0 & nswitch<6, vce(robust)
estimate save $result/xt_nsw_mm.ster, replace
outreg, tex bdec(3) ctitle("", "(1)") drop(_I*) varlabels se replace fragment

*xi: reg mm nswitch $xlist $zlist i.ind_1d i.occ_1d  if nswitch>0, vce(robust)
xi: reg mm nswitch l.mm $zlist i.ind_1d i.occ_1d  if nswitch>0 & nswitch<6, vce(robust)
estimate save $result/ols_nsw_mm.ster, replace
outreg, tex merge bdec(3) ctitle("", "(2)") drop(_I*) varlabels se replace fragment

gen delmm = mm - l.mm
xi: xtreg delmm nswitch $zlist i.ind_1d i.occ_1d  if nswitch>0 & nswitch<6 , vce(robust)
estimate save $result/del_nsw_mm.ster, replace
outreg using $result/table_mm.tex, tex merge bdec(3) ctitle("", "(3)") drop(_I*) varlabels se replace fragment

/*
use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year

global xlist exp exp2 oj
global ivlist exp_iv exp2_iv oj_iv
xi: xtivreg mm ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d
estimate save $result/iv_exp_mm.ster, replace
outreg, tex merge bdec(3) ctitle("", "(3)") drop(_I* ) varlabels se replace fragment

xi: xtreg mm $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/xt_exp_mm.ster, replace
outreg, tex merge bdec(3) ctitle("", "(4)") drop(_I* ) varlabels se replace fragment

xi: reg mm $xlist $zlist i.ind_1d i.occ_1d 
estimate save $result/ols_exp_mm.ster, replace
outreg using $result/table_mm.tex, tex merge bdec(3) ctitle("", "(5)") drop(_I* ) varlabels se replace fragment
*/

* Distance
use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 oj 
*xi: xtreg mm nswitch $xlist $zlist i.ind_1d i.occ_1d if nswitch>0, vce(robust)
xi: xtreg dsw_ONET_rnk  nswitch nswitch2 $zlist i.ind_1d i.occ_1d if nswitch>0 & nswitch<6, vce(robust)
estimate save $result/xt_nsw_dsw.ster, replace
outreg, tex bdec(3) ctitle("", "(1)") drop(_I*) varlabels se replace fragment

*xi: reg mm nswitch $xlist $zlist i.ind_1d i.occ_1d  if nswitch>0, vce(robust)
xi: reg dsw_ONET_rnk  nswitch nswitch2 $zlist i.ind_1d i.occ_1d  if nswitch>0 & nswitch<6, vce(robust)
estimate save $result/ols_nsw_dsw.ster, replace
outreg, tex merge bdec(3) ctitle("", "(2)") drop(_I*) varlabels se replace fragment

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year

global xlist exp exp2 oj
global ivlist exp_iv exp2_iv oj_iv
xi: xtivreg dsw_ONET_rnk  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d
estimate save $result/iv_exp_dsw.ster, replace
outreg, tex merge bdec(3) ctitle("", "(3)") drop(_I* ) varlabels se replace fragment

xi: xtreg dsw_ONET_rnk  $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/xt_exp_dsw.ster, replace
outreg, tex merge bdec(3) ctitle("", "(4)") drop(_I* ) varlabels se replace fragment

xi: ivreg dsw_ONET_rnk  ($xlist=$ivlist) $zlist i.ind_1d i.occ_1d 
estimate save $result/iv_exp_dsw.ster, replace
outreg using $result/table_dsw.tex, tex merge bdec(3) ctitle("", "(5)") drop(_I* ) varlabels se replace fragment

/*------------------------------------------------------------------------------------*/
/* Auto-corrleation of match and mismatch */
/*------------------------------------------------------------------------------------*/

xi: xtivreg mm (lmm=cmm_exp) exp exp2 $zlist i.occ_1d i.ind_1d if switch_occ==1
outreg2 using $result/mm_lmm.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*) replace
outreg2 using $result/mm_lmm_match_lmatch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*) replace
xi: xtreg mm lmm exp exp2 $zlist i.occ_1d i.ind_1d if switch_occ==1
outreg2 using $result/mm_lmm.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*)
outreg2 using $result/mm_lmm_match_lmatch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*) 
xi: reg mm lmm exp exp2 i.occ_1d i.ind_1d if switch_occ==1
outreg2 using $result/mm_lmm.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*)
outreg2 using $result/mm_lmm_match_lmatch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*) 

xi: xtivreg match (lmatch=cmatch_exp) exp exp2 $zlist i.occ_1d i.ind_1d if switch_occ==1
outreg2 using $result/match_lmatch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*) replace
outreg2 using $result/mm_lmm_match_lmatch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*) 
xi: xtreg match lmatch exp exp2 $zlist i.occ_1d i.ind_1d if switch_occ==1
outreg2 using $result/match_lmatch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*)
outreg2 using $result/mm_lmm_match_lmatch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*) 
xi: reg match lmatch exp exp2 $zlist i.occ_1d i.ind_1d if switch_occ==1
outreg2 using $result/match_lmatch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*) 
outreg2 using $result/mm_lmm_match_lmatch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop(_I* $zlist exp exp2 occ_1d* ind_1d*) sortvar(lmm lmatch)


/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* graphs */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

use $result/yearly_03_corr.dta, clear

/* distance by labor market experience */
lpoly dsw_ONET_rnk exp if exp <= 30, ci noscatter ///
ytitle("Average O*NET Distance") ///
xtitle("Labor Market Experience") ///
title("Distance of Switch by Labor Market Experience") ///
xlabel(5(5)30) ///
saving($result/d_switch_labexp, replace)
graph export $result/d_switch_labexp.eps, replace

/* distance by number of switches */
/* people who switches more than 6 times is 1.1 % in the sample */
lpoly dsw_ONET_rnk nswitch if nswitch <= 5, ci noscatter ///
ytitle("Average O*NET Distance") ///
xtitle("Number of Switches") ///
title("Distance of Switch by the Number of Switches") ///
xlabel(1(1)5) ///
saving($result/d_switch_labexp, replace)
graph export $result/d_switch_nswitches.eps, replace

/* probability of switches Over the Life-Cycle */
probit switch_occ $zlist
predict probit_sw
qui sum probit_sw, meanonly
gen  pr_sw_noedu = switch_occ-probit_sw+r(mean)
lpoly pr_sw_noedu age if age >= 20 & age <= 55, noscatter /// ci
ytitle("Probability of a Switch") ///
xtitle("Age") ///
title("Occupational Switches of Over the Life-Cycle") ///
xlabel(20(5)55) ///
ylabel(0.05(0.05)0.3) legend(off) ///
saving($result/p_switch_by_age_noedu, replace)
graph export $result/p_switch_by_age_noedu.eps, replace


lpoly switch_occ age if age >= 20 & age <= 55, noscatter /// ci
ytitle("Probability of a Switch") ///
xtitle("Age") ///
title("Occupational Switches Over the Life-Cycle") ///
xlabel(20(5)55) ///
ylabel(0.05(0.05)0.3) legend(off) ///
saving($result/p_switch_by_age, replace)
graph export $result/p_switch_by_age.eps, replace

/* Match quality super-imposed on switch pr */
twoway (lpoly switch_occ age, yaxis(1) ylabel(0.05(0.05)0.3) ytitle("Probability of a Switch", axis(1)) ) ///
(lpoly match age, yaxis(2) ytitle("Match Quality Measure" , axis(2))) if age >= 20 & age <= 55, ///
xtitle("Age") ///
title("Occupational Switches Over the Life-Cycle") ///
xlabel(20(5)55) legend(label(1 "Pr[Switch]") label(2 "Match")) /// legend(off) 
saving($result/p_match_switch_by_age, replace)
graph export $result/p_match_switch_by_age.eps, replace

/* average match quality by age */
lpoly match age if age >= 18 & age <= 55, noscatter /// ci
ytitle("Match Quality Measure") ///
xtitle("Age") ///
title("Match Quality Over the Life-Cycle") ///
xlabel(20(5)55) legend(off) ///
saving($result/match_by_age, replace)
graph export $result/match_by_age.eps, replace

/* average match quality by labor market experience */
lpoly match exp if exp <= 25, ci noscatter ///
ytitle("Match Quality Measure") ///
xtitle("Labor Market Experience") ///
title("Match Quality by Labor Market Experience") ///
xlabel(5(5)25) legend(off) ///
saving($result/match_labexp, replace)
graph export $result/match_labexp.eps, replace

/* average match quality by number of switches */
//bysort nswitch: egen match_sw= mean(match)
lpoly match nswitch if age >= 18 & age <= 50 & nswitch <= 5, ci noscatter ///
ytitle("Match Quality Measure") ///
xtitle("Number of Switches") ///
title("Match Quality by the Number of Switches") ///
saving($result/match_by_sw, replace)
graph export $result/match_by_sw.eps, replace

/* Mismatch super-imposed on switch pr */
twoway (lpoly switch_occ age, yaxis(1) ylabel(0.05(0.05)0.3) ytitle("Probability of a Switch", axis(1)) ) ///
(lpoly mm age, yaxis(2) ytitle("Mismatch", axis(2))) if age >= 20 & age <= 55, ///
xtitle("Age") ///
title("Occupational Switches Over the Life-Cycle") ///
xlabel(20(5)55) legend(label(1 "Pr[Switch]") label(2 "Mismatch"))  /// legend(off) 
saving($result/p_mm_switch_by_age, replace)
graph export $result/p_mm_switch_by_age.eps, replace


/* average mismatch by age */
lpoly mm age if age <= 55 & age >=18, noscatter /// ci 
ytitle("Mismatch Measure") ///
xtitle("Age") ///
title("Mismatch by Age") ///
xlabel(20(5)55) legend(off) ///
saving($result/mismatch_by_age, replace)
graph export $result/mismatch_by_age.eps, replace

/* average mismatch by labor market experience */
lpoly mm exp if exp <= 25, noscatter /// ci
ytitle("Mismatch Measure") ///
xtitle("Labor Market Experience") ///
title("Mismatch by Labor Market Experience") ///
xlabel(5(5)25) ///
saving($result/mismatch_labexp, replace)
graph export $result/mismatch_labexp.eps, replace

/* average mismatch by number of switches */
//bysort nswitch: egen mm_sw= mean(mm_log1)
lpoly mm_rnk1 nswitch if age >= 18 & age <= 50 & nswitch <= 5, ci noscatter ///
ytitle("Mismatch Measure") ///
xtitle("Number of Switches") ///
title("Mismatch by the Number of Switches") ///
saving($result/mismatch_by_sw, replace)
graph export $result/mismatch_by_sw.eps, replace

/*------------------------------------------------------------------------------------*/
                                                                                                                   /* CHNAGED */
use $result/yearly_03_corr.dta, clear
local y_title "Density"
local x_title "Density of ASVAB Score"

order asvab_sec0?, sequential
corr asvab_sec0?

* change name
rename asvab_sec01 AR_ASVAB
rename asvab_sec02 MK_ASVAB
rename asvab_sec03 WK_ASVAB
rename asvab_sec04 PC_ASVAB
rename asvab_sec05 GS_ASVAB
rename asvab_sec06 MC_ASVAB
rename asvab_sec07 EI_ASVAB

foreach section_name in "AR" "MK" "WK" "PC" "GS" "MC" "EI" {
	kdensity `section_name'_ASVAB if id != id[_n-1], ///
	legend(off) ///
	title("`section_name'_ASVAB") ///
	xtitle("") ///
	ytitle("") ///
	saving($result/ASVAB_`section_name'_density, replace)
	
	local files `files' $result/ASVAB_`section_name'_density.gph
}


//display "`files'"
graph combine `files', col(3) ycommon t1title(`x_title') l1title(`y_title') saving(ASVAB_density, replace)
graph export $result/ASVAB_density.eps, replace
local files ""

/*------------------------------------------------------------------------------------*/

                                                                                                                   /* CHNAGED */
use $result/yearly_03_corr.dta, clear
local y_title "Density"
local x_title "Density of ONET (in ASVAB Scores)"

order ONET_ASVAB*, sequential
corr ONET_ASVAB*

* change name
rename ONET_ASVAB_1 AR_ONET
rename ONET_ASVAB_2 MK_ONET
rename ONET_ASVAB_3 WK_ONET
rename ONET_ASVAB_4 PC_ONET
rename ONET_ASVAB_5 GS_ONET
rename ONET_ASVAB_6 MC_ONET
rename ONET_ASVAB_7 EI_ONET

foreach section_name in "AR" "MK" "WK" "PC" "GS" "MC" "EI" {
	kdensity `section_name'_ONET if id != id[_n-1], ///
	legend(off) ///
	title("`section_name'_ONET") ///
	xtitle("") ///
	ytitle("") ///
	saving($result/ONET_`section_name'_density, replace)
	
	local files `files' $result/ONET_`section_name'_density.gph
}


//display "`files'"
graph combine `files', col(3) ycommon t1title(`x_title') l1title(`y_title') saving(ONET_density, replace)
graph export $result/ONET_density.eps, replace
local files ""

/*------------------------------------------------------------------------------------*/
/*
/* density O*NET */
kdensity sum_ONET, ///
ytitle("Density") ///
xtitle("Sum of O*NET Score") ///
title("Density of the Sum of O*NET Score") ///
saving($result/ONET_sum_density, replace)
graph export $result/ONET_sum_density.eps, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* Probability of switch */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* u-shape by wage */
lpoly switch_occ wage_rnk, noscatter /// ci 
ytitle("Probability of Occupational Switch") ///
xtitle("Percentile in Wage Distribution") ///
title("Probability of Occupational Switch") ///
saving($result/u-shape_wage, replace)
graph export $result/u-shape_wage.eps, replace

/* u-shape by match */
lpoly switch_occ match_rnk, noscatter /// ci 
ytitle("Probability of Occupational Switch") ///
xtitle("Percentile in Match Distribution") ///
title("Probability of Occupational Switch") ///
saving($result/u-shape_match_rnk, replace)
graph export $result/u-shape_rnkmatch.eps, replace


lpoly switch_occ match, noscatter /// ci 
ytitle("Probability of Occupational Switch") ///
xtitle("Match Quality") ///
title("Probability of Occupational Switch") ///
xlabel(-.6(.2).6) ylabel(.15(.05).3) legend(off) ///
saving($result/u-shape_match, replace)
graph export $result/u-shape_match.eps, replace

lpoly switch_occ mm, noscatter /// ci 
ytitle("Probability of Occupational Switch") ///
xtitle("Mismatch") ///
title("Probability of Occupational Switch") ///
legend(off) ///
saving($result/u-shape_mm, replace)
graph export $result/u-shape_mm.eps, replace

xtset id year
gen age2=age*age

xi: probit switch_occ mm age age2 $zlist i.ind_1d i.occ_1d
outreg2 using $result/pswitch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop($zlist _I*) sortvar(mm age age2 ) replace
xi: xtprobit switch_occ mm age age2 $zlist i.ind_1d i.occ_1d
outreg2 using $result/pswitch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop($zlist _I*) sortvar(mm age age2 )
xi: ivprobit switch_occ (mm=cmm_exp) age age2 $zlist i.ind_1d i.occ_1d
*outreg2 using $result/pswitch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
*	drop($zlist age age2 _I*) sortvar(mm age age2 )

xi: probit switch_occ match age age2 $zlist i.ind_1d i.occ_1d
outreg2 using $result/pswitch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop($zlist age age2 _I*) sortvar(mm match age age2 )

xi: xtprobit switch_occ match age age2 $zlist i.ind_1d i.occ_1d
outreg2 using $result/pswitch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
	drop($zlist age age2 _I*) sortvar(mm match age age2 )

ivprobit switch_occ (match=cmatch_exp) age age2 $zlist i.ind_1d i.occ_1d
*outreg2 using $result/pswitch.tex, bdec(3) tex(fragment) ctitle("", " ") label se  nocons ///
*	drop($zlist age age2 _I*) sortvar(mm match age age2 )






/*---------------------------------------------------------------------------------------------*/
* A few exploratory for match quality

use $result/yearly_03_corr.dta, clear

gen educ_coarse = (hs==1) + (univ==1)

reg cmatch_exp i.educ_coarse
reg match i.educ_coarse

sum ind_1d, meanonly
local Nind = r(max)
matrix ind_match = J(`Nind',3,0.0)

forvalues i=1/`Nind'{
	qui sum match if ind_1d ==`i'
	matrix ind_match[`i',1]  = r(mean)
	matrix ind_match[`i',2]  = r(sd)
*	_pctile match if ind_1d ==`i', p(10 50 90)
*	matrix ind_match[`i',3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )
	sum rwage if ind_1d ==`i', meanonly
	matrix ind_match[`i',3] = r(mean)
}

*matrix colnames ind_match = "Average~Match" "Standard~Deviation" "Kelley's" "Average~Wage"
matrix colnames ind_match = "Average~Match" "Standard~Deviation" "Average~Wage"
matrix rownames ind_match =  "Agriculture" "Mining" "Construction" "Mfg" ///
"Trans~Comm~Util" "Retail~Trade" "FIRE" ///
"Business~Svcs" " Personal~Svcs" "Entertainment~Rec" "Prof~Services" "Public~Admin"

matsort ind_match 1 "up"

outtable using $result/ind_match , mat(ind_match) f(%9.2f) replace caption("Match quality across industries") clabel("tab:ind_match") nobox


sum ind_1d, meanonly
local Nind = r(max)
matrix ind_cmatch = J(`Nind',3,0.0)

forvalues i=1/`Nind'{
	qui sum cmatch_exp if ind_1d ==`i'
	matrix ind_cmatch[`i',1]  = r(mean)
	matrix ind_cmatch[`i',2]  = r(sd)
*	_pctile cmatch_exp if ind_1d ==`i', p(10 50 90)
*	matrix ind_cmatch[`i',3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )
	sum rwage if ind_1d ==`i', meanonly
	matrix ind_cmatch[`i',3] = r(mean)
}

*matrix colnames ind_cmatch = "Average~Match" "Standard~Deviation" "Kelley's" "Average~Wage"
matrix colnames ind_cmatch = "Average~Match" "Standard~Deviation" "Average~Wage"
matrix rownames ind_cmatch =  "Agriculture" "Mining" "Construction" "Mfg" ///
"Trans~Comm~Util" "Retail~Trade" "FIRE" ///
"Business~Svcs" " Personal~Svcs" "Entertainment~Rec" "Prof~Services" "Public~Admin"

matsort ind_match 1 "up"

outtable using $result/ind_cmatch , mat(ind_cmatch) f(%9.2f) replace caption("Cumulative match quality across industries") clabel("tab:ind_cmatch") nobox


matrix edu_match = J(3,2,0.0)

qui sum match if lths==1
matrix edu_match[1,1] = r(mean)
matrix edu_match[1,2] = r(sd)
*_pctile match if lths==1, p(10 50 90)
*matrix edu_match[1,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum match if hs ==1 & univ==0
matrix edu_match[2,1] = r(mean)
matrix edu_match[2,2] = r(sd)
*_pctile match if hs==1 & univ==0, p(10 50 90)
*matrix edu_match[2,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum match if univ==1
matrix edu_match[3,1] = r(mean)
matrix edu_match[3,2] = r(sd)
*_pctile match if univ==1, p(10 50 90)
*matrix edu_match[3,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

*matrix colnames edu_match = "Average~Match" "Standard~Deviation" "Kelley's"
matrix colnames edu_match = "Average~Match" "Standard~Deviation"
matrix rownames edu_match =  "Less~than~HS" "HS" "College"

outtable using $result/edu_match , mat(edu_match) f(%9.2f) replace caption("Match quality across education groups") clabel("tab:edu_match") nobox

matrix edu_cmatch = J(3,2,0.0)

qui sum cmatch_exp if lths==1
matrix edu_cmatch[1,1] = r(mean)
matrix edu_cmatch[1,2] = r(sd)
*_pctile cmatch_exp if lths==1, p(10 50 90)
*matrix edu_cmatch[1,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum cmatch_exp if hs ==1 & univ==0
matrix edu_cmatch[2,1] = r(mean)
matrix edu_cmatch[2,2] = r(sd)
*_pctile cmatch_exp if hs==1 & univ==0, p(10 50 90)
*matrix edu_cmatch[2,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum cmatch_exp if univ==1
matrix edu_cmatch[3,1] = r(mean)
matrix edu_cmatch[3,2] = r(sd)
*_pctile cmatch_exp if univ==1, p(10 50 90)
*matrix edu_cmatch[3,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

*matrix colnames edu_cmatch = "Average~Match" "Standard~Deviation" "Kelley's"
matrix colnames edu_cmatch = "Average~Match" "Standard~Deviation"
matrix rownames edu_cmatch =  "Less~than~HS" "HS" "College"

outtable using $result/edu_cmatch , mat(edu_cmatch) f(%9.2f) replace caption("Cumulative match quality across education groups") clabel("tab:edu_cmatch")  nobox


/*---------------------------------------------------------------------------------------------*/
* A few exploratory for mismatch

use $result/yearly_03_corr.dta, clear

xtreg mm, i(ind_1d)


gen educ_coarse = (hs==1) + (univ==1)


reg cmm_exp i.educ_coarse
reg mm i.educ_coarse

sum ind_1d, meanonly
local Nind = r(max)
matrix ind_mm = J(`Nind',3,0.0)

forvalues i=1/`Nind'{
	qui sum mm if ind_1d ==`i'
	matrix ind_mm[`i',1]  = r(mean)
	matrix ind_mm[`i',2]  = r(sd)
*	_pctile mm if ind_1d ==`i', p(10 50 90)
*	matrix ind_mm[`i',3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )
	sum rwage if ind_1d ==`i', meanonly
	matrix ind_mm[`i',3] = r(mean)
}

*matrix colnames ind_mm = "Average~Mismatch" "Standard~Deviation" "Kelley's" "Average~Wage"
matrix colnames ind_mm = "Average~Mismatch" "Standard~Deviation" "Average~Wage"
matrix rownames ind_mm =  "Agriculture" "Mining" "Construction" "Mfg" ///
"Trans~Comm~Util" "Retail~Trade" "FIRE" ///
"Business~Svcs" " Personal~Svcs" "Entertainment~Rec" "Prof~Services" "Public~Admin"

matsort ind_mm 1 "up"

outtable using $result/ind_mm , mat(ind_mm) f(%9.2f) replace caption("Mismatch across industries") clabel("tab:ind_mm") nobox


sum ind_1d, meanonly
local Nind = r(max)
matrix ind_cmm = J(`Nind',3,0.0)

forvalues i=1/`Nind'{
	qui sum cmm_exp if ind_1d ==`i'
	matrix ind_cmm[`i',1]  = r(mean)
	matrix ind_cmm[`i',2]  = r(sd)
	*_pctile cmm_exp if ind_1d ==`i', p(10 50 90)
	*matrix ind_cmm[`i',3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )
	sum rwage if ind_1d ==`i', meanonly
	matrix ind_cmm[`i',3] = r(mean)
}

*matrix colnames ind_cmm = "Average~Mismatch" "Standard~Deviation" "Kelley's" "Average~Wage"
matrix colnames ind_cmm = "Average~Mismatch" "Standard~Deviation" "Average~Wage"
matrix rownames ind_cmm =  "Agriculture" "Mining" "Construction" "Mfg" ///
"Trans~Comm~Util" "Retail~Trade" "FIRE" ///
"Business~Svcs" " Personal~Svcs" "Entertainment~Rec" "Prof~Services" "Public~Admin"

matsort ind_mm 1 "up"

outtable using $result/ind_cmm , mat(ind_cmm) f(%9.2f) replace caption("Cumulative mismatch across industries") clabel("tab:ind_cmm") nobox


matrix edu_mm = J(3,2,0.0)

qui sum mm if lths==1
matrix edu_mm[1,1] = r(mean)
matrix edu_mm[1,2] = r(sd)
*_pctile mm if lths==1, p(10 50 90)
*matrix edu_mm[1,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum mm if hs ==1 & univ==0
matrix edu_mm[2,1] = r(mean)
matrix edu_mm[2,2] = r(sd)
*_pctile mm if hs==1 & univ==0, p(10 50 90)
*matrix edu_mm[2,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum mm if univ==1
matrix edu_mm[3,1] = r(mean)
matrix edu_mm[3,2] = r(sd)
*_pctile mm if univ==1, p(10 50 90)
*matrix edu_mm[3,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

*matrix colnames edu_mm = "Average~Mismatch" "Standard~Deviation" "Kelley's"
matrix colnames edu_mm = "Average~Mismatch" "Standard~Deviation"
matrix rownames edu_mm =  "Less~than~HS" "HS" "College"

outtable using $result/edu_mm , mat(edu_mm) f(%9.2f) replace caption("Mismatch across education groups") clabel("tab:edu_mm") nobox

matrix edu_cmm = J(3,2,0.0)

qui sum cmm_exp if lths==1
matrix edu_cmm[1,1] = r(mean)
matrix edu_cmm[1,2] = r(sd)
*_pctile cmm_exp if lths==1, p(10 50 90)
*matrix edu_cmm[1,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum cmm_exp if hs ==1 & univ==0
matrix edu_cmm[2,1] = r(mean)
matrix edu_cmm[2,2] = r(sd)
*_pctile cmm_exp if hs==1 & univ==0, p(10 50 90)
*matrix edu_cmm[2,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum cmm_exp if univ==1
matrix edu_cmm[3,1] = r(mean)
matrix edu_cmm[3,2] = r(sd)
*_pctile cmm_exp if univ==1, p(10 50 90)
*matrix edu_cmm[3,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

*matrix colnames edu_cmm = "Average~Mismatch" "Standard~Deviation" "Kelley's"
matrix colnames edu_cmm = "Average~Mismatch" "Standard~Deviation"
matrix rownames edu_cmm =  "Less~than~HS" "HS" "College"

outtable using $result/edu_cmm , mat(edu_cmm) f(%9.2f) replace caption("Cumulative mismatch across education groups") clabel("tab:edu_cmm")  nobox
*/

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

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* setup variables */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* generate some  other covariates */
gen hs = (grade>=12)
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
	qui gen asvab_log0`i' = log(asvab_res0`i')
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
	gen ONET_log`i' = log(ONET_ASVAB_`i'*6)
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


/* USE THIS FOR THE RAW SCORES

egen asvab_m = rowmean(asvab_res0?)
egen ONET_m = rowmean(ONET_ASVAB_?)
egen asvab_sd = rowsd(asvab_res0?)
egen ONET_sd = rowsd(ONET_ASVAB_?)
gen match = 0
forvalues i = 1/7 {
	quietly replace match = match + (asvab_res0`i' - asvab_m)*(ONET_ASVAB_`i'-ONET_m)/7.
}
 */
/* USE LOG SCORES HERE */

egen asvab_m = rowmean(asvab_log0?)
egen ONET_m = rowmean(ONET_log?)
egen asvab_sd = rowsd(asvab_log0?)
egen ONET_sd = rowsd(ONET_log?)
gen match = 0
forvalues i = 1/7 {
	quietly replace match = match + (asvab_log0`i' - asvab_m)*(ONET_log`i'-ONET_m)/7.
}


/*  USE RANK SCORES HERE: 

egen asvab_m = rowmean(asvab_rnk?)
egen ONET_m = rowmean(ONET_rnk_?)
egen asvab_sd = rowsd(asvab_rnk?)
egen ONET_sd = rowsd(ONET_rnk_?)
gen match = 0
forvalues i = 1/7 {
	quietly replace match = match + (asvab_rnk`i' - asvab_m)*(ONET_rnk_`i'-ONET_m)/7.
}
*/

/*  USE AFQT equalized SCORES HERE:

egen asvab_m = rowmean(asvab_AFQT?)
egen ONET_m = rowmean(ONET_AFQT?)
egen asvab_sd = rowsd(asvab_AFQT?)
egen ONET_sd = rowsd(ONET_AFQT?)
gen match = 0
forvalues i = 1/7 {
	quietly replace match = match + (asvab_AFQT`i' - asvab_m)*(ONET_AFQT`i'-ONET_m)/7.
}
 */


/*  USE SD STANDARDIZED SCORES HERE:

egen asvab_m = rowmean(asvab_std0?)
egen ONET_m = rowmean(ONET_std?)
egen asvab_sd = rowsd(asvab_std0?)
egen ONET_sd = rowsd(ONET_std?)
gen match = 0
forvalues i = 1/7 {
	quietly replace match = match + (asvab_std0`i' - asvab_m)*(ONET_std`i'-ONET_m)/7.
}
 */


qui replace match = match/asvab_sd/ONET_sd 


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
gen ASVAB_ONET_X = asvab_m*ONET_m/asvab_sd/ONET_sd
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


xtset id time
gen count_occ_sw = 0
replace count_occ_sw = switch_occ + l.count_occ_sw if id==l.id/*[_n-1] if id==id[_n-1]*/
gen num_switch = count_occ_sw if switch_occ == 1

/* creating distance measure */

xtset id year
gen nswitch = 0
*replace nswitch = nswitch[_n-1] + switch_occ if id == id[_n-1]
replace nswitch = l.nswitch + switch_occ if id == l.id
ta nswitch
gen nswitch2 = nswitch^2

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
/* Don't recalculate this each time! the answers change depending on computer architecture! 
matrix L = ( ///
 .38557307 , -.27529333 ,  .48971223 \ ///
 .38193267 , -.29487417 ,  .52687555 \ ///
 .38471021 , -.25565833 , -.42482962 \ ///
 .36987644 ,  -.3630384 , -.41461873 \ ///
 .39404484 ,  .12956885 , -.21899979 \ ///
 .35259561 ,  .63336826 ,  .22557634 \ ///
 .37557332 ,  .47133989 , -.17705819 )
*/

/*now L has the weights from this PCA. We will apply them to the absolute difference in each dimension */
*quietly predict rnk_mismatch_ag1 rnk_mismatch_ag2 rnk_mismatch_ag3
forvalues i = 1/3{
	quietly gen PCA_mm`i' = 0
	forvalues k=1/7{
		quietly replace PCA_mm`i' = L[`k',`i']*abs_mismatch`k' +PCA_mm`i'
	}
*	quietly sum rnk_mismatch_ag`i', meanonly
*	quietly replace rnk_mismatch_ag`i' =  (rnk_mismatch_ag`i'-r(min))/(r(max)-r(min) )
}
forvalues i=1/3{
	sort PCA_mm`i'
	gen mm_rnk = sum(swt)
	quietly sum mm_rnk, meanonly
	gen mm_rnk`i' = mm_rnk/r(max)
	gen mm_log`i' = log(PCA_mm`i')
	drop mm_rnk
}
gen mm = mm_rnk1
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
by id: gen cmatch = (sum(lmatch) )/nswitch if switch_occ==1 & nswitch >=1
by id: gen cmm    = (sum(lmm) )/nswitch if switch_occ==1  & nswitch >=1
by id: gen cmatch_exp = (sum(lmatch*lexp) )/nswitch if switch_occ==1  & nswitch >=1
by id: gen cmm_exp = (sum(lmm*lexp) )/nswitch if switch_occ==1  & nswitch >=1

xtset id year
replace cmatch = l.cmatch if switch_occ==0
replace cmm = l.cmm if switch_occ==0
replace cmatch_exp = l.cmatch_exp if switch_occ==0
replace cmm_exp = l.cmm_exp if switch_occ==0


*gen lmm_exp = lmm*(exp - tenure_occ)
*gen cmm_exp = cmm*(exp - tenure_occ)
gen lmm_exp = lmm*lexp
*gen cmm_exp = mm+l.mm if id == l.id & switch_occ==1

gen lmatch_exp = lmatch*lexp
*gen cmatch_exp = cmatch*(exp - tenure_occ)

label var lmm "Last Mismatch"
label var cmm "Cumul Mismatch"
label var lmm_exp "Last Mismatch $\times$ Past Exp"
label var cmm_exp "Cumul Mismatch $\times$ Past Exp"

label var lmatch "Last Match Quality"
label var cmatch "Cumul Match Quality"
label var lmatch_exp "Last Match Quality $\times$ Past Exp"
label var cmatch_exp "Cumul Match Quality $\times$ Past Exp"

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
outreg, bdec(3) tex ctitle("", "Benchmark") drop(_I* $zlist) varlabels se replace fragment
*outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", "Benchmark") drop(_I* $zlist) label se replace sortvar( $xlist)

/*------------------------------------------------------------------------------------*/
/* Match quality regressions */
/*------------------------------------------------------------------------------------*/

/* ols with match */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage match match_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols.ster, replace
outreg, tex merge bdec(3) ctitle("", "(1)") fragment drop(_I* $zlist)  varlabels se replace
*outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", "(1)") drop(_I* $zlist) label se ///
*	sortvar(match match_ten_occ $xlist)

/*------------------------------------------------------------------------------------*/

/* ols with match + sum of asvab score + sum of ONET score */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
xi: reg lwage match ASVAB_ONET_X asvab_m ONET_m $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_asvab_ONET.ster, replace
outreg , tex merge bdec(3) ctitle("", "(2)") drop(_I* $zlist ) fragment varlabels se replace
*outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", "(2)") drop(_I* $zlist) label se ///
*	sortvar(match asvab_m ONET_m $xlist)

/*------------------------------------------------------------------------------------*/

/* ols with match + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
xi: reg lwage match match_ten_occ  ASVAB_ONET_X   ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_asvab_ONET_occten.ster, replace
outreg, tex merge bdec(3) ctitle("", "(3)") fragment drop(_I* $zlist ) varlabels se replace
*outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", "(3)") drop(_I* $zlist) label se ///
*	sortvar(match match_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist)

/*------------------------------------------------------------------------------------*/

/* ols with match + sum of ONET score + ONET*occ_ternure  + ONET*occ_ternure^2 + sum of ASVAB score + ASVAB*occ_ternure + ASVAB*occ_ternure^2

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
xi: reg lwage $xlist match match_ten_occ match_ten_occ2 $zlist ONET_m ONET_m_ten_occ ONET_m_ten_occ2 asvab_m asvab_m_ten_occ asvab_m_ten_occ2 i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_asvab_ONET_occten2.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(4)") fragment drop(_I* $zlist ) varlabels se replace
outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", "(4)") drop(_I* $zlist) label se ///
	sortvar(match match_ten_occ match_ten_occ2 asvab_m asvab_m_ten_occ asvab_m_ten_occ2 ONET_m ONET_m_ten_occ ONET_m_ten_occ2 $xlist)*/

/*------------------------------------------------------------------------------------*/

/* ols with match + cumulative match + sum of ONET score + sum of ASVAB score */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
xi: reg lwage match match_ten_occ  ASVAB_ONET_X  ASVAB_ONET_X_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ cmatch cmatch_exp $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corrols_cmatch.ster, replace
outreg using $result/table_corrols.tex, tex merge bdec(3) ctitle("", "(5)") fragment drop(_I* $zlist ) varlabels se replace
*outreg2 using $result/table_corrols.tex, bdec(3) tex(fragment) ctitle("", "(4)") drop(_I* $zlist) label se ///
*	sortvar(match match_ten_occ sum_asvab_rnk asvab_m_ten_occ ONET_m ONET_m_ten_occ cmatch cmatch_exp $xlist)
stop

/*------------------------------------------------------------------------------------*/
/* Mismatch regressions */
/*------------------------------------------------------------------------------------*/


estimate use $result/bench_ols.ster
outreg , tex bdec(3) ctitle("", "Benchmark") drop(_I* $zlist ) varlabels fragment se replace
*outreg2 using $result/table_ols_mm.tex, bdec(3) tex(fragment) ctitle("", "Benchmark") drop(_I* $zlist ) ///
* label se replace sortvar( $xlist)

/* ols with mismatch */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
qui xi: reg lwage mm mm_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/ols_mm.ster, replace
outreg , merge tex bdec(3) ctitle("", "(1)") drop(_I* $zlist )  fragment varlabels se replace
*outreg2 using $result/table_ols_mm.tex, bdec(3) tex(fragment) ctitle("", "(1)") drop(_I* $zlist) label se ///
*	sortvar(mm mm_ten_occ $xlist)


/*------------------------------------------------------------------------------------*/

/* ols with mismatch + sum of asvab score + sum of ONET score */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
xi: reg lwage mm asvab_m sum_ONET_rnk $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/ols_asvab_ONET_mm.ster, replace
outreg , tex merge bdec(3) ctitle("", "(2)") drop(_I* $zlist ) fragment varlabels se replace
*outreg2 using $result/table_ols_mm.tex, bdec(3) tex(fragment) ctitle("", "(2)") drop(_I* $zlist) label se ///
*	sortvar(mm asvab_m sum_ONET_rnk $xlist)

/*------------------------------------------------------------------------------------*/

/* ols with mismatch + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
xi: reg lwage mm mm_ten_occ sum_asvab_rnk asvab_ten_occ sum_ONET_rnk ONET_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/ols_asvab_ONET_occten_mm.ster, replace
outreg, tex merge bdec(3) ctitle("", "(3)") fragment drop(_I* $zlist ) varlabels se replace
*outreg2 using $result/table_ols_mm.tex, bdec(3) tex(fragment) ctitle("", "(3)") drop(_I* $zlist) label se ///
*	sortvar(mm mm_ten_occ sum_asvab_rnk asvab_ten_occ sum_ONET_rnk ONET_ten_occ $xlist)

/*------------------------------------------------------------------------------------

/* ols with mismatch + sum of ONET score + ONET*occ_ternure  + ONET*occ_ternure^2 + sum of ASVAB score + ASVAB*occ_ternure + ASVAB*occ_ternure^2*/

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
xi: reg lwage $xlist mm mm_ten_occ mm_ten_occ2 $zlist sum_ONET_rnk ONET_ten_occ ONET_ten_occ2 sum_asvab_rnk asvab_ten_occ asvab_ten_occ2 i.ind_1d i.occ_1d, vce(robust)
estimate save $result/ols_asvab_ONET_occten2_mm.ster, replace
outreg, tex merge bdec(3) ctitle("", "(4)") fragment drop(_I* $zlist ) varlabels se replace
*/

/*------------------------------------------------------------------------------------*/

/* ols with mismatch + cumulative match + sum of ONET score + sum of ASVAB score */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
xi: reg lwage mm mm_ten_occ sum_asvab_rnk asvab_ten_occ sum_ONET_rnk ONET_ten_occ cmm cmm_exp $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/ols_cmm.ster, replace
outreg using $result/table_ols_mm.tex, tex merge bdec(3) ctitle("", "(5)") fragment drop(_I* $zlist ) varlabels se replace
*outreg2 using $result/table_ols_mm.tex, bdec(3) tex(fragment) ctitle("", "(4)") drop(_I* $zlist) label se ///
*	sortvar(mm mm_ten_occ sum_asvab_rnk asvab_ten_occ sum_ONET_rnk ONET_ten_occ cmm cmm_exp $xlist)

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
outreg, tex fragment bdec(3) ctitle("", "Benchmark") drop(_I* $zlist )  varlabels se
*outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", "Benchmark") ///
* drop(_I* $zlist) label se replace sortvar( $xlist)

/*------------------------------------------------------------------------------------*/
/* Match quality IV reqressions*/
/*------------------------------------------------------------------------------------*/

/* iv regression with match */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist match_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match_ten_occ_iv
qui xi: ivregress 2sls lwage match ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv.ster, replace
outreg, tex merge bdec(3) ctitle("", "(1)") drop(_I* $zlist ) fragment varlabels se
*outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", "(1)") drop(_I* $zlist) label se ///
*	sortvar(match match_ten_occ $xlist)


/*------------------------------------------------------------------------------------*/

/* IV with match + sum of asvab score + sum of ONET score */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv 
xi: ivregress 2sls lwage match  ASVAB_ONET_X asvab_m ONET_m ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_asvab_ONET.ster, replace
outreg , tex merge bdec(3) ctitle("", "(2)") drop(_I* $zlist ) fragment varlabels se replace
*outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", "(2)") drop(_I* $zlist) label se ///
*	sortvar(match asvab_m ONET_m $xlist)

/*------------------------------------------------------------------------------------*/

/* IV with match + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist match_ten_occ  ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  match_ten_occ_iv  ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage match  ASVAB_ONET_X asvab_m ONET_m  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_asvab_ONET_occten.ster, replace
outreg , tex merge bdec(3) ctitle("", "(3)") drop(_I* $zlist ) fragment varlabels se replace
*outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", "(3)") drop(_I* $zlist) label se ///
*	sortvar(match match_ten_occ  asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist)

/*------------------------------------------------------------------------------------*/

/* IV with match + sum of ONET score + ONET*occ_ternure  + ONET*occ_ternure^2 + sum of ASVAB score + ASVAB*occ_ternure + ASVAB*occ_ternure^2*/

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj match_ten_occ match_ten_occ2  ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ ONET_m_ten_occ2 asvab_m_ten_occ2  ASVAB_ONET_X_ten_occ2
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  match_ten_occ_iv match_ten_occ2_iv  ASVAB_ONET_X_ten_occ_iv  ASVAB_ONET_X_ten_occ2_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv ONET_m_ten_occ2_iv asvab_m_ten_occ2_iv
xi: ivregress 2sls lwage  ($xlist = $ivlist) match $zlist  ASVAB_ONET_X ONET_m asvab_m i.ind_1d i.occ_1d, vce(robust)
estimate save $result/corriv_asvab_ONET_occten2.ster, replace
*outreg, tex merge bdec(3) ctitle("", "(4)") fragment drop(_I* $zlist ) varlabels se replace


/*------------------------------------------------------------------------------------*/

/* IV with match + cumulative match + sum of ONET score + sum of ASVAB score */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist match_ten_occ  ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match_ten_occ_iv  ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv 
qui xi: ivregress 2sls lwage match ONET_m asvab_m cmatch cmatch_exp ASVAB_ONET_X ($xlist = $ivlist) $zlist  i.ind_1d i.occ_1d , vce(robust)
estimate save $result/corriv_cmatch.ster, replace
outreg using $result/table_corriv.tex, tex merge bdec(3) ctitle("", "(4)") fragment drop(_I* $zlist ) varlabels se replace
*outreg2 using $result/table_corriv.tex, bdec(3) tex(fragment) ctitle("", "(4)") drop(_I* $zlist) label se ///
*	sortvar(match match_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ cmatch cmatch_exp $xlist)

/*------------------------------------------------------------------------------------*/
/*Mismatch IV reqressions*/
/*------------------------------------------------------------------------------------*/

estimate use $result/bench_iv.ster
outreg, tex bdec(3) ctitle("", "Benchmark") drop(_I* $zlist )  varlabels se
*outreg2 using $result/table_iv_mm.tex, bdec(3) tex(fragment) ctitle("", "Benchmark")  ///
* drop(_I* $zlist) label se replace sortvar( $xlist)

/* iv regression with mismatch */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist mm_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_ten_occ_iv
qui xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/iv_mm.ster, replace
outreg, merge tex bdec(3) ctitle("", "(1)") drop(_I* $zlist ) fragment varlabels se
*outreg2 using $result/table_iv_mm.tex, bdec(3) tex(fragment) ctitle("", "(1)") drop(_I* $zlist) label se ///
*	sortvar(mm mm_ten_occ $xlist)



/*------------------------------------------------------------------------------------*/

/* IV with mimatch + sum of asvab score + sum of ONET score */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv 
xi: ivregress 2sls lwage mm sum_asvab_rnk sum_ONET_rnk ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/iv_asvab_ONET_mm.ster, replace
outreg , tex merge bdec(3) ctitle("", "(2)") drop(_I* $zlist ) fragment varlabels se replace
*outreg2 using $result/table_iv_mm.tex, bdec(3) tex(fragment) ctitle("", "(2)") drop(_I* $zlist) label se ///
*	sortvar(mm sum_asvab_rnk sum_ONET_rnk $xlist)
	
/*------------------------------------------------------------------------------------*/

/* IV with mismatch + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj mm_ten_occ  asvab_ten_occ ONET_ten_occ
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  mm_ten_occ_iv asvab_ten_occ_iv ONET_ten_occ_iv
xi: ivregress 2sls lwage mm sum_asvab_rnk sum_ONET_rnk ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save $result/iv_asvab_ONET_occten_mm.ster, replace
outreg , tex merge bdec(3) ctitle("", "(3)") drop(_I* $zlist ) fragment varlabels se replace
*outreg2 using $result/table_iv_mm.tex, bdec(3) tex(fragment) ctitle("", "(3)") drop(_I* $zlist) label se ///
*	sortvar(mm mm_ten_occ  sum_asvab_rnk asvab_ten_occ sum_ONET_rnk ONET_ten_occ $xlist)

/*------------------------------------------------------------------------------------*/

/* IV with mismatch + sum of ONET score + ONET*occ_ternure  + ONET*occ_ternure^2 + sum of ASVAB score + ASVAB*occ_ternure + ASVAB*occ_ternure^2

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj mm_ten_occ mm_ten_occ2 asvab_ten_occ ONET_ten_occ ONET_ten_occ2 asvab_ten_occ2 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  mm_ten_occ_iv mm_ten_occ2_iv asvab_ten_occ_iv ONET_ten_occ_iv ONET_ten_occ2_iv asvab_ten_occ2_iv
xi: ivregress 2sls lwage  ($xlist = $ivlist) mm $zlist sum_ONET_rnk sum_asvab_rnk i.ind_1d i.occ_1d, vce(robust)
estimate save $result/iv_asvab_ONET_occten2_mm.ster, replace
outreg, tex merge bdec(3) ctitle("", "(4)") fragment drop(_I* $zlist ) varlabels se replace
*/

/*------------------------------------------------------------------------------------*/

/* IV with mismatch + cumulative mismatch + sum of ONET score + sum of ASVAB score */

use $result/yearly_03_corr.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj mm_ten_occ asvab_ten_occ ONET_ten_occ 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv mm_ten_occ_iv asvab_ten_occ_iv ONET_ten_occ_iv 
qui xi: ivregress 2sls lwage ($xlist = $ivlist) mm cmm cmm_exp $zlist  sum_ONET_rnk sum_asvab_rnk i.ind_1d i.occ_1d , vce(robust)
estimate save $result/iv_cmm.ster, replace
outreg using $result/table_iv_mm.tex, tex merge bdec(3) ctitle("", "(4)") fragment drop(_I* $zlist ) varlabels se replace
*outreg2 using $result/table_iv_mm.tex, bdec(3) tex(fragment) ctitle("", "(4)") drop(_I* $zlist) label se ///
*	sortvar(mm mm_ten_occ sum_asvab_rnk asvab_ten_occ sum_ONET_rnk ONET_ten_occ cmm cmm_exp $xlist)

xi: ivregress 2sls lwage ($xlist = $ivlist) mm cmm $zlist  sum_ONET_rnk sum_asvab_rnk i.ind_1d i.occ_1d , vce(robust)


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
estimate use $result/corrols_asvab_ONET_occten.ster
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
title("Marginal Effect of Match: OLS (1)") ///
lpattern("_" "-" "_-" ) ///
legend(label(1 "+1 Standard Dev") label(2 "Mean") label(3 "-1 Standard Deviation") ) ///
saving($result/marg_effect_ols, replace)
graph export $result/marg_effect_ols.eps, replace
drop marg_match*

estimate use $result/corriv_asvab_ONET_occten.ster
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
title("Marginal Effect of Match: IV (1)") ///
lpattern("_" "-" "_-" ) ///
legend(label(1 "+1 Standard Dev") label(2 "Mean") label(3 "-1 Standard Deviation") ) ///
saving($result/marg_effect_iv, replace)
graph export $result/marg_effect_iv.eps, replace
drop marg_match*

graph combine $result/marg_effect_ols.gph $result/marg_effect_iv.gph, ycommon
graph export $result/marg_effect_ols_iv.eps, replace

clear
estimate use $result/ols_asvab_ONET_occten_mm.ster
matrix eb=e(b)
matrix marg_match = J(16,4,0.0)
forvalues tt=0/15{
	matrix marg_match[`tt'+1,1] = `tt'
*	matrix marg_match[`tt'+1,2] = _b["mm"]+_se["mm"] + (_b["mm_ten_occ"]+_se["mm_ten_occ"])*`tt' +(_b["mm_ten_occ2"]+_se["mm_ten_occ2"])*`tt'^2/100
*	matrix marg_match[`tt'+1,3] = _b["mm"]+_b["mm_ten_occ"]*`tt' +_b["mm_ten_occ2"]*`tt'^2/100
*	matrix marg_match[`tt'+1,4] = _b["mm"]-_se["mm"] + (_b["mm_ten_occ"]-_se["mm_ten_occ"])*`tt' +(_b["mm_ten_occ2"]-_se["mm_ten_occ2"])*`tt'^2/100
	matrix marg_match[`tt'+1,2] = _b["mm"]+_se["mm"] + (_b["mm_ten_occ"]+_se["mm_ten_occ"])*`tt' 
	matrix marg_match[`tt'+1,3] = _b["mm"]+_b["mm_ten_occ"]*`tt' 
	matrix marg_match[`tt'+1,4] = _b["mm"]-_se["mm"] + (_b["mm_ten_occ"]-_se["mm_ten_occ"])*`tt' 
}
svmat marg_match
twoway line marg_match2-marg_match4 marg_match1, ///
ytitle("Marginal Effect") ///
xtitle("Occupational Tenure") ///
title("Marginal Effect of Mismatch: OLS (1)") ///
lpattern("_" "-" "_-" ) ///
legend(label(1 "+1 Standard Dev") label(2 "Mean") label(3 "-1 Standard Deviation") ) ///
saving($result/marg_effect_ols_mm, replace)
graph export $result/marg_effect_ols_mm.eps, replace
drop marg_match*

estimate use $result/iv_asvab_ONET_occten_mm.ster
matrix eb=e(b)
matrix marg_match = J(16,4,0.0)
forvalues tt=0/15{
	matrix marg_match[`tt'+1,1] = `tt'
*	matrix marg_match[`tt'+1,2] = _b["mm"]+_se["mm"] + (_b["mm_ten_occ"]+_se["mm_ten_occ"])*`tt' +(_b["mm_ten_occ2"]+_se["mm_ten_occ2"])*`tt'^2/100
*	matrix marg_match[`tt'+1,3] = _b["mm"]+_b["mm_ten_occ"]*`tt' +_b["mm_ten_occ2"]*`tt'^2/100
*	matrix marg_match[`tt'+1,4] = _b["mm"]-_se["mm"] + (_b["mm_ten_occ"]-_se["mm_ten_occ"])*`tt' +(_b["mm_ten_occ2"]-_se["mm_ten_occ2"])*`tt'^2/100
	matrix marg_match[`tt'+1,2] = _b["mm"]+_se["mm"] + (_b["mm_ten_occ"]+_se["mm_ten_occ"])*`tt'
	matrix marg_match[`tt'+1,3] = _b["mm"]+_b["mm_ten_occ"]*`tt'
	matrix marg_match[`tt'+1,4] = _b["mm"]-_se["mm"] + (_b["mm_ten_occ"]-_se["mm_ten_occ"])*`tt'
}
svmat marg_match
twoway line marg_match2-marg_match4 marg_match1, ///
ytitle("Marginal Effect") ///
xtitle("Occupational Tenure") ///
title("Marginal Effect of Mismatch: IV (1)") ///
lpattern("_" "-" "_-" ) ///
legend(label(1 "+1 Standard Dev") label(2 "Mean") label(3 "-1 Standard Deviation") ) ///
saving($result/marg_effect_iv_mm, replace)
graph export $result/marg_effect_iv_mm.eps, replace
drop marg_match*

graph combine $result/marg_effect_ols_mm.gph $result/marg_effect_iv_mm.gph, ycommon
graph export $result/marg_effect_ols_iv_mm.eps, replace


/*------------------------------------------------------------------------------------*/
/* Predicted profiles*/
/*------------------------------------------------------------------------------------*/

use $result/yearly_03_log.dta, clear
_pctile match, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)
clear
clear
estimate use $result/corrols_asvab_ONET_occten.ster
matrix eb=e(b)
matrix pred_match = J(16,6,0.0)
forvalues tt=0/15{
	matrix pred_match[`tt'+1,1] = `tt'
*	matrix pred_match[`tt'+1,2] = _b["match"]*`p90'+_b["match_ten_occ"]*`p90'*`tt' +_b["match_ten_occ2"]*`p90'*`tt'^2/100
*	matrix pred_match[`tt'+1,3] = _b["match"]*`p70'+_b["match_ten_occ"]*`p70'*`tt' +_b["match_ten_occ2"]*`p70'*`tt'^2/100
*	matrix pred_match[`tt'+1,4] = _b["match"]*`p50'+_b["match_ten_occ"]*`p50'`tt' +_b["match_ten_occ2"]*`p50'*`tt'^2/100
*	matrix pred_match[`tt'+1,5] = _b["match"]*`p30'+_b["match_ten_occ"]*`p30'*`tt' +_b["match_ten_occ2"]*`p30'*`tt'^2/100
*	matrix pred_match[`tt'+1,6] = _b["match"]*`p10'+_b["match_ten_occ"]*`p10'*`tt' +_b["match_ten_occ2"]*`p10'*`tt'^2/100
	matrix pred_match[`tt'+1,2] = _b["match"]*`p90'+_b["match_ten_occ"]*`p90'*`tt' 
	matrix pred_match[`tt'+1,3] = _b["match"]*`p70'+_b["match_ten_occ"]*`p70'*`tt' 
	matrix pred_match[`tt'+1,4] = _b["match"]*`p50'+_b["match_ten_occ"]*`p50'`tt' 
	matrix pred_match[`tt'+1,5] = _b["match"]*`p30'+_b["match_ten_occ"]*`p30'*`tt' 
	matrix pred_match[`tt'+1,6] = _b["match"]*`p10'+_b["match_ten_occ"]*`p10'*`tt' 
}
svmat pred_match
twoway line pred_match2-pred_match6 pred_match1, ///
ytitle("Predicted Return") ///
xtitle("Occupational Tenure") ///
title("Predicted Profile by Match: OLS (1)") ///
lpattern("l" "_" "_-" "_--" "--") ///
legend(label(1 "Top 10%") label(2 "Top 30%") label(3 "Top 50%") label(4 "Top 70%") label(5 "Top 90%")) ///
saving($result/pred_effect_ols, replace)
graph export $result/pred_effect_ols.eps, replace
drop pred_match*

estimate use $result/corriv_asvab_ONET_occten.ster
matrix eb=e(b)
matrix pred_match = J(16,6,0.0)
forvalues tt=0/15{
	matrix pred_match[`tt'+1,1] = `tt'
*	matrix pred_match[`tt'+1,2] = _b["match"]*`p90'+_b["match_ten_occ"]*`p90'*`tt' +_b["match_ten_occ2"]*`p90'*`tt'^2/100
*	matrix pred_match[`tt'+1,3] = _b["match"]*`p70'+_b["match_ten_occ"]*`p70'*`tt' +_b["match_ten_occ2"]*`p70'*`tt'^2/100
*	matrix pred_match[`tt'+1,4] = _b["match"]*`p50'+_b["match_ten_occ"]*`p50'`tt' +_b["match_ten_occ2"]*`p50'*`tt'^2/100
*	matrix pred_match[`tt'+1,5] = _b["match"]*`p30'+_b["match_ten_occ"]*`p30'*`tt' +_b["match_ten_occ2"]*`p30'*`tt'^2/100
*	matrix pred_match[`tt'+1,6] = _b["match"]*`p10'+_b["match_ten_occ"]*`p10'*`tt' +_b["match_ten_occ2"]*`p10'*`tt'^2/100
	matrix pred_match[`tt'+1,2] = _b["match"]*`p90'+_b["match_ten_occ"]*`p90'*`tt' 
	matrix pred_match[`tt'+1,3] = _b["match"]*`p70'+_b["match_ten_occ"]*`p70'*`tt' 
	matrix pred_match[`tt'+1,4] = _b["match"]*`p50'+_b["match_ten_occ"]*`p50'`tt' 
	matrix pred_match[`tt'+1,5] = _b["match"]*`p30'+_b["match_ten_occ"]*`p30'*`tt' 
	matrix pred_match[`tt'+1,6] = _b["match"]*`p10'+_b["match_ten_occ"]*`p10'*`tt' 

}
svmat pred_match
twoway line pred_match2-pred_match6 pred_match1, ///
ytitle("Predicted Return") ///
xtitle("Occupational Tenure") ///
title("Predicted Profile by Match: IV (1)") ///
lpattern("l" "_" "_-" "_--" "--") ///
legend(label(1 "Top 10%") label(2 "Top 30%") label(3 "Top 50%") label(4 "Top 70%") label(5 "Top 90%")) ///
saving($result/pred_effect_iv, replace)
graph export $result/pred_effect_iv.eps, replace
drop pred_match*

graph combine $result/pred_effect_ols.gph $result/pred_effect_iv.gph, ycommon
graph export $result/pred_effect_ols_iv.eps, replace


estimate use $result/ols_asvab_ONET_occten_mm.ster
matrix eb=e(b)
matrix pred_match = J(16,6,0.0)
forvalues tt=0/15{
	matrix pred_match[`tt'+1,1] = `tt'
*	matrix pred_match[`tt'+1,2] = _b["mm"]*0.90+_b["mm_ten_occ"]*0.90*`tt' +_b["mm_ten_occ2"]*0.90*`tt'^2/100
*	matrix pred_match[`tt'+1,3] = _b["mm"]*0.70+_b["mm_ten_occ"]*0.70*`tt' +_b["mm_ten_occ2"]*0.70*`tt'^2/100
*	matrix pred_match[`tt'+1,4] = _b["mm"]*0.50+_b["mm_ten_occ"]*0.50*`tt' +_b["mm_ten_occ2"]*0.50*`tt'^2/100
*	matrix pred_match[`tt'+1,5] = _b["mm"]*0.30+_b["mm_ten_occ"]*0.30*`tt' +_b["mm_ten_occ2"]*0.30*`tt'^2/100
*	matrix pred_match[`tt'+1,6] = _b["mm"]*0.10+_b["mm_ten_occ"]*0.10*`tt' +_b["mm_ten_occ2"]*0.10*`tt'^2/100
	matrix pred_match[`tt'+1,2] = _b["mm"]*0.90+_b["mm_ten_occ"]*0.90*`tt' 
	matrix pred_match[`tt'+1,3] = _b["mm"]*0.70+_b["mm_ten_occ"]*0.70*`tt' 
	matrix pred_match[`tt'+1,4] = _b["mm"]*0.50+_b["mm_ten_occ"]*0.50*`tt' 
	matrix pred_match[`tt'+1,5] = _b["mm"]*0.30+_b["mm_ten_occ"]*0.30*`tt' 
	matrix pred_match[`tt'+1,6] = _b["mm"]*0.10+_b["mm_ten_occ"]*0.10*`tt' 
}
svmat pred_match
twoway line pred_match2-pred_match6 pred_match1, ///
ytitle("Predicted Return") ///
xtitle("Occupational Tenure") ///
title("Predicted Profile By Mismatch: OLS (1)") ///
lpattern("l" "_" "_-" "_--" "--") ///
legend(label(1 "Top 10%") label(2 "Top 30%") label(3 "Top 50%") label(4 "Top 70%") label(5 "Top 90%")) ///
saving($result/pred_effect_ols_mm, replace)
graph export $result/pred_effect_ols_mm.eps, replace
drop pred_match*

estimate use $result/iv_asvab_ONET_occten_mm.ster
matrix eb=e(b)
matrix pred_match = J(16,6,0.0)
forvalues tt=0/15{
	matrix pred_match[`tt'+1,1] = `tt'
*	matrix pred_match[`tt'+1,2] = _b["mm"]*0.90+_b["mm_ten_occ"]*0.90*`tt' +_b["mm_ten_occ2"]*0.90*`tt'^2/100
*	matrix pred_match[`tt'+1,3] = _b["mm"]*0.70+_b["mm_ten_occ"]*0.70*`tt' +_b["mm_ten_occ2"]*0.70*`tt'^2/100
*	matrix pred_match[`tt'+1,4] = _b["mm"]*0.50+_b["mm_ten_occ"]*0.50*`tt' +_b["mm_ten_occ2"]*0.50*`tt'^2/100
*	matrix pred_match[`tt'+1,5] = _b["mm"]*0.30+_b["mm_ten_occ"]*0.30*`tt' +_b["mm_ten_occ2"]*0.30*`tt'^2/100
*	matrix pred_match[`tt'+1,6] = _b["mm"]*0.10+_b["mm_ten_occ"]*0.10*`tt' +_b["mm_ten_occ2"]*0.10*`tt'^2/100
	matrix pred_match[`tt'+1,2] = _b["mm"]*0.90+_b["mm_ten_occ"]*0.90*`tt' 
	matrix pred_match[`tt'+1,3] = _b["mm"]*0.70+_b["mm_ten_occ"]*0.70*`tt' 
	matrix pred_match[`tt'+1,4] = _b["mm"]*0.50+_b["mm_ten_occ"]*0.50*`tt' 
	matrix pred_match[`tt'+1,5] = _b["mm"]*0.30+_b["mm_ten_occ"]*0.30*`tt' 
	matrix pred_match[`tt'+1,6] = _b["mm"]*0.10+_b["mm_ten_occ"]*0.10*`tt' 
}
svmat pred_match
twoway line pred_match2-pred_match6 pred_match1, ///
ytitle("Predicted Return") ///
xtitle("Occupational Tenure") ///
title("Predicted Profile By Mismatch: IV (1)") ///
lpattern("l" "_" "_-" "_--" "--") ///
legend(label(1 "Top 10%") label(2 "Top 30%") label(3 "Top 50%") label(4 "Top 70%") label(5 "Top 90%")) ///
saving($result/pred_effect_iv_mm, replace)
graph export $result/pred_effect_iv_mm.eps, replace
drop pred_match*

graph combine $result/pred_effect_ols_mm.gph $result/pred_effect_iv_mm.gph, ycommon
graph export $result/pred_effect_ols_iv_mm.eps, replace


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
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* graphs */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*
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

/* probability of switches over life-cycle */
lpoly switch_occ age if age >= 20 & age <= 55, ci noscatter ///
ytitle("Probability of Switches") ///
xtitle("Age") ///
title("Probability of Switches over Life-Cycle") ///
xlabel(20(5)54) ///
ylabel(0.05(0.05)0.3) ///
saving($result/p_switch_by_age, replace)
graph export $result/p_switch_by_age.eps, replace

/* average match quality by age */
lpoly match age if age >= 18 & age <= 50, ci noscatter ///
ytitle("Match Quality Measure") ///
xtitle("Age") ///
title("Match Quality over Life-Cycle") ///
xlabel(20(5)50) ///
saving($result/match_by_age, replace)
graph export $result/match_by_age.eps, replace

/* average match quality by labor market experience */
lpoly match exp if exp <= 20, ci noscatter ///
ytitle("Match Quality Measure") ///
xtitle("Labor Market Experience") ///
title("Match Quality by Labor Market Experience") ///
xlabel(5(5)20) legend(off) ///
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

/* average match quality by labor market experience */
lpoly mm_rnk1 exp if exp <= 30, ci noscatter ///
ytitle("Mismatch Measure") ///
xtitle("Labor Market Experience") ///
title("Mismatch by Labor Market Experience") ///
xlabel(5(5)30) ///
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
*/
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

/* u-shape by wage */
lpoly switch_occ wage_rnk, ci noscatter ///
ytitle("Probability of Occupational Switch") ///
xtitle("Percentile in Wage Distribution") ///
title("Probability of Occupational Switch") ///
saving($result/u-shape_wage, replace)
graph export $result/u-shape_wage.eps, replace

/* u-shape by match */
lpoly switch_occ match_rnk, ci noscatter ///
ytitle("Probability of Occupational Switch") ///
xtitle("Percentile in Match Distribution") ///
title("Probability of Occupational Switch") ///
saving($result/u-shape_match, replace)
graph export $result/u-shape_match.eps, replace
*/

/*---------------------------------------------------------------------------------------------*/
* A few exploratory 
sum ind_1d, meanonly
local Nind = r(max)

matrix ind_match_mm = J(`Nind',2,0.0)

forvalues i=1/`Nind'{
	sum match if ind_1d ==`i', meanonly
	matrix ind_match_mm[`i',1]  = r(mean)
	sum mm if ind_1d ==`i', meanonly
	matrix ind_match_mm[`i',2]  = r(mean)
}
matrix colnames ind_match_mm = "Match" "Mismatch"
matrix rownames ind_match_mm =  "Agriculture" "Mining" "Construction" "Manufacturing" ///
"Transport/Comm/Util" "Retail Trade" "FIRE" ///
"Business,~Repair~Services" " Personal~Svcs" "Entertainment~Recreation" "Professional~Services" "Public~Administration"
outmatrix ind_match_mm , using($result/ind_match_mm) rowlab




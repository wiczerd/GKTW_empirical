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
global match_centered=1
global two_skills = 0

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

rename asvab_sec01_std asvab_sec1_std
rename asvab_sec02_std asvab_sec2_std
rename asvab_sec03_std asvab_sec3_std
rename asvab_sec04_std asvab_sec4_std
rename asvab_sec05_std asvab_sec5_std
rename asvab_sec06_std asvab_sec6_std
rename asvab_sec07_std asvab_sec7_std
rename asvab_sec08_std asvab_sec8_std
rename asvab_sec09_std asvab_sec9_std


/* clean the age effects from tests by means: */
gen tmp = age -(year-1980) 
bysort id: egen tmp2 = min(tmp)
qui tab tmp2, gen(I_enterage)

forvalues i=1/10{
	qui reg asvab_sec`i' I_enterage1-I_enterage8
	qui predict asvab_res`i', residual 
	qui replace asvab_res`i' =asvab_res`i' +_b["_cons"]
}

/* make the standard deviation the same*/
qui forvalues i=1/10{
	sum asvab_res`i' if tmp2== 20
	local sd20 = r(sd)
	forvalues ai=16/24{
		sum asvab_res`i' if tmp2 ==`ai'
		replace asvab_res`i' = asvab_res`i'/r(sd)*`sd20' if tmp2 ==`ai'
	}
}
* normalize everything to have standard deviation of 1
qui forvalues i=1/10{
	sum asvab_res`i'
	gen asvab_std`i' = asvab_res`i' /r(sd)
	/* normalize the population-weighted  */
	replace asvab_sec`i'_std = (asvab_sec`i'_std-50)/10 + r(mean)/r(sd)
}


/* Combine into 3 groups using the principal components as weights*/
pca asvab_res2 asvab_res8 asvab_res3 asvab_res4, comp(1)
predict asvab_comp1, score /* this composite is the math & verbal scores */
qui sum asvab_comp1
replace asvab_comp1 = (asvab_comp1 - r(min) )/r(sd)

pca asvab_res1 asvab_res9 asvab_res10, comp(1)
predict asvab_comp2, score /* this composite is the technical scores */
qui sum asvab_comp2
replace asvab_comp2 = (asvab_comp2 - r(min) )/r(sd)

/* Attentiveness! The 3rd component*/
pca asvab_res5 asvab_res6, comp(1)
predict asvab_comp3, score /* this composite is the attentiveness scores */
qui sum asvab_comp3
replace asvab_comp3 = (asvab_comp3 - r(min) )/r(sd)


/*------------------------------------------------------------------------------------*/
/*--3 component groups from the ONET--------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
pca ONET_ASVAB_1 ONET_ASVAB_2 ONET_ASVAB_3 ONET_ASVAB_4, comp(1)
predict ONET_comp1, score /* this composite is the math and verbal scores */
qui sum ONET_comp1
replace ONET_comp1 = (ONET_comp1 - r(min) )/r(sd)

pca ONET_ASVAB_5 ONET_ASVAB_6 ONET_ASVAB_7, comp(1)
predict ONET_comp2, score /* this composite is the technical scores */
qui sum ONET_comp2
replace ONET_comp2 = (ONET_comp2 - r(min) )/r(sd)

/*------------------------------------------------------------------------------------*/
/*--Attentiveness from the ONET-------------------------------------------------------*/

pca ONET_15-ONET_21, comp(1)
predict ONET_comp3, score
qui sum ONET_comp3
replace ONET_comp3 = (ONET_comp3 - r(min))/r(sd)


/*------------------------------------------------------------------------------------*/
/*--Doctor up a few of the numbers-------------------------------------------------------*/

* compute ranks
sort id
gen ones = 1
egen swt = total(ones), by(id)
replace swt = 1/swt

bysort occ: gen occ_indic=_n==1
by occ	: egen occ_wt = total(ones)

qui forvalues i=1/3{
/*	Comput ranks by hand:
	qui gen rsum = sum(ones) if occ_indic==1
	*qui gen rsum = sum(occ_wt) if occ_indic==1
	gsort -occ_indic +ONET_comp`i'
	qui sum rsum if occ_indic==1, meanonly
	gen ONET_rnk`i' = rsum/r(max) if occ_indic==1
	bysort occ: egen occ_rnk = max(ONET_rnk`i')
	qui replace ONET_rnk`i' = occ_rnk
	drop occ_rnk rsum
	
	sort asvab_comp`i' id
	gen rsum = sum(swt)
	sum rsum, meanonly
	gen asvab_rnk`i' = rsum/r(max)
	gsort -rsum
	replace asvab_rnk`i' = asvab_rnk`i'[_n-1] if asvab_res`i' == asvab_res`i'[_n-1]
	drop rsum	
*/
	
	cumul ONET_comp`i' if occ_indic ==1 [aw = occ_wt], equal gen(occ_rnk)
	bysort occ: egen ONET_rnk`i' = max(occ_rnk)
	drop occ_rnk
	cumul asvab_comp`i' [aw=swt], gen(asvab_rnk`i') equal
}

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* creating mismatch measure */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/


/* distance in rank asvab and ONET */
forvalues i=1/3{
	quietly gen mm_`i' =    (asvab_rnk`i'- ONET_rnk`i')
	quietly gen absmm_`i' = abs(asvab_rnk`i'- ONET_rnk`i')
}

/* Generate mismatch (signed and abs) as first principal component */
pca absmm_? [aw = swt], comp(1) tol(1e-7)
order mm_*, before(absmm_1) sequential
estat loadings, cnorm(unit)
matrix L = e(L)

/*now L has the weights from this PCA. We will apply them to the absolute difference in each dimension */
gen mm = 0
gen mm_neg = 0
gen mm_pos = 0
local Lwt = 0
forvalues k=1/3{
	replace mm = L[`k',1]*absmm_`k' +mm
	local Lwt = `Lwt' +  L[`k',1]
	replace mm_neg = L[`k',1]*mm_`k' +mm_neg if mm_`k'<0
	replace mm_pos = L[`k',1]*mm_`k' +mm_pos if mm_`k'>0
}
replace mm = mm/`Lwt'
replace mm_neg = mm_neg/`Lwt'
replace mm_pos = mm_pos/`Lwt'


* Do the 2-dimensional
pca mm_1 mm_2
estat loadings, cnorm(unit)
matrix L = e(L)
gen mm2 = 0
local Lwt = 0
forvalues i=1/2{
	local Lwt = `Lwt' + L[`i',1]
	replace mm2 = mm2+ mm_`i'*L[`i',1]
	replace mm2_neg = L[`k',1]*mm_`k' +mm2_neg if mm_`k'<0
	replace mm2_pos = L[`k',1]*mm_`k' +mm2_pos if mm_`k'>0
}
replace mm2 = mm2/`Lwt'
replace mm2_pos = mm2_pos/`Lwt'
replace mm2_neg = mm2_neg/`Lwt'

if($two_skills ==1){
	rename mm mm3
	rename mm_neg mm3_neg
	rename mm_pos mm3_pos
	rename mm2 mm
	rename mm2_neg mm_neg
	rename mm2_pos mm_pos
}


cumul mm [aw=swt], gen(mm_rnk) equal
gen mm_log = log(mm)
cumul mm_neg [aw=swt], gen(mm_rnk_neg) equal
gen mm_log_neg = log(PCA_mm_neg)
cumul mm_pos [aw=swt], gen(mm_rnk_pos) equal
gen mm_log_pos = log(mm_pos)

gen mm_ten_occ = mm*tenure_occ
gen mm_ten_occ2 = mm*ten_occ2
gen mm_ten_occ_iv = mm*ten_occ_iv
gen mm_ten_occ2_iv = mm*ten_occ2_iv
label var mm "Mismatch"
label var mm_ten_occ "Mismatch $\times$ Occ Tenure"
label var mm_ten_occ2 "Mismatch $\times$ Occ Tenure$^2 \times$ 100"



forvalues i=1/3{
	gen mm_`i'_ten_occ = mm_`i'*tenure_occ
	gen mm_`i'_ten_occ2 = mm_`i'*ten_occ2
	gen mm_`i'_ten_occ_iv = mm_`i'*ten_occ_iv
	gen mm_`i'_ten_occ2_iv = mm_`i'*ten_occ2_iv
	label var mm_`i' "Mismatch `i' "
	label var mm_`i'_ten_occ "Mismatch `i' $\times$ Occ Tenure"
	label var mm_`i'_ten_occ2 "Mismatch `i' $\times$ Occ Tenure$^2 \times$ 100"
}


gen mm_neg_ten_occ = mm_neg*tenure_occ
gen mm_neg_ten_occ2 = mm_neg*ten_occ2
gen mm_neg_ten_occ_iv = mm_neg*ten_occ_iv
gen mm_neg_ten_occ2_iv = mm_neg*ten_occ2_iv
label var mm_neg "Mismatch negative"
label var mm_neg_ten_occ "Mismatch neg $\times$ Occ Tenure"
label var mm_neg_ten_occ2 "Mismatch neg $\times$ Occ Tenure$^2 \times$ 100"

gen mm_pos_ten_occ = mm_pos*tenure_occ
gen mm_pos_ten_occ2 = mm_pos*ten_occ2
gen mm_pos_ten_occ_iv = mm_pos*ten_occ_iv
gen mm_pos_ten_occ2_iv = mm_pos*ten_occ2_iv
label var mm_pos "Mismatch positive"
label var mm_pos_ten_occ "Mismatch pos $\times$ Occ Tenure"
label var mm_pos_ten_occ2 "Mismatch pos $\times$ Occ Tenure$^2 \times$ 100"

/*------------------------------------------------------------------------------------*/
/*--Correlation measures (1) between 3 cognitive and (2) cog-attn---------------------*/
/*------------------------------------------------------------------------------------*/

egen asvab_m = rowmean(asvab_comp?)
egen ONET_m = rowmean(ONET_comp?)
egen asvab_sd = rowsd(asvab_comp?)
egen ONET_sd = rowsd(ONET_comp?)

egen asvab_m2 = rowmean(asvab_comp1 asvab_comp2)
egen ONET_m2 = rowmean(ONET_comp1 ONET_comp2)
egen asvab_sd2 = rowsd(asvab_comp1 asvab_comp2)
egen ONET_sd2 = rowsd(ONET_comp1 ONET_comp2)


gen match = 0
quietly forvalues i = 1/3{
	if($match_centered == 1){
		replace match = match + (asvab_comp`i' - asvab_m)*(ONET_comp`i'-ONET_m)/3
		gen match_`i' = (asvab_comp`i' - asvab_m)*(ONET_comp`i'-ONET_m)
	}
	else{
		replace match = match + asvab_comp`i'*ONET_comp`i'/3
		gen match_`i' = asvab_comp`i'*ONET_comp`i'
	}
}
*replace match = match/asvab_sd/ONET_sd
replace match = log(match)
if($match_centered ==1){
	gen match2 = ( (asvab_comp1 - asvab_m2)*(ONET_comp1 - ONET_m2) + (asvab_comp2 - asvab_m2)*(ONET_comp2 - ONET_m2) )/2
}
else{
	gen match2 = ( asvab_comp1*ONET_comp1 + asvab_comp2*ONET_comp2 )/2
}
*replace match2 = match2/asvab_sd2/ONET_sd2
replace match2 = log(match2)

if($two_skills ==1){
	rename match match3
	rename match2 match
}


/*------------------------------------------------------------------------------------*/
/* Generate cumulative and lagged measures of mismatch/match quality */
/*------------------------------------------------------------------------------------*/

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
replace cmatch_exp = l.cmatch_exp if switch_occ==0 & match<.
replace cmm_exp = l.cmm_exp if switch_occ==0 & mm<.
replace cmm_neg_exp = l.cmm_neg_exp if switch_occ==0 & mm<.
replace cmm_pos_exp = l.cmm_pos_exp if switch_occ==0 & mm<.

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

qui forvalues i=1/3{
	gen match_`i'_ten_occ = match_`i'*tenure_occ
	gen match_`i'_ten_occ2 = match_`i'*ten_occ2
	gen match_`i'_ten_occ_iv = match_`i'*ten_occ_iv
	gen match_`i'_ten_occ2_iv = match_`i'*ten_occ2_iv
	label var match_`i'_ten_occ "Match `i' $\times$ Occ Tenure"
	label var match_`i'_ten_occ2 "Match `i' $\times$ Occ Tenure$^2 \times$ 100"
}


save $result/yearly_03_attn.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* estimation */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* benchmark ols regression */

use $result/yearly_03_attn.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage $xlist $zlist i.ind_1d i.occ_1d, vce(robust)

/*------------------------------------------------------------------------------------*/
/* Match quality regressions */
/*------------------------------------------------------------------------------------*/

/* ols with cum match + match + (asvab +onet + asvab_X_onet)(1+tenure)  */

use $result/yearly_03_attn.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
xi: reg lwage match_? match_?_ten_occ asvab_m asvab_m_ten_occ ONET_m ONET_m_ten_occ $xlist $zlist i.ind_1d i.occ_1d, vce(robust)

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* benchmarnk iv regression (Altonji and Shakotko) */

use $result/yearly_03_attn.dta, clear
keep if $sample_select
xtset id year
global xlist tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)

/*------------------------------------------------------------------------------------*/
/* Match quality IV reqressions*/
/*------------------------------------------------------------------------------------*/


/* IV with match + cum match + sum of ONET score + ONET * occ_ternure + sum of ASVABV score + ASVAB * occ_ternure */

use $result/yearly_03_attn.dta, clear
keep if $sample_select
xtset id year
global xlist match_?_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv match_?_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage match_? asvab_m ONET_m ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)

global xlist match_?_ten_occ  ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  match_?_ten_occ_iv ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage match_? cmatch_exp ASVAB_ONET_X asvab_m ONET_m  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)

global xlist mm_?_ten_occ  ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  mm_?_ten_occ_iv ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage absmm_? cmm_exp ASVAB_ONET_X asvab_m ONET_m  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)

global xlist mm_ten_occ ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  mm_ten_occ_iv ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage mm cmm_exp ASVAB_ONET_X asvab_m ONET_m  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)

global xlist mm_neg_ten_occ mm_pos_ten_occ ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  mm_neg_ten_occ_iv mm_pos_ten_occ_iv ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage mm_neg mm_pos cmm_pos_exp cmm_neg_exp ASVAB_ONET_X asvab_m ONET_m  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)

global xlist mm_neg_ten_occ mm_pos_ten_occ ASVAB_ONET_X_ten_occ asvab_m_ten_occ ONET_m_ten_occ tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj 
global ivlist ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv  mm_neg_ten_occ_iv mm_pos_ten_occ_iv ASVAB_ONET_X_ten_occ_iv asvab_m_ten_occ_iv ONET_m_ten_occ_iv
xi: ivregress 2sls lwage mm_neg mm_pos cmm_neg_exp cmm_pos_exp ASVAB_ONET_X asvab_m ONET_m  ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)

probit switch_occ age age2 mm_neg mm_pos $zlist

/*--------------------------------------------------------------------------------------
* name: mismatch_11_xdimphysical.do
* made by: david wiczer, moidfied by: satoshi tanaka
* date: 08/21/2010
*       03/18/2014
* description: this code is for the project 'occupation skill mismatch'
--------------------------------------------------------------------------------------*/

/* this code runs the regressions */ 

/*------------------------------------------------------------------------------------*/

use ${result}/yearly_02.dta, clear
set more off
/*--------------------------*/
* DANIEL: subset here on the second pass
/*--------------------------*/

global sample_select =.
global hlth_comp_use = 1
global hlth_idx_use = 1
global match_centered = 1
global corrmatch = 1
/* put a 1 in only one of the global variables */
/* two-dimensional measures */
global cogmech	= 0
global cogphys	= 0
global totphys	= 0
global xdimphys = 0
global verbmath	= 0
/* three-dimensional measures */
global verbmathphys	= 1
global cogmechphys	= 0
global verbmathmech 	= 0
/* seven-dimensional measure */
global seven = 0

global tot_eq_cog = 1 /*This switches whether "total" ASVAB is all 7 tests or just math and verbal test (only matters for totphys==1) */

if($cogmech == 1 | $cogphys == 1 | $totphys == 1 | $verbmath == 1 | $xdimphys ==1){
	global twodim = 1
	global threedim = 0
	global sevendim = 0
}
else{
	if($verbmathphys == 1 | $cogmechphys == 1 | $verbmathmech == 1){
		global twodim = 0
		global threedim = 1
		global sevendim = 0
	}
	else{
		global twodim = 0
		global threedim = 0
		global sevendim = 1
	}
}

if($verbmath == 1){
	global diminitls "vm"
}
if($verbmathmech == 1){
	global diminitls "vmm"
}
if($verbmathphys == 1){
	global diminitls "vmp"
}
if($totphys == 1){
	global diminitls "tp"
}
if($xdimphys == 1){
	global diminitls "xp"
}

/*
*  stdize = 1 makes the min 0 and standard deviation 1
*  stdize = 2 uses ranks
*/
global stdize = 2

/* generate some  other covariates */

global zlist lths univ hispanic black 

gen hs = (grade>=12)
gen lths = (grade<12)
gen univ = (grade>=16)
gen hispanic = (race == 1)
gen black = (race == 2)
gen yob = year - age
bysort id: egen yob_tmp = mode(yob)
* fix height variable:
by id: egen maxheight = max(height) if age>=25
replace height = maxheight if age >=25 & height == .

label var hs	"High School"
label var lths	"< High School"
label var univ	"4-Year College"
label var hispanic	"Hispanic"
label var black	"Black"
label var yob "Year of birth"

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
label var age "Age"
label var age2 "Age$^2$"

/*------------------------------------------------------------------------------------*/
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
/* defining residual wages */
xtset id year
gen delwage = lwage - l.lwage
 
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
	gen asvab_std`i' = asvab_res`i' /r(sd)
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

/* total skills */
*pca ONET_ASVAB_1-ONET_ASVAB_7, components(1)
pca ONET_expert*
predict skill_0, score
sum skill_0 if skill_0 != .
replace skill_0 = (skill_0 - r(min))/r(sd)  if skill_0 != .

/* cognitive skill */
pca ONET_ASVAB_1-ONET_ASVAB_4, components(1)
predict skill_c, score
sum skill_c if skill_c != .
replace skill_c = (skill_c - r(min))/r(sd)  if skill_c != .

if($tot_eq_cog ==1){
	replace skill_0 = skill_c
}

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

/* raw skills */
forvalues i=1/7{
	gen skill_`i' = ONET_ASVAB_`i'
}

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

/* total ability */
pca asvab_std1-asvab_std7 if ind_indic == 1, components(1)
predict ability_0 if ind_indic == 1, score
sum ability_0 if ability_0 != . & ind_indic == 1
replace ability_0 = (ability_0 - r(min))/r(sd)  if ability_0 != . & ind_indic == 1
bysort id: egen ability_tmp = max(ability_0)
replace ability_0 = ability_tmp
drop ability_tmp

if($tot_eq_cog ==1){
	replace ability_0 = ability_c
}

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

pca hlth_score_* if ind_indic == 1, components(1)
predict ability_h if ind_indic == 1, score
sum ability_h if ability_h != . & ind_indic == 1
replace ability_h = (ability_h - r(min))/r(sd)  if ability_h != . & ind_indic == 1
bysort id: egen ability_tmp = max(ability_h)
replace ability_h = ability_tmp
drop ability_tmp

if($hlth_comp_use ==1){
	replace hlth_composite = . if hlth_composite < 0
	sum hlth_composite if hlth_composite != .
	replace ability_h = (hlth_composite - r(min))/r(sd)  if hlth_composite != .
}

/* create health index */
if($hlth_idx_use ==1){
	replace hlth_composite = . if hlth_composite < 0
	gen hlth_tv = hlth_tvhrs + hlth_tvmins/60
	replace hlth_tv = hlth_tvhrs if hlth_tvhrs<. & hlth_tv==.
	foreach vi of varlist bdad_alive bdad_hlth bdad_death bmom_alive bmom_hlth bmom_death hlth_tv hlth_smoke hlth_alchl_days_2{
		replace `vi' = . if `vi'<0
	}
	sort id
	foreach vi of varlist bdad_alive bdad_hlth bdad_death bmom_alive bmom_hlth bmom_death hlth_tv hlth_smoke{
		by id: egen `vi'_tmp = max(`vi')
		replace `vi' = `vi'_tmp if `vi'_tmp<.
		gen `vi'_obs = `vi'<.
	}
	by id: egen hlth_alchl_days_min = min(hlth_alchl_days_2)
	by id: egen hlth_alchl_days_max = max(hlth_alchl_days_2)
	replace bdad_death = 0 if bdad_alive == 1
	replace bmom_death = 0 if bmom_alive == 1
	label var bdad_death "Dad Death Age"
	label var bmom_death "Mom Death Age"
	label var bdad_alive "Dad Alive"
	label var bmom_alive "Mom Alive"

	xtset id year

	replace bmi = . if bmi<15 | bmi > 40
	gen bmi_init = bmi 
	replace bmi_init = l.bmi_init if id ==l.id & l.bmi_init < .
	gen age_bmi_init = age if bmi==bmi_init

	bysort id:  egen age_bmi_initTMP =min(age_bmi_init)
	replace  age_bmi_init= age_bmi_initTMP
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
	
	global hlth_list bmi_init_p25 bmi_init_m25 bmi_init2 height yob AFQT_std bdad_alive bdad_death bmom_alive bmom_death $zlist
	*reg hlth_composite bmi_init bdad_alive bdad_hlth bdad_death bmom_alive bmom_hlth bmom_death hlth_tv hlth_smoke hlth_alchl_days_max black hispanic i.occ_1d i.ind_1d
	reg hlth_composite $hlth_list
	
	outreg2 using ${result}/table_hlth_comp.tex, bdec(3) tex(fragment) title("Health Index Regression") ctitle("", " ") drop(_I* $zlist) label se replace sortvar(bmi_init height yob AFQT_std bdad_alive bdad_death bmom_alive bmom_death $zlist)
	
	predict hlth_fit if e(sample), xb
	gen hlth_resid = hlth_composite - hlth_fit
	sum hlth_resid
	replace ability_h = .
	replace ability_h = (hlth_fit - r(min))/r(sd)
	drop *_tmp
}

stop

/* raw abilities */
forvalues i=1/7{
	gen ability_`i' = asvab_std`i'
}


/* Export the correlation matrix between health and cog */
corr skill_h skill_0 ability_h ability_0
matrix corr_hlthcog = r(C)
matrix colnames corr_hlthcog = OP OC WP WC
matrix rownames corr_hlthcog = "Occ~Reqs~Phys" "Occ~Reqs~Cog" "Worker~Abilities~Phys" "Worker~Abilities~Cog"
outtable using ${result}/corr_hlthcog, mat(corr_hlthcog) nobox replace f(%9.2f) center caption("Correlations between Physical and Cognitive Scores") clabel(tab:corr_hlthcog)

/* Export the correlation matrix between cog abilities */
corr ability_1-ability_7 ability_v ability_m
matrix corrASVAB = r(C)
matrix colnames corrASVAB = AR  MK   WK   PC   GS   MC Verbal Math
matrix rownames corrASVAB = "Arith~Reasoning" "Math~Knowledge" "Word~Knowledge" "Paragraph~Comp" "General~Sci" "Mech~Comp" "Elec~Info" "Verbal" "Math"
outtable using ${result}/corrASVAB, mat(corrASVAB) nobox replace f(%9.2f) center caption("Individuals' Correlations between Scores") clabel(tab:corrASVAB)


/* Export the correlation matrix between cog skills */
corr skill_1-skill_7 skill_v skill_m
matrix corrONET = r(C)
matrix colnames corrONET = AR  MK   WK   PC   GS  MC EI Verbal Math
matrix rownames corrONET = "Arith~Reasoning" "Math~Knowledge" "Word~Knowledge" "Paragraph~Comp" "General~Sci" "Mech~Comp" "Elec~Info" "Verbal" "Math"
outtable using ${result}/corrONET, mat(corrONET) nobox replace f(%9.2f) center caption("Occupations' Correlations between Scores") clabel(tab:corrONET)


/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* generate mismatch measure */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

forvalues i = 0/7 {
	cumul ability_`i' if ind_indic == 1, gen(ability_rnk_tmp) equal
	bysort id: egen ability_rnk_`i' = max(ability_rnk_tmp)
	drop ability_rnk_tmp
	cumul skill_`i', gen(skill_rnk_`i') equal
	gen mm_`i' = ability_rnk_`i' - skill_rnk_`i'
	gen absmm_`i' = abs(mm_`i')	
}


foreach i in "v" "m" "c" "e" "h" {
	cumul ability_`i' if ind_indic == 1, gen(ability_rnk_tmp) equal
	bysort id: egen ability_rnk_`i' = max(ability_rnk_tmp)
	drop ability_rnk_tmp
	cumul skill_`i', gen(skill_rnk_`i') equal
	gen mm_`i' = ability_rnk_`i' - skill_rnk_`i'
	gen absmm_`i' = abs(mm_`i')
}

/*element-wise mismatch */
pca absmm_1 absmm_2 absmm_3 absmm_4 absmm_5 absmm_6 absmm_7, components(1)
predict absmm_x, score
pca mm_1 mm_2 mm_3 mm_4 mm_5 mm_6 mm_7, components(1)
predict mm_x, score

/* Use the components we need, given the dimensions we are analyzing */

/*three dimensions: */
if($verbmathphys == 1){
	gen absmm_aa = absmm_v
	gen absmm_bb = absmm_m
	gen absmm_cc = absmm_h
	gen mm_aa = mm_v
	gen mm_bb = mm_m
	gen mm_cc = mm_h
}
if($verbmathmech == 1){
	gen absmm_aa = absmm_v
	gen absmm_bb = absmm_m
	gen absmm_cc = absmm_e
	gen mm_aa = mm_v
	gen mm_bb = mm_m
	gen mm_cc = mm_e
}
if($cogmechphys == 1){ 
	gen absmm_aa = absmm_c
	gen absmm_bb = absmm_e
	gen absmm_cc = absmm_h
	gen mm_aa = mm_c
	gen mm_bb = mm_e
	gen mm_cc = mm_h
}

* Do the 2-dimensional
if($cogphys == 1){
	gen absmm_aa = absmm_c
	gen absmm_bb = absmm_h
	gen mm_aa = mm_c
	gen mm_bb = mm_h
}
if($cogmech == 1){
	gen absmm_aa = absmm_c
	gen absmm_bb = absmm_e
	gen mm_aa = mm_c
	gen mm_bb = mm_e

}
if($totphys == 1){
	gen absmm_aa = absmm_0 
	gen absmm_bb = absmm_h
	gen mm_aa = mm_0
	gen mm_bb = mm_h
}
if($verbmath == 1){
	gen absmm_aa = absmm_v
	gen absmm_bb = absmm_m
	gen mm_aa = mm_v
	gen mm_bb = mm_m	
}
if($xdimphys == 1){
	gen absmm_aa = absmm_x
	gen absmm_bb = absmm_h
	gen mm_aa = mm_x
	gen mm_bb = mm_h	
}

/* the seven dimensional */
if($sevendim == 1){
	gen absmm_aa = absmm_1
	gen absmm_bb = absmm_2
	gen absmm_cc = absmm_3
	gen absmm_dd = absmm_4
	gen absmm_ee = absmm_5
	gen absmm_ff = absmm_6
	gen absmm_gg = absmm_7
	gen mm_aa = mm_1
	gen mm_bb = mm_2
	gen mm_cc = mm_3
	gen mm_dd = mm_4
	gen mm_ee = mm_5
	gen mm_ff = mm_6
	gen mm_gg = mm_7
}

gen mm = 0
gen mm_sgn = 0
gen mm_neg = 0
gen mm_pos = 0

local Lwt = 0

if($twodim == 1){
	local nlist "aa bb"
	local ndim =2
}
if($threedim == 1){
	local nlist "aa bb cc"
	local ndim = 3
}
if($sevendim == 1){
	local nlist "aa bb cc dd ee ff gg"
	local ndim = 7
}

foreach i of local nlist {
	gen mm_`i'_neg = 0
	gen mm_`i'_pos = 0
}
factor absmm_??
predict mm_pred /* this is just a check, should be proportional to mm*/
pca absmm_??
estat loadings, cnorm(unit)
matrix L = e(L)
local Li = 1
foreach i of local nlist {
	local Lhere = abs(L[`Li',1])
	local Lwt = `Lwt' +  `Lhere'
	replace mm = mm + absmm_`i'*`Lhere'
	replace mm_sgn = mm_sgn + mm_`i'*`Lhere'
	
	replace mm_neg = mm_neg + mm_`i'*`Lhere' if mm_`i' <0
	replace mm_pos = mm_pos + mm_`i'*`Lhere' if mm_`i' >0
	local Li = `Li' + 1
	replace mm_`i'_neg = mm_`i'_neg + mm_`i'*`Lhere' if mm_`i' <0
	replace mm_`i'_pos = mm_`i'_pos + mm_`i'*`Lhere' if mm_`i' >0
}


/* Split between people with average negative or positive mismatch*/
gen mm_aveneg = mm_neg if abs(mm_neg) > mm_pos
replace mm_aveneg = 0 if abs(mm_neg) < mm_pos
gen mm_avepos = mm_pos if abs(mm_neg) < mm_pos
replace mm_avepos = 0 if abs(mm_neg) > mm_pos

replace mm = mm/`Lwt'
replace mm_sgn = mm_sgn/`Lwt'

replace mm_neg = mm_neg/`Lwt'
replace mm_pos = mm_pos/`Lwt'
replace mm_aveneg = mm_aveneg/`Lwt'
replace mm_avepos = mm_avepos/`Lwt'

foreach i of local nlist {
	gen mm_`i'_neg_ten_occ = mm_neg*tenure_occ
	gen mm_`i'_neg_ten_occ_iv = mm_neg*ten_occ_iv
	gen mm_`i'_pos_ten_occ = mm_pos*tenure_occ
	gen mm_`i'_pos_ten_occ_iv = mm_pos*ten_occ_iv
}

gen mm_ten_occ = mm*tenure_occ
gen mm_ten_occ2 = mm*ten_occ2
gen mm_ten_occ_iv = mm*ten_occ_iv
gen mm_ten_occ2_iv = mm*ten_occ2_iv


gen mm_neg_ten_occ = mm_neg*tenure_occ
gen mm_neg_ten_occ_iv = mm_neg*ten_occ_iv
label var mm_neg "Mismatch Negative Components"	
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

label var mm "Mismatch"

/* create a bunch of rank measures */
foreach mmv of varlist mm mm_pos mm_neg absmm_??{
	cumul `mmv', equal gen(`mmv'_rnk)
}

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* creating a correlation measure */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

if( $stdize==2 ){
	forvalues i=0/7{
		rename	ability_`i' ability_std_`i'
		rename	skill_`i' skill_std_`i'
	}
	forvalues i=0/7{
		rename	ability_rnk_`i' ability_`i'
		rename	skill_rnk_`i' skill_`i'
	}
	foreach i in "v" "m" "c" "e" "h"{
		rename	ability_`i' ability_std_`i'
		rename	skill_`i' skill_std_`i'
	}
	foreach i in "v" "m" "c" "e" "h"{
		rename	ability_rnk_`i' ability_`i'
		rename	skill_rnk_`i' skill_`i'
	}
}

if($verbmath == 1){
	gen ability_aa = ability_v
	gen ability_bb = ability_m
	gen skill_aa = skill_v
	gen skill_bb = skill_m	
}
if($cogphys == 1){
	gen ability_aa = ability_c
	gen ability_bb = ability_h
	gen skill_aa = skill_c
	gen skill_bb = skill_h	
}
if($cogmech == 1){
	gen ability_aa = ability_c
	gen ability_bb = ability_e
	gen skill_aa = skill_c
	gen skill_bb = skill_e
}
if($totphys == 1){
	gen ability_aa = ability_0
	gen ability_bb = ability_h
	gen skill_aa = skill_0
	gen skill_bb = skill_h
}
if($xdimphys == 1){
	gen ability_aa = ability_0
	gen ability_bb = ability_h
	gen skill_aa = skill_0
	gen skill_bb = skill_h
}

if($cogmechphys == 1){
	gen ability_aa = ability_c
	gen ability_bb = ability_e
	gen ability_cc = ability_h
	gen skill_aa = skill_c
	gen skill_bb = skill_e
	gen skill_cc = skill_h

}
if($verbmathphys == 1){
	gen ability_aa = ability_v
	gen ability_bb = ability_m
	gen ability_cc = ability_h
	gen skill_aa = skill_v
	gen skill_bb = skill_m
	gen skill_cc = skill_h
}
if($verbmathmech == 1){
	gen ability_aa = ability_v
	gen ability_bb = ability_m
	gen ability_cc = ability_e
	gen skill_aa = skill_v
	gen skill_bb = skill_m
	gen skill_cc = skill_e
}
if($seven == 1){
	gen ability_aa = ability_1
	gen ability_bb = ability_2
	gen ability_cc = ability_3
	gen ability_dd = ability_4
	gen ability_ee = ability_5
	gen ability_ff = ability_6
	gen ability_gg = ability_7
	gen skill_aa = skill_1
	gen skill_bb = skill_2
	gen skill_cc = skill_3
	gen skill_dd = skill_4
	gen skill_ee = skill_5
	gen skill_ff = skill_6
	gen skill_gg = skill_7
}
/*if($xdimphys == 1){
	egen ability_aa = rowmean(ability_1 ability_2 ability_3 ability_4 ability_5 ability_6 ability_7)
	egen skill_aa = rowmean(skill_1 skill_2 skill_3 skill_4 skill_5 skill_6 skill_7)
	gen ability_bb = ability_h
	gen skill_bb = skill_h
}
*/

if($sevendim ==1){
	label var ability_aa "Worker Ability aa"
	label var ability_bb "Worker Ability bb"
	label var ability_cc "Worker Ability cc"
	label var ability_aa "Worker Ability dd"
	label var ability_bb "Worker Ability ee"
	label var ability_cc "Worker Ability ff"
	label var ability_cc "Worker Ability gg"
	label var skill_aa "Occ Reqs aa"
	label var skill_bb "Occ Reqs bb"
	label var skill_cc "Occ Reqs cc"
	label var skill_aa "Occ Reqs dd"
	label var skill_bb "Occ Reqs ee"
	label var skill_cc "Occ Reqs ff"
	label var skill_cc "Occ Reqs gg"
}

if($twodim == 1){
	gen ability_mean = (ability_aa + ability_bb )/2
	gen skill_mean  = (skill_aa + skill_bb )/2
	egen ability_stdev = rowsd(ability_aa ability_bb )
	egen skill_stdev = rowsd(skill_aa skill_bb )
}
if($threedim ==1){
	gen ability_mean = (ability_aa + ability_bb + ability_cc)/3
	gen skill_mean  = (skill_aa + skill_bb + skill_cc)/3
	egen ability_stdev = rowsd(ability_aa ability_bb ability_cc)
	egen skill_stdev = rowsd(skill_aa skill_bb skill_cc)
}
if($sevendim ==1){
	gen ability_mean = (ability_aa + ability_bb + ability_cc + ability_dd + ability_ee + ability_ff + ability_gg)/7
	gen skill_mean  = (skill_aa + skill_bb + skill_cc + skill_dd + skill_ee + skill_ff + skill_gg)/7
	egen ability_stdev = rowsd(ability_aa ability_bb ability_cc ability_dd ability_ee ability_ff ability_gg)
	egen skill_stdev = rowsd(skill_aa skill_bb skill_cc skill_dd skill_ee skill_ff skill_gg)
}

gen match = 0

if($twodim == 1){
	local nlist "aa bb"
	local ndim =2
}
if($threedim == 1){
	local nlist "aa bb cc"
	local ndim = 3
}
if($sevendim == 1){
	local nlist "aa bb cc dd ee ff gg"
	local ndim = 7
}
foreach i of local nlist{
	if($match_centered == 1){
		gen match_`i' = (ability_`i' - ability_mean)*(skill_`i' - skill_mean)
		replace match = match + 1/(`ndim'-1)*match_`i'
		
	}
	else{
		gen match_`i' = ability_`i'*skill_`i'
		replace match = match + 1/(`ndim'-1)*match_`i'
	}
}
if($corrmatch == 1){
	replace match = match/ability_stdev/skill_stdev
}

if($xdimphys == 1){
	gen ability_mean_1 = (ability_1 + ability_2 + ability_3 + ability_4 + ability_5 + ability_6 + ability_7)/7
	gen skill_mean_1 = (skill_1 + skill_2 + skill_3 + skill_4 + skill_5 + skill_6 + skill_7)/7
	egen ability_stdev_1 = rowsd(ability_1  ability_2  ability_3  ability_4  ability_5  ability_6  ability_7)
	egen skill_stdev_1 = rowsd(skill_1  skill_2  skill_3  skill_4  skill_5  skill_6  skill_7)
	replace match_aa = 0
	forvalues i=1/7{
		replace match_aa = match_aa + (ability_`i' - ability_mean_1)*(skill_`i' - skill_mean_1)
	}
	replace match_aa = match_aa/ability_stdev_1/skill_stdev_1/6
	qui sum match_bb, meanonly
	replace match_bb = (match_bb-r(min) )*2/(r(max)-r(min))-1
	
}
if($twodim == 1){
	qui replace match_aa = ability_aa*skill_aa
	qui replace match_bb = ability_bb*skill_bb
	qui sum match_aa
	replace match_aa = (match_aa-r(min) )/(r(max)-r(min))
	qui sum match_bb
	replace match_bb = (match_bb-r(min) )/(r(max)-r(min))
}


label var match "Match"

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* create cumulative measures */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

sort id year
gen cmatch_cur = 0
gen cmm_cur = 0
by id: replace cmatch_cur = sum(match)/exp
by id: replace cmm_cur 	= sum(mm)/exp

if($twodim == 1){
	local nlist "aa bb"
}
if($threedim == 1){
	local nlist "aa bb cc"
}
if($sevendim == 1){
	local nlist "aa bb cc dd ee ff gg"
}

foreach i of local nlist{
	gen cmatch_cur_`i' = 0
	gen cmm_cur_`i' = 0
	by id: replace cmatch_cur_`i' = sum(match_`i')/exp
	by id: replace cmm_cur_`i' = sum(absmm_`i')/exp
	gen cmatch_exp_`i'= cmatch_cur_`i'*exp
	gen cmm_exp_`i'	= cmm_cur_`i'*exp
}

/* asvab and onet interaction */
gen ability_skill_mean = ability_mean*skill_mean
gen ability_skill_mean_ten_occ = ability_skill_mean*tenure_occ/100
gen ability_skill_mean_ten_occ2 = ability_skill_mean*ten_occ2
gen ability_skill_mean_ten_occ_iv = ability_skill_mean*ten_occ_iv/100
gen ability_skill_mean_ten_occ2_iv = ability_skill_mean*ten_occ2_iv

label var ability_skill_mean "Worker Ability $\times$ Occ Reqs $\times$ 100"
label var ability_skill_mean_ten_occ "Worker Ability $\times$ Occ Reqs $\times$ Occ Tenure"
label var ability_skill_mean_ten_occ2 "Worker Ability $\times$ Occ Reqs $\times$ Occ Tenure$^2 \times$ 100"

gen ability_mean_ten_occ = ability_mean*tenure_occ
gen ability_mean_ten_occ2 = ability_mean*ten_occ2
gen ability_mean_ten_occ_iv = ability_mean*ten_occ_iv
gen ability_mean_ten_occ2_iv = ability_mean*ten_occ2_iv
gen skill_mean_ten_occ = skill_mean*tenure_occ
gen skill_mean_ten_occ2 = skill_mean*ten_occ2
gen skill_mean_ten_occ_iv = skill_mean*ten_occ_iv
gen skill_mean_ten_occ2_iv = skill_mean*ten_occ2_iv

label var ability_mean "Mean Worker Ability"
label var skill_mean "Mean Occ Reqs"
label var ability_mean_ten_occ "Worker Ability $\times$ Occ Tenure"
label var ability_mean_ten_occ2 "Worker Ability $\times$ Occ Tenure$^2 \times$ 100"
label var skill_mean_ten_occ "Occ Reqs $\times$ Occ Tenure"
label var skill_mean_ten_occ2 "Occ Reqs $\times$ Occ Tenure$^2 \times$ 100"

gen match_ten_occ = match*tenure_occ
gen match_ten_occ2 = match*ten_occ2
gen match_ten_occ_iv = match*ten_occ_iv
gen match_ten_occ2_iv = match*ten_occ2_iv

gen cmatch_exp 	= cmatch_cur*exp
gen cmm_exp 	= cmm_cur*exp
gen cmatch_exp_iv= cmatch_cur*exp_iv
gen cmm_exp_iv	= cmm_cur*exp_iv

label var match_ten_occ "Match $\times$ Occ Tenure"
label var mm_ten_occ "Mismatch $\times$ Occ Tenure"
label var mm_ten_occ "Misatch $\times$ Occ Tenure"
label var cmatch_exp "Cumul Match $\times$ Exper"
label var cmm_exp "Cumul Mismatch $\times$ Exper"
label var cmatch_cur "Cumul Match "
label var cmm_cur "Cumul Mismatch "


if($twodim == 1){
	local nlist "aa bb"
}
if($threedim == 1){
	local nlist "aa bb cc"
}
if($sevendim == 1){
	local nlist "aa bb cc dd ee ff gg"
}

foreach i of local nlist{
	gen match_`i'_ten_occ = match_`i'*tenure_occ
	gen match_`i'_ten_occ_iv = match_`i'*ten_occ_iv
	gen ability_`i'_ten_occ = ability_`i'*tenure_occ
	gen ability_`i'_ten_occ_iv = ability_`i'*ten_occ_iv
	gen skill_`i'_ten_occ = skill_`i'*tenure_occ
	gen skill_`i'_ten_occ_iv = skill_`i'*ten_occ_iv

	gen absmm_`i'_ten_occ = absmm_`i'*tenure_occ
	gen absmm_`i'_ten_occ_iv = absmm_`i'*ten_occ_iv

	gen cmatch_`i'_exp 	= cmatch_cur_`i'*exp
	gen cmm_`i'_exp 	= cmm_cur_`i'*exp
	gen cmatch_`i'_exp_iv= cmatch_cur_`i'*exp_iv
	gen cmm_`i'_exp_iv	= cmm_cur_`i'*exp_iv
	
	label var ability_`i' "Worker Ability `i'"
	label var skill_`i' "Occ Reqs `i'"
	label var ability_`i'_ten_occ "Worker Ability `i' $\times$ Occ Tenure"
	label var skill_`i'_ten_occ "Occ Reqs `i' $\times$ Occ Tenure"		
	
	label var cmatch_`i'_exp "Cumul Match `i' $\times$ Exper"
	label var match_`i'_ten_occ "Match  `i' $\times$ Occ Tenure"
	label var cmm_`i'_exp "Cumul Mismatch `i' $\times$ Exper"
	label var absmm_`i'_ten_occ "Mismatch `i' $\times$ Occ Tenure"
	
	label var cmatch_cur_`i' "Cumul Match `i'"
	label var cmm_cur_`i' "Cumul Mismatch `i'"
	label var match_`i' "Match `i'"
	label var absmm_`i' "Mismatch `i'"
	label var mm_`i'_neg "Negative Mismatch `i'"
	label var mm_`i'_pos "Positive Mismatch `i'"
}
if($totphys == 1){
	local llist "Cog Phys"
}
if($verbmath == 1){
	local llist "Verbal Math"
}
if($verbmathphys == 1){
	local llist "Verbal Math Phys"
}
local ll = 1
foreach i of local nlist{

	local l: word `ll' of `llist'
	label var ability_`i' "Worker Ability `l'"
	label var skill_`i' "Occ Reqs `l'"
	label var ability_`i'_ten_occ "Worker Ability `l' $\times$ Occ Tenure"
	label var skill_`i'_ten_occ "Occ Reqs `l' $\times$ Occ Tenure"		
	
	label var cmatch_`i'_exp "Cumul Match `l' $\times$ Exper"
	label var match_`i'_ten_occ "Match  `l' $\times$ Occ Tenure"
	label var cmm_`i'_exp "Cumul Mismatch `l' $\times$ Exper"
	label var absmm_`i'_ten_occ "Mismatch `l' $\times$ Occ Tenure"
	
	label var cmatch_cur_`i' "Cumul Match `l'"
	label var cmm_cur_`i' "Cumul Mismatch `l'"
	label var match_`i' "Match `l'"
	label var absmm_`i' "Mismatch `l'"
	label var mm_`i'_neg "Negative Mismatch `l'"
	label var mm_`i'_pos "Positive Mismatch `l'"
	label var mm_`i'_neg_ten_occ "Neg Mismatch `l' $\times$ Occ Tenure"
	label var mm_`i'_pos_ten_occ "Pos Mismatch `l' $\times$ Occ Tenure"
	
	local ll = `ll' + 1
}

/*------------------------------------------------------------------------------------*/
/* create average cumulative measures */
/*------------------------------------------------------------------------------------*/


egen idocc = group(id occ)

xtset id year
by id: egen nswitch = total(switch_occ)
by id: gen switch_count = sum(switch_occ)

gen lmm = l.mm if switch_occ==1
replace lmm = l2.mm if switch_occ==1 & lmm ==. /* go back 2 periods if a missing obs */
replace lmm = l.lmm if switch_occ==0 & lmm ==.
gen lmatch = l.match if id == l.id & switch_occ==1
replace lmatch = l2.match if switch_occ==1 & lmatch ==.
replace lmatch  = l.lmatch if switch_occ==0  & lmatch ==. 
gen lten_occ = l.tenure_occ if id == l.id & switch_occ==1
replace lten_occ  = l2.tenure_occ  if switch_occ==1 & lten_occ ==.
replace lten_occ = l.lten_occ if switch_occ==0 /*BIG CHANGE HERE, CHECK IF IT MESSED THINGS UP!! */

gen loccwage = l.lwage if switch_occ==1
replace loccwage = l2.lwage if switch_occ==1 & loccwage ==. /* go back 2 periods if a missing obs */
replace loccwage = l.loccwage  if switch_occ==0 & loccwage ==.

gen loccten = l.tenure_occ if switch_occ==1
replace loccten = l2.tenure_occ if switch_occ==1 & loccten ==. /* go back 2 periods if a missing obs */
replace loccten = l.loccten if switch_occ==0 & loccten ==.



if($twodim == 1){
	local nlist "aa bb"
}
if($threedim == 1){
	local nlist "aa bb cc"
}
if($sevendim == 1){
	local nlist "aa bb cc dd ee ff gg"
}

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
	gen lmatch_`i' = l.match_`i' if id == l.id & switch_occ==1
	replace lmatch_`i' = l2.match_`i' if switch_occ==1 & lmatch_`i'==.
	replace lmatch_`i'  = l.lmatch_`i' if switch_occ==0
}

gen lmm_neg = l.mm_neg if id == l.id & switch_occ==1
replace lmm_neg = l.lmm_neg if switch_occ==0
gen lmm_pos = l.mm_pos if id == l.id & switch_occ==1
replace lmm_pos = l.lmm_pos if switch_occ==0
gen lmm_sgn = l.mm_sgn if id == l.id & switch_occ==1
replace lmm_sgn = l.lmm_sgn if switch_occ==0

bysort idocc: egen lmm_gapfill = min(lmm)
replace lmm = lmm_gapfill if lmm==. & lmm_gapfill <. & switch_count>0
by idocc: egen lmm_sgn_gapfill = min(lmm_sgn)
replace lmm_sgn= lmm_sgn_gapfill if lmm_sgn==. & lmm_sgn_gapfill <. & switch_count>0
by idocc: egen lmm_neg_gapfill = min(lmm_neg)
replace lmm_neg = lmm_neg_gapfill if lmm_neg==. & lmm_neg_gapfill <. & switch_count>0
by idocc: egen lmm_pos_gapfill = min(lmm_pos)
replace lmm_pos = lmm_pos_gapfill if lmm_pos==. & lmm_pos_gapfill <. & switch_count>0
by idocc: egen lmatch_gapfill = min(lmatch)
replace lmatch = lmatch_gapfill if lmatch==. & lmatch_gapfill <. & switch_count>0
by idocc: egen lten_gapfill = min(lten_occ)
replace lten_occ = lten_gapfill if lten_occ==. & lten_gapfill <. & switch_count>0
foreach i of local nlist{
	by idocc: egen lmm_`i'_gapfill = min(lmm_`i')
	replace lmm_`i' = lmm_`i'_gapfill if lmm_`i'==. & lmm_`i'_gapfill <. & switch_count>0
	by idocc: egen lmm_`i'_pos_gapfill = min(lmm_`i'_pos)
	replace lmm_`i'_pos = lmm_`i'_pos_gapfill if lmm_`i'_pos==. & lmm_`i'_pos_gapfill <. & switch_count>0
	by idocc: egen lmm_`i'_neg_gapfill = min(lmm_`i'_neg)
	replace lmm_`i'_neg = lmm_`i'_neg_gapfill if lmm_`i'_neg==. & lmm_`i'_neg_gapfill <. & switch_count>0
	by idocc: egen lmatch_`i'_gapfill = min(lmatch_`i')
	replace lmatch_`i' = lmatch_`i'_gapfill if lmatch_`i'==. & lmatch_`i'_gapfill <. & switch_count>0
}
drop l*_gapfill

label var lmm "Last Mismatch"
label var lmm_pos "Last Mismatch, Pos"
label var lmm_neg "Last Mismatch, Neg"
label var lmatch "Last Match Quality"


gsort +id -switch_occ +year
by id: gen cmatch_ave = sum(lmatch*lten_occ) if switch_occ==1 & lmatch<. & lten_occ<.
by id: gen cmm_ave = sum(lmm*lten_occ)  if switch_occ==1  & lmm<. & lten_occ<.

by id: gen cmm_neg_ave = sum(lmm_neg*lten_occ)  if switch_occ==1 
by id: gen cmm_pos_ave = sum(lmm_pos*lten_occ)  if switch_occ==1 

by id: gen totexp = sum(lten_occ) if switch_occ==1
replace cmatch_ave = cmatch_ave/totexp if switch_occ==1 
replace cmm_ave = cmm_ave/totexp if switch_occ==1 
replace cmm_neg_ave = cmm_neg_ave/totexp if switch_occ==1 
replace cmm_pos_ave = cmm_pos_ave/totexp if switch_occ==1 

foreach i of local nlist{
	by id: gen cmatch_ave_`i' = sum(lmatch_`i'*lten_occ) if switch_occ==1  & lmatch_`i'<. & lten_occ<.
	by id: gen cmm_ave_`i' = sum(lmm_`i'*lten_occ)  if switch_occ==1  & lmm_`i'<. & lten_occ<.
	replace cmatch_ave_`i' = cmatch_ave_`i'/totexp if switch_occ==1 
	replace cmm_ave_`i' = cmm_ave_`i'/totexp if switch_occ==1 
		
	by id: gen cmm_neg_ave_`i' = sum(lmm_`i'_neg*lten_occ)  if switch_occ==1 
	by id: gen cmm_pos_ave_`i' = sum(lmm_`i'_pos*lten_occ)  if switch_occ==1 
}

xtset id year
replace cmatch_ave = l.cmatch_ave if switch_occ==0 & match<.
replace cmm_ave = l.cmm_ave if switch_occ==0 & mm<.

replace cmm_neg_ave = l.cmm_neg_ave if switch_occ==0 & mm<.
replace cmm_pos_ave = l.cmm_pos_ave if switch_occ==0 & mm<.

foreach i of local nlist{
	replace cmatch_ave_`i' = l.cmatch_ave_`i' if switch_occ==0 & match<.
	replace cmm_ave_`i' = l.cmm_ave_`i' if switch_occ==0 & mm<.
	replace cmm_neg_ave_`i' = l.cmm_neg_ave_`i' if switch_occ==0 & mm<.
	replace cmm_pos_ave_`i' = l.cmm_pos_ave_`i' if switch_occ==0 & mm<.
	label var cmatch_ave_`i' "Cumul Match `i'"
	label var cmm_ave_`i' "Cumul Mismatch `i'"	
}
label var cmatch_ave "Cumul Match"
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
by idocc: egen cmatch_gapfill = min(cmatch_ave)
replace cmatch_ave = cmatch_gapfill if cmatch_ave ==. & cmatch_gapfill <. & switch_count>0
foreach i of local nlist{
	by idocc: egen cmm_`i'_gapfill = min(cmm_ave_`i')
	replace cmm_ave_`i' = cmm_`i'_gapfill if cmm_ave_`i'==. & cmm_`i'_gapfill <. & switch_count>0
	by idocc: egen cmatch_`i'_gapfill = min(cmatch_ave_`i')
	replace cmatch_ave_`i' = cmatch_`i'_gapfill if cmatch_ave_`i'==. & cmatch_`i'_gapfill <. & switch_count>0
}
drop c*_gapfill


if($twodim == 1){
	local nlist "aa bb"
}
if($threedim == 1){
	local nlist "aa bb cc"
}
if($sevendim == 1){
	local nlist "aa bb cc dd ee ff gg"
}
if($totphys == 1){
	local llist "Cog Phys"
}
if($verbmath == 1){
	local llist "Verbal Math"
}
if($verbmathphys == 1){
	local llist "Verbal Math Phys"
}
local ll = 1
foreach i of local nlist{
	local l: word `ll' of `llist'
	label var lmm_`i' "Last Mismatch, `l'"
	label var lmatch_`i' "Last Match Quality, `l'"
	label var lmm_`i'_neg "Last Mismatch Neg, `l'"
	label var lmm_`i'_pos "Last Mismatch Pos, `l'"
	label var cmm_ave_`i' "Cumul Mismatch, `l'"
	label var cmatch_ave_`i' "Cumul Mismatch, `l'"
	label var cmm_neg_ave_`i' "Cumul Mismatch Neg, `l'"
	label var cmm_pos_ave_`i' "Cumul Mismatch Pos, `l'"	
	local ll = `ll' + 1
}

save ${result}/yearly_03_phys_${diminitls}.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* estimation */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

global zlist_0 lths univ hispanic black AFQT_std
/* this is the same as 	global hlth_list */
global zlist bmi_init_p25 bmi_init_m25 bmi_init2 height yob AFQT_std bdad_alive bdad_death bmom_alive bmom_death $zlist
	
global xlist_0 tenure_emp ten_emp2 tenure_occ ten_occ2 ten_occ3 exp exp2 exp3 oj
global ivlist_0 ten_emp_iv ten_emp2_iv ten_occ_iv ten_occ2_iv ten_occ3_iv exp_iv exp2_iv exp3_iv oj_iv

/* benchmark ols regression */

use ${result}/yearly_03_phys_${diminitls}.dta, clear
keep if $sample_select
xtset id year
global xlist $xlist_0
xi: reg lwage $xlist $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/bench_ols.ster, replace

/* benchmarnk iv regression (Altonji and Shakotko) */

use ${result}/yearly_03_phys_${diminitls}.dta, clear
keep if $sample_select
xtset id year
global xlist $xlist_0
global ivlist $ivlist_0
xi: ivregress 2sls lwage ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/bench_iv.ster, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* mismatch */

global xlist mm_ten_occ $xlist_0
global ivlist $ivlist_0 mm_ten_occ_iv
xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_mm.ster, replace

global xlist mm_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist $ivlist_0 mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv 
xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_mm_means.ster, replace

xi: reg lwage mm $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/ols_mm_means.ster, replace

global xlist mm_pos_ten_occ mm_neg_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist $ivlist_0 mm_pos_ten_occ_iv mm_neg_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv 
xi: ivregress 2sls lwage mm_pos mm_neg ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_mm_means_pos_neg.ster, replace

xi: reg lwage mm_pos mm_neg $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/ols_mm_means_pos_neg.ster, replace


xtset id year
global xlist mm_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ ability_skill_mean_ten_occ
global ivlist $ivlist_0 mm_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage mm ($xlist = $ivlist) $zlist ability_mean skill_mean ability_skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_mm_means_x.ster, replace

/* creating a table */
estimate use ${result}/bench_iv.ster
outreg2 using ${result}/table_${diminitls}_mm.tex, bdec(3) tex(fragment) title("Mismatch Regression") ctitle("", "Benchmark IV") drop(_I* $zlist) label se replace sortvar(mm* $xlist $zlist)
estimate use ${result}/iv_mm.ster
outreg2 using ${result}/table_${diminitls}_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(mm* $xlist $zlist)
estimate use ${result}/iv_mm_means.ster
outreg2 using ${result}/table_${diminitls}_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(mm* $xlist $zlist)
estimate use ${result}/iv_mm_means_x.ster
outreg2 using ${result}/table_${diminitls}_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(mm* $xlist $zlist)


/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* cumulative mismatch */

global xlist  $xlist_0
global ivlist $ivlist_0
xi: ivregress 2sls lwage cmm_ave ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_cmm.ster, replace

global xlist  mm_ten_occ $xlist_0
global ivlist mm_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage mm cmm_ave ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_cmm_mm.ster, replace

global xlist  $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv
xi: ivregress 2sls lwage cmm_ave ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_cmm_means.ster, replace

global xlist  mm_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist mm_ten_occ_iv $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv
xi: ivregress 2sls lwage mm cmm_ave ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_cmm_mm_means.ster, replace
predict resid_mm_cmm_means, resid
gen r2_mm_cmm_means = resid_mm_cmm_means*resid_mm_cmm_means

xi: reg lwage mm cmm_ave $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust) /*  [aw=sweight]*/
estimate save ${result}/ols_cmm_mm_means.ster, replace

global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist mm_pos_ten_occ_iv mm_neg_ten_occ_iv $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv
xi: ivregress 2sls lwage mm_neg mm_pos cmm_pos_ave cmm_neg_ave ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust) /*  [aw=sweight]*/
estimate save ${result}/iv_cmm_mm_means_pos_neg.ster, replace

xi: reg lwage mm_neg mm_pos cmm_pos_ave cmm_neg_ave $xlist $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust) /*  [aw=sweight]*/
estimate save ${result}/ols_cmm_mm_means_pos_neg.ster, replace


global xlist  $xlist_0 ability_mean_ten_occ skill_mean_ten_occ  ability_skill_mean_ten_occ
global ivlist $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage cmm_ave ($xlist = $ivlist) $zlist ability_mean skill_mean ability_skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_cmm_x.ster, replace

global xlist  mm_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ  ability_skill_mean_ten_occ
global ivlist mm_ten_occ_iv $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage mm cmm_ave ($xlist = $ivlist) $zlist ability_mean skill_mean ability_skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_cmm_mm_x.ster, replace

/* creating a table */
estimate use ${result}/bench_iv.ster
outreg2 using ${result}/table_${diminitls}_cmm.tex, bdec(3) tex(fragment) title("Cumulative Mismatch Regression") ctitle("", "Benchmark IV") drop(_I* $zlist) label se replace sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmm.ster
outreg2 using ${result}/table_${diminitls}_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* mm mm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmm_mm.ster
outreg2 using ${result}/table_${diminitls}_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* mm mm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmm_means.ster
outreg2 using ${result}/table_${diminitls}_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_*  mm mm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmm_mm_means.ster
outreg2 using ${result}/table_${diminitls}_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_*  mm mm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmm_x.ster
outreg2 using ${result}/table_${diminitls}_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_*  mm mm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmm_mm_x.ster
outreg2 using ${result}/table_${diminitls}_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_*  mm mm_* cmatch_* cmm_* $xlist $zlist)



/* individual component mismatch */

global xlist absmm_??_ten_occ $xlist_0
global ivlist $ivlist_0 absmm_??_ten_occ_iv
xi: ivregress 2sls lwage absmm_?? ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_mm.ster, replace

global xlist absmm_??_ten_occ $xlist_0 ability_??_ten_occ skill_??_ten_occ
global ivlist $ivlist_0 absmm_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv 
xi: ivregress 2sls lwage absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_mm_means.ster, replace

xi: reg lwage absmm_?? $xlist $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/ols_ind_mm_means.ster, replace


/* THIS REGERSSION DOESN"T MAKE SENSE: COLINEAR
global xlist mm_??_pos_ten_occ mm_??_neg_ten_occ $xlist_0 ability_??_ten_occ skill_??_ten_occ
global ivlist $ivlist_0 mm_??_pos_ten_occ_iv mm_??_neg_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv 
xi: ivregress 2sls lwage mm_??_pos mm_??_neg ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_mm_means_pos_neg.ster, replace
*/

global xlist absmm_??_ten_occ $xlist_0 ability_??_ten_occ skill_??_ten_occ ability_skill_mean_ten_occ
global ivlist $ivlist_0 absmm_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? ability_skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_mm_means_x.ster, replace

/* creating a table */
estimate use ${result}/bench_iv.ster
outreg2 using ${result}/table_${diminitls}_ind_mm.tex, bdec(3) tex(fragment) title("Mismatch Regression") ctitle("", "Benchmark IV") drop(_I* $zlist) label se replace sortvar(mm_* absmm_* cmm_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_mm.ster
outreg2 using ${result}/table_${diminitls}_ind_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_mm_means.ster
outreg2 using ${result}/table_${diminitls}_ind_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_mm_means_x.ster
outreg2 using ${result}/table_${diminitls}_ind_mm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* individual component cumulative mismatch */

global xlist  $xlist_0
global ivlist $ivlist_0
xi: ivregress 2sls lwage cmm_ave_?? ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmm.ster, replace

global xlist  absmm_??_ten_occ $xlist_0
global ivlist absmm_??_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage cmm_ave_?? absmm_?? ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmm_mm.ster, replace

global xlist  $xlist_0 ability_??_ten_occ skill_??_ten_occ
global ivlist $ivlist_0 ability_??_ten_occ_iv skill_??_ten_occ_iv
xi: ivregress 2sls lwage cmm_ave_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_cmm_means.ster, replace

global xlist  absmm_??_ten_occ $xlist_0 ability_??_ten_occ skill_??_ten_occ
global ivlist absmm_??_ten_occ_iv $ivlist_0 ability_??_ten_occ_iv skill_??_ten_occ_iv
xi: ivregress 2sls lwage cmm_ave_?? absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmm_mm_means.ster, replace

xi: reg lwage cmm_ave_?? absmm_?? $xlist $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ols_ind_cmm_mm_means.ster, replace

/* THIS REGRESSION DOESN'T MAKE SENSE: COLINEAR
global xlist  mm_??_neg_ten_occ mm_??_pos_ten_occ $xlist_0 ability_??_ten_occ skill_??_ten_occ
global ivlist mm_??_neg_ten_occ_iv mm_??_pos_ten_occ_iv $ivlist_0 ability_??_ten_occ_iv skill_??_ten_occ_iv
xi: ivregress 2sls lwage cmm_pos_ave_?? cmm_neg_ave_?? mm_??_pos mm_??_neg ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmm_mm_means_pos_neg.ster, replace
*/

global xlist  $xlist_0 ability_??_ten_occ skill_??_ten_occ  ability_skill_mean_ten_occ
global ivlist $ivlist_0 ability_??_ten_occ_iv skill_??_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage cmm_ave_?? ($xlist = $ivlist) $zlist ability_?? skill_?? ability_skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmm_x.ster, replace

global xlist  absmm_??_ten_occ  $xlist_0 ability_??_ten_occ skill_??_ten_occ  ability_skill_mean_ten_occ
global ivlist absmm_??_ten_occ_iv  $ivlist_0 ability_??_ten_occ_iv skill_??_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage cmm_ave_?? absmm_?? ($xlist = $ivlist) $zlist ability_?? skill_?? ability_skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmm_mm_x.ster, replace

/* creating a table */
estimate use ${result}/bench_iv.ster
outreg2 using ${result}/table_${diminitls}_ind_cmm.tex, bdec(3) tex(fragment) title("Cumulative Mismatch Regression") ctitle("", "Benchmark IV") drop(_I* $zlist) label se replace sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmm.ster
outreg2 using ${result}/table_${diminitls}_ind_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmm_mm.ster
outreg2 using ${result}/table_${diminitls}_ind_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmm_means.ster
outreg2 using ${result}/table_${diminitls}_ind_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmm_mm_means.ster
outreg2 using ${result}/table_${diminitls}_ind_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmm_x.ster
outreg2 using ${result}/table_${diminitls}_ind_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmm_mm_x.ster
outreg2 using ${result}/table_${diminitls}_ind_cmm.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* MATCH QUALITY */
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/


/* match */

global xlist match_ten_occ $xlist_0
global ivlist $ivlist_0 match_ten_occ_iv
xi: ivregress 2sls lwage match ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_match.ster, replace

global xlist match_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist $ivlist_0 match_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv 
xi: ivregress 2sls lwage match ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_match_means.ster, replace

xtset id year
global xlist match_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ ability_skill_mean_ten_occ
global ivlist $ivlist_0 match_ten_occ_iv ability_mean_ten_occ_iv skill_mean_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage match ($xlist = $ivlist) $zlist ability_mean skill_mean ability_skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_match_means_x.ster, replace

/* creating a table */
estimate use ${result}/bench_iv.ster
outreg2 using ${result}/table_${diminitls}_match.tex, bdec(3) tex(fragment) title("Match Regression") ctitle("", "Benchmark IV") drop(_I* $zlist) label se replace sortvar(match* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_match.ster
outreg2 using ${result}/table_${diminitls}_match.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_match_means.ster
outreg2 using ${result}/table_${diminitls}_match.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se  sortvar(match* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_match_means_x.ster
outreg2 using ${result}/table_${diminitls}_match.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se  sortvar(match* absmm_* cmatch_* cmm_* $xlist $zlist)


/* cumulative match */

global xlist  $xlist_0
global ivlist $ivlist_0
xi: ivregress 2sls lwage cmatch_ave ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_cmatch.ster, replace

global xlist  match_ten_occ $xlist_0
global ivlist match_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage match cmatch_ave ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_cmatch_match.ster, replace

global xlist  $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv
xi: ivregress 2sls lwage cmatch_ave ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_cmatch_means.ster, replace

global xlist  match_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist match_ten_occ_iv $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv
xi: ivregress 2sls lwage match cmatch_ave ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_cmatch_match_means.ster, replace

global xlist  $xlist_0 ability_mean_ten_occ skill_mean_ten_occ  ability_skill_mean_ten_occ
global ivlist $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage cmatch_ave ($xlist = $ivlist) $zlist ability_mean skill_mean ability_skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_cmatch_x.ster, replace

global xlist  match_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ  ability_skill_mean_ten_occ
global ivlist match_ten_occ_iv $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage match cmatch_ave ($xlist = $ivlist) $zlist ability_mean skill_mean ability_skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_cmatch_match_x.ster, replace

/* creating a table */
estimate use ${result}/bench_iv.ster
outreg2 using ${result}/table_${diminitls}_cmatch.tex, bdec(3) tex(fragment) title("Cumulative Match Regression") ctitle("", "Benchmark IV") drop(_I* $zlist) label se replace sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmatch.ster
outreg2 using ${result}/table_${diminitls}_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmatch_match.ster
outreg2 using ${result}/table_${diminitls}_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmatch_means.ster
outreg2 using ${result}/table_${diminitls}_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmatch_match_means.ster
outreg2 using ${result}/table_${diminitls}_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmatch_x.ster
outreg2 using ${result}/table_${diminitls}_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_cmatch_match_x.ster
outreg2 using ${result}/table_${diminitls}_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)

/* individual component */

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* individual component match */

global xlist match_??_ten_occ $xlist_0
global ivlist $ivlist_0 match_??_ten_occ_iv
xi: ivregress 2sls lwage match_?? ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_match.ster, replace

global xlist match_??_ten_occ $xlist_0 ability_??_ten_occ skill_??_ten_occ
global ivlist $ivlist_0 match_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv 
xi: ivregress 2sls lwage match_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_match_means.ster, replace

global xlist match_??_ten_occ $xlist_0 ability_??_ten_occ skill_??_ten_occ ability_skill_mean_ten_occ
global ivlist $ivlist_0 match_??_ten_occ_iv ability_??_ten_occ_iv skill_??_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage match_?? ($xlist = $ivlist) $zlist ability_?? skill_?? ability_skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_match_means_x.ster, replace

/* creating a table */
estimate use ${result}/bench_iv.ster
outreg2 using ${result}/table_${diminitls}_ind_match.tex, bdec(3) tex(fragment) title("Match Regression") ctitle("", "Benchmark IV") drop(_I* $zlist) label se replace sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_match.ster
outreg2 using ${result}/table_${diminitls}_ind_match.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_match_means.ster
outreg2 using ${result}/table_${diminitls}_ind_match.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_match_means_x.ster
outreg2 using ${result}/table_${diminitls}_ind_match.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* individual component cumulative match */

global xlist  $xlist_0
global ivlist $ivlist_0
xi: ivregress 2sls lwage cmatch_ave_?? ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmatch.ster, replace

global xlist  match_??_ten_occ $xlist_0
global ivlist match_??_ten_occ_iv $ivlist_0
xi: ivregress 2sls lwage cmatch_ave_?? match_?? ($xlist = $ivlist) $zlist i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/iv_ind_cmatch_match.ster, replace

global xlist  $xlist_0 ability_??_ten_occ skill_??_ten_occ
global ivlist $ivlist_0 ability_??_ten_occ_iv skill_??_ten_occ_iv
xi: ivregress 2sls lwage cmatch_ave_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_cmatch_means.ster, replace

global xlist  match_??_ten_occ  $xlist_0 ability_??_ten_occ skill_??_ten_occ
global ivlist match_??_ten_occ_iv $ivlist_0 ability_??_ten_occ_iv skill_??_ten_occ_iv
xi: ivregress 2sls lwage cmatch_ave_?? match_?? ($xlist = $ivlist) $zlist ability_?? skill_?? i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_cmatch_match_means.ster, replace

global xlist  $xlist_0 ability_??_ten_occ skill_??_ten_occ  ability_skill_mean_ten_occ
global ivlist $ivlist_0 ability_??_ten_occ_iv skill_??_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage cmatch_ave_?? ($xlist = $ivlist) $zlist ability_?? skill_?? ability_skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_cmatch_x.ster, replace

global xlist  match_??_ten_occ $xlist_0 ability_??_ten_occ skill_??_ten_occ  ability_skill_mean_ten_occ
global ivlist match_??_ten_occ_iv $ivlist_0 ability_??_ten_occ_iv skill_??_ten_occ_iv ability_skill_mean_ten_occ_iv
xi: ivregress 2sls lwage cmatch_ave_?? match_?? ($xlist = $ivlist) $zlist ability_?? skill_?? ability_skill_mean i.ind_1d i.occ_1d , vce(robust)
estimate save ${result}/iv_ind_cmatch_match_x.ster, replace

/* creating a table */
estimate use ${result}/bench_iv.ster
outreg2 using ${result}/table_${diminitls}_ind_cmatch.tex, bdec(3) tex(fragment) title("Cumulative Match Regression") ctitle("", "Benchmark IV") drop(_I* $zlist) label se replace sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmatch.ster
outreg2 using ${result}/table_${diminitls}_ind_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmatch_match.ster
outreg2 using ${result}/table_${diminitls}_ind_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmatch_means.ster
outreg2 using ${result}/table_${diminitls}_ind_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmatch_match_means.ster
outreg2 using ${result}/table_${diminitls}_ind_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmatch_x.ster
outreg2 using ${result}/table_${diminitls}_ind_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)
estimate use ${result}/iv_ind_cmatch_match_x.ster
outreg2 using ${result}/table_${diminitls}_ind_cmatch.tex, bdec(3) tex(fragment) ctitle("", " ") drop(_I* $zlist) label se sortvar(match_* absmm_* cmatch_* cmm_* $xlist $zlist)

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/


/*------------------------------------------------------------------------------------*/
/*----------------Main mm and match output tables-------------------------------------*/
/*------------------------------------------------------------------------------------*/

if($totphys == 1){
	estimate use ${result}/iv_cmm_mm_means.ster
	outreg2 using ${result}/table_tp_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm* absmm_* cmm_ave cmm_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist) replace
	outreg2 using ${result}/apxtable_tp_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm* absmm_* cmm_ave cmm_ave_* ability* skill* ) drop(_I*) replace

	estimate use ${result}/iv_mm_means.ster
	outreg2 using ${result}/table_tp_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm* absmm_* cmm_ave cmm_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist) 
	outreg2 using ${result}/apxtable_tp_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm* absmm_* cmm_ave cmm_ave_* ability* skill* ) drop(_I*)

	estimate use ${result}/iv_ind_cmm_mm_means.ster
	outreg2 using ${result}/table_tp_mm_comp.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(mm* absmm_* cmm_ave cmm_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist)
	outreg2 using ${result}/apxtable_tp_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm* absmm_* cmm_ave cmm_ave_* ability* skill* ) drop(_I*)

	estimate use ${result}/iv_ind_mm_means.ster
	outreg2 using ${result}/table_tp_mm_comp.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(mm* absmm_* cmm_ave cmm_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist) 
	outreg2 using ${result}/apxtable_tp_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm* absmm_* cmm_ave cmm_ave_* ability* skill* ) drop(_I*)
* match regressions
/*	estimate use ${result}/iv_cmatch_match_means.ster
	outreg2 using ${result}/table_tp_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match* cmatch_ave cmatch_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist) replace

	estimate use ${result}/iv_ind_cmatch_match_means.ster
	outreg2 using ${result}/table_tp_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match* cmatch_ave cmatch_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist)

	estimate use ${result}/iv_ind_match_means.ster
	outreg2 using ${result}/table_tp_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match* cmatch_ave cmatch_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist) 
*/
	estimate use ${result}/iv_cmatch_match_means.ster
	outreg2 using ${result}/table_tp_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist) replace
	outreg2 using ${result}/apxtable_tp_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill*) drop(_I*) replace

	estimate use ${result}/iv_match_means.ster
	outreg2 using ${result}/table_tp_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist) 
	outreg2 using ${result}/apxtable_tp_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill*) drop(_I*)


	estimate use ${result}/iv_ind_cmatch_match_means.ster
	outreg2 using ${result}/table_tp_match_comp.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist)
	outreg2 using ${result}/apxtable_tp_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill*) drop(_I*)

	estimate use ${result}/iv_ind_match_means.ster
	outreg2 using ${result}/table_tp_match_comp.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist) 
	outreg2 using ${result}/apxtable_tp_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill*) drop(_I*)
}

/*------------------------------------------------------------------------------------*/
/*---------------Drill down to math verbal--------------------------------------------*/
/*------------------------------------------------------------------------------------*/
if($verbmath == 1 | $verbmathmech == 1 | $verbmathphys == 1){
	
* mismatch iv	

	estimate use ${result}/iv_mm_means.ster
	outreg2 using ${result}/table_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave  ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_*) drop(_I* oj ten* exp* $zlist) replace
	outreg2 using ${result}/apxtable_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave  ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ten* exp* oj $zlist) drop(_I* ) replace

	estimate use ${result}/iv_cmm_mm_means.ster
	outreg2 using ${result}/table_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_*  cmm_ave cmm_ave_* cmm_*_ave ability* skill* ) drop(_I* oj ten* exp* $zlist) 
	outreg2 using ${result}/apxtable_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_*  cmm_ave cmm_ave_* cmm_*_ave ability* skill* ten* exp* oj $zlist) drop(_I* ) 

	estimate use ${result}/iv_cmm_mm_means_pos_neg.ster
	outreg2 using ${result}/table_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_*  cmm_ave cmm_*_ave cmm_ave_* ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ) drop(_I* oj ten* exp* $zlist)
	outreg2 using ${result}/apxtable_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_*  cmm_ave cmm_*_ave cmm_ave_* ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ten* exp* oj $zlist) drop(_I* )

	estimate use ${result}/iv_ind_mm_means.ster
	outreg2 using ${result}/table_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave cmm_ave_*  ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ) drop(_I* oj ten* exp* $zlist) 
	outreg2 using ${result}/apxtable_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave cmm_ave_*  ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ten* exp* oj $zlist) drop(_I* ) 

	estimate use ${result}/iv_ind_cmm_mm_means.ster
	outreg2 using ${result}/table_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave cmm_ave_* ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ) drop(_I* oj ten* exp* $zlist)
	outreg2 using ${result}/apxtable_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave cmm_ave_* ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ten* exp* oj $zlist) drop(_I* )	

* mismatch ols

	estimate use ${result}/ols_mm_means.ster
	outreg2 using ${result}/table_${diminitls}_mm_ols.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave  ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_*) drop(_I* oj ten* exp* $zlist) replace
	outreg2 using ${result}/apxtable_${diminitls}_mm_ols.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave  ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ten* exp* oj $zlist) drop(_I* ) replace

	estimate use ${result}/ols_cmm_mm_means.ster
	outreg2 using ${result}/table_${diminitls}_mm_ols.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg  absmm_* cmm_ave cmm_ave_* cmm_*_ave ability* skill* ) drop(_I* oj ten* exp* $zlist) 
	outreg2 using ${result}/apxtable_${diminitls}_mm_ols.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg  absmm_* cmm_ave cmm_ave_* cmm_*_ave ability* skill* ten* exp* oj $zlist) drop(_I* ) 

	estimate use ${result}/ols_cmm_mm_means_pos_neg.ster
	outreg2 using ${result}/table_${diminitls}_mm_ols.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave cmm_ave_* ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ) drop(_I* oj ten* exp* $zlist)
	outreg2 using ${result}/apxtable_${diminitls}_mm_ols.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave cmm_ave_* ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ten* exp* oj $zlist) drop(_I* )

	estimate use ${result}/ols_ind_mm_means.ster
	outreg2 using ${result}/table_${diminitls}_mm_ols.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(mmm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave cmm_ave_*  ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ) drop(_I* oj ten* exp* $zlist) 
	outreg2 using ${result}/apxtable_${diminitls}_mm_ols.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave cmm_ave_*  ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ten* exp* oj $zlist) drop(_I* ) 

	estimate use ${result}/ols_ind_cmm_mm_means.ster
	outreg2 using ${result}/table_${diminitls}_mm_ols.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(mm mm_ten* mm_pos mm_neg absmm_* cmm_ave cmm_*_ave cmm_ave_* ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ) drop(_I* oj ten* exp* $zlist)
	outreg2 using ${result}/apxtable_${diminitls}_mm_ols.tex, bdec(3) tex(fragment) label ctitle("", " ") stats(coef) sortvar(mm mm_ten* mm_pos mm_neg  absmm_* cmm_ave cmm_*_ave cmm_ave_* ability_mean* ability_?? ability_??_* skill_mean* skill_?? skill_??_* ten* exp* oj $zlist) drop(_I* )	
	
* match regressions
	estimate use ${result}/iv_cmatch_match_means.ster
	outreg2 using ${result}/table_${diminitls}_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist) replace

	estimate use ${result}/iv_match_means.ster
	outreg2 using ${result}/table_${diminitls}_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist)

	estimate use ${result}/iv_ind_cmatch_match_means.ster
	outreg2 using ${result}/table_${diminitls}_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist)

	estimate use ${result}/iv_ind_match_means.ster
	outreg2 using ${result}/table_${diminitls}_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(match match_ten_occ match_?? match_??_* cmatch_ave cmatch_ave_* ability* skill* ) drop(_I* oj ten* exp* $zlist) 
}

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* ----------- SOME EXPERIMENTAL REGRESSIONS -----------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

global xlist  $xlist_0 mm
global ivlist $ivlist_0 lmm
xi: ivregress 2sls delwage ($xlist = $ivlist) $zlist ability_mean skill_mean i.ind_1d i.occ_1d, vce(robust)
estimate save ${result}/ivdel_mm_means.ster, replace


global xlist  mm_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist mm_ten_occ_iv $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv
xi: ivregress 2sls lwage mm cmm_ave ($xlist = $ivlist) $zlist ability_mean skill_mean loccwage i.ind_1d i.occ_1d, vce(robust) /*  [aw=sweight]*/
estimate save ${result}/iv_cmm_mm_lw_means.ster, replace


global xlist  mm_pos_ten_occ mm_neg_ten_occ $xlist_0 ability_mean_ten_occ skill_mean_ten_occ
global ivlist mm_pos_ten_occ_iv mm_neg_ten_occ_iv $ivlist_0 ability_mean_ten_occ_iv skill_mean_ten_occ_iv
xi: ivregress 2sls lwage mm_neg mm_pos cmm_pos_ave cmm_neg_ave ($xlist = $ivlist) $zlist ability_mean skill_mean loccwage i.ind_1d i.occ_1d, vce(robust) /*  [aw=sweight]*/
estimate save ${result}/iv_cmm_mm_lw_means_pos_neg.ster, replace


/*------------------------------------------------------------------------------------*/
/*------Switching probability regressions---------------------------------------------*/
/*------------------------------------------------------------------------------------*/

* following : https://diffuseprior.wordpress.com/2012/08/15/probit-models-with-endogeneity/ or http://www.cemfi.es/~arellano/binary-endogeneity.pdf

global prswlist $zlist_0 ability_mean skill_mean i.occ_1d  i.ind_1d oj exp exp2 tenure_occ ten_occ2 skill_mean_ten_occ ability_mean_ten_occ 
global prswlist_x tenure_occ ten_occ2 skill_mean_ten_occ ability_mean_ten_occ 
global prswlist_z $zlist_0 ability_mean skill_mean i.occ_1d  i.ind_1d exp exp2 oj
global prswlist_iv lwage_occ_iv lwage_occ2_iv skill_mean_ten_occ_iv ability_mean_ten_occ_iv 

use ${result}/yearly_03_phys_${diminitls}.dta, clear

xtset id year
gen fswitch_occ = f.switch_occ

/* 1st stage for probit regressions */
replace skill_mean_ten_occ_iv = skill_mean*lwage_occ_iv
replace ability_mean_ten_occ_iv = ability_mean*lwage_occ_iv


global prswlist_xhat 
global prswlist_ctrl
qui foreach xvar of varlist $prswlist_x{
	reg `xvar' mm $prswlist_iv $prswlist_z
	predict `xvar'hat
	predict resid`xvar', resid
	global prswlist_xhat $prswlist_xhat `xvar'hat 
	global prswlist_ctrl $prswlist_ctrl resid`xvar'
	
}
*label var exphat "Experience"
*label var exp2hat "Experience$^2$"
* label var ojhat "Old Job"
label var tenure_occhat "Occ Tenure"
label var ten_occ2hat "Occ Tenure$^2$" 
label var ability_mean_ten_occhat "Worker Ability $\times$ Occ Tenure"
label var skill_mean_ten_occhat "Occ Reqs $\times$ Occ Tenure"

/*
stset tenure_occ, failure(fswitch_occ)
/* proportional hazard regressions */
xi: stcox mm cmm_ave $prswlist
margins, dydx(*) post
estimate save ${result}/dur_${diminitls}_mm.ster, replace
xi: stcox mm_neg mm_pos cmm_ave $prswlist
margins, dydx(*) post
estimate save ${result}/dur_${diminitls}_mm_sgn.ster, replace

xi: stcox mm_??_neg mm_??_pos cmm_ave_?? $prswlist 
margins, dydx(*) post
estimate save ${result}/dur_${diminitls}_mm_ind_sgn.ster, replace
xi: stcox absmm_?? cmm_ave_?? $prswlist 
margins, dydx(*) post
estimate save ${result}/dur_${diminitls}_mm_ind.ster, replace
*/

/* probit regressions */
xi: probit fswitch_occ mm $prswlist 
matrix matV_mm = e(V)
margins, dydx(*) post
estimate save ${result}/pr_${diminitls}_mm.ster, replace
xi: probit fswitch_occ mm_neg mm_pos $prswlist 
matrix matV_mm_sgn = e(V)
margins, dydx(*) post
estimate save ${result}/pr_${diminitls}_mm_sgn.ster, replace

xi: probit fswitch_occ mm_??_neg mm_??_pos $prswlist  
matrix matV_mm_ind_sgn = e(V)
margins, dydx(*) post
estimate save ${result}/pr_${diminitls}_mm_ind_sgn.ster, replace
xi: probit fswitch_occ absmm_?? $prswlist 
matrix matV_mm_ind = e(V)
margins, dydx(*) post
estimate save ${result}/pr_${diminitls}_mm_ind.ster, replace


/* ivprobit regressions, wage_iv */

xi: probit fswitch_occ mm $prswlist_x $prswlist_z $prswlist_ctrl
margins, dydx(*) post
estimate save ${result}/ivpr_${diminitls}_mm.ster, replace
qui foreach xvar of varlist $prswlist_x{
	reg `xvar' mm_neg mm_pos $prswlist_iv $prswlist_z
	predict rr, resid
	replace resid`xvar' = rr 
	drop rr
}
xi: probit fswitch_occ mm_neg mm_pos $prswlist_x $prswlist_z $prswlist_ctrl
margins, dydx(*) post
estimate save ${result}/ivpr_${diminitls}_mm_sgn.ster, replace

qui foreach xvar of varlist $prswlist_x{
	reg `xvar' mm_??_neg mm_??_pos $prswlist_iv $prswlist_z
	predict rr, resid
	replace resid`xvar' = rr 
	drop rr
}
xi: probit fswitch_occ mm_??_neg mm_??_pos $prswlist_x $prswlist_z $prswlist_ctrl
margins, dydx(*) post
estimate save ${result}/ivpr_${diminitls}_mm_ind_sgn.ster, replace
qui foreach xvar of varlist $prswlist_x{
	reg `xvar' absmm_?? $prswlist_iv $prswlist_z
	predict rr, resid
	replace resid`xvar' = rr 
	drop rr
}
xi: probit fswitch_occ absmm_?? $prswlist_x $prswlist_z $prswlist_ctrl
margins, dydx(*) post
estimate save ${result}/ivpr_${diminitls}_mm_ind.ster, replace


/* 2 stage for probit regressions using control function approach of Rivers & Vuong (1988)
reg tenure_occ mm ten_occ_iv $prswlist_z
local sighat = e(rss)/(e(N)-1)
qui predict ten_occ_mmctrl, resid
qui replace ten_occ_mmctrl = ten_occ_mmctrl /`sighat'
xi: probit fswitch_occ mm $prswlist_z $prswlist_x ten_occ_mmctrl, ltolerance(1e-3) nrtolerance(1e-3) showtolerance iterate(30)*/
/*
capture program drop TSPROBIT
program define TSPROBIT, eclass
	args mmvar1 mmvar2 coef_scal
	
	qui foreach xvar of varlist $prswlist_x{
		reg `xvar' `mmvar1' `mmvar2' $prswlist_iv $prswlist_z
		predict pred
		replace `xvar'hat = pred
		drop pred
	}
	qui xi: probit fswitch_occ `mmvar1' `mmvar2' $prswlist_z $prswlist_xhat
	matrix ivmatV = e(V)
	local cor_fac = `coef_scal'/ivmatV[colsof(ivmatV),colsof(ivmatV)]
	matrix b = e(b)*`cor_fac'^.5
	matrix V = e(V)*`cor_fac'
	ereturn repost b = b
	ereturn repost V = V
	margins, dydx(*) post
end 

local coef_scal  = matV_mm[colsof(matV_mm),colsof(matV_mm)]
TSPROBIT mm " " `coef_scal'
estimate save ${result}/ivpr_${diminitls}_mm.ster, replace

local coef_scal  = matV_mm_sgn[colsof(matV_mm_sgn),colsof(matV_mm_sgn)]
TSPROBIT mm_neg mm_pos `coef_scal'
estimate save ${result}/ivpr_${diminitls}_mm_sgn.ster, replace

local coef_scal  = matV_mm_ind_sgn[colsof(matV_mm_ind_sgn),colsof(matV_mm_ind_sgn)]
TSPROBIT mm_??_neg mm_??_pos `coef_scal'
estimate save ${result}/ivpr_${diminitls}_mm_ind_sgn.ster, replace

local coef_scal  = matV_mm_ind[colsof(matV_mm_ind),colsof(matV_mm_ind)]
TSPROBIT absmm_?? " " `coef_scal'
estimate save ${result}/ivpr_${diminitls}_mm_ind.ster, replace
*/

/*
qui xi: probit fswitch_occ mm $prswlist_z $prswlist_xhat
matrix ivmatV_mm = e(V)
local cor_fac = (matV_mm[colsof(matV_mm),colsof(matV_mm)]/ivmatV_mm[colsof(ivmatV_mm),colsof(ivmatV_mm)])
ereturn repost b = e(b)*`cor_fac'^.5
ereturn repost V = e(V)*`cor_fac'
margins, dydx(*) post
estimate save ${result}/ivpr_${diminitls}_mm.ster, replace

qui foreach xvar of varlist $prswlist_x{
	reg `xvar' mm_neg mm_pos $prswlist_iv $prswlist_z
	predict pred
	replace `xvar'hat = pred
	drop pred
}
xi: probit fswitch_occ mm_neg mm_pos $prswlist_z $prswlist_xhat
matrix ivmatV_mm_sgn = e(V)
local cor_fac = (matV_mm_sgn[colsof(matV_mm_sgn),colsof(matV_mm_sgn)]/ivmatV_mm_sgn[colsof(ivmatV_mm_sgn),colsof(ivmatV_mm_sgn)])
ereturn post b = e(b)*`cor_fac'^.5
ereturn post V = e(V)*`cor_fac'
margins, dydx(*) post
estimate save ${result}/ivpr_${diminitls}_mm_sgn.ster, replace

qui foreach xvar of varlist $prswlist_x{
	reg `xvar' mm_??_neg mm_??_pos $prswlist_iv $prswlist_z
	predict pred
	replace `xvar'hat = pred
	drop pred
}
xi: probit fswitch_occ mm_??_neg mm_??_pos $prswlist_z $prswlist_xhat 
matrix ivmatV_mm_ind_sgn = e(V)
local cor_fac = (matV_mm_ind_sgn[colsof(matV_mm_ind_sgn),colsof(matV_mm_ind_sgn)]/ivmatV_mm_ind_sgn[colsof(ivmatV_mm_ind_sgn),colsof(ivmatV_mm_ind_sgn)])
ereturn post b = e(b)*`cor_fac'^.5
ereturn post V = e(V)*`cor_fac'
margins, dydx(*) post
estimate save ${result}/ivpr_${diminitls}_mm_ind_sgn.ster, replace

qui foreach xvar of varlist $prswlist_x{
	reg `xvar' absmm_?? $prswlist_iv $prswlist_z
	predict pred
	replace `xvar'hat = pred
	drop pred
}
xi: probit fswitch_occ absmm_?? $prswlist_z $prswlist_xhat
matrix ivmatV_mm_ind = e(V)
local cor_fac = (matV_mm_ind[colsof(matV_mm_ind),colsof(matV_mm_ind)]/ivmatV_mm_ind[colsof(ivmatV_mm_ind),colsof(ivmatV_mm_ind)])
ereturn post b = e(b)*`cor_fac'^.5
ereturn post V = e(V)*`cor_fac'
margins, dydx(*) post
estimate save ${result}/ivpr_${diminitls}_mm_ind.ster, replace
*/

/* random effects probit regressions:
global prswlist $zlist_0 ability_mean skill_mean i.occ_1d  i.ind_1d oj exp exp2 exp3 
xi: xtprobit fswitch_occ mm cmm_ave $prswlist tenure_occ ten_occ2
*margins, dydx(*) post
*estimate save ${result}/pr_${diminitls}_mm.ster, replace
xi: xtprobit fswitch_occ mm_neg mm_pos cmm_ave $prswlist 
*margins, dydx(*) post
*estimate save ${result}/pr_${diminitls}_mm_sgn.ster, replace

xi: xtprobit fswitch_occ mm_??_neg mm_??_pos cmm_ave $prswlist 
*margins, dydx(*) post
*estimate save ${result}/pr_${diminitls}_mm_ind_sgn.ster, replace
xi: probit fswitch_occ absmm_?? cmm_ave $prswlist 
*margins, dydx(*) post
*estimate save ${result}/pr_${diminitls}_mm_ind.ster, replace
*/

estimate use ${result}/pr_${diminitls}_mm.ster
outreg2 using ${result}/tablepr_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ ten_occ2 exp exp2) drop(_I* hispanic black lths univ) replace
estimate use ${result}/pr_${diminitls}_mm_sgn.ster
outreg2 using ${result}/tablepr_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ ten_occ2 exp exp2) drop(_I* hispanic black lths univ) 
estimate use ${result}/pr_${diminitls}_mm_ind.ster
outreg2 using ${result}/tablepr_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ ten_occ2 exp exp2) drop(_I* hispanic black lths univ) 
estimate use ${result}/pr_${diminitls}_mm_ind_sgn.ster
outreg2 using ${result}/tablepr_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ ten_occ2 exp exp2) drop(_I* hispanic black lths univ) 


estimate use ${result}/ivpr_${diminitls}_mm.ster
outreg2 using ${result}/tableivpr_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ ten_occ2 exp exp2) drop(_I* hispanic black lths univ) replace
estimate use ${result}/ivpr_${diminitls}_mm_sgn.ster
outreg2 using ${result}/tableivpr_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ ten_occ2 exp exp2) drop(_I* hispanic black lths univ) 
estimate use ${result}/ivpr_${diminitls}_mm_ind.ster
outreg2 using ${result}/tableivpr_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ ten_occ2 exp exp2) drop(_I* hispanic black lths univ) 
estimate use ${result}/ivpr_${diminitls}_mm_ind_sgn.ster
outreg2 using ${result}/tableivpr_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ ten_occ2 exp exp2) drop(_I* hispanic black lths univ) 


estimate use ${result}/dur_${diminitls}_mm.ster
outreg2 using ${result}/tabledur_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ exp exp2 exp3) drop(_I* hispanic black lths univ) replace
estimate use ${result}/dur_${diminitls}_mm_sgn.ster
outreg2 using ${result}/tabledur_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ exp exp2 exp3) drop(_I* hispanic black lths univ) 
estimate use ${result}/dur_${diminitls}_mm_ind.ster
outreg2 using ${result}/tabledur_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ exp exp2 exp3) drop(_I* hispanic black lths univ) 
estimate use ${result}/dur_${diminitls}_mm_ind_sgn.ster
outreg2 using ${result}/tabledur_${diminitls}_mm_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm absmm_* mm_* cmm_ave cmm_ave_* AFQT_std ability_mean skill_mean tenure_occ exp exp2 exp3) drop(_I* hispanic black lths univ) 

*difference between 90-10:
qui {
stcox mm $prswlist 
margins, dydx(*) post at( (p90) mm)
matrix eat = e(at)
local p90 = eat[1,1]
local bmm90 = _b["mm"]
stcox mm $prswlist 
margins, dydx(*) post at( (p10) mm)
matrix eat = e(at)
local p10 = eat[1,1]
local bmm10 = _b["mm"]
sum fswitch_occ if e(sample)
}

disp (`p90'*`bmm90' - `p10'*`bmm10')
disp (`p90'*`bmm90' - `p10'*`bmm10')/r(mean)


* for match quality
xi: probit fswitch_occ match  $prswlist 
margins, dydx(*) post
estimate save ${result}/pr_match.ster, replace

xi: probit fswitch_occ match_??  $prswlist 
margins, dydx(*) post
estimate save ${result}/pr_match_ind.ster, replace

estimate use ${result}/pr_match.ster
outreg2 using ${result}/tablepr_${diminitls}_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm* absmm_* cmm_ave cmm_ave_* AFQT_std age age2) drop(_I* hispanic black lths univ) replace
estimate use ${result}/pr_match_ind.ster
outreg2 using ${result}/tablepr_${diminitls}_match_comp.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(mm* absmm_* cmm_ave cmm_ave_* AFQT_std age age2) drop(_I* hispanic black lths univ) 


qui foreach mvar of varlist mm mm_neg mm_pos fswitch_occ{
	reg `mvar'  $prswlist /*age age2 $zlist_0 i.occ_1d  i.ind_1d*/
	predict `mvar'_resid, residuals
	sum `mvar', meanonly
	replace `mvar'_resid = `mvar'_resid +r(mean)
}

save ${result}/yearly_03_sw_${diminitls}.dta, replace


/*
xtprobit switch_occ mm_neg mm_pos ability_mean age age2 $zlist i.occ_1d  i.ind_1d
xtprobit switch_occ mm ability_mean age age2 $zlist i.occ_1d  i.ind_1d

probit switch_occ match ability_mean skill_mean age age2 $zlist i.occ_1d  i.ind_1d
xtprobit switch_occ match ability_mean skill_mean age age2 $zlist i.occ_1d  i.ind_1d
*/


/*------------------------------------------*/
/*------------------------------------------*/
/* switch rate by negative/positive mismatch */
twoway (lpoly fswitch_occ lmm_neg, lwidth(thick)) (lpoly fswitch_occ lmm_pos, lwidth(thick)), ///
ytitle("Probability of Occupational Switch") ///
xtitle("Mismatch") ///
title("Probability of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pswitch_mm, replace)
graph export ${result}/pswitch_${diminitls}_mm_sgn.eps, replace


/* -----------------------------------------------------------------------------------*/
/* Where do they go?  */
/*------------------------------------------------------------------------------------*/

use ${result}/yearly_03_phys_${diminitls}.dta, clear


xtset id year

gen lexp = l.exp
label var lexp "Experience"
gen lexp2 = l.exp2
label var lexp2 "Experience$^2$"
gen ltenure_occ = l.tenure_occ
label var ltenure_occ "Occupational Tenure"
gen lten_occ2 = l.ten_occ2 
label var lten_occ2 "Occupational Tenure$^2$"
gen loj = l.oj 

if($twodim == 1){
	local nlist "aa bb"
}
if($threedim == 1){
	local nlist "aa bb cc" 
}
if($totphys == 1){
	local llist "Cog Phys"
}
if($verbmath == 1){
	local llist "Verbal Math"
}
if($verbmathphys == 1){
	local llist "Verbal Math Phys"
}


local ll =1
foreach i of local nlist {

	local l: word `ll' of `llist'
	*THIS is qualitatively similar gen chng_skill_`i' = (skill_`i' - l.skill_`i')/l.skill_`i'
	gen chng_skill_`i' = (skill_`i' - l.skill_`i')
	label var chng_skill_`i' "Skill Change `l'"
	gen lmm_`i'_sgn = l.mm_`i'
	label var lmm_`i'_sgn "Last Mismatch `l'"
	gen ability_`i'_lten = ability_`i'*l.tenure_occ
	label var ability_`i'_lten "Ability Ability $\times$  Tenure"
	
	global swlist ability_`i' ability_`i'_lten $zlist lexp lexp2 ltenure_occ lten_occ2 loj i.occ_1d i.ind_1d
	
	
	xi: reg chng_skill_`i' lmm_`i'_sgn $swlist if switch_occ==1
	estimate save ${result}/mm_${diminitls}_where_`i'.ster, replace
	
	xi: reg chng_skill_`i' lmm_`i'_neg lmm_`i'_pos $swlist if switch_occ==1 & lmm_neg <. & lmm_pos<.
	estimate save ${result}/mm_${diminitls}_where_`i'_pos_neg.ster, replace
	
	
	reg lmm_`i'_sgn $swlist if switch_occ==1
	predict lmm_`i'_resid if switch_occ==1, resid
	label var lmm_`i'_resid "Last Mismatch `l' (resid)"
	
	reg lmm_`i'_pos $swlist if switch_occ==1
	predict lmm_`i'_pos_resid if switch_occ==1, resid
	label var lmm_`i'_pos_resid "Last Mismatch `l' (resid)"
	reg lmm_`i'_neg $swlist if switch_occ==1
	predict lmm_`i'_neg_resid if switch_occ==1, resid
	label var lmm_`i'_neg_resid "Last Mismatch `l' (resid)"
	
	
	reg chng_skill_`i' $swlist if switch_occ==1
	predict chng_skill_`i'_resid if switch_occ==1, resid
	label var chng_skill_`i'_resid "Skill Change `l' (resid)"
	local ll =`ll'+1
	
}
gen ability_mean_lten = ability_mean*ltenure_occ

global swlist ability_mean ability_mean_lten $zlist lexp lexp2 ltenure_occ lten_occ2 loj i.occ_1d i.ind_1d

egen chng_skill = rowmean(chng_skill_??)
*gen lmm_sgn = l.mm_sgn if switch_occ == 1
label var lmm_sgn "Last mismatch"
label var chng_skill "Change in Skill"
xi: reg chng_skill lmm_sgn $swlist if switch_occ==1
estimate save ${result}/mm_${diminitls}_where.ster, replace
*gen lmm_neg = l.mm_neg if switch_occ == 1
*gen lmm_pos = l.mm_pos if switch_occ == 1
xi: reg chng_skill lmm_neg lmm_pos $swlist if switch_occ==1
estimate save ${result}/mm_${diminitls}_where_pos_neg.ster, replace


reg chng_skill $swlist if switch_occ==1
predict chng_skill_resid if switch_occ==1 , resid
label var chng_skill_resid "Average change in skills (resid)"
reg lmm_sgn $swlist if switch_occ==1
predict lmm_sgn_resid if switch_occ==1 , resid
label var lmm_sgn_resid "Last mismatch (resid)"

reg lmm_neg $swlist if switch_occ==1
predict lmm_neg_resid if switch_occ==1 , resid
label var lmm_neg_resid "Last mismatch (resid)"
reg lmm_pos $swlist if switch_occ==1
predict lmm_pos_resid if switch_occ==1 , resid
label var lmm_pos_resid "Last mismatch (resid)"


estimate use ${result}/mm_${diminitls}_where.ster
outreg2 using ${result}/table_${diminitls}_mm_where.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(lmm_sgn lmm_*_sgn ability* age* $zlist) drop(_I* chng*) replace
foreach i of local nlist{
	estimate use ${result}/mm_${diminitls}_where_`i'.ster
	outreg2 using ${result}/table_${diminitls}_mm_where.tex, bdec(3) tex(fragment) ctitle("", " ") sortvar(lmm_sgn lmm_*_sgn ability* age* $zlist) drop(_I* chng*) label stats(coef)
}

estimate use ${result}/mm_${diminitls}_where_pos_neg.ster
outreg2 using ${result}/table_${diminitls}_mm_where_pos_neg.tex, bdec(3) tex(fragment) ctitle("", " ") label stats(coef) sortvar(lmm_pos lmm_neg lmm_*_pos lmm_*_neg ability* age* $zlist) drop(_I* chng*) replace
foreach i of local nlist{
	estimate use ${result}/mm_${diminitls}_where_`i'_pos_neg.ster
	local b_pos = _b["lmm_`i'_pos"]
	local b_neg = _b["lmm_`i'_neg"]
	outreg2 using ${result}/table_${diminitls}_mm_where_pos_neg.tex, bdec(3) tex(fragment) ctitle("", " ") sortvar(lmm_pos lmm_neg lmm_*_pos lmm_*_neg ability* age* $zlist) drop(_I* chng*) label stats(coef)
	* average effect at median mismatch
	qui _pctile lmm_`i'_neg if lmm_`i'_neg<0, p(50)
	local neg_med = r(r1)
	qui _pctile lmm_`i'_pos if lmm_`i'_pos>0, p(50)
	local pos_med = r(r1)
	disp `b_pos'*`pos_med'
	disp `b_neg'*`neg_med'
}


save ${result}/yearly_03_where_${diminitls}.dta, replace


/*------------------------------------------*/
/* Plot the change in skills*/
/*------------------------------------------*/
twoway (lpoly chng_skill lmm_sgn if switch_occ==1, lwidth(thick)), ///
ytitle("Average change in skill") ///
xtitle("Signed mismatch") ///
title("Direction of occupational mobility") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/where_mm, replace)
graph export ${result}/where_${diminitls}_mm.eps, replace

lpoly chng_skill lmm_sgn if switch_occ==1, ci ms(p) ///
ytitle("Average change in skill") ///
xtitle("Signed mismatch") ///
title("Direction of occupational mobility") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/where_mm, replace)
graph export ${result}/where_${diminitls}_mm_scat.eps, replace

twoway (lpoly chng_skill_resid lmm_sgn_resid, lwidth(thick))  if switch_occ==1, ///
ytitle("Average change in skill (resid)") ///
xtitle("Signed mismatch (resid)") ///
title("Direction of occupational mobility") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/where_mmresid, replace)
graph export ${result}/where_${diminitls}_mmresid.eps, replace

lpoly chng_skill_resid lmm_sgn_resid if switch_occ==1, ci ms(p) ///
ytitle("Average change in skill") ///
xtitle("Signed mismatch") ///
title("Direction of occupational mobility") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/where_mm, replace)
graph export ${result}/where_${diminitls}_mmresid_scat.eps, replace


twoway (lpoly chng_skill lmm_neg if lmm_neg<0, lwidth(thick)) (lpoly chng_skill lmm_pos if lmm_pos>0, lwidth(thick)) if switch_occ==1, ///
ytitle("Average change in skill") ///
xtitle("Last Mismatch") ///
title("Direction of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/where_${diminitls}_mm_neg_pos, replace)
graph export ${result}/where_${diminitls}_mm_neg_pos.eps, replace

twoway (lpoly chng_skill lmm_neg if lmm_neg<0, lwidth(thick)) (lpoly chng_skill lmm_pos if lmm_pos>0, lwidth(thick)) ///
(scatter chng_skill lmm_neg if lmm_neg<0, msymbol(p)) (scatter chng_skill lmm_pos if lmm_pos>0, msymbol(p))  if switch_occ==1, ///
ytitle("Average change in skill") ///
xtitle("Last Mismatch") ///
title("Direction of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/where_${diminitls}_mm_neg_pos, replace)
graph export ${result}/where_${diminitls}_mm_scat_neg_pos.eps, replace


twoway (lpoly chng_skill_resid lmm_neg_resid, lwidth(thick)) (lpoly chng_skill_resid lmm_pos_resid, lwidth(thick))  if switch_occ==1, ///
ytitle("Average change in skill (residuals)") ///
xtitle("Last Mismatch (residuals)") ///
title("Direction of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/where_${diminitls}_mmresid_neg_pos, replace)
graph export ${result}/where_${diminitls}_mmresid_neg_pos.eps, replace

if($twodim == 1){
	local nlist "aa bb"
}
if($threedim == 1){
	local nlist "aa bb cc" 
}
if($totphys == 1){
	local llist "Cog Phys"
}
if($verbmath == 1){
	local llist "Verbal Math"
}
if($verbmathphys == 1){
	local llist "Verbal Math Phys"
}

local ll =1
foreach i of local nlist {

	local l: word `ll' of `llist'
	
	twoway (lpoly chng_skill_`i' lmm_`i'_sgn, lwidth(thick)) if switch_occ==1, ///
	ytitle("Change in skill, `l'") ///
	xtitle("Signed mismatch, `l'") ///
	title("Direction of occupational mobility, `l'") ///
	legend(off) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/where_mm_`i', replace)
	graph export ${result}/where_${diminitls}_mm_`i'.eps, replace

	lpoly chng_skill_`i' lmm_`i'_sgn if switch_occ==1, ci ms(p) ///
	ytitle("Change in skill, `l'") ///
	xtitle("Signed mismatch, `l'") ///
	title("Direction of occupational mobility, `l'") ///
	legend(off) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/where_mm_`i', replace)
	graph export ${result}/where_${diminitls}_mm_`i'_scat.eps, replace

	twoway (lpoly chng_skill_`i'_resid lmm_`i'_resid, lwidth(thick)) if switch_occ==1, ///
	ytitle("Change in skill, `l' (resid)") ///
	xtitle("Signed mismatch, `l' (resid)") ///
	title("Direction of occupational mobility, `l'") ///
	legend(off) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/where_mmresid_`i', replace)
	graph export ${result}/where_${diminitls}_mmresid_`i'.eps, replace

	lpoly chng_skill_`i'_resid lmm_`i'_resid if switch_occ==1, ci ms(p) ///
	ytitle("Change in skill, `l'") ///
	xtitle("Signed mismatch, `l'") ///
	title("Direction of occupational mobility, `l'") ///
	legend(off) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/where_mm_`i', replace)
	graph export ${result}/where_${diminitls}_mmresid_scat_`i'.eps, replace

	twoway (lpoly chng_skill_`i' lmm_`i'_neg  if lmm_`i'_neg<0, lwidth(thick)) (lpoly chng_skill_`i' lmm_`i'_pos if lmm_`i'_pos>0, lwidth(thick))  if switch_occ==1, ///
	ytitle("Change in skill, `l'") ///
	xtitle("Last Mismatch") ///
	title("Direction of Occupational Switch, `l'") ///
	legend(off) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/where_${diminitls}_mm_neg_pos, replace)
	graph export ${result}/where_${diminitls}_mm_neg_pos_`i'.eps, replace

	twoway (lpoly chng_skill_`i' lmm_`i'_neg  if lmm_`i'_neg<0, lwidth(thick)) (lpoly chng_skill_`i' lmm_`i'_pos if lmm_`i'_pos>0, lwidth(thick)) ///
	(scatter chng_skill_`i' lmm_`i'_neg  if lmm_`i'_neg<0, msymbol(p)) (scatter chng_skill_`i' lmm_`i'_pos if lmm_`i'_pos>0, msymbol(p))  if switch_occ==1, ///
	ytitle("Change in skill, `l'") ///
	xtitle("Last Mismatch") ///
	title("Direction of Occupational Switch, `l'") ///
	legend(off) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/where_${diminitls}_mm_neg_pos, replace)
	graph export ${result}/where_${diminitls}_mm_scat_neg_pos_`i'.eps, replace


	twoway (lpoly chng_skill_`i'_resid lmm_neg_resid, lwidth(thick)) (lpoly chng_skill_`i'_resid lmm_pos_resid, lwidth(thick))  if switch_occ==1, ///
	ytitle("Change in skill, `l' (residuals)") ///
	xtitle("Last Mismatch (residuals)") ///
	title("Direction of Occupational Switch, `l'") ///
	legend(off) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/where_${diminitls}_mmresid_neg_pos, replace)
	graph export ${result}/where_${diminitls}_mmresid_neg_pos_`i'.eps, replace


	local ll = `ll'+1
}




/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* Predicted profiles*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* ------------------------------------------ */
/* ---------Current with tenure------- */
/* ------------------------------------------ */

use ${result}/yearly_03_phys_${diminitls}.dta, clear
_pctile match, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)
clear

estimate use ${result}/iv_match_means.ster
matrix eb=e(b)
matrix pred_match = J(11,4,0.0)
forvalues tt=0/10{
	matrix pred_match[`tt'+1,1] = `tt'
	matrix pred_match[`tt'+1,2] = _b["match"]*`p90'+_b["match_ten_occ"]*`p90'*`tt' 
	matrix pred_match[`tt'+1,3] = _b["match"]*`p50'+_b["match_ten_occ"]*`p50'*`tt' 
	matrix pred_match[`tt'+1,4] = _b["match"]*`p10'+_b["match_ten_occ"]*`p10'*`tt' 
}
svmat pred_match
twoway line pred_match2-pred_match4 pred_match1, ///
ytitle("Predicted Return")  lwidth(thick thick thick) ///
xtitle("Occupational Tenure") ///
title("Predicted Profile by Match Quality") ///
lpattern("_" "_" "_") lcolor("red" "black" "blue") ///
legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") c(1)) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pred_effect_${diminitls}_match_means, replace)
graph export ${result}/pred_effect_${diminitls}_match_means.eps, replace
drop pred_match*

use ${result}/yearly_03_phys_${diminitls}.dta, clear
_pctile mm, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)
clear

estimate use ${result}/iv_cmm_mm_means.ster
matrix eb=e(b)
matrix pred_match = J(11,4,0.0)
forvalues tt=0/10{
	matrix pred_match[`tt'+1,1] = `tt'
	matrix pred_match[`tt'+1,2] = _b["mm"]*`p90'+_b["mm_ten_occ"]*`p90'*`tt' 
	matrix pred_match[`tt'+1,3] = _b["mm"]*`p50'+_b["mm_ten_occ"]*`p50'*`tt' 
	matrix pred_match[`tt'+1,4] = _b["mm"]*`p10'+_b["mm_ten_occ"]*`p10'*`tt' 	

}
svmat pred_match
twoway line pred_match2-pred_match4 pred_match1, ///
ytitle("Predicted Return")  lwidth(thick thick thick) ///
xtitle("Occupational Tenure") ///
title("Predicted Profile by Mismatch") ///
lpattern("_" "_" "_") lcolor("red" "black" "blue") ///
legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") c(1)) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pred_effect_${diminitls}_mm_means, replace)
graph export ${result}/pred_effect_${diminitls}_mm_means.eps, replace
drop pred_match*

graph combine ${result}/pred_effect_${diminitls}_match_means.gph ${result}/pred_effect_${diminitls}_mm_means.gph, ycommon
graph export ${result}/pred_effect_${diminitls}_match_mm_means.eps, replace

/* ------------------------------------------ */
/* ---------Cumulative with experience------- */
/* ------------------------------------------ */

use ${result}/yearly_03_phys_${diminitls}.dta, clear
_pctile cmatch_ave, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)

_pctile match, p(10 30 50 70 90)
local p90_m = r(r5)
local p70_m = r(r4)
local p50_m = r(r3)
local p30_m = r(r2)
local p10_m = r(r1)
clear

estimate use ${result}/iv_cmatch_match_means.ster
matrix eb=e(b)
matrix pred_match = J(11,4,0.0)
forvalues tt=0/10{
	matrix pred_match[`tt'+1,1] = `tt'
	matrix pred_match[`tt'+1,2] = _b["cmatch_ave"]*`p90'/*+_b["match"]*`p90_m'+_b["match_ten_occ"]*`p90_m'*`tt' */
	matrix pred_match[`tt'+1,3] = _b["cmatch_ave"]*`p50'/*+_b["match"]*`p50_m'+_b["match_ten_occ"]*`p50_m'*`tt' */
	matrix pred_match[`tt'+1,4] = _b["cmatch_ave"]*`p10'/*+_b["match"]*`p10_m'+_b["match_ten_occ"]*`p10_m'*`tt' */
}
svmat pred_match
twoway line pred_match2-pred_match4 pred_match1, ///
ytitle("Predicted Return")  lwidth(thick thick thick) ///
xtitle("Labor Market Experience") ///
title("Predicted Profile by Cumulative Match Quality") ///
lpattern("l" "l" "l") lcolor("red" "black" "blue") ///
legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") c(1)) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pred_effect_${diminitls}_cmatch_means, replace)
graph export ${result}/pred_effect_${diminitls}_cmatch_means.eps, replace
drop pred_match*


use ${result}/yearly_03_phys_${diminitls}.dta, clear
_pctile cmm_ave, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)

_pctile mm, p(10 30 50 70 90)
local p90_m = r(r5)
local p70_m = r(r4)
local p50_m = r(r3)
local p30_m = r(r2)
local p10_m = r(r1)
clear

estimate use ${result}/iv_cmm_mm_means.ster
matrix eb=e(b)
matrix pred_match = J(11,4,0.0)
forvalues tt=0/10{
	matrix pred_match[`tt'+1,1] = `tt'
	matrix pred_match[`tt'+1,2] = _b["cmm_ave"]*`p90'/*+_b["mm"]*`p90_m'+_b["mm_ten_occ"]*`p90_m'*`tt' */
	matrix pred_match[`tt'+1,3] = _b["cmm_ave"]*`p50'/*+_b["mm"]*`p50_m'+_b["mm_ten_occ"]*`p50_m'*`tt' */
	matrix pred_match[`tt'+1,4] = _b["cmm_ave"]*`p10'/*+_b["mm"]*`p10_m'+_b["mm_ten_occ"]*`p10_m'*`tt' */	

}
svmat pred_match
twoway line pred_match2-pred_match4 pred_match1, ///
ytitle("Predicted Return")  lwidth(thick thick thick) ///
xtitle("Labor Market Experience") ///
title("Predicted Profile by Cumulative Mismatch") ///
lpattern("l" "l" "l") lcolor("red" "black" "blue") ///
legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") c(1) ///
ring(0) pos(6) ) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pred_effect_${diminitls}_cmm_means, replace)
graph export ${result}/pred_effect_${diminitls}_cmm_means.eps, replace
drop pred_match*

graph combine ${result}/pred_effect_${diminitls}_cmatch_means.gph ${result}/pred_effect_${diminitls}_cmm_means.gph, ycommon
graph export ${result}/pred_effect_${diminitls}_cmatch_cmm_means.eps, replace

* current and cumulative together
use ${result}/yearly_03_phys_${diminitls}.dta, clear
_pctile cmm_ave, p(10 30 50 70 90)
local p90 = r(r5)
local p70 = r(r4)
local p50 = r(r3)
local p30 = r(r2)
local p10 = r(r1)

_pctile mm, p(10 30 50 70 90)
local p90_m = r(r5)
local p70_m = r(r4)
local p50_m = r(r3)
local p30_m = r(r2)
local p10_m = r(r1)
clear

estimate use ${result}/iv_cmm_mm_means.ster
matrix eb=e(b)
matrix pred_match = J(11,7,0.0)
forvalues tt=0/10{
	matrix pred_match[`tt'+1,1] = `tt'
	matrix pred_match[`tt'+1,2] = _b["cmm_ave"]*`p90'
	matrix pred_match[`tt'+1,3] = _b["cmm_ave"]*`p50'
	matrix pred_match[`tt'+1,4] = _b["cmm_ave"]*`p10'
	matrix pred_match[`tt'+1,5] = _b["mm"]*`p90_m'+_b["mm_ten_occ"]*`p90_m'*`tt'
	matrix pred_match[`tt'+1,6] = _b["mm"]*`p50_m'+_b["mm_ten_occ"]*`p50_m'*`tt'
	matrix pred_match[`tt'+1,7] = _b["mm"]*`p10_m'+_b["mm_ten_occ"]*`p10_m'*`tt'

}
svmat pred_match
twoway line pred_match2-pred_match7 pred_match1, ///
ytitle("Predicted Return")  lwidth(thick thick thick thick thick thick) ///
xtitle("Occupational Tenure") ///
title("Predicted Profile by Mismatch & Cumulative Mismatch") ///
lpattern("l" "l" "l" "_" "_" "_") lcolor("red" "black" "blue" "red" "black" "blue") ///
legend(label(1 "90 pctl cumul") label(2 "50 pctl cumul") label(3 "10 pctl cumul") ///
label(4 "90 pctl cur") label(5 "50 pctl cur") label(6 "10 pctl cur") c(2) order(1 4 2 5 3 6) ///
ring(0) pos(8) ) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pred_effect_${diminitls}_cmm__mm_means, replace)
graph export ${result}/pred_effect_${diminitls}_cmm_mm_means.eps, replace
drop pred_match*


/* ------------------------------------------ */
/* ---------Current with tenure (component-wise)------- */
/* ------------------------------------------ */



if($twodim == 1){
	local nlist "aa bb"
	local ndim = 2
}
if($threedim == 1){
	local nlist "aa bb cc"
	local ndim = 3
}
if($sevendim == 1){
	local nlist "aa bb cc dd ee ff gg"
	local ndim = 7
}

use ${result}/yearly_03_phys_${diminitls}.dta, clear
foreach i of local nlist{
	_pctile match_`i', p(10 30 50 70 90)
	local p90_`i' = r(r5)
	local p70_`i' = r(r4)
	local p50_`i' = r(r3)
	local p30_`i' = r(r2)
	local p10_`i' = r(r1)
}
clear
	
estimate use ${result}/iv_ind_match_means.ster
matrix eb=e(b)
matrix pred_match = J(11,1+3*`ndim',0.0)
local j = 0
foreach ii of local nlist {
	forvalues tt=0/10{
		matrix pred_match[`tt'+1,1] = `tt'
		matrix pred_match[`tt'+1,2+`j'*3] = _b["match_`ii'"]*`p90_`ii''+_b["match_`ii'_ten_occ"]*`p90_`ii''*`tt'
		matrix pred_match[`tt'+1,3+`j'*3] = _b["match_`ii'"]*`p50_`ii''+_b["match_`ii'_ten_occ"]*`p50_`ii''*`tt' 
		matrix pred_match[`tt'+1,4+`j'*3] = _b["match_`ii'"]*`p10_`ii''+_b["match_`ii'_ten_occ"]*`p10_`ii''*`tt' 
	}
	local j = `j'+1
}
svmat pred_match

if($twodim == 1){
	twoway line pred_match2-pred_match7 pred_match1, ///
	ytitle("Predicted Return")  lwidth(thick thick thick thick thick thick) ///
	xtitle("Occupational Tenure") ///
	title("Predicted Profile by Match Quality") ///
	lpattern( "_" "_" "_" "_" "_" "_") lcolor("red" "red" "red" "blue" "blue" "blue") ///
	legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") label(4 "90 pctl") label(5 "50 pctl") label(6 "10 pctl") c(2) order(1 4 2 5 3 6) ) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pred_effect_${diminitls}_ind_match_means, replace)
}

if($threedim == 1){
	twoway line pred_match2-pred_match10 pred_match1, ///
	ytitle("Predicted Return")  lwidth(thick thick thick thick thick thick thick thick thick) ///
	xtitle("Occupational Tenure") ///
	title("Predicted Profile by Match Quality") ///
	lpattern("l" "l" "l" "l" "l" "l" "l" "l" "l") lcolor("red" "red" "red" "blue" "blue" "blue" "green" "green" "green") ///
	legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") label(4 "90 pctl") label(5 "50 pctl") label(6 "10 pctl") label(7 "90 pctl") label(8 "50 pctl") label(9 "10 pctl") c(3) order(1 4 7 2 5 8 3 6 9) ) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pred_effect_${diminitls}_ind_match_means, replace)
}
graph export ${result}/pred_effect_${diminitls}_ind_match_means.eps, replace
drop pred_match*


if($twodim == 1){
	local nlist "aa bb"
	local ndim = 2
}
if($threedim == 1){
	local nlist "aa bb cc"
	local ndim = 3
}
if($sevendim == 1){
	local nlist "aa bb cc dd ee ff gg"
	local ndim = 7
}

use ${result}/yearly_03_phys_${diminitls}.dta, clear
foreach i of local nlist{
	_pctile absmm_`i', p(10 30 50 70 90)
	local p90_`i' = r(r5)
	local p70_`i' = r(r4)
	local p50_`i' = r(r3)
	local p30_`i' = r(r2)
	local p10_`i' = r(r1)
}
clear

estimate use ${result}/iv_ind_cmm_mm_means.ster
matrix eb=e(b)
matrix pred_match = J(11,1+3*`ndim',0.0)
local j = 0
foreach ii of local nlist {
	forvalues tt=0/10{
		matrix pred_match[`tt'+1,1] = `tt'
		matrix pred_match[`tt'+1,2+`j'*3] = _b["absmm_`ii'"]*`p90_`ii''+_b["absmm_`ii'_ten_occ"]*`p90_`ii''*`tt'
		matrix pred_match[`tt'+1,3+`j'*3] = _b["absmm_`ii'"]*`p50_`ii''+_b["absmm_`ii'_ten_occ"]*`p50_`ii''*`tt' 
		matrix pred_match[`tt'+1,4+`j'*3] = _b["absmm_`ii'"]*`p10_`ii''+_b["absmm_`ii'_ten_occ"]*`p10_`ii''*`tt' 
	}
	local j = `j'+1
}
svmat pred_match

if($twodim == 1 || $diminitls == "vmp"){
	twoway ////
	(line pred_match2-pred_match7 pred_match1, ///
	ytitle("Predicted Return")  lwidth(thick thick thick thick thick thick) ///
	xtitle("Occupational Tenure") ///
	lpattern("-" "-" "-" "-" "-" "-") lcolor( "emerald" "emerald" "emerald" "magenta" "magenta" "magenta") ) ///
	(scatter pred_match2-pred_match7 pred_match1, msymbol(0 0 0 S S S) ///
	mcolor("red" "black" "blue" "red" "black" "blue") lwidth(thick thick thick thick thick thick) ) ///
	, title("Predicted Profile by Mismatch") ///
	legend(label(1 "90 pctl verb") label(2 "50 pctl verb") label(3 "10 pctl verb") label(4 "90 pctl math") label(5 "50 pctl math") label(6 "10 pctl math") c(2) order(1 4 2 5 3 6) ///
	ring(0) pos(8) ) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pred_effect_${diminitls}_ind_mm_means, replace)
}

if($threedim == 1){
	twoway line pred_match2-pred_match10 pred_match1, ///
	ytitle("Predicted Return")  lwidth(thick thick thick thick thick thick thick thick thick) ///
	xtitle("Occupational Tenure") ///
	title("Predicted Profile by Mismatch") ///
	lpattern("l" "l" "l" "l" "l" "l" "l" "l" "l") lcolor("red" "red" "red" "blue" "blue" "blue" "green" "green" "green") ///
	legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") label(4 "90 pctl") label(5 "50 pctl") label(6 "10 pctl") label(7 "90 pctl") label(8 "50 pctl") label(9 "10 pctl") c(3) order(1 4 7 2 5 8 3 6 9) ) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pred_effect_${diminitls}_ind_mm_means, replace)
}
graph export ${result}/pred_effect_${diminitls}_ind_mm_means.eps, replace
drop pred_match*


graph combine ${result}/pred_effect_${diminitls}_ind_match_means.gph ${result}/pred_effect_${diminitls}_ind_mm_means.gph, ycommon
graph export ${result}/pred_effect_${diminitls}_ind_match_mm_means.eps, replace

/* ------------------------------------------ */
/* ---------Cumulative with experience------- */
/* ------------------------------------------ */


if($twodim == 1){
	local nlist "aa bb"
	local ndim = 2
}
if($threedim == 1){
	local nlist "aa bb cc"
	local ndim = 3
}
if($sevendim == 1){
	local nlist "aa bb cc dd ee ff gg"
	local ndim = 7
}

use ${result}/yearly_03_phys_${diminitls}.dta, clear
foreach i of local nlist{
	_pctile cmatch_ave_`i', p(10 30 50 70 90)
	local p90_`i' = r(r5)
	local p70_`i' = r(r4)
	local p50_`i' = r(r3)
	local p30_`i' = r(r2)
	local p10_`i' = r(r1)
}
clear
	
estimate use ${result}/iv_ind_cmatch_means.ster
matrix eb=e(b)
matrix pred_match = J(11,1+3*`ndim',0.0)
local j = 0
foreach ii of local nlist {
	forvalues tt=0/10{
		matrix pred_match[`tt'+1,1] = `tt'
		matrix pred_match[`tt'+1,2+`j'*3] = _b["cmatch_ave_`ii'"]*`p90_`ii''
		matrix pred_match[`tt'+1,3+`j'*3] = _b["cmatch_ave_`ii'"]*`p50_`ii''
		matrix pred_match[`tt'+1,4+`j'*3] = _b["cmatch_ave_`ii'"]*`p10_`ii''
	}
	local j = `j'+1
}
svmat pred_match

if($twodim == 1){
	twoway line pred_match2-pred_match7 pred_match1, ///
	ytitle("Predicted Return")  lwidth(thick thick thick thick thick thick) ///
	xtitle("Labor Market Experience") ///
	title("Predicted Profile by Predicted Cumulative Match Quality") ///
	lpattern("l" "l" "l" "l" "l" "l") lcolor("red" "red" "red" "blue" "blue" "blue") ///
	legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") label(4 "90 pctl") label(5 "50 pctl") label(6 "10 pctl") c(2) order(1 4 2 5 3 6) ) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pred_effect_${diminitls}_ind_cmatch_means, replace)
}

if($threedim == 1){
	twoway line pred_match2-pred_match10 pred_match1, ///
	ytitle("Predicted Return")  lwidth(thick thick thick thick thick thick thick thick thick) ///
	xtitle("Labor Market Experience") ///
	title("Predicted Profile by Predicted Cumulative Match Quality") ///
	lpattern("l" "l" "l" "l" "l" "l" "l" "l" "l") lcolor("red" "red" "red" "blue" "blue" "blue" "green" "green" "green") ///
	legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") label(4 "90 pctl") label(5 "50 pctl") label(6 "10 pctl") label(7 "90 pctl") label(8 "50 pctl") label(9 "10 pctl") c(3) order(1 4 7 2 5 8 3 6 9) ) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pred_effect_${diminitls}_ind_cmatch_means, replace)
}
graph export ${result}/pred_effect_${diminitls}_ind_cmatch_means.eps, replace
drop pred_match*


if($twodim == 1){
	local nlist "aa bb"
	local ndim = 2
}
if($threedim == 1){
	local nlist "aa bb cc"
	local ndim = 3
}
if($sevendim == 1){
	local nlist "aa bb cc dd ee ff gg"
	local ndim = 7
}

use ${result}/yearly_03_phys_${diminitls}.dta, clear
foreach i of local nlist{
	_pctile cmm_ave_`i', p(10 30 50 70 90)
	local p90_`i' = r(r5)
	local p70_`i' = r(r4)
	local p50_`i' = r(r3)
	local p30_`i' = r(r2)
	local p10_`i' = r(r1)
}
clear
	
estimate use ${result}/iv_ind_cmm_means.ster
matrix eb=e(b)
matrix pred_match = J(11,1+3*`ndim',0.0)
local j = 0
foreach ii of local nlist {
	forvalues tt=0/10{
		matrix pred_match[`tt'+1,1] = `tt'
		matrix pred_match[`tt'+1,2+`j'*3] = _b["cmm_ave_`ii'"]*`p90_`ii''
		matrix pred_match[`tt'+1,3+`j'*3] = _b["cmm_ave_`ii'"]*`p50_`ii''
		matrix pred_match[`tt'+1,4+`j'*3] = _b["cmm_ave_`ii'"]*`p10_`ii''
	}
	local j = `j'+1
}
svmat pred_match

if($twodim == 1){
	twoway line pred_match2-pred_match7 pred_match1, ///
	ytitle("Predicted Return")  lwidth(thick thick thick thick thick thick) ///
	xtitle("Labor Market Experience") ///
	title("Predicted Profile by Predicted Cumulative Mismatch") ///
	lpattern("l" "l" "l" "l" "l" "l") lcolor("red" "red" "red" "blue" "blue" "blue") ///
	legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") label(4 "90 pctl") label(5 "50 pctl") label(6 "10 pctl") c(2) order(1 4 2 5 3 6) ) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pred_effect_${diminitls}_ind_cmm_means, replace)
}

if($threedim == 1){
	twoway line pred_match2-pred_match10 pred_match1, ///
	ytitle("Predicted Return")  lwidth(thick thick thick thick thick thick thick thick thick) ///
	xtitle("Labor Market Experience") ///
	title("Predicted Profile by Predicted Cumulative Mismatch") ///
	lpattern("l" "l" "l" "l" "l" "l" "l" "l" "l") lcolor("red" "red" "red" "blue" "blue" "blue" "green" "green" "green") ///
	legend(label(1 "90 pctl") label(2 "50 pctl") label(3 "10 pctl") label(4 "90 pctl") label(5 "50 pctl") label(6 "10 pctl") label(7 "90 pctl") label(8 "50 pctl") label(9 "10 pctl") c(3) order(1 4 7 2 5 8 3 6 9) ) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pred_effect_${diminitls}_ind_cmm_means, replace)
}
graph export ${result}/pred_effect_${diminitls}_ind_cmm_means.eps, replace
drop pred_match*


graph combine ${result}/pred_effect_${diminitls}_ind_cmatch_means.gph ${result}/pred_effect_${diminitls}_ind_cmm_means.gph, ycommon
graph export ${result}/pred_effect_${diminitls}_ind_cmatch_cmm_means.eps, replace


/********************************************/
/* Plot the paths */


use ${result}/yearly_03_phys_${diminitls}.dta, clear

/* average match quality by labor market experience */
twoway (lpoly match age if age <= 55 & age>=18, lwidth(thick) ) , ///
ytitle("Match Quality Measure") ///
xtitle("Age") ///
title("Match Quality by Age") ///
xlabel(20(5)55) legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/match_age, replace)
graph export ${result}/match_age_${diminitls}.eps, replace


/* average match quality by labor market experience */
twoway (lpoly mm age if age <= 55 & age>=18, lwidth(thick) ) , ///
ytitle("Mismatch Measure") ///
xtitle("Age") ///
title("Mismatch by Age") ///
xlabel(20(5)55) legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/mm_age, replace)
graph export ${result}/mm_age_${diminitls}.eps, replace


/* switch rate by mismatch */
twoway (lpoly fswitch_occ mm, lwidth(thick) ), ///
ytitle("Probability of Occupational Switch") ///
xtitle("Mismatch") ///
title("Probability of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pswitch_mm, replace)
graph export ${result}/pswitch_${diminitls}_mm.eps, replace

twoway (lpoly fswitch_occ_resid mm_resid , lwidth(thick)), ///
ytitle("Probability of Occupational Switch (residuals)") ///
xtitle("Mismatch (residuals)") ///
title("Probability of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pswitch_resid_mm, replace)
graph export ${result}/pswitch_resid_${diminitls}_mm.eps, replace


/* switch rate by negative/positive mismatch */
twoway (lpoly fswitch_occ mm_neg, lwidth(thick)) (lpoly fswitch_occ mm_pos, lwidth(thick)), ///
ytitle("Probability of Occupational Switch") ///
xtitle("Mismatch") ///
title("Probability of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pswitch_mm, replace)
graph export ${result}/pswitch_${diminitls}_mm_sgn.eps, replace

twoway (lpoly fswitch_occ_resid mm_neg_resid, lwidth(thick)) (lpoly fswitch_occ_resid mm_pos_resid, lwidth(thick)), ///
ytitle("Probability of Occupational Switch (residuals)") ///
xtitle("Mismatch (residuals)") ///
title("Probability of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pswitch_mm, replace)
graph export ${result}/pswitch_resid_${diminitls}_mm_sgn.eps, replace

/* switch rate by signed mismatch */
twoway (lpoly fswitch_occ mm_sgn, lwidth(thick)), ///
ytitle("Probability of Occupational Switch") ///
xtitle("Signed Mismatch") ///
title("Probability of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pswitch_sgnmm, replace)
graph export ${result}/pswitch_${diminitls}_sgnmm.eps, replace
/* switch rate by signed mismatch  with ci
twoway (lpoly fswitch_occ mm_sgn, lwidth(thick)), ///
ytitle("Probability of Occupational Switch") ///
xtitle("Signed Mismatch") ///
title("Probability of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot))
*/


twoway (lpoly fswitch_occ_resid mm_neg_resid, lwidth(thick)) (lpoly fswitch_occ_resid mm_pos_resid, lwidth(thick)), ///
ytitle("Probability of Occupational Switch (residuals)") ///
xtitle("Mismatch (residuals)") ///
title("Probability of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pswitch_mm, replace)
graph export ${result}/pswitch_resid_${diminitls}_mm_sgn.eps, replace

if($totphys == 1){
	/* switch rate by mismatch aa and bb*/
	twoway (lpoly fswitch_occ absmm_aa, lwidth(thick)) (lpoly fswitch_occ absmm_bb, lwidth(thick)), ///
	ytitle("Probability of Occupational Switch") ///
	xtitle("Mismatch") ///
	title("Probability of Occupational Switch") ///
	legend(lab(1 "Cognitive Mismatch") lab(2 "Physical  Mismatch")) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pswitch_mm, replace)
	graph export ${result}/pswitch_${diminitls}_mm_aa_mm_bb.eps, replace

	/* switch rate by negative/positive mismatch */
	twoway (lpoly fswitch_occ mm_aa_neg, lwidth(thick)) (lpoly fswitch_occ mm_aa_pos, lwidth(thick)) ///
	(lpoly switch_occ mm_bb_neg, lwidth(thick)) (lpoly switch_occ mm_bb_pos, lwidth(thick)), ///
	ytitle("Probability of Occupational Switch") ///
	xtitle("Mismatch") ///
	title("Probability of Occupational Switch") ///
	legend(lab(1 "Cognitive Mismatch") lab(2 "Cognitive Mismatch") lab(3 "Physical Mismatch") lab(4 "Physical Mismatch")) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pswitch_mm, replace)
	graph export ${result}/pswitch_${diminitls}_mm_aa_mm_bb_sgn.eps, replace
}
if($verbmathphys == 1){
	/* switch rate by mismatch aa and bb*/
	twoway (lpoly fswitch_occ absmm_aa, lwidth(thick)) (lpoly fswitch_occ absmm_bb, lwidth(thick)) (lpoly fswitch_occ absmm_cc, lwidth(thick)), ///
	ytitle("Probability of Occupational Switch") ///
	xtitle("Mismatch") ///
	title("Probability of Occupational Switch") ///
	legend(lab(1 "Verbal Mismatch") lab(2 "Math Mismatch") lab(3 "Physical  Mismatch")) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pswitch_mm, replace)
	graph export ${result}/pswitch_${diminitls}_mm_aa_mm_bb_mm_cc.eps, replace

	/* switch rate by negative/positive mismatch */
	twoway (lpoly fswitch_occ mm_aa_neg, lwidth(thick)) (lpoly fswitch_occ mm_aa_pos, lwidth(thick)) ///
	(lpoly switch_occ mm_bb_neg, lwidth(thick)) (lpoly switch_occ mm_bb_pos, lwidth(thick)) ///
	(lpoly switch_occ mm_cc_neg, lwidth(thick)) (lpoly switch_occ mm_cc_pos, lwidth(thick)), ///
	ytitle("Probability of Occupational Switch") ///
	xtitle("Mismatch") ///
	title("Probability of Occupational Switch") ///
	legend(lab(1 "Verbal Mismatch") lab(2 "Verbal Mismatch") lab(3 "Math Mismatch") lab(4 "Math Mismatch") ///
		lab(5 "Physical Mismatch") lab(6 "Physical Mismatch")) ///
	graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
	saving(${result}/pswitch_mm, replace)
	graph export ${result}/pswitch_${diminitls}_mm_aa_mm_bb_mm_cc_sgn.eps, replace
}

/* switch rate by match quality*/
twoway (lpoly fswitch_occ match, lwidth(thick) ), ///
ytitle("Probability of Occupational Switch") ///
xtitle("Match") ///
title("Probability of Occupational Switch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/pswitch_match, replace)
graph export ${result}/pswitch_${diminitls}_match.eps, replace

/* residuals
twoway (lpoly r2_mm_cmm_means  exp, lwidth(thick) ), ///
ytitle("Squared Residuals") ///
xtitle("Experience") ///
title("Heteroskedasticity in Experience") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/r2_mm_cmm_exp, replace)
graph export ${result}/r2_${diminitls}mm_cmm_exp.eps, replace
graph export ${result}/r2_${diminitls}mm_cmm_exp.png, replace

cumul skill_mean, gen(skill_mean_rnk) equal
cumul ability_mean, gen(ability_mean_rnk) equal

twoway (lpoly r2_mm_cmm_means  ability_mean_rnk, lwidth(thick) ), ///
ytitle("Squared Residuals") ///
xtitle("Mean Worker Skill (rank)") ///
title("Heteroskedasticity in Workers' Skill Rank") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/r2_mm_cmm_abil, replace)
graph export ${result}/r2_${diminitls}mm_cmm_abil.eps, replace
graph export ${result}/r2_${diminitls}mm_cmm_abil.png, replace

twoway (lpoly r2_mm_cmm_means  skill_mean_rnk, lwidth(thick) ), ///
ytitle("Squared Residuals") ///
xtitle("Mean Skill Requirements (rank)") ///
title("Heteroskedasticity in Occupational Requirements Rank") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/r2_mm_cmm_abil, replace)
graph export ${result}/r2_${diminitls}mm_cmm_skil.eps, replace
graph export ${result}/r2_${diminitls}mm_cmm_skil.png, replace


twoway (lpoly r2_mm_cmm_means  mm, lwidth(thick) ), ///
ytitle("Squared Residuals") ///
xtitle("Mismatch") ///
title("Heteroskedasticity in Mismatch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/r2_mm_cmm_mm, replace)
graph export ${result}/r2_${diminitls}mm_cmm_mm.eps, replace
graph export ${result}/r2_${diminitls}mm_cmm_mm.png, replace


twoway (lpoly resid_mm_cmm_means  exp, lwidth(thick) ) (scatter resid_mm_cmm_means  exp, msymbol(p) ), ///
ytitle("Residuals") ///
xtitle("Experience") ///
title("Residuals by Experience") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/resid_mm_cmm_exp, replace)
graph export ${result}/resid_${diminitls}mm_cmm_exp.eps, replace
graph export ${result}/resid_${diminitls}mm_cmm_exp.png, replace

twoway (lpoly resid_mm_cmm_means  exp, lwidth(thick) ), ///
ytitle("Residuals") ///
xtitle("Experience") ///
title("Residuals by Experience") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/resid_mm_cmm_exp, replace)
graph export ${result}/resid_${diminitls}mm_cmm_exp_nos.eps, replace
graph export ${result}/resid_${diminitls}mm_cmm_exp_nos.png, replace



*/


if($totphys ==1){
	local ti "CP"
}
if($verbmathphys ==1){
	local ti "VMP"
}

/* kernel density of mismatch */
kdensity mm, lwidth(thick) ///
ytitle("Density") ///
xtitle("Mismatch") ///
title("Density of mismatch, `ti' ") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/mm_density, replace)
graph export ${result}/mm_density_${diminitls}.eps, replace

kdensity mm, lwidth(thick) ///
ytitle("Density") ///
xtitle("Mismatch") ///
title("Density of mismatch") ///
legend(off) ///
graphregion(color(white)) xlabel(,grid gstyle(dot)) ylabel(,grid gstyle(dot)) ///
saving(${result}/mm_density, replace)
graph export ${result}/mm_density_${diminitls}_nolab.eps, replace






if($totphys ==1){
	local ti "CP"
}
if($verbmathphys ==1){
	local ti "VMP"
}

/*---------------------------------------------------------------------------------------------*/
* A few exploratory for match quality

use ${result}/yearly_03_phys_${diminitls}.dta, clear

gen educ_coarse = (hs==1) + (univ==1)

reg cmatch_ave i.educ_coarse
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

matsort ind_match 1 "down"

outtable using ${result}/ind_${diminitls}_match , mat(ind_match) f(%9.2f) replace caption("Match quality across industries, `ti'") clabel("tab:ind_${diminitls}_match") nobox


sum ind_1d, meanonly
local Nind = r(max)
matrix ind_cmatch = J(`Nind',3,0.0)

forvalues i=1/`Nind'{
	qui sum cmatch_ave if ind_1d ==`i'
	matrix ind_cmatch[`i',1]  = r(mean)
	matrix ind_cmatch[`i',2]  = r(sd)
*	_pctile cmatch_ave if ind_1d ==`i', p(10 50 90)
*	matrix ind_cmatch[`i',3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )
	sum rwage if ind_1d ==`i', meanonly
	matrix ind_cmatch[`i',3] = r(mean)
}

*matrix colnames ind_cmatch = "Average~Match" "Standard~Deviation" "Kelley's" "Average~Wage"
matrix colnames ind_cmatch = "Average~Match" "Standard~Deviation" "Average~Wage"
matrix rownames ind_cmatch =  "Agriculture" "Mining" "Construction" "Mfg" ///
"Trans~Comm~Util" "Retail~Trade" "FIRE" ///
"Business~Svcs" " Personal~Svcs" "Entertainment~Rec" "Prof~Services" "Public~Admin"

matsort ind_match 1 "down"

outtable using ${result}/ind_${diminitls}_cmatch , mat(ind_cmatch) f(%9.2f) replace caption("Cumulative match quality across industries") clabel("tab:ind_${diminitls}_cmatch") nobox


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

outtable using ${result}/edu_${diminitls}_match , mat(edu_match) f(%9.2f) replace caption("Match quality across education groups, `ti'") clabel("tab:edu_${diminitls}_match") nobox

matrix edu_cmatch = J(3,2,0.0)

qui sum cmatch_ave if lths==1
matrix edu_cmatch[1,1] = r(mean)
matrix edu_cmatch[1,2] = r(sd)
*_pctile cmatch_ave if lths==1, p(10 50 90)
*matrix edu_cmatch[1,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum cmatch_ave if hs ==1 & univ==0
matrix edu_cmatch[2,1] = r(mean)
matrix edu_cmatch[2,2] = r(sd)
*_pctile cmatch_ave if hs==1 & univ==0, p(10 50 90)
*matrix edu_cmatch[2,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum cmatch_ave if univ==1
matrix edu_cmatch[3,1] = r(mean)
matrix edu_cmatch[3,2] = r(sd)
*_pctile cmatch_ave if univ==1, p(10 50 90)
*matrix edu_cmatch[3,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

*matrix colnames edu_cmatch = "Average~Match" "Standard~Deviation" "Kelley's"
matrix colnames edu_cmatch = "Average~Match" "Standard~Deviation"
matrix rownames edu_cmatch =  "Less~than~HS" "HS" "College"

outtable using ${result}/edu_${diminitls}_cmatch , mat(edu_cmatch) f(%9.2f) replace caption("Cumulative match quality across education groups, `ti'") clabel("tab:edu_${diminitls}_cmatch")  nobox


/*---------------------------------------------------------------------------------------------*/
* A few exploratory for mismatch

use ${result}/yearly_03_phys_${diminitls}.dta, clear

xtreg mm, i(ind_1d)


gen educ_coarse = (hs==1) + (univ==1)


reg cmm_ave i.educ_coarse
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

outtable using ${result}/ind_${diminitls}_mm , mat(ind_mm) f(%9.2f) replace caption("Mismatch across industries, `ti'") clabel("tab:ind_${diminitls}_mm") nobox


sum ind_1d, meanonly
local Nind = r(max)
matrix ind_cmm = J(`Nind',3,0.0)

forvalues i=1/`Nind'{
	qui sum cmm_ave if ind_1d ==`i'
	matrix ind_cmm[`i',1]  = r(mean)
	matrix ind_cmm[`i',2]  = r(sd)
	*_pctile cmm_ave if ind_1d ==`i', p(10 50 90)
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

outtable using ${result}/ind_${diminitls}_cmm , mat(ind_cmm) f(%9.2f) replace caption("Cumulative mismatch across industries, `ti'") clabel("tab:ind_${diminitls}_cmm") nobox


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

outtable using ${result}/edu_${diminitls}_mm , mat(edu_mm) f(%9.2f) replace caption("Mismatch across education groups, `ti'") clabel("tab:edu_${diminitls}_mm") nobox

matrix edu_cmm = J(3,2,0.0)

qui sum cmm_ave if lths==1
matrix edu_cmm[1,1] = r(mean)
matrix edu_cmm[1,2] = r(sd)
*_pctile cmm_ave if lths==1, p(10 50 90)
*matrix edu_cmm[1,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum cmm_ave if hs ==1 & univ==0
matrix edu_cmm[2,1] = r(mean)
matrix edu_cmm[2,2] = r(sd)
*_pctile cmm_ave if hs==1 & univ==0, p(10 50 90)
*matrix edu_cmm[2,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

qui sum cmm_ave if univ==1
matrix edu_cmm[3,1] = r(mean)
matrix edu_cmm[3,2] = r(sd)
*_pctile cmm_ave if univ==1, p(10 50 90)
*matrix edu_cmm[3,3]  = (r(r3) - 2*r(r2) +r(r1) )/(r(r3)-r(r1) )

*matrix colnames edu_cmm = "Average~Mismatch" "Standard~Deviation" "Kelley's"
matrix colnames edu_cmm = "Average~Mismatch" "Standard~Deviation"
matrix rownames edu_cmm =  "Less~than~HS" "HS" "College"

outtable using ${result}/edu_${diminitls}_cmm , mat(edu_cmm) f(%9.2f) replace caption("Cumulative mismatch across education groups, `ti'") clabel("tab:edu_${diminitls}_cmm")  nobox


/* Export the correlation matrix between mismatch and other stuff*/
corr mm cmm_ave reswage AFQT_std 
matrix corr_mm = r(C)
matrix colnames corr_mm = "MM" "Cum~MM" "Wage" "AFQT"
matrix rownames corr_mm = "Mismatch" "Cumulative~Mismatch" "Residual~Wage" "AFQT"
outtable using ${result}/corr_${diminitls}_mm, mat(corr_mm) nobox replace f(%9.2f) center caption("Correlations with mismatch, `ti'") clabel(tab:corr_${diminitls}_mm)

/* Export the correlation matrix between mismatch dimensions */
corr absmm_??  if ind_indic ==1
matrix corr_mm = r(C)
matrix colnames corr_mm = "Verb" "Math" "Phys"
matrix rownames corr_mm = "Verb" "Math" "Phys"
outtable using ${result}/corr_${diminitls}_ind_mm, mat(corr_mm) nobox replace f(%9.2f) center caption("Correlations between mismatch dimensions") clabel(tab:corr_${diminitls}_ind_mm)

/* Export the correlation matrix between mismatch dimensions */
corr ability_?? skill_?? if ind_indic ==1
matrix corr_mm = r(C)
matrix colnames corr_mm = "Worker~Verb" "Worker~Math" "Worker~Phys"  "Occ~Verb" "Occ~Math" "Occ~Phys"
matrix rownames corr_mm = "Worker~Verb" "Worker~Math" "Worker~Phys"  "Occ~Verb" "Occ~Math" "Occ~Phys"
outtable using ${result}/corr_${diminitls}_abil_req, mat(corr_mm) nobox replace f(%9.2f) center caption("Correlations between skill dimensions") clabel(tab:corr_${diminitls}_abil_req)


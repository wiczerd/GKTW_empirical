/*--------------------------------------------------------------------------------------
* name: mismatch_03_cleaning.do
* made by: satoshi tanaka
* date: 08/21/2010
*       03/21/2014
* description: this code is for the project 'occupation skill mismatch'
* this code is partially based on pavan (2011).
--------------------------------------------------------------------------------------*/

/* this code is to create a set of necessary variables for our analysis */

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* first we clean the employment file */
use $data/employment_01.dta, clear

/* clean the link to the prev employer. the link points to two different employers, assume the first one is correct */
replace prev_emp2=-99 if prev_emp1==prev_emp2 & prev_emp1!=. & prev_emp1>0
replace prev_emp3=-99 if prev_emp1==prev_emp3 & prev_emp1!=. & prev_emp1>0
replace prev_emp4=-99 if prev_emp1==prev_emp4 & prev_emp1!=. & prev_emp1>0
replace prev_emp5=-99 if prev_emp1==prev_emp5 & prev_emp1!=. & prev_emp1>0
replace prev_emp3=-99 if prev_emp2==prev_emp3 & prev_emp2!=. & prev_emp2>0
replace prev_emp4=-99 if prev_emp2==prev_emp4 & prev_emp2!=. & prev_emp2>0
replace prev_emp5=-99 if prev_emp2==prev_emp5 & prev_emp2!=. & prev_emp2>0
replace prev_emp4=-99 if prev_emp3==prev_emp4 & prev_emp3!=. & prev_emp3>0		
replace prev_emp5=-99 if prev_emp3==prev_emp5 & prev_emp3!=. & prev_emp3>0
replace prev_emp5=-99 if prev_emp4==prev_emp5 & prev_emp4!=. & prev_emp4>0

/* define missing values */

forvalues i = 1/5 {
replace prev_emp`i' = -10 if prev_emp`i' == .
}

/* assign a potential number for each job. */
gen job_num_1 = (intvw_num-1)*5 + 1
gen job_num_2 = (intvw_num-1)*5 + 2
gen job_num_3 = (intvw_num-1)*5 + 3
gen job_num_4 = (intvw_num-1)*5 + 4
gen job_num_5 = (intvw_num-1)*5 + 5

/* replace each potential job number with the one from the prev year if it is the same */

sort id intvw_num
forvalues i=2/20 {
forvalues j=1/5 {
forvalues k=1/5 {
by id: replace job_num_`k'= job_num_`j'[_n-1] if prev_emp`k'==`j' & _n == `i'
}
}
}

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

sort id intvw_num

forvalues i = 1/5 {
replace hrwkd_emp`i' = -10 if hrwkd_emp`i' == .
}

drop if check_cps_emp1 == -5                                                                                                 /* CHECK LATER */

forvalues i=1/5 {
replace hrwkd_emp`i'= hrwkd_cps if check_cps_emp`i'==1 & (hrwkd_cps > 0 & hrwkd_cps !=. ) & (hrwkd_emp`i' < 0 | hrwkd_emp`i' == .)
                 }

sort id intvw_num

drop hrwkd_cps

save $data/employment_02.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* start cleaning weekly work history file */
use $data/weekly_01.dta, clear

gen intvw_num=int(lab_status/100)
                                                                                                                             /* DROP LATER */
*keep if time <= 1200

replace dual_emp1 = -4 if dual_emp1 == .
replace dual_emp2 = -4 if dual_emp2 == .
replace dual_emp3 = -4 if dual_emp3 == .
replace dual_emp4 = -4 if dual_emp4 == .
sort id intvw_num
merge m:1 id intvw_num using $data/employment_02
tab _m
drop if _m == 2
drop _m

/* input hours worked and wage for each job */
gen hrwkd = hrwkd_emp1 if lab_status == intvw_num*100 + 1

forvalues i=2/5 {
                 replace hrwkd = hrwkd_emp`i' if lab_status == intvw_num*100 +`i'
                 }
replace hrwkd = -10 if hrwkd ==.
replace hrwkd = -11 if lab_status ==0

keep id time lab_status dual_emp* hrwkd

forvalues i = 1/4 {
gen intvw_num = int(dual_emp`i'/100)

sort id intvw_num
merge m:1 id intvw_num using $data/employment_02
tab _m
drop if _m == 2
drop _m

gen hrwkd_d`i' = hrwkd_emp1 if dual_emp`i' == intvw_num*100 + 1

forvalues j = 2/5 {
replace hrwkd_d`i'= hrwkd_emp`j' if dual_emp`i' == intvw_num*100 + `j'
}

replace hrwkd_d`i' = -10 if hrwkd_d`i' ==.
replace hrwkd_d`i' = -11 if dual_emp`i' <= 0

keep id time lab_status dual_emp* hrwkd hrwkd_d* 
}

sort id time

save $data/weekly_02.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

use $data/weekly_02.dta, clear

/*creates the variable year*/
gen day = 6575 + 7*(time-1)
gen year = year(day)
drop day

/* for each week chose the job that has been worked the highest number of hours */
gen hrwkd_main=max(hrwkd,hrwkd_d1,hrwkd_d2,hrwkd_d3,hrwkd_d4)

gen job = lab_status
replace job = dual_emp1 if hrwkd_main == hrwkd_d1 & hrwkd_main > hrwkd /* & hrwkd_main > 0 */                                /* CHECK LATER */
replace job = dual_emp2 if hrwkd_main == hrwkd_d2 & hrwkd_main > hrwkd /* & hrwkd_main > 0 */
replace job = dual_emp3 if hrwkd_main == hrwkd_d3 & hrwkd_main > hrwkd /* & hrwkd_main > 0 */
replace job = dual_emp4 if hrwkd_main == hrwkd_d4 & hrwkd_main > hrwkd /* & hrwkd_main > 0 */

drop lab_status dual_emp* hrwkd hrwkd_d*

rename hrwkd_main hrs

/* let hours worked positive, for when I select one observation per year looking at the hours worked*/
replace hrs = 0.01 if hrs < 0
replace hrs = 0.5  if hrs == 0.01 & job > 100

/* now generates a variable that tell you how many hours you worked in one year (for jobs with more than 30 hours)*/
gen total_hrs = 0
replace total_hrs = hrs if hrs >= 30
sort id year
replace total_hrs = total_hrs + total_hrs[_n-1] if id == id[_n-1] & year == year[_n-1]
egen fulltime = max(total_hrs), by(id year)
drop total_hrs

save $data/weekly_03.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

use $data/weekly_03.dta, clear

/*get the number of the job*/
gen intvw_num = int(job/100)

sort id intvw_num
merge m:1 id intvw_num using $data/employment_02
tab _m
drop if _m == 2
drop _m
sort id year time

/*associate each job with its number*/
gen job_number = 0
replace job_number=job_num_1 if job-int(job/100)*100==1 & job>100
replace job_number=job_num_2 if job-int(job/100)*100==2 & job>100
replace job_number=job_num_3 if job-int(job/100)*100==3 & job>100
replace job_number=job_num_4 if job-int(job/100)*100==4 & job>100
replace job_number=job_num_5 if job-int(job/100)*100==5 & job>100

/* and now calculates how many hours have been worked in that job */

/* look at the sorting here, so if I have multiple non continuous spells of the same job, I can still add them up */
sort id year job_number

gen cum_hrs = hrs
by id year job_number: replace cum_hrs = sum(cum_hrs)
by id year job_number: egen hrs_each_job = max(cum_hrs)
drop cum_hrs

by id year: egen max_hrs = max(hrs_each_job)

gen job_index = 0
by id year: replace job_index = 1 if max_hrs == hrs_each_job

sort id year time
keep if job_index == 1
by id year: drop if job == job[_n-1]

keep id time year hrs fulltime intvw_num job job_number

save $data/weekly_04.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

use $data/employment_02.dta, clear

/* use occ_cps and ind_cps information if occ_emp and ind_emp are missing */
replace occ_emp1 = occ_cps if intvw_num == 1
replace ind_emp1 = ind_cps if intvw_num == 1

/* for intvw_num == 15, we need a special treatment because it uses census 1980 code for occ_emp and ind_emp, and census 1970 code for occ_cps and ind_cps */
/* convert census 1980 occupational code to occ1990 code */

forvalues i = 1/5 {
gen occ1980 = .
replace occ1980 = occ_emp`i' if intvw_num == 15
merge m:1 occ1980 using $crosswalk/occ1980_2_occ1990.dta
replace occ_emp`i' = occ1990 if intvw_num == 15
drop if _merge == 2
drop _merge occ1980 occ1990
}
/* convert census 1970 occupational code to occ1990 code */
gen occ1970 = .
replace occ1970 = occ_cps if intvw_num == 15
merge m:1 occ1970 using $crosswalk/occ1970_2_occ1990.dta
replace occ_cps = occ1990 if intvw_num == 15
drop if _merge == 2
drop _merge occ1970 occ1990

/* convert 3 digit level industry code to 1 digit level in 1970 */
forvalues i = 1/5 {
replace ind_emp`i' = 1  if ind_emp`i' >= 10  & ind_emp`i' <= 31  & intvw_num == 15 /* agriculture, foresty, and fisheries */
replace ind_emp`i' = 2  if ind_emp`i' >= 40  & ind_emp`i' <= 50  & intvw_num == 15 /* mining */
replace ind_emp`i' = 3  if ind_emp`i' == 60                      & intvw_num == 15 /* construction */
replace ind_emp`i' = 4  if ind_emp`i' >= 100 & ind_emp`i' <= 392 & intvw_num == 15 /* manufacturing */
replace ind_emp`i' = 5  if ind_emp`i' >= 400 & ind_emp`i' <= 472 & intvw_num == 15 /* transportation, communications, and other public utilities */
replace ind_emp`i' = 6  if ind_emp`i' >= 500 & ind_emp`i' <= 691 & intvw_num == 15 /* wholesale and retail trade */
replace ind_emp`i' = 7  if ind_emp`i' >= 700 & ind_emp`i' <= 712 & intvw_num == 15 /* finance, insurance and real estate */
replace ind_emp`i' = 8  if ind_emp`i' >= 721 & ind_emp`i' <= 760 & intvw_num == 15 /* business and repair services */
replace ind_emp`i' = 9  if ind_emp`i' >= 761 & ind_emp`i' <= 791 & intvw_num == 15 /* personal services */
replace ind_emp`i' = 10 if ind_emp`i' >= 800 & ind_emp`i' <= 802 & intvw_num == 15 /* entertainment and receation services */
replace ind_emp`i' = 11 if ind_emp`i' >= 812 & ind_emp`i' <= 892 & intvw_num == 15 /* profesional and related services */
replace ind_emp`i' = 12 if ind_emp`i' >= 900 & ind_emp`i' <= 932 & intvw_num == 15 /* public administration */
replace ind_emp`i' = -5 if ind_emp`i' >= 991 & ind_emp`i' <= 992 & intvw_num == 15 /* unemployed */
}

replace ind_cps = 1  if ind_cps >= 17  & ind_cps <= 29  & intvw_num == 15 /* agriculture, foresty, and fisheries */
replace ind_cps = 2  if ind_cps >= 47  & ind_cps <= 58  & intvw_num == 15 /* mining */
replace ind_cps = 3  if ind_cps >= 67  & ind_cps <= 78  & intvw_num == 15 /* construction */
replace ind_cps = 4  if ind_cps >= 107 & ind_cps <= 399 & intvw_num == 15 /* manufacturing */
replace ind_cps = 5  if ind_cps >= 407 & ind_cps <= 499 & intvw_num == 15 /* transportation, communications, and other public utilities */
replace ind_cps = 6  if ind_cps >= 507 & ind_cps <= 699 & intvw_num == 15 /* wholesale and retail trade */
replace ind_cps = 7  if ind_cps >= 707 & ind_cps <= 719 & intvw_num == 15 /* finance, insurance and real estate */
replace ind_cps = 8  if ind_cps >= 727 & ind_cps <= 767 & intvw_num == 15 /* business and repair services */
replace ind_cps = 9  if ind_cps >= 769 & ind_cps <= 799 & intvw_num == 15 /* personal services */
replace ind_cps = 10 if ind_cps >= 807 & ind_cps <= 817 & intvw_num == 15 /* entertainment and receation services */
replace ind_cps = 11 if ind_cps >= 828 & ind_cps <= 899 & intvw_num == 15 /* profesional and related services */
replace ind_cps = 12 if ind_cps >= 907 & ind_cps <= 947 & intvw_num == 15 /* public administration */
replace ind_cps = -5 if ind_cps >= 997 & ind_cps <= 999 & intvw_num == 15 /* unemployed */

forvalues i = 1/5 {
replace ind_emp`i' = -5 if ind_emp`i' > 12 & intvw_num == 15
replace ind_cps = -5 if ind_cps > 12 & intvw_num == 15
}

/* now both occupation and industry codes are consistent between occ_emp (ind_emp) and occ_cps (ind_cps) */

/*------------------------------------------------------------------------------------*/

/* use occ_cps and ind_cps information if occ_emp and ind_emp are missing */
forvalues i = 1/5 {
replace occ_emp`i' = occ_cps if intvw_num == 15 & `i' == 1 /* this is the only year which use census 1980 code */
replace ind_emp`i' = ind_cps if intvw_num == 15 & `i' == 1 /* this is the only year which use census 1980 code */
replace occ_emp`i' = occ_cps if intvw_num != 15 & check_cps_emp`i' == 1 & (occ_cps > 0 & occ_cps != .) & (occ_emp`i' <= 0 | occ_emp`i' == .)
replace ind_emp`i' = ind_cps if intvw_num != 15 & check_cps_emp`i' == 1 & (ind_cps > 0 & ind_cps != .) & (ind_emp`i' <= 0 | ind_emp`i' == .)
replace occ_emp`i' = occ_last_emp`i' if (occ_last_emp`i' > 0 & occ_last_emp`i' != .) & (occ_emp`i' <= 0 | occ_emp`i' == .) 
replace ind_emp`i' = ind_last_emp`i' if (ind_last_emp`i' > 0 & ind_last_emp`i' != .) & (ind_emp`i' <= 0 | ind_emp`i' == .)
replace occ_emp`i' = occ_all_emp`i' if (occ_all_emp`i' > 0 & occ_all_emp`i' != .) & (occ_emp`i' <= 0 | occ_emp`i' == .) 
replace ind_emp`i' = ind_all_emp`i' if (ind_all_emp`i' > 0 & ind_all_emp`i' != .) & (ind_emp`i' <= 0 | ind_emp`i' == .)
}

/*------------------------------------------------------------------------------------*/

/* if year > 2002, convert 'census 2000 4 digit' to 'census 2000 3 digit' */
forvalues i = 1/5{
replace occ_emp`i' = occ_emp`i'/10 if intvw_num > 20 & occ_emp`i' > 0
replace ind_emp`i' = int(ind_emp`i'/10) if intvw_num > 20 & ind_emp`i' > 0 
}

/* convert census 1970 and 2000 code to occ90 code */
forvalues i = 1/5{
sort occ_emp`i'

gen occ1970 = .
replace occ1970 = occ_emp`i' if intvw_num < 20 & intvw_num != 15
merge m:1 occ1970 using $crosswalk/occ1970_2_occ1990.dta
replace occ_emp`i' = occ1990 if intvw_num < 20 & intvw_num != 15
drop if _merge == 2
drop _merge occ1970 occ1990

gen occ2000 = .
replace occ2000 = occ_emp`i' if intvw_num >= 20
merge m:1 occ2000 using $crosswalk/occ2000_2_occ1990.dta
replace occ_emp`i' = occ1990 if intvw_num >= 20
drop if _merge == 2
drop _merge occ2000 occ1990
}

/*------------------------------------------------------------------------------------*/

/* convert industry code to 1 digit level */

forvalues i = 1/5 {
replace ind_emp`i' = 1  if ind_emp`i' >= 17  & ind_emp`i' <= 29  & intvw_num < 20 & intvw_num != 15 /* agriculture, foresty, and fisheries */
replace ind_emp`i' = 2  if ind_emp`i' >= 47  & ind_emp`i' <= 58  & intvw_num < 20 & intvw_num != 15 /* mining */
replace ind_emp`i' = 3  if ind_emp`i' >= 67  & ind_emp`i' <= 78  & intvw_num < 20 & intvw_num != 15 /* construction */
replace ind_emp`i' = 4  if ind_emp`i' >= 107 & ind_emp`i' <= 399 & intvw_num < 20 & intvw_num != 15 /* manufacturing */
replace ind_emp`i' = 5  if ind_emp`i' >= 407 & ind_emp`i' <= 499 & intvw_num < 20 & intvw_num != 15 /* transportation, communications, and other public utilities */
replace ind_emp`i' = 6  if ind_emp`i' >= 507 & ind_emp`i' <= 699 & intvw_num < 20 & intvw_num != 15 /* wholesale and retail trade */
replace ind_emp`i' = 7  if ind_emp`i' >= 707 & ind_emp`i' <= 719 & intvw_num < 20 & intvw_num != 15 /* finance, insurance and real estate */
replace ind_emp`i' = 8  if ind_emp`i' >= 727 & ind_emp`i' <= 767 & intvw_num < 20 & intvw_num != 15 /* business and repair services */
replace ind_emp`i' = 9  if ind_emp`i' >= 769 & ind_emp`i' <= 799 & intvw_num < 20 & intvw_num != 15 /* personal services */
replace ind_emp`i' = 10 if ind_emp`i' >= 807 & ind_emp`i' <= 817 & intvw_num < 20 & intvw_num != 15 /* entertainment and receation services */
replace ind_emp`i' = 11 if ind_emp`i' >= 828 & ind_emp`i' <= 899 & intvw_num < 20 & intvw_num != 15 /* profesional and related services */
replace ind_emp`i' = 12 if ind_emp`i' >= 907 & ind_emp`i' <= 947 & intvw_num < 20 & intvw_num != 15 /* public administration */
replace ind_emp`i' = -5 if ind_emp`i' >= 997 & ind_emp`i' <= 999 & intvw_num < 20 & intvw_num != 15 /* unemployed */
}

forvalues i = 1/5 {
replace ind_emp`i' = 1  if   ind_emp`i' >=  17 & ind_emp`i' <=  29   & intvw_num >= 20 /* agriculture, foresty, and fisheries */
replace ind_emp`i' = 2  if   ind_emp`i' >=  37 & ind_emp`i' <=  49   & intvw_num >= 20 /* mining */
replace ind_emp`i' = 3  if   ind_emp`i' ==  77                       & intvw_num >= 20 /* construction */
replace ind_emp`i' = 4  if ((ind_emp`i' >= 107 & ind_emp`i' <= 399) ///
                           |(ind_emp`i' >= 647 & ind_emp`i' <= 659) ///
			   |(ind_emp`i' >= 678 & ind_emp`i' <= 679)) & intvw_num >= 20 /* manufacturing */
replace ind_emp`i' = 5  if ((ind_emp`i' >=  57 & ind_emp`i' <=  69) ///
                           |(ind_emp`i' >= 607 & ind_emp`i' <= 639) ///
			   |(ind_emp`i' >= 667 & ind_emp`i' <= 669)) & intvw_num >= 20 /* transportation, communications, and other public utilities */
replace ind_emp`i' = 6  if ((ind_emp`i' >= 407 & ind_emp`i' <= 579) ///
                           |(ind_emp`i' >= 868 & ind_emp`i' <= 869)) & intvw_num >= 20 /* wholesale and retail trade */
replace ind_emp`i' = 7  if   ind_emp`i' >= 687 & ind_emp`i' <= 719   & intvw_num >= 20 /* finance, insurance and real estate */
replace ind_emp`i' = 8  if ((ind_emp`i' >= 877 & ind_emp`i' <= 879) ///
                           |(ind_emp`i' == 887))                     & intvw_num >= 20 /* business and repair services */
replace ind_emp`i' = 9  if ((ind_emp`i' >= 866 & ind_emp`i' <= 867) ///
                           |(ind_emp`i' >= 888 & ind_emp`i' <= 929)) & intvw_num >= 20 /* personal services */
replace ind_emp`i' = 10 if   ind_emp`i' >= 856 & ind_emp`i' <= 859   & intvw_num >= 20 /* entertainment and receation services */
replace ind_emp`i' = 11 if ((ind_emp`i' == 677)                     ///
                           |(ind_emp`i' >= 727 & ind_emp`i' <= 779) ///
                           |(ind_emp`i' >= 786 & ind_emp`i' <= 847)) & intvw_num >= 20 /* profesional and related services */
replace ind_emp`i' = 12 if   ind_emp`i' >= 937 & ind_emp`i' <= 959   & intvw_num >= 20 /* public administration */
replace ind_emp`i' = -5 if   ind_emp`i' == 992                       & intvw_num >= 20 /* unemployed */
}

forvalues i = 1/5 {
replace ind_emp`i' = -5 if ind_emp`i' > 12
}

/*------------------------------------------------------------------------------------*/

/* the following code clean occupational titles. given a spell of employment, occupational title should be 
the one which is most often observed during the spell, assuming that occupational switch doesn't happen without
changing employers */

forvalues i = 1/5 {
gen occ_num`i' = -99
gen ind_num`i' = -99
gen switch_num`i'= 1
}

/* this mapping arranges employer spells in order */
forvalues i = 1/5 {
gen emp_2_num`i' = -99
}

forvalues i = 1/5 {
replace emp_2_num`i' = `i'      if intvw_num == 1
replace occ_num`i' = occ_emp`i' if intvw_num == 1
replace ind_num`i' = ind_emp`i' if intvw_num == 1
}

gen tmp_occ = .
gen tmp_ind = .
sort id intvw_num
forvalues i = 1/5  {
replace occ_num`i' = occ_emp`i'
replace ind_num`i' = ind_emp`i'
replace emp_2_num`i' = `i'
}

/* occ_num are similar to occ_emp except that now employment spells are in order without using prev_emp indicator */
forvalues n = 2/24  {
forvalues i = 1/5  {
forvalues j = 1/5  {
forvalues k = 1/5  {
/* restore the occ and ind date for a moment */
replace tmp_occ    = occ_num`k' if intvw_num == `n' & prev_emp`i' == `j' & emp_2_num`j'[_n-1] == `k'
replace tmp_ind    = ind_num`k' if intvw_num == `n' & prev_emp`i' == `j' & emp_2_num`j'[_n-1] == `k'
/* rearrange the data */
replace occ_num`k' = occ_emp`i' if intvw_num == `n' & prev_emp`i' == `j' & emp_2_num`j'[_n-1] == `k'
replace ind_num`k' = ind_emp`i' if intvw_num == `n' & prev_emp`i' == `j' & emp_2_num`j'[_n-1] == `k'
replace switch_num`k'= 0        if intvw_num == `n' & prev_emp`i' == `j' & emp_2_num`j'[_n-1] == `k'
/* set new mapping */
forvalues l = 1/5  {
/* put the occ and ind data in the correct place */
replace occ_num`l' = tmp_occ    if intvw_num == `n' & prev_emp`i' == `j' & emp_2_num`j'[_n-1] == `k' & emp_2_num`i' == `l'
replace ind_num`l' = tmp_ind    if intvw_num == `n' & prev_emp`i' == `j' & emp_2_num`j'[_n-1] == `k' & emp_2_num`i' == `l'
forvalues m = 1/5  {
replace emp_2_num`m' = `l'      if intvw_num == `n' & prev_emp`i' == `j' & emp_2_num`j'[_n-1] == `k' & emp_2_num`i' == `l' & emp_2_num`m' == `k'
}
}
replace emp_2_num`i' = `k'      if intvw_num == `n' & prev_emp`i' == `j' & emp_2_num`j'[_n-1] == `k'
}
}
}
}
drop tmp_occ tmp_ind

sort id intvw_num

forvalues i = 1/5 {
replace occ_num`i' = . if occ_num`i' < 0
replace ind_num`i' = . if ind_num`i' < 0
}

forvalues i = 1/5 {
sort id intvw_num
by id: gen part_num`i' = sum(switch_num`i')
sort id part_num`i' occ_num`i' intvw_num
gen tenure_occ`i' = 1
replace tenure_occ`i' = tenure_occ`i'[_n-1] + 1 if id == id[_n-1] & part_num`i' == part_num`i'[_n-1] & occ_num`i' == occ_num`i'[_n-1] & occ_num`i' != .
bysort id part_num`i': egen max_tenure_occ`i' = max(tenure_occ`i')
gen true_occ`i' = occ_num`i' if max_tenure_occ`i' == tenure_occ`i' & occ_num`i' != .
/* cleaning */
sort id part_num`i' intvw_num
replace true_occ`i' = true_occ`i'[_n-1] if id == id[_n-1] & part_num`i' == part_num`i'[_n-1] & true_occ`i'[_n-1] != . & true_occ`i' == .
gsort +id +part_num`i' -intvw_num
replace true_occ`i' = true_occ`i'[_n-1] if id == id[_n-1] & part_num`i' == part_num`i'[_n-1] & true_occ`i'[_n-1] != .
replace occ_num`i' = true_occ`i'
}

forvalues i = 1/5 {
sort id intvw_num
sort id part_num`i' ind_num`i' intvw_num
gen tenure_ind`i' = 1
replace tenure_ind`i' = tenure_ind`i'[_n-1] + 1 if id == id[_n-1] & part_num`i' == part_num`i'[_n-1] & ind_num`i' == ind_num`i'[_n-1] & ind_num`i' != .
bysort id part_num`i': egen max_tenure_ind`i' = max(tenure_ind`i')
gen true_ind`i' = ind_num`i' if max_tenure_ind`i' == tenure_ind`i' & ind_num`i' != .
/* cleaning */
sort id part_num`i' intvw_num
replace true_ind`i' = true_ind`i'[_n-1] if id == id[_n-1] & part_num`i' == part_num`i'[_n-1] & true_ind`i'[_n-1] != . & true_ind`i' == .
gsort +id +part_num`i' -intvw_num
replace true_ind`i' = true_ind`i'[_n-1] if id == id[_n-1] & part_num`i' == part_num`i'[_n-1] & true_ind`i'[_n-1] != .
replace ind_num`i' = true_ind`i'
}

drop part_num* tenure_occ* max_tenure_occ* true_occ* tenure_ind* max_tenure_ind* true_ind*
sort id intvw_num

/* restore cleaned occ titles */
forvalues i = 1/5 {
forvalues j = 1/5 {
replace occ_emp`i' = occ_num`j' if emp_2_num`i' == `j'
replace ind_emp`i' = ind_num`j' if emp_2_num`i' == `j'
}
}

drop occ_num* ind_num* switch_num* emp_2_num*

save $data/employment_03.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

/* the following code clean demographic variables */
use $data/demographics_01.dta, clear

/* generate year */
gen     year = 1977 + intvw_num if intvw_num <= 16
replace year = 1993 + (intvw_num - 16)*2 if intvw_num > 16
sort id year

/* clean age */                                                                                                             /* CHECK LATER */ 
sort id intvw_num
replace age  = age[_n-1] + 1 if age[_n-1] > 0 & age[_n-1] != . & id == id[_n-1] & intvw_num <= 16
replace age  = age[_n-1] + 2 if age[_n-1] > 0 & age[_n-1] != . & id == id[_n-1] & intvw_num > 16
gsort id -intvw_num
replace age  = age[_n-1] - 1 if age[_n-1] > 0 & age[_n-1] != . & id == id[_n-1] & intvw_num < 16
replace age  = age[_n-1] - 2 if age[_n-1] > 0 & age[_n-1] != . & id == id[_n-1] & intvw_num >= 16
sort id intvw_num

/*------------------------------------------------------------------------------------*/

/* clean grade variable (1) */
/*
replace degree = -10 if degree == .
replace enrollmt = -10 if enrollmt == .

sort id intvw_num
replace grade = grade[_n-1] if ((grade < 0 & grade[_n-1] >= 0) | (grade < grade[_n-1] & grade >= 0))  &  id == id[_n-1]

gsort +id -intvw_num
replace grade = grade[_n-1] - 2 if intvw_num >= 16 & grade[_n-1] > 0 & grade[_n-1] > grade + 2 & id == id[_n-1]
replace grade = grade[_n-1] - 1 if intvw_num <  16 & grade[_n-1] > 0 & grade[_n-1] > grade + 1 & id == id[_n-1]

drop hsdiploma ged degree enrollmt
*/

/*------------------------------------------------------------------------------------*/

/* clean grade variable (2) */

sum
drop grade
rename grade_rev grade
drop if hsdiploma == -5                                                                                                     /* CHECK LATER */ 

by id:replace grade = grade[_n-1] if (grade  == -4 | grade  == . ) & grade[_n-1]  !=. & grade[_n-1]  > 0
by id:replace degree = degree[_n-1] if (degree == -4 | degree == . ) & degree[_n-1] !=. & degree[_n-1] > 0
by id:replace enrollmt = enrollmt[_n-1] if (enrollmt == -4 | enrollmt == .) & enrollmt[_n-1] !=. & enrollmt[_n-1] >= 0

tab grade
tab degree
tab ged

gen educ = -1
replace educ = 1 if degree == 1 | hsdiploma == 1
replace educ = 2 if degree == 2
replace educ = 3 if degree == 3 | degree == 4
replace educ = 4 if degree == 5 | degree == 6 | degree == 7

/* let's clean some errors */
by id: replace grade = grade[_n-1] + 1 if grade != grade[_n-1] & grade[_n+1] == grade[_n-1] + 2 & grade[_n-1] != .
by id: replace grade = grade[_n-1] if grade != grade[_n-1] & grade[_n+2] == grade[_n-1] & grade[_n-1] != .
by id: replace grade = grade[_n-1] if grade != grade[_n-1] & grade[_n+1] == grade[_n-1] & grade[_n-1] !=.

* fill some gaps and fix some issues
gsort id -intvw_num
by id: replace grade = grade[_n-1] if grade[_n-1] > 0 & grade[_n-1] != . & grade < 0 & enrollmt == 0
by id: replace grade = grade[_n-1] -1 if grade[_n-1] > 0 & grade[_n-1] != . & grade < 3 & enrollmt == 1

sort id intvw_num
by id: replace grade = grade[_n-1] if grade[_n-1] > 0 & grade[_n-1] != . & grade < 0 & enrollmt == 0
by id: replace grade = grade[_n+1] if grade < grade[_n-1] & grade < grade[_n+1] & grade[_n-1] != . & grade[_n+1] != .
by id: replace grade = grade[_n+1] if grade > grade[_n-1] & grade > grade[_n+1] & grade[_n-1] != . & grade[_n+1] != .
by id: replace grade = grade[_n-1] if grade[_n-1] > 0 & grade[_n-1] !=. & grade < grade[_n-1]-1 & grade[_n-1] <= grade[_n-2] + 2

by id:egen mgrade = max(grade)
replace educ = 0 if grade >= 0  & grade < 12
replace educ = 1 if grade >= 12 & grade < 13
replace educ = 2 if grade >= 13 & grade < 16
replace educ = 3 if grade >= 16
replace educ = 4 if grade >  16

/* they already reached the top */
by id:egen meduc = max(educ)
by id:replace educ = meduc if educ == -1 & educ[_n-1] == meduc
drop meduc

* let's look case by case
tab id if educ == -1
tab id if grade <= 0

/*
. tab id if educ == -1

        ID# |
  (1-12686) |
         79 |      Freq.     Percent        Cum.
------------+-----------------------------------
       5534 |          1      100.00      100.00
------------+-----------------------------------
      Total |          1      100.00

. tab id if grade <= 0

        ID# |
  (1-12686) |
         79 |      Freq.     Percent        Cum.
------------+-----------------------------------
       2815 |          1       50.00       50.00
       5534 |          1       50.00      100.00
------------+-----------------------------------
      Total |          2      100.00
*/

*this is the only place where I use ged or hs
replace grade = 8 if id == 2815 & grade <= 0
replace grade = 10 if id == 5534 & grade <= 0
replace educ = 0 if id == 5534 & educ == -1

sort id intvw_num

/*clear errors*/
by id:gen flag_1 = (educ[_n-1] == educ[_n+1] & educ != educ[_n-1] & educ[_n-1] != . )
*by id:replace educ = educ[_n-1] if educ[_n-1] == educ[_n+1] & educ != educ[_n-1] & educ[_n-1] != .

tab id if flag_1 == 1

drop sample_id enrollmt ged degree hsdiploma flag_1

/*no negative changes*/
by id:gen flag_2 = (educ[_n-1] !=. & educ < educ[_n-1] & educ <= 1 & educ[_n-1] > 1)

tab flag_2
drop flag_2 mgrade 

tab educ

save $data/demographics_02.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/
/* sample selection */

use $data/weekly_04.dta, clear

/*------------------------------------------------------------------------------------*/

/* select cross sectional sample only (not supplemental and military sample) */
/*   2236       1 CROSS MALE WHITE
      203       2 CROSS MALE WH. POOR
      346       3 CROSS MALE BLACK
      218       4 CROSS MALE HISPANIC
     2279       5 CROSS FEMALE WHITE
      198       6 CROSS FEMALE WH POOR
      405       7 CROSS FEMALE BLACK
      226       8 CROSS FEMALE HISPANIC
      742       9 SUP MALE WH POOR
     1105      10 SUP MALE BLACK
      729      11 SUP MALE HISPANIC
      901      12 SUP FEM WH POOR
     1067      13 SUP FEMALE BLACK
      751      14 SUP FEMALE HISPANIC
      609      15 MIL MALE WHITE
      162      16 MIL MALE BLACK
       53      17 MIL MALE HISPANIC
      342      18 MIL FEMALE WHITE
       89      19 MIL FEMALE BLACK
       25      20 MIL FEMALE HISPANIC   */
                                                                                                            /* CHECK LATER */ 
keep if year <= 2010

/*------------------------------------------------------------------------------------*/

/* this says how many people and how many obs we have */
sort id year
by id: gen nsmpl =_n
sum id if nsmpl == 1
by id year: gen nobs = _n
sum id if nobs == 1
drop nsmpl nobs

/*------------------------------------------------------------------------------------*/

/* these are the people that i consider already in the labor market when the survey started, because they worked more tha 1200 hours a year */
sort id time
gen nostart = 0
by id: replace nostart = 1 if year[_n-1] == 1977 & year == 1978 & hrs[_n-1] >= 30 & fulltime >= 1200 & job[_n-1] > 100
by id: replace nostart = 1 if year[_n-1] == 1977 & year == 1978 & fulltime >= 1200 & fulltime[_n+1] >= 1200 & job[_n-1] == 0
by id: egen nostarter = max(nostart)

sort id year time
gen interval = 0
replace interval = 1 if intvw_num == 0
by id: replace interval = interval[_n-1] + 1 if intvw_num == 0 & interval[_n-1] != .

merge m:m id year using $data/demographics_02.dta
ta _m
drop _m

/* identify people who get a degree after a pause */
sort id time
gen diff = 0
by id: replace diff = educ - educ[_n-1-interval[_n-1]] if intvw_num > 0 & intvw_num[_n-1] == 0  & job[_n-1] > 0
replace diff = 0 if diff == . 
ta diff

/* replace those people inside the sample if they were nostarters */
gen flag_1 = 0
replace flag_1 = 1 if diff > 0 & nostarter == 1
by id: egen flag_2 = max(flag_1)
replace nostarter = 0 if flag_2 == 1

/* drop samples before going to school for nostarter */
gen drop_obs_01 = 0
by id: replace drop_obs_01 = 1 if flag_1[_n+1] == 1
gsort +id - time
by id: replace drop_obs_01 = 1 if drop_obs_01[_n-1] == 1

/* drop nonstarter --- WE ARE STILL DOING THIS PART!!!!*/
gen drop_obs_02 = 0
replace drop_obs_02 = 1 if nostarter == 1

/* create a dummy for people who get a degree after a pause. I will check not to include obs before this */
gen school = 0
replace school = 1 if diff > 0

/* this says how many people and how many obs we have */
sort id year
by id: gen nsmpl =_n
sum id if nsmpl == 1
by id year: gen nobs = _n
sum id if nobs == 1
drop nsmpl nobs

/* drop observations in 1977. information in 1977 is summary of all the informaion before the survey. */
drop if year == 1977

/* this says how many people and how many obs we have */
sort id year
by id: gen nsmpl =_n
sum id if nsmpl == 1
by id year: gen nobs = _n
sum id if nobs == 1
drop nsmpl nobs

/* drop sample */
drop if drop_obs_01 == 1
drop if drop_obs_02 == 1

drop nostart nostarter flag_1 flag_2 educ interval diff

/* this says how many people and how many obs we have */
sort id year
by id: gen nsmpl =_n
sum id if nsmpl == 1
by id year: gen nobs = _n
sum id if nobs == 1
drop nsmpl nobs

/*------------------------------------------------------------------------------------*/

/* select people when they are in the labor force, drop the observations before then */
/* people start working if they work more than 1200 hours in two consecutive period */
sort id time
gen flag_1 = (fulltime >= 1200 & fulltime[_n+1] >= 1200 & id == id[_n+1])
replace flag_1 = 1 if flag_1[_n+1] == 1 & job_number == job_number[_n+1] & job_number > 0 & job_number !=. & id == id[_n+1]
replace flag_1 = 1 if flag_1[_n-1] == 1 & id == id[_n-1]                                                                      /* CHECK LATER */ 
gen drop_obs_03 = 0
replace drop_obs_03 = 1 if flag_1 == 0

/* drop sample */
drop if drop_obs_03 == 1

drop flag_1

/* this says how many people and how many obs we have */
sort id year
by id: gen nsmpl =_n
sum id if nsmpl == 1
by id year: gen nobs = _n
sum id if nobs == 1
drop nsmpl nobs

/*------------------------------------------------------------------------------------*/

/* erase obs if people are in the military force for at most 2 years, */
/* assume it does not count as experience. delete individuals otherwise */
gen military = 0
replace military = 1 if job == 7
by id: gen cum_mil = sum(military)
by id: egen check_mil = max(cum_mil)
gen drop_obs_04 = 0
replace drop_obs_04 = 1 if check_mil > 2 & check_mil != .
gen drop_obs_05 = 0
replace drop_obs_05 = 1 if military == 1

/* drop sample */
drop if drop_obs_04 == 1
drop if drop_obs_05 == 1

drop military cum_mil check_mil

/* this says how many people and how many obs we have */
sort id year
by id: gen nsmpl =_n
sum id if nsmpl == 1
by id year: gen nobs = _n
sum id if nobs == 1
drop nsmpl nobs

/*------------------------------------------------------------------------------------*/

/* take care of people that leave the market to go back to school */
sort id time
gen exp = 0
by id: replace exp = 1 if _n == 1
by id: replace exp = exp[_n-1] + year - year[_n-1] if year[_n-1] != .

gen flag_1 = 0
by id: replace flag_1 = 1 if exp <= 3 & school == 1 & year < 1994                                                            /* CHECK LATER */ 
by id: replace flag_1 = 1 if exp <= 5 & school == 1 & job[_n-2] < 100 & year < 1994
by id: replace flag_1 = 1 if exp <= 7 & school == 1 & job[_n-2] < 100 & job[_n-3] < 100 & job[_n-4] < 100 & year < 1994
by id: replace flag_1 = 1 if school == 1 & job[_n-2] < 100 & job[_n-3] < 100 & job[_n-4] < 100 & job[_n-5] < 100 & year < 1994

gen flag_2 = 0
by id: replace flag_2 = 1 if flag_1[_n+1] == 1
/* clean the preceading periods */
gsort +id -time
by id: replace flag_2 = 1 if flag_2[_n-1] == 1 & id == id[_n-1]
gen drop_obs_06 = 0
replace drop_obs_06 = 1 if flag_2 == 1	
sort id time

/* drop sample */
drop if drop_obs_06 == 1

drop flag_1 flag_2 exp school

/* this says how many people and how many obs we have */
sort id year
by id: gen nsmpl =_n
sum id if nsmpl == 1
by id year: gen nobs = _n
sum id if nobs == 1
drop nsmpl nobs

/*------------------------------------------------------------------------------------*/

/* delete last observations if empty */
gsort +id -time
gen empty = 0
by id: replace empty = 1 if _n == 1 & job < 100
by id: replace empty = 1 if empty[_n-1] == 1 & job < 100
gen drop_obs_07 = 0
replace drop_obs_07 =1 if empty == 1
sort id time

/* drop sample */
drop if drop_obs_07 == 1

drop empty

/* this says how many people and how many obs we have */
sort id year
by id: gen nsmpl =_n
sum id if nsmpl == 1
by id year: gen nobs = _n
sum id if nobs == 1
drop nsmpl nobs

/*------------------------------------------------------------------------------------*/

/* I drop individuals who have been more than one year out of the regular labor force */
/* (or only the obs afterwards if they have been at least 10 years in the market) */

gen skip = 0
replace skip = 1 if job < 100
by id: gen cum_skip = sum(skip)
egen total_skip = max(cum_skip), by(id)

gen nyears = 0
replace nyears = 1 if year != year[_n-1] & id == id[_n-1]
by id: replace nyears = sum(nyears)

/* drop observations if one and only one year out of the market */
gen drop_obs_08 = 0
replace drop_obs_08 = 1 if skip == 1 & cum_skip == 1

/* drop sample */
drop if drop_obs_08 == 1

/* this says how many people and how many obs we have */
sort id year
by id: gen nsmpl =_n
sum id if nsmpl == 1
by id year: gen nobs = _n
sum id if nobs == 1
drop nsmpl nobs

/*------------------------------------------------------------------------------------*/

/* drop obs for people after the second time they left the market but they stayed in it long enough */
sort id time
by id: replace skip = 2 if cum_skip[_n-1] < 2 & cum_skip == 2 & nyears >= 10
by id: replace skip = 2 if skip[_n-1] == 2
gen drop_obs_09 = 0
replace drop_obs_09 = 1 if skip ==2

/* drop sample */
drop if drop_obs_09 == 1

/* this says how many people and how many obs we have */
sort id year
by id: gen nsmpl =_n
sum id if nsmpl == 1
by id year: gen nobs = _n
sum id if nobs == 1
drop nsmpl nobs

/*------------------------------------------------------------------------------------*/

/* delete individuals if they are weakly attached to the labor force within 10 years */

by id: egen freq_skip = max(skip)
gen drop_obs_10 = 0
replace drop_obs_10 = 1 if freq_skip == 1

/* drop sample */
drop if drop_obs_10 == 1

drop skip total_skip freq_skip nyears cum_skip

/* this says how many people and how many obs we have */
sort id year
by id: gen nsmpl =_n
sum id if nsmpl == 1
by id year: gen nobs = _n
sum id if nobs == 1
drop nsmpl nobs

drop drop_obs*

/*------------------------------------------------------------------------------------*/
/* take care of double entry */

sort id year time
by id year: gen dup =_n
by id year: egen max_dup = max(dup)
tab max_dup
drop dup max_dup

/* drop the one sample from the double entry if the information is alredy used */
gen double_entry = 0
/* always the same job but more than one entry. the first has been used in the previous interview */
by id: replace double_entry = 1 if year==year[_n+1] & year!=year[_n-1] & job_number==job_number[_n-1] & job_number==job_number[_n+1] & job==job[_n-1]
drop if double_entry == 1

/* more than one entry. the last has been used in the next interview */
by id: replace double_entry = 1 if year==year[_n-1] & year!=year[_n+1] & job_number==job_number[_n-1] & job_number==job_number[_n+1] & job==job[_n+1]             /* CHECK LATER */
drop if double_entry == 1
drop double_entry

/* count numbers */
sort id year time
by id year: gen dup =_n
by id year: egen max_dup = max(dup)
tab max_dup
drop dup max_dup

save $data/weekly_05.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

use $data/weekly_05.dta, clear

sort id intvw_num
merge m:1 id intvw_num using $data/employment_03.dta
ta _m
drop if _m == 2
drop _m

gen occ = occ_emp1 if job-int(job/100)*100==1 & job>100
replace occ = occ_emp2 if job-int(job/100)*100==2 & job>100
replace occ = occ_emp3 if job-int(job/100)*100==3 & job>100
replace occ = occ_emp4 if job-int(job/100)*100==4 & job>100
replace occ = occ_emp5 if job-int(job/100)*100==5 & job>100
replace occ = -10 if occ == .

gen ind = ind_emp1 if job-int(job/100)*100==1 & job>100
replace ind = ind_emp2 if job-int(job/100)*100==2 & job>100
replace ind = ind_emp3 if job-int(job/100)*100==3 & job>100
replace ind = ind_emp4 if job-int(job/100)*100==4 & job>100
replace ind = ind_emp5 if job-int(job/100)*100==5 & job>100
replace ind = -10 if ind == .


gen wage = wage_emp1 if job-int(job/100)*100==1 & job>100
replace wage = wage_emp2 if job-int(job/100)*100==2 & job>100
replace wage = wage_emp3 if job-int(job/100)*100==3 & job>100
replace wage = wage_emp4 if job-int(job/100)*100==4 & job>100
replace wage = wage_emp5 if job-int(job/100)*100==5 & job>100
replace wage = -10 if wage == .

/* These are occ90 codes for which there is no occ2000 counterpart	*/
/* and therefore will not map into ONET information.   			*/
/* Instead, I took the next closest where it was clear that a close	*/ 
/* analog existed */

quietly replace occ = 4   if occ == 3
quietly replace occ = 26  if occ == 37
quietly replace occ = 69  if occ == 76
quietly replace occ = 154 if occ >= 113 & occ<=150
quietly replace occ = 214 if occ >= 213 & occ<=215
quietly replace occ = 315 if occ == 314
***quietly replace occ = 343 if occ == 344
quietly replace occ = 347 if occ == 345 | occ == 346
quietly replace occ = 389 if occ == 387
quietly replace occ = 405 if occ == 407
quietly replace occ = 427 if occ == 415
quietly replace occ = 448 if occ == 454
quietly replace occ = 439 if occ == 438
quietly replace occ = 475 if occ == 484
quietly replace occ = 549 if occ == 538
quietly replace occ = 666 if occ == 667
quietly replace occ = 637 if occ == 653
quietly replace occ = 666 if occ == 674
quietly replace occ = 733 if occ == 728
quietly replace occ = 727 if occ == 726
quietly replace occ = 727 if occ == 733
quietly replace occ = 734 if occ == 735
quietly replace occ = 783 if occ == 784
quietly replace occ = 799 if occ == 796
quietly replace occ = 883 if occ == 877
quietly replace occ = -10 if occ >= 900

sort occ

/*  Need to replace a few for which we do not have ONET data
      343 => 344 (cost and rate clerks to billing and related financial record clerks)
      346 => 347 (Mail and paper handlers to office machine operators, n.e.c.)
      474 => 475 (horticultural specialty farmers to horticultural specialty farm managers)
      717 => 779 (Fabricating machine operators, n.e.c. to machine operators n.e.c.)
      789 => 759 (Painting, coating and decorating to painting machine operators)
*/
***replace occ = 344 if occ==343
replace occ = 347 if occ==346
replace occ = 475 if occ==474
replace occ = 779 if occ==717
replace occ = 759 if occ==789


/* this was created by mismatch_01_KSA_setup.do */
rename occ occ90
merge m:1 occ90 using $onet/ONET_ASVAB_occ90.dta
rename occ90 occ
drop if _merge == 2
drop _merge

drop occ_emp* ind_emp* wage_emp*


save $data/weekly_06.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

use $data/weekly_06.dta, clear

/* convert occ nad ind to 1-digit level */
gen occ_1d = -10
replace occ_1d = 1  if occ >= 004 & occ<=199
replace occ_1d = 2  if occ >= 203 & occ<=389
replace occ_1d = 3  if occ >= 405 & occ<=472
replace occ_1d = 4  if occ >= 473 & occ<=498
replace occ_1d = 5  if occ >= 503 & occ<=699
replace occ_1d = 6  if occ >= 703 & occ<=889

/* for industry code, it's already in 1 digit level */
gen ind_1d = -10
replace ind_1d = ind

/* if wage information is missing in one entry, use the information from the other entry if job number is the same */
/* foward */
sort id year job_number time
replace wage = wage[_n-1] if wage < 0 & wage[_n-1] > 0 & id == id[_n-1] & year == year[_n-1] & job_number == job_number[_n-1]
/* backward */
gsort id year job_number -time
replace wage = wage[_n-1] if wage < 0 & wage[_n-1] > 0 & id == id[_n-1] & year == year[_n-1] & job_number == job_number[_n-1]

/* drop the one sample from the double entry if occ information is missing */
gen missing = 0
replace missing = 1 if (occ < 0)

sort id year missing
local count_dup = 1
while `count_dup' > 0 {
drop if (occ < 0) & (occ[_n-1] > 0) & id == id[_n-1] & year == year[_n-1]
quietly su if (occ < 0) & (occ[_n-1] > 0) & id == id[_n-1] & year == year[_n-1], meanonly
local count_dup = r(N)
}
drop missing

/* drop the one sample from the double entry if ind information is missing */
gen missing = 0
replace missing = 1 if (ind < 0)

sort id year missing
local count_dup = 1
while `count_dup' > 0 {
drop if (ind < 0) & (ind[_n-1] > 0) & id == id[_n-1] & year == year[_n-1]
quietly su if (ind < 0) & (ind[_n-1] > 0) & id == id[_n-1] & year == year[_n-1], meanonly
local count_dup = r(N)
}
drop missing

/* drop the one sample from the double entry if wage information is missing */
gen missing = 0
replace missing = 1 if (wage < 0)

sort id year missing
local count_dup = 1
while `count_dup' > 0 {
drop if (wage < 0) & (wage[_n-1] > 0) & id == id[_n-1] & year == year[_n-1]
quietly su if (wage < 0) & (wage[_n-1] > 0) & id == id[_n-1] & year == year[_n-1], meanonly
local count_dup = r(N)
}
drop missing

save $data/weekly_07.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

use $data/weekly_07.dta, clear

sort id year time
by id year: gen check_dup =_n
by id year: egen num_dup = max(check_dup)
tab num_dup
drop check_dup num_dup

/* the same info is used more times in different years, creating double entries (it might be that the double entry is with a different job)*/
sort id year time
by id year: gen check_dup =_n
by id year: egen num_dup = max(check_dup)
by id: drop if job==job[_n-1] & num_dup >1
drop check_dup num_dup

by id year: gen check_dup =_n
by id year: egen num_dup = max(check_dup)
by id: drop if job == job[_n+1] & num_dup >1
drop check_dup num_dup

/* drop the one sample from the double entry if wage information is missing */
gen missing = 0
replace missing = 1 if (wage < 0)

sort id year missing
local count_dup = 1
while `count_dup' > 0 {
drop if (wage < 0) & (wage[_n-1] > 0) & id == id[_n-1] & year == year[_n-1]
quietly su if (wage < 0) & (wage[_n-1] > 0) & id == id[_n-1] & year == year[_n-1], meanonly
local count_dup = r(N)
}
drop missing

sort id year time
by id year: gen check_dup =_n
by id year: egen num_dup = max(check_dup)
tab num_dup

/*------------------------------------------------------------------------------------*/

/* get the last wage info for each year so we have more info about the last wage before leaving a job*/
drop if num_dup == 2 & check_dup == 1 & job_number == job_number[_n+1]
drop if num_dup == 3 & check_dup == 1 & job_number == job_number[_n+1] & job_number[_n+1] != job_number[_n+2]
drop if num_dup == 3 & check_dup == 2 & job_number == job_number[_n+1] & job_number != job_number[_n-1]
drop num_dup

sort id year time
quietly by id year:replace check_dup = _n
quietly by id year: egen num_dup = max(check_dup)
by id: drop if num_dup == 2 & check_dup == 2 & job_number[_n-1] == job_number[_n-2] & job_number == job_number[_n+1]
quietly by id year:replace num_dup =_n
drop num_dup

/* take the entry that correspond to the right year*/
quietly by id year: egen num_dup = max(check_dup)
tab num_dup
drop if num_dup == 2 & check_dup == 1 & year-1978 != int(job/100) & year[_n+1]-1978 == int(job[_n+1]/100)
quietly by id year:replace check_dup =_n
drop num_dup
quietly by id year: egen num_dup = max(check_dup)
drop if num_dup == 2 & check_dup == 2 & year-1978 != int(job/100) & year[_n-1]-1978 == int(job[_n-1]/100)
quietly by id year:replace check_dup =_n
drop num_dup
quietly by id year: egen num_dup = max(check_dup)
drop if num_dup==2 & check_dup == 1
quietly by id year:replace check_dup = _n
tab check_dup

/* final care for duplications */
drop if check_dup > 1
drop check_dup num_dup

save $data/yearly_01.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

use $data/health_01.dta, clear

/* reshape */
reshape long q11_4_ q11_5_ q11_9_ q11_10_a_ q11_10_b_ q12_5_ alch_13_1_ alch_13_2_ alch_13_3_ alch_13_4_ alch_13_5_ alch_13_6_ alch_14_ health_height_ timeusetv_2_hrs_ timeusetv_2_mins_ ds_4_, i(id) j(year)

/* create height */
gen height = health_height_ if year == 1985 | year == 1982
replace height = round(health_height_/100)*12 + health_height_ - round(health_height_/100)*100 if year ==1981
replace height = q11_10_b_ + q11_10_a_*12 if q11_10_b_<=12
replace height = q11_10_b_ if q11_10_b_>12 & q11_10_b_<.
replace height = . if height<50 | height> 90 /* there're some people who answered crazy and also throw out height<0 */

rename q11_9_ weight
rename q11_5_ hlth_limamtwk
rename q11_4_ hlth_limkndwk
rename timeusetv_2_hrs_ hlth_tvhrs
rename timeusetv_2_mins_ hlth_tvmins
rename alch_14_ hlth_alchl_days_1
rename q12_5_ hlth_alchl_days_2
rename alch_13_1_ hlth_alchl_amt_1
rename alch_13_2_ hlth_alchl_amt_2
rename alch_13_3_ hlth_alchl_amt_3
rename alch_13_4_ hlth_alchl_amt_4
rename alch_13_5_ hlth_alchl_amt_5
rename alch_13_6_ hlth_alchl_amt_6
rename ds_4_ hlth_smoke

sort id year
keep id year hlth* bmom* bdad* height weight

save $data/health_02.dta, replace

/*------------------------------------------------------------------------------------*/
/*------------------------------------------------------------------------------------*/

use $data/yearly_01.dta, clear

/*------------------------------------------------------------------------------------*/
/* merging health variable */

merge m:m id year using $data/health_02.dta
keep if _merge == 3 | _merge == 1
drop _merge

/* clean height */
xtset id year
/* interpolate missing values*/
by id: ipolate height year, gen(tmp_height)
replace height = tmp_height if height>=. & tmp_height<.
drop tmp_height
/* fill missing values foward */
gsort +id +year
by id: replace height = height[_n-1] if height == . & height[_n-1] != .
/* fill missing values backward */
gsort +id -year
by id: replace height = height[_n-1] if height == . & height[_n-1] != .

/* clean weight */
sort id year
replace weight = . if weight < 0
by id: ipolate weight year, gen(tmp_weight)
replace weight = tmp_weight if weight == . & tmp_weight !=.
drop tmp_weight
/* fill missing values foward */
gsort +id +year
by id: replace weight = weight[_n-1] if weight == . & weight[_n-1] !=.
/* fill missing values backward */
gsort +id -year
by id: replace weight = weight[_n-1] if weight == . & weight[_n-1] !=.

/* calculate bmi */
gen bmi = weight/(height^2)*703

/* clean hlth_composite */
replace hlth_composite = . if hlth_composite < 0
gsort +id +year
by id: replace hlth_composite = hlth_composite[_n-1] if hlth_composite ==. & hlth_composite[_n-1] != .
gsort +id -year
by id: replace hlth_composite = hlth_composite[_n-1] if hlth_composite ==. & hlth_composite[_n-1] != .

/* clean other health variables */
foreach vi of varlist bdad_alive bdad_hlth bdad_death bmom_alive bmom_hlth bmom_death {
	replace `vi' = . if `vi'<0
}
sort id
foreach vi of varlist bdad_alive bdad_hlth bdad_death bmom_alive bmom_hlth bmom_death {
	by id: egen `vi'_tmp = max(`vi')
	replace `vi' = `vi'_tmp if `vi'_tmp<.
	drop `vi'_tmp
}

replace bdad_death = 0 if bdad_alive == 1
replace bmom_death = 0 if bmom_alive == 1

label var height "Height"
label var weight "Weight"
label var bdad_death "Dad Death Age"
label var bmom_death "Mom Death Age"
label var bdad_alive "Dad Alive"
label var bmom_alive "Mom Alive"
label var bmi "Body Mass Index"
label var hlth_limamtwk "HEALTH LIMITS AMOUNT OF WORK?"
label var hlth_limkndwk "HEALTH LIMITS KIND OF WORK?"
label var hlth_tvhrs "TIME USE TOT #HRS WATCH TV LAST WK"
label var hlth_tvmins "TIME USE TOT #MIN WATCH TV LAST WK"
label var hlth_alchl_days_1 "ALCHL-TOTAL # DAYS DRNK IN LAST MO"
label var hlth_alchl_days_2 "ALCHL-#DAYS DRANK ALCOHOL LAST MO"
label var hlth_alchl_amt_1 "ALCHL-#DAYS HAD 1 DRNK LAST MO"
label var hlth_alchl_amt_2 "ALCHL-#DAYS HAD 2 DRNK LAST MO"
label var hlth_alchl_amt_3 "ALCHL-#DAYS HAD 3 DRNK LAST MO"
label var hlth_alchl_amt_4 "ALCHL-#DAYS HAD 4 DRNK LAST MO"
label var hlth_alchl_amt_5 "ALCHL-#DAYS HAD 5 DRNK LAST MO"
label var hlth_alchl_amt_6 "ALCHL-#DAYS HAD 6 DRNK LAST MO"
label var hlth_smoke "AGE WHEN 1ST STARTED SMOKING DAILY?"


sort id year

/*------------------------------------------------------------------------------------*/

/* merging social variable */

merge m:1 id using $data/social_01.dta
drop _merge

/*------------------------------------------------------------------------------------*/

save $data/yearly_02.dta, replace

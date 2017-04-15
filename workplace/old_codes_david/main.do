/*--------------------------------------------------------------------------------------
* name: main.do
* made by: satoshi tanaka
* date: 08/21/2010
*       02/12/2014
* description: this code is for the project 'occupation skill mismatch'
* this code is partially the similar method as pavan (2011).
--------------------------------------------------------------------------------------*/

clear all
set maxvar 10000
set more off

/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/
/* CHANGE THIS PART */

global base_folder "~/workspace/GKTW_empirical/nlsy79"
global workplace "~/workspace/GKTW_empirical/nlsy79/workplace"
global download "~/workspace/GKTW_empirical/nlsy79/download"
global result "~/workspace/GKTW_empirical/nlsy79/result"
global onet "~/workspace/GKTW_empirical/nlsy79/onetstata"
global crosswalk "~/workspace/GKTW_empirical/nlsy79/crosswalk"

*global base_folder "/media/satoshi/portable_hdd/data/mismatch/nlsy79"
*global workplace "/media/satoshi/portable_hdd/data/mismatch/nlsy79/workplace"
*global download "/media/satoshi/portable_hdd/data/mismatch/nlsy79/download"
*global result "/media/satoshi/portable_hdd/data/mismatch/nlsy79/result"
*global onet /media/satoshi/portable_hdd/data/mismatch/nlsy79/onetstata"

/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/
*do $workplace/mismatch_01_KSA_setup.do
*do $workplace/mismatch_02_nlsy79_setup.do
*do $workplace/mismatch_03_cleaning.do
*do $workplace/mismatch_04_regression.do

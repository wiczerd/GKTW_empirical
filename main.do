/*--------------------------------------------------------------------------------------
* name: main.do
* made by: satoshi tanaka
* date: 08/21/2010
*       04/03/2015
* description: this code is for the project 'occupation skill mismatch'
* this code is partially the similar method as pavan (2011).
--------------------------------------------------------------------------------------*/

clear all
set maxvar 10000
set more off

/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/
/* CHANGE THIS PART */

*global base_folder "/home/satoshi/data/mismatch/nlsy79"
*global base_folder "/media/satoshi/portable_hdd/data/mismatch/nlsy79"
global base_folder "~/workspace/GKTW_empirical/repo"

/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/

global workplace $base_folder/workplace
global crosswalk $base_folder/crosswalk
global download $base_folder/download
global result $base_folder/result
global data $base_folder/data
global onet $base_folder/onetstata

/*--------------------------------------------------------------------------------------*/
/*--------------------------------------------------------------------------------------*/
*do $workplace/mismatch_01_KSA_setup.do
*do $workplace/mismatch_02_nlsy79_setup.do
*do $workplace/mismatch_03_cleaning.do
*do $workplace/mismatch_04_regression.do

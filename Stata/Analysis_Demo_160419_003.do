clear
clear matrix
version 13.1
set more off
capture log close

local filedate 160419
local filenum 003

log using ${anal}/Analysis_Demo_`filedate'_`filenum'.log, replace

/*******************************************************************************
DESCRIPTION
***********
Compared to ..._160419_001 this file estimates the effect of the youth share on 
age-specific (un-)employmen shares.
*******************************************************************************/

* OPEN DATA
***********

use ${data}/Data_Demo_Set-up_lmr_160407_001_saveold, clear


* MERGE IN POPULATION DATA
**************************

merge 1:1 lmr_id year using ${data_pop}/Data_Demo_Pop_15_17_lmr_160414_001, ///
nogen keep(match master) keepusing(pop_15_17)

merge 1:1 lmr_id year using ${data_pop}/Data_Demo_Pop_18_19_lmr_160414_001, ///
nogen keep(match master) keepusing(pop_18_19)


* MERGE IN INSTRUMENT DATA
**************************

merge 1:1 lmr_id year using ${data_ins2}/Data_Demo_Ins2_lmr_160411_001, ///
nogen keep(match master) keepusing(pop_20_24_ins2)

merge 1:1 lmr_id year using ${data_ins2}/Data_Demo_Ins2a_lmr_160411_001, ///
nogen keep(match master) keepusing(pop_18_24_ins2)

merge 1:1 lmr_id year using ${data_ins2}/Data_Demo_Ins2b_lmr_160411_001, ///
nogen keep(match master) keepusing(pop_15_24_ins2)


* GENERATE YOUTH SHARE VARIABLES
********************************

gen pop_share_20_24 = pop_20_24/(pop_20_24 + pop_25_29 + pop_30_34 + ///
pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64)

gen pop_share_18_24 = (pop_18_19 + pop_20_24)/(pop_18_19 + pop_20_24 + ///
pop_25_29 + pop_30_34 + pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + ///
pop_55_59 + pop_60_64)

gen pop_share_15_24 = (pop_15_17 + pop_18_19 + pop_20_24)/(pop_15_17 + ///
pop_18_19 + pop_20_24 + pop_25_29 + pop_30_34 + pop_35_39 + pop_40_44 + ///
pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64)

label variable pop_share_20_24 "Share of population 20-24 in population 20-64"

label variable pop_share_18_24 "Share of population 20-24 in population 18-64"

label variable pop_share_15_24 "Share of population 20-24 in population 15-64"


* GENERATE DEPENDENT VARIABLES
******************************

gen emp_share_20_24 = emp_20_24/(pop_20_24 + pop_25_29 + pop_30_34 + ///
pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64)

label variable emp_share_20_24 "Share of employed aged 20-24 in population aged 20-64"


* GENERATE LOGGED VARIABLES
***************************

foreach var of varlist pop_share_20_24 pop_share_18_24 pop_share_15_24 {

	gen ln_`var' = ln(`var')
	
}

foreach var of varlist pop_20_24_ins2 pop_18_24_ins2 pop_15_24_ins2 {

	gen ln_`var' = ln(`var')
	
}

foreach var of varlist emp_share_20_24 {

	gen ln_`var' = ln(`var')
	
}


* RESTRICT SAMPLE TO PERIOD 2000-2011
*************************************

drop if year>2011


* ANALYSIS
**********

regress ln_emp_share_20_24 ln_pop_share_20_24 i.year i.lmr_id, ///
vce(robust)
		
outreg2 using ${anal}/Analysis_Demo_`filedate'_`filenum'.xls, ///
excel append dec(5) ///
ctitle("ln_emp_share_20_24", "OLS", "robust") ///
sortvar(ln_pop_share_20_24 ln_pop_share_18_24 ln_pop_share_15_24)
		
ivregress 2sls ln_emp_share_20_24 i.year i.lmr_id ///
(ln_pop_share_20_24 = ln_pop_20_24_ins2), ///
vce(robust)
	
outreg2 using ${anal}/Analysis_Demo_`filedate'_`filenum'.xls, ///
excel append dec(5) ///
ctitle("ln_emp_share_20_24", "2SLS", "robust") ///
sortvar(ln_pop_share_20_24 ln_pop_share_18_24 ln_pop_share_15_24)


log close

exit, clear

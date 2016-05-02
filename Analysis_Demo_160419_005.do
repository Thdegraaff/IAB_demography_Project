clear
clear matrix
version 13.1
set more off
capture log close

local filedate 160419
local filenum 005

log using ${anal}/Analysis_Demo_`filedate'_`filenum'.log, replace

/*******************************************************************************
DESCRIPTION
***********
This file assesses the sensitivity of the coefficients to the exclusion of 
individual years. 

This is done for the unemployment outcome and the age group 18-24.
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

gen unemp_rate_20_24 = unemp/(pop_20_24 + pop_25_29 + pop_30_34 + ///
pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64)

gen emp_rate_20_24 = emp/(pop_20_24 + pop_25_29 + pop_30_34 + ///
pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64)


gen unemp_rate_18_24 = unemp/(pop_18_19 + pop_20_24 + pop_25_29 + pop_30_34 + ///
pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64)

gen emp_rate_18_24 = emp/(pop_18_19 + pop_20_24 + pop_25_29 + pop_30_34 + ///
pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64)


gen unemp_rate_15_24 = unemp/(pop_15_17 + pop_18_19 + pop_20_24 + pop_25_29 + ///
pop_30_34 + pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64)

gen emp_rate_15_24 = emp/(pop_15_17 + pop_18_19 + pop_20_24 + pop_25_29 + ///
pop_30_34 + pop_35_39 + pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64)

label variable unemp_rate_20_24 "Ratio of unemployed to population 20-64"

label variable emp_rate_20_24 "Ratio of employed to population 20-64"

label variable unemp_rate_18_24 "Ratio of unemployed to population 18-64"

label variable emp_rate_18_24 "Ratio of employed to population 18-64"

label variable unemp_rate_15_24 "Ratio of unemployed to population 15-64"

label variable emp_rate_15_24 "Ratio of employed to population 15-64"


* GENERATE LOGGED VARIABLES
***************************

foreach var of varlist pop_share_20_24 pop_share_18_24 pop_share_15_24 {

	gen ln_`var' = ln(`var')
	
}

foreach var of varlist pop_20_24_ins2 pop_18_24_ins2 pop_15_24_ins2 {

	gen ln_`var' = ln(`var')
	
}

foreach var of varlist unemp_rate_20_24 unemp_rate_18_24 unemp_rate_15_24 {

	gen ln_`var' = ln(`var')
	
}

foreach var of varlist emp_rate_20_24 emp_rate_18_24 emp_rate_15_24 {

	gen ln_`var' = ln(`var')
	
}


* RESTRICT SAMPLE TO PERIOD 2000-2011
*************************************

drop if year>2011


* ANALYSIS (FULL MODEL)
***********************

ivregress 2sls ln_unemp_rate_18_24 i.year i.lmr_id ///
(ln_pop_share_18_24 = ln_pop_18_24_ins2), ///
vce(robust)
	
outreg2 using ${anal}/Analysis_Demo_`filedate'_`filenum'.xls, ///
excel append dec(5) ///
ctitle("ln_unemp_rate_18_24", "2SLS", "robust", "full") ///
sortvar(ln_pop_share_20_24 ln_pop_share_18_24 ln_pop_share_15_24)

gen coef_full = .
gen upper_full = .
gen lower_full = .

replace coef_full = _b[ln_pop_share_18_24]

replace upper_full = _b[ln_pop_share_18_24] + 1.96*_se[ln_pop_share_18_24]

replace lower_full = _b[ln_pop_share_18_24] - 1.96*_se[ln_pop_share_18_24]


* ANALYSIS (RESTRICTED)
***********************

gen coef = .
gen upper = .
gen lower = .

forvalues y=2000/2011 {

	ivregress 2sls ln_unemp_rate_18_24 i.year i.lmr_id ///
	(ln_pop_share_18_24 = ln_pop_18_24_ins2) ///
	if year!=`y', ///
	vce(robust)
	
	outreg2 using ${anal}/Analysis_Demo_`filedate'_`filenum'.xls, ///
	excel append dec(5) ///
	ctitle("ln_unemp_rate_18_24", "2SLS", "robust", "drop: `y'") ///
	sortvar(ln_pop_share_20_24 ln_pop_share_18_24 ln_pop_share_15_24)
	
	replace coef = _b[ln_pop_share_18_24] if year==`y'
	
	replace upper = _b[ln_pop_share_18_24] + 1.96*_se[ln_pop_share_18_24] if year==`y'
	
	replace lower = _b[ln_pop_share_18_24] - 1.96*_se[ln_pop_share_18_24] if year==`y'
	
}


* COLLAPSE DATA
***************

collapse (mean) coef_full upper_full lower_full coef upper lower, by(year)

foreach var of varlist coef_full upper_full lower_full {

	sum `var'
	
	local l`var' = r(mean)
	
}


* DOT PLOT
**********

graph dot lower coef upper, over(year) ///
marker(1, symbol(x) mcolor(red)) ///
marker(2, symbol(diamond) mcolor(red)) ///
marker(3, symbol(x) mcolor(red)) ///
yline(`lupper_full', lpattern(dash) lcolor(blue)) ///
yline(`lcoef_full', lpattern(solid) lcolor(blue)) ///
yline(`llower_full', lpattern(dash) lcolor(blue)) ///
legend(label(1 "lower CI (95%)") label(2 "Coefficient") label(3 "upper CI (95%)")) ///
legend(rows(1))

graph save ${anal}/Analysis_Demo_`filedate'_`filenum'.gph, replace

graph export ${anal}/Analysis_Demo_`filedate'_`filenum'.emf, replace 

log close

exit, clear


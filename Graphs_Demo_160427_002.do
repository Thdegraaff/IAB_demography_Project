clear
clear matrix
version 13.1
set more off
capture log close

local filedate 160427
local filenum 002

log using ${graphs}/Graphs_Demo_`filedate'_`filenum'.log, replace

/*******************************************************************************
DESCRIPTION
***********
This file plots the employment rate, unemployment rate and the youth share over 
time for selected regions. These are chosen according to the estimated effect of 
the youth share in the FMM analysis:

1) Wolfsburg (7)
2) Hamburg (5)
3) Regensburg (90)
4) Uckermark (120)
5) Bochum (37)
6) Chemnitz (126)

The graphs uses unadjusted and mean-adjusted variables.
*******************************************************************************/

* OPEN DATA
***********

use ${data}/Data_Demo_Set-up_lmr_160407_001_saveold.dta, replace


* MERGE IN POPULATION DATA
**************************

merge 1:1 lmr_id year using ${data_pop}/Data_Demo_Pop_18_19_lmr_160414_001, ///
nogen keep(match master) keepusing(pop_18_19)


* GENERATE POPULATION VARIABLES
*******************************

gen pop_18_24 = pop_18_19 + pop_20_24

gen pop_18_64 = pop_18_19 + pop_20_24 + pop_25_29 + pop_30_34 + pop_35_39 + ///
pop_40_44 + pop_45_49 + pop_50_54 + pop_55_59 + pop_60_64


* GENERATE VARIABLES
********************

gen emp_rate = emp/pop_18_64

gen unemp_rate = unemp/pop_18_64

gen youth_share = pop_18_24/pop_18_64


* DROP REDUNDANT OBSERVATIONS
*****************************

drop if year>2010

keep if inlist(lmr_id, 5, 7, 37, 90, 120, 126)


* FORMAT VARIABLES
******************

format %9.1f emp_rate unemp_rate

format %9.0f youth_share


* GENERATE GRAPHS
*****************

graph twoway (line emp_rate year, yaxis(1) lpattern(___###)) ///
(line unemp_rate year, yaxis(1) lpattern(---...)) ///
(line youth_share year, yaxis(2) lpattern(solid)), ///
by(lmr_name, ixaxes note("") rows(2)) ///
legend(label(1 "Employment rate") label(2 "Unemployment rate") ///
label(3 "Youth share")) ///
legend(rows(2)) ///
ylabel(0.11(0.025)0.16, axis(2)) ///
ytitle("(Un-)employment rate", axis(1)) ytitle("Youth share", axis(2)) ///
xtick(2001(1)2009)

graph save ${graphs}/Graphs_Demo_`filedate'_`filenum'_unadj.gph, replace

graph export ${graphs}/Graphs_Demo_`filedate'_`filenum'_unadj.png, replace


* GENERATE MEAN-ADJUSTED VARIABLES
**********************************

bysort lmr_id: egen emp_rate_m = mean(emp_rate)

bysort lmr_id: egen unemp_rate_m = mean(unemp_rate)

bysort lmr_id: egen youth_share_m = mean(youth_share)

foreach var of varlist emp_rate unemp_rate youth_share {

	gen `var'_adj = `var' - `var'_m
	
}


* FORMAT VARIABLES
******************

format %9.2f emp_rate_adj unemp_rate_adj

format %9.2f youth_share_adj


* GENERATE GRAPHS
*****************

graph twoway (line emp_rate_adj year, yaxis(1) lpattern(___###)) ///
(line unemp_rate_adj year, yaxis(1) lpattern(---...)) ///
(line youth_share_adj year, yaxis(2) lpattern(solid)), ///
by(lmr_name, ixaxes note("") rows(2)) ///
legend(label(1 "Employment rate") label(2 "Unemployment rate") ///
label(3 "Youth share")) ///
legend(rows(2)) ///
ytitle("(Un-)employment rate", axis(1)) ytitle("Youth share", axis(2)) ///
ylabel(-0.04(0.04)0.04, axis(1)) ///
ylabel(-0.02(0.02)0.02, axis(2)) ///
ytick(-0.02(0.04)0.02, axis(1)) ///
ytick(-0.01(0.02)0.01, axis(2)) ///
yline(0, lcolor(black)) ///
xtick(2001(1)2009)

graph save ${graphs}/Graphs_Demo_`filedate'_`filenum'_adj.gph, replace

graph export ${graphs}/Graphs_Demo_`filedate'_`filenum'_adj.png, replace


log close

exit, clear



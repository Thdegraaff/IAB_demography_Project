clear all
clear matrix
set more on
version 13.1

/*******************************************************************************
DESCRIPTION
***********
This file is for ex post analysis of FMM clusters and analysis have clusters are  
different from each other on the basis of demographic and economic indicators

*******************************************************************************/

* log using "C:\Users\tgf200\Dropbox\Demography_Project\Employment\Output\ExpostAnalysis.log", replace

import delimited C:\Users\tgf200\Dropbox\Demography_Project\Employment\Data\EmpData.csv,clear
merge m:1 lmr_id using C:\Users\tgf200\Dropbox\Demography_Project\Employment\Data\Clusters.dta
drop id _merge
rename clustermean cluster


***ANALYSIS OF CLUSTERS

* LMRs by clusters
bysort cluster: tab lmr_name cluster

* Clusters by demographic and economic characteristics over time
local graphs "C:\Users\tgf200\Dropbox\Demography_Project\Employment\Figs\"

foreach var of varlist unemprate emprate foremprate youthshare{
	bysort year cluster: egen trend`var'=mean(`var')
	bysort year: egen `var'm= mean(`var')
	gen trend1`var'=trend`var' if cluster==1
	gen trend2`var'=trend`var' if cluster==2
	gen trend3`var'=trend`var' if cluster==3
	gen trend4`var'=trend`var' if cluster==4
	graph twoway (line trend1`var' year) (line trend2`var' year) (line trend3`var' year) (line trend4`var' year) (line `var'm year, lpattern(longdash)), ///
	legend(label(1 "Cluster 1") label(2 "Cluster 2") label(3 "Cluster 3") label(4 "Cluster 4") label(5 "Mean `var' Germany ")) ///
	title("Mean `var' by clusters")
	graph export ${graphs}\`var'_byclusters.png, replace
	drop trend* `var'm
	}
	
* Clusters by occupational distribution

destring emp_occ2, replace force
recode emp_occ2 (.=0)
recast long emp_occ1 emp_occ2 emp_occ3 emp_occ4 emp_occ5 emp_occ6 ,force
format %9.0f emp  emp_occ1 emp_occ2 emp_occ3  emp_occ4 emp_occ5  emp_occ6

forvalues i=1(1)6 {
	gen shemp_occ`i'=(emp_occ`i'/emp)
} 

forvalues i=1(1)12 {
	gen shemp_task`i'=(emp_task_`i'/emp)
} 


gen shpop_ger=pop_ger/pop
gen shpop_for=pop_for/pop

gen e_occ1_share = emp_occ1/(emp_occ1 + emp_occ2 + emp_occ3 + emp_occ4 + emp_occ5 + emp_occ6)
gen e_occ2_share = emp_occ2/(emp_occ1 + emp_occ2 + emp_occ3 + emp_occ4 + emp_occ5 + emp_occ6)
gen e_occ3_share = emp_occ3/(emp_occ1 + emp_occ2 + emp_occ3 + emp_occ4 + emp_occ5 + emp_occ6)
gen e_occ4_share = emp_occ4/(emp_occ1 + emp_occ2 + emp_occ3 + emp_occ4 + emp_occ5 + emp_occ6)
gen e_occ5_share = emp_occ5/(emp_occ1 + emp_occ2 + emp_occ3 + emp_occ4 + emp_occ5 + emp_occ6)
gen e_occ6_share = emp_occ6/(emp_occ1 + emp_occ2 + emp_occ3 + emp_occ4 + emp_occ5 + emp_occ6)

gen t_sh1 = emp_task_1/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)
gen t_sh2 = emp_task_2/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)
gen t_sh3 = emp_task_3/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)
gen t_sh4 = emp_task_4/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)						
gen t_sh5 = emp_task_5/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)
gen t_sh6 = emp_task_6/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)
gen t_sh7 = emp_task_7/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)
gen t_sh8 = emp_task_8/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)
gen t_sh9 = emp_task_9/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)
gen t_sh10 = emp_task_10/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)
gen t_sh11 = emp_task_11/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)
gen t_sh12 = emp_task_12/(emp_task_1+ emp_task_2+ emp_task_3+ emp_task_4+ emp_task_5+ emp_task_6 + ///
						emp_task_7+ emp_task_8+ emp_task_9+ emp_task_10+ emp_task_11+ emp_task_12)
						
						* Summary statistics (by clusters)
sum youthshare logyouthshare pop shpop_ger shpop_for participationrate shemp_task* ///
emp unemp emprate unemprate foremprate

bysort cluster:sum  youthshare logyouthshare pop shpop_ger shpop_for participationrate
bysort cluster:sum logemptask1 logemptask2 logemptask3 logemptask4 logemptask5 logemptask6 logemptask7 ///
logemptask8 logemptask9 logemptask10 logemptask11 logemptask12
bysort cluster:sum shemp_task*	

bysort cluster:sum emp_occ1 emp_occ2 emp_occ3  emp_occ4 emp_occ5  emp_occ6
bysort cluster:sum e_occ1_share e_occ2_share e_occ3_share e_occ4_share e_occ5_share e_occ6_share 
bysort cluster:sum t_sh1 t_sh2 t_sh3 t_sh4 t_sh5 t_sh6 t_sh7 t_sh8 t_sh9 t_sh10 t_sh11 t_sh12

bysort cluster:sum participationrate popshare

*Cluster 4, excluding Berlin, Frankfurt, Munch
sum youthshare logyouthshare pop shpop_ger shpop_for participationrate shemp_task* /// 
emp unemp emprate unemprate foremprate if cluster==4 & lmr_id!=80 & lmr_id!=109& lmr_id!=43

graph box youthshare, over(cluster)
graph export "C:\Users\con600\Dropbox\Demography_Project\Employment\Figs\Youthshare_byclusters.eps",  replace

/* Scatters of youthshare vs depvars across 10 years study period
graph dir
graph drop _all

forvalues i=2000(1)2010{
scatter unemprate youthshare if year==`i', saving(UnempYS`i') 
graph export "C:\Users\con600\Dropbox\Demography_Project\Employment\Figs\UnempYS`i'.png", replace
}

gr combine UnempYS2000.gph UnempYS2001.gph UnempYS2002.gph UnempYS2003.gph ///
UnempYS2004.gph UnempYS2005.gph UnempYS2006.gph UnempYS2007.gph UnempYS2008.gph ///
UnempYS2009.gph UnempYS2010.gph
graph export "C:\Users\con600\Dropbox\Demography_Project\Employment\Figs\UnempYS.png", replace

forvalues i=2000(1)2010{
scatter emprate youthshare if year==`i', saving(empYS`i') 
} 

gr combine empYS2000.gph empYS2001.gph empYS2002.gph empYS2003.gph ///
empYS2004.gph empYS2005.gph empYS2006.gph empYS2007.gph empYS2008.gph ///
empYS2009.gph empYS2010.gph
graph export "C:\Users\con600\Dropbox\Demography_Project\Employment\Figs\EmpYS.png", replace
*/

log close

* Excel table of summary statistics of clusters 
local s "pop shpop_for shpop_ger youthshare logyouthshare emp emprate foremprate unemprate participationrate shemp_task1-shemp_task12"
logout, clear: bysort cluster*: sum `s'
logout, save(C:\Users\con600\Dropbox\Demography_Project\Employment\Output\Econvars) excel replace


* Further analysis

* Dummies explain substantial variation in the logyouthshare 
sum logyouthshare, d

reg logyouthshare i.lmr_id i.year [aweight=popshare]
predict yhat
sum yhat, d


//////////////////////////////////////////////////////////////////////////////////
	
/*graph twoway (line emprate year, yaxis(1) lpattern(___###)) ///
(line unemprate year, yaxis(1) lpattern(---...)) ///
(line youthshare year, yaxis(2) lpattern(solid)), ///
by(lmr_name, ixaxes note("") rows(2)) ///
legend(label(1 "Employment rate") label(2 "Unemployment rate") ///
label(3 "Youth share")) ///
legend(rows(2)) ///
ylabel(0.11(0.025)0.16, axis(2)) ///
ytitle("(Un-)employment rate", axis(1)) ytitle("Youth share", axis(2)) ///
xtick(2001(1)2009)	

graph export "C:\Users\con600\Dropbox\Demography_Project\Employment\Figs\YouthShare_Emprate_Unemprate.eps", replace
*/



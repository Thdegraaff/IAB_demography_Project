clear
clear matrix
version 13.1
set more off
capture log close

local filedate 160403
local filenum 001

log using ${graphs}/Graphs_Demo_`filedate'_`filenum'.log, replace

/*******************************************************************************
DESCRIPTION
***********
This fileplots the development of the youth share over time for different 
labour-market regions. The youth share is defined as the population aged 
20-24 divided by total population.

It can be seen that the there is a sharp decrease in the size of the youth share 
in the year 2011 in many East German regions. Could this be because the 
population figures changed because of the census?
*******************************************************************************/

* OPEN DATA
***********

use ${data}/Data_Demo_Set-up_lmr_160303_001.dta, replace


* GENERATE YOUTH-SHARE VARIABLE
*******************************

gen youth_share = pop_20_24/pop


* SELECT LABOUR-MARKET REGIONS
******************************

keep if inlist(lmr_id, 5, 29, 80, 109, 116, 118, 123, 126, 127, 129, 131, 132)


* PLOT YOUTH SHARE
******************

graph twoway (line youth_share year), by(lmr_name, ixaxes note("")) ///
xlabel(2000(4)2013) ///
xtick(2002(4)2013) ///
xmtick(2001(2)2013) ///
xline(2010)

graph save ${graphs}/Graphs_Demo_`filedate'_`filenum'.gph, replace

graph export ${graphs}/Graphs_Demo_`filedate'_`filenum'.emf, replace


log close

exit, clear

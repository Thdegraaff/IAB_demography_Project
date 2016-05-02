clear
clear matrix
version 13.1
set more off
capture log close


/*******************************************************************************
DESCRIPTION
***********
This file defines the necessary paths and stores them in form of global macros. 
*******************************************************************************/

* 1 PATHS
*********

global data "C:\Users\con600\Dropbox\Demography_Project\SourceData\RegionData\Data"
global graphs "C:\Users\con600\Dropbox\Demography_Project\Employment\Figs"
global data_mig "C:\Users\con600\Dropbox\Demography_Project\SourceData\RegionData\Data"
global data_unemp "C:\Users\con600\Dropbox\Demography_Project\SourceData\RegionData\Data"
global data_bir "C:\Users\con600\Dropbox\Demography_Project\SourceData\RegionData\Data"
global data_ins2 "C:\Users\con600\Dropbox\Demography_Project\SourceData\RegionData\Data"
global data_pop "C:\Users\con600\Dropbox\Demography_Project\SourceData\RegionData\Data"
global anal "C:\Users\con600\Dropbox\Demography_Project\Employment\Output"


* 2 DO FILES
************

do ${graphs}/Graphs_Demo_160427_001.do

do ${graphs}/Graphs_Demo_160427_002.do

do ${graphs}/Graphs_Demo_160427_003.do

do ${anal}/Analysis_Demo_160419_001.do

do ${anal}/Analysis_Demo_160419_002.do

do ${anal}/Analysis_Demo_160419_003.do

do ${anal}/Analysis_Demo_160419_004.do

do ${anal}/Analysis_Demo_160419_005.do

do ${anal}/Analysis_Demo_160419_006.do

do ${anal}/Analysis_Demo_160419_007.do

do ${anal}/Analysis_Demo_160419_008.do

do ${anal}/Analysis_Demo_160419_009.do


exit, clear

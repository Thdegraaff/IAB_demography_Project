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


exit, clear

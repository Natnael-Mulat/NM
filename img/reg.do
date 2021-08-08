*===============================================================================
*Date: July, 2021
*Paper: The effect delayed graduation has on income
*This program relaxes discrete variabes and runs regressions
*Database Used: NLSY97 with selected and created variables
*Output: outreg2 docx regression results
*Key Variables: - gapsTaken
*				- logInc
*
*===============================================================================

#delimit;    /*semicolon signals end of each line*/     
set more 1;  /* stata does not stop at each screen of output */
drop _all ;  /* clear all the variables in memory, if any */
capture log close; /* close any open log files */ 
cd \\apporto.com\dfs\DVD\Users\namulat_dvd\Desktop\cross1;/*access file dir.*/
log using cross1.txt , text replace;/*Stata log file defination*/
use cross12.dta;/*specifies which .dta file to use for this analysis*/
ssc install outreg2;/*installs outreg2 package to current server*/

/*define time variables that will be used later on*/
local years 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2013 2015 2017;
local combinedyears1 2001 2002 2003 2004;
local combinedyears2 2005 2006 2007  2008;
local combinedyears3 2009 2010 2013 2015 2017;
local base = 2001;

/*iterating through the years using variable ("years") defined above to generate
different variables through the years*/
foreach year in `years'{;

	/*define years of education for potiential experince variable based degree*/
	generate eduyear`year' = .;
	replace eduyear`year' = 16 if (CV_HIGHEST_DEGREE_EVER_EDT_`year' == 1 |  
    CV_HIGHEST_DEGREE_EVER_EDT_`year' == 2);
	replace eduyear`year' = 18 if (CV_HIGHEST_DEGREE_EVER_EDT_`year'==3);
	replace eduyear`year' = 20 if (CV_HIGHEST_DEGREE_EVER_EDT_`year'==4);
	replace eduyear`year' = 22 if (CV_HIGHEST_DEGREE_EVER_EDT_`year'==5);
	replace eduyear`year' = 25 if (CV_HIGHEST_DEGREE_EVER_EDT_`year'==6|
    CV_HIGHEST_DEGREE_EVER_EDT_`year'==7);
	
	/*define age from KEY_BDATE_Y_1997 variable */
	generate age`year' = `year' - KEY_BDATE_Y_1997;
	/*define potiential experience variable from age, years of educ, and 
	education starting age assummed = 6*/
	generate potexp`year' = age`year' - eduyear`year' - 6;
	/*droping variable after use to lessen number of variables*/
	drop eduyear`year';
	
	/*creating job satisfaction variable from discrete value of 
	YEMP_101200_01_`year' assuming the first two response out 5 as satisfied 
	while repsonse strictly above 2 as not satisfied*/
	generate jobsats`year' = .;
	replace jobsats`year' = 1 if (YEMP_101200_01_`year'==1|
	YEMP_101200_01_`year'==2);
	replace jobsats`year' = 0 if (YEMP_101200_01_`year'>2);
	
	/*generating missing varible for those that do not have statisfaction 
	record*/
	generate jobsats`year'missing = 1 if (YEMP_101200_01_`year'==.| 
	YEMP_101200_01_`year'<0);
	replace jobsats`year'missing = 0 if (YEMP_101200_01_`year'!=. & 
	YEMP_101200_01_`year'>0);

	/*generating natural log of income */
	generate loginc`year' = ln(YINC_1700_`year');
	
	/*iterating through discrete value(0-5) of MAR_STATUS_`year'_12_XRND and 
	creating variable that will relax with binary values*/ 
	forvalues j = 0/5{;
	  generate marstat`year'_`j' = .;
	  replace marstat`year'_`j' = 1 if (MAR_STATUS_`year'_12_XRND==`j');
	  replace marstat`year'_`j' = 0 if (MAR_STATUS_`year'_12_XRND!=`j' & 
	  MAR_STATUS_`year'_12_XRND<.);
	};
	
	/*generating missing varible for those that do not have maritial status 
	record*/
	generate marstat`year'missing = 1 if (MAR_STATUS_`year'_12_XRND==.| 
	MAR_STATUS_`year'_12_XRND<0);
	replace marstat`year'missing = 0 if (MAR_STATUS_`year'_12_XRND!=. & 
	MAR_STATUS_`year'_12_XRND!=-3 & MAR_STATUS_`year'_12_XRND!=-4);
	/*testing if defination of binary variable was a success*/
	egen testdummy`year' = rowtotal(marstat`year'_0-marstat`year'missing);
	sum marstat`year'* testdummy`year';
};

/*generating gapsTaken dummy from gapsTaken created variable*/
generate gapstakendummy = .;
replace gapstakendummy = 1 if (gapsTaken>0 & gapsTaken<.);
replace gapstakendummy = 0 if (gapsTaken==0);
/*generating missing varible for those that do not have gapsTaken record*/
generate gapsmissing= 1 if (gapsTaken==.);
replace gapsmissing = 0 if (gapsTaken!=. & gapsTaken>=0);

/*iterating through CVC_HIGHEST_DEGREE_EVER_XRND discrete value and generating
binary variables for each discrete value*/
forvalues j = 0/7{;
	generate highestdegree`j' = .;
	replace highestdegree`j' = 1 if (CVC_HIGHEST_DEGREE_EVER_XRND==`j');
	replace highestdegree`j' = 0 if (CVC_HIGHEST_DEGREE_EVER_XRND!=`j' & CVC_HIGHEST_DEGREE_EVER_XRND<.);
};

/*generating missing varible for those that do not have degree record*/
generate highestdegreemissing = 1 if (CVC_HIGHEST_DEGREE_EVER_XRND==.| CVC_HIGHEST_DEGREE_EVER_XRND<0);
replace highestdegreemissing = 0 if (CVC_HIGHEST_DEGREE_EVER_XRND!=. & CVC_HIGHEST_DEGREE_EVER_XRND!=-3);
/*testing if defination of binary variable was a success*/
egen testdummyhighestdegree = rowtotal(highestdegree0 - highestdegreemissing);
sum highestdegree* testdummyhighestdegree;

/*iterating through discrete value of KEY_SEX_1997 and generating binary 
variable for each discrete value*/
forvalues j = 1/2{;
		generate sex`j' = .;
		replace sex`j' = 1 if (KEY_SEX_1997==`j');
		replace sex`j' = 0 if (KEY_SEX_1997!=`j' & KEY_SEX_1997<.);
	};
/*generating missing varible for those that do not have sex record*/
generate sexmissing = 1 if (KEY_SEX_1997==.| KEY_SEX_1997<0);
replace sexmissing = 0 if (KEY_SEX_1997!=. & KEY_SEX_1997>0);
/*testing if defination of binary variable was a success*/
egen testdummysex = rowtotal(sex1-sexmissing);
sum sex* testdummysex;


/*iterating through discrete value of KEY_RACE_ETHNICITY_1997 and generating 
binary variable for each discrete value*/
forvalues j = 1/4{;
	generate race`j' = .;
	replace race`j' = 1 if (KEY_RACE_ETHNICITY_1997==`j');
	replace race`j' = 0 if (KEY_RACE_ETHNICITY_1997!=`j' & 
	KEY_RACE_ETHNICITY_1997<.);
};

/*generating missing varible for those that do not have race&ethnicity record*/
generate racemissing = 1 if (KEY_RACE_ETHNICITY_1997==.| 
KEY_RACE_ETHNICITY_1997<0);
replace racemissing = 0 if (KEY_RACE_ETHNICITY_1997!=. & 
KEY_RACE_ETHNICITY_1997>0);
/*testing if defination of binary variable was a success*/
egen testdummyrace = rowtotal(race1-racemissing);
sum race* testdummyrace;

/*change the gapsTaken variable in terms of year from months*/
replace gapsTaken = gapsTaken/12;

/*iterating through a bacth of years for running regression since outreg2 will 
dump all output in a single docx file but we want to have separate files for 
defined years in the beginning*/
forvalues i = 1/3{;
	local years `combinedyears`i'';
	/*nested loop for each years in combined years variables*/
	foreach year in `years'{;
		reg loginc`year' gapsTaken  marstat`year'_1-marstat`year'missing 
		c.potexp`year' c.potexp`year'#c.potexp`year' race2-racemissing 
		sex2-sexmissing  highestdegree0-highestdegreemissing if 
		(CV_HIGHEST_DEGREE_EVER_EDT_`year'>3&
		CV_HIGHEST_DEGREE_EVER_EDT_`year'<.), robust;
		/*test c.potexp`year' c.potexp`year'#c.potexp`year';
		bysort gapstakendummy: sum loginc`year';*/
		/*creating docx file for table outputs*/
		outreg2 using regtabels`i'.doc, append;
	};
};



/*reg loginc2015 gapsTaken marstat2015_1-marstat2015missing jobsats2015_2-jobsats2015missing age2015 race2-racemissing sex2-sexmissing  if (CVC_HIGHEST_DEGREE_EVER_XRND>3&CVC_HIGHEST_DEGREE_EVER_XRND<.) , robust;
summarize if e(sample)==1;

test c.potexp2017 c.potexp2017#c.potexp2017;

reg loginc2013 gapstakendummy marstat2013_1-marstat2013missing c.potexp2013 c.potexp2013#c.potexp2013 race2-racemissing sex2-sexmissing  highestdegree0-highestdegreemissing, robust;

test c.potexp2013 c.potexp2013#c.potexp2013;
bysort gapstakendummy: sum loginc2013;

bysort gapstakendummy: sum loginc2017;



*/

log close;  /* close the log file  */
#delimit cr /* return the signal for end of each line to the default of Carriage Return  */
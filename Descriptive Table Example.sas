

/************************************************************************************************************
Last Updated 1/11/21 by Jennifer Scodes- 

This file is created by the Division of Mental Health Data Science in New York State Psychiatric Institute - 
The code below is an example of how to implement the descriptive table macros provided in 'Descriptive Table Macros.sas'

Data are simulated to present descriptive summaries of baseline data by treatment assignment. The results 
can be output using PROC REPORT with and without test statistics and p-values and examples of both are presented
below.  

************************************************************************************************************/
ods listing close;


/* Save Current Date*/
%let date=%sysfunc(today(),yymmdd6.);


/* Load Data */
libname home 'C:\Users\jscodes\Desktop\Table 1 Macros';


** Measures that are used in the macro need to be numeric;
data data2; 
	set home.exampledata;

	** GROUPING VARIABLE: TREATMENT ASSIGNMENT;
	* Needs to be numeric starting from 1 upto the number of groups;
	if TRT="Waitlist" then treatment = 1;
	else if TRT="CBT" then treatment = 2;

run;


/* Create Formats */

proc format;
	value YNF 0="No" 1="Yes";
	value GENDER 1="Male" 2="Female";
	value HISPANIC 0="Non-Hispanic" 1="Hispanic";
	value EDUC 1="<HS" 2="HS/GED" 3=">HS";
	value ANXDEP 1="None" 2="Moderate" 3="Extreme";
	value pvalBold low-.05='bold' .05-high=normal; *this can be used to bold significant differences at 5% level;
run;




*******************************************************************************************************************************
*********************************************************************************************************************** TABLE 1;

/*Load Macros */
%inc "C:\Users\jscodes\Desktop\Table 1 Macros\Descriptive Table Macros.sas";


/* Run appropriate macro for each variable to be added into the descriptive table */
%let group= treatment; *stratifying measure;
%let group_max= 2; *maximum number of groups;

data output; length var var_label $ 100; stop; run;

data category; var_label="^S={font_weight=bold}Demographics^S={}"; run; data output; set output category; run;
%cat(data2, GENDER, 		&group, &group_max,		GENDER., 	8.1,	"Gender");
%cont(data2, AGE,  			&group, &group_max,					8.1,	"Age");
%cat(data2, HISPANIC, 		&group, &group_max, 	HISPANIC., 	8.1,	"Hispanic");
%cat(data2, EDUC, 			&group, &group_max,		EDUC., 		8.1,	"Education");

data category; var_label="^S={font_weight=bold}Clinical^S={}"; run; data output; set output category; run;
%skew(data2, HAMD,  		&group, &group_max,					8.1,	"Hamilton Depression Scale (Median, IQR)");
%cat(data2,  ANXDEP, 		&group, &group_max, 	ANXDEP., 	8.1,	"Anxiety-depression");
%cat(data2, PRIORTRT, 		&group, &group_max, 	YNF., 		8.1,	"Prior Treatment");


data ALL; set output; var_name=var; run;
data TABLE1_DEMOS; set ALL; variable_fixed= strip(var_label)||"     "||var_cat; n=_n_; run;


/*pull n's for all subjects*/
ods trace on; proc freq data=data2; table treatment/; ods output onewayFreqs=onewayFreqs; run;ods trace off;
data _null_; set onewayFreqs; if treatment=1 ; keep frequency; call symput('n1', compress(round(frequency,.1))); run; 
data _null_; set onewayFreqs; if treatment=2 ; keep frequency; call symput('n2', compress(round(frequency,.1))); run; 
data _null_; set onewayFreqs; if treatment=2 ; keep frequency; call symput('ntot', compress(round(CumFrequency,.1))); run; 


options missing = ' ' nodate;  
options orientation=portrait;
ods escapechar="^";
ods rtf bodytitle style=statistical 
file="C:\Users\jscodes\Desktop\Table 1 Macros\Descriptive Table Example Output- &date..doc"; 

/*Example without pvalues between groups*/
title1 j=l "Table 1. Descriptive summaries of baseline characteristics by treatment assignment (N=&ntot)"; 

proc report data=TABLE1_DEMOS nowd headline headskip box STYLE(column header)=[background=white just=center vjust=bottom ];
column (variable_fixed) ( "Grouped by Treatment Assignment"  
								( "Waitlist/(N=&n1)" group_n_1 group_1) 
								( "CBT/(N=&n2)" group_n_2 group_2) ); /*add more columns if you have more than 2 groups*/
define variable_fixed/STYLE=[just=left asis=yes];
label 	group_1 ="% or/M (SD)"  group_n_1 ="N" group_2 ="% or/M (SD)"  group_n_2 ="N"
		variable_fixed="Measure";
run;


/*Example with test statistics and p-values between groups*/
title1 j=l "Table 2. Descriptive summaries of baseline characteristics by treatment assignment with test of differences between groups (N=&ntot)"; 

proc report data=TABLE1_DEMOS nowd headline headskip box STYLE(column header)=[background=white just=center vjust=bottom ];
column (variable_fixed) ( "Grouped by Treatment Assignment"  
								( "Waitlist/(N=&n1)" group_n_1 group_1) 
								( "CBT/(N=&n2)" group_n_2 group_2) 
								( "Difference/between groups" test prob) );
define variable_fixed/STYLE=[just=left asis=yes];
define prob/ STYLE=[font_weight=pvalBold.]; *this will bold the pvalues with significant differences between groups;
label 	group_1 ="% or/M (SD)"  group_n_1 ="N" group_2 ="% or/M (SD)"  group_n_2 ="N"
		variable_fixed="Measure" test="Test Statistic" prob="p-value";
format prob pvalue6.3;
run;


ods rtf close;
options missing = '.'; 
















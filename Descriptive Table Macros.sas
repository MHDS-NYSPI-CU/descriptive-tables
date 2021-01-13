


/*********************************************************************************************************************************************
Last Updated 1/11/21 by Jennifer Scodes- 

** The code for the following macros were created within the Division of Mental Health Data Science in New York State Psychiatric Institute
The macros provided below can be used to produce a Demographics Table 1. 

1) The 'categorical' macro calculates n's and column % for up to X number of groups and runs a chi-square across multiple groups. An alternate
version of the macro is also provided that computes row % instead of column %
2) The 'continuous' macro calculates mean and standard deviation for up to X groups and runs a one-way ANOVA (ie t-test when only two groups)
3) The 'skew' macro calculates the median and IQR for up to X groups and runs a wilcoxon-rank test if 2 groups or a kruskal-wallis test if more than 2 groups.
Note the skew macro only works for SAS 9.4 M6 or later


*********************************************************************************************************************************************/




*********************************************************************************************************************************************
******************************************************************************************* Categorical Variable Macro (with column percents);
%macro cat(data, var, group, group_max, var_format, num_format, var_label);
*ods select none;
proc freq data=&data;  table &var*&group/ chisq; format &var &var_format; ods output crosstabfreqs=X chisq=cs;run;
data X; set X; format percent colpercent &num_format; run;
data cs; set cs; format value pvalue6.2; run;*updated 7/16/19 added to format test statistic value;

%do i=1 %to &group_max; ** updated 7/16/19 to incorporate into one macro;

data n&i; set X; where &group =&i and &var ne .; keep &var frequency ; run;
data perc&i; set X; where &group =&i and &var ne .; keep &var colpercent ; run;
data group_&i ; merge n&i perc&i; 
by &var; 
group_&i = strip(strip(vvalue(colpercent))||'%'); 
group_n_&i=frequency;
keep &var group_&i group_n_&i; run;

%end;

data n; set X; where &group =. and &var ne .; keep &var frequency ; run;
data perc; set X; where &group =. and &var ne .; keep &var percent ; run;
data group_tot ; merge n perc; by &var; group_tot = strip(strip(vvalue(percent))||'%'); 
group_tot_n = frequency;
keep &var group_tot group_tot_n; run;
data cs2; set cs; where statistic = "Chi-Square"; var="&var"; var_label=&var_label; 
	ts=value; df=df; test="x2"||'('||compress(df)||')'||'='||compress(vvalue(value)); 
	type=1; 
keep var var_label ts df test prob type; run;

data a; merge group_tot group_1-group_&group_max ;by &var; var_cat = vvalue(&var); drop &var; run;
data table; set cs2 a ; run;
data output; set output table; run;
*ods select all;
%mend;





*********************************************************************************************************************************************
********************************************************************************************* Categorical Variable Macro (with row percents);
%macro catR(data, var, group, group_max, var_format, num_format, var_label);
ods select none;
proc freq data=&data;  table &var*&group/ chisq; format &var &var_format; ods output crosstabfreqs=X chisq=cs;run;
data X; set X; format percent rowpercent &num_format; run;
data cs; set cs; format value pvalue6.2; run;

%do i=1 %to &group_max;

data n&i; set X; where &group =&i and &var ne .; keep &var frequency ; run;
data perc&i; set X; where &group =&i and &var ne .; keep &var rowpercent ; run;
data group_&i ; merge n&i perc&i; 
by &var; 
group_&i = strip(strip(vvalue(rowpercent))||'%'); 
group_n_&i=frequency;
keep &var group_&i group_n_&i; run;

%end;

data n; set X; where &group =. and &var ne .; keep &var frequency ; run;
data perc; set X; where &group =. and &var ne .; keep &var percent ; run;
data group_tot ; merge n perc; by &var; group_tot = strip(strip(vvalue(percent))||'%'); 
group_tot_n = frequency;
keep &var group_tot group_tot_n; run;
data cs2; set cs; where statistic = "Chi-Square"; var="&var"; var_label=&var_label; 
	ts=value; df=df; 
	test='x2('||compress(df)||')='||compress(vvalue(value)); 
	type=1; 
keep var var_label ts df test prob type; run;

data a; merge group_tot group_1-group_&group_max ;by &var; var_cat = vvalue(&var); drop &var; run;
data table; set cs2 a ; run;
data output; set output table; run;
ods select all;
%mend;




*********************************************************************************************************************************************
*********************************************************************************************** Continuous Variable Macro (with mean and SD);
%macro cont(data, var, group, group_max, num_format, var_label);
ods select none;
*overall mean;
proc means data=&data n mean std maxdec=1;  var &var ; ods output summary=tot; run;
data tot; set tot; format &var._mean  &var._stddev &num_format; run;
data total; set tot; var = "&var"; var_label=&var_label; 
	group_tot= strip(vvalue(&var._mean))||' ('||strip(vvalue(&var._stddev))||')';
	group_tot_n=&var._n; 
	keep var var_label group_tot group_tot_n; run;

*mean by group;
proc means data=&data n mean std t maxdec=1;  class &group; var &var ; ods output summary=summary; run;
data summary; set summary; format &var._mean  &var._stddev &num_format; run;

%do i=1 %to &group_max;
	data r&i; set summary; if &group=&i; 
	group_&i = strip(vvalue(&var._mean))||' ('||strip(vvalue(&var._stddev))||')'; 
	group_n_&i=&var._n; var_cat=''; keep var_cat group_&i group_n_&i; run;
%end;

*ods select none;
proc glimmix data=&data; 
class &group;
model &var=&group;
lsmeans &group;
ods output tests3=t3;
run;
data t3; set t3; tvalue=sqrt(fvalue); format tvalue fvalue pvalue6.2; run;

%if &group_max = 2 %then %do; *if groups=2 (ie, numerator df of f-test is 1) then the t-test value is equivalent to the sqrt of the f-value;
data t3_; set t3; 
	ts=tvalue; df=dendf; 
	test='t('||compress(df)||')='||compress(vvalue(tvalue)); 
	prob=probf; 
	type=2;
keep ts df test prob type; run;
%end;

%else %do;
data t3_; set t3; 
	ts=fvalue; df1=numdf; df=dendf; 
	test='F('||compress(df1)||', '||compress(df)||') ='||compress(vvalue(fvalue)); 
	prob=probf; 
	type=2;
keep ts df1 df test prob type; run;
%end;

data table; merge total r1-r&group_max t3_; run;
data output; set output table; run;
ods select all;
%mend;



*********************************************************************************************************************************************
************************************************************************************************* Skewed Variable macro (with median and IQR)
NOTE: only works for SAS M6 in current form;

%macro skew(data, var, group, group_max, num_format, var_label);

*overall median;
proc means data=&data n mean std maxdec=1 median q1 q3;  var &var ; ods output summary=tot; run;
data tot; set tot; format &var._median  &var._q1 &var._q3 &num_format; run;
data total; set tot; var = "&var"; var_label=&var_label; 
	group_tot= strip(vvalue(&var._median))||' ('||strip(vvalue(&var._q1))||'-'||strip(vvalue(&var._q3))||')'; 
	group_tot_n=&var._n; 
	keep var var_label group_tot group_tot_n; 
run;

*median by group;
proc means data=&data n mean std t maxdec=1 median q1 q3;  class &group; var &var ; ods output summary=summary; run;
data summary; set summary; format &var._median  &var._q1 &var._q3 &num_format; run;

%do i=1 %to &group_max;
	data r&i; set summary; if &group=&i; 
	group_&i = strip(vvalue(&var._median))||' ('||strip(vvalue(&var._q1))||'-'||strip(vvalue(&var._q3))||')'; 
	group_n_&i=&var._n; var_cat=''; keep var_cat group_&i group_n_&i; run;
%end;

/*non-parametric test*/
proc npar1way wilcoxon data=&data;
	class &group;
	var &var;
	ods output WilcoxonTest=wt KruskalWallisTest=kw; 
run;

/*If number of groups =2, then pull wilcoxon-rank sum test*/
%if &group_max = 2 %then %do;
data wt; set wt; format Z pvalue6.2; run;
data nonpara; set wt; * Updated 6/4/19 for M6;
	prob =Prob2 ; 
	ts=Z; 
	test='Z='||compress(vvalue(Z));
	keep ts test prob; 
run;  
%end;

/*If number of groups >2, then pull kruskal-wallis test*/
%else %do;
data kw; set kw; format ChiSquare pvalue6.2; run;
data nonpara; set kw; * Updated 6/4/19 for M6; 
	prob = prob; 
	ts=ChiSquare; df=df; 
	test='x2('||compress(df)||')='||compress(vvalue(ChiSquare)); 
	type=3; 
	keep ts df test prob type; run;
run; 
%end;

/*combine all together*/
data table; merge total r1-r&group_max nonpara; run;
data output; set output table; run;
ods select all;
%mend;

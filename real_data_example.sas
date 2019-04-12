*let path=C:\Users\gz223\Box Sync\CORE_write\reliability_paper\Adams_Snijders_reliability;
%let path=X:\reliability_paper\Adams_Snijders_reliability;
%include "&path\betabin-v22.sas";

***********************
* cancer 
***********************; 
proc import datafile=
"&path\Cancer_Treatment_Measures___PPS-Exempt_Cancer_Hospital.csv"
out=dea dbms=csv replace;
RUN;
proc print data=dea(obs=1); run;

proc contents data=dea(obs=10); run;
data dea2; set dea;
mi = input(NUMERATOR,7.);
ni =input(DENOMINATOR,7.); 
if NUMERATOR="1,039" then mi = 1039;
if DENOMINATOR="1,046" then ni = 1046;
if ~missing(mi);
score_pct = mi*100/ni;
run;

proc sort data=dea2; by MEASURE_DESCRIPTION  Provider_ID; run;
proc print data=dea2; 
var MEASURE_DESCRIPTION Provider_ID NUMERATOR m DENOMINATOR n score_pct; run;
 

%let measure=Adjuvant Chemotherapy Colon Cancer;
*%let measure=Adjuvant Hormonal Therapy;
*%let measure=Combination Chemotherapy Breast Cancer;

%let measureAbbve=ACCC;
*%let measureAbbve=AHT;
*%let measureAbbve=CCBC;
 
%macro realdata(measure=,measureAbbve=);

* original and revised Adams approaches; 
title "&measure";
data temp;
set dea2;
where MEASURE_DESCRIPTION="&measure";
run;
%betabin(data=temp,title=,ntrials=ni,
nsucc=mi,nullprob=0.5,binest=binest, est=est,cov=cov);

data _NULL_; set est; 
if Label="alpha" then call symput("alpha_hat",Estimate);
if Label="beta" then call symput("beta_hat",Estimate);
run;
%put &alpha_hat;
%put &beta_hat;

data _NULL_; 
sigma2_between_provider = &alpha_hat*&beta_hat/( (&alpha_hat + &beta_hat + 1)*(&alpha_hat + &beta_hat)**2 );
call symput("sigma2_between_provider",sigma2_between_provider);
run;
%put &sigma2_between_provider;


data temp;
set temp;
pihat = mi/ni;
pihat_1 = (1+mi)/(2+ni);
pihat_2 = (0.5+mi)/(1+ni);
pihat_3 = (mi+sqrt(ni)*0.5)/(sqrt(ni)+ni);
var_within_provider = pihat*(1-pihat)/ni;
var_within_provider_1 = pihat_1*(1-pihat_1)/ni;
var_within_provider_2 = pihat_2*(1-pihat_2)/ni;
var_within_provider_3 = pihat_3*(1-pihat_3)/ni;
var_between_provider = &sigma2_between_provider;
alpha_hat_beta_binom = &alpha_hat;
beta_hat_beta_binom = &beta_hat;
Adams_orig = var_between_provider/(var_between_provider + var_within_provider);
Adams_1 = var_between_provider/(var_between_provider + var_within_provider_1);
Adams_2 = var_between_provider/(var_between_provider + var_within_provider_2);
Adams_3 = var_between_provider/(var_between_provider + var_within_provider_3);
Fake_provid = _N_;
keep MEASURE_DESCRIPTION Fake_provid Provider_ID ni mi pihat pihat_1 pihat_2 pihat_3 var_between_provider 
alpha_hat_beta_binom beta_hat_beta_binom
var_within_provider var_within_provider_1 var_within_provider_2 var_within_provider_3
Adams_orig Adams_1 Adams_2 Adams_3;
run;
proc print data=temp(obs=100); run;

proc sgscatter data=temp;
matrix Adams_orig Adams_1 Adams_2 Adams_3; run;

* HGLM; 
proc glimmix data=temp method=quad;
class Fake_provid;
ods output covParms=cov;
model  mi/ni=/distribution=binomial link=logit solution;
random intercept/subject=Fake_provid solution;
run;


/*
* construct patient level data;
proc sql noprint; select count(n) into: num_provider from temp;
%put &num_provider;

%macro one_subject_per_time;
%do sub_number=1 %to &num_provider; 
DATA dset&sub_number;
set temp;
where Fake_provid=&sub_number; 
       DO nij = 1 TO n;
       if nij<=m then yij=1; else yij=0;
       OUTPUT;
       END;
RUN;
%end;
%mend;
%one_subject_per_time;

data Append_out; set dset1; run; 
%macro merge_patien_level;
%do sub_number=2 %to &num_provider; * must start from 2;
proc append base=Append_out  data=dset&sub_number; run;
%end;
%mend;
%merge_patien_level;

proc contents data=Append_out; run;

%macro delete_temp;
%do sub_number=1 %to &num_provider;
proc delete data=dset&sub_number;
%end;
%mend;
%delete_temp;



* fit glimmix; 
proc glimmix data=Append_out method=quad;
class Fake_provid;
ods output covParms=cov;
model  yij(event='1')=/distribution=binary link=logit solution;
random intercept/subject=Fake_provid solution;
run;
*/

proc sql noprint; select Estimate into: tau2 from cov where CovParm="Intercept";
%put &tau2;

 
data d3; 
set temp;
HGLM_var_within = 3.1415926535897**2/(3*ni);
tau2 = &tau2;
HGLM_R = tau2/(tau2 + HGLM_var_within);
run;

proc export data=d3 outfile="X:\reliability_paper\Adams_Snijders_reliability\real_data_&measureAbbve..csv"
replace dbms=csv; run;
proc print data=d3; run;
%mend realdata;


%realdata(measure=Adjuvant Chemotherapy Colon Cancer,measureAbbve=ACCC);
%realdata(measure=Adjuvant Hormonal Therapy,measureAbbve=AHT);
%realdata(measure=Combination Chemotherapy Breast Cancer,measureAbbve=CCBC);


* Test retest done in R; 





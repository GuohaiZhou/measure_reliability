****************************
* estiamte reliability;
****************************;
%let path=X:\reliability_paper\Adams_Snijders_reliability;
%include "&path\betabin-v22.sas";
%let simu_iter=1;
%let nicut=11;
%macro est_reliability(nicut=,start_simu=,end_simu=);
%DO simu_iter=&start_simu %TO &end_simu;
* original and revised Adam; 
proc import datafile="&path\under_real_AHT\simu_&simu_iter..csv" out=temp dbms=csv replace; run;
data dat&nicut;
set temp;
if _N_<=&nicut;
run;
proc print data=dat&nicut;run;

%betabin(data=dat&nicut,title=Simu,ntrials=ni,
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

* ni*10 version;
%betabin(data=dat&nicut,title=Simu,ntrials=ni10,
nsucc=mi10,nullprob=0.5,binest=binest, est=est,cov=cov);
data _NULL_; set est; 
if Label="alpha" then call symput("alpha_hat10",Estimate);
if Label="beta" then call symput("beta_hat10",Estimate);
run;
%put &alpha_hat10;
%put &beta_hat10;
data _NULL_; 
sigma2_between_provider10 = &alpha_hat10*&beta_hat10/( (&alpha_hat10 + &beta_hat10 + 1)*(&alpha_hat10 + &beta_hat10)**2 );
call symput("sigma2_between_provider10",sigma2_between_provider10);
run;
%put &sigma2_between_provider10;

* ni*20 version;
%betabin(data=dat&nicut,title=Simu,ntrials=ni20,
nsucc=mi20,nullprob=0.5,binest=binest, est=est,cov=cov);
data _NULL_; set est; 
if Label="alpha" then call symput("alpha_hat20",Estimate);
if Label="beta" then call symput("beta_hat20",Estimate);
run;
%put &alpha_hat20;
%put &beta_hat20;
data _NULL_; 
sigma2_between_provider20 = &alpha_hat20*&beta_hat20/( (&alpha_hat20 + &beta_hat20 + 1)*(&alpha_hat20 + &beta_hat20)**2 );
call symput("sigma2_between_provider20",sigma2_between_provider20);
run;
%put &sigma2_between_provider20;


data dat&nicut;
set dat&nicut;
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
Adam_orig = var_between_provider/(var_between_provider + var_within_provider);
Adam_1 = var_between_provider/(var_between_provider + var_within_provider_1);
Adam_2 = var_between_provider/(var_between_provider + var_within_provider_2);
Adam_3 = var_between_provider/(var_between_provider + var_within_provider_3);

pihat_10 = mi10/ni10;
pihat_1_10 = (1+mi10)/(2+ni10);
pihat_2_10 = (0.5+mi10)/(1+ni10);
pihat_3_10 = (mi10+sqrt(ni10)*0.5)/(sqrt(ni10)+ni10);
var_within_provider_10 = pihat_10*(1-pihat_10)/ni10;
var_within_provider_1_10 = pihat_1_10*(1-pihat_1_10)/ni10;
var_within_provider_2_10 = pihat_2_10*(1-pihat_2_10)/ni10;
var_within_provider_3_10 = pihat_3_10*(1-pihat_3_10)/ni10;
var_between_provider_10 = &sigma2_between_provider10;
alpha_hat_beta_binom10 = &alpha_hat10;
beta_hat_beta_binom10 = &beta_hat10;
Adam_orig_10 = var_between_provider_10/(var_between_provider_10 + var_within_provider_10);
Adam_1_10 = var_between_provider_10/(var_between_provider_10 + var_within_provider_1_10);
Adam_2_10 = var_between_provider_10/(var_between_provider_10 + var_within_provider_2_10);
Adam_3_10 = var_between_provider_10/(var_between_provider_10 + var_within_provider_3_10);

pihat_20 = mi20/ni20;
pihat_1_20 = (1+mi20)/(2+ni20);
pihat_2_20 = (0.5+mi20)/(1+ni20);
pihat_3_20 = (mi20+sqrt(ni20)*0.5)/(sqrt(ni20)+ni20);
var_within_provider_20 = pihat_20*(1-pihat_20)/ni20;
var_within_provider_1_20 = pihat_1_20*(1-pihat_1_20)/ni20;
var_within_provider_2_20 = pihat_2_20*(1-pihat_2_20)/ni20;
var_within_provider_3_20 = pihat_3_20*(1-pihat_3_20)/ni20;
var_between_provider_20 = &sigma2_between_provider20;
alpha_hat_beta_binom20 = &alpha_hat20;
beta_hat_beta_binom20 = &beta_hat20;
Adam_orig_20 = var_between_provider_20/(var_between_provider_20 + var_within_provider_20);
Adam_1_20 = var_between_provider_20/(var_between_provider_20 + var_within_provider_1_20);
Adam_2_20 = var_between_provider_20/(var_between_provider_20 + var_within_provider_2_20);
Adam_3_20 = var_between_provider_20/(var_between_provider_20 + var_within_provider_3_20);

run;

 
* HGLM ;
data dat&nicut;
set dat&nicut;
if &nicut=11 then fake_id = fake_provider_id_11;
if &nicut=110 then fake_id = fake_provider_id_110;
if &nicut=1100 then fake_id = fake_provider_id_1100;
run;

proc glimmix data=dat&nicut method=quad;
class fake_id;
ods output covParms=cov;
model  mi/ni=/distribution=binomial link=logit solution;
random intercept/subject=fake_id solution;
run;
proc sql noprint; select Estimate into: tau2 from cov where CovParm="Intercept";
%put &tau2;

proc glimmix data=dat&nicut method=quad;
class fake_id;
ods output covParms=cov10;
model  mi10/ni10=/distribution=binomial link=logit solution;
random intercept/subject=fake_id solution;
run;
proc sql noprint; select Estimate into: tau2_10 from cov10 where CovParm="Intercept";
%put &tau2_10;

proc glimmix data=dat&nicut method=quad;
class fake_id;
ods output covParms=cov20;
model  mi20/ni20=/distribution=binomial link=logit solution;
random intercept/subject=fake_id solution;
run;
proc sql noprint; select Estimate into: tau2_20 from cov20 where CovParm="Intercept";
%put &tau2_20;

data dat&nicut;
set dat&nicut;
HGLM_var_within = 3.1415926535897**2/(3*ni);
HGLM_var_within_10 = 3.1415926535897**2/(3*ni10);
HGLM_var_within_20 = 3.1415926535897**2/(3*ni20);
tau2 = &tau2;
tau2_10=&tau2_10;
tau2_20=&tau2_20;
HGLM_R = tau2/(tau2 + HGLM_var_within);
HGLM_R_10 = tau2_10/(tau2_10 + HGLM_var_within_10);
HGLM_R_20 = tau2_20/(tau2_20 + HGLM_var_within_20);
run;
proc export outfile="&path\under_real_AHT\simu_&simu_iter._out_n_&nicut..csv" data=dat&nicut 
dbms=csv replace; run;
  DM 'LOG; CLEAR';
DM 'OUTPUT; CLEAR';
/* */
%END;
%MEND est_reliability;

* %est_reliability(nicut=11,start_simu=931,end_simu=1000);

 *%est_reliability(nicut=110,start_simu=901,end_simu=1000);

%est_reliability(nicut=1100,start_simu=921,end_simu=1000);

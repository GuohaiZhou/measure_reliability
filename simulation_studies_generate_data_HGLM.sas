************************
generate true measure score from HGLM; 
************************
* known provider level and measure level reliability;
 *let path=C:\Users\gz223\Box Sync\CORE_write\reliability_paper\Adams_Snijders_reliability;
%let path=X:\reliability_paper\Adams_Snijders_reliability;
%include "&path\betabin-v22.sas";

%let simu_iter=1;
%let tau2_true=0.4318;
%let mu_true=3.5519;

%macro HGLM_data_generation(tau2_true=,mu_true=,num_simu=);
%DO simu_iter=1 %TO &num_simu;
* generate 1000 random beta numbers as true score;
data true_scores_1000;
simu_iteration = &simu_iter;
do i=1 to 1000;
true_bi=RAND('NORMAL',0,&tau2_true);
true_score = 1/(1+exp(-&mu_true-true_bi));
fake_provider_id_10=mod(i,10);
if fake_provider_id_10=0 then fake_provider_id_10=10;

if fake_provider_id_10=1 then ni=12;
if fake_provider_id_10=2 then ni=14;
if fake_provider_id_10=3 then ni=16;
if fake_provider_id_10=4 then ni=29;
if fake_provider_id_10=5 then ni=103;
if fake_provider_id_10=6 then ni=13;
if fake_provider_id_10=7 then ni=26;
if fake_provider_id_10=8 then ni=28;
if fake_provider_id_10=9 then ni=55;
if fake_provider_id_10=10 then ni=16;

ni10=ni*10;
ni20=ni*20;

mi=RAND('BINOMIAL',true_score,ni);
mi10=RAND('BINOMIAL',true_score,ni10);
mi20=RAND('BINOMIAL',true_score,ni20);

fake_provider_id_100=mod(i,100);
if fake_provider_id_100=0 then fake_provider_id_100=100;

fake_provider_id_1000=i; 

true_var_within = 3.1415926**2/(3*ni); 
true_var_within10 = 3.1415926**2/(3*ni10); 
true_var_within20 = 3.1415926**2/(3*ni20); 
true_var_between = &tau2_true;
true_reliability = true_var_between/(true_var_between + true_var_within);
true_reliability10 = true_var_between/(true_var_between + true_var_within10);
true_reliability20 = true_var_between/(true_var_between + true_var_within20);
OUTPUT;
END;
drop i;
run;

proc export data=true_scores_1000 outfile="&path\under_HGLM\simu_&simu_iter..csv" dbms=csv replace; run;

DM 'LOG; CLEAR';
DM 'OUTPUT; CLEAR';

%END;
%MEND HGLM_data_generation;

%HGLM_data_generation(tau2_true=0.4318,mu_true=3.5519,num_simu=1000);
 /* */





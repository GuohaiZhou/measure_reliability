************************
generate true measure score as observed rates from the AHT measure;
************************
* known provider level and measure level reliability;
 *let path=C:\Users\gz223\Box Sync\CORE_write\reliability_paper\Adams_Snijders_reliability;
%let path=X:\reliability_paper\Adams_Snijders_reliability;
%include "&path\betabin-v22.sas";

%let simu_iter=1;
 
%macro AHT_real_data_generation(num_simu=);
%DO simu_iter=1 %TO &num_simu;
* generate 1000 random beta numbers as true score;
data true_scores_1100;
simu_iteration = &simu_iter;
do i=1 to 1100;

fake_provider_id_11=mod(i,11);
if fake_provider_id_11=0 then fake_provider_id_11=11;

if fake_provider_id_11=1 then ni=228;
if fake_provider_id_11=2 then ni=46;
if fake_provider_id_11=3 then ni=195;
if fake_provider_id_11=4 then ni=452;
if fake_provider_id_11=5 then ni=677;
if fake_provider_id_11=6 then ni=1046;
if fake_provider_id_11=7 then ni=150;
if fake_provider_id_11=8 then ni=316;
if fake_provider_id_11=9 then ni=207;
if fake_provider_id_11=10 then ni=627;
if fake_provider_id_11=11 then ni=271;

if fake_provider_id_11=1 then true_score=0.961;
if fake_provider_id_11=2 then true_score=0.935;
if fake_provider_id_11=3 then true_score=0.995;
if fake_provider_id_11=4 then true_score=0.947;
if fake_provider_id_11=5 then true_score=0.953;
if fake_provider_id_11=6 then true_score=0.993;
if fake_provider_id_11=7 then true_score=0.947;
if fake_provider_id_11=8 then true_score=0.997;
if fake_provider_id_11=9 then true_score=0.932;
if fake_provider_id_11=10 then true_score=0.946;
if fake_provider_id_11=11 then true_score=0.963;

ni10=ni*10;
ni20=ni*20;

mi=RAND('BINOMIAL',true_score,ni);
mi10=RAND('BINOMIAL',true_score,ni10);
mi20=RAND('BINOMIAL',true_score,ni20);

fake_provider_id_110=mod(i,110);
if fake_provider_id_110=0 then fake_provider_id_110=110;

fake_provider_id_1100=i; 

true_var_within = true_score*(1-true_score)/ni;
true_var_within10 = true_score*(1-true_score)/ni10;
true_var_within20 = true_score*(1-true_score)/ni20;

true_var_between_11 = 0.0005677636; *obtained from R code: var(rep(c(.961,.935,.995,.947,.953,
.993,.947,.997,.932,.946,.963),1));
true_var_between_110 = 0.0005208841; *obtained from R code: var(rep(c(.961,.935,.995,.947,.953,
.993,.947,.997,.932,.946,.963),10));
true_var_between_1100 = 0.0005166184; *obtained from R code: var(rep(c(.961,.935,.995,.947,.953,
.993,.947,.997,.932,.946,.963),100));


true_reliability_11 = true_var_between_11/(true_var_between_11 + true_var_within);
true_reliability_11_10 = true_var_between_11/(true_var_between_11 + true_var_within10);
true_reliability_11_20 = true_var_between_11/(true_var_between_11 + true_var_within20);

true_reliability_110 = true_var_between_110/(true_var_between_110 + true_var_within);
true_reliability_110_10 = true_var_between_110/(true_var_between_110 + true_var_within10);
true_reliability_110_20 = true_var_between_110/(true_var_between_110 + true_var_within20);

true_reliability_1100 = true_var_between_1100/(true_var_between_1100 + true_var_within);
true_reliability_1100_10 = true_var_between_1100/(true_var_between_1100 + true_var_within10);
true_reliability_1100_20 = true_var_between_1100/(true_var_between_1100 + true_var_within20);
OUTPUT;
END;
drop i;
run;

proc export data=true_scores_1100 outfile="&path\under_real_AHT\simu_&simu_iter..csv" dbms=csv replace; run;

DM 'LOG; CLEAR';
DM 'OUTPUT; CLEAR';

%END;
%MEND AHT_real_data_generation;

%AHT_real_data_generation(num_simu=1000);
 /* */





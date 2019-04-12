
########
# organize results (true measure score generated from beta distribution)
########
rm(list=ls())
 #setwd("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model")
#setwd("X:/reliability_paper/Adams_Snijders_reliability/under_HGLM")
 setwd("X:/reliability_paper/Adams_Snijders_reliability/under_real_AHT")
#setwd("c:/Users/Guohai/Documents/MEGAsync/Reliability_article/")
library(data.table)

n10_summary= data.table(n_provders_with_rate_1=rep(as.numeric(NA),1000),pct_provders_with_rate_1=rep(as.numeric(NA),1000),true_measure_R=rep(as.numeric(NA),1000),avg_Adams_orig=rep(as.numeric(NA),1000),avg_Adams_1=rep(as.numeric(NA),1000),avg_Adams_2=rep(as.numeric(NA),1000), avg_Adams_3=rep(as.numeric(NA),1000),avg_HGLM_R=rep(as.numeric(NA),1000),n_provders_with_rate_1_10=rep(as.numeric(NA),1000),pct_provders_with_rate_1_10=rep(as.numeric(NA),1000),true_measure_R_10=rep(as.numeric(NA),1000),avg_Adams_orig_10=rep(as.numeric(NA),1000),avg_Adams_1_10=rep(as.numeric(NA),1000),avg_Adams_2_10=rep(as.numeric(NA),1000),avg_Adams_3_10=rep(as.numeric(NA),1000),avg_HGLM_R_10=rep(as.numeric(NA),1000),n_provders_with_rate_1_20=rep(as.numeric(NA),1000),pct_provders_with_rate_1_20=rep(as.numeric(NA),1000),true_measure_R_20=rep(as.numeric(NA),1000),avg_Adams_orig_20=rep(as.numeric(NA),1000),avg_Adams_1_20=rep(as.numeric(NA),1000),avg_Adams_2_20=rep(as.numeric(NA),1000),avg_Adams_3_20=rep(as.numeric(NA),1000),avg_HGLM_R_20=rep(as.numeric(NA),1000))


n100_summary= data.table(n_provders_with_rate_1=rep(as.numeric(NA),1000),pct_provders_with_rate_1=rep(as.numeric(NA),1000),true_measure_R=rep(as.numeric(NA),1000),avg_Adams_orig=rep(as.numeric(NA),1000),avg_Adams_1=rep(as.numeric(NA),1000),avg_Adams_2=rep(as.numeric(NA),1000), avg_Adams_3=rep(as.numeric(NA),1000),avg_HGLM_R=rep(as.numeric(NA),1000),n_provders_with_rate_1_10=rep(as.numeric(NA),1000),pct_provders_with_rate_1_10=rep(as.numeric(NA),1000),true_measure_R_10=rep(as.numeric(NA),1000),avg_Adams_orig_10=rep(as.numeric(NA),1000),avg_Adams_1_10=rep(as.numeric(NA),1000),avg_Adams_2_10=rep(as.numeric(NA),1000),avg_Adams_3_10=rep(as.numeric(NA),1000),avg_HGLM_R_10=rep(as.numeric(NA),1000),n_provders_with_rate_1_20=rep(as.numeric(NA),1000),pct_provders_with_rate_1_20=rep(as.numeric(NA),1000),true_measure_R_20=rep(as.numeric(NA),1000),avg_Adams_orig_20=rep(as.numeric(NA),1000),avg_Adams_1_20=rep(as.numeric(NA),1000),avg_Adams_2_20=rep(as.numeric(NA),1000),avg_Adams_3_20=rep(as.numeric(NA),1000),avg_HGLM_R_20=rep(as.numeric(NA),1000))

n1000_summary= data.table(n_provders_with_rate_1=rep(as.numeric(NA),1000),pct_provders_with_rate_1=rep(as.numeric(NA),1000),true_measure_R=rep(as.numeric(NA),1000),avg_Adams_orig=rep(as.numeric(NA),1000),avg_Adams_1=rep(as.numeric(NA),1000),avg_Adams_2=rep(as.numeric(NA),1000), avg_Adams_3=rep(as.numeric(NA),1000),avg_HGLM_R=rep(as.numeric(NA),1000),n_provders_with_rate_1_10=rep(as.numeric(NA),1000),pct_provders_with_rate_1_10=rep(as.numeric(NA),1000),true_measure_R_10=rep(as.numeric(NA),1000),avg_Adams_orig_10=rep(as.numeric(NA),1000),avg_Adams_1_10=rep(as.numeric(NA),1000),avg_Adams_2_10=rep(as.numeric(NA),1000),avg_Adams_3_10=rep(as.numeric(NA),1000),avg_HGLM_R_10=rep(as.numeric(NA),1000),n_provders_with_rate_1_20=rep(as.numeric(NA),1000),pct_provders_with_rate_1_20=rep(as.numeric(NA),1000),true_measure_R_20=rep(as.numeric(NA),1000),avg_Adams_orig_20=rep(as.numeric(NA),1000),avg_Adams_1_20=rep(as.numeric(NA),1000),avg_Adams_2_20=rep(as.numeric(NA),1000),avg_Adams_3_20=rep(as.numeric(NA),1000),avg_HGLM_R_20=rep(as.numeric(NA),1000))

# k=1;
for(k in 1:1000)
{
  if(k%%20==0) print(k)
eval(parse(text=paste0("dt=fread('simu_",k,"_out_n_11.csv')")))

 n10_summary[k,n_provders_with_rate_1:=sum(dt[,ni]==dt[,mi])]
 n10_summary[k,pct_provders_with_rate_1:=(n_provders_with_rate_1*100/10)]
 n10_summary[k,true_measure_R:=median(dt[,true_reliability_11])]
 n10_summary[k,avg_Adams_orig:=median(dt[,Adam_orig])]
 n10_summary[k,avg_Adams_1:=median(dt[,Adam_1])]
 n10_summary[k,avg_Adams_2:=median(dt[,Adam_2])]
 n10_summary[k,avg_Adams_3:=median(dt[,Adam_3])]
 n10_summary[k,avg_HGLM_R:=median(dt[,HGLM_R],na.rm = TRUE)] 
 
 n10_summary[k,n_provders_with_rate_1_10:=sum(dt[,ni10]==dt[,mi10])]
 n10_summary[k,pct_provders_with_rate_1_10:=(n_provders_with_rate_1_10*100/10)]
 n10_summary[k,true_measure_R_10:=median(dt[,true_reliability_11_10])]
 n10_summary[k,avg_Adams_orig_10:=median(dt[,Adam_orig_10])]
 n10_summary[k,avg_Adams_1_10:=median(dt[,Adam_1_10])]
 n10_summary[k,avg_Adams_2_10:=median(dt[,Adam_2_10])]
 n10_summary[k,avg_Adams_3_10:=median(dt[,Adam_3_10])]
 n10_summary[k,avg_HGLM_R_10:=median(dt[,HGLM_R_10],na.rm = TRUE)] 

 n10_summary[k,n_provders_with_rate_1_20:=sum(dt[,ni20]==dt[,mi20])]
 n10_summary[k,pct_provders_with_rate_1_20:=(n_provders_with_rate_1_20*100/10)]
 n10_summary[k,true_measure_R_20:=median(dt[,true_reliability_11_20])]
 n10_summary[k,avg_Adams_orig_20:=median(dt[,Adam_orig_20])]
 n10_summary[k,avg_Adams_1_20:=median(dt[,Adam_1_20])]
 n10_summary[k,avg_Adams_2_20:=median(dt[,Adam_2_20])]
 n10_summary[k,avg_Adams_3_20:=median(dt[,Adam_3_20])]
 n10_summary[k,avg_HGLM_R_20:=median(dt[,HGLM_R_20],na.rm = TRUE)]   
 
 eval(parse(text=paste0("dt=fread('simu_",k,"_out_n_110.csv')")))
 n100_summary[k,n_provders_with_rate_1:=sum(dt[,ni]==dt[,mi])]
 n100_summary[k,pct_provders_with_rate_1:=(n_provders_with_rate_1*100/100)]
 n100_summary[k,true_measure_R:=median(dt[,true_reliability_110])]
 n100_summary[k,avg_Adams_orig:=median(dt[,Adam_orig])]
 n100_summary[k,avg_Adams_1:=median(dt[,Adam_1])]
 n100_summary[k,avg_Adams_2:=median(dt[,Adam_2])]
 n100_summary[k,avg_Adams_3:=median(dt[,Adam_3])]
 n100_summary[k,avg_HGLM_R:=median(dt[,HGLM_R],na.rm = TRUE)] 
 
 n100_summary[k,n_provders_with_rate_1_10:=sum(dt[,ni10]==dt[,mi10])]
 n100_summary[k,pct_provders_with_rate_1_10:=(n_provders_with_rate_1_10*100/100)]
 n100_summary[k,true_measure_R_10:=median(dt[,true_reliability_110_10])]
 n100_summary[k,avg_Adams_orig_10:=median(dt[,Adam_orig_10])]
 n100_summary[k,avg_Adams_1_10:=median(dt[,Adam_1_10])]
 n100_summary[k,avg_Adams_2_10:=median(dt[,Adam_2_10])]
 n100_summary[k,avg_Adams_3_10:=median(dt[,Adam_3_10])]
 n100_summary[k,avg_HGLM_R_10:=median(dt[,HGLM_R_10],na.rm = TRUE)] 
 
 n100_summary[k,n_provders_with_rate_1_20:=sum(dt[,ni20]==dt[,mi20])]
 n100_summary[k,pct_provders_with_rate_1_20:=(n_provders_with_rate_1_20*100/100)]
 n100_summary[k,true_measure_R_20:=median(dt[,true_reliability_110_20])]
 n100_summary[k,avg_Adams_orig_20:=median(dt[,Adam_orig_20])]
 n100_summary[k,avg_Adams_1_20:=median(dt[,Adam_1_20])]
 n100_summary[k,avg_Adams_2_20:=median(dt[,Adam_2_20])]
 n100_summary[k,avg_Adams_3_20:=median(dt[,Adam_3_20])]
 n100_summary[k,avg_HGLM_R_20:=median(dt[,HGLM_R_20],na.rm = TRUE)]   
 
 eval(parse(text=paste0("dt=fread('simu_",k,"_out_n_1100.csv')")))
 n1000_summary[k,n_provders_with_rate_1:=sum(dt[,ni]==dt[,mi])]
 n1000_summary[k,pct_provders_with_rate_1:=(n_provders_with_rate_1*100/1000)]
 n1000_summary[k,true_measure_R:=median(dt[,true_reliability_1100])]
 n1000_summary[k,avg_Adams_orig:=median(dt[,Adam_orig])]
 n1000_summary[k,avg_Adams_1:=median(dt[,Adam_1])]
 n1000_summary[k,avg_Adams_2:=median(dt[,Adam_2])]
 n1000_summary[k,avg_Adams_3:=median(dt[,Adam_3])]
 n1000_summary[k,avg_HGLM_R:=median(dt[,HGLM_R],na.rm = TRUE)] 
 
 n1000_summary[k,n_provders_with_rate_1_10:=sum(dt[,ni10]==dt[,mi10])]
 n1000_summary[k,pct_provders_with_rate_1_10:=(n_provders_with_rate_1_10*100/1000)]
 n1000_summary[k,true_measure_R_10:=median(dt[,true_reliability_1100_10])]
 n1000_summary[k,avg_Adams_orig_10:=median(dt[,Adam_orig_10])]
 n1000_summary[k,avg_Adams_1_10:=median(dt[,Adam_1_10])]
 n1000_summary[k,avg_Adams_2_10:=median(dt[,Adam_2_10])]
 n1000_summary[k,avg_Adams_3_10:=median(dt[,Adam_3_10])]
 n1000_summary[k,avg_HGLM_R_10:=median(dt[,HGLM_R_10],na.rm = TRUE)] 
 
 n1000_summary[k,n_provders_with_rate_1_20:=sum(dt[,ni20]==dt[,mi20])]
 n1000_summary[k,pct_provders_with_rate_1_20:=(n_provders_with_rate_1_20*100/1000)]
 n1000_summary[k,true_measure_R_20:=median(dt[,true_reliability_1100_20])]
 n1000_summary[k,avg_Adams_orig_20:=median(dt[,Adam_orig_20])]
 n1000_summary[k,avg_Adams_1_20:=median(dt[,Adam_1_20])]
 n1000_summary[k,avg_Adams_2_20:=median(dt[,Adam_2_20])]
 n1000_summary[k,avg_Adams_3_20:=median(dt[,Adam_3_20])]
 n1000_summary[k,avg_HGLM_R_20:=median(dt[,HGLM_R_20],na.rm = TRUE)]
 
}

n10_summary[,Adams_orig_bias:=(avg_Adams_orig-true_measure_R_)]
n10_summary[,Adams_1_bias:=(avg_Adams_1-true_measure_R)]
n10_summary[,Adams_2_bias:=(avg_Adams_2-true_measure_R)]
n10_summary[,Adams_3_bias:=(avg_Adams_3-true_measure_R)]
n10_summary[,avg_HGLM_R_bias:=(avg_HGLM_R-true_measure_R)]
n10_summary[,Adams_orig_10_bias:=(avg_Adams_orig_10-true_measure_R_10)]
n10_summary[,Adams_1_10_bias:=(avg_Adams_1_10-true_measure_R_10)]
n10_summary[,Adams_2_10_bias:=(avg_Adams_2_10-true_measure_R_10)]
n10_summary[,Adams_3_10_bias:=(avg_Adams_3_10-true_measure_R_10)]
n10_summary[,avg_HGLM_R_10_bias:=(avg_HGLM_R_10-true_measure_R_10)]
n10_summary[,Adams_orig_20_bias:=(avg_Adams_orig_20-true_measure_R_20)]
n10_summary[,Adams_1_20_bias:=(avg_Adams_1_20-true_measure_R_20)]
n10_summary[,Adams_2_20_bias:=(avg_Adams_2_20-true_measure_R_20)]
n10_summary[,Adams_3_20_bias:=(avg_Adams_3_20-true_measure_R_20)]
n10_summary[,avg_HGLM_R_20_bias:=(avg_HGLM_R_20-true_measure_R_20)]

n100_summary[,Adams_orig_bias:=(avg_Adams_orig-true_measure_R)]
n100_summary[,Adams_1_bias:=(avg_Adams_1-true_measure_R)]
n100_summary[,Adams_2_bias:=(avg_Adams_2-true_measure_R)]
n100_summary[,Adams_3_bias:=(avg_Adams_3-true_measure_R)]
n100_summary[,avg_HGLM_R_bias:=(avg_HGLM_R-true_measure_R)]
n100_summary[,Adams_orig_10_bias:=(avg_Adams_orig_10-true_measure_R_10)]
n100_summary[,Adams_1_10_bias:=(avg_Adams_1_10-true_measure_R_10)]
n100_summary[,Adams_2_10_bias:=(avg_Adams_2_10-true_measure_R_10)]
n100_summary[,Adams_3_10_bias:=(avg_Adams_3_10-true_measure_R_10)]
n100_summary[,avg_HGLM_R_10_bias:=(avg_HGLM_R_10-true_measure_R_10)]
n100_summary[,Adams_orig_20_bias:=(avg_Adams_orig_20-true_measure_R_20)]
n100_summary[,Adams_1_20_bias:=(avg_Adams_1_20-true_measure_R_20)]
n100_summary[,Adams_2_20_bias:=(avg_Adams_2_20-true_measure_R_20)]
n100_summary[,Adams_3_20_bias:=(avg_Adams_3_20-true_measure_R_20)]
n100_summary[,avg_HGLM_R_20_bias:=(avg_HGLM_R_20-true_measure_R_20)]

n1000_summary[,Adams_orig_bias:=(avg_Adams_orig-true_measure_R)]
n1000_summary[,Adams_1_bias:=(avg_Adams_1-true_measure_R)]
n1000_summary[,Adams_2_bias:=(avg_Adams_2-true_measure_R)]
n1000_summary[,Adams_3_bias:=(avg_Adams_3-true_measure_R)]
n1000_summary[,avg_HGLM_R_bias:=(avg_HGLM_R-true_measure_R)]
n1000_summary[,Adams_orig_10_bias:=(avg_Adams_orig_10-true_measure_R_10)]
n1000_summary[,Adams_1_10_bias:=(avg_Adams_1_10-true_measure_R_10)]
n1000_summary[,Adams_2_10_bias:=(avg_Adams_2_10-true_measure_R_10)]
n1000_summary[,Adams_3_10_bias:=(avg_Adams_3_10-true_measure_R_10)]
n1000_summary[,avg_HGLM_R_10_bias:=(avg_HGLM_R_10-true_measure_R_10)]
n1000_summary[,Adams_orig_20_bias:=(avg_Adams_orig_20-true_measure_R_20)]
n1000_summary[,Adams_1_20_bias:=(avg_Adams_1_20-true_measure_R_20)]
n1000_summary[,Adams_2_20_bias:=(avg_Adams_2_20-true_measure_R_20)]
n1000_summary[,Adams_3_20_bias:=(avg_Adams_3_20-true_measure_R_20)]
n1000_summary[,avg_HGLM_R_20_bias:=(avg_HGLM_R_20-true_measure_R_20)]

fwrite(n10_summary,"gen_data_from_AHT_n11_summary.csv"); fwrite(n100_summary,"gen_data_from_AHT_n110_summary.csv"); fwrite(n1000_summary,"gen_data_from_AHT_n1100_summary.csv")




### can restart from here
rm(list=ls())
library(data.table)
dig3_mean = function(x) round(mean(x,na.rm=TRUE),digits = 3)

#n10_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model/gen_data_from_beta_dist_n10_summary.csv")
# n10_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_HGLM/gen_data_from_HGLM_n10_summary.csv")
n10_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_HGLM/gen_data_from_HGLM_n10_summary.csv")

#n100_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model/gen_data_from_beta_dist_n100_summary.csv")
n100_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_HGLM/gen_data_from_HGLM_n100_summary.csv")

# n1000_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model/gen_data_from_beta_dist_n1000_summary.csv")
n1000_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_HGLM/gen_data_from_HGLM_n1000_summary.csv")

apply(n10_summary[,.(n_provders_with_rate_1,pct_provders_with_rate_1,Adams_orig_bias,Adams_1_bias,Adams_2_bias,Adams_3_bias,avg_HGLM_R_bias)],2,dig3_mean)
apply(n100_summary[,.(n_provders_with_rate_1,pct_provders_with_rate_1,Adams_orig_bias, Adams_1_bias,Adams_2_bias,Adams_3_bias,avg_HGLM_R_bias)],2,dig3_mean)
apply(n1000_summary[,.(n_provders_with_rate_1,pct_provders_with_rate_1,Adams_orig_bias, Adams_1_bias,Adams_2_bias,Adams_3_bias,avg_HGLM_R_bias)],2,dig3_mean)

apply(n10_summary[,.(n_provders_with_rate_1_10,pct_provders_with_rate_1_10,Adams_orig_10_bias, Adams_1_10_bias,Adams_2_10_bias,Adams_3_10_bias,avg_HGLM_R_10_bias)],2,dig3_mean)
apply(n100_summary[,.(n_provders_with_rate_1_10,pct_provders_with_rate_1_10,Adams_orig_10_bias, Adams_1_10_bias,Adams_2_10_bias,Adams_3_10_bias,avg_HGLM_R_10_bias)],2,dig3_mean)
apply(n1000_summary[,.(n_provders_with_rate_1_10,pct_provders_with_rate_1_10,Adams_orig_10_bias, Adams_1_10_bias,Adams_2_10_bias,Adams_3_10_bias,avg_HGLM_R_10_bias)],2,dig3_mean)

apply(n10_summary[,.(n_provders_with_rate_1_20,pct_provders_with_rate_1_20,Adams_orig_20_bias, Adams_1_20_bias,Adams_2_20_bias,Adams_3_20_bias,avg_HGLM_R_20_bias)],2,dig3_mean)
apply(n100_summary[,.(n_provders_with_rate_1_20,pct_provders_with_rate_1_20,Adams_orig_20_bias, Adams_1_20_bias,Adams_2_20_bias,Adams_3_20_bias,avg_HGLM_R_20_bias)],2,dig3_mean)
apply(n1000_summary[,.(n_provders_with_rate_1_20,pct_provders_with_rate_1_20,Adams_orig_20_bias, Adams_1_20_bias,Adams_2_20_bias,Adams_3_20_bias,avg_HGLM_R_20_bias)],2,dig3_mean) 


### SEs  can restart from here
rm(list=ls())
library(data.table)
dig3_SE = function(x) round(sqrt(var(x,na.rm=TRUE)),digits = 2) 
#n10_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model/gen_data_from_beta_dist_n10_summary.csv")
n10_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_HGLM/gen_data_from_HGLM_n10_summary.csv")

n100_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model/gen_data_from_beta_dist_n100_summary.csv")
#n100_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_HGLM/gen_data_from_HGLM_n100_summary.csv")

n1000_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model/gen_data_from_beta_dist_n1000_summary.csv")
#n1000_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_HGLM/gen_data_from_HGLM_n1000_summary.csv")

apply(n10_summary[,.(Adams_orig_bias,Adams_1_bias,Adams_2_bias,Adams_3_bias,avg_HGLM_R_bias)],2,dig3_SE) # var(X-a)=Var(X)
apply(n100_summary[,.(Adams_orig_bias, Adams_1_bias,Adams_2_bias,Adams_3_bias,avg_HGLM_R_bias)],2,dig3_SE)
apply(n1000_summary[,.(Adams_orig_bias, Adams_1_bias,Adams_2_bias,Adams_3_bias,avg_HGLM_R_bias)],2,dig3_SE)

apply(n10_summary[,.(Adams_orig_10_bias, Adams_1_10_bias,Adams_2_10_bias,Adams_3_10_bias,avg_HGLM_R_10_bias)],2,dig3_SE)
apply(n100_summary[,.(Adams_orig_10_bias, Adams_1_10_bias,Adams_2_10_bias,Adams_3_10_bias,avg_HGLM_R_10_bias)],2,dig3_SE)
apply(n1000_summary[,.(Adams_orig_10_bias, Adams_1_10_bias,Adams_2_10_bias,Adams_3_10_bias,avg_HGLM_R_10_bias)],2,dig3_SE)

apply(n10_summary[,.(Adams_orig_20_bias, Adams_1_20_bias,Adams_2_20_bias,Adams_3_20_bias,avg_HGLM_R_20_bias)],2,dig3_SE)
apply(n100_summary[,.(Adams_orig_20_bias, Adams_1_20_bias,Adams_2_20_bias,Adams_3_20_bias,avg_HGLM_R_20_bias)],2,dig3_SE)
apply(n1000_summary[,.(Adams_orig_20_bias, Adams_1_20_bias,Adams_2_20_bias,Adams_3_20_bias,avg_HGLM_R_20_bias)],2,dig3_SE)

########
# can restart from here: compute test-retest reliablity 
########
rm(list=ls())
setwd("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model")
#setwd("c:/Users/Guohai/Documents/MEGAsync/Reliability_article/")
library(data.table)
# dt = fread("simu_1.csv"); name_n_col="n"; name_m_col="m"; dt=dt[1:10]

test_retest_one_estimate = function(dt,repeat_times=1000,name_n_col,name_m_col)
{
  num_provider = nrow(dt)
  anova_input = data.table(provider=rep(1:num_provider,each=2),sub_sample_label=rep(1:2,num_provider),y=as.numeric(NA))
  for(k in 1:num_provider) 
  { 
    eval(parse(text=paste0("ni=dt[k,",name_n_col,"]") ) )
    eval(parse(text=paste0("mi=dt[k,",name_m_col,"]") ) )
    if(ni==mi) zero_one_data=rep(1,each=mi) else zero_one_data = c(rep(1,each=mi),rep(0,each=(ni-mi)) )
    
    re_shuffle_index = sample(ni)
    # setdiff(re_shuffle_index,1:ni)
     
    half=round(ni/2,0)
    sub_sample1_rate  = mean(zero_one_data[1:half])
    sub_sample2_rate  = mean(zero_one_data[(half+1):ni]) 
    anova_input[provider==k&sub_sample_label==1,y:=sub_sample1_rate]
    anova_input[provider==k&sub_sample_label==2,y:=sub_sample2_rate]
  }
  provider_mean = anova_input[,mean(y),by=provider]
  names(provider_mean)[2]="yibar"
  y_bar= mean(provider_mean[,yibar])
  BMS = sum(  2*(provider_mean[,yibar]- y_bar)^2 )/(num_provider-1)
  
  anova_input = merge(anova_input,provider_mean,by.x = "provider",by.y="provider")
  anova_input[,yij_minus_yibar_square:=(y-yibar)^2]
  within_provider_sum_square = sum(anova_input[,yij_minus_yibar_square])
  WMS = within_provider_sum_square/(num_provider*2-num_provider)
  R = (BMS-WMS)/( BMS+(2-1)*WMS )
  # ICC(1,1) 
  corr = cor(anova_input[sub_sample_label==1,y],anova_input[sub_sample_label==2,y])
  c(R,corr)
}

set.seed(1)
for(k in 1:1000)
{
  if(k%%20==0) print(k)
  eval(parse(text=paste0("dt=fread('simu_",k,"_out_n_10.csv')")))

}


n10_summary = fread("gen_data_from_beta_dist_n10_summary.csv")
n100_summary = fread("gen_data_from_beta_dist_n100_summary.csv")
n1000_summary = fread("gen_data_from_beta_dist_n1000_summary.csv")





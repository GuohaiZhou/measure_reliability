########
# can restart from here: compute test-retest reliablity 
########
rm(list=ls())
setwd("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model")
#setwd("c:/Users/Guohai/Documents/MEGAsync/Reliability_article/")
library(data.table)

test_retest_one_estimate = function(ni_vec,mi_vec,num_provider,repeat_times=1000)
{
  anova_input = data.table(provider=rep(1:num_provider,each=2),sub_sample_label=rep(1:2,num_provider),y=as.numeric(NA))
  for(s in 1:num_provider) 
  { 
    ni = ni_vec[s]
    mi = mi_vec[s]
    if(ni==mi) zero_one_data=rep(1,each=mi) else zero_one_data = c(rep(1,each=mi),rep(0,each=(ni-mi)) )
    
    re_shuffle_index = sample(ni)
    # setdiff(re_shuffle_index,1:ni)
    zero_one_data = zero_one_data[re_shuffle_index]
    half=round(ni/2,0)
    sub_sample1_rate  = mean(zero_one_data[1:half])
    sub_sample2_rate  = mean(zero_one_data[(half+1):ni]) 
    anova_input[provider==s&sub_sample_label==1,y:=sub_sample1_rate]
    anova_input[provider==s&sub_sample_label==2,y:=sub_sample2_rate]
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
n10_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model/gen_data_from_beta_dist_n10_summary.csv")
n100_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model/gen_data_from_beta_dist_n100_summary.csv")
n1000_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_beta_binomial_model/gen_data_from_beta_dist_n1000_summary.csv")
n10_summary[,test_retest_ICC:=rep(as.numeric(NA),1000)]
n10_summary[,test_retest_ICC_10:=rep(as.numeric(NA),1000)]
n10_summary[,test_retest_ICC_20:=rep(as.numeric(NA),1000)]
n100_summary[,test_retest_ICC:=rep(as.numeric(NA),1000)]
n100_summary[,test_retest_ICC_10:=rep(as.numeric(NA),1000)]
n100_summary[,test_retest_ICC_20:=rep(as.numeric(NA),1000)]
n1000_summary[,test_retest_ICC:=rep(as.numeric(NA),1000)]
n1000_summary[,test_retest_ICC_10:=rep(as.numeric(NA),1000)]
n1000_summary[,test_retest_ICC_20:=rep(as.numeric(NA),1000)]

# k=1

set.seed(1)
for(k in 1:1000)
{
  if(k%%5==0) print(k)
  eval(parse(text=paste0("dt=fread('simu_",k,"_out_n_10.csv')")))
  test_retest_out = test_retest_one_estimate(dt[,ni],dt[,mi],nrow(dt)) # ni_vec=dt[,ni];mi_vec=dt[,mi];num_provider=nrow(dt)
  n10_summary[k,test_retest_ICC:=test_retest_out[1]]
  test_retest_out_10 = test_retest_one_estimate(dt[,ni10],dt[,mi10],nrow(dt))
  n10_summary[k,test_retest_ICC_10:=test_retest_out_10[1]]
  test_retest_out_20 = test_retest_one_estimate(dt[,ni20],dt[,mi20],nrow(dt))
  n10_summary[k,test_retest_ICC_20:=test_retest_out_20[1]]
  
  eval(parse(text=paste0("dt=fread('simu_",k,"_out_n_100.csv')")))
  test_retest_out = test_retest_one_estimate(dt[,ni],dt[,mi],nrow(dt)) # ni_vec=dt[,ni];mi_vec=dt[,mi];num_provider=nrow(dt)
  n100_summary[k,test_retest_ICC:=test_retest_out[1]]
  test_retest_out_10 = test_retest_one_estimate(dt[,ni10],dt[,mi10],nrow(dt))
  n100_summary[k,test_retest_ICC_10:=test_retest_out_10[1]]
  test_retest_out_20 = test_retest_one_estimate(dt[,ni20],dt[,mi20],nrow(dt))
  n100_summary[k,test_retest_ICC_20:=test_retest_out_20[1]]
  
  eval(parse(text=paste0("dt=fread('simu_",k,"_out_n_1000.csv')")))
  test_retest_out = test_retest_one_estimate(dt[,ni],dt[,mi],nrow(dt)) # ni_vec=dt[,ni];mi_vec=dt[,mi];num_provider=nrow(dt)
  n1000_summary[k,test_retest_ICC:=test_retest_out[1]]
  test_retest_out_10 = test_retest_one_estimate(dt[,ni10],dt[,mi10],nrow(dt))
  n1000_summary[k,test_retest_ICC_10:=test_retest_out_10[1]]
  test_retest_out_20 = test_retest_one_estimate(dt[,ni20],dt[,mi20],nrow(dt))
  n1000_summary[k,test_retest_ICC_20:=test_retest_out_20[1]] 
}

n10_summary[,test_retest_ICC_bias:=(test_retest_ICC-true_measure_R)]
n10_summary[,test_retest_ICC_10_bias:=(test_retest_ICC_10-true_measure_R_10)]
n10_summary[,test_retest_ICC_20_bias:=(test_retest_ICC_20-true_measure_R_20)]
n100_summary[,test_retest_ICC_bias:=(test_retest_ICC-true_measure_R)]
n100_summary[,test_retest_ICC_10_bias:=(test_retest_ICC_10-true_measure_R_10)]
n100_summary[,test_retest_ICC_20_bias:=(test_retest_ICC_20-true_measure_R_20)]
n1000_summary[,test_retest_ICC_bias:=(test_retest_ICC-true_measure_R)]
n1000_summary[,test_retest_ICC_10_bias:=(test_retest_ICC_10-true_measure_R_10)]
n1000_summary[,test_retest_ICC_20_bias:=(test_retest_ICC_20-true_measure_R_20)]

dig3_mean = function(x) round(mean(x,na.rm=TRUE),digits = 3)
apply(n10_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_mean)
apply(n100_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_mean)
apply(n1000_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_mean)

dig3_SE = function(x) round(sqrt(var(x,na.rm=TRUE)),digits = 2)
apply(n10_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_SE)
apply(n100_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_SE)
apply(n1000_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_SE)

 fwrite(n10_summary,"n10_test_retest_added_beta.csv")
 fwrite(n100_summary,"n100_test_retest_added_beta.csv")
 fwrite(n1000_summary,"n1000_test_retest_added_beta.csv")



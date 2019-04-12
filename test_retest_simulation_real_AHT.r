########
# can restart from here: compute test-retest reliablity 
########
rm(list=ls())
setwd("X:/reliability_paper/Adams_Snijders_reliability/under_real_AHT")
#setwd("c:/Users/Guohai/Documents/MEGAsync/Reliability_article/")
library(data.table)
# dt = fread("simu_1.csv"); name_n_col="ni"; name_m_col="mi"; dt=dt[1:10]

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
n11_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_real_AHT/gen_data_from_AHT_n11_summary.csv")
n110_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_real_AHT/gen_data_from_AHT_n11_summary.csv")
n1100_summary = fread("X:/reliability_paper/Adams_Snijders_reliability/under_real_AHT/gen_data_from_AHT_n1100_summary.csv")
n11_summary[,test_retest_ICC:=rep(as.numeric(NA),1000)]
n11_summary[,test_retest_ICC_10:=rep(as.numeric(NA),1000)]
n11_summary[,test_retest_ICC_20:=rep(as.numeric(NA),1000)]
n110_summary[,test_retest_ICC:=rep(as.numeric(NA),1000)]
n110_summary[,test_retest_ICC_10:=rep(as.numeric(NA),1000)]
n110_summary[,test_retest_ICC_20:=rep(as.numeric(NA),1000)]
n1100_summary[,test_retest_ICC:=rep(as.numeric(NA),1000)]
n1100_summary[,test_retest_ICC_10:=rep(as.numeric(NA),1000)]
n1100_summary[,test_retest_ICC_20:=rep(as.numeric(NA),1000)]
# k=1
set.seed(1)
for(k in 1:1000)
{
  if(k%%5==0) print(k)
  eval(parse(text=paste0("dt=fread('simu_",k,"_out_n_11.csv')")))
  test_retest_out = test_retest_one_estimate(dt[,ni],dt[,mi],nrow(dt)) # ni_vec=dt[,ni];mi_vec=dt[,mi];num_provider=nrow(dt)
  n11_summary[k,test_retest_ICC:=test_retest_out[1]]
  test_retest_out_10 = test_retest_one_estimate(dt[,ni10],dt[,mi10],nrow(dt))
  n11_summary[k,test_retest_ICC_10:=test_retest_out_10[1]]
  test_retest_out_20 = test_retest_one_estimate(dt[,ni20],dt[,mi20],nrow(dt))
  n11_summary[k,test_retest_ICC_20:=test_retest_out_20[1]]
  
  eval(parse(text=paste0("dt=fread('simu_",k,"_out_n_110.csv')")))
  test_retest_out = test_retest_one_estimate(dt[,ni],dt[,mi],nrow(dt)) # ni_vec=dt[,ni];mi_vec=dt[,mi];num_provider=nrow(dt)
  n110_summary[k,test_retest_ICC:=test_retest_out[1]]
  test_retest_out_10 = test_retest_one_estimate(dt[,ni10],dt[,mi10],nrow(dt))
  n110_summary[k,test_retest_ICC_10:=test_retest_out_10[1]]
  test_retest_out_20 = test_retest_one_estimate(dt[,ni20],dt[,mi20],nrow(dt))
  n110_summary[k,test_retest_ICC_20:=test_retest_out_20[1]]
  
  eval(parse(text=paste0("dt=fread('simu_",k,"_out_n_1100.csv')")))
  test_retest_out = test_retest_one_estimate(dt[,ni],dt[,mi],nrow(dt)) # ni_vec=dt[,ni];mi_vec=dt[,mi];num_provider=nrow(dt)
  n1100_summary[k,test_retest_ICC:=test_retest_out[1]]
  test_retest_out_10 = test_retest_one_estimate(dt[,ni10],dt[,mi10],nrow(dt))
  n1100_summary[k,test_retest_ICC_10:=test_retest_out_10[1]]
  test_retest_out_20 = test_retest_one_estimate(dt[,ni20],dt[,mi20],nrow(dt))
  n1100_summary[k,test_retest_ICC_20:=test_retest_out_20[1]] 
}


n11_summary[,test_retest_ICC_bias:=(test_retest_ICC-true_measure_R)]
n11_summary[,test_retest_ICC_10_bias:=(test_retest_ICC_10-true_measure_R_10)]
n11_summary[,test_retest_ICC_20_bias:=(test_retest_ICC_20-true_measure_R_20)]
n110_summary[,test_retest_ICC_bias:=(test_retest_ICC-true_measure_R)]
n110_summary[,test_retest_ICC_10_bias:=(test_retest_ICC_10-true_measure_R_10)]
n110_summary[,test_retest_ICC_20_bias:=(test_retest_ICC_20-true_measure_R_20)]
n1100_summary[,test_retest_ICC_bias:=(test_retest_ICC-true_measure_R)]
n1100_summary[,test_retest_ICC_10_bias:=(test_retest_ICC_10-true_measure_R_10)]
n1100_summary[,test_retest_ICC_20_bias:=(test_retest_ICC_20-true_measure_R_20)]

dig3_mean = function(x) round(mean(x,na.rm=TRUE),digits = 3)
apply(n11_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_mean)
apply(n110_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_mean)
apply(n1100_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_mean)

dig3_SE = function(x) round(sqrt(var(x,na.rm=TRUE)),digits = 3)
apply(n11_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_SE)
apply(n110_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_SE)
apply(n1100_summary[,.(test_retest_ICC_bias,test_retest_ICC_10_bias,test_retest_ICC_20_bias)],2,dig3_SE)


fwrite(n11_summary,"n11_test_retest_added_real_AHT.csv")
fwrite(n110_summary,"n110_test_retest_added_real_AHT.csv")
fwrite(n1100_summary,"n1100_test_retest_added_real_AHT.csv")
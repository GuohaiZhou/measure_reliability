  library(data.table)
  rm(list=ls());gc()
  setwd("C:/Users/gz223/Box Sync/Reliablity_paper_full_draft")
  # setwd("C:/Users/gz223/Box Sync")
  # setwd("C:/Users/GUOHA/Dropbox/Reliability_article")
  # setwd("c:/Users/Guohai/Documents/MEGAsync/Reliability_article")
  lambda=1
  
  real_data_figure=function(lambda)
  {  
   ### plot provider level reliablity 
   # HGLM vs 4 Adams, orignal Adams vs 3 revised Adams 
   d1=fread("real_data_ACCC.csv")
   
   d1[,var_between_provider_lambda:=(lambda*var_between_provider)]
   d1[,Adams_orig:=( var_between_provider_lambda/(var_within_provider+var_between_provider_lambda) )]
   d1[,Adams_1:=( var_between_provider_lambda/(var_within_provider_1+var_between_provider_lambda) )]
   d1[,Adams_2:=( var_between_provider_lambda/(var_within_provider_2+var_between_provider_lambda) )]
   d1[,Adams_3:=( var_between_provider_lambda/(var_within_provider_3+var_between_provider_lambda) )]
   d1[,HGLM_R:=( tau2*lambda/(tau2*lambda+HGLM_var_within) )]
   
   round(quantile( d1[,Adams_orig],probs=c(0.5,0.025,0.975)),3)
   round(quantile( d1[,Adams_1],probs=c(0.5,0.025,0.975)),3)
   round(quantile( d1[,Adams_2],probs=c(0.5,0.025,0.975)),3)
   round(quantile( d1[,Adams_3],probs=c(0.5,0.025,0.975)),3)
   round(quantile( d1[,HGLM_R],probs=c(0.5,0.025,0.975)),3)
   
   d1[,ni_grp:=ifelse(ni<15,1,ifelse(ni<50,2,3))]
   d1[,pihat_grp:=ifelse(pihat<0.95,1,ifelse(pihat<0.9999,2,3))]
   d1[,.(ni,ni_grp,pihat,pihat_grp)]
   d1[,pch:=ifelse(pihat_grp==1,1,16)] #ifelse(n<50,2,3)
   d1[,col:=ifelse(pihat_grp==1,"black",ifelse(pihat_grp==2,"gray70","gray20"))]
   
    eval(parse(text=paste0("tiff('ACCC_lambda_",lambda,".tiff',width=8,units = 'in',height = 16,res = 300)")))
  
  par(mfrow=c(4,2))
   par(mar=c(5.0,5.3,1,1)) #  sets the bottom, left, top and right margins 
   #1
   plot(cex.lab=1.5,cex.axis=2,y=d1[,HGLM_R],x=d1[,Adams_orig],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("HGLM approach",hat(R)[i]^(HGLM))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Original Adams approach ",hat(R)[i]^(Adams))), line=3.99, cex.lab=1.5) 
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   
   #2
   plot(cex.lab=1.5,cex.axis=2,y=d1[,Adams_1],x=d1[,Adams_orig],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="" ,xlab=""  )
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("Revised Adams approach ",hat(R)[i]^(1))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Original Adams approach ",hat(R)[i]^(Adams))), line=3.99, cex.lab=1.5) 
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  

   #3
   plot(cex.lab=1.5,cex.axis=2,y=d1[,HGLM_R],x=d1[,Adams_1],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("HGLM approach",hat(R)[i]^(HGLM))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Revised Adams approach ",hat(R)[i]^(1))) , line=3.99, cex.lab=1.5) 
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   
   #4
   plot(cex.lab=1.5,cex.axis=2,y=d1[,Adams_2],x=d1[,Adams_orig],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("Revised Adams approach ",hat(R)[i]^(2))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Original Adams approach ",hat(R)[i]^(Adams))), line=3.99, cex.lab=1.5)
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   
   #5
   plot(cex.lab=1.5,cex.axis=2,y=d1[,HGLM_R],x=d1[,Adams_2],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("HGLM approach",hat(R)[i]^(HGLM))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Revised Adams approach ",hat(R)[i]^(2))), line=3.99, cex.lab=1.5)
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   
   #6
   plot(cex.lab=1.5,cex.axis=2,y=d1[,Adams_3],x=d1[,Adams_orig],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("Revised Adams approach ",hat(R)[i]^(3))) , line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Original Adams approach ",hat(R)[i]^(Adams)))  , line=3.99, cex.lab=1.5)
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   #7
   plot(cex.lab=1.5,cex.axis=2,y=d1[,HGLM_R],x=d1[,Adams_3],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("HGLM approach",hat(R)[i]^(HGLM))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Revised Adams approach ",hat(R)[i]^(3)))  , line=3.99, cex.lab=1.5)
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);
   
   initial_pos = 0.05
   plot(y=d1[,HGLM_R],x=d1[,Adams_3],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),type="n",ylab="",xlab="",xaxt="n",yaxt="n")
   points(x=c(initial_pos,initial_pos,initial_pos),y=c(0.8,0.5,0.2),cex=1:3)
   text(x=initial_pos-0.003,y=0.9,labels = "# of cases",cex = 1.7,pos=4,offset=0)
   text(x=initial_pos+0.05,y=0.8,labels = "<15",cex = 1.3,pos=4,offset=0)
   text(x=initial_pos+0.05,y=0.5,labels = ">=15, <50",cex = 1.3,pos=4,offset=0)
   text(x=initial_pos+0.05,y=0.2,labels = ">=50",cex = 1.3,pos=4,offset=0)
   
   gap_adjust=0.07
   points(x=c(0.5+gap_adjust,0.5+gap_adjust,0.5+gap_adjust),y=c(0.8,0.5,0.2),cex=4,pch=c(1,16,16),col=c("black","gray70","gray20"))
   text(x=0.48+gap_adjust,y=0.9,labels = "Crude event rate",cex = 1.7,pos=4,offset=0)
   text(x=0.55+gap_adjust,y=0.8,labels = "<95%",cex = 1.3,pos=4,offset=0)
   text(x=0.55+gap_adjust,y=0.5,labels = ">=95%, <100%",cex = 1.3,pos=4,offset=0)
   text(x=0.55+gap_adjust,y=0.2,labels = "=100%",cex = 1.3,pos=4,offset=0)
   par(mfrow=c(1,1))
   dev.off()
   
 
   ### plot provider level reliablity 
   # HGLM vs 4 Adams, orignal Adams vs 3 revised Adams 
   d1=fread("real_data_AHT.csv")
   
   d1[,var_between_provider_lambda:=(lambda*var_between_provider)]
   d1[,Adams_orig:=( var_between_provider_lambda/(var_within_provider+var_between_provider_lambda) )]
   d1[,Adams_1:=( var_between_provider_lambda/(var_within_provider_1+var_between_provider_lambda) )]
   d1[,Adams_2:=( var_between_provider_lambda/(var_within_provider_2+var_between_provider_lambda) )]
   d1[,Adams_3:=( var_between_provider_lambda/(var_within_provider_3+var_between_provider_lambda) )]
   d1[,HGLM_R:=( tau2*lambda/(tau2*lambda+HGLM_var_within) )]
   
   round(quantile( d1[,Adams_orig],probs=c(0.5,0.025,0.975)),3)
   round(quantile( d1[,Adams_1],probs=c(0.5,0.025,0.975)),3)
   round(quantile( d1[,Adams_2],probs=c(0.5,0.025,0.975)),3)
   round(quantile( d1[,Adams_3],probs=c(0.5,0.025,0.975)),3)
   round(quantile( d1[,HGLM_R],probs=c(0.5,0.025,0.975)),3)
   
   
   d1[,ni_grp:=ifelse(ni<200,1,ifelse(ni<450,2,3))]
   d1[,pihat_grp:=ifelse(pihat<0.95,1,ifelse(pihat<0.99,2,3))]
   d1[,.(ni,ni_grp,pihat,pihat_grp)]
   d1[,pch:=ifelse(pihat_grp==1,1,16)] #ifelse(n<50,2,3)
   d1[,col:=ifelse(pihat_grp==1,"black",ifelse(pihat_grp==2,"gray70","gray20"))]
   eval(parse(text=paste0("tiff('AHT_lambda_",lambda,".tiff',width=8,units = 'in',height = 16,res = 300)")))
   par(mfrow=c(4,2))
   par(mar=c(5.0,5.3,1,1)) #  sets the bottom, left, top and right margins 
   #1
   plot(cex.lab=1.5,cex.axis=2,y=d1[,HGLM_R],x=d1[,Adams_orig],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("HGLM approach",hat(R)[i]^(HGLM))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Original Adams approach ",hat(R)[i]^(Adams))), line=3.99, cex.lab=1.5) 
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   #2
   plot(cex.lab=1.5,cex.axis=2,y=d1[,Adams_1],x=d1[,Adams_orig],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="" ,xlab=""  )
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("Revised Adams approach ",hat(R)[i]^(1))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Original Adams approach ",hat(R)[i]^(Adams))), line=3.99, cex.lab=1.5) 
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   
   
   #3
   plot(cex.lab=1.5,cex.axis=2,y=d1[,HGLM_R],x=d1[,Adams_1],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("HGLM approach",hat(R)[i]^(HGLM))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Revised Adams approach ",hat(R)[i]^(1))) , line=3.99, cex.lab=1.5) 
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   
   #4
   plot(cex.lab=1.5,cex.axis=2,y=d1[,Adams_2],x=d1[,Adams_orig],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("Revised Adams approach ",hat(R)[i]^(2))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Original Adams approach ",hat(R)[i]^(Adams))), line=3.99, cex.lab=1.5)
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   
   #5
   plot(cex.lab=1.5,cex.axis=2,y=d1[,HGLM_R],x=d1[,Adams_2],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("HGLM approach",hat(R)[i]^(HGLM))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Revised Adams approach ",hat(R)[i]^(2))), line=3.99, cex.lab=1.5)
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   
   #6
   plot(cex.lab=1.5,cex.axis=2,y=d1[,Adams_3],x=d1[,Adams_orig],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("Revised Adams approach ",hat(R)[i]^(3))) , line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Original Adams approach ",hat(R)[i]^(Adams)))  , line=3.99, cex.lab=1.5)
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   
   #7
   plot(cex.lab=1.5,cex.axis=2,y=d1[,HGLM_R],x=d1[,Adams_3],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),ylab="",xlab="")
   abline(0,1,lwd=1,lty=2)
   title(ylab=expression(paste("HGLM approach",hat(R)[i]^(HGLM))), line=2.3, cex.lab=1.5) 
   title(xlab=expression(paste("Revised Adams approach ",hat(R)[i]^(3)))  , line=3.99, cex.lab=1.5)
   abline(h=c(0.2,0.6,1),v=c(0.2,0.6,1),lty=3,lwd=1);  
   
   plot(y=d1[,HGLM_R],x=d1[,Adams_3],pch= d1[,pch],col= d1[,col],cex=d1[,ni_grp],xlim=c(0,1),ylim=c(0,1),type="n",ylab="",xlab="",xaxt="n",yaxt="n")
   points(x=rep(initial_pos,3),y=c(0.8,0.5,0.2),cex=1:3)
   text(x=initial_pos-0.003,y=0.9,labels = "# of cases",cex = 1.7,pos=4,offset=0)
   text(x=initial_pos+0.05,y=0.8,labels = "<200",cex = 1.3,pos=4,offset=0)
   text(x=initial_pos+0.05,y=0.5,labels = ">=200, <450",cex = 1.3,pos=4,offset=0)
   text(x=initial_pos+0.05,y=0.2,labels = ">=450",cex = 1.3,pos=4,offset=0)
   
   points(x=rep(0.5+gap_adjust,3),y=c(0.8,0.5,0.2),cex=4,pch=c(1,16,16),col=c("black","gray70","gray20"))
   text(x=0.48+gap_adjust,y=0.9,labels = "Crude event rate",cex = 1.7,pos=4,offset=0)
   text(x=0.55+gap_adjust,y=0.8,labels = "<95%",cex = 1.3,pos=4,offset=0)
   text(x=0.55+gap_adjust,y=0.5,labels = ">=95%, <99%",cex = 1.3,pos=4,offset=0)
   text(x=0.55+gap_adjust,y=0.2,labels = ">=99%. <100%",cex = 1.3,pos=4,offset=0)
   dev.off()
  
  }
  real_data_figure(lambda=1);
  real_data_figure(lambda=0.7);
  real_data_figure(lambda=0.3);
 
  ### measure level reliablity point estimate
  library(data.table)
  rm(list=ls());gc()
  setwd("C:/Users/gz223/Box Sync/Reliablity_paper_full_draft")
  median_3digits=function(x) round(median(x),3)
  real_data_measure_reliability_point_estimate=function(lambda)
  {  
    ### plot provider level reliablity 
    # HGLM vs 4 Adams, orignal Adams vs 3 revised Adams 
    d1=fread("real_data_ACCC.csv")
    
    d1[,var_between_provider_lambda:=(lambda*var_between_provider)]
    d1[,Adams_orig:=( var_between_provider_lambda/(var_within_provider+var_between_provider_lambda) )]
    d1[,Adams_1:=( var_between_provider_lambda/(var_within_provider_1+var_between_provider_lambda) )]
    d1[,Adams_2:=( var_between_provider_lambda/(var_within_provider_2+var_between_provider_lambda) )]
    d1[,Adams_3:=( var_between_provider_lambda/(var_within_provider_3+var_between_provider_lambda) )]
    d1[,HGLM_R:=( tau2*lambda/(tau2*lambda+HGLM_var_within) )]
    cat(paste0("ACCC measure level reliablity point estimate for lambda=",lambda,"\n"))
    print(apply(d1[,.(Adams_orig,Adams_1,Adams_2,Adams_3,HGLM_R)],2,median_3digits))
    
    d1=fread("real_data_AHT.csv")
    d1[,var_between_provider_lambda:=(lambda*var_between_provider)]
    d1[,Adams_orig:=( var_between_provider_lambda/(var_within_provider+var_between_provider_lambda) )]
    d1[,Adams_1:=( var_between_provider_lambda/(var_within_provider_1+var_between_provider_lambda) )]
    d1[,Adams_2:=( var_between_provider_lambda/(var_within_provider_2+var_between_provider_lambda) )]
    d1[,Adams_3:=( var_between_provider_lambda/(var_within_provider_3+var_between_provider_lambda) )]
    d1[,HGLM_R:=( tau2*lambda/(tau2*lambda+HGLM_var_within) )]
    cat(paste0("\n\nAHT measure level reliablity point estimate for lambda=",lambda,"\n"))
    apply(d1[,.(Adams_orig,Adams_1,Adams_2,Adams_3,HGLM_R)],2,median_3digits)
  }
  real_data_measure_reliability_point_estimate(1)
  real_data_measure_reliability_point_estimate(0.7)
  real_data_measure_reliability_point_estimate(0.3)
  
### measure level reliablity CI: bootstrap median   
  library(data.table)
  rm(list=ls());gc()
  
  boot_median_CI=function(x)
  {
    n=length(x)
    replicate=rep(NA,1000)
    for(k in 1:1000) 
    { 
      index=sample(1:n,replace = TRUE)
      x1=x[index]
      replicate[k] = median(x1)
    }
    round(quantile(replicate,probs = c(0.025,0.975)),3)
  }
  
  setwd("C:/Users/gz223/Box Sync/Reliablity_paper_full_draft")
  
  

  lambda=0.7
  real_data_measure_reliability_CI=function(lambda)
  {
    d1=fread("real_data_ACCC.csv")
    
    d1[,var_between_provider_lambda:=(lambda*var_between_provider)]
    d1[,Adams_orig:=( var_between_provider_lambda/(var_within_provider+var_between_provider_lambda) )]
    d1[,Adams_1:=( var_between_provider_lambda/(var_within_provider_1+var_between_provider_lambda) )]
    d1[,Adams_2:=( var_between_provider_lambda/(var_within_provider_2+var_between_provider_lambda) )]
    d1[,Adams_3:=( var_between_provider_lambda/(var_within_provider_3+var_between_provider_lambda) )]
    d1[,HGLM_R:=( tau2*lambda/(tau2*lambda+HGLM_var_within) )]
   
  ACCC_adam_orig_CI = boot_median_CI(d1[,Adams_orig])
  ACCC_adam_1_CI = boot_median_CI(d1[,Adams_1])
  ACCC_adam_2_CI = boot_median_CI(d1[,Adams_2])
  ACCC_adam_3_CI = boot_median_CI(d1[,Adams_3])
  ACCC_HGLM_R_CI = boot_median_CI(d1[,HGLM_R])
  cat(paste0("ACCC measure level reliablity CI with lambda=",lambda,"\n"))
  cat(paste0("\nAdams original: ",ACCC_adam_orig_CI[1],' - ',ACCC_adam_orig_CI[2],"\n"))
  cat(paste0("\nAdams 1: ",ACCC_adam_1_CI[1],' - ',ACCC_adam_1_CI[2],"\n"))
  cat(paste0("\nAdams 2: ",ACCC_adam_2_CI[1],' - ',ACCC_adam_2_CI[2],"\n"))
  cat(paste0("\nAdams 3: ",ACCC_adam_3_CI[1],' - ',ACCC_adam_3_CI[2],"\n"))
  cat(paste0("\nHGLM: ",ACCC_HGLM_R_CI[1],' - ',ACCC_HGLM_R_CI[2],"\n"))
 
  
  
  d1=fread("real_data_AHT.csv")
  
  d1[,var_between_provider_lambda:=(lambda*var_between_provider)]
  d1[,Adams_orig:=( var_between_provider_lambda/(var_within_provider+var_between_provider_lambda) )]
  d1[,Adams_1:=( var_between_provider_lambda/(var_within_provider_1+var_between_provider_lambda) )]
  d1[,Adams_2:=( var_between_provider_lambda/(var_within_provider_2+var_between_provider_lambda) )]
  d1[,Adams_3:=( var_between_provider_lambda/(var_within_provider_3+var_between_provider_lambda) )]
  d1[,HGLM_R:=( tau2*lambda/(tau2*lambda+HGLM_var_within) )]
  AHT_adam_orig_CI = boot_median_CI(d1[,Adams_orig])
  AHT_adam_1_CI = boot_median_CI(d1[,Adams_1])
  AHT_adam_2_CI = boot_median_CI(d1[,Adams_2])
  AHT_adam_3_CI = boot_median_CI(d1[,Adams_3])
  AHT_HGLM_R_CI = boot_median_CI(d1[,HGLM_R])
  
  cat(paste0("AHT measure level reliablity CI with lambda=",lambda,"\n"))
  cat(paste0("\nAdams original: ",AHT_adam_orig_CI[1],' - ',AHT_adam_orig_CI[2],"\n"))
  cat(paste0("\nAdams 1: ",AHT_adam_1_CI[1],' - ',AHT_adam_1_CI[2],"\n"))
  cat(paste0("\nAdams 2: ",AHT_adam_2_CI[1],' - ',AHT_adam_2_CI[2],"\n"))
  cat(paste0("\nAdams 3: ",AHT_adam_3_CI[1],' - ',AHT_adam_3_CI[2],"\n"))
  cat(paste0("\nHGLM: ",AHT_HGLM_R_CI[1],' - ',AHT_HGLM_R_CI[2],"\n"))
  
  }
  
  set.seed(1)
  real_data_measure_reliability_CI(1)
  real_data_measure_reliability_CI(0.7)
  real_data_measure_reliability_CI(0.3)
 
## calculate test-retest meausre level reliablity
# random permutation of observations within each provider, 1st half as sub-sample 1, 2-nd half as sub-sample 2; 
   library(data.table)
   rm(list=ls());gc()
 setwd("C:/Users/gz223/Box Sync/Reliablity_paper_full_draft")
   # setwd("c:/Users/Guohai/Documents/MEGAsync/Reliability_article")
 
# d1=fread("real_data_AHT.csv"); ni_vec = d1[,ni]; mi_vec=d1[,mi]; num_provider=nrow(d1); lambda_vec=c(1,0.7,0.3);repeat_times=10
test_retest_estimate_and_CI = function(ni_vec,mi_vec,num_provider,lambda_vec,repeat_times=1000)
{
  anova_input = data.table(provider=rep(1:num_provider,each=2),sub_sample_label=rep(1:2,num_provider),y=as.numeric(NA))
  number_of_lambda_candidates = length(lambda_vec)
  reliability = matrix(rep(as.numeric(NA),repeat_times*number_of_lambda_candidates),ncol=3)
 
   
  for( k in 1:repeat_times)
 { 
    if(k%%100==0) print(k)
 
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
  
  anova_input2 = merge(anova_input,provider_mean,by.x = "provider",by.y="provider")
  anova_input2[,yij_minus_yibar_square:=(y-yibar)^2]
  within_provider_sum_square = sum(anova_input2[,yij_minus_yibar_square])
  within_provider_variance = WMS = within_provider_sum_square/(num_provider*2-num_provider)
  between_provider_variance = (1/2)*(BMS-WMS)
  
  for(lambda_candidate_index in 1:number_of_lambda_candidates)  reliability[k,lambda_candidate_index] = between_provider_variance*lambda_vec[lambda_candidate_index]/(between_provider_variance*lambda_vec[lambda_candidate_index]+within_provider_variance)
    # when lambda =1, reliability[k,lambda_candidate_index] is equal to  (BMS-WMS)/( BMS+(2-1)*WMS ), the ICC(1,1) formula in Shrout 1979
    
 
  }
   reliability 
}
set.seed(1)
d1=fread("real_data_AHT.csv"); 
AHT_out =  test_retest_estimate_and_CI(ni_vec=d1[,ni],mi_vec=d1[,mi],num_provider=nrow(d1),lambda_vec=c(1,0.7,0.3),repeat_times=1000);
 AHT_out2 = apply(AHT_out,2,quantile,probs=c(0.5,0.025,0.975))
colnames(AHT_out2)=c("lambda1","lambda0.7","lambda0.3"); round(AHT_out2,3)
#        lambda1 lambda0.7 lambda0.3
#50%     0.541     0.452     0.261
#2.5%   -0.004    -0.003    -0.001
# 97.5%   0.764     0.694     0.493

d1=fread("real_data_ACCC.csv"); 
ACCC_out =  test_retest_estimate_and_CI(ni_vec=d1[,ni],mi_vec=d1[,mi],num_provider=nrow(d1),lambda_vec=c(1,0.7,0.3),repeat_times=1000);
ACCC_out2 = apply(ACCC_out,2,quantile,probs=c(0.5,0.025,0.975))
colnames(ACCC_out2)=c("lambda1","lambda0.7","lambda0.3"); round(ACCC_out2,3)
#      lambda1 lambda0.7 lambda0.3
#50%     0.120     0.087     0.039
#2.5%   -0.212    -0.139    -0.055
#97.5%   0.581     0.492     0.294

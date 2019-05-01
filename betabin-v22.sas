
/* MACRO BETABIN  Version 2.2  March 2005

   SUMMARY: Fits a Beta Binomial Model.

   AUTHOR: Ian Wakeling - Qi Statistics
           please send any comments, bug-reports to: ian@qistatistics.co.uk
           Web site: www.qistatistcs.co.uk

   REQUIREMENTS:
           This macro requires SAS/BASE and SAS/STAT to be licensed on your
           machine.  SAS/GRAPH is optional.  SAS version 8 or later.

   METHOD: The macro makes use of the procedure NLMIXED and its facility to 
           perform a maximum likelihood estimation on a general function 
           that is defined by programming statements. Both the standard
           Beta-Bimomial and the Corrected Beta-Binomial (Brockhoff 2003,
           FQ&P, vol14, 405-417) models are supported.  For comparison
           purposes, the procedure GENMOD is used to derive the simple
           binomial model estimate.

   INPUT:  A data set with one record per subject, and with two variables
           indicating the number of trials and the number of successes per
           subject.

   OUTPUT: Estimates of the proportion of successes (with standard error),
           and a test of difference of this proportion from that expected
           under the null hypothesis, for both the Binomial and the Beta
           Binomial Models.  For the latter estimates of the parameters
           gamma and theta are provided which can be used to diagnose the
           amount of extra-binomial variation that the subjects introduce.
           Gamma is a useful measure of departure from the binomial model,
           it is constrained to the interval 0-1 and takes a value of
           zero when no extra-binomial variation is present, i.e. when the
           observed proportions for all subjects are equal.

    LEGAL: The BETABIN macro is (C) Ian Wakeling 2004-2005, it is free
           software and may be redistributed in its original form provided
           that no charge is made. This macro is provided 'as is'.  There
           are no warranties, expressed or implied, as to merchantability or
           fitness for a particular purpose.

   CHANGES: Version 1.2 to Version 1.5
            -Exit is more tidy when key variables are not found.
            -Added summary table by adapting summary info from GENMOD.
            -Added estimates of alpha and beta for the beta-binomial.
            -Changed bounds to gamma>0 to prevent division by zero errors
             for gamma=0. Added messages when gamma approaches its bounds.
            -Turned off page breaks between the sections of output.
            -Added option for Per Brockhoff's Corrected Beta-Binomial.

            Version 1.5 to Version 1.6
            -Corrected a few problems with titles.

            Version 1.6 to Version 2.0
            -Switched to Jian Bi's form of the pdf for the corrected
             beta-binomial. (Many thanks to Jian for providing a pre-
             publication version of this.)
            -Fixed bug that caused data set to be kept open if an input 
             variable did not exist
            -Added code to draw a graph of the estimated beta distribution.

            Version 2.1
            -Prevented attempt to draw graph if SAS/GRAPH is not installed
            -Prevented unwanted output going to ODS streams other than LISTING.

            Version 2.2
            -In SAS Version 9 the simple binomial in GENMOD was failing since
            the format of the ODS output files are changed from version 8.
            The macro now has different code for different SAS versions.
            -Changed graphics font to Arial, reduced font sizes a bit and 
            specified smaller graphics area.

    USAGE: Simplest syntax would be:

           %betabin(data=<dataset name>,ntrials=<var name>,nsucc=<var name>);

           Note that the parameters nullprob and alpha would take on their
           default settings of 0.5 and 0.05 respectively.

           For data from a triangle test, where the probability of guessing
           correct is one third, where the user wants to supress all printed
           output and receive the beta-binomial estimates in an output
           data set the syntax would be:
          
           %betabin(data=<input dataset>,ntrials=<var name>,nsucc=<var name>,
                    nullprob=0.3333,est=<output dataset name>,print=no);

*/

options nocentre nodate nonumber;

%macro betabin(data=,        /* Input SAS data set name                   */
               title=BETABIN Macro,
			                 /* Title for the output                      */
               ntrials=,     /* Variable name giving number of trials     */
               nsucc=,       /* Variable name giving number of successes  */
			   method=BB,    /* +Method either BB or CBB (corrected BB)   */
               nullprob=0.5, /* +Expected proportion of successes under H0*/
			   alpha=0.05,   /* +Alpha level for confidence intervals     */
               binest=,      /* +Name of data set to save simple Binomial */
                             /*  model estimate of mu                     */ 
               est=,         /* +Name of data set to save Beta Binomial   */
                             /*  estimates                                */
               cov=,         /* +Name of data set to save var-cov matrix  */
			                 /*  for the Beta Binomial model parameters   */
               print=yes);   /* +Produce printed output yes/no            */ 

/*  above '+' indicates an optional macro parameter */

%let bberror=0;
%let eprefix=ERROR in Macro BetaBin:;

/* Error checking on the mandatory parameters */
%if %sysfunc(exist(&data))=0 %then %do;
  %put &eprefix Data set &data does not exist;
  %goto bbexit;
%end;
%let dsid=%sysfunc(open(&data));
%if &dsid=0 %then %do;
  %put &eprefix Data set &data already in use (attempt to open failed);
  %goto bbexit;
%end;
%if %sysfunc(varnum(&dsid,&ntrials))=0 %then %do;
  %put &eprefix Variable &ntrials not found on the data set &data;
  %let dsid=%sysfunc(close(&dsid));
  %goto bbexit;
%end;
%if %sysfunc(varnum(&dsid,&nsucc))=0 %then %do;
  %put &eprefix Variable &nsucc not found on the data set &data;
  %let dsid=%sysfunc(close(&dsid));
  %goto bbexit;
%end;
%let dsid=%sysfunc(close(&dsid));

/* Error checking on the data, also total up the degrees of freedom */
data _null_;
  set &data end=fini;
  if &nsucc>&ntrials or &nsucc<0 or &ntrials<1 then do;
    put 'Invalid number of trials & successes (' &ntrials ','
        &nsucc ') for subject ' _n_;
    call symput('bberror','1');
  end;
  total+&ntrials;
  tsucc+&nsucc;
  if fini then do;
    call symput('rdf',trim(left(put(total-2,8.0))));
	call symput('ts',trim(left(put(tsucc,8.0))));
  end;
run;

%if &bberror>0 %then %do;
  %put ERROR in Macro BetaBin: Check Data for errors;
  %goto bbexit;
%end;
%if &ts=0 %then %do;
  %put ERROR in Macro BetaBin: No observed successes, model fitting can not procede;
  %goto bbexit;
%end;

/* Local names for the output data sets if the user has not requested them */
%if %length(&binest)=0 %then %let binest=_binest; %else %let binest=&binest;
%if %length(&est)=0 %then %let est=_est; %else %let est=&est;
%if %length(&cov)=0 %then %let cov=_cov; %else %let cov=&cov;

/* get the SAS version number (taking number to left of the decimal point) */
%let version=%scan(&sysver,1,.);

ods exclude all;
%if &version<9 %then %do;
  ods output ModelInfo=_summary ParameterEstimates=&binest;
  /* the variable names for the conf limits are dependent of SAS version */
  %let UCL=UpperCL;
  %let LCL=LowerCL;
%end; %else %do;
  /* in SAS9 NObs is a second summary data set */
  ods output ModelInfo=_summary NObs=_nobs ParameterEstimates=&binest;
  %let UCL=UpperWaldCL;
  %let LCL=LowerWaldCL;
%end;
/* fit the ordinary binomial model */
proc genmod data=&data;
  model &nsucc/&ntrials= / dist=bin link=id alpha=&alpha type1;
run;
/* take the useful summary information from GENMOD */
data _summary;
  length Label1 $27;
  set _summary(drop=nValue1);
  if _n_ in (2 3) then delete;
  cValue1=left(cValue1);
  label Label1='Item' cValue1='Value';
run;
%if &version>8 %then %do;
  /* for version 9 the numeric summary info is in a separate data set, so
     merge together into something like the old version 8 summary */
  data _nobs;
    set _nobs(keep=Label N rename=(Label=Label1));
	cValue1=left(put(N,7.0));
	drop N;
  run;
  data _summary;
    set _summary _nobs;
  run;
%end;

/* transform the output of GENMOD into something similar to NLMIXED */
data &binest;
  length parameter $20;
  set &binest(drop=DF rename=(chisq=tvalue probchisq=probt &UCL=Upper
             &LCL=Lower));
  parameter='mu';
  tvalue=sqrt(tvalue);
  Alpha=&alpha;
  output;
  diff=Upper-estimate;
  parameter="|mu-&nullprob|"; estimate=abs(estimate-&nullprob);
  Upper=estimate+diff; Lower=estimate-diff;
  tvalue=estimate/stderr;
  probt=2*(1-probt(tvalue,&rdf+1));
  output;
  stop;
  label tvalue='t Value' probt='Prob > |t|' upper=' ' lower=' ';
run;

ods output ConvergenceStatus=_conv AdditionalEstimates=&est
           CovMatAddEst=&cov;
%if %upcase(&method=BB) %then %do;
  /* Estimate parameters mu and gamma for the Beta-Binomial */
  proc nlmixed data=&data fconv=1E-14 df=&rdf alpha=&alpha ecov cov;
    parms mu=0.5 gamma=0.5;
    bounds mu>=0, mu<=1, gamma>0, gamma<1;
    ll=0;
    theta=gamma/(1-gamma);
    do i=1 to &ntrials;
      if i<=&nsucc
        then ll=ll+log(mu+(i-1)*theta);
	    else ll=ll+log((1-mu)+(i-&nsucc-1)*theta);
      ll=ll-log(1+(i-1)*theta);
    end;
	alpha=mu/theta;
	beta=(1-mu)/theta;
    model &nsucc~general(ll);
    estimate 'mu'    mu;
    estimate 'alpha' alpha;
    estimate 'beta'  beta;
    estimate 'gamma' gamma;
    estimate 'theta' gamma/(1-gamma);
    estimate "|mu-&nullprob|" abs(mu-&nullprob);
    call symput('gamma',trim(left(put(gamma,best12.))));
    call symput('alpha',trim(left(put(alpha,best12.))));
    call symput('beta',trim(left(put(beta,best12.))));
  run;
  quit;
  %let cutoff=0;
%end; %else %do;
  /* Estimate parameters a and b for the Corrected Beta-Binomial. This uses
     Jian Bi's form of the pdf which is more efficient than Brockhoff's
     version that was used in version 1.6 and earlier. Have had to change
     bounds to exclude zero as it is possible to get a=b=0 and then a
     division by zero error from the estimate statement.
  */
  proc nlmixed data=&data fconv=1E-14 df=&rdf alpha=&alpha ecov cov;
    parms a=1 b=1;
    bounds a>0, b>0;
    ll=0;
    sum=0;
    gaba=lgamma(a+b)-lgamma(a)-lgamma(b)+
         lgamma(&ntrials+1)-lgamma(&nsucc+1)-lgamma(&ntrials-&nsucc+1);
    do i=0 to &nsucc;
      v1=lgamma(&nsucc+1)-lgamma(i+1)-lgamma(&nsucc-i+1);
	  v2=(&ntrials-&nsucc+i)*log(1-&nullprob)+(&nsucc-i)*log(&nullprob);
	  v3=lgamma(a+i)+lgamma(&ntrials-&nsucc+b)-lgamma(&ntrials-&nsucc+i+a+b);
	  sum=sum+exp(gaba+v1+v2+v3);
    end;
	g=1/(a+b+1);
	ll=ll+log(sum);
    model &nsucc~general(ll);
    estimate 'mu' &nullprob+(1-&nullprob)*a/(a+b);
    estimate 'alpha' a;
    estimate 'beta' b;
	estimate 'gamma' g;
    estimate 'pi' a/(a+b);
    call symput('gamma',trim(left(put(g,best12.))));
    call symput('alpha',trim(left(put(a,best12.))));
    call symput('beta',trim(left(put(b,best12.))));
  quit;
  run;
  %let cutoff=&nullprob;
%end;

ods select all;

data _null_;
  set _conv;
  call symput('status',put(status,1.0));
run;

%let titfont=%str(h=18 pt f='Arial' color=black);

%if &status=0 %then %do;
  data &est;
    set &est(drop=DF);
  %if %upcase(&print)=YES %then %do;
    title &titfont "&title.: Raw Data Summary";
    proc print data=_summary label noobs;
    run;
	options formdlim=' ';
    title &titfont "&title.: Simple Binomial Model";
    proc print data=&binest label noobs;
	  var parameter estimate stderr tvalue probt Alpha Lower Upper;
    run;
    %if &method=BB
      %then %do;
        title &titfont "&title.: Beta-Binomial Model Parameters";
		/* remove last row and col from the var-covar matrix & tidy up a bit */
		data &cov;
          set &cov(drop=row cov6);
	      if _n_<6;
	      label cov1='mu' cov2='alpha' cov3='beta' cov4='gamma' cov5='theta';
		run;
      %end; %else %do;
        title &titfont "&title.: Corrected Beta-Binomial Model Parameters (c=&nullprob)";
		data &cov;
          set &cov(drop=row);
	      label cov1='mu' cov2='alpha' cov3='beta' cov4='gamma' cov5='pi';
		run;
      %end;
	%if %sysevalf(&gamma<1E-6,boolean)
      %then title2 'Note: Gamma is close to zero, alpha & beta tend to infinity, solution collapses to simple binomial';
	  %else %if %sysevalf(&gamma>0.999999,boolean)
        %then title2 'Note: Gamma is close to one, alpha & beta tend to zero, estimates below are unreliable';;
    proc print data=&est label noobs;
	  label label='parameter';
	run;
	title &titfont "&title.: Variance-Covariance Matrix of Estimated Parameters";
	title2;
    proc print data=&cov label noobs;
	run;
  %end;

  %if %sysprod(graph)=1 %then %do;
    /* make a graph of the beta distribution if SAS/GRAPH is installed */
    data _betadist;
      do p=0.000001, 0.00001, 0.0001, 0.001, 0.01,
           0.025 to 0.975 by 0.025,
           0.99, 0.999, 0.9999, 0.99999, 0.999999;
        d=exp(lgamma(&alpha+&beta)-lgamma(&alpha)-lgamma(&beta)+
          (&alpha-1)*log(p)+(&beta-1)*log(1-p))/(1-&cutoff);
	    q=&cutoff+p*(1-&cutoff);
	    if d<8 then output;
      end;
      drop p;
    run;

    goptions ftext='Arial' hsize=12 cm  vsize=12 cm;
    %if %upcase(&method=BB)
      %then title &titfont "Beta Distribution for the Binomial Proportion";
      %else title &titfont "Corrected Beta Distribution";;

    symbol1 interpol=join color=black;
    proc gplot data=_betadist;
        plot d * q   / frame overlay
                       vaxis=axis2 haxis=axis1
                       vminor=1 hminor=1
                       name = 'betabin' nolegend
           description="Beta-Distribution for the Binomial parameter";
        axis1
            color=black
            order=(0 to 1 by 0.1)
            value=(a=0 r=0 h=12 pt)
            label=(h=16 pt "Proportion of Successes");
        axis2
            color=black
            value=(a=0 r=0 h=12 pt)
            label=(h=16 pt a=90 "Probability Density");
        symbol v=none;
    run;
    quit;
  %end;
%end; %else
  %put ERROR in Macro BetaBin: terminating as PROC NLMIXED failed to converge;

%bbexit:

proc datasets library=work nolist nodetails;
  %if %sysfunc(exist(_summary)) %then delete _summary;;
  %if %sysfunc(exist(_nobs)) %then delete _nobs;;
  %if %sysfunc(exist(_conv)) %then delete _conv;;
  %if %sysfunc(exist(_cov)) %then delete _cov;;
  %if %sysfunc(exist(_binest)) %then delete _binest;;
  %if %sysfunc(exist(_est)) %then delete _est;;
  %if %sysfunc(exist(_betadist)) %then delete _betadist;;
quit;

options formdlim='';

%mend;


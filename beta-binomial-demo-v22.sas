/*
   Demonstration SAS program for the BETABIN macro.

   To run this software you need a current license for the SAS modules
   SAS/BASE and SAS/STAT. SAS/GRAPH is optional.

   In the first example below, there are results from a replicated paired-
   preference test where 48 consumers each evaluate the pair (A,B) four
   times. We need to set up a SAS data set with two variables. The first is
   'n', the number of pairs (or trials) per consumer, this is the same for
   all consumers so we need the statement 'n=4' in the data step. The second
   variable 'x' is the number of pairs that each consumer gets correct or
   successes for short. Values for 'x' are read in from the data lines
   following the cards statement. Note that a success is defined as preference
   for product (A) from the pair (A,B).

   From the output we can be see that the simple binomial model indicates that
   58.33% of the time product A is preferred, and that this is significantly
   different (p=0.0202) from the chance value of 50.00%. However the Beta
   Binomial model has a gamma estimate of 0.4196 that is significantly
   different from zero (p<.0001) indicating that there is extra-binomial
   variation present introduced by differences in the way that consumers are
   performing the task. A consequence is that the standard error for mu is now
   inflated (0.0531 vs 0.0356 for the simple binomial), to the extent that
   we can no longer reject the null hypothesis (p=0.1203) that mu is different
   from the probability of guessing correctely (nullprob=0.5).


   Important, change the path name below to the directory on your computer
   where you have saved the macro file.
*/
%let path=\\cms-storage\cms\Share\guohai\Adams_Snijders_reliability;
%include "&path\betabin-v22.sas";

/*
   EXAMPLE 1
*/
data paircomp;
  n=4;   /* four reps for all consumers */
  input x @@;
  cards;
  1 4 2 4 0 3 0 3 4 1 3 0 1 4 2 3 4 1 4 2 0 0 4 3
  1 2 0 4 1 4 1 3 3 1 3 4 4 0 3 4 0 2 4 3 3 4 4 1
  ;
run;
proc print data=paircomp; run;

%betabin(data=paircomp,title=Paired-Pref,ntrials=n,nsucc=x,nullprob=0.5);


/*
   EXAMPLE 2
   This triangle test data is "Hunter Experiment 3" analysed
   by Brockhoff (2003)
*/
data triangle;
  input x @@;
  n=12;  /* 12 reps for all consumers */
  cards;
    2 2 3 4 5 5 5 5 6 6 6 7 7 7 7 7 7 8 8 8 9 11 12
;
run;

/*
   Run an ordinary beta-binomial model to reproduce the
   published results alpha=5.34 and beta=4.66.
   Note that as this is a triangle test it is important
   to set the parameter nullprob to one third as this is
   the probability of guessing the correct answer. 
*/
%betabin(data=triangle,title=Hunter 3,ntrials=n,nsucc=x,
         nullprob=0.3333);

/*
   The betabin macro also has the facility to fit the corrected beta-
   binomial model when "method=cbb" is added to the parameter list.  In
   the output the macro gives alpha=0.9297 beta=2.1681 which are close
   to the published values of 0.932 and 2.17 respectively.  The
   discrepency is since Brockhoff uses an approximation to the
   probability function while the macro is using an exact formulation
   due to Bi (2004).
*/
%betabin(data=triangle,title=Hunter 3,ntrials=n,nsucc=x,
         nullprob=0.3333,method=cbb);

/*
   References

   Jian Bi (2004) "Probability Distribution Function of the Corrected
   Beta-Binomial Model", personal communication.

   Per Brockhoff (2003) "The statistical power of replications in
   difference tests" Food Quality & Preference, vol 14, 405-417

*/

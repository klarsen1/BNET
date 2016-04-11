
%macro gainschart(data=,y=,score=,out=,groups=20);

   %local TempData TempOut TempScored MPRINTopt NOTESopt;
   %if %sysfunc(exist(&data))=0 %then %do;
       %put ERROR: File &data does not exist;
       data _null_;
          abort;
       run;
   %end;
   %let dataid=%sysfunc(open(&data,i));
   %if %sysfunc(varnum(&dataid,&score))=0 %then %do;
       %put ERROR: Score variable %upcase(&score) does not exist;
       %put;
       %let rc=%sysfunc(close(&dataid));
       data _null_;
         abort;
       run;
   %end;
   %else %if %sysfunc(varnum(&dataid,&y))=0 %then %do;
       %put ERROR: Y variable %upcase(&y) does not exist;
       %put;
       %let rc=%sysfunc(close(&dataid));
       data _null_;
         abort;
       run;
   %end;
   %else %let rc=%sysfunc(close(&dataid));

   %let TempData=work.r%sysfunc(round(10000000*%sysfunc(ranuni(0))));
   %let TempOut=&TempData.O;
   %let TempScored=&TempData.S;

   %let MPRINTopt=%OptionFlag(MPRINT);

   %if (&MPRINTopt eq 1) %then %do;
      options nomprint;
   %end;

   %let NOTESopt=%OptionFlag(NOTES);

   %if (&NOTESopt eq 1) %then %do;
      options nonotes;
   %end;

   %SaveTitles(&TempData);

   title1 "GAINSCHART Macro ";
   title2 "Y=%upcase(&y), SCORE=%upcase(&score)";  

   proc rank data=&data groups=&groups out=&TempScored descending; 
      var &score; 
      ranks rank; 
   run; 

   proc sql; 
      create table &TempOut as
      select rank+1 as rank,
             sum(&y) as &y, 
             count(*) as n, 
             (select sum(&y) from &TempScored) as total_&y, 
           (select count(*) from &TempScored) as total_n
      from &TempScored
      group by rank 
      order by rank;
   quit; 

   data &TempOut;
      set &TempOut; 
      if _n_=1 then do; 
         depth=0; 
         concentration=0; 
         output;
      end; 
      sum&y+&y; 
      sumn+n; 
      depth=sumn/total_n; 
      hitrate=&y/n; 
      concentration=sum&y/total_&y;
      overall_hitrate=total_&y/total_n; 
      lift=hitrate/overall_hitrate-1; 
      format lift depth concentration hitrate overall_hitrate percent10.2; 
      label depth='Depth'
            concentration='Concentration'
          hitrate='Hit rate'
          overall_hitrate='Overall hitrate'
            lift = 'Lift'
             r_&score = "Rank";  
      output; 
   run; 

   proc sql; 
      create table &out as
      select rank, n, depth, concentration, hitrate, overall_hitrate, lift
      from &TempOut; 
   quit; 

   proc print data=&out noobs label; 
      where hitrate>.; 
      var rank depth concentration hitrate lift; 
   run;  

   proc sgplot data=&out;
      series X=depth Y=depth;
      series X=depth Y=concentration;
   run; 
   proc sgplot data=&out;
      vbar rank / response=lift;
   run; 
 

   %if (&MPRINTopt eq 1) %then %do;
      options mprint;
   %end;

   %if (&NOTESopt eq 1) %then %do;
      options notes;
   %end;

   %ResetTitles(&TempData);
   proc delete data=&TempOut &TempScored &TempData; 
   run; 
%mend; 

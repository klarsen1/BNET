%macro incremental(data=,y=,by=,treatment=,out=,revenue=); 

   %local TempData MPRINTopt NOTESopt DATEopt NUMBERopt;
  
   ***********************************************************
   *  Check the input dataset                                *
   *********************************************************;
   %if %sysfunc(exist(&data))=0 %then %do;
       %put ERROR: File &data does not exist;
       data _null_;
          abort;
       run;
   %end;

   ***********************************************************
   *  Check input variables                                  *
   *********************************************************;
   %let dataid=%sysfunc(open(&data,i));
   %if %sysfunc(varnum(&dataid,&y))=0 %then %do;
      %put ERROR: Target variable &y does not exist;
      %put;
      %let rc=%sysfunc(close(&dataid)); 
      data _null_;
        abort;
      run;
   %end;

   %if %sysfunc(varnum(&dataid,&treatment))=0 %then %do;
      %put ERROR: Treatment variable &treatment does not exist;
      %put;
      %let rc=%sysfunc(close(&dataid));
      data _null_;
        abort;
      run;
   %end;

   %if %bquote(&by) ne  %then %do; 
      %if %sysfunc(varnum(&dataid,&by))=0 %then %do;
         %put ERROR: By variable %bquote(&by) does not exist;
         %put;
         %let rc=%sysfunc(close(&dataid));
         data _null_;
           abort;
         run;
      %end;
   %end; 

   %if %bquote(&revenue) ne  %then %do; 
      %if %sysfunc(varnum(&dataid,&revenue))=0 %then %do;
         %put ERROR: REVENUE variable %upcase(&revenue) does not exist;
         %put;
         %let rc=%sysfunc(close(&dataid));
         data _null_;
           abort;
         run;
      %end;
   %end; 

   %let rc=%sysfunc(close(&dataid));

   %let TempData=work.r%sysfunc(round(10000000*%sysfunc(ranuni(0))));

   %let MPRINTopt=%OptionFlag(MPRINT);

   %if (&MPRINTopt eq 1) %then %do;
      options nomprint;
   %end;

   %let NOTESopt=%OptionFlag(NOTES);

   %if (&NOTESopt eq 1) %then %do;
      options nonotes;
   %end;

   %let DATEopt=%OptionFlag(DATE);

   %if (&DATEopt eq 1) %then %do;
      options nodate;
   %end;

   %let NUMBERopt=%OptionFlag(NUMBER);

   %if (&NUMBERopt eq 1) %then %do;
      options nonumber;
   %end;

   %SaveTitles(&TempData);

   title1 "INCREMENTAL Macro";
   title2 "Y=%upcase(%bquote(&y)) TREATMENT=%upcase(%bquote(&treatment))";

   proc sort data=&data(keep=&y) out=levels nodupkey;
      by &y;
   run; 

   proc sql noprint;
      select count(*) into :levels
      from levels;
      select min(&y) into :miny
      from levels;
      select max(&y) into :maxy
      from levels;
   quit;

   proc delete data=levels;
   run; 

   data _null_;
      if &levels <= 1 or &levels>2 then do;
         put "ERROR: Target variable can only have 2 levels ";
         abort;
      end;
      if &maxy ne 1 or &miny ne 0 then do;
         put "ERROR: Target variable must be either 0 or 1 ";
         abort;
      end;
      put "  ";
   run;

   proc sort data=&data(keep=&treatment) out=_levels_ nodupkey;
      by &treatment;
   run; 

   proc sql noprint;
      select count(*) into :levels
      from _levels_;
      select min(&treatment) into :mint
      from _levels_;
      select max(&treatment) into :maxt
      from _levels_;
   quit;

   proc delete data=_levels_;
   run; 

   data _null_;
      if &levels <= 1 or &levels>2 then do;
         put "ERROR: Treatment variable can only have 2 levels ";
         abort;
      end;
      if &maxt ne 1 or &mint ne 0 then do;
         put "ERROR: Treatment variable must be either 0 or 1 ";
         abort;
      end;
      put "  ";
   run;

   data _base_; 
      set &data (keep=&treatment &by &y &revenue); 
   run; 

   %if %bquote(&by) ne %then %do; 
      proc sort data=_base_; 
         by &by; 
      run;  
   %end; 

   proc freq data=_base_ noprint;
      %if %bquote(&by) ne %then %do; 
        by &by; 
     %end; 
      tables &treatment * &y / out=_count_ chisq;  
      output out=_chisq_ (keep=p_pchi &by) pchi;
   run; 

   %if %bquote(&revenue) ne %then %do; 
   proc sql; 
      create table _revenue_ as
     select 
      %if %bquote(&by) ne  %then %do; 
        &by,
     %end; 
      mean(&revenue) as _revenue_
     from _base_
     %if %bquote(&by) ne  %then %do; 
        group by &by
       order by &by;
     %end; 
   quit; 
   %end; 

   %if %bquote(&revenue) ne  and %bquote(&by)= %then %do;
      data _chisq_; 
         merge _chisq_ _revenue_; 
     run; 
   %end; 

   data _output_;
     %if %bquote(&by) ne  %then %do;
         merge _count_ _chisq_ (keep=p_pchi &by) 
         %if %bquote(&revenue) ne %then %do;
            _revenue_
         %end;; 
        by &by; 
     %end;
      %else %do; 
         if _n_=1 then do; 
            set _chisq_;
         end; 
       set _count_ end=eof; 
     %end; 
     test_group_d+count*(&treatment=1); 
     test_group_n+count*(&treatment=1)*(&y=1);
     cntl_group_d+count*(&treatment=0); 
     cntl_group_n+count*(&treatment=0)*(&y=1);
     n+count;
     %if %bquote(&by) ne  %then %do;
        if last.&by then do; 
     %end; 
     %else %do; 
        if eof then do; 
     %end; 
        test_group=test_group_n/test_group_d; 
        cntl_group=cntl_group_n/cntl_group_d;
         incremental_rate=test_group-cntl_group; 
       incremental_purchases=round(test_group_n - cntl_group_n*test_group_d/cntl_group_d);
         %if %bquote(&revenue) ne  %then %do; 
          incremental_revenue=incremental_rate*_revenue_; 
          format incremental_revenue dollar10.2;
       %end; 
       format test_group cntl_group incremental_rate 
         p_pchi percent9.2;
         format incremental_purchases n comma8.0;  
       label test_group = "Test group"
             cntl_group = "Control group"
               incremental_rate = "Incremental rate"
               incremental_purchases = "Incremental purchases"
            %if %bquote(&revenue) ne  %then %do;
               incremental_revenue="Incremental revenue"
            %end; 
               p_pchi = "P-value";
       output; 
       test_group_d=0; 
       test_group_n=0;
       cntl_group_d=0; 
       cntl_group_n=0;
       n=0;
     end; 
   run; 

   proc print data=_output_ noobs label;
      var 
      %if %bquote(&by) ne  %then %do; 
         &by 
      %end;  
      n test_group cntl_group incremental: p_pchi; 
     sum n; 
   run; 

   %if %bquote(&out) ne %then %do; 

      data &out; 
         set _output_; 
        keep %if %bquote(&by) ne  %then %do; 
                &by 
              %end;  
              test_group cntl_group incremental: 
               p_pchi; 
       run; 

   %end; 

   %ResetTitles(&TempData);

   proc delete data=_count_ &TempData
   %if %bquote(&revenue) ne  %then %do; 
      _revenue_ 
   %end; 
      _output_ _base_ _chisq_; 
   run;

   %if (&MPRINTopt eq 1) %then %do;
      options mprint;
   %end;

   %if (&NOTESopt eq 1) %then %do;
      options notes;
   %end;

   %if (&DATEopt eq 1) %then %do;
      options date;
   %end;

   %if (&NUMBERopt eq 1) %then %do;
      options number;
   %end;
 
%mend; 


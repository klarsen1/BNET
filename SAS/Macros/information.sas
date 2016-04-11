/*----  To Do: Move error checking to the top  ----*/
%macro information(var=ALL, data=, y=, printt=n, printg=n, output=, missing=y,
                   treatment=, crossvaldata=, scale=10, globalbins=10, summary=);

   %local TempData MPRINTopt NOTESopt DATEopt NUMBERopt;
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

   ***********************************************************
   *  WOE macro                                              *
   *********************************************************;

   %macro woe(type,val);
        proc summary %if &val=0 %then %do;
                      data=
                          %if &type=1 %then %do;
                               _woe_
                          %end;
                          %else %do;
                               _base_
                          %end;
                     %end;
                     %else %do;
                          data=__val__
                     %end;;
            var set1: set0: %if &type=1 %then %do;
                               &&var&i
                            %end;;
            %if &type=1 %then %do;
               class r_&&var&i
            %end;
            %else %do;
               class &&var&i
            %end;
            %if %upcase(&missing)=Y %then %do;
               / missing
            %end;;
            output out=_woe_
            %if %bquote(&treatment) ne  %then %do;
               sum(set1_t)=set1_t
               sum(set0_t)=set0_t
               sum(set1_c)=set1_c
               sum(set0_c)=set0_c
            %end;
            %else %do;
               sum(set1)=set1
               sum(set0)=set0
            %end;
            %if &type=1 %then %do;
               min(&&var&i)=start
               max(&&var&i)=end
            mean(&&var&i)=m&&var&i 
            %end;;
         run;
         data __woe__;
            if _n_=1 then set _woe_
            (where=(_type_=0) rename=(%if %bquote(&treatment) ne  %then %do;
                                         set1_t=totalSet1_t
                                         set0_t=totalSet0_t
                                         set1_c=totalSet1_c
                                         set0_c=totalSet0_c
                                      %end;
                                      %else %do;
                                         set1=totalSet1
                                         set0=totalSet0
                                      %end;
                                      _freq_=total)
                                      %if &type=1 %then %do;
                                         drop=start end
                                      %end;);
            length %if &type=1 %then %do;
                       range $25
                   %end; variable $&maxlength;
            set _woe_ (where=(_type_=1)) end=eof;
            %if %bquote(&treatment) ne  %then %do;
               percSet1_t=set1_t/totalSet1_t;
               percSet0_t=set0_t/totalSet0_t;
               percSet1_c=set1_c/totalSet1_c;
               percSet0_c=set0_c/totalSet0_c;
               if min(percSet1_t,percSet0_t)>0 then woe_t=log(percSet1_t/percSet0_t);
               if min(percSet1_c,percSet0_c)>0 then woe_c=log(percSet1_c/percSet0_c);
               if nmiss(woe_t,woe_c)=0 then nwoe=woe_t-woe_c;
               weight=&scale*(percSet1_t*percSet0_c-percSet0_t*percSet1_c);
               gweight=percSet1_t-percSet0_t;
            %end;
            %else %do;
               percSet1=set1/totalSet1;
               percSet0=set0/totalSet0;
               if min(percSet1,percSet0)>0 then woe=log(percSet1/percSet0);
               weight=percSet1-percSet0;
               gweight=weight;
            %end;
             variable=upcase(compress("&&var&i"));
        perc=_freq_/total;
        rename _freq_=n;
        %if %bquote(&treatment) ne %then %do;
           n_t=sum(set1_t,set0_t);
           n_c=sum(set1_c,set0_c);
        %end;
        %if &type=1 %then %do;
        select;
               when(start=.) range=".";
           otherwise
              range=trim(left(round(start,0.001)))
            || " - " ||trim(left(round(end,0.001)));
        end;
        %end;
        label %if &type=1 %then %do;
                  range="Range"
              %end;
              %else %do;
                  &&var&i="&label"
              %end;
              %if %bquote(&treatment) ne %then %do;
                 n_t="Obs (test)"
                 n_c="Obs (ctrl)"
                 woe_t="WOE (train/test)"
                 woe_c="WOE (train/ctrl)"
                 nwoe="NWOE (train)"
              %end;
              %else %do;
                 woe="WOE"
                 percset1='% Y=1'
                 percset0='% Y=0'
             set1="Y=1"
             set0="Y=0"
              %end;
              variable="Variable"
              perc='Percent'
              _freq_='Obs'
              weight='Net weight'
              gweight='Gross weight';
        format perc: percent10.2;
     run;
     proc sql;
        create table %if &val=0 %then %do;
                           &&var&i
                     %end;
                     %else %do;
                           &&var&i.._V
                     %end; as
            select variable, %if &type=1 %then %do;
                       range, r_&&var&i, m&&var&i
                    %end;
                    %else %do;
                       &&var&i
                    %end; ,
            %if %bquote(&treatment) ne %then %do;
               n, n_t, n_c, woe_t, woe_c, nwoe,
            %end;
            %else %do;
               n, set1, set0, percset1, percset0, woe,
            %end; weight, gweight, perc
            from __woe__;
     quit;

     proc delete data=_woe_ __woe__;
     run;
   %mend;

   ***********************************************************
   *  IV macro                                               *
   *********************************************************;

   %macro iv;
         data &&var&i _iv_
         %if %bquote(&treatment) ne %then %do;
           (keep=variable cumNIV cumGIV cumPenalty
            rename=(cumNiv=niv cumGiv=giv cumPenalty=penalty))
         %end;
         %else %do;
           (keep=variable cumIV cumPenalty
            rename=(cumiv=iv cumPenalty=penalty))
         %end;;
            set &&var&i end=eof;
            %if %bquote(&treatment) ne %then %do;
               giv=gweight*woe_t;
               penalty=abs(nwoe-sum(nwoe_v,0))*abs(weight);
               niv=weight*nwoe;
               cumNiv+niv;
               cumGiv+giv;
            %end;
            %else %do;
               penalty=abs(woe-sum(woe_v,0))*abs(weight);
               iv=weight*woe;
               cumIV+iv;
            %end;
            cumPenalty+penalty;
            label %if %bquote(&treatment) ne %then %do;
                     cumNIV="Cumulative NIV"
                     niv="NIV"
                     cumGIV="Cumulative GIV"
                     giv="GIV"
                     woe_t="WOE (train/test)"
                     nwoe_v="NWOE (xval)"
                  %end;
                  %else %do;
                     cumIV="Cumulative IV"
                     iv="IV"
                     woe="WOE (test)"
                     woe_v="WOE (xval)"
                  %end;
                  penalty="Penalty"
                  cumPenalty="Cum Penalty";
            output &&var&i;
            if eof then output _iv_;
         run;
   %mend;



   ***********************************************************
   *  Set up the output library                              *
   *********************************************************;
   data _null_;
      save=(compress("&output") ne "");
      call symputx("save",save);
   run;
   %if &save=1 %then %do;
   data _null_;
      if %sysfunc(libname(_out_,&output)) notin (0,-70004) then do;
      put "ERROR: Output folder does not exist";
      abort;
      end;
      else do;
          put "Output will be saved in &output";
      end;
   run;
   %end;
   %put;

   ***********************************************************
   *  Read the inputs                                        *
   *********************************************************;
   %let numInputs=0;
   %put DEPENDENT VARIABLE: &y;
   %put;
   %if %upcase(%scan(&var,1)) ne ALL %then %do;
      %do i=1 %to 100;
         %if %scan(&var,&i) ne %then %do;
            %let numInputs=%eval(&numInputs+1);
            %let input&numInputs=%scan(&var,&i);
         %end;
      %end;
      %let dim=1;
      %put;
      %put INDEPENDENT VARIABLES:;
      %do i=1 %to %eval(&numInputs-1) %by 2;
         %put %str( ) &&input&i;
      %let var&dim=%upcase(&&input&i);
      %let dim=%eval(&dim+1);
      %end;
      %put;
      %let dim=1;
      %put GROUPING PARAMETERS:;
      %do i=2 %to &numInputs %by 2;
         %put %str( ) &&input&i;
      %let group&dim=&&input&i;
      %let dim=%eval(&dim+1);
      %end;
      %let dim=%eval(&dim-1);
      %put;
   %end;
   %else %do;
      proc contents data=&data
                    out=_dim_(keep=name
                    where=(upcase(name) ne upcase(compress("&y")) and
                           upcase(name) ne upcase(compress("&treatment"))
                           and name notin ('_FREQ_','_TYPE_')))
                    noprint;
      data _null_;
         set _dim_ nobs=dim end=eof;
         if eof then call symput('dim',trim(left(dim)));
      run;
      %put INDEPENDENT VARIABLES:;
      %do i=1 %to &dim;
         data _null_;
            set _dim_;
            name=upcase(name);
            if _n_=&i;
            put "  " name;
            call symput("var&i",trim(left(name)));
         run;
      %end;
      proc delete data=_dim_;
      run;
   %end;

   *********************************************************
   *  Clean up                                             *
   *********************************************************;
   %if %upcase(%bquote(&printt))=YES or %upcase(%bquote(&printt))=Y %then %let printt=Y;
   %if %upcase(%bquote(&printg))=YES or %upcase(%bquote(&printg))=Y %then %let printg=Y;
   %if %upcase(%bquote(&missing))=YES or %upcase(%bquote(&missing))=Y %then %let missing=Y;


   *********************************************************
   *  Check if data exist   NOTE: Out of order             *
   *********************************************************;
   %if %sysfunc(exist(&data))=0 %then %do;
       %put ERROR: File &data does not exist;
      title1; 
      title2; 
       %let rc=%sysfunc(close(&dataid));
       data _null_;
          abort;
       run;
   %end;
   %else %do;
      proc contents data=&data noprint out=nobs(keep=nobs);
      data _null_;
         set nobs;
         call symput('nobs',trim(left(nobs)));
      run;
      proc delete data=nobs;
      run;
      %put;
      %put There were &nobs observations read from the DATASET: &data;
      %put;
   %end;

   ***********************************************************
   *  Check if variables exist                               *
   *********************************************************;
   %let dataid=%sysfunc(open(&data,i));
   %do i=1 %to &dim;
      %if %sysfunc(varnum(&dataid,&&var&i))=0 %then %do;
         %put ERROR: Variable &&var&i does not exist;
         %put;
         %let rc=%sysfunc(close(&dataid));
         data _null_;
           abort;
         run;
      %end;
   %end;
   %if %sysfunc(varnum(&dataid,&y))=0 %then %do;
      %put ERROR: Dependent variable does not exist;
      %put;
      %let rc=%sysfunc(close(&dataid));
      data _null_;
         abort;
      run;
   %end;
   %if %bquote(&treatment) ne %then %do;
      %if %sysfunc(varnum(&dataid,&treatment))=0 %then %do;
         %put ERROR: Treatment variable does not exist;
         %put;
         %let rc=%sysfunc(close(&dataid));
         data _null_;
            abort;
         run;
      %end;
   %end;
   %let rc=%sysfunc(close(&dataid));

   ***********************************************************
   *  Check the title                                        *
   *********************************************************;
   %if %bquote(&treatment) ne %then %do;
      title1 "INFORMATION Macro: Y=%upcase(&y), TREATMENT=%upcase(&treatment)";
   %end;
   %else %do;
      title1 "INFORMATION Macro: Y=%upcase(&y)";
   %end;


   ***********************************************************
   *  Get the maximum variable-length                        *
   *********************************************************;
   data _null_;
      length=0;
      %do i=1 %to &dim;
         l=length(compress("&&var&i"));
         if l>length then length=l;
      %end;
      call symput('maxlength',length);
   run;

   ***********************************************************
   *  Count levels of Y                                      *
   *********************************************************;
   proc sql;
      create table _levels as
      select distinct &y
      from &data
      order by &y;
   quit;
   proc sql noprint;
      select count(*) into :levels
      from _levels;
      select min(&y) into :miny
      from _levels;
      select max(&y) into :maxy
      from _levels;
   quit;
   proc delete data=_levels;
   data _null_;
      if &levels <= 1 or &levels>2 then do;
      put "ERROR: Y-variable must have 2 levels ";
      abort;
      end;
      if &maxy ne 1 or &miny ne 0 then do;
         put "ERROR: Y-varible must be between 0 and 1 ";
         abort;
      end;
      put "  ";
   run;


   ***********************************************************
   *  Count levels of Treatment                              *
   *********************************************************;
   %if %bquote(&treatment) ne %then %do;
       proc sql;
          create table _levels as
          select distinct &treatment
          from &data
          order by &treatment;
       quit;
       proc sql noprint;
          select count(*) into :levels
          from _levels;
          select min(&treatment) into :mint
          from _levels;
          select max(&treatment) into :maxt
          from _levels;
       quit;
       proc delete data=_levels;
       data _null_;
          if &levels <= 1 or &levels>2 then do;
          put "ERROR: Treatmenmt variable must have 2 levels ";
          abort;
          end;
          if &maxt ne 1 or &mint ne 0 then do;
             put "ERROR: Treatment variable must be between 0 and 1 ";
             abort;
          end;
          put "  ";
       run;
   %end;

   %if %bquote(&crossvaldata) ne  %then %do;
       %if %sysfunc(exist(&crossvaldata))=0 %then %do;
          %put ERROR: Cross validation dataset does not exist;
          %put;
        title1; 
         title2; 
          data _null_;
             abort;
          run;
       %end;
       %let dataid=%sysfunc(open(&crossvaldata,i));
       %do i=1 %to &dim;
           %if %sysfunc(varnum(&dataid,&&var&i))=0 %then %do;
           %put ERROR: Variable &&var&i does not exist in the cross validation file;
           %put;
         title1; 
          title2; 
           data _null_;
              abort;
           run;
           %end;
       %end;
       %let rc=%sysfunc(close(&dataid));
    %end;


   ***********************************************************
   *  Create a _base_ file                                   *
   *********************************************************;
   data _base_;
      set &data
      (keep=%do i=1 %to &dim;
               &&var&i
            %end; &y &treatment);
            %if %bquote(&treatment) ne %then %do;
               set1_t=(&y=1)*(&treatment=1);
               set1_c=(&y=1)*(&treatment=0);
               set0_t=(&y=0)*(&treatment=1);
               set0_c=(&y=0)*(&treatment=0);
           %end;
           %else %do;
               set1=(&y=1);
               set0=(&y=0);
           %end;
   run;

   %if %bquote(&crossvaldata)= %then %do;
      proc surveyselect data=&data out=_val_ samprate=0.5 noprint seed=2008;
      run;
   %end;

   data _val_;
      %if %bquote(&crossvaldata) ne  %then %do;
         set &crossvaldata;
      %end;
      %else %do;
         set _val_;
      %end;
      %if %bquote(&treatment) ne %then %do;
         set1_t=(&y=1)*(&treatment=1);
         set1_c=(&y=1)*(&treatment=0);
         set0_t=(&y=0)*(&treatment=1);
         set0_c=(&y=0)*(&treatment=0);
      %end;
      %else %do;
         set1=(&y=1);
         set0=(&y=0);
      %end;
  run;

   ***********************************************************
   *  Create the WOE tables                                  *
   *********************************************************;
   %if %sysfunc(exist(_ivlist_)) %then %do;
      proc delete data=_ivlist_;
      run;
   %end;
   %if %sysfunc(exist(_infotable_)) %then %do;
      proc delete data=_infotable_;
      run;
   %end;
   %do i=1 %to &dim;
      proc contents data=_base_ out=__type__(keep=name type label) noprint;
      data _null_;
         set __type__ (where=(upcase(name)=upcase(compress("&&var&i"))));
         call symput("type&i",trim(left(type)));
         call symput('label',trim(left(label)));
      run;

      proc sql;
         create table _unique_ as
         select distinct &&var&i
         from _base_
         order by &&var&i;
      quit;

      data _null_;
         set _unique_ nobs=n end=eof;
         call symput('levels',trim(left(n)));
      proc delete data=_unique_ __type__;
      run;

      proc sql noprint;
         select (sum(&&var&i is missing)=count(*))  into :allmissing&i
         from _base_;

         select (min(&&var&i)=max(&&var&i))         into :onelevel&i
         from _base_;

         select sum(&&var&i is missing)/count(*)    into :pmiss
         from _base_;
      quit;


      %if %upcase(%scan(&var,1)) = ALL %then %do;
          %if %bquote(&globalbins)= %then %do;
              %if %bquote(&treatment)= %then %do;
                 %let globalbins=10;
              %end;
              %else %do;
                 %let globalbins=10;
              %end;
          %end;
          data _null_;
            if &&type&i=2 then do;
               call symput("group&i",0);
               put "&&var&i treated as a categorical variable (character variable)";
             end;
             else if &&type&i=1 and &levels<5 then do;
                call symput("group&i",0);
                put "&&var&i treated as a categorical variable (unique levels=&levels)";
             end;
             else do;
                call symput("group&i",min(&levels,&globalbins));
                put "&&var&i treated as a continuous variable (unique levels=&levels)";
             end;
          run;
      %end;
      %else %do;
          data _null_;
              if &&type&i=2 and &&group&i>0 then do;
                 put "Variable &&var&i is categorical, but groups>0";
                 put "Groups will be set to 0";
                 call symput("group&i",0);
                 put " ";
              end;
              else if &&type&i=1 then call symput("group&i",min(&levels,&&group&i));
          run;
      %end;

      data _null_;
         rank=(&&group&i>0);
         call symput('rank',rank);
      run;
      data _info_;
         length variable $&maxlength type $4 skipped $3;
         variable=upcase("&&var&i");
         levels=&levels;
         groups=&&group&i;
         if &&type&i=2 then type="Char";
         else type="Num";
         percent_missing=&pmiss;
         if &&allmissing&i=1 or &&onelevel&i=1 then skipped="Yes";
         else skipped="No";
         label variable="Variable"
         levels="Levels"
         type="Type"
         percent_missing='%Missing'
         skipped="Skipped";
      run;

      proc append data=_info_ base=_infotable_;
      proc delete data=_info_;
      run;

      %if &rank and &&allmissing&i=0 and &&onelevel&i=0 %then %do;
         proc rank data=_base_ groups=&&group&i out=_woe_;
            var &&var&i;
            ranks r_&&var&i;
         run;

         proc summary data=_woe_ nway;
            class r_&&var&i / missing;
            var &&var&i;
            output out=_fmt_ max=max;
         run;

         data _fmt_;
            keep start end fmtname eexcl sexcl label hlo;
            set _fmt_ end=eof;
            start=lag(max);
            end=max;
            type='n';
            fmtname='temp';
            eexcl='N';
            sexcl='Y';
            label=r_&&var&i;
            retain firstRowMiss 0;
            if _n_=1 then firstRowMiss=(label=.);
            if _n_=1 and not firstRowMiss then do;
               sexcl='N';
               start=-100000000000;
            end;
            else if _n_=1 then do;
               sexcl='N';
               start=.;
               end=.;
            end;
            if _n_=2 and firstRowMiss then do;
               sexcl='N';
               start=-100000000000;
            end;
            if eof then hlo='O';
        run;

        proc format cntlin=_fmt_;
        proc delete data=_fmt_;
        run;

        data __val__;
            set _val_;
            r_&&var&i=put(&&var&i,temp.)*1;
        run;

        %woe(1,0);
        %woe(1,1);

        data &&var&i;
            merge &&var&i (in=a)
            %if %bquote(&treatment) ne %then %do;
                &&var&i.._V (keep=nwoe r_&&var&i
                rename=(nwoe=nwoe_v));
            %end;
            %else %do;
                &&var&i.._V (keep=woe r_&&var&i
                rename=(woe=woe_v));
            %end;
            by r_&&var&i;
            if a;
        run;
        %iv;
        proc append data=_iv_ base=_ivlist_;
        proc delete data=__val__ _iv_;
        run;
      %end;
      %else %if &&allmissing&i=0 and &&onelevel&i=0 %then %do;
         data __val__;
            set _val_;
         run;
         %woe(2,0);
         %woe(2,1);
         data &&var&i;
            merge &&var&i (in=a)
            %if %bquote(&treatment) ne %then %do;
                &&var&i.._V (keep=nwoe &&var&i
                rename=(nwoe=nwoe_v));
            %end;
            %else %do;
                &&var&i.._V (keep=woe &&var&i
                rename=(woe=woe_v));
            %end;
            by &&var&i;
            if a;
         run;
         %iv;
         proc append data=_iv_ base=_ivlist_;
         proc delete data=__val__ _iv_;
         run;
      %end;
      %else %if &&allmissing&i %then %do;
         %put All values for &&var&i are missing;
         %put Variable &&var&i will be ignored;
         %put;
      %end;
      %else %do;
         %put Variable &&var&i is a constant;
         %put Variable &&var&i will be ignored;
         %put;
      %end;

      %if %upcase(&printt)=Y and &&allmissing&i=0 and &&onelevel&i=0 %then %do;
         proc print data=&&var&i label noobs split='*';
         %if &rank %then %do;
            var range
     %end;
     %else %do;
        var &&var&i
     %end;
     %if %bquote(&treatment) ne %then %do;
        n woe_t woe_c nwoe nwoe_v niv penalty giv;
        sum n niv penalty giv;
     %end;
     %else %do;
        n percset1 percset0 woe woe_v penalty iv;
        sum n iv penalty;
     %end;
     %if %NRBQUOTE(&label)= %then %do;
         title2 "Variable = &&var&i";
     %end;
     %else %do;
         title2 "Variable = &&var&i (%NRBQUOTE(&label))";
     %end;
     run;
     %end;
     %if %upcase(&printg)=Y and &&allmissing&i=0 and &&onelevel&i=0 %then %do;
     %if %bquote(&treatment) ne %then %do;
        proc sql noprint;
           select max(abs(nwoe)) into :ul
           from &&var&i;
         quit;
         data _null_;
           call symputx('ll',&ul*-1);
         run;
       %if &rank=0 %then %do; 
           proc plot data=&&var&i hpercent=90 vpercent=100;
           plot &&var&i*nwoe = '*'
           / box href = 0 haxis = &ll 0 &ul;
           run;
       %end;
        %else %do; 
           proc plot data=&&var&i hpercent=90 vpercent=100;
           plot m&&var&i*nwoe = '*'
           / box href = 0 haxis = &ll 0 &ul;
           run;
        %end;  
     %end;
     %else %do;
        proc sql noprint;
            select max(abs(woe)) into :ul
            from &&var&i;
         quit;
         data _null_;
            call symputx('ll',&ul*-1);
         run;

       %if &rank=0 %then %do; 
           proc plot data=&&var&i hpercent=90 vpercent=100;
           plot &&var&i*woe = '*'
           / box href = 0 haxis = &ll 0 &ul;
           run;
       %end;
        %else %do; 
           proc plot data=&&var&i hpercent=90 vpercent=100;
           plot m&&var&i*woe = '*'
           / box href = 0 haxis = &ll 0 &ul;
           run;
        %end;  
     %end;
     %end;
     %put &&var&i processed;
      data _null_;
         p=100 * &i/&dim;
         _p=round(p,.1)||'%';
         call symputx('p',_p);
      run;
      %put &p of variables processed;
      %put;
      %if &save=1 %then %do;
          %if %sysfunc(exist(&&var&i)) and &&allmissing&i=0 and &&onelevel&i=0 %then %do;
              %if %bquote(&treatment) ne %then %do;
                 proc sql;
                    create table _out_.&&var&i as
                    select variable, %if &rank %then %do;
                                        range, m&&var&i,
                                     %end;
                                     %else %do;
                                        &&var&i,
                                     %end;
                                     n,
                                     perc,
                                     n_t, n_c,
                                     woe_t, woe_c,
                                     weight, gweight,
                                     nwoe, nwoe_v,
                                     penalty, cumPenalty,
                                     giv, cumGiv,
                                     niv, cumNiv
                    from &&var&i;
                 quit;
              %end;
              %else %do;
                 proc sql;
                    create table _out_.&&var&i as
                    select variable, %if &rank %then %do;
                                        range, m&&var&i,
                                     %end;
                                     %else %do;
                                        &&var&i,
                                     %end;
                                     n,
                            set1, set0,
                                     perc,
                                     woe,
                                     gweight,
                                     penalty, cumPenalty,
                                     iv, cumiv
                    from &&var&i;
                 quit;
              %end;
          %end;
      %end;
      %if %sysfunc(exist(&&var&i)) %then %do;
          proc delete data=&&var&i &&var&i.._V;
          run;
      %end;
   %end;

   %if %bquote(&treatment) ne  %then %do;
       proc rank data=_ivlist_ out=_ivlist_ descending;
          var niv giv;
          ranks niv_rank giv_rank;
       run;
   %end;

   data _ivlist_;
      %if %bquote(&treatment) ne %then %do;
          retain variable niv penalty adj_niv giv niv_rank giv_rank;
          set _ivlist_;
          adj_niv=niv-penalty;
          label adj_niv="Adj. NIV"
                penalty="Penalty"
                giv="GIV"
                niv="NIV"
                niv_rank="NIV Rank"
                giv_rank="GIV Rank"
                variable="Variable";
      %end;
      %else %do;
          retain variable adj_iv iv penalty;
          set _ivlist_;
          adj_iv=iv-penalty;
          label iv="IV"
                adj_iv="Adj. IV"
                penalty="Penalty"
                variable="Variable";
      %end;
   run;

   data _infotable_;
      set _infotable_;
      format percent_missing percent8.2;
   proc print data=_infotable_ label noobs;
      title2 "Variable information";
   run;

   proc sort data=_ivlist_;
      by descending %if %bquote(&treatment) ne %then %do;
                       adj_niv;
                    %end;
                    %else %do;
                       adj_iv;
                    %end;
   proc print data=_ivlist_ label noobs;
      title2 "Variable strength (all variables)";
   run;

   %if %bquote(&summary) ne %then %do ; 
       data &summary ; 
	       set _ivlist_ ; 
	   run ; 
   %end ; 

   %if &save=1 %then %do;
      data _out_._ivlist_;
         set _ivlist_;
      data _out_.variable_information;
         set _infotable_;
      run;
     libname _out_ clear; 
   %end;
   proc delete data=_infotable_;
   run;

   %ResetTitles(&TempData);

   proc delete data=_base_ _ivlist_ _val_ &TempData;
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


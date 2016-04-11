/*----  Miscellaneous macros to support BNET macros and programs  ----*/

/*----  Check Options                                      ----*/
/*----  This only works for options of the form OPT/NOOPT  ----*/
/*----  Returns 1 if option is set (OPT), otherwise        ----*/
/*----  returns 0 if option is not set (NOOPT).            ----*/
%macro OptionFlag(OptionName);
   %if (%upcase(%sysfunc(getoption(&OptionName))) eq %upcase(&OptionName)) %then %do;
      1
   %end;
   %else %do;
      0
   %end;
%mend OptionFlag;

*-----------------------------------------------------*/
/*----  Get number of observations in a data set.  ----*/
/*----  Returns zero (0) if the dataset does not   ----*/
/*----  exist. Valid in dataset or macro code.     ----*/
/*-----------------------------------------------------*/ ; 
%macro GetNumObs(DSNAME);
   %local DSID RC;
   %if %eval(%sysfunc(exist(&DSName,DATA)) eq 1) or
       %eval(%sysfunc(exist(&DSName,VIEW)) eq 1) %then %do;

      %let DSID=%sysfunc(open(&DSNAME));
      %if (&DSID eq 0) %then %do;
         0
      %end;
      %else %do;
         %sysfunc(attrn(&DSID,NOBS))
         /*----  Must use LET because close has a return code  ----*/
         %let RC=%sysfunc(close(&DSID));
      %end;
   %end;
   %else %do;
      0
   %end;
%mend GetNumObs;

/* Ref: SUGI31
   How Do I Look it Up If I Cannot Spell It: An Introduction to SAS® Dictionary Tables
   Peter Eberhardt, Fernwood Consulting Group Inc, Toronto, ON
   Ilene Brill, University of Alabama at Birmingham, Birmingham, AL
 */
%macro SaveTitles(DSName);
   data &DSName;
      set sashelp.vtitle;
   run;
%mend SaveTitles;

%macro ResetTitles(DSName);
   %local TitleVars NumTitles TitleNum;
   %let NumTitles=%GetNumObs(&DSName);
   %if (%eval(&NumTitles>0) eq 1) %then %do;
   %do TitleNum=1 %to &NumTitles;
      %local title&TitleNum;
   %end;
   data _null_;
      set &DSName end=lastobs;
      length title $20.;
      /*----  A title can be 200 chars, allow 2 extra for the quotes */
      length newtext $202.;
      /*----  Convert the title number to a char string  ----*/
      anum = compress(put(number, 2.));
      /*----  put a single quote at the beginning of the text  ----*/
      newtext = "'" || text;
      /*----  find the end of the text and add another quote  ----*/
      l = length(newtext);
      substr(newtext,l+1,1) = "'";
      /*----  for titles, type = 'T' */
      if (type = "T") then do;
         /*----  first make the string TITLE1 etc  ----*/
         title = "TITLE" || anum;
         /*----  now add the text so we end up with TITLE1 'This is the TEXT'  ----*/
         titletext = title || newtext;
         /*----  and put it out to the symbol table  ----*/
         call symput(title, titletext);
      end;
      if (lastobs) then do;
         call symput("TitleVars", put(_n_, 2.));
      end;
   run;
   %if (&NumTitles ne &TitleVars) %then %do;
      %put ERROR: Title number mismatch in macro ResetTitles.;
   %end;
   %do TitleNum = 1 %to &TitleVars;
      &&title&TitleNum;
   %end;
   %end;
%mend ResetTitles;

* Macros to dummy flag and replace missing values with averages ; 

%macro miss(data=,out=,instat=,outstat=) ; 
   %local TempData TempMap MPRINTopt;

   %let TempData=work.r%sysfunc(round(10000000*%sysfunc(ranuni(0))));
   %let TempMap=&TempData.M;

   %let MPRINTopt=%OptionFlag(MPRINT);

   %if (&MPRINTopt eq 0) %then %do;
      options mprint;
   %end;

   proc summary data=&data ; 
      var _numeric_ ; 
     output out=&TempData (drop=_freq_ _type_) nmiss= ; 
   run ; 

   proc transpose data=&TempData out=&TempData ; 
   run ; 

   proc sql noprint ; 
      select _name_ into :missvars separated by ' '
     from &TempData (where=(col1>0)) ; 

      select count(*) into :nmissvars
     from &TempData (where=(col1>0)) ; 
   quit ;

   data &TempMap ; 
      length dummy $10 ; 
      set &TempData (where=(col1>0)) ; 
     %do i=1 %to &nmissvars ; 
        if _n_=&i then dummy="_MISS_&i" ; 
     %end ;
      rename _name_=variable ;  
   run ; 

   proc print noobs ; 
      var dummy variable ; 
   run ; 

   data &out ; 
      set &data ; 
     array __miss__[&nmissvars] &missvars ;
      array _miss_[&nmissvars] ;  
     do i=1 to &nmissvars ; 
        _miss_[i]=(__miss__[i]=.) ; 
     end ; 
     drop i ; 
   run ; 
 
   %if %bquote(&instat)= %then %do ; 
      proc stdize data=&out out=&out method=mean reponly 
         %if %bquote(outstat) ne %then %do; 
            outstat=&outstat
         %end ; ;
         var _numeric_ ; 
      run ; 
   %end ;
   %else %do ; 
      proc stdize data=&out out=&out method=in(&instat) reponly ; 
         var _numeric_ ; 
      run ; 
   %end ;
 
 
   proc delete data=&TempMap &TempData ; 
   run ; 
 
   %if (&MPRINTopt eq 0) %then %do;
      options nomprint;
   %end;

%mend ; 


* macro to count the number elements in a macro string ; 

%macro nlist(list);
   %let i=0;
   %do %while("%scan(&list,%eval(&i+1))" ne "");
      %let i=%eval(&i+1);
   %end;
   &i
%mend ;

* macro to cap the data at the 99th percentile (for GAM) ; 

%macro cap(basedata=, newdata=, VarList=) ;
   %let MPRINTopt=%OptionFlag(MPRINT);
   %if (&MPRINTopt eq 1) %then %do;
      options nomprint;
   %end;
   %let NOTESopt=%OptionFlag(NOTES);
   %if (&NOTESopt eq 1) %then %do;
      options nonotes;
   %end;

   %if %bquote(&newdata)= %then %do ; 
      %let newdata=&basedata ; 
   %end ;  
   %local NumVar ThisVar Row;
   %let NumVar=%nlist(&VarList);
   %do Row=1 %to &NumVar;
      %let ThisVar=%scan(&VarList,&Row) ;
	     proc summary data=&basedata ; 
	        var &ThisVar ; 
			output out=p p99=p ; 
	     run ; 
		 data _null_ ; 
	        set p ; 
			call symputx('p', p) ; 
	     run ; 
         data &newdata ; 
	         set &newdata ; 
		     if &ThisVar>&p then &ThisVar=&p ;
	     run ; 
   %end ; 
   %if (&MPRINTopt eq 1) %then %do;
      options mprint;
   %end;
   %if (&NOTESopt eq 1) %then %do;
      options notes;
   %end;
%mend ; 


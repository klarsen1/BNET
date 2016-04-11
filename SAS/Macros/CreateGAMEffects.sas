
**** get variable types and return a macro string ; 

options mprint ; 

%macro GetTypes(VarList=, data=); 
   %let MPRINTopt=%OptionFlag(MPRINT);
   %if (&MPRINTopt eq 1) %then %do;
      options nomprint;
   %end;
   %let NOTESopt=%OptionFlag(NOTES);
   %if (&NOTESopt eq 1) %then %do;
      options nonotes;
   %end;

   %global types ; 
   %let types= ; 
   %let NumVar=%nlist(&VarList);
   %do Row=1 %to &NumVar;
      %let ThisVar=%scan(&VarList,&Row);
	  proc sql noprint ; 
	     select count(distinct(&ThisVar)) into :unique
	     from &data ; 
	  quit ; 
	  %if %eval(&unique)>10 %then %do ; 
         %let types = &types 1 ; 
	  %end ;
      %else %do ; 
         %let types = &types 0 ; 
      %end ;  
   %end;
   %if (&MPRINTopt eq 1) %then %do;
      options mprint;
   %end;
   %if (&NOTESopt eq 1) %then %do;
      options notes;
   %end;
%mend ;

**** macro to set up the GLMSELECT model ; 
%macro SetUpGLMSelect(VarList=, TypeList=, DepVar=, knots=);
   %local NumVar ThisVar ThisType Row;
   %let NumVar=%nlist(&VarList);
   %do Row=1 %to &NumVar;
      %let ThisVar=%scan(&VarList,&Row) ;
	  %let ThisType=%scan(&TypeList, &Row) ; 
	  %if &ThisType=1 %then %do ; 
	     %if &knots ne %then %do ;  
            effect spl_&ThisVar = spline(&ThisVar / knotmethod=percentiles(&knots)) ; 
         %end ;  
		 %else %do ; 
            effect spl_&ThisVar = spline(&ThisVar) ; 
		 %end ; 
	  %end ;
   %end ; 
   model &DepVar = 
   %do Row=1 %to &NumVar;
      %let ThisVar=%scan(&VarList,&Row) ;
	  %let ThisType=%scan(&TypeList, &Row) ; 
	  %if &ThisType=1 %then %do ; 
         spl_&ThisVar    
	  %end ;
	  %else %do ; 
	     &ThisVar  
	  %end ; 
   %end ; 
%mend ;

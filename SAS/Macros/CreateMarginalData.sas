%macro CreateMarginalData(data=, var=, out=, VarList=) ; 
   %local NumVar ThisVar Row;
   %let NumVar=%nlist(&VarList);
   proc sql ; 
      create table &out as
	  select  
   %do Row=1 %to &NumVar;
      %let ThisVar=%scan(&VarList,&Row) ;
	  %if %upcase(&ThisVar) ne %upcase(&var) %then %do ; 
      mean(&ThisVar) as &ThisVar, 
	  %end ; 
   %end ; 
   &var as &var
   from &data
   quit ; 

proc sort data=&out nodupkey ; 
   by &var ; 
run ; 

%mend ; 


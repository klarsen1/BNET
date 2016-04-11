data train; 
   drop purchase treatment unique_id ; 
   set bnet.train (in=a) bnet.valid (in=b) ;
   if a then validation=1 ; 
   else validation=0 ; 
   select ; 
      when(purchase=1 & treatment=1) class=1 ; 
      when(purchase=0 & treatment=1) class=2 ; 
      when(purchase=1 & treatment=0) class=3 ; 
      when(purchase=0 & treatment=0) class=4 ; 
   end ; 
run; 

proc contents data=train out=names (keep=name) noprint ; 
run ; 

proc sql noprint ;
   select name into :names separated by " "
   from names (where=(upcase(name) ne "CLASS")) ; 
quit ;  


**** grow the tree ; 
proc hpsplit data=train maxdepth=7 maxbranch=2 ;
   target class ;
   input &names ; 
   prune entropy / N <= 10;
   partition ROLEVAR=validation(TRAIN=0 VALIDATE=1) ;
   code file="&root\tree.sas" ; 
run;

**** validate ; 
data scored ; 
   set bnet.test ; 
   %include "&root\tree.sas" ;
   net_score=p_class1/(p_class1+p_class2)-p_class3/(p_class3+p_class4) ; 
   revenue=25 ; 
run ; 

proc rank data=scored out=scored groups=10; 
   var net_score; 
   ranks rank; 
run; 

%incremental(data=scored, by=rank, treatment=treatment, y=purchase, revenue=revenue)

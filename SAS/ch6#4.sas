**** set up the data ;
** we need both test and control records for all datasets ; 
data train; 
   set bnet.train ; 
run; 

data test ; 
   set bnet.test ; 
run ; 

data valid ; 
   set bnet.valid ; 
run ; 


**** first, eliminate variables with weak NIVs ; 
%information(
data=train,
crossvaldata=valid,
printt=y,
y=purchase,
summary=nivs,
treatment=treatment)

proc sql noprint ; 
   select variable into :vars7 separated by ' '
   from nivs (where=(adj_niv>0.07)) ; 
quit ; 


**** cluster to remove highly correlated variables ; 
%clusteriv(data=train, ivs=nivs, vars=&vars7, metric=adj_niv, out=clusters, maxeigen=0.7) 

proc sql noprint  ;
   select variable into :vars8 separated by ' '
   from clusters ; 
quit ;


**** prepare for KNN ; 
data train ; 
   set train ; 
   select ; 
      when(purchase=1 & treatment=1) class=1 ; 
      when(purchase=0 & treatment=1) class=2 ; 
      when(purchase=1 & treatment=0) class=3 ; 
      when(purchase=0 & treatment=0) class=4 ; 
   end ; 
run ; 

proc stdize data=train method=std outstat=stats out=train ;
   var &vars8 ; 
run ; 

proc stdize data=test method=in(stats) out=test ;
   var &vars8 ; 
run ; 


**** run the KNN classifier ; 
proc discrim data=train testdata=test testout=scored noprint
   method=npar k=100 out=train ; 
   class class ; 
   priors equal ;
   var &vars8  ; 
run ;


**** validate ; 
data scored ; 
   set scored ; 
   net_score=_1/(_1+_2)-_3/(_3+_4) ; 
run ;  

proc rank data=scored out=scored groups=10 ; 
   var net_score ;
   ranks rank ;  
run ; 

%incremental(data=scored, y=purchase, treatment=treatment, by=rank)  


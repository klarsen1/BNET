**** eliminate variables with weak NIVs ; 
%information(
data=bnet.train,
crossvaldata=bnet.valid,
printt=y,
y=purchase,
summary=nivs, 
treatment=treatment)

proc sql noprint ; 
   select variable into :vars7 separated by ' '
   from nivs (where=(adj_niv>0.02)) ; 
quit ; 

%clusteriv(data=bnet.train, ivs=nivs, vars=&vars7, metric=adj_niv, out=clusters, maxeigen=1) 

proc sql noprint  ;
   select variable into :vars8 separated by ' '
   from clusters ; 
quit ;


**** prepare for KNN ; 
data train ; 
   set bnet.train ;
   select ; 
      when(purchase=1 & treatment=1) class=1 ; 
      when(purchase=0 & treatment=1) class=2 ; 
      when(purchase=1 & treatment=0) class=3 ; 
      when(purchase=0 & treatment=0) class=4 ; 
   end ; 
run ; 

data valid ; 
   set bnet.valid ;
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

proc stdize data=valid method=in(stats) out=valid ;
   var &vars8 ; 
run ; 

proc discrim data=train testdata=valid testout=valid2 noprint
   method=npar k=100 out=train2 ; 
   class class ; 
   priors equal ;
   var &vars8  ; 
run ;

**** create an artifical dependent variable; 
data train ; 
   merge train2 (keep=_1 _2 _3 _4 unique_id) bnet.train ; 
   by unique_id ; 
   net_score=_1/(_1+_2)-_3/(_3+_4) ; 
   if net_score>0.05 then d_swing=1 ; 
   else if net_score<0 then d_swing=0 ;
   else delete ; 
run ; 

data valid ; 
   merge valid2 (keep=_1 _2 _3 _4 unique_id) bnet.valid ; 
   by unique_id ; 
   net_score=_1/(_1+_2)-_3/(_3+_4) ; 
   if net_score>0.05 then d_swing=1 ; 
   else if net_score<0 then d_swing=0 ;
   else delete ; 
run ; 

proc freq data=train ; 
    tables d_swing ; 
run ; 

data test ; 
   set bnet.test ; 
run ; 


**** fit a GAM to the artifical dependent variable; 
%GetTypes(VarList=&vars8, data=train) 
%cap(basedata=train, newdata=test, VarList=&vars8) ;
%cap(basedata=train, newdata=valid, VarList=&vars8) ;
%cap(basedata=train, VarList=&vars8) ;

%CreateMarginalData(data=train, var=n_open_rev_acts, VarList=&vars8, out=fake)

proc glmselect data=train valdata=valid ; 
   %SetUpGLMSelect(VarList=&vars8, TypeList=&types, DepVar=d_swing)  
   / selection=elasticnet(steps=120 choose=validate) ;
   score data=fake p=p_swing out=fake ; 
   score data=test p=p_swing out=scored ; 
run;

**** validate ;
proc rank data=scored out=scored groups=10; 
   var p_swing; 
   ranks rank; 
run; 

data scored ; 
   set scored ; 
   revenue=25 ; 
run ; 

%incremental(data=scored, by=rank, treatment=treatment, y=purchase, revenue=revenue)

**** plot marginal function for N_OPEN_REV_ACTS ; 
proc sgplot data=fake ;
   yaxis display=(nolabel);
   series    x=n_open_rev_acts y=p_swing / lineattrs=(color=red);
run;
 

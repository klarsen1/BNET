**** fit the self-selection model: P(purchase | control) ;
** follow the model fitting process from 4.1 ; 
data train; 
   set bnet.train (where=(not treatment)); 
run; 

data valid; 
   set bnet.valid (where=(not treatment)); 
run; 

data test ; 
   set bnet.test ; * this is for net lift validation and hence contains both test and control records ; 
run ; 

%information
(data=train,
 crossvaldata=valid,
 printt=n,
 y=purchase,
 summary=ivs) ;  

proc sql noprint  ;
   select variable into :vars5 separated by ' '
   from ivs (where=(adj_iv ge 0.05)) ; 
quit ;

%clusteriv(data=train, ivs=ivs, vars=&vars5, metric=adj_iv, out=clusters, maxeigen=1) 

proc sql noprint  ;
   select variable into :vars6 separated by ' '
   from clusters ; 
quit ;

%GetTypes(VarList=&vars6, data=train) 
%cap(basedata=train, newdata=test, VarList=&vars6) ;
%cap(basedata=train, newdata=valid, VarList=&vars6) ;
%cap(basedata=train, VarList=&vars6) ;

proc glmselect data=train valdata=valid ; 
   %SetUpGLMSelect(VarList=&vars6, TypeList=&types, DepVar=purchase)  
   / selection=elasticnet(steps=120 choose=validate) ;
   score data=test p=p2 out=scored ; 
run;

**** combine and validate ;
data scored ; 
   merge bnet.propensity_scores (rename=(p_purchase=p1)) scored ; 
   by unique_id ; 
   p_net = p1 - p2 ;
   revenue=125 ; 
run ; 

proc rank data=scored out=scored groups=10; 
   var p_net; 
   ranks rank; 
run; 

%incremental(data=scored, by=rank, treatment=treatment, y=purchase, revenue=revenue)


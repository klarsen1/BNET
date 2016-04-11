
**** fit the secondary model for the PDM: P(treatment | purchase) ;
** Follow the model fitting process from 4.1 ; 
data train; 
   set bnet.train (where=(purchase)); 
run; 

data valid; 
   set bnet.valid (where=(purchase)); 
run; 

data test ; 
   set bnet.test ; * this is for net lift validation and hence contains both test and control records ;  
run ; 

%information
(data=train,
 crossvaldata=valid,
 printt=n,
 y=treatment,
 summary=ivs) ;  

proc sql noprint  ;
   select variable into :vars3 separated by ' '
   from ivs (where=(adj_iv ge 0.02)) ; 
quit ;

%GetTypes(VarList=&vars3, data=train) 
%cap(basedata=train, newdata=test, VarList=&vars3) ;
%cap(basedata=train, newdata=valid, VarList=&vars3) ;
%cap(basedata=train, VarList=&vars3) ;

proc glmselect data=train valdata=valid ; 
   %SetUpGLMSelect(VarList=&vars3, TypeList=&types, DepVar=treatment)  
   / selection=elasticnet(steps=120 choose=validate) ;
   score data=test p=p2 out=scored2 ; 
run;

**** combine and validate ;
data scored ; 
   merge bnet.propensity_scores (rename=(p_purchase=p1)) scored2 ; 
   by unique_id ; 
   p_net = p1*(2-1/p2) ;
   revenue=125 ; 
run ; 

proc rank data=scored out=scored groups=10; 
   var p_net; 
   ranks rank; 
run; 

%incremental(data=scored, by=rank, treatment=treatment, y=purchase, revenue=revenue)

**** set up the datasets (exclude the control group) ; 
data train; 
   set bnet.train (where=(treatment)); 
run; 

data valid; 
   set bnet.valid (where=(treatment)); 
run; 

data test ; 
   set bnet.test ; 
run ; 

**** run the information macro to screen out the weakest variables ; 
%information
(data=train,
 crossvaldata=valid,
 printt=n,
 y=purchase,
 summary=ivs) ;  

proc sql noprint  ;
   select variable into :vars1 separated by ' '
   from ivs (where=(adj_iv ge 0.05)) ; 
quit ;

**** variable clustering to remove highly correlated variables ; 
%clusteriv(data=train, ivs=ivs, vars=&vars1, metric=adj_iv, out=clusters, maxeigen=1) 

proc sql noprint  ;
   select variable into :vars2 separated by ' '
   from clusters ; 
quit ;

**** get the variable types ; 
%GetTypes(VarList=&vars2, data=train) 

**** cap the data ; 
%cap(basedata=train, newdata=test, VarList=&vars2) ;
%cap(basedata=train, newdata=valid, VarList=&vars2) ;
%cap(basedata=train, VarList=&vars2) ;

**** Create a marginal dataset to visualize marginal impacts ; 
%CreateMarginalData(data=train, var=n_open_rev_acts, VarList=&vars2, out=fake)

**** fit the penalized pseudo-GAM ; 
proc glmselect data=train valdata=valid ; 
   %SetUpGLMSelect(VarList=&vars2, TypeList=&types, DepVar=purchase)  
   / selection=elasticnet(steps=120 choose=validate) ;
   score data=fake p=p_purchase out=fake ; 
   score data=test out=bnet.propensity_scores (keep=purchase treatment unique_id p_purchase) p=p_purchase; 
run;

**** validate the model ; 
data scores ; 
   set bnet.propensity_scores (where=(treatment)) ; 
run ; 

%gainschart(data=scores, score=p_purchase, groups=10, y=purchase, out=gains) 

**** just to get the c statistic -- NOT fitting a model ; 
proc logistic data=scores ; 
   model purchase = p_purchase ; 
run ; 

*** plot the marginal effect for a N_OPEN_REV_ACTS ; 
proc sgplot data=fake ;
   yaxis display=(nolabel);
   series    x=n_open_rev_acts y=p_purchase / lineattrs=(color=red);
run;

%let vars3=N_OPEN_REV_ACTS N_OF_SATISFY_BNKREV_ACTS PREM_BANKCARD_CRED_LMT M_SNC_MSTREC_INSTL_TRD_OPN
D_M_SNC_MSTREC_INSTL_TRD_OPN D_DEPTCARD AGRGT_BAL_ALL_XCLD_MRTG D_UPSCALE_OPEN_DATE_YRS ; 

%let vars2=N_OPEN_REV_ACTS RATIO_RETAIL_BAL2HI_CRDT D_CA AVG_BAL_ALL_FNC_REV_ACTS N_DISPUTED_ACTS
N_OF_SATISFY_FNC_REV_ACTS D_RATIO_BAL_TO_HI_CRDT RATIO_BAL_TO_HI_CRDT N_BC_ACTS_OPN_IN_24M
TOT_HI_CRDT_CRDT_LMT M_SNC_MST_RCNT_ACT_OPN M_SNC_MSTREC_INSTL_TRD_OPN
M_SNC_OLDST_RETAIL_ACT_OPN ; 

**** marginal dataset for both test and control. will be used to produce a marginal impact graph for N_OPEN_REV_ACTS  ; 
data fake ; 
   drop treatment purchase ; 
   set bnet.train ; 
run ; 

proc contents data=fake out=names (keep=name) noprint ; 
run ; 

proc sql noprint ; 
   select name into :names separated by ' '
   from names; 
quit ; 

%cap(basedata=fake, VarList=&names)

%CreateMarginalData(data=fake, var=n_open_rev_acts, VarList=&names, out=fake) ;


**** re-fit the propensity model ; 

data train ; 
   set bnet.train (where=(treatment)) ; 
run ; 

data valid ; 
   set bnet.valid (where=(treatment)) ; 
run ; 


%GetTypes(VarList=&vars2, data=train) 

%cap(basedata=train, newdata=valid, VarList=&vars2) ;
%cap(basedata=train, VarList=&vars2) ;

proc glmselect data=train valdata=valid ; 
   %SetUpGLMSelect(VarList=&vars2, TypeList=&types, DepVar=purchase)  
   / selection=elasticnet(steps=120 choose=validate) ;
   score data=fake p=p1 out=fake1 ; 
run;


**** re-fit the secondary model -- P(treatment | purchase) ; 

data train ; 
   set bnet.train (where=(purchase)) ; 
run ; 

data valid ; 
   set bnet.train (where=(purchase)) ; 
run ; 

%GetTypes(VarList=&vars3, data=train) 

%cap(basedata=train, newdata=valid, VarList=&vars3) ;
%cap(basedata=train, VarList=&vars3) ;

proc glmselect data=train valdata=valid ; 
   %SetUpGLMSelect(VarList=&vars3, TypeList=&types, DepVar=treatment)  
   / selection=elasticnet(steps=120 choose=validate) ;
   score data=fake p=p2 out=fake2 ; 
run;


**** plot marginal impact graph for N_OPEN_REV_ACTS ;
data fake_all ; 
  merge fake1 (in=a) fake2 (in=b) ;
  by n_open_rev_acts ;  
  if a and b ; 
  p_net = p1*(2-1/p2) ; 
run ; 

proc sgplot data=fake_all ;
   yaxis display=(nolabel);
   series    x=n_open_rev_acts y=p_net / lineattrs=(color=red);
run;

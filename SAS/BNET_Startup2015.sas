/*----  BNET Start Code  ----*/

/*----  In revision: copy BNET_Revised to BNET  ----*/
/*----  Rename this file to BNET_Startup.sas    ----*/
%let root=C:\Users\stormy\Documents\BNET\BNET3.0;  

%include "&root\Macros\information.sas" ; 
%include "&root\Macros\gainschart.sas" ; 
%include "&root\Macros\incremental.sas" ;
%include "&root\Macros\miscellaneous.sas" ;
%include "&root\Macros\CreateGAMEffects.sas" ;
%include "&root\Macros\CreateMarginalData.sas" ;  
%include "&root\Macros\clusteriv.sas" ; 


libname bnet "&root\Data" ; 
 
title1 "Net Lift Modeling";


data train ; 
   set bnet.train (where=(treatment)) ; 
run ; 

data valid ; 
   set bnet.valid (where=(treatment)) ; 
run ; 
   
%information(data=train,
             crossvaldata=valid,
             y=purchase, 
             printt=y) 

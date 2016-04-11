
data all ; 
   set bnet.train bnet.valid bnet.test ; 
   revenue=125 ; 
run ; 

%incremental(
	data=all,
	revenue=revenue,
	TREATMENT=treatment,
	Y=purchase)

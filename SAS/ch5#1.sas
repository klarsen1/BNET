
data scores ; 
  set bnet.propensity_scores ; 
  revenue=125 ;
run ; 

proc rank data=scores 
  out=scores groups=10 ; 
  var p_purchase ; 
  ranks rank ; 
run ; 

%incremental(data=scores, y=purchase, by=rank, treatment=treatment, revenue=revenue)



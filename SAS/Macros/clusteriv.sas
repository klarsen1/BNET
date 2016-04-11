%macro clusteriv(data=, ivs=, vars=, metric=, out=, maxeigen=) ; 

   %let MPRINTopt=%OptionFlag(MPRINT);

   %if (&MPRINTopt eq 0) %then %do;
      options mprint;
   %end;

   ods listing close ; 
   ods output clustersummary=summary rsquare(match_all)=clusters; 
   proc varclus data=&data (keep=&vars) maxeigen=&maxeigen ; 
      var &vars ; 
   run ; 
   ods listing ; 

   data _null_ ; 
      set summary end=eof ; 
      if eof then call symputx('nclusters',cluster-2) ; 
   run ; 

   proc contents data=&ivs out=length (where=(name="variable")) noprint ; 
   run ; 

   data _null_ ; 
      set length ; 
      call symputx('length',length) ; 
   run ; 

   data clustermap (keep=cluster_full variable) ; 
      length Variable $ &length ; 
      set clusters&nclusters ; 
      retain cluster_full ; 
      if cluster ne '' then cluster_full=cluster ; 
      variable=upcase(variable) ; 
   run ; 

   proc sql ; 
      create table &out as
      select a.variable, 
             a.cluster_full as cluster, 
		     b.&metric, 
		     max(b.&metric) as max&metric
      from clustermap as a inner join &ivs as b
      on a.variable=b.variable 
      group by a.cluster_full
      having b.&metric=calculated max&metric 
      order by cluster, b.&metric ;
   quit ;
 
   %if (&MPRINTopt eq 0) %then %do;
      options nomprint;
   %end;

%mend ; 

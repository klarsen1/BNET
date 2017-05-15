t <- filter(train, TREATMENT==1)
v <- filter(valid, TREATMENT==1)


### Run the WOE analysis
WOE <- Information::create_infotables(data=t, valid=v, y="PURCHASE", bins=10)

### Look at some output
S <- WOE$Summary
WOE$Tables$REGION
WOE$Tables$N_OPEN_REV_ACTS
WOE$Tables$PRCNT_OF_ACTS_NEVER_DLQNT

Information::plot_infotables(WOE, subset(S, AdjIV>0.6)$Variable)
Information::plot_infotables(WOE, "N_OPEN_REV_ACTS")
alldata <- bind_rows(test, train, valid)

### Calculate the net lift
NetLiftCurve(alldata, "PURCHASE", "TREATMENT")


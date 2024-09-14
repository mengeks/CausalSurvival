jasa$subject <- 1:nrow(jasa)

tdata <- with(
  jasa, 
  data.frame(
    subject = subject,
    futime= pmax(.5, fu.date - accept.dt),
    txtime= ifelse(tx.date == fu.date,
                   (tx.date -accept.dt) -.5,
                   (tx.date - accept.dt)),
    fustat = fustat
  ))
xdata <- tmerge(jasa, tdata, id=subject,
                  death = event(futime, fustat),
                  transplant = tdc(txtime),
                  options= list(idname="subject"))
sdata <- tmerge(jasa, tdata, id=subject,
                  death = event(futime, fustat),
                  trt = tdc(txtime),
                  options= list(idname="subject"))
attr(sdata, "tcount")


sdata$age <- sdata$age -48
sdata$year <- as.numeric(sdata$accept.dt - as.Date("1967-10-01"))/365.25

coxph(formula = Surv(tstart, tstop, death) ~ 
        # age * trt + surgery + year, 
        trt,
      data = sdata, 
      ties = "breslow")

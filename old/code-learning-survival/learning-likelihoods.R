library(survival)

# Synthetic data in Table 4.1
tt<-c(6,7,10,15,19,25)
delta<-c(1,0,1,1,0,1)
trt<-c(0,0,1,0,1,1)
survdiff(Surv(tt,delta)~trt)

# Example 5.1
plsimple <- function(beta) { 
  psi <- exp(beta) 
  result <- log(psi) - 
    log(3*psi + 3)-log(3*psi + 1) - log(2*psi + 1) 
  result 
}
result<- optim(
  par=0,
  fn=plsimple,
  method="L-BFGS-B",
  control=list(fnscale=-1),
  lower=-3,
  upper=1
)

# 
result.cox<-coxph(Surv(tt,delta)~trt)
summary(result.cox)

basehaz(result.cox, centered=F)

library(numDeriv)
grad(func=plsimple,x=0)
hessian(func=plsimple,x=0)






### 1.1 
library(lme4)
clothing <- read.table(file = 'clothingFullAss03.csv', sep = ",",header=TRUE)
no.persons <- max(clothing$subjId)
## Test result 
fit0 <- lmer(clo~sex+(1|subjId),data=clothing,REML=FALSE)
summary(fit0)
#function to optimize
nll.0 <- function(theta,dat,X) { 
  
  params <- X %*% t(t(theta[1:2]))
  sigma <- exp(theta[3])
  sigma.u <- exp(theta[4])
  
  L = 0 
  
  # loop over subjects
  for(i in 0:no.persons){
    
    y_i <- dat$clo[dat$subjId==i]
    params_i <- params[dat$subjId==i]
    n_i <- length(params_i)
    ones <- matrix(1,n_i,n_i)
    V_i <- diag(n_i)*sigma+ones*sigma.u
    
    likelihood = log(1/((2*pi)^(n_i/2)*sqrt(det(V_i)))*exp(-0.5*t(y_i-params_i)%*%solve(V_i)%*%(y_i-params_i)))
    L = L - likelihood 
  }
  
  # output Likelihood  
  L 
}

X <- model.matrix(fit0)
theta0 <- c(0.5, -0.1, 0.1, 0.1)
fit.nll.0 <- nlminb(theta0, nll.0, dat = clothing, X = X)

#COMPARING
print(c(fit.nll.0$objective,logLik(fit0)))


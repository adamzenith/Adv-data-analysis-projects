## Advanced Data Analysis and Statistical Modelling 
## Assignment 3 
## part 2

require(plotrix)
library(lme4)

setwd('/Users/idabukhvillesen/Dropbox/8 SEMESTER/Adv. Data Ana. & Stat. Mod./Assignments/Assignment 3')

clothing <- read.table(file = 'clothingFullAss03.csv', sep = ",",header=TRUE)
no.persons <- max(clothing$subjId)

#####################
### 1.1 

## Test result 
fit0 <- lmer(clo~sex+(1|subjId),data=clothing,REML=FALSE)
summary(fit0)
  
nll <- function(theta,dat,X) { 
  
    mu <- X %*% t(t(theta[1:2]))
    sigma <- exp(theta[3])
    sigma.u <- exp(theta[4])
    
    L = 0 
    
    # loop over subjects
    for(i in 0:no.persons){
      
      y_i <- dat$clo[dat$subjId==i]
      mu_i <- mu[dat$subjId==i]
      n_i <- length(mu_i)
      ones <- matrix(1,n_i,n_i)
      V_i <- diag(n_i)*sigma+ones*sigma.u
    
      likelihood = log(1/((2*pi)^(n_i/2)*sqrt(det(V_i)))*exp(-0.5*t(y_i-mu_i)%*%solve(V_i)%*%(y_i-mu_i)))
      L = L - likelihood 
    
    }
    # output Likelihood  
    L 
}

X <- model.matrix(fit0)
theta0 <- c(0.5, -0.1, 0.1, 0.1)
fit <- nlminb(theta0, nll, dat = clothing, X = X)

fit
sqrt(exp(fit$par[3:4]))
fit0
#####################
### 1.2


## Test result 
fit1 <- lmer(clo~sex+(1|subjId)+(1|subjId:day),data=dat,REML=FALSE)


#####################
### 1.3




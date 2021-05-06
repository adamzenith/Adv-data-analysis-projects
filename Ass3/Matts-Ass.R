clothing = read.csv("clothingFullAss03.csv", header=TRUE, na.strings="?")

summary(clo)

#Plotting female stuff----
par(mfrow=c(1,2))
plot(clo$clo[clo$subjId==0 & clo$sex=="female"],ylim=c(0,1))
for(i in 1:46){
  points(clo$clo[clo$subjId==i & clo$sex=="female"],col=i+1)
}

#Plotting male stuff----
plot(clo$clo[clo$subjId==4 & clo$sex=="male"],ylim=c(0,1))
for(i in 1:46){
  points(clo$clo[clo$subjId==i & clo$sex=="male"],col=i+1)
}


#Aggregate clo by subjId and Gender----
means=aggregate(clo ~ subjId + s+, data=clo,FUN=mean)


#linear mixed effect model----
library(nlme)
fit<-lme(clo~tOut+tInOp, random = ~1|sex, data=clo)
summary(fit)
plot(fit)



#Part 2-------
library(mvtnorm)
no.persons <- max(clothing$subjId)

fit0 <- lmer(clo~sex+(1|subjId),data=clothing,REML=FALSE)
summary(fit0)

nll <- function(theta,dat,X) { 
  
  mu <- X %*% t(t(theta[1:2]))
  sigma <- exp(theta[3])
  sigma.u <- exp(theta[4])
  
  L = 0 
  
  # loop over subjects
  for(i in 0:no.persons){
    
    y_i <- dat$clo[dat$subjId==i & dat$da]
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



## Advanced Data Analysis and Statistical Modelling 
## Assignment 3 
## part 2

require(plotrix)
library(lme4)
library(magic)

setwd('/Users/idabukhvillesen/Dropbox/8 SEMESTER/Adv. Data Ana. & Stat. Mod./Assignments/Assignment 3')

clothing <- read.table(file = 'clothingFullAss03.csv', sep = ",",header=TRUE)
no.persons <- max(clothing$subjId)

#####################
### 1.1 

## Test result 
fit0 <- lmer(clo~sex+(1|subjId),data=clothing,REML=FALSE)
summary(fit0)
  
nll.0 <- function(theta,dat,X) { 
  
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
fit.nll.0 <- nlminb(theta0, nll.0, dat = clothing, X = X)

fit.nll.0
sqrt(exp(fit.nll.0$par[3:4]))
fit0



#####################
### 1.2

## Test result 
fit1 <- lmer(clo~sex+(1|subjId)+(1|subjId:day),data=clothing,REML=FALSE)



nll.1 <- function(theta,dat,X) { 
  
  mu <- X %*% t(t(theta[1:2]))
  sigma <- exp(theta[3])
  sigma.u <- exp(theta[4])
  sigma.v <- exp(theta[5])
  
  L = 0 
  
  # loop over subjects
  for(i in 0:no.persons){ 
    
    y_i <- dat$clo[dat$subjId==i]
    mu_i <- mu[dat$subjId==i]
    n_i <- length(mu_i)
    
    ones <- matrix(1,n_i,n_i)
    V_i <- diag(n_i)*sigma+ones*sigma.u
    
    max.day <- max(dat$day[dat$subjId==i]) #max no of days
    
    for(j in 1:max.day){
      if (j == 1){
        n <- length(dat$day[dat$day==j & dat$subjId==i])
        ma <- matrix(1,n,n)
      }
      else{
        n <- length(dat$day[dat$day==j & dat$subjId==i])
        ones.2 <- matrix(1,n,n)
        ma <- adiag(ma,ones.2)
      }
    }

    V_i <- V_i + ma*sigma.v
    
    likelihood = log(1/((2*pi)^(n_i/2)*sqrt(det(V_i)))*exp(-0.5*t(y_i-mu_i)%*%solve(V_i)%*%(y_i-mu_i)))
    L = L - likelihood 
    
  }
  
  # output Likelihood  
  L 
}

X <- model.matrix(fit1)
theta1 <- c(0.5, -0.1, 0.1, 0.1, 0.1)
fit.nll.1 <- nlminb(theta1, nll.1, dat = clothing, X = X)

fit.nll.1
sqrt(exp(fit.nll.1$par[3:5]))
fit1


#####################
### 1.3


nll.3 <- function(theta,dat,X) { 
  
  mu <- X %*% t(t(theta[1:2]))
  sigma <- exp(theta[3])
  sigma.u <- exp(theta[4])
  sigma.v <- exp(theta[5])
  alpha.f <- theta[6]
  alpha.m <- theta[7]
  
  L = 0 
  
  # loop over subjects
  for(i in 0:no.persons){ 
    
    y_i <- dat$clo[dat$subjId==i]
    mu_i <- mu[dat$subjId==i]
    n_i <- length(mu_i)
    
    if(dat$sex[dat$subjId==i][1]=="female"){
      alpha <- alpha.f
    }
    else{
      alpha <- alpha.m
    }
    
    ones <- matrix(1,n_i,n_i)
    V_i <- diag(n_i)*sigma*alpha+ones*sigma.u*alpha
    
    max.day <- max(dat$day[dat$subjId==i]) #max no of days
    
    for(j in 1:max.day){
      if (j == 1){
        n <- length(dat$day[dat$day==j & dat$subjId==i])
        ma <- matrix(1,n,n)
      }
      else{
        n <- length(dat$day[dat$day==j & dat$subjId==i])
        ones.2 <- matrix(1,n,n)
        ma <- adiag(ma,ones.2)
      }
    }
    
    V_i <- V_i + ma*sigma.v*alpha
    
    likelihood = log(1/((2*pi)^(n_i/2)*sqrt(det(V_i)))*exp(-0.5*t(y_i-mu_i)%*%solve(V_i)%*%(y_i-mu_i)))
    L = L - likelihood 
    
  }
  
  # output Likelihood  
  L 
}

X <- model.matrix(fit1)
theta3 <- c(0.5, -0.1, 0.1, 0.1, 0.1,0.5,0.5)
fit.nll.3 <- nlminb(theta3, nll.3, dat = clothing, X = X)

fit.nll.3
sqrt(exp(fit.nll.3$par[3:5]))
fit.nll.3$par[6:7]
fit1



#####################
### 1.6
sexId <- c(1)
for(i in 2:max(clothing$subjId)){
  if(clothing$sex[clothing$subjId==i]=="female"){
    sexId <- append(sexId, 1)
  }
  else{
    sexId <- append(sexId, 0)
  }
}

## Implement in TMB
library(TMB)                          

setwd("/Users/idabukhvillesen/Documents/GitHub/Adv-data-analysis-projects/Ass3")

compile("clothing.cpp")
#dyn.load(dynlib("beetle"))
parameters <- list(Id = rep(1, no.persons), ## noticfe random effects also parameters
                   sexID = sexId,
                   mu = c(0,0),
                   sigma = 1,
                   sigmau = 1,
                   sigmav = 1,
                   alphaf = 0.5,
                   alpham = 0.5
)


## Define objective function
obj <- MakeADFun(data = clothing,
                 parameters = parameters,
                 random = c("Id"),
                 DLL = "clothing"
)


## Optimize using 1. and second derivative 
system.time(opt <- nlminb(obj$par, obj$fn, obj$gr))
opt$par
opt$objective



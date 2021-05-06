clo = read.csv("clothingFullAss03.csv", header=TRUE, na.strings="?")

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




library(lme4)
fit0 <- lmer(clo~sex+(1|subjId),data=clo,REML=FALSE)
logLik(fit0)
X <- model.matrix(fit0)

f <- function(beta, u) {
  (beta[1] + rep(u[1:5], each = 7))/
  (1 + exp((beta[2] - time)/beta[3])) }

l <- function(u, beta, sigma, sigma.u) {
  -sum(dnorm(x = circumference, mean = f(beta, u),
             sd = sigma, log = TRUE)) -
  sum(dnorm(x = u[1:5], sd = sigma.u, log = TRUE)) }

l.LA <- function(theta) {
  beta <- theta[1:3]
  sigma <- exp(theta[4])
  sigma.u <- exp(theta[5])
  est <- nlminb(start = rep(0,5), objective = l, beta = beta,
                sigma = sigma, sigma.u = sigma.u)
  u <- est$par
  l.u <- est$objective
  Jac.f <- jacobian(func = f, x = u, beta = beta)
  H <- crossprod(Jac.f)/sigma^2 + diag(1/sigma.u^2, 5)
  l.u + 1/2 * log(det(H/(2 * pi))) }.


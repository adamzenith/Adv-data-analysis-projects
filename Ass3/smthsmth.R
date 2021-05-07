# Assignment 3 - Adv. Data Analysis and Statistical Modelling
library(lme4)
library(readr)
library(xtable)
library(plyr)
library(corrplot)
library(nlme)
library(gclus)
library(stats)
library(emmeans)

setwd("~/Documents/Studie/Mst-Sem2-2021/Advanced Data Analysis and Stastitical Modelling/Assignment 3")

Clothing = read.csv('clothingFullAss03.csv')

## Exploratory Analysis #### 

# The subject is measured 6 times a day for 3 days.

par(mfrow=c(1,2))
with(Clothing, {
  interaction.plot(subjId, day, clo, legend=FALSE, 
                   bty="n", col=2:11, xtick = TRUE)
  interaction.plot(sex, day, clo, legend=FALSE, 
                   bty="n", col=2:11, xtick = TRUE)})
par(mfrow=c(1,1))


par(mfrow=c(1,3))
boxplot(Clothing$clo ~Clothing$sex, col=2:6, xlab = "Sex", ylab = "Clothing Level")
boxplot(Clothing$clo ~Clothing$day, col=2:6, xlab = "Day", ylab = "Clothing Level")
boxplot(Clothing$clo ~Clothing$subjId, col=2:6, xlab = "SubjId", ylab = "Clothing Level")

par(mfrow=c(1,2))
boxplot(Clothing$tInOp ~Clothing$day, col=2:6, xlab = "Day", ylab = "Temp. Indoor")
boxplot(Clothing$tOut ~Clothing$day, col=2:6, xlab = "Day", ylab = "Temp. Outdoor")

par(mfrow=c(1,1))
# DENSITY PLOT OF CLOTHING  
clothing = Clothing$clo
# Density plot of Clo: 
f<-function(x){dnorm(x,mean=mean(clothing,na.rm=T),
                     sd=sd(clothing,na.rm=T))} 
hist(clothing,xlab='Clothing',probability=T)
curve(f,0,1,lwd=3,col="red",add=T) 
rm(f)



## Mixed Effect Model that use subjId as a random effect ####

#weights

#

Clothing$sex = factor(Clothing$sex)
Clothing$day = factor(Clothing$day)
par(mfrow=c(2,2))
model0 <- lme(clo ~ tOut*tInOp*sex+day, random= ~ 1|subjId, data=Clothing, method="REML")
logLik(model0)
anova(model0)
summary(model0)

model1 <- lme(clo ~ tOut*tInOp+ tOut*sex+tInOp*sex+tOut*day+tInOp*day, random= ~ 1|subjId, data=Clothing, method="REML")
logLik(model1)
anova(model1)
summary(model1)
plot(model1)

model2 <- lme(clo ~ tOut+tInOp+ tOut*sex+tInOp*sex+tOut*day+tInOp*day, random= ~ 1|subjId, data=Clothing, method="REML")
logLik(model2)
anova(model2)
summary(model2)
plot(model2)

model3 <- lme(clo ~ tOut*sex+tOut*day, random= ~ 1|subjId, data=Clothing, method="REML")#,weights = ~var)
logLik(model3)
anova(model3)
summary(model3)
plot(model3)
# add weights as their is dispersion in the end of the residuals


varmale=var(Clothing$clo[Clothing$sex=="male"])
varfemale=var(Clothing$clo[Clothing$sex=="female"])
Clothing$var<-Clothing$clo
for(i in 1:length(Clothing$var)){
  if(Clothing$sex[i]=="male"){
    Clothing$var[i] = varmale
  }
  else{
    Clothing$var[i] = varfemale
  }
}


Clothing$var<-Clothing$clo
for(i in 1:length(Clothing$var)){
  Clothing$var[i]=var(Clothing$clo[Clothing$subjId==Clothing$subjId[i]])
}
means=aggregate(var ~ subjId, data=Clothing,FUN=mean)

Clothing$var<-Clothing$clo
for(i in 1:length(Clothing$var)){
  if(Clothing$sex[i]=="male"){
    Clothing$var[i] = var(Clothing$clo[Clothing$day==Clothing$day[i] & Clothing$sex=="male"])
  }
  else{
    Clothing$var[i] = var(Clothing$clo[Clothing$day==Clothing$day[i] & Clothing$sex=="female"])
  }
}




optimize = function(vars){
  for(i in 1:length(Clothing$var)){
    Clothing$var[i]=means$var[means$subjId==Clothing$subjId[i]]
  }
  print(Clothing$var)
  ########################### Insert model here
  model3 <- lme(clo ~ tOut*sex+tOut*day, random= ~ 1|subjId, data=Clothing, method="REML",weights = ~1/var)
  ###########################
  -logLik(model3)
}
varr=means$var
opt = optim(varr,optimize)
## Mixed Effect Model that 

model3 <- lme(clo ~ tInOp*sex+ tOut*sex+tOut*tInOp, random= ~ 1|subjId/day, data=Clothing, method="REML")
logLik(model3)
anova(model3)
summary(model3)
plot(model3)
# add weights as their is dispersion in the end of the residuals

model4 <- lme(clo ~ tInOp*sex+ tOut*sex, random= ~ 1|subjId/day, data=Clothing, method="REML")
logLik(model4)
anova(model4)
summary(model4)
plot(model4)


model5 <- lme(clo ~ tInOp*sex+ tOut, random= ~ 1|subjId/day, data=Clothing, method="REML",weights=~var)
logLik(model5)
anova(model5)
summary(model5)
plot(model5)

## Q.4 - model including within day auto-correlation 
# (repeated measurement set up), with subDay as random effect, you should only 
# consider random intercepts in these models.



fit.exp<- lme(clo ~ tInOp+sex+ tOut,random=~1|subDay,
              correlation=corExp(form=~1|subDay),
              data=Clothing,
              method="ML")
logLik(fit.exp)

plot(Variogram(fit.exp), main='Exp')

plot(fit.exp)
qqnorm(fit.exp)


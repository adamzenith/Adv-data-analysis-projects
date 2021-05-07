## Advanced Data Analysis and Statistical Modelling 
## Assignment 3 
## part 1

require(plotrix)
library(nlme)
library(car)

#setwd('/Users/idabukhvillesen/Dropbox/8 SEMESTER/Adv. Data Ana. & Stat. Mod./Assignments/Assignment 3')
setwd("~/Documents/GitHub/Adv-data-analysis-projects/Ass3") #Helena directory

clothing <- read.table(file = 'clothingFullAss03.csv', sep = ",",header=TRUE)

# Data presentation ----
# Plot colors 
colors=clothing$sex
colors[clothing$sex=='female']="red"
colors[clothing$sex=='male']="blue"

no.persons <- max(clothing$subjId)
persons <- c(0:no.persons)
clo.mean <- rep(0,no.persons)
clo.std <- rep(0,no.persons)
clo.sex <- rep(0,no.persons)

for(i in 0:no.persons){
  clo.mean[i+1] <- mean(clothing$clo[clothing$subjId==i])
  clo.std[i+1] <- sd(clothing$clo[clothing$subjId==i])
  
  if (clothing$sex[clothing$subjId==i] == 'female'){
    clo.sex[i+1] <- 1
  }
}

plotCI(persons[clo.sex==1],clo.mean[clo.sex==1],uiw=clo.std[clo.sex==1],liw= clo.std[clo.sex==1], col="lightpink1",xlab="Subject ID",ylab="Clothing",main="Average clothing for the Subjects")
plotCI(persons[clo.sex==0],clo.mean[clo.sex==0],uiw=clo.std[clo.sex==0],liw= clo.std[clo.sex==0], col="lightskyblue1",add=TRUE)
legend("topright", legend = c("Female","Male"),col=c("lightpink1","cadetblue1"), pch=c(1,1),cex=0.85)





# Fit mixed effects models that use subjId as a random effect ----


lme.1 <- lme(clo ~ factor(sex), random = ~1|subjId, data=clothing)
summary(lme.1)
Anova(lme.1, type='III')
plot(lme.1)

lme.2 <- lme(clo ~ factor(sex) + factor(day), random = ~1|subjId, data=clothing)
summary(lme.2)
Anova(lme.2, type='III')
plot(lme.2)

lme.3 <- lme(clo ~ factor(sex) + factor(day) + time, random = ~1|subjId, data=clothing)
summary(lme.3)
Anova(lme.3, type='III')
plot(lme.3)

anova(lme.2,lme.3)

lme.4 <- lme(clo ~ factor(sex) + factor(day) + time + tOut, random = ~1|subjId, data=clothing)
summary(lme.4)
Anova(lme.4, type='III')
plot(lme.4)

lme.5 <- lme(clo ~ factor(sex) + factor(day) + time + tOut + tInOp, random = ~1|subjId, data=clothing)
summary(lme.5)
Anova(lme.5, type='III')
plot(lme.5)

lme.6 <- lme(clo ~ factor(sex) + factor(day) + time + tOut*factor(day) + tInOp, random = ~1|subjId, data=clothing)
summary(lme.6)
Anova(lme.6, type='III')
plot(lme.6)

lme.7 <- lme(clo ~ factor(sex) + factor(day) + time + tOut*factor(day) + tInOp*factor(sex), random = ~1|subjId, data=clothing)
summary(lme.7)
Anova(lme.7, type='III')
plot(lme.7)

lme.8 <- lme(clo ~  time + tOut*factor(day) + tInOp*factor(sex), random = ~1|subjId, data=clothing)
summary(lme.8)
Anova(lme.8, type='III')
plot(lme.8)
# it can be seen that weights should be added.

# Add Weights

clothing$var<-clothing$clo
for(i in 1:length(clothing$var)){
  clothing$var[i]=var(clothing$clo[clothing$subjId==clothing$subjId[i]])
}

# FINAL MODEL
lme.9 <- lme(clo ~    time + tOut*factor(day) + tInOp*factor(sex), random = ~1|subjId,
             weights = ~var, data=clothing)
summary(lme.9)
Anova(lme.9,type ='III')
plot(lme.9)
# the residuals still look very strange - quite varying +-2.



# Fit mixed effects models that use subjId and day as a random effect ----

lme.10 <- lme(clo ~ time+tOut+ tInOp + factor(sex), random = ~ 1|subjId/day,weights = ~var,
              data=clothing)
summary(lme.10)
Anova(lme.10, type = 'III')
plot(lme.10)
# remove tInOp

lme.11 <- lme(clo ~time+ tOut + factor(sex), random = ~ 1|subjId/day,weights = ~var,
              data=clothing)
summary(lme.11)
Anova(lme.11, type = 'III')
plot(lme.11)
# remove time

lme.11 <- lme(clo ~ tOut+factor(sex), random = ~ 1|subjId/day,weights = ~var,
              data=clothing)
summary(lme.11)
Anova(lme.11, type = 'III')
plot(lme.11)

#FINAL MODEL
lme.12 <- lme(clo ~ factor(sex), random = ~ 1|subjId/day,weights = ~var,
              data=clothing)
summary(lme.12)
Anova(lme.12, type = 'III')
plot(lme.12)



# Fit mixed effects models that use autocorrelation ----

## by using subDay, we remove the factor day and subjID in this way 

fit.exp<- lme(clo ~ tInOp+factor(sex)+ tOut,random=~1|subDay,
              correlation=corExp(form=~1|subDay),
              data=clothing,
              method="ML")
logLik(fit.exp)

plot(Variogram(fit.exp), main='Exp')









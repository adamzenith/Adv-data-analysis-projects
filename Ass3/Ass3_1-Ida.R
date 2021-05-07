## Advanced Data Analysis and Statistical Modelling 
## Assignment 3 
## part 1

require(plotrix)
library(nlme)

setwd('/Users/idabukhvillesen/Dropbox/8 SEMESTER/Adv. Data Ana. & Stat. Mod./Assignments/Assignment 3')

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
anova(lme.1)
plot(lme.1)

lme.2 <- lme(clo ~ factor(sex) + factor(day), random = ~1|subjId, data=clothing)
summary(lme.2)
anova(lme.2)
plot(lme.2)

lme.3 <- lme(clo ~ factor(sex) + factor(day) + time, random = ~1|subjId, data=clothing)
summary(lme.3)
anova(lme.3)
plot(lme.3)

anova(lme.2,lme.3)

lme.4 <- lme(clo ~ factor(sex) + factor(day) + time + time2, random = ~1|subjId, data=clothing)
summary(lme.4)
anova(lme.4)
plot(lme.4)
# time2 does not seem relevant 

lme.5 <- lme(clo ~ factor(sex) + factor(day) + time + tOut + tInOp, random = ~1|subjId, data=clothing)
summary(lme.5)
anova(lme.5)
plot(lme.5)

lme.6 <- lme(clo ~ factor(sex) + factor(day) + time + tOut + tInOp, random = ~1|subjId, data=clothing)
summary(lme.6)
anova(lme.6)
plot(lme.6)



# Fit mixed effects models that use subjId and day as a random effect ----

lme.11 <- lme(clo ~ factor(sex), random = ~ 1|subjId/day, data=clothing)
summary(lme.11)
anova(lme.11)
plot(lme.11)




# Fit mixed effects models that use autocorrelation ----

## by using subDay, we remove the factor day and subjID in this way 

model <- lme(clo ~ factor(sex), random = ~ 1|subDay,
              correlation = corExp(form = ~1|subDay), data = clothing)

model

plot(Variogram(model), main='Exp')








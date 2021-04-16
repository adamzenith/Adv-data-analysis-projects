# _________________
# Load data 
# _________________

library(ISwR)
library(MASS)
library(dplyr)
library(car)
library(ordinal)
library(rcompanion)
library(car)

#clothing <- read.table(file = '/Users/idabukhvillesen/Dropbox/8 SEMESTER/Adv. Data Ana. & Stat. Mod./Assignments/Assignment 2/dat_count.csv', sep = ";")
#clothing <- read.table(file = 'C:/Users/Bruger/Documents/GitHub/Adv-data-analysis-projects/Ass2/dat_count.csv', sep = ";")
clothing <- read.table(file= "~/Documents/Studie/Mst-Sem2-2021/Advanced Data Analysis and Stastitical Modelling/Assigm2/dat_count.csv", sep = ';')

# INITIAL DATA EXPLORATION
par ( mfrow =c(2 ,1))
interaction.plot(clothing$clo, clothing$sex, clothing$tInOp, xlab = 'Times Changed', ylab = 'Temperature Indoor', main= '')
interaction.plot(clothing$clo, clothing$sex, clothing$tOut, xlab = 'Times Changed', ylab = 'Temperature Outdoor', main= '')
par ( mfrow =c(1 ,1))

# SEPERATING DATA BETWEEN MALE AND FEMALE
clothing$isFemale <- (clothing$sex) 
for(i in 1:length(clothing$isFemale)){
  if (clothing$isFemale[i] == 'female'){
    clothing$isFemale[i] = 0
  }
    
}

dataFem <- clothing[1,1:8] 
dataMale <- clothing[1,1:8] 
#make a female and male data set 
for(i in 1:length(clothing$isFemale)){
  if (clothing$isFemale[i] == 0){
    dataFem <- rbind(dataFem,clothing[i,1:8])
  }
  else{
    dataMale <- rbind(dataMale,clothing[i,1:8])
  }
}



# indoor temp vs sex 
plot(dataFem$clo,dataFem$tInOp,col=2,main="Indoor Temperature vs Clothing Level",pch=2,
     xlab = "Clothing Level", ylab = "Indoor Temperature", ylim = c(10,30))
points(dataMale$clo,dataMale$tInOp,col=1,pch=4)
legend("bottomright", legend=c("Female", "Male"),
       pch= c(2,4), col=c(2,1), cex=0.8)

par ( mfrow =c(1 ,1))
boxplot(clothing$clo ~clothing$sex, col=2:6, xlab = "Sex", ylab = "Clothing Changes")


coTable = table(clothing$clo, clothing$sex) #contingency
xtable(coTable)
chisq.test(coTable) #low p-value -> independent
fisher.test(clothing$clo, clothing$sex)
cramerV(coTable,bias.correct =TRUE)


############# Binomial Distribution #######################
###########################################################

clothing$sex <- factor(clothing$sex)
#clothing$nobs <- factor(clothing$nobs)
#clothing$clo.adj <- factor(clothing$clo.adj)
#clothing$clo <- factor(clothing$clo)
clothing$bin<-cbind(clothing$clo,clothing$nobs-clothing$clo)
model0.glm<- glm( cbind(clo,nobs-clo) ~ sex*tOut*tInOp , weights = nobs, family = binomial(link='logit'), data = clothing )
model0.glm
summary ( model0.glm, test = 'Chisq')
Anova ( model0.glm, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model0.glm)

## should we use quasibinomial or binomial??
# remove sex:tInOp

model1.glm<- glm( cbind(clo,nobs-clo) ~ sex:tOut:tInOp+tOut:tInOp+sex:tOut+tInOp+tOut+sex , weights = nobs, family = binomial(link='logit'), data = clothing )
model1.glm
summary ( model1.glm, test = 'Chisq')
Anova ( model1.glm, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model1.glm)


model2.glm<- glm( cbind(clo,nobs-clo) ~ sex:tOut:tInOp+tOut:tInOp+sex:tOut+tOut+sex , weights = nobs, family = binomial(link='logit'), data = clothing )
model2.glm
summary ( model2.glm, test = 'Chisq')
Anova ( model2.glm, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model2.glm)



anova (model0.glm, model1.glm, test='Chisq')
anova (model1.glm, model2.glm, test='Chisq')
anova (model0.glm, model2.glm, test='Chisq')

(AIC(model0.glm, model1.glm, model2.glm))

############# Poisson Distribution ########################
###########################################################

model0.poi<- glm( clo ~ sex*tOut*tInOp, weights = nobs , family = poisson(), data = clothing )
summary ( model0.poi)
Anova ( model0.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model0.poi) ## remove sex:tInOp


model1.poi<- glm( clo ~ sex:tOut:tInOp+tOut:tInOp+sex:tOut+tInOp+tOut+sex, weights = nobs , family = poisson(), data = clothing )
summary ( model1.poi)
Anova ( model1.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model1.poi) ## remove tInOp

model2.poi<- glm( clo ~ sex:tOut:tInOp+tOut:tInOp+sex:tOut+tOut+sex, weights = nobs , family = poisson(), data = clothing )
summary ( model2.poi)
Anova ( model2.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model2.poi) ## remove tInOp


anova (model0.poi, model1.poi, test='Chisq')
anova (model1.poi, model2.poi, test='Chisq')
anova (model0.poi, model2.poi, test='Chisq')

AIC(model0.poi, model1.poi, model2.poi)

### With an off-set #####

# Before the off-set, non of them are significant 
model3.poi<- glm( clo ~ sex*tOut*tInOp+offset(log(clothing$nobs)), family = poisson(link = 'log'), data = clothing )
summary ( model3.poi)
Anova ( model3.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model3.poi) ## remove tInOp:sex

model4.poi<- glm( clo ~ sex:tOut:tInOp+sex+tInOp+tOut+tInOp:tOut+tOut:sex+ offset(log(clothing$nobs)), family = poisson(link = 'log'), data = clothing )
summary ( model4.poi)
Anova ( model4.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model4.poi) ## remove tInOp:sex

model5.poi<- glm( clo ~ sex:tOut:tInOp+sex+tOut+tInOp:tOut+tOut:sex+offset(log(clothing$nobs)), family = poisson(link = 'log'), data = clothing )
summary ( model5.poi)
Anova ( model5.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model5.poi) ## remove tInOp

model6.poi<- glm( clo ~ sex:tOut:tInOp+ offset(log(clothing$nobs)), family = poisson(link = 'log'), data = clothing )
summary ( model6.poi)
Anova ( model6.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model6.poi) ## remove tInOp


############## Binomial Distribution - by gender ################################

# FEMALE

model1.fem<- glm( cbind(clo,nobs-clo) ~ tOut*tInOp , weights = nobs,family = binomial(), data = dataFem )
summary ( model1.fem)
Anova ( model1.fem, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model1.fem) ## remove tInOp

drop1(model1.fem, test='Chisq')

model2.fem<- glm( cbind(clo,nobs-clo) ~ tOut:tInOp+tOut, weights = nobs , family = binomial(), data = dataFem )
summary ( model2.fem)
Anova ( model2.fem, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model2.fem)

anova (model1.fem, model2.fem, test='Chisq')
logLik(model1.fem)
logLik(model2.fem)
AIC(model1.fem, model2.fem)

# MALE

model1.mal<- glm( cbind(clo,nobs-clo) ~ tOut*tInOp, weights = nobs , family = binomial(), data = dataMale )
summary ( model1.mal)
Anova ( model1.mal, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model1.mal) ## remove tInOp

drop1(model1.mal, test='Chisq')

model2.mal<- glm( cbind(clo,nobs-clo) ~ tOut:tInOp+tOut, weights = nobs , family = binomial(), data = dataMale )
summary ( model2.mal)
Anova ( model2.mal, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model2.mal)

anova (model1.mal, model2.mal, test='Chisq')
logLik(model1.mal)
logLik(model2.mal)
AIC(model1.mal, model2.mal)


par(mfrow = c(2,2))
plot(dataMale$clo,dataMale$tInOp,col='red')
plot(dataMale$clo,dataMale$tOut,col='red')

plot(dataFem$clo,dataFem$tInOp,col='blue')
plot(dataFem$clo,dataFem$tOut,col='blue')

############# Poisson Distribution - by gender ############
###########################################################

# FEMALE
model1.fem.poi<- glm( clo ~ tOut*tInOp, weights = nobs , family = poisson(link='log'), data = dataFem )
summary ( model1.fem.poi)
Anova ( model1.fem.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model1.fem.poi) ## remove tInOp

model2.fem.poi<- glm( clo ~ tOut:tInOp+tOut, weights = nobs , family = poisson(link='log'), data = dataFem )
summary ( model2.fem.poi)
Anova ( model2.fem.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model2.fem.poi) ## remove tInOp

anova (model1.fem.poi, model2.fem.poi, test='Chisq')
logLik(model1.fem.poi)
logLik(model2.fem.poi)
AIC(model1.fem, model2.fem,model1.fem.poi, model2.fem.poi)

#MALE

model1.mal.poi<- glm( clo ~ tOut*tInOp, weights = nobs , family = poisson(link='log'), data = dataMale )
summary ( model1.mal.poi)
Anova ( model1.mal.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model1.mal.poi) ## remove tInOp

model2.mal.poi<- glm( clo ~ tOut:tInOp+tOut, weights = nobs , family = poisson(link='log'), data = dataMale )
summary ( model2.mal.poi)
Anova ( model2.mal.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model2.mal.poi) ## remove tInOp

anova (model1.mal.poi, model2.mal.poi, test='Chisq')
logLik(model1.mal.poi)
logLik(model2.mal.poi)
AIC(model1.mal, model2.mal,model1.mal.poi, model2.mal.poi)














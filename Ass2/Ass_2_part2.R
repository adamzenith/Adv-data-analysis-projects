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
require(multcomp)
require(lmerTest)
require(xtable)
library(CombMSC)
library(diagram)
library(lsmeans)

clothing <- read.table(file= "~/Documents/Studie/Mst-Sem2-2021/Advanced Data Analysis and Stastitical Modelling/Assigm2/dat_count.csv", sep = ';')

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



### Initial Data Exploration 
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
clothing$bin<-cbind(clothing$clo,clothing$nobs-clothing$clo)

#clothing=clothing[clothing$clo!=3,]
#clothing=clothing[clothing$clo!=4,]


model0.glm<- glm( cbind(clo,nobs-clo) ~ sex*tOut*tInOp , weights = nobs, family = binomial(link='logit'), data = clothing )
model0.glm
summary ( model0.glm, test = 'Chisq')
Anova ( model0.glm, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model0.glm)
# remove sex:tInOp

model1.glm<- glm( cbind(clo,nobs-clo) ~ sex:tOut:tInOp+tOut:tInOp+sex:tOut+tInOp+tOut+sex , weights = nobs, family = binomial(link='logit'), data = clothing )
model1.glm
summary ( model1.glm, test = 'Chisq')
Anova ( model1.glm, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model1.glm)
# remove tInOp

model2.glm<- glm( cbind(clo,nobs-clo) ~ sex:tOut:tInOp+tOut:tInOp+sex:tOut+tOut+sex , weights = nobs, family = binomial(link='logit'), data = clothing )
model2.glm
summary ( model2.glm, test = 'Chisq')
Anova ( model2.glm, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model2.glm)
# Final Model


anova (model0.glm, model1.glm, test='Chisq')
anova (model1.glm, model2.glm, test='Chisq')
anova (model0.glm, model2.glm, test='Chisq')

xtable(AIC(model0.glm, model1.glm, model2.glm))

xtable(confint(model2.poi, oldNames = FALSE))

############# Poisson Distribution ########################
###########################################################

model0.poi<- glm( clo ~ sex*tOut*tInOp, weights = nobs , family = poisson(), data = clothing )
summary ( model0.poi,test = 'Chisq')
Anova ( model0.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model0.poi) 
# remove sex:tInOp

model1.poi<- glm( clo ~ sex:tOut:tInOp+tOut:tInOp+sex:tOut+tInOp+tOut+sex, weights = nobs , family = poisson(), data = clothing )
summary ( model1.poi)
Anova ( model1.poi, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model1.poi) 
# remove tInOp

model2.poi<- glm( clo ~ sex:tOut:tInOp+tOut:tInOp+sex:tOut+tOut+sex, weights = nobs , family = poisson(), data = clothing )
summary ( model2.poi)
Anova ( model2.poi, type = 'III' ) 
par ( mfrow =c(1 ,2))
plot(model2.poi) 
# Final Model

anova (model0.poi, model1.poi, test='Chisq')
anova (model1.poi, model2.poi, test='Chisq')
anova (model0.poi, model2.poi, test='Chisq')

xtable(AIC(model0.poi, model1.poi, model2.poi))

xtable(confint(model2.poi, oldNames = FALSE))

############# Data Visualisation ########################
###########################################################






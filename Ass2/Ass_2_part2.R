# _________________
# Load data 
# _________________

library(MASS)
library(dplyr)
library(car)

#clothing <- read.table(file = '/Users/idabukhvillesen/Dropbox/8 SEMESTER/Adv. Data Ana. & Stat. Mod./Assignments/Assignment 2/dat_count.csv', sep = ";")
#clothing <- read.table(file = 'C:/Users/Bruger/Documents/GitHub/Adv-data-analysis-projects/Ass2/dat_count.csv', sep = ";")
clothing <- read.table(file= "~/Documents/Studie/Mst-Sem2-2021/Advanced Data Analysis and Stastitical Modelling/Assigm2/dat_count.csv", sep = ';')

# INITIAL DATA EXPLORATION
par ( mfrow =c(2 ,1))
interaction.plot(clothing$clo, clothing$sex, clothing$tInOp, xlab = 'Times Changed', ylab = 'Temperature Indoor', main= '')
interaction.plot(clothing$clo, clothing$sex, clothing$tOut, xlab = 'Times Changed', ylab = 'Temperature Outdoor', main= '')


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

boxplot(clothing$clo ~clothing$sex, col=2:6, xlab = "Sex", ylab = "Clothing Changes")


clothing$sex <- factor(clothing$sex)
#clothing$nobs <- factor(clothing$nobs)
#clothing$clo.adj <- factor(clothing$clo.adj)
#clothing$clo <- factor(clothing$clo)
clothing$bin<-cbind(clothing$clo,clothing$nobs-clothing$clo)
model0.glm<- glm( cbind(clo,nobs-clo) ~ time+sex+tOut+tInOp , family = binomial(), data = clothing )
model0.glm
summary ( model0.glm)
Anova ( model0.glm, type = 'III' ) 
par ( mfrow =c(2 ,2))
plot(model0.glm)

############# Binomial Distribution #######################
###########################################################

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

############# Poisson Distribution ########################
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


#####################################
#####################################











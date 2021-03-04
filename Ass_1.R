# Advanced Data Analysis and Statistical Modelling 
# Assigment 1 

# _________________
# Load data 
# _________________
#Helena 
setwd("~/Documents/Studie/Mst-Sem2-2021/Advanced Data Analysis and Stastitical Modelling/Assigm1")

#Ida
#clothingSum <- read.csv(file = '/Users/idabukhvillesen/Documents/GitHub/Adv-data-analysis-projects/clothingSum.csv')

#Load 
clothingSum <- read.csv(file = 'clothingSum.csv')

# make sex binary variable
clothingSum$sex <- as.factor (clothingSum$sex) 
clothingSum$day <- as.factor(clothingSum$day)

clothingSum$isFemale <- as.numeric (clothingSum$sex) 
for(i in 1:length(clothingSum$isFemale)){
  if (clothingSum$isFemale[i] == 2){
      clothingSum$isFemale[i] = 0
  }
}

dataFem <- clothingSum[1,1:7] 
dataMale <- clothingSum[1,1:7] 
#make a female and male data set 
for(i in 1:length(clothingSum$isFemale)){
  if (clothingSum$isFemale[i] == 1){
    dataFem <- rbind(dataFem,clothingSum[i,1:7])
  }
  else{
    dataMale <- rbind(dataMale,clothingSum[i,1:7])
  }
}

# _________________
# A.1 Explore data
# _________________

# _____
#clothing scale vs in + out door temp 
# _____
ClothTemp <- cbind(clothingSum$clo,clothingSum$tOut,clothingSum$tInOp);

# correlations and co-variances in the above data 
cov(ClothTemp)
cor(ClothTemp)
library(corrplot)
M <- cor(ClothTemp)
colnames(M) <- c("Clothing", "Outdoor","Indoor")
rownames(M) <- c("Clothing", "Outdoor","Indoor")
corrplot.mixed(M)

# _____
# clothing scale vs sex 
# _____
plot(clothingSum$isFemale,clothingSum$clo)
#from the plot is is clear that females generally wear much more clothes than men 

#mean and variance clothing female and male 
mean_fem <- mean(dataFem$clo)
mean_male <- mean(dataMale$clo)
var_fem <- var(dataFem$clo)
var_male <- var(dataMale$clo)
#the variance for men is much lower
#maybe bc men all dress the same, and women can choose more freely, lol
#like dresses, skirts, tops etc. 
par(mfrow=c(1,1))
# out-door temp vs sex 
plot(dataFem$clo,dataFem$tOut,col=2,main="Outdoor Temperature vs Clothing Level",pch=2,
     xlab = "Clothing Level", ylab = "Outdoor Temperature",ylim = c(10,30))
points(dataMale$clo,dataMale$tOut,col=1,pch=4)
legend("bottomright", legend=c("Female", "Male"),
       pch= c(2,4), col=c(2,1), cex=0.8)

# indoor temp vs sex 
plot(dataFem$clo,dataFem$tInOp,col=2,main="Indoor Temperature vs Clothing Level",pch=2,
     xlab = "Clothing Level", ylab = "Indoor Temperature", ylim = c(10,30))
points(dataMale$clo,dataMale$tInOp,col=1,pch=4)
legend("bottomright", legend=c("Female", "Male"),
       pch= c(2,4), col=c(2,1), cex=0.8)


# Helena Section
############################################################
############################################################

clothingSum$sex = factor(clothingSum$sex)
clothingSum$day = factor(clothingSum$day)
clothingSum$subjId = factor(clothingSum$subjId)

Clothing = clothingSum$clo
par(mfrow=c(1,2))
# Density plot of Clo: 
f<-function(x){dnorm(x,mean=mean(Clothing,na.rm=T),
                     sd=sd(clothingSum$clo,na.rm=T))} 
hist(Clothing,xlab='Clothing',probability=T)
curve(f,0,1,lwd=3,col="red",add=T) 
rm(f)
par(mfrow=c(1,1))



pairs(clothingSum, panel = panel.smooth, main = "Clothing Data")

summary(clothingSum)


par(mfrow=c(1,3))
boxplot(clothingSum$clo ~clothingSum$sex, col=2:6, xlab = "Sex", ylab = "Clothing Level")
boxplot(clothingSum$tInOp ~clothingSum$sex, col=2:6, xlab = "Sex", ylab = "Indoor Temperature")
boxplot(clothingSum$tOut ~clothingSum$sex, col=2:6, xlab = "Sex", ylab = "Outdoor Temperature")

############################################################
# Models ###################################################
############################################################

model0 <- lm(clo ~ tInOp*tOut*sex, data = clothingSum)

summary(model0)
anova(model0)
par(mfrow=c(2,2))
plot(model0)

model1 <- lm(clo ~ tInOp*sex+tOut, data = clothingSum)
summary(model1)
anova(model1)
par(mfrow=c(2,2))
plot(model1)

# try higher order
model2 <- lm(clo ~ tInOp*sex+tOut^2, data = clothingSum)
summary(model2)
anova(model2)
par(mfrow=c(2,2))
plot(model2)
#does not really improve the model here 

#test model level 1 vs higehr order 
anova(model1,model2,test="Chisq") #exactly the same 


# try log transform 
model3 <- lm(log(clo) ~ tInOp*sex+tOut, data = clothingSum)
summary(model3)
anova(model3)
par(mfrow=c(2,2))
plot(model3)

library(MASS)
qqPlot(model3,reps=10000)
qqPlot(model3,simulate=FALSE)
#from the qq-plot the log transformes is not too good either :( 


#try another type of higer order 
model4 <- lm(clo ~ (tInOp*sex+tOut)^2, data = clothingSum)
summary(model4)
anova(model4)
par(mfrow=c(2,2))
plot(model4)
#still the same variables that are important

#make model smaller
model5 <- lm(clo ~ tInOp+sex+tOut+tInOp:sex, data = clothingSum)
summary(model5)
anova(model5)
par(mfrow=c(2,2))
plot(model5)


#log
model6 <- lm(log(clo) ~ tInOp+sex+tOut+tInOp:sex, data = clothingSum)
summary(model6)
anova(model6)
par(mfrow=c(2,2))
plot(model6)


model6 <- lm(log(clo) ~ tInOp+sex+tOut+tInOp:sex, data = clothingSum)
summary(model6)
anova(model6)
par(mfrow=c(2,2))
plot(model6)


# Add weights to the model -> sub question A.4
model7 <- lm(log(clo) ~ tInOp+sex+tOut+tInOp:sex, data = clothingSum, weights=(rep(0.5, 136))^(1/2))
summary(model7)
anova(model7)
par(mfrow=c(2,2))
plot(model7)



# ______________
# DOING WEIGHTS 
varMale <- var(dataMale$clo)
varFem <- var(dataFem$clo)

n <- 136
v <- rep(0,n)
for(i in 1:n){
  if (clothingSum$isFemale[i] == 0){
    v[i] = varMale
  }
  else {
    v[i] = varFem 
  }
}

model8 <- lm(clo ~ tInOp*sex+I(tOut^2), data = clothingSum, weights = 1/v)
summary(model8)
anova(model8)
par(mfrow=c(2,2))
plot(model8)
# argue to remove leverage point no 22 
library(car)
qqPlot(model8,reps=10000)
qqPlot(model8,simulate=FALSE)

library(emmeans)
plot(emmeans(model8, 'sex'))


#log trans good model
model10 <- lm(log(clo) ~ tInOp*sex+I(tOut^2), data = clothingSum, weights = 1/v)
summary(model10)
anova(model10)
par(mfrow=c(2,2))
plot(model10)
# not a good idea :( (:


# same model no weights 
model9 <- lm(clo ~ tInOp*sex+I(tOut^2), data = clothingSum)
summary(model9)
anova(model9)
par(mfrow=c(2,2))
plot(model9)
# we see that the quantiles in the qqPlot is fucked, tihi 

alt = clothingSum$tOut
y = clothingSum$clo
sex = clothingSum$sex
op = clothingSum$tInOp












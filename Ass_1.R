# Advanced Data Analysis and Statistical Modelling 
# Assigment 1 

# _________________
# Load data 
# _________________
#Helena 
#setwd("~/Documents/Studie/Mst-Sem2-2021/Advanced Data Analysis and Stastitical Modelling/Assigm1")

#Ida
clothingSum <- read.csv(file = '/Users/idabukhvillesen/Documents/GitHub/Adv-data-analysis-projects/clothingSum.csv')

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

#################################
# SECTION A ----
# _________________
# A.1 Explore data
# _________________

ClothTemp <- cbind(clothingSum$clo,clothingSum$tOut,clothingSum$tInOp);

# CORRELATIONS AND COVARIANCE
cov(ClothTemp)
cor(ClothTemp)
library(corrplot)
corrplot(cor(ClothTemp))


# SEX VS TEMP PLOTS 
# indoor
plot(dataFem$clo,dataFem$tInOp,col=2,main="Indoor temp vs sex",pch=2)
points(dataMale$clo,dataMale$tInOp,col=1,pch=4)
# out-door 
plot(dataFem$clo,dataFem$tOut,col=2,main="Outdoor temp vs sex",pch=2)
points(dataMale$clo,dataMale$tOut,col=1,pch=4)


# making data factors
clothingSum$sex = factor(clothingSum$sex)
clothingSum$day = factor(clothingSum$day)
clothingSum$subjId = factor(clothingSum$subjId)

# DENSITY PLOT OF CLOTING  
f<-function(x){dnorm(x,mean=mean(clothingSum$clo,na.rm=T),
                     sd=sd(clothingSum$clo,na.rm=T))} 
hist(clothingSum$clo,xlab='Clothing',probability=T)
curve(f,0,1,lwd=3,col="red",add=T) 
rm(f)
par(mfrow=c(1,1))

# ???? NOT SURE WHAT THIS IS 
par(mfrow=c(2,1))
plot(clothingSum$clo~clothingSum$tOut+clothingSum$tInOp,pch=as.numeric(clothingSum$sex),
     col=rep(c(0,1), each=50)+1)
pairs(clothingSum, panel = panel.smooth, main = "Clothing Data")

# SUMMARY OF ALL DATA 
summary(clothingSum)

library(xtable)
library(plyr)
dtf <- sapply(clothingSum, each(min, max, mean, sd, var, median, IQR))
xtable(dtf)

# BOX PLOTS 
par(mfrow=c(1,3))
boxplot(clothingSum$clo ~clothingSum$sex, col=c("firebrick2","blue"))
boxplot(clothingSum$tInOp ~clothingSum$sex, col=c("firebrick2","blue"))
boxplot(clothingSum$tOut ~clothingSum$sex, col=c("firebrick2","blue"))


# MODELS A.2

# initial model
# all possible combinations of paramters 
model0 <- lm(clo ~ tInOp*tOut*sex, data = clothingSum)
summary(model0)
anova(model0)
par(mfrow=c(2,2))
plot(model0)

# decrease model complexity 
model1 <- lm(clo ~ tInOp*sex+tOut, data = clothingSum)
summary(model1)
anova(model1)
par(mfrow=c(2,2))
plot(model1)
# all parameters significant 

# try higher order
model2 <- lm(clo ~ tInOp*sex+tOut^2, data = clothingSum)
summary(model2)
anova(model2)
par(mfrow=c(2,2))
plot(model2)
#does not really improve the model here 

#test model level 1 vs higehr order 
anova(model1,model2,test="Chisq") #exactly the same 

# try log transform the data 
model3 <- lm(log(clo) ~ tInOp*sex+tOut^2, data = clothingSum)
summary(model3)
anova(model3)
par(mfrow=c(2,2))
plot(model3)
# does not really do anything for the data as well 

library(car)
qqPlot(model3,reps=10000)
qqPlot(model3,simulate=FALSE)
#from the qq-plot the log transformes is not too good either :( 
# the quantiles and the center is outside CI 


#try another type of higer order 
model4 <- lm(clo ~ (tInOp*sex+tOut)^2, data = clothingSum)
summary(model4)
anova(model4)
par(mfrow=c(2,2))
plot(model4)
#still the same variables that are important
# also the quantiles here are all off 

#test model 
anova(model1,model4,test="Chisq") 

#make model smaller
model5 <- lm(clo ~ tInOp+sex+tOut+tInOp:sex, data = clothingSum)
summary(model5)
anova(model5)
par(mfrow=c(2,2))
plot(model5)

# IN CONCLUSION 
# we choose this model 
model6 <- lm(clo ~ tInOp*sex+I(tOut^2), data = clothingSum)
summary(model6)
anova(model6)
par(mfrow=c(2,2))
plot(model6)

par(mfrow=c(1,1))
qqPlot(model6,reps=10000)
qqPlot(model6,simulate=FALSE,ylab="Studentized Residuals - model 4", main="qqPlot for model 4 \n Best non-weighted model")



#test model
anova(model1,model6,test="Chisq") #

# but the quantiles are still of, so we add weight 
# -> weighted least squares 
# variances on the diagonal 

# ______________
# DOING WEIGHTS A.4
# ______________

# find the variances for clo for the sexes
# this is used as weights in v for the model 
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

# add weights to the model 
model_final <- lm(clo ~ tInOp*sex+I(tOut^2), data = clothingSum, weights = 1/v)
summary(model_final)
anova(model_final)
par(mfrow=c(2,2))
plot(model_final)
# argue to remove leverage point no 22 
library(car)
par(mfrow=c(1,1))
qqPlot(model_final,reps=10000)
qqPlot(model_final,simulate=FALSE, main="qqPlot for the weighted best model \n (model 4)")



# Using the likelihood to optimize the parameters 
# with initial guesses as the variances 
Function<-function(v){
  model_final_loglik <- lm(clo ~ tInOp*sex+I(tOut^2), data = clothingSum, weights = 1/v)
  -logLik(model_final_loglik)
}
new_loglik_func=optim(v,Function)
par(mfrow=c(2,2))
model_final_loglik <- lm(clo ~ tInOp*sex+I(tOut^2), data = clothingSum, weights = 1/new_loglik_func$par)
plot(model_final_loglik)
par(mfrow=c(1,1))
qqPlot(model_final_loglik,reps=10000)
qqPlot(model_final_loglik,simulate=FALSE, main="qqPlot for the weighted best optimzed model \n (model 4)")


#log trans good model
model10 <- lm(log(clo) ~ tInOp*sex+I(tOut^2), data = clothingSum, weights = 1/new_loglik_func$par)
summary(model10)
anova(model10)
par(mfrow=c(2,2))
plot(model10)
# not a good idea :( (:



# POINT A.5
# make model prediction and plot for model_final


## PREDICTION PLOTS
model_final <- lm(clo ~ tInOp*sex+I(tOut^2), data = clothingSum, weights = 1/v)
pred_male_out <- predict(model_final,type="response",interval = "confidence",newdata =clothingSum_male_out )
pred_female_out <- predict(model_final,type="response",interval = "confidence",newdata =clothingSum_female_out )
pred_male_in <- predict(model_final,type="response",interval = "confidence",newdata =clothingSum_male_in )
pred_female_in <- predict(model_final,type="response",interval = "confidence",newdata =clothingSum_female_in )
par(mfrow=c(2,2))
plot(temp,pred_male_out[,1],pch = 19,col=4,xlab = "Outdoor Temperature",
     ylab = "Level of Clothing", main = "Male", ylim = c(0.38,0.65))
lines(temp,pred_male_out[,2],col=2,lty = 2)
lines(temp,pred_male_out[,3],col=2,lty = 2)
legend("topright", c("Confidence Interval"), col=2,lty = 2)
plot(temp,pred_female_out[,1],pch = 19,col=4,xlab = "Outdoor Temperature",
     ylab = "Level of Clothing", main = "Female", ylim = c(0.38,0.65))
lines(temp,pred_female_out[,2],col=2,lty = 2)
lines(temp,pred_female_out[,3],col=2,lty = 2)

plot(temp,pred_male_in[,1],pch = 19,col=3,xlab = "Indoor Temperature",
     ylab = "Level of Clothing", main = "Male", ylim = c(0.38,0.65))
lines(temp,pred_male_in[,2],col=2,lty = 2)
lines(temp,pred_male_in[,3],col=2,lty = 2)

plot(temp,pred_female_in[,1],pch = 19,col=3,xlab = "Indoor Temperature",
     ylab = "Level of Clothing", main = "Female", ylim = c(0.38,0.65))
lines(temp,pred_female_in[,2],col=2,lty = 2)
lines(temp,pred_female_in[,3],col=2,lty = 2)




# POINT A.6 
par(mfrow=c(1,1))
plot(as.numeric(clothingSum$subjId),model_final_loglik$residuals,main="Residuals of weighted optmized model", xlab="Index for sujectIDs", ylab="Residuals")
abline(h=0,col=2,lty=1)
abline(h=mean(model_final_loglik$residuals), col="blue",lty="dotted")
abline(h=mean(model_final_loglik$residuals)+1.96*sqrt(var(subjectID_coef)), col="blue",lty = 2)
abline(h=mean(model_final_loglik$residuals)-1.96*sqrt(var(subjectID_coef)), col="blue",lty = 2)
legend(0, -0.22, legend=c("0","Mean", "95% CI"),
       col=c("red","blue", "blue"), lty=c(1,2,2), cex=0.8)


#################################
# SECTION B ----

#MODEL with subjId 

# B.1
model11 <- lm(clo ~ tInOp*sex+I(tOut^2)+subjId, data = clothingSum, weights = 1/new_loglik_func$par)
summary(model11)
anova(model11)
par(mfrow=c(2,2))
plot(model11)

Function_subjectID<-function(v){
  model_subject_loglik <- lm(clo ~ tInOp*sex+I(tOut^2)+subjId, data = clothingSum, weights = 1/v)
  -logLik(model_subject_loglik)
}
loglik_func_subject=optim(v,Function_subjectID)
final_loglik_subject <- lm(clo ~ tInOp*sex+I(tOut^2)+subjId, data = clothingSum, weights = 1/loglik_func_subject$par)
par(mfrow=c(2,2))
plot(final_loglik_subject)
summary(final_loglik_subject)
anova(final_loglik_subject)

#Visual presentation of params
# B.2 
par(mfrow=c(1,1))
plot(final_loglik_subject$coefficients)

subjectID_coef <- as.numeric(c(final_loglik_subject$coefficients[5:47],final_loglik_subject$coefficients[49:50]))
plot(subjectID_coef,main="Estimated parameters for SubjectID ",ylab="Parameter estimate",xlab="Index for SubjectID parameters", col= 2, pch=19)
abline(h=mean(subjectID_coef), col="blue")
abline(h=mean(subjectID_coef)+1.96*sqrt(var(subjectID_coef)), col="blue",lty = 2)
abline(h=mean(subjectID_coef)-1.96*sqrt(var(subjectID_coef)), col="blue",lty = 2)
legend(0, -0.3, legend=c("Mean", "95% CI"),
       col=c("blue", "blue"), lty=1:2, cex=0.8)



#################################
# SECTION C ----







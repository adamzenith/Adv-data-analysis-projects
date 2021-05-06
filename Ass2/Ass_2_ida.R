
## Advanced Data Analysis and Statistical Modelling 
## Assignment 2 

setwd('/Users/idabukhvillesen/Dropbox/8 SEMESTER/Adv. Data Ana. & Stat. Mod./Assignments/Assignment 2')

library(gclus)
library ( corrplot )
library(car)



## Data 
data(ozone)
head(ozone)

## Summary and exploration of data 
summary(ozone)
plot(ozone)

## Correlations
par(mfrow=c(1,2))
corrplot (cor( ozone ))

# DENSITY PLOT OF OZONE CONCENTRATION
Ozone = ozone$Ozone
# Density plot of Clo:
f<-function(x){dnorm(x,mean=mean(Ozone,na.rm=T),
                     sd=sd(ozone$Ozone,na.rm=T))}
hist(Ozone,xlab='Ozone',probability=T)
curve(f,0,40,lwd=3,col="blue",add=T)
rm(f)
par(mfrow=c(1,1))

#######
 
# MODELS 

#######

## General LM MODELS 
model0 <- lm( Ozone ~ Temp+InvHt+Pres+Vis+Hgt+Hum+InvTmp+Wind , data = ozone )
summary ( model0 )
Anova ( model0, type = 'II' ) # Remove least significant -> Press
par ( mfrow =c(2 ,2))
plot ( model0 )
# Obs on square term in residuals 


model1 <- lm( Ozone ~ Temp+InvHt+Wind+Vis+Hgt+Hum+InvTmp , data = ozone )
summary ( model1 )
Anova ( model1, type = 'II' ) # Remove least significant -> Wind
par ( mfrow =c(2 ,2))
plot ( model1 )
# Looks like the residuals are missing a squared term 
# But we are not allowed to use that... 

model2 <- lm( Ozone ~ Temp+InvHt+Hgt+Vis+Hum+InvTmp , data = ozone )
summary ( model2 )
Anova ( model2, type = 'II' ) # Remove least significant -> Hgt 
par ( mfrow =c(2 ,2))
plot ( model2 )

model3 <- lm( Ozone ~ Temp+InvHt+InvTmp+Vis+Hum , data = ozone )
summary ( model3 )
Anova ( model3, type = 'II' ) # Remove least significant -> InvTmp
par ( mfrow =c(2 ,2))
plot ( model3 )

model4 <- lm( Ozone ~ Temp+InvHt+Vis+Hum , data = ozone )
summary ( model4 )
Anova ( model4, type = 'II' ) # Remove least significant -> Vis
par ( mfrow =c(2 ,2))
plot ( model4 )

model5 <- lm( Ozone ~ Temp+InvHt+Hum , data = ozone )
summary ( model5 )
Anova ( model5, type = 'II' )
par ( mfrow =c(1 ,2))
plot ( model5 )
# NOW ALL SIGNIFICANT 

model6 <- lm( log(Ozone) ~ Temp+InvHt+Hum , data = ozone )
summary ( model6 )
Anova ( model6, type = 'II' )
par ( mfrow =c(1 ,2))
plot ( model6 )
# With log transform the residuals become way better 
# Also the leverage plot is better 
# But quantiles a little of 
# Probably as a distribution assumption and link function
# ie. --> make it into a generalized model!! 

par ( mfrow =c(1 ,2))
## Turning it into a GENERALIZED MODEL
model00.glm<- glm( Ozone ~ Temp+InvHt+Pres+Vis+Hgt+Hum+InvTmp+Wind , family = poisson, data = ozone )
summary ( model00.glm)
Anova ( model00.glm, type = 'II' ) #remove hgt
par ( mfrow =c(2 ,2))
plot(model00.glm)

model01.glm<- glm( Ozone ~ Temp+InvHt+Pres+Vis+Hum+InvTmp+Wind , family = poisson, data = ozone )
summary ( model01.glm)
Anova ( model01.glm, type = 'II' ) #remove wind
par ( mfrow =c(2 ,2))
plot(model01.glm)

model02.glm<- glm( Ozone ~ Temp+InvHt+Pres+Vis+Hum+InvTmp , family = poisson, data = ozone )
summary ( model02.glm)
Anova ( model02.glm, type = 'II' ) #remove press
par ( mfrow =c(2 ,2))
plot(model02.glm)


model03.glm<- glm( Ozone ~ Temp+InvHt+Vis+Hum+InvTmp , family = poisson, data = ozone )
summary ( model03.glm)
Anova ( model03.glm, type = 'II' ) #remove invtmp
par ( mfrow =c(2 ,2))
plot(model03.glm)

model04.glm<- glm( Ozone ~ Temp+InvHt+Vis+Hum , family = poisson, data = ozone )
summary ( model04.glm)
Anova ( model04.glm, type = 'II' ) 
par ( mfrow =c(2 ,2))
plot(model04.glm)
# FINAL PARAMETERS

## TEST FOR DISTRIBUTION AND LINK 
model0.glm<- glm( Ozone ~ Temp+InvHt+Hum+Vis , family = poisson, data = ozone )
summary ( model0.glm)
Anova ( model0.glm, type = 'II' )
par ( mfrow =c(2 ,2))
plot(model0.glm)

model1.glm<- glm( Ozone ~ Temp+InvHt+Hum+Vis , family = quasipoisson(), data = ozone ) 
#If you have overdispersion (see if residual deviance is much larger than degrees of freedom), you may want to use quasipoisson() instead of poisson().
summary ( model1.glm)
Anova ( model1.glm, type = 'II' )
par ( mfrow =c(2 ,2))
plot(model1.glm)
# Not a big difference

model12.glm<- glm( Ozone ~ Temp+InvHt+Hum+Vis , family = Gamma(link="log"), data = ozone ) 
summary ( model12.glm)
Anova ( model12.glm, type = 'II' ) #remove Vis 
par ( mfrow =c(2 ,2))
plot(model12.glm)


model2.glm<- glm( Ozone ~ Temp+InvHt+Hum , family = Gamma(link="log"), data = ozone ) 
summary ( model2.glm)
Anova ( model2.glm, type = 'II' )
par ( mfrow =c(2 ,2))
plot(model2.glm)

model13.glm<- glm( Ozone ~ Temp+InvHt+Hum+Vis , family = Gamma(link="inverse"), data = ozone ) 
summary ( model13.glm)
Anova ( model13.glm, type = 'II' )
par ( mfrow =c(2 ,2))
plot(model13.glm)


model3.glm<- glm( Ozone ~ Temp+InvHt+Hum , family = Gamma(link="inverse"), data = ozone ) 
summary ( model3.glm)
Anova ( model3.glm, type = 'II' )
par ( mfrow =c(2 ,2))
plot(model3.glm)

# Compare glm models quantatively  
anova.glm <- anova(model2.glm,model3.glm, test = "Chisq")
AIC(model0.glm,model1.glm,model2.glm,model3.glm)
BIC(model0.glm,model1.glm,model2.glm,model3.glm)
# both in AIC and BIC model2.glm has lower (ie better) values  
anova.glm
p_val_glm <- 1-pchisq( abs(anova.glm$Deviance[2]), abs(anova.glm$Df[2]))
p_val_glm
anova(model2.glm,model3.glm, test = "Chisq")
anova(model2.glm,model3.glm, test="LRT")

logLik(model0.glm)
logLik(model1.glm)
logLik(model2.glm)
logLik(model3.glm)


## Compare lm (model6) and glm model (model0.glm, poisson)

# make lm as glm for comparison
#model6.1 <- glm( log(Ozone) ~ Temp+InvHt+Hum , family = gaussian(link = "identity"),data = ozone )
#summary ( model6.1 )
#Anova ( model6.1, type = 'II' )
#par ( mfrow =c(2 ,2))
#plot ( model6.1 )

# quantative test 
#logLik(model0.glm)
#logLik(model6.1)
#AIC(model0.glm, model6)

# BASE THE CONCLUSION ON THE VISUAL RESIDUAL PLOTS 
par ( mfrow =c(1,1))
plot(exp(model6$fitted.values), model6$residuals,xlab = "Fitted values", ylab = "Residuals", main = "Residual comparison",ylim=c(-1.5,2.0),xlim=c(0,30),cex=0.7, pch=10)
points(model0.glm$fitted.values, model0.glm$residuals, pch=16, col=2,cex=0.7)
legend("topright", legend = c("lm","glm"),col=c(1,2), pch=c(10,16))
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")


## Presentation of model 
## PREDICTIONS 

# make 80/20 training test and make predictions on the data 
dt = sort(sample(nrow(ozone), nrow(ozone)*.8))
train <- ozone[dt,]
test <- ozone[-dt,]

# Calculate model on training data 
model.f.glm<- glm( Ozone ~ Temp+InvHt+Hum+Vis , family = poisson, data = train )
summary ( model.f.glm)
Anova ( model.f.glm, type = 'II' )
par ( mfrow =c(2 ,2))
plot(model.f.glm)

# predict from test data 
pred.f <- predict(model.f.glm, newdata = test,
        type = c("response"),
        se.fit = FALSE, dispersion = NULL, terms = NULL,
        na.action = na.pass)

# predict from original ozone data set 
pred.ref <- predict(model0.glm, newdata = ozone,
                  type = c("response"),
                  se.fit = FALSE, dispersion = NULL, terms = NULL,
                  na.action = na.pass)


# calculate RMSE for the two predictions 
sqrt(mean((pred.f-test$Ozone)^2))
sqrt(mean((pred.ref-ozone$Ozone)^2))

#plot predictions 
par ( mfrow =c(1 ,1))
plot(ozone$Ozone, pred.ref,main="Final Model Predictions",xlab = "True values",ylab = "Predicted values")
lines(c(0:35),c(0:35),lty=3)
grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")
points(test$Ozone, pred.f,pch=16, col=2,cex=1.15)
legend("bottomright", legend = c("Original full data-set predictions","Test-set predictions"),col=c(1,2), pch=c(1,16),cex=0.85)


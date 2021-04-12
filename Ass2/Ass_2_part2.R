# _________________
# Load data 
# _________________

clothing <- read.table(file = '/Users/idabukhvillesen/Dropbox/8 SEMESTER/Adv. Data Ana. & Stat. Mod./Assignments/Assignment 2/dat_count.csv', sep = ";")
clothing <- read.table(file = 'C:/Users/Bruger/Documents/GitHub/Adv-data-analysis-projects/Ass2/dat_count.csv', sep = ";")
# prepare clo data
clothing$clo.adj <- clothing$clo
clothing$clo.adj[clothing$clo.adj > 0] = 1

clothing$sex <- factor(clothing$sex)
#clothing$nobs <- factor(clothing$nobs)
#clothing$clo.adj <- factor(clothing$clo.adj)
#clothing$clo <- factor(clothing$clo)
clothing$bin<-cbind(clothing$clo,clothing$nobs-clothing$clo)
model0.glm<- glm( cbind(clo,nobs-clo) ~ time+sex+tOut+tInOp , family = binomial(), data = clothing )
summary ( model0.glm)
Anova ( model0.glm, type = 'II' ) 
par ( mfrow =c(2 ,2))
plot(model0.glm)

model1.glm.m<- glm( cbind(clo,nobs-clo) ~ tOut+tInOp , family = binomial(), data = clothing )
summary ( model1.glm)
Anova ( model1.glm, type = 'II' ) 
par ( mfrow =c(2 ,2))
plot(model1.glm)

model1.glm.f<- glm( cbind(clo,nobs-clo) ~ tOut+tInOp , family = binomial(), data = clothing )
summary ( model1.glm)
Anova ( model1.glm, type = 'II' ) 
par ( mfrow =c(2 ,2))
plot(model1.glm)

model2.glm<- glm( cbind(clo,nobs-clo) ~ sex+tInOp , family = binomial(), data = clothing )
summary ( model2.glm)
Anova ( model2.glm, type = 'II' ) 
par ( mfrow =c(2 ,2))
plot(model2.glm)

model3.glm<- glm( cbind(clo,nobs-clo) ~ sex+tOut+tInOp , family = binomial(), data = clothing )
summary ( model3.glm)
Anova ( model3.glm, type = 'II' ) 
par ( mfrow =c(2 ,2))
plot(model3.glm)


model0.glm<- glm( clo ~ time+nobs+sex+tOut+tInOp , family = poisson(link = "log"), data = clothing )
summary ( model0.glm)
Anova ( model0.glm, type = 'II' ) 
par ( mfrow =c(2 ,2))
plot(model0.glm)


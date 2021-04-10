# _________________
# Load data 
# _________________

clothing <- read.table(file = '/Users/idabukhvillesen/Dropbox/8 SEMESTER/Adv. Data Ana. & Stat. Mod./Assignments/Assignment 2/dat_count.csv', sep = ";")

# prepare clo data
clothing$clo.adj <- clothing$clo
clothing$clo.adj[clothing$clo.adj > 0] = 1

clothing$sex <- factor(clothing$sex)
clothing$nobs <- factor(clothing$nobs)
#clothing$clo.adj <- factor(clothing$clo.adj)
#clothing$clo <- factor(clothing$clo)

model0.glm<- glm( clo.adj ~ time+nobs+sex+tOut+tInOp , family = binomial(link = "logit"), data = clothing )
summary ( model0.glm)
Anova ( model0.glm, type = 'II' ) 
par ( mfrow =c(2 ,2))
plot(model0.glm)




model0.glm<- glm( clo ~ time+nobs+sex+tOut+tInOp , family = poisson(link = "log"), data = clothing )
summary ( model0.glm)
Anova ( model0.glm, type = 'II' ) 
par ( mfrow =c(2 ,2))
plot(model0.glm)


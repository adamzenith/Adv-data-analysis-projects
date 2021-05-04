clo = read.csv("clothingFullAss03.csv", header=TRUE, na.strings="?")

summary(clo)

#Plotting female stuff----
par(mfrow=c(1,2))
plot(clo$clo[clo$subjId==0 & clo$sex=="female"],ylim=c(0,1))
for(i in 1:46){
  points(clo$clo[clo$subjId==i & clo$sex=="female"],col=i+1)
}

#Plotting male stuff----
plot(clo$clo[clo$subjId==4 & clo$sex=="male"],ylim=c(0,1))
for(i in 1:46){
  points(clo$clo[clo$subjId==i & clo$sex=="male"],col=i+1)
}


#Aggregate clo by subjId and Gender----
means=aggregate(clo ~ subjId + sex, data=clo,FUN=mean)


#linear mixed effect model----
library(nlme)
fit<-lme(clo~tOut+tInOp, random = ~1|sex, data=clo)
summary(fit)
plot(fit)



#Part 2-------
library(lme4)
fit0 <- lmer(clo~sex+(1|subjId),data=clo,REML=FALSE)
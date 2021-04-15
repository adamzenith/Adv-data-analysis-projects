# Ass 2, Advanced Data Analysis 
# Part 3 

library(ordinal)
library(rcompanion)
library(car)
library(effects)
library(MASS)
library(usingR)
require(gridExtra)
### LOAD DATA 
fan.data <- read.table(file ='C:/Users/matth/Documents/GitHub/Adv-data-analysis-projects/Ass2/CeilingFan.csv', sep = ";")

fan.data$fanSpeed.f <- factor(fan.data$fanSpeed)
fan.data$fanType.f <- factor(fan.data$fanType)
fan.data$TSV <- factor(fan.data$TSV)

# TEST FOR INDEPENDENCE 
coTable = table(fan.data$TSV,fan.data$fanSpeed) #contingency 
chisq.test(table(fan.data$TSV,fan.data$fanSpeed)) #low p-value -> independent
fisher.test(fan.data$TSV,fan.data$fanSpeed)
cramerV(coTable,bias.correct =TRUE)
0.# MAKE MODELS WITH ORDINAL PACKAGE 
# ordinal variables (ranked data, distance between values not known)
model0.clm <- clm( TSV ~ fanSpeed, data = fan.data )
summary(model0.clm) #
model0f.clm <- clm( TSV ~ fanSpeed.f, data = fan.data )
summary(model0f.clm)

#Anova(model0.clm,type = 'II') 
anova(model0f.clm)
anova(model0.clm)
anova(model0f.clm,model0.clm) #independence not assumed 
par(mfrow=c(2,2))
plot(model0.clm)
# CLM FOR TWO DEPENDENT VARIABLES 
model1f.clm <- clm( TSV ~ fanSpeed.f + fanType.f, data = fan.data )
model1f.clm
anova(model1f.clm) 
anova(model1f.clm,model0f.clm) # new model is better, both AIC and LogLik wise 

x=predict(model0.clm,data=1:20)
plot(predict(model1f.clm)$fit)
plot(fan.data$TSV)

#Plots
boxplot(fan.data$TSV ~fan.data$fanSpeed, col=2:6, xlab = "TSV", ylab = "Fan Speed")


plot2 = plot(Effect("fanSpeed.f",model1f.clm))
plot1 = plot(Effect("fanType.f",model1f.clm))
grid.arrange(plot1, plot2, ncol=2)

plot2 = plot(Effect("fanSpeed.f",model1f.clm),multiline=2)
plot1 = plot(Effect("fanType.f",model1f.clm),multiline=2)
grid.arrange(plot1, plot2, ncol=2)

plot2 = plot(Effect("fanSpeed.f",model1f.clm),style="stacked")
plot1 = plot(Effect("fanType.f",model1f.clm),style="stacked")
grid.arrange(plot1, plot2, ncol=2)


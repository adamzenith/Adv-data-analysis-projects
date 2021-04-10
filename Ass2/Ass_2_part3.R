# Ass 2, Advanced Data Analysis 
# Part 3 

library(ordinal)

### LOAD DATA 
fan.data <- read.table(file = '/Users/idabukhvillesen/Dropbox/8 SEMESTER/Adv. Data Ana. & Stat. Mod./Assignments/Assignment 2/CeilingFan.csv', sep = ";")

fan.data$fanSpeed.f <- factor(fan.data$fanSpeed)
fan.data$fanType.f <- factor(fan.data$fanType)
fan.data$TSV <- factor(fan.data$TSV)

# TEST FOR INDEPENDENCE 
table(fan.data$TSV,fan.data$fanSpeed) #contingency 
chisq.test(fan.data$TSV,fan.data$fanSpeed) #low p-value -> independent
fisher.test(fan.data$TSV,fan.data$fanSpeed)

# MAKE MODELS WITH ORDINAL PACKAGE 
# ordinal variables (ranked data, distance between values not known)
model0.clm <- clm( TSV ~ fanSpeed, data = fan.data )
model0.clm 
model0f.clm <- clm( TSV ~ fanSpeed.f, data = fan.data )
model0f.clm
anova(model0.clm) 
anova(model0f.clm)
anova(model0f.clm,model0.clm) #independence not assumed 


# CLM FOR TWO DEPENDENT VARIABLES 
model1f.clm <- clm( TSV ~ fanSpeed.f + fanType.f, data = fan.data )
model1f.clm
anova(model1f.clm) 
anova(model1f.clm,model0f.clm) # new model is better, both AIC and LogLik wise 


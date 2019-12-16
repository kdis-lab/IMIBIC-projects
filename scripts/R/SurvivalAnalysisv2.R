# Survival Analysis in R

# install.packages("survival")
library(survival)

mydata<- read.csv("D:/OneDrive - Universidad de Córdoba/workspace/IMIBIC/datasets/cancer/survival.csv", sep = ";")
attach(mydata)

# Define variables 
time <- meses
event <- os
group <-grupo
otherVar <- os

# Descriptive statistics
summary(time)
summary(event)
summary(group)
summary(otherVar)

X <- cbind(grupo,os)

# Non-parametric analysis

# Kaplan-Meier non-parametric analysis by group
kmsurvival1 <- survfit(Surv(time, event) ~ grupo)
summary(kmsurvival1)
plot(kmsurvival1, xlab="Time", ylab="Survival Probability")

# Nelson-Aalen non-parametric analysis
nasurvival <- survfit(coxph(Surv(time,event)~otherVar), type="aalen")
summary(nasurvival)
plot(nasurvival, xlab="Time", ylab="Survival Probability")

# Semi-parametric analysis

# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(time,event) ~ group, method="breslow")
summary(coxph)

# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(time,event) ~ group, method="efron")
summary(coxph)

# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(time,event) ~ group, method="exact")
summary(coxph)

# Cox proportional hazard model - coefficients and hazard rates
coxph <- coxph(Surv(time,event) ~ group + frailty(otherVar, method= "aic"), method="breslow")
summary(coxph)

# Parametric analysis

# Exponential, Weibull, and log-logistic parametric model coefficients
exponential <- survreg(Surv(time,event) ~ group, dist="exponential")
summary(exponential)

weibull <- survreg(Surv(time,event) ~ group, dist="weibull")
summary(weibull)

loglogistic <- survreg(Surv(time,event) ~ group, dist="loglogistic")
summary(loglogistic)
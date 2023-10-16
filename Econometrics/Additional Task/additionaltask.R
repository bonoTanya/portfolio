library(knitr)
library(xtable)
library(printr)
library(effects)
library(car)
library(AER)
library(broom)
library(tseries)
library(kableExtra)
library(ppcor)
library(corrr)
library(tidyverse)
library(stargazer)
library('rlist')
library(forecast)
library(tseries) 

# Model 1
data_1 <- read.csv(file='D:\\BIRMINGHAM\\STUDIES\\SEMESTER2\\STATMETHODS\\ADDITIONALTSK\\cps4_small.csv')

model_ols <- lm(log(wage) ~ educ + exper + I(exper^2) + I(exper*educ), data = data_1)
summary(model_ols)
cov1 <- hccm(model_ols, type="hc1") 
model_heterorobust <- coeftest(model_ols, vcov.=cov1) 
model_heterorobust
model_1_res = resid(model_ols)

# Model 2
model_ols2 <- lm(log(wage) ~ educ + exper + I(exper^2) + I(exper*educ) + married, data = data_1)
summary(model_ols2)
cov2 <- hccm(model_ols2, type="hc1") 
model_heterorobust_2 <- coeftest(model_ols2, vcov.=cov2) 
model_heterorobust_2
model_heterorobust_2[6]/model_heterorobust_2[12] < 2.326 #So we don't reject Ho, the same results we got from the table

# 3
model_1_res = resid(model_ols)
plot(data_1$married, resid(model_ols))
#It's hard to say for sure

# 4 Use Qoldfeld-Quandt test to answer the question for sure
li <- data_1[which(data_1$married == 1),]
hi <- data_1[which(data_1$married == 0),]
eqli <- lm(log(wage) ~ educ + exper + I(exper^2) + I(exper*educ), data=li)
eqhi <- lm(log(wage) ~ educ + exper + I(exper^2) + I(exper*educ), data=hi)

dfli <- eqli$df.residual
dfhi <- eqhi$df.residual

sigsqli <- glance(eqli)$sigma^2
sigsqhi <- glance(eqhi)$sigma^2

fstat <- sigsqli/sigsqhi
fstat
Fc <- qf(0.99, dfhi, dfli)
Fc
fstat < Fc #Since it's true we reject Ho -> there is heteroskedasticity
gqtest(model_ols, alternative="greater", order.by=data_1$married)
# 5. GLS
#For model 1
ehatsq <- resid(model_ols)^2  

#Don't understand what model do we have to use to estimate residual squared!!
w <- 1/data_1$exper
w
#Такой вектор?
model_fgls_known <- lm(log(wage) ~ educ + exper + I(exper^2) + I(exper*educ), weights=w, data=data_1)  #weight is 1/variance!!!! variance is called vari
summary(model_fgls_known)
summary(model_ols)

#For model 2
#Fit DLS when you know the model
model_fgls_known2 <- lm(log(wage) ~ educ + exper + I(exper^2) + I(exper*educ) + married, weights=w, data=data_1)  
summary(model_fgls_known2)
#Hard to say, as the term married is now significant with 10%.

# 6. Find intervals for marginal effects (educ)
#Marginal effect 1, when exper = 12
lambda = as.numeric(model_heterorobust[2]) + as.numeric(model_heterorobust[5])*25
lambda
se = sqrt(cov1[7] + 625*cov1[25]+2*25*cov1[10])
t_crit = 1.645
interval_1 = c(lambda-se*t_crit, lambda+se*t_crit) 
interval_1
#Marginal effect 2, when exper = 25
lambda_2 = as.numeric(model_fgls_known$coefficients[2]) + as.numeric(model_fgls_known$coefficients[5])*25
lambda_2
se_2 = sqrt(vcov(model_fgls_known)[7] + 625*vcov(model_fgls_known)[25]+2*25*vcov(model_fgls_known)[10])
t_crit = 1.645
interval_2 = c(lambda_2-se_2*t_crit, lambda_2+se_2*t_crit) 
interval_2
#When expirience increase then less wage increases with increase in 1 point in education

# 7. Plots
plot(data_1$educ, resid(model_ols))
plot(data_1$exper, resid(model_ols))
#For exper the variance looks (un)?stable (constant), meanwhile the variance from educ looks unctable

# 8. Breusch-Pegan test
bptest(log(wage) ~ educ + exper + I(exper^2) + I(exper*educ), data = data_1) 
#Since p-value less than 0.05, we reject H0 -> there is heteroskedasticity
#What do I conclude at 5% significance?

# 9. Estimate the variance function, estimate the standard deviation for each observation

ehatsq <- resid(model_ols)^2  

sighatsq.ols<- lm(log(ehatsq)~educ + exper + married, data=data_1)
summary(sighatsq.ols)
vari <- exp(fitted(sighatsq.ols)) #fitted sigmathat_sq
sd <- sqrt(vari)
sd[1:10]

# 10. Find the GLS estimates
model_fgls <- lm(log(wage) ~ educ + exper + I(exper^2) + I(exper*educ), weights=1/sd, data=data_1) 
summary(model_fgls)
stargazer(model_ols, model_heterorobust, model_fgls,
          header=FALSE, 
          title="Comparing various 'Wage' models",
          type="text",        
          keep.stat="n",      
          omit.table.layout="n",
          star.cutoffs=NA,
          digits=3, 
          intercept.bottom=FALSE,   
          column.labels=c("OLS","WHITE","FGLS"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption="Dependent variable: 'wage'",
          model.names=FALSE,
          star.char=NULL)   
# 11. Find intervals for marginal effect (exper)
# For Whtes's estimates
model_heterorobust
lambda_exper1 = as.numeric(model_heterorobust[3])+20*as.numeric(model_heterorobust[4])+16*as.numeric(model_heterorobust[5])
lambda_exper1
se_exper1 = sqrt(cov1[13] + 400*cov1[19] + 256*cov1[25] + 2*20*cov1[18] + 2*16*cov1[23]+2*16*20*cov1[20])
t_crit = 1.645
interval_exper1 = c(lambda_exper1-se_exper1*t_crit, lambda_exper1+se_exper1*t_crit) 
interval_exper1
# For GLS estimates
vcov(model_fgls)
lambda_exper2 = as.numeric(model_fgls$coefficients[3])+20*as.numeric(model_fgls$coefficients[4])+16*as.numeric(model_fgls$coefficients[5])
lambda_exper2
se_exper2 = sqrt(vcov(model_fgls)[13] + 400*vcov(model_fgls)[19] + 256*vcov(model_fgls)[25] + 2*20*vcov(model_fgls)[18] + 2*16*vcov(model_fgls)[23]+2*16*20*vcov(model_fgls)[20])
t_crit = 1.645
interval_exper2 = c(lambda_exper2-se_exper2*t_crit, lambda_exper2+se_exper2*t_crit) 
interval_exper2

# 12. Forecast the wage
pred_wage <- as.numeric(exp(model_ols2$coefficients[1]+ model_ols2$coefficients[2]*18 + model_ols2$coefficients[3]*16 + model_ols2$coefficients[4]*16*16 + 16*18*model_ols2$coefficients[5] + model_ols2$coefficients[6]))
pred_wage
corrected_pred <- pred_wage * exp(summary(model_ols2)$sigma^2/2) #is that the right sigma?
corrected_pred


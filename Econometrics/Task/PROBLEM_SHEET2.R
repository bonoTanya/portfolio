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

#Questin 2

data_1 <- read.csv(file='D:\\BIRMINGHAM\\STUDIES\\SEMESTER2\\STATMETHODS\\ASSIGNMENT1\\br2.csv')

# i. Run the model and save the residuals
model_ols <- lm(log(price) ~ I(sqft/100) + Age + I(Age^2), data = data_1)
summary(model_ols)
model_1_res = resid(model_ols)

# ii. Plot residuals against age and sqft/100 explanatory variables

plot(data_1$Age, model_1_res, ylab="Residuals", xlab="Age")
abline(0, 0)

plot((data_1$sqft)/100, model_1_res, ylab="Residuals", 
     xlab="Size of houses, hundreds of square feet") 
abline(0, 0)

#Look like there is heteroskedasticity as the variance of the residuals is not constant in both
#For Age variables the less age number the less variance, in opposite for size of houses, where the less size the bigger variance

# iii. Breusch-Pagan test
ressq <- resid(model_ols)^2         
modres <- lm(ressq~I(sqft/100) + Age, data=data_1) 
N <- nobs(modres)
gmodres <- glance(modres)   
S <- gmodres$df 
chisqcr <- qchisq(0.99, S) 
Rsqres <- gmodres$r.squared 
chisq <- N*Rsqres   
chisq
pval <- 1-pchisq(chisq,S)
pval

bptest(log(price) ~ I(sqft/100) + Age + I(Age^2), data = data_1)

#p-value = 0 - > reject Ho, there is a heteroskedasticity

# iv. Estimate variance function. White robust standard error

model_1_res_sqrd = model_1_res^2
data_1 <- cbind(data_1, model_1_res_sqrd)
estimate_alfa <- lm(log(model_1_res_sqrd) ~ Age + I(sqft/100), data = data_1)
estimate_alfa 

#White se

cov1 <- hccm(model_ols, type="hc1") 
model_wc <- coeftest(model_ols, vcov.=cov1) 
model_wc  

# v. Find variance estimates 

vari = exp(fitted(estimate_alfa))
model_gls <- lm(log(price) ~ I(sqft/100) + Age + I(Age^2), weights=1/estimate_var, data=data_1)
summary(model_gls) 


# vi. Visualization
stargazer(model_ols, model_wc, model_gls,
          header=FALSE, 
          title="Comparing various 'Price' models",
          type="text",        
          keep.stat="n",      
          omit.table.layout="n",
          star.cutoffs=NA,
          digits=3, 
          intercept.bottom=FALSE,   
          column.labels=c("OLS","WHITE","FGLS"),
          dep.var.labels.include = FALSE,
          model.numbers = FALSE,
          dep.var.caption="Dependent variable: 'price'",
          model.names=FALSE,
          star.char=NULL)           

# vii. Do Breusch-Pagan test to transformed model to check heteroskedasticity

bptest(log(price) ~ I(sqft/100) + Age + I(Age^2), data = data_1)


ressq <- resid(model_gls)^2         
modres <- lm(ressq~I(sqft/100) + Age, data=data_1) 
N <- nobs(modres)
gmodres <- glance(modres)   
S <- gmodres$df  
chisqcr <- qchisq(0.99, S) 
Rsqres <- gmodres$r.squared   
chisq <- N*Rsqres   
chisq
pval <- 1-pchisq(chisq,S)
pval
#p-value = 0 - > reject Ho, there is a heteroskedasticity


# Question 3
data_2 <- read.csv(file='D:\\BIRMINGHAM\\STUDIES\\SEMESTER2\\STATMETHODS\\ASSIGNMENT1\\growth47-test.csv')

# i. Estimate AR(2) model. Check the residuals correlations
data_2_ts <- ts(data_2, start=c(1947,2), end=c(2019,1),frequency=4)
data_2_ts
model_AR2 <- dynlm(g~L(g, 1:2), data=data_2_ts)
model_AR2
res <- resid(model_AR2)
acf(res)
# We see that r0 is significantly different from 0. Some of them are borderline cases, like r5, r9, r10, r11, r16, r18, r20 and the remainder are not significantly different from zero.
a <- bgtest(model_AR2, order=1, type="F", fill=0) 
b <- bgtest(model_AR2, order=1, type="F", fill=NA) 
c <- bgtest(model_AR2, order=1, type="Chisq", fill=0)
d <- bgtest(model_AR2, order=1, type="Chisq", fill=NA)
dfr <- data.frame(rbind(a[c(1,2,4)],
                        b[c(1,2,4)],
                        c[c(1,2,4)],
                        d[c(1,2,4)]
))
dfr <- cbind(c("1, F, 0", 
               "1, F, NA", "1, Chisq, 0", "1, Chisq, NA"), dfr)
names(dfr)<-c("Method", "Statistic", "Parameters", "p-Value")
dfr

# ii. repeat with AR(3)
model_AR3 <- dynlm(g~L(g, 1:3), data=data_2_ts)
model_AR3
res <- resid(model_AR3)
acf(res)

a <- bgtest(model_AR3, order=2, type="F", fill=0) 
b <- bgtest(model_AR3, order=2, type="F", fill=NA) 
c <- bgtest(model_AR3, order=2, type="Chisq", fill=0)
d <- bgtest(model_AR3, order=2, type="Chisq", fill=NA)
dfr <- data.frame(rbind(a[c(1,2,4)],
                        b[c(1,2,4)],
                        c[c(1,2,4)],
                        d[c(1,2,4)]
))
dfr <- cbind(c("2, F, 0", 
               "2, F, NA", "2, Chisq, 0", "2, Chisq, NA"), dfr)
names(dfr)<-c("Method", "Statistic", "Parameters", "p-Value")
dfr

# iii. Forecasting 

ar3g <- ar(data_2, aic=FALSE, order.max=3, method="ols")
fcst <- data.frame(forecast(ar3g, 3))

t <- qt(1-0.05/2, 284)
sig <- summary(model_AR3)$sigma
sig2 <- sig * sqrt(1+ model_AR3$coefficients[2]**2)
sig3 <- sig * sqrt((model_AR3$coefficients[2]**2 + model_AR3$coefficients[3])**2 + 1 + model_AR3$coefficients[2]**2)

a <- c('2019Q2', '2019Q3', '2019Q4')
b <- c(1.02,0.99,0.96)
c <- c(fcst$Point.Forecast[1]-t*sig, fcst$Point.Forecast[2]-t*sig2, fcst$Point.Forecast[3]-t*sig3)
d <- c(fcst$Point.Forecast[1]+t*sig, fcst$Point.Forecast[2]+t*sig2, fcst$Point.Forecast[3]+t*sig3)
dfr <- data.frame(a, b, c, d)
names(dfr)<-c("Quarter", "Actual growth", "Predicted min of Interval", "Predicted max of Interval")


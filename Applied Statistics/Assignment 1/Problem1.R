igfdata = read.csv("D:/BIRMINGHAM/RStudio/igfdata.csv", header=T)
library(ggplot2)
library(dplyr)
ggplot(igfdata, aes(x=age, y=igf)) +
  geom_point()+ labs(x="Age, years", y="Insulin-like growth factor")

ggplot(igfdata,aes(x=factor(sex), y=igf)) +geom_boxplot(colour="black", fill="blue")+ 
  labs(x=" ", y="Insulin-like growth factor") + 
  stat_summary(fun = mean, geom = "point", col = "red") +  
  stat_summary(fun = mean, geom = "text", col = "red",vjust = 1.5,
               aes(label = paste("Mean:", round(..y.., digits = 1))))+ 
  stat_summary(fun = sd, geom = "text", col = "white", vjust = -3, 
              aes(label = paste("Standart deviation:",round(..y.., digits = 1))))

##Consider significant difference in mean

m1 = 368.1
m2 = 310.9
sd1 = 167.3
sd2 = 169.7
num1 <- filter(igfdata, sex == "M")
num1 = nrow(num1)
num2 <- filter(igfdata, sex == "F")
num2 = nrow(num2)
se <- sqrt(sd1*sd1/num1+sd2*sd2/num2)
t <- (m1-m2)/se
pt(-t,min(num1,num2)-1)<0.05 

## p values for a one-sided test that is why df = n-1
##TRUE. That means that it is a significant difference

ggplot(igfdata, aes(x=age, y=igf)) +geom_point()+ labs(x="Age, years", 
                                                       y="Insulin-like growth factor")

linear_model1 = lm(igf~age, data = igfdata)
plot(igfdata$age,igfdata$igf, xlab="Age, years", 
     ylab="Insulin-like growth factor", col="blue")
abline(linear_model1)
summary(linear_model1)

##Model for ages less or equal to 15
igfdata <- igfdata %>%
  mutate(gender= if_else(sex == "M", 1, 0))
head(igfdata)
subset_age1 <- filter(igfdata, age <= 15)
linear_model2 = lm(log(igf)~age+gender, data = subset_age1)
plot(subset_age1$age,log(subset_age1$igf), xlab="Age, years", ylab="Insulin-like growth factor", col="blue")
abline(linear_model2)
summary(linear_model2)

##Residual plot
res <- resid(linear_model2)
plot(res, xlab="Index", ylab="Residual", col="blue")
abline(0,0)

##Model for ages greater then 15

subset_age2 <- filter(igfdata, age >15)
linear_model3 = lm(log(igf)~age+gender, data = subset_age2)
plot(subset_age2$age,log(subset_age2$igf), xlab="Age, years", ylab="Insulin-like growth factor", col="blue")
abline(linear_model3)
summary(linear_model3)

##Residual plot
res <- resid(linear_model2)
plot(res, xlab="Index", ylab="Residual", col="blue")
abline(0,0)

Model1 = 4.32 + 0.13*igfdata$age - 0.18*igfdata$gender
Model2 = 6.52 - 0.03*igfdata$age + 0.01*igfdata$gender

plot(igfdata$age, log(igfdata$igf), xlab="Age, years", ylab="Insulin-like growth factor", col="blue")
abline(linear_model2)
abline(linear_model3)

compareLM(linear_model2, linear_model3)

igfdata <- igfdata %>% mutate(linear_model2)
anova(linear_model2)
linear_model2

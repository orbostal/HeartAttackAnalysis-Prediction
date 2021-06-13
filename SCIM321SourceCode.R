## ANOVA test

rm(list=ls())
#Perform frequency table
data <- read.csv("D:/Com App/heart.csv", header = TRUE)
data.age <- (data[,1])
bins = seq(min(data.age)-1,max(data.age)+1,by=16)
amount_freq = cut(data.age,bins)
amount_freq = transform(table(amount_freq))
amount_freq = transform(amount_freq,rel_freq=prop.table(Freq),cum_freq=cumsum(Freq))
amount_freq

#Perform data visualization of range of the age
bins = seq(min(data.age)-1,max(data.age)+1,by=16)
age <- cut(data.age,bins)
transform(table(age))
plot((table(age)),type='h',main='The range of the age',ylab='Number of patients',lwd=25)

#Perform data visualization of relation in each quantitative variables
keeps = c("age","chol","trtbps","thalachh","caa")
df = heart[keeps]
pairs(df, panel = panel.smooth, main = "Relations of Quantitative Variables")

#Chol
chol = data$chol
age = c(rep(1,56),rep(2,168),rep(3,78))
range1 = data.frame(chol = chol, age = factor(age))

fit1 = aov(range1$chol ~ range1$age)
summary(fit1)
model.tables(fit1,"means")

library(DescTools)
tukey1=PostHocTest(fit1,method = "hsd")
tukey1
plot(tukey1)
library(agricolae)
print(HSD.test(fit1,"range1$age",group = TRUE))

#trtbps
blood = data$trtbps
range2 = data.frame(blood = blood, age = factor(age))

fit2 = aov(range2$blood ~ range2$age)
summary(fit2)
model.tables(fit2,"means")

tukey2=PostHocTest(fit2,method = "hsd")
tukey2
plot(tukey2)
print(HSD.test(fit2,"range2$age",group = TRUE))

#thalachh
heartrate = data$thalachh
range3 = data.frame(heartrate = heartrate, age = factor(age))

fit3 = aov(range3$heartrate ~ range3$age)
summary(fit3)
model.tables(fit3,"means")

tukey3=PostHocTest(fit3,method = "hsd")
tukey3
plot(tukey3)
print(HSD.test(fit3,"range3$age",group = TRUE))

## Logistic Regression Model

heart = read.csv("D:/Com App/heart.csv", header = TRUE)
fit2 = glm(output ~ factor(sex) + factor(cp) + trtbps + thalachh + factor(exng) + caa,
           data = heart, family=binomial())
summary(fit2)

fit1 = glm(output ~ factor(sex) + factor(cp) + trtbps + thalachh + factor(exng) 
           + caa + age + chol + factor(restecg) + factor(fbs),
           data = heart, family=binomial())
summary(fit1)
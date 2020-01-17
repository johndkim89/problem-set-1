## problem-set-1
See syllabus for submission details.
rm(list=ls())

install.packages("rmarkdown")
library(GGally)
library(OneR)

setwd("C:/Users/john_kim/Documents/ML Class/Day 1/")



LinearMpgCyl <- lm(mpg ~ cyl, data=mtcars)  # build linear regression model for mpg as a function of cyl 
summary(LinearMpgCyl)
print(LinearMpgCyl)

LinearMpgCylWt <- lm(mpg ~ cyl+wt, data = mtcars)
summary(LinearMpgCylWt)
print(LinearMpgCylWt)

CylWt <- mtcars$cyl * mtcars$wt
InteractLin <- lm(mpg ~ cyl+wt+CylWt, data = mtcars)
summary(InteractLin)
print(InteractLin)


plot(LinearMpgCylWt)
plot(InteractLin)
plot(LinearMpgCyl)


wd<- read.csv("wage_data.csv")

set.seed(0408)
polreg <- lm(wd$wage ~ poly(wd$age, 2))
summary(polreg)
plot(polreg)
predwd <- predict(polreg, data = wd)
line(wd$age, predict, lwd = 3, col = "red")

library(ggplot2)
ggplot(data = wd, aes(x = age, y = wage)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y~I(x^2)+I(x^1))




set.seed(1234)
plot(wage ~ age, data = wd)
polreg <- lm(wd$wage ~ poly(wd$age, 2))
predictmodel <- predict(polreg , interval="predict")

ix <- sort(wd$age, index.return=T)$ix
lines(wd$age[ix], predictmodel[ix , 1], col=2, lwd=2)
polygon(c(rev(wd$age[ix]), wd$age[ix]), c(rev(predictmodel[ix,3]), predictmodel[ix,2]), 
        col = rgb(0.7,0.7,0.7,0.4) , border = NA)



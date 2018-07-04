# LabStat2 02.12.2016

rm(list=ls()) 

# install.packages("ggplot2")
library(ggplot2)
# install.packages("moments")
library(moments)
# install.packages("lmtest")
library(lmtest)
# install.packages("MASS")
library(MASS)
# install.packages("GGally")
library(GGally)
# install.packages("car")
library(car)
# install.packages("ggfortify")
library(ggfortify)

########## DATA LOADING

?diamonds
data(diamonds)
str(diamonds)

########## SET DIVIDING

set.seed(23) #MEGA WA¯NE!!!!!
train= sample(1:nrow(diamonds),floor(0.6 * nrow(diamonds)))
diamonds_train=diamonds[train,]
diamonds_rest=diamonds[-train,]
query=sample(1:nrow(diamonds_rest),floor(0.5 * nrow(diamonds_rest)))
diamonds_query=diamonds_rest[query,]
diamonds_test=diamonds_rest[-query,]

str(diamonds_train)
summary(diamonds_train)

ggplot(diamonds_train, aes(x=price)) +
    geom_histogram(aes(y =..count..), colour="black", fill="white") +
    geom_vline(aes(xintercept=mean(price), color="blue"), linetype="dashed", size=1) +
    geom_vline(aes(xintercept=median(price), color="green"), linetype="dashed", size=1) +
    labs(title="Histogram", x="Cena [USD]", y="Licznoœæ") +
    scale_colour_manual(name="Statystyki",values=c('green'='green','blue'='blue'),
                        labels=c(paste('Mediana =', median(diamonds_train$price)),
                                 paste('Œrednia =', round(mean(diamonds_train$price)),2))) +
    annotate("text", x=15000, y=6000, label=paste("Occhylenie std. =", round(sqrt(var(diamonds_train$price)), 2))) +
    annotate("text", x=15000, y=6500, label=paste("Skoœnoœæ =", round(skewness(diamonds_train$price), 2))) +
    annotate("text", x=15000, y=7000, label=paste("Kurtoza =", round(kurtosis(diamonds_train$price), 2)))

diamonds_train$price <- log(diamonds_train$price)

ggplot(diamonds_train, aes(x=price)) +
    geom_histogram(aes(y =..count..), colour="black", fill="white") +
    geom_vline(aes(xintercept=mean(price), color="blue"), linetype="dashed", size=1) +
    geom_vline(aes(xintercept=median(price), color="green"), linetype="dashed", size=1) +
    labs(title="Histogram", x="Cena [USD]", y="Licznoœæ") +
    scale_colour_manual(name="Statystyki",values=c('green'='green','blue'='blue'),
                        labels=c(paste('Mediana =', median(diamonds_train$price)),
                                 paste('Œrednia =', round(mean(diamonds_train$price)),2)))

ggplot(diamonds_train, aes(price, carat)) +
    geom_point() +
    geom_smooth()
?lm
lm.diamonds_train <- lm(price~., data=diamonds_train)
summary(lm.diamonds_train)
autoplot(lm.diamonds_train, which = 1:6, ncol = 3, label.size = 1)



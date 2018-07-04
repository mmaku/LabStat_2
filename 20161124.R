# LabStat2 24.11.2016

rm(list = ls()) 


# install.packages("PBImisc")
# install.packages("MASS")
# install.packages("ggplot2")
library(PBImisc)
library(MASS)
library(ggplot2)

#################################################################################################################
############################################ ZADANIE 1 ##########################################################
#################################################################################################################

#### WORKING DIRECTORY ######################################

MyPath        = "C:/Users/Michal/Documents/LabStat2/" 
setwd(MyPath)

#### DATA LOADING ###########################################

tempPath = paste(MyPath, "datasets/palce.csv", sep = "")
palce   = read.csv2(tempPath, sep = ";", dec = ",", head = T)

#### ANALIZA ###############################################

attach(palce)
head(palce)

#### JEDNOCZYNNIKOWA ANOVA #################################

m1=lm(dr~as)
model.matrix(m1)
summary(m1)
sum(m1$residuals^2)
sum(lm(dr~1)$residuals^2)
anova(lm(dr~1),m1)
anova(m1)
anova(lm(dr~ch))
summary(lm(dr~ch))
t.test(dr~ch,var.equal=TRUE)

#### 2-WAY ANOVA ###########################################

m2=lm(dr~as+ch)
model.matrix(m2)
anova(m2)
anova(lm(dr~1),m1,m2)
drop1(lm(dr~as+ch),test="F")
add1(lm(dr~1),scope=~as+ch,test="F")

#### 2-WAY ANOVA Z INTERAKCJIAMI ###########################

m3=lm(dr~as*ch) # alternatywnie: lm(dr~as+ch+as:ch)
model.matrix(m3)
drop1(m3,test="F")
step(m3)
anova(lm(dr~1),m1,m2,m3)
anova(lm(dr~1),lm(dr~ch),m2,m3)
anova(lm(dr~1),m2)
anova(lm(dr~1),m3)

#### TESTOWANIE NORMALNOŒCI ###############################

m3$residuals->e
shapiro.test(e)
qqnorm(e)
hist(e)
bartlett.test(dr~paste(as,ch))

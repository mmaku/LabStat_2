# LabStat2 03.11.2016

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

tempPath = paste(MyPath, "datasets/wordrecall.txt", sep = "")
wrdrcl   = read.csv(tempPath, sep = "")

#### ANALIZA ###############################################

summary(wrdrcl)
lm.wrdrcl = lm(prop~time, data=wrdrcl)
summary(lm.wrdrcl)

#### WYKRESY ###############################################

ggplot(wrdrcl, aes(x=time, y=prop)) + geom_point()
ggplot(wrdrcl, aes(x=log(time), y=prop)) + geom_point()
ggplot(wrdrcl,aes(x=time, y=prop)) +  geom_point() +
stat_smooth(method = "lm", col = "red", fullrange = T)

par(mfrow=c(2,3))
plot(lm.wrdrcl, 1:6)
par(mfrow=c(1,1))

#### TRANSFORMACJA BOXA-COXA ###############################

boxcox(data = wrdrcl, prop~time)

lm.wrdrclBC = lm(prop~log(time), data=wrdrcl)
summary(lm.wrdrclBC)
par(mfrow=c(2,3))
plot(lm.wrdrclBC, 1:6)
par(mfrow=c(1,1))

#################################################################################################################
############################################ ZADANIE 2 ##########################################################
#################################################################################################################

#### WORKING DIRECTORY ######################################

MyPath        = "C:/Users/Michal/Documents/LabStat2/" 
setwd(MyPath)

#### DATA LOADING ###########################################

tempPath           = paste(MyPath, "datasets/mammgest.txt", sep = "")
mammgest           = read.csv(tempPath, sep = "", fileEncoding = "UTF-16")
rownames(mammgest) = mammgest$Mammals

#### ANALIZA ###############################################

summary(mammgest)
lm.mammgest = lm(Gestation~Birthwgt, data=mammgest)
summary(lm.mammgest)

#### WYKRESY ###############################################

ggplot(mammgest, aes(x=Birthwgt, y=Gestation, col=Mammal)) + geom_point(size = 5)
ggplot(mammgest, aes(x=Birthwgt, y=Gestation, col=Mammal)) +  geom_point(size = 3) +
    stat_smooth(method = "lm", col = "red", fullrange = T)

par(mfrow=c(2,3))
plot(lm.mammgest, 1:6)
par(mfrow=c(1,1))

#### TRANSFORMACJA BOXA-COXA ###############################

boxcox(data = mammgest, Gestation~Birthwgt)

lm.mammgestBC = lm(log(Gestation)~Birthwgt, data=mammgest)
summary(lm.mammgestBC)
par(mfrow=c(2,3))
plot(lm.mammgestBC, 1:6)
par(mfrow=c(1,1))


#################################################################################################################
############################################ ZADANIE 3 ##########################################################
#################################################################################################################

#### DATA LOADING ###########################################

data(women)
women

#### ANALIZA ###############################################

summary(women)
lm.women = lm(weight~height, data=women)
summary(lm.women)

#### WYKRESY ###############################################

ggplot(women, aes(x=height, y=weight)) + geom_point()
ggplot(women, aes(x=height, y=weight)) +  geom_point() +
    stat_smooth(method = "lm", col = "red", fullrange = T)

par(mfrow=c(2,3))
plot(lm.women, 1:6)
par(mfrow=c(1,1))

#### REGRESJA WIELOMIANOWA ################################

lm.womenSQ = lm(weight~height+I(height^2), data=women)
summary(lm.womenSQ)
par(mfrow=c(2,3))
plot(lm.womenSQ, 1:6)
par(mfrow=c(1,1))

#### REGRESJA WIELOMIANOWA BEZ 15-ego ELEMENTU ############

lm.womenSQ = lm(weight~height+I(height^2), data=women, subset = -15)
summary(lm.womenSQ)
par(mfrow=c(2,3))
plot(lm.womenSQ, 1:6)
par(mfrow=c(1,1))

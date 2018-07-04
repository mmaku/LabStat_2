# LabStat2 08.12.2016r.

# install.packages("ggplot2")
library(ggplot2)
# install.packages("GGally")
library(GGally)
# install.packages("glmnet")
library(glmnet)

url <- "http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.data"
pcancer <- read.csv(url(url), header=TRUE, sep="\t", row.names = 1)
str(pcancer)
summary(pcancer)

train <- pcancer[which(pcancer$train),1:9]
validation <- pcancer[-which(pcancer$train),1:9]
# ggpairs(train)

url2 <- url("http://statweb.stanford.edu/~tibs/ElemStatLearn/datasets/prostate.info.txt")
cat(paste(readLines(url2), collapse="\n"))

# BIC

n <- nrow(train)
modelBIC <- step(lm(lpsa~., data=train), direction="both", k=log(n))

# AIC

modelAIC <- step(lm(lpsa~., data=train), direction="both")

# par(mfrow=c(2,3))
# autoplot(modelBIC, which = 1:6, ncol = 3, label.size = 1)

results <- matrix(nrow = 2^8-1, ncol = 3)
i <- 1
for(j in 1:8)
{
    combi <- combn(colnames(train[,-9]), j)
    
    for(k in 1:ncol(combi))
    {
        results[i,1] <- paste("lpsa ~ ", paste(combi[,k], collapse= "+"))
        results[i,2] <- AIC(lm(as.formula(results[i,1]), data = train))
        results[i,3] <- BIC(lm(as.formula(results[i,1]), data = train))
        i <- i+1
    }
}


# head(results[order(results[,2]),])
# head(results[order(results[,3]),])

bestAICformula <- as.formula(results[order(results[,2]),][1,1])
bestBICformula <- as.formula(results[order(results[,3]),][1,1])

step(lm(bestAICformula, data=train),direction="both") 
step(lm(bestBICformula, data=train),direction="both", k=log(n))

formula(step(lm(bestAICformula, data=train),direction="both")) == formula(modelAIC)
formula(step(lm(bestBICformula, data=train),direction="both", k=log(n))) == formula(modelBIC)

# CROSS WALIDACJA

# set.seed(23) #MEGA WA¯NE!!!!!
# intCount <- 10
# crossSample <- train[sample(nrow(train)),]
# resultsCross <- matrix(nrow = 2^8-1, ncol = 3)
# 
# 
# for(i in 1:intCount)
# {
#     testSet <- crossSample[c(ceiling(nrow(crossSample)/intCount):floor(nrow(crossSample)/intCount)),]
#     diamonds_train=diamonds[train,]
#     diamonds_rest=diamonds[-train,]
#     i <- 1
#     for(j in 1:8)
#     {
#         combi <- combn(colnames(train[,-9]), j)
#         
#         for(k in 1:ncol(combi))
#         {
#             results[i,1] <- paste("lpsa ~ ", paste(combi[,k], collapse= "+"))
#             results[i,2] <- AIC(lm(as.formula(results[i,1]), data = train))
#             results[i,3] <- BIC(lm(as.formula(results[i,1]), data = train))
#             i <- i+1
#         }
#     }
# }
# 

train.lasso=glmnet(x=as.matrix(train[,-ncol(train)]), y = train$lpsa, alpha = 1)
train.ridge=glmnet(x=as.matrix(train[,-ncol(train)]), y = train$lpsa, alpha = 0)
plot(train.lasso, xvar = "lambda")

train.lasso.cv=cv.glmnet(x=as.matrix(train[,-ncol(train)]), y = train$lpsa, alpha = 1)
train.ridge.cv=cv.glmnet(x=as.matrix(train[,-ncol(train)]), y = train$lpsa, alpha = 0)

lambdamin.lasso=train.lasso.cv$lambda.min
lambdamin.ridge=train.ridge.cv$lambda.min

train.lasso=glmnet(x=as.matrix(train[,-ncol(train)]), y = train$lpsa, alpha = 1, lambda=lambdamin.lasso)
train.ridge=glmnet(x=as.matrix(train[,-ncol(train)]), y = train$lpsa, alpha = 0, lambda=lambdamin.lasso)



                
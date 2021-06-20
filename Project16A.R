##### Coursework Part 3 #####

#setting data source directory
setwd("C:/Users/User/Documents/Machine Learning R Project")

#load marketing dataset
bank=read.table("bank.csv",sep=";",header=TRUE)

#load library
library(rpart)
library(rpart.plot)
library(dplyr)
library(caTools)
library(ggplot2)
library(lattice)
library(caret)
library(splitstackshape)
library(carData)
library(car)
library(boot)
library(e1071)
library(kernlab)
library(ggcorrplot)
library(polycor)
library(prodlim)
library(ROCit)
library(pROC)
library(ROCR)
library(randomForest)



#transform data
dat3.raw <- bank
dat3.raw[,c(2:5,7:11,16:17)] <- lapply(dat3.raw[,c(2:5,7:11,16:17)], as.factor)
dat3.redefined <- as.data.frame(dat3.raw)
dat3.y.column.reorder <- relevel(dat3.redefined$y, "yes")

info.dat3.y <- summary(dat3.redefined$y)

#set seed
set.seed(1004)

#stratified train test split by yes/no for term deposit subscription
strf.train.test.set.dat3 <- stratified(dat3.redefined, c("y"), 
                                  0.7, bothSets = TRUE)
strf.train.dat3 <- strf.train.test.set.dat3$SAMP1
strf.test.dat3 <- strf.train.test.set.dat3$SAMP2



#Logistic regression (LGT) for Binary Classification (Yes/No)
lgt.strf.train.dat3 <- glm(y ~  +age +job +marital +education +default  
                           +balance +housing +loan +contact +day      
                           +month +campaign +pdays +previous +poutcome, 
                           strf.train.dat3, family = "binomial"(link = "logit"))


summary(lgt.strf.train.dat3)
par(mfrow = c(2,2))
plot(lgt.strf.train.dat3)
par(mfrow = c(1,1))

lgt.strf.train.threshold <- 0.50
lgt.strf.test.threshold <- 0.50

train.predict.lgt.dat3.probvals <- predict(lgt.strf.train.dat3, strf.train.dat3, type = "response")
lgt.strf.train.yes.prediction <- ifelse(train.predict.lgt.dat3.probvals > 
                                         lgt.strf.train.threshold, 
                                       "yes", "no")
lgt.strf.train.yes.prediction.factor <- 
  as.factor(lgt.strf.train.yes.prediction)

cfm.lgt.train.dat3 <- confusionMatrix(lgt.strf.train.yes.prediction.factor, strf.train.dat3$y)
print(cfm.lgt.train.dat3)


predict.train.lgt.dat3.probsvals.resps <- cbind(train.predict.lgt.dat3.probvals, lgt.strf.train.yes.prediction.factor, strf.train.dat3$y)
roc.train.predict.lgt.dat3 <- roc(predict.train.lgt.dat3.probsvals.resps[,3] ~ as.numeric(predict.train.lgt.dat3.probsvals.resps[,1]), 
                            plot = TRUE, print.auc = TRUE, col = "gray60")





predict.lgt.dat3.probvals <- predict(lgt.strf.train.dat3, strf.test.dat3, type = "response")
lgt.strf.test.yes.prediction <- ifelse(predict.lgt.dat3.probvals > 
                                           lgt.strf.test.threshold, 
                                         "yes", "no")
lgt.strf.test.yes.prediction.factor <- 
  as.factor(lgt.strf.test.yes.prediction)

cfm.lgt.test.dat3 <- confusionMatrix(lgt.strf.test.yes.prediction.factor, strf.test.dat3$y)
print(cfm.lgt.test.dat3)


predict.lgt.dat3.probsvals.resps <- cbind(predict.lgt.dat3.probvals, lgt.strf.test.yes.prediction.factor, strf.test.dat3$y)
roc.predict.lgt.dat3 <- roc(predict.lgt.dat3.probsvals.resps[,3] ~ as.numeric(predict.lgt.dat3.probsvals.resps[,1]), 
                              plot = TRUE, print.auc = TRUE, col = "gray60")


#multi collinearity (MC) check
vif(lgt.strf.train.dat3)
##no MC

#perform step wise logistic regression (LogR)
swlgt.strf.train.dat3 <- step(lgt.strf.train.dat3, direction = "both")
summary(swlgt.strf.train.dat3)

#multi collinearity (MC) check
vif(swlgt.strf.train.dat3)
##all variables contain GVIF value < 2; no multi-collinearity

swlgt.strf.train.probvals <- predict(swlgt.strf.train.dat3, type = "response")
swlgt.strf.test.yes.prediction <- ifelse(swlgt.strf.train.probvals > 
                                         lgt.strf.test.threshold, 
                                       "yes", "no")

#optimise probability threshold 



swlgt.strf.train.threshold <- 0.50

swlgt.strf.train.yes.prediction <- ifelse(swlgt.strf.train.probvals > 
                                            swlgt.strf.train.threshold, 
                                         "yes", "no")
swlgt.strf.train.yes.prediction.factor <- 
  as.factor(swlgt.strf.train.yes.prediction)

train.predict.swlgt.dat3.probvals <- predict(swlgt.strf.train.dat3, strf.train.dat3, type = "response")
train.predict.swlgt.dat3.probsvals.resps <- cbind(train.predict.swlgt.dat3.probvals, swlgt.strf.train.yes.prediction.factor, strf.train.dat3$y)
roc.train.predict.swlgt.dat3 <- roc(train.predict.swlgt.dat3.probsvals.resps[,3] ~ as.numeric(train.predict.swlgt.dat3.probsvals.resps[,1]), 
                              plot = TRUE, print.auc = TRUE, col = "gray60")


cfm.swlgt.train.dat3 <- confusionMatrix(swlgt.strf.train.yes.prediction.factor,strf.train.dat3$y)
print(cfm.swlgt.train.dat3)

swlgt.strf.test.threshold <- 0.50


predict.swlgt.dat3.probvals <- predict(swlgt.strf.train.dat3, strf.test.dat3, type = "response")
swlgt.strf.test.yes.prediction <- ifelse(predict.swlgt.dat3.probvals > 
                                            swlgt.strf.test.threshold, 
                                          "yes", "no")
swlgt.strf.test.yes.prediction.factor <- 
  as.factor(swlgt.strf.test.yes.prediction)

cfm.swlgt.test.dat3 <- confusionMatrix(swlgt.strf.test.yes.prediction.factor, strf.test.dat3$y)
print(cfm.swlgt.test.dat3)


predict.swlgt.dat3.probsvals.resps <- cbind(predict.swlgt.dat3.probvals, swlgt.strf.test.yes.prediction.factor, strf.test.dat3$y)
roc.predict.swlgt.dat3 <- roc(predict.swlgt.dat3.probsvals.resps[,3] ~ as.numeric(predict.swlgt.dat3.probsvals.resps[,1]), 
                                  plot = TRUE, print.auc = TRUE, col = "gray60")

measure.acc.cutoff <- measureit(score = swlgt.strf.train.dat3$fitted.values, 
                                class = swlgt.strf.train.dat3$y,
                     measure = c("ACC", "SENS", "FSCR"))

measure.sens.spec <- measureit(score = swlgt.strf.train.dat3$fitted.values, 
                               class = swlgt.strf.train.dat3$y,
                                measure = c("TPR", "FPR", "FSCR"))

names(measure.acc.cutoff)
names(measure.sens.spec)

#base train set SWLGT ROC
roc.train.predict.swlgt.dat3 <- roc(train.predict.swlgt.dat3.probsvals.resps[,3] ~ as.numeric(train.predict.swlgt.dat3.probsvals.resps[,1]), 
                                    plot = TRUE, print.auc = TRUE, col = "gray60")


# Add ROC test set SWLGT ROC
lines(roc.predict.swlgt.dat3, type="l", pch=2, col="darkgreen", bg="grey", lwd = 2)



plot(measure.acc.cutoff$ACC~measure.acc.cutoff$Cutoff, type = "l")
abline(coef = c(0,1))
plot(measure.sens.spec$TPR~measure.sens.spec$Cutoff, type = "l")
abline(coef = c(0,1))
plot(measure.sens.spec$TNR~measure.sens.spec$Cutoff, type = "l")
abline(coef = c(0,1))
plot(measure.sens.spec$TPR~measure.sens.spec$FPR, type = "l")
abline(coef = c(0,1))

#LogR model binary choice (yes/no) pseudo R^2; comparisons

lgt.strf.train.dat3.pseudoR.sq <-
  1 - (lgt.strf.train.dat3$deviance/lgt.strf.train.dat3$null.deviance)
print(lgt.strf.train.dat3.pseudoR.sq)


swlgt.strf.train.dat3.pseudoR.sq <- 
  1 - (swlgt.strf.train.dat3$deviance/swlgt.strf.train.dat3$null.deviance)
print(swlgt.strf.train.dat3.pseudoR.sq)


#CART analysis

tree.strf.train.dat3 <- rpart(y ~ +age +job +marital +education +default  
                      +balance +housing +loan +contact +day      
                      +month +campaign +pdays +previous +poutcome, 
                      strf.train.dat3, method = "class", control = 
                        rpart.control(minsplit = 2, xval = 10, cp = 0))


#display CART (using rpart package and standard R)
rpart.plot(tree.strf.train.dat3, nn = T, main = "Maximal Tree for Yes/No Subscription", cex = 0.5)
plot(tree.strf.train.dat3, main = "Maximal Tree for Yes/No Subscription")
text(tree.strf.train.dat3, pretty=0, cex = 0.35)


#display CART result in values
print(tree.strf.train.dat3)

#display Complexity Parameter (CP) result of pruning sequence and 10-fold cross-validation (CV) errors
printcp(tree.strf.train.dat3)

#display relationship between pruning sequence and 10-fold CV errors
#note(I): the CP values on the x-axis  are based on geometric mean
#note(II): the size of trees on the x-axis indicates number of terminal nodes
plotcp(tree.strf.train.dat3)

#identify optimal CP value based on formula below
#CV error cap = minimum CV error + positive value of 1 Standard Error (SE)
tree.strf.train.dat3.min.CV.error <- tree.strf.train.dat3$cptable[which.min(tree.strf.train.dat3$cptable[,"xerror"]), "xerror"]
tree.strf.train.dat3.CV.error.cap <- tree.strf.train.dat3$cptable[which.min(tree.strf.train.dat3$cptable[,"xerror"]), "xerror"] +
  tree.strf.train.dat3$cptable[which.min(tree.strf.train.dat3$cptable[,"xerror"]), "xstd"]

#optimal CP region whose CV error falls below the CV error cap 
i <- 1; j <- 4
while (tree.strf.train.dat3$cptable[i,j] > tree.strf.train.dat3.CV.error.cap) {
  i <- i + 1
}

#CP value for minimum cross-validation error
k <- match(tree.strf.train.dat3.min.CV.error, tree.strf.train.dat3$cptable[,4], nomatch = NA)

#obtain geometric mean of the identified CP values in the optimal region
optimal.cp.tree.strf.train.dat3 <- ifelse(i > 1, sqrt(tree.strf.train.dat3$cptable[i,1] * tree.strf.train.dat3$cptable[i-1,1]), 1)
cp.tree.strf.train.dat3 <- ifelse(k > 1, sqrt(tree.strf.train.dat3$cptable[k,1] * tree.strf.train.dat3$cptable[k-1,1]), 1)


#prune tree based on optimal CP
tree.strf.train.dat3.pruned <- prune(tree.strf.train.dat3, cp = optimal.cp.tree.strf.train.dat3)
rpart.plot(tree.strf.train.dat3.pruned, nn = T, 
           box.palette = (c("red","green")),
           main = "CART Analysis for Yes/No Subscription (Pruned)")

#prune tree based on minimum cross validation error
tree.strf.train.dat3.pruned2 <- prune(tree.strf.train.dat3, cp = cp.tree.strf.train.dat3)
rpart.plot(tree.strf.train.dat3.pruned2, nn = T, 
           box.palette = (c("red","green")),
           main = "CART Analysis for Yes/No Subscription (Pruned)")

train.predict.tree.strf.dat3 <- predict(tree.strf.train.dat3.pruned, strf.train.dat3, type = "class")
cfm.train.tree.strf.train.dat3 <- confusionMatrix(train.predict.tree.strf.dat3, strf.train.dat3$y)
print(cfm.train.tree.strf.train.dat3)

train.predict.tree.strf.dat3.probvals <- predict(tree.strf.train.dat3.pruned, strf.train.dat3, type = "prob")
train.predict.tree.strf.dat3.probsvals.resps <- cbind(train.predict.tree.strf.dat3.probvals, train.predict.tree.strf.dat3, strf.train.dat3$y)
roc.train.predict.tree.strf.dat3 <- roc(train.predict.tree.strf.dat3.probsvals.resps[,4] ~ as.numeric(train.predict.tree.strf.dat3.probvals[,2]), 
                                  plot = TRUE, print.auc = TRUE, col = "gray60")



predict.tree.strf.dat3 <- predict(tree.strf.train.dat3.pruned, strf.test.dat3, type = "class")
cfm.tree.strf.train.dat3 <- confusionMatrix(predict.tree.strf.dat3, strf.test.dat3$y)
print(cfm.tree.strf.train.dat3)

predict.tree.strf.dat3.probvals <- predict(tree.strf.train.dat3.pruned, strf.test.dat3, type = "prob")
predict.tree.strf.dat3.probsvals.resps <- cbind(predict.tree.strf.dat3.probvals, predict.tree.strf.dat3, strf.test.dat3$y)
roc.predict.tree.strf.dat3 <- roc(predict.tree.strf.dat3.probsvals.resps[,4] ~ as.numeric(predict.tree.strf.dat3.probvals[,2]), 
                             plot = TRUE, print.auc = TRUE, col = "gray60")


train.predict2.tree.strf.dat3 <- predict(tree.strf.train.dat3.pruned2, strf.train.dat3, type = "class")
cfm2.train.tree.strf.train.dat3 <- confusionMatrix(train.predict2.tree.strf.dat3, strf.train.dat3$y)
print(cfm2.train.tree.strf.train.dat3)

train.predict2.tree.strf.dat3.probvals <- predict(tree.strf.train.dat3.pruned2, strf.train.dat3, type = "prob")
train.predict2.tree.strf.dat3.probsvals.resps <- cbind(train.predict2.tree.strf.dat3.probvals, train.predict2.tree.strf.dat3, strf.train.dat3$y)
roc.train.predict2.tree.strf.dat3 <- roc(train.predict2.tree.strf.dat3.probsvals.resps[,4] ~ as.numeric(train.predict2.tree.strf.dat3.probsvals.resps[,2]), 
                                   plot = TRUE, print.auc = TRUE, col = "gray60")


predict2.tree.strf.dat3 <- predict(tree.strf.train.dat3.pruned2, strf.test.dat3, type = "class")
cfm2.tree.strf.train.dat3 <- confusionMatrix(predict2.tree.strf.dat3, strf.test.dat3$y)
print(cfm2.tree.strf.train.dat3)

predict2.tree.strf.dat3.probvals <- predict(tree.strf.train.dat3.pruned2, strf.test.dat3, type = "prob")
predict2.tree.strf.dat3.probsvals.resps <- cbind(predict2.tree.strf.dat3.probvals, predict2.tree.strf.dat3, strf.test.dat3$y)
roc.predict2.tree.strf.dat3 <- roc(predict2.tree.strf.dat3.probsvals.resps[,4] ~ as.numeric(predict2.tree.strf.dat3.probsvals.resps[,2]), 
                                  plot = TRUE, print.auc = TRUE, col = "gray60")


#Boosting Decision Tree

strf.train.dat3.ycol.code.column <- ifelse(strf.train.dat3$y =="yes",1,0)
strf.train.dat3.ycol.coded <- cbind(strf.train.dat3,strf.train.dat3.ycol.code.column)

strf.test.dat3.ycol.code.column <- ifelse(strf.test.dat3$y =="yes",1,0)
strf.test.dat3.ycol.coded <- cbind(strf.test.dat3,strf.test.dat3.ycol.code.column)


boost.dat3 <- gbm(strf.train.dat3.ycol.coded$strf.train.dat3.ycol.code.column
                  ~ +age +job +marital +education +default  
                      +balance +housing +loan +contact +day      
                      +month +campaign +pdays +previous +poutcome, 
                      strf.train.dat3.ycol.coded, 
                      distribution = "bernoulli", n.trees = 10000, shrinkage = 0.00125,
                      interaction.depth = 5)
##requires higher computational power for boost process


boost.train.predict.dat3 <- predict(boost.dat3, strf.train.dat3.ycol.coded, n.trees = 5000)

boost.train.predict.dat3.probvals <- predict(boost.dat3, strf.train.dat3.ycol.coded, 
                                             n.trees = 5000, type = "response")
boost.train.predict.dat3.probvals.resps <- cbind(boost.train.predict.dat3.probvals, 
                                                 boost.train.predict.dat3, 
                                                 strf.train.dat3.ycol.coded$strf.train.dat3.ycol.code.column)
roc.boost.train.predict.dat3 <- roc(boost.train.predict.dat3.probvals.resps[,3] ~ as.numeric(boost.train.predict.dat3.probvals.resps[,1]), 
                                  plot = TRUE, print.auc = TRUE, col = "gray60")



boost.test.predict.dat3 <- predict(boost.dat3, strf.test.dat3.ycol.coded, n.trees = 5000)
boost.test.predict.dat3.link <- predict(boost.dat3, strf.test.dat3.ycol.coded, n.trees = 5000, type = "link")

boost.test.predict.dat3.probvals <- predict(boost.dat3, strf.test.dat3.ycol.coded, 
                                             n.trees = 5000, type = "response")
boost.test.predict.dat3.probvals.resps <- cbind(boost.test.predict.dat3.probvals, 
                                                 boost.test.predict.dat3, 
                                                 strf.test.dat3.ycol.coded$strf.test.dat3.ycol.code.column)
roc.boost.test.predict.dat3 <- roc(boost.test.predict.dat3.probvals.resps[,3] ~ as.numeric(boost.test.predict.dat3.probvals.resps[,1]), 
                                    plot = TRUE, print.auc = TRUE, col = "gray60")

boost.dat3.test.threshold <- 0.50

boost.dat3.test.yes.prediction <- ifelse(boost.test.predict.dat3.probvals > 
                                           boost.dat3.test.threshold, 
                                         "yes", "no")
boost.dat3.test.yes.prediction.factor <- 
  as.factor(boost.dat3.test.yes.prediction)

cfm.boost.test.dat3 <- confusionMatrix(boost.dat3.test.yes.prediction.factor, strf.test.dat3$y)
print(cfm.boost.test.dat3)


# Base Plot of ROC Boost Train
roc.boost.train.predict.dat3 <- roc(boost.train.predict.dat3.probvals.resps[,3] ~ as.numeric(boost.train.predict.dat3.probvals.resps[,1]), 
                                    plot = TRUE, print.auc = TRUE, col = "green1", asp = NA, main = "ROC for CART vs Gradient Boosted Tree - Train Data")

# Add ROC train set CART
lines(roc.train.predict2.tree.strf.dat3, type="l", pch=2, col="red1", bg="grey", lwd = 1)



# Base Plot of ROC Boost Test
roc.boost.test.predict.dat3 <- roc(boost.test.predict.dat3.probvals.resps[,3] ~ as.numeric(boost.test.predict.dat3.probvals.resps[,1]), 
                                   plot = TRUE, print.auc = TRUE, col = "green1", asp = NA, main = "ROC for CART vs Gradient Boosted Tree - Test Data")

# Add ROC test set CART
lines(roc.predict2.tree.strf.dat3, type="l", pch=2, col="red1", lwd = 1)



#Random Forest

rdfr.dat3 <- randomForest(y ~ +age +job +marital +education +default  
                          +balance +housing +loan +contact +day      
                          +month +campaign +pdays +previous +poutcome, 
                          strf.train.dat3, mtry = 13, ntree = 1000, proximity = TRUE)


#Variable Importance determining subscription
#mean decrease gini indicates how well of how each variable contributes to the 
#homogeneity of the nodes and leaves in the resulting random forest
varImpPlot(rdfr.dat3, main = "Variable Importance Determining Subscription")

rdfr.train.predict.dat3 <- predict(rdfr.dat3, strf.train.dat3, type = "response")
cfm.rdfr.train.predict.dat3 <- confusionMatrix(rdfr.train.predict.dat3, strf.train.dat3$y)
print(cfm.rdfr.train.predict.dat3)

rdfr.train.predict.dat3.probvals <- as.data.frame(predict(rdfr.dat3, strf.train.dat3, type = "prob"))
rdfr.train.predict.dat3.probsvals.resps <- cbind(rdfr.train.predict.dat3.probvals,rdfr.train.predict.dat3,strf.train.dat3$y)


rdfr.predict.dat3 <- predict(rdfr.dat3, strf.test.dat3, type = "response")
cfm.rdfr.predict.dat3 <- confusionMatrix(rdfr.predict.dat3, strf.test.dat3$y)
print(cfm.rdfr.predict.dat3)

rdfr.predict.dat3.probvals <- as.data.frame(predict(rdfr.dat3, strf.test.dat3, type = "prob"))
rdfr.predict.dat3.probsvals.resps <- cbind(rdfr.predict.dat3.probvals,rdfr.predict.dat3,strf.test.dat3$y)


#RDFR ROC curve

roc.rdfr.train.predict.dat3 <- roc(rdfr.train.predict.dat3.probsvals.resps$`strf.train.dat3$y` ~ 
                               as.numeric(rdfr.train.predict.dat3.probsvals.resps$no), 
                             plot = TRUE, print.auc = TRUE, col = "purple")

roc.rdfr.predict.dat3 <- roc(rdfr.predict.dat3.probsvals.resps$`strf.test.dat3$y` ~ 
                               as.numeric(rdfr.predict.dat3.probsvals.resps$no), 
                plot = TRUE, print.auc = TRUE, col = "gray60")


#SVM

dat3.dummy.vars <- dummyVars(~. , strf.train.dat3[,-17])
dat3.train.dummy <- predict(dat3.dummy.vars, strf.train.dat3[,-17])
dat3.dummy.training <- as.data.frame(cbind(strf.train.dat3[,17], 
                                           dat3.train.dummy))
colnames(dat3.dummy.training)[4] <- "job.blue_collar"
colnames(dat3.dummy.training)[9] <- "job.self_employed"

dat3.dummy.vars.test <- dummyVars(~. , strf.test.dat3[,-17])
dat3.non.dummy.vars.test <- strf.test.dat3[,c(6,13,14)]
dat3.test.dummy <- predict(dat3.dummy.vars.test, strf.test.dat3[,-17])
dat3.dummy.testing <- as.data.frame(cbind(strf.test.dat3[,17],
                                          dat3.test.dummy))
colnames(dat3.dummy.testing)[4] <- "job.blue_collar"
colnames(dat3.dummy.testing)[9] <- "job.self_employed"



svm.dat3 <- svm(y ~ ., dat3.dummy.training, type = "C-classification", scale = FALSE,
                kernel = "linear", cost = 0.1, gamma = 0.05)

##warning: high computational power required; 
##svm process may take awhile for completion

plot(svm.dat3, dat3.dummy.training, balance ~ age, fill = TRUE, grid = 50, slice = list(),
     symbolPalette = palette(), svSymbol = "x", dataSymbol = "o", main = "SVM Binary Classification")

summary(svm.dat3)

svm.train.predict.dat3 <- predict(svm.dat3, dat3.dummy.training)
cfm.svm.train.predict.dat3 <- confusionMatrix(svm.train.predict.dat3, dat3.dummy.training$y)
print(cfm.svm.train.predict.dat3)

svm.train.predict.dat3.probvals <- predict(svm.dat3, dat3.dummy.training, type = "prob")
svm.train.predict.dat3.probvals.resps <- cbind(svm.train.predict.dat3.probvals, svm.train.predict.dat3, dat3.dummy.training$y)
roc.svm.train.predict.dat3 <- roc(svm.train.predict.dat3.probvals.resps[,3] ~ as.numeric(svm.train.predict.dat3.probvals.resps[,1]),
                            plot = TRUE, print.auc = TRUE, col = "gray60")

svm.predict.dat3 <- predict(svm.dat3, dat3.dummy.testing)
cfm.svm.test.dat3 <- confusionMatrix(svm.predict.dat3, dat3.dummy.testing$y)
print(cfm.svm.test.dat3)

svm.predict.dat3.probvals <- predict(svm.dat3, dat3.dummy.testing, type = "prob")
svm.predict.dat3.probvals.resps <- cbind(svm.predict.dat3.probvals, svm.predict.dat3, dat3.dummy.testing$y)
roc.svm.predict.dat3 <- roc(svm.predict.dat3.probvals.resps[,3] ~ as.numeric(svm.predict.dat3.probvals.resps[,1]),
                            plot = TRUE, print.auc = TRUE, col = "gray60")



#plot all ROC curves
#base test set ROC
roc.boost.test.predict.dat3 <- roc(boost.test.predict.dat3.probvals.resps[,3] ~ as.numeric(boost.test.predict.dat3.probvals.resps[,1]), 
                                   plot = TRUE, print.auc = TRUE, col = "blue", asp = NA, main = "ROC Comparison All Models Test Data")

# Add Legend
legend (0.25, 0.45, legend=c("LOGR (75.0%)","SW-LOGR (74.5%)",
                             "CART (58.8%)","CART-I (61.8%)","RF (75.8%)",
                             "GB (77.5%)","SVM (56.9%)"), 
        col=c("darkgreen","darkgoldenrod1","cyan",
              "red","purple","blue","black"), lty = 1:7, cex =0.55)


# Add ROC test set logistic regression 
lines(roc.predict.lgt.dat3, type="l", pch=2, col="darkgreen", bg="grey", lwd = 1)

# Add ROC test set stepwise logistic regression
lines(roc.predict.swlgt.dat3, type="l", pch=2, col="darkgoldenrod1", lwd = 1)

# Add ROC test set CART
lines(roc.predict.tree.strf.dat3, type="l", pch=2, col="cyan", lwd = 1.2)

# Add ROC test set CART minimum ten-fold cross validation error
lines(roc.predict2.tree.strf.dat3, type="l", pch=2, col="red", lwd = 1.2)

# Add ROC test set SVM 
lines(roc.svm.predict.dat3, type="l", pch=2, col="black", lwd = 1.2)

# Add ROC test set RF 
lines(roc.rdfr.predict.dat3, type="l", pch=2, col="purple", lwd = 1.2)



#base train set ROC
roc.rdfr.train.predict.dat3 <- roc(rdfr.train.predict.dat3.probsvals.resps$`strf.train.dat3$y` ~ 
                               as.numeric(rdfr.train.predict.dat3.probsvals.resps$no), 
                             plot = TRUE, print.auc = TRUE, col = "purple", asp = NA, main = "ROC Comparison All Models Training Data")

# Add Legend
legend (0.25, 0.45, legend=c("LOGR (76.9%)","SW-LOGR (73.9%)",
                           "CART (56.8%)","CART-I (61.1%)","RF (100.0%)",
                           "GB (89.0%)","SVM (59.2%)"), 
                           col=c("darkgreen","darkgoldenrod1","cyan",
                                 "red","purple","blue","black"), lty = 1:7, cex =0.55)

# Add ROC train set logistic regression 
lines(roc.train.predict.lgt.dat3, type="l", pch=2, col="darkgreen", bg="grey", lwd = 1)

# Add ROC train set stepwise logistic regression
lines(roc.train.predict.swlgt.dat3, type="l", pch=2, col="darkgoldenrod1", lwd = 1)

# Add ROC train set CART
lines(roc.train.predict.tree.strf.dat3, type="l", pch=2, col="cyan", lwd = 1.2)

# Add ROC train set CART minimum ten-fold cross validation error
lines(roc.train.predict2.tree.strf.dat3, type="l", pch=2, col="red", lwd = 1.2)

# Add ROC train set SVM 
lines(roc.svm.train.predict.dat3, type="l", pch=2, col="black", lwd = 1.2)

# Add ROC train set Boosting 
lines(roc.boost.train.predict.dat3, type="l", pch=2, col="blue", lwd = 1.2)




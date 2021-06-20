##### Coursework Part 1 #####

#import packages
library(rpart)
library(ggplot2)
library(RColorBrewer)
library(devtools)



#setting data source directory
setwd("C:/Users/User/Documents/Machine Learning R Project")


#load EWCS dataset
ewcs=read.table("EWCS_2016.csv",sep=",",header=TRUE)
ewcs[,][ewcs[, ,] == -999] <- NA
kk=complete.cases(ewcs)
ewcs=ewcs[kk,]


#assign code to original data
dat1 <- data.frame(ewcs[1:11])


#principal component analysis of full data
pca1 <- princomp(dat1, scores=TRUE, cor=TRUE)
info.pca1 <-summary(pca1)
loadings(pca1)
plot(pca1)
screeplot(pca1, type="line", main="Scree Plot")
biplot(pca1)

#principal component analysis of data under columns from 3 to 11
dat1.questionnaire <- cbind(ewcs[3:11])
pca1a <- princomp (dat1.questionnaire, scores=TRUE, cor=TRUE)
summary(pca1a)
loadings(pca1a)
plot(pca1a)
screeplot(pca1a, type="line", main="Scree Plot")
biplot(pca1a)

#categorize each observation's age according to their respective age group
mtx1 <- as.matrix(dat1)
mtx1a <- matrix(mtx1, ncol = ncol(dat1), dimnames = NULL)
age <- as.data.frame(mtx1a[,2])
numr.age <- as.numeric (mtx1a[,2])
hist(numr.age)


#replace 'age' column and its observations with 'age group' column and its observations
numr.age.grp<- cut(numr.age, c(14,24,34,44,54,64,74,101),
                   labels=c("15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))

#assign code to observations under 'age group' column
coded.numr.age.grp<- as.numeric(cut(numr.age, c(14,24,34,44,54,64,74,101),
                                    labels=c("1", "2", "3", "4", "5", "6", "7")))


age.grp<- as.data.frame(numr.age.grp)
coded.age.grp<- as.data.frame(coded.numr.age.grp)
dat1a <- dat1
dat1a[,2]<- coded.age.grp
dat1a[,2] <- as.numeric(as.character(dat1a[,2]))


#principal component analysis of full data with modified observations (age group)
pca1b <- princomp(dat1a, scores=TRUE, cor=TRUE)
summary(pca1b)
loadings(pca1b)
plot(pca1b, type = "l")
screeplot(pca1b, type="line", main="Scree Plot")
biplot(pca1b, xlabs = dat1a[,2], scale = 0, cex = 0.7)


#create cluster analysis plot
str(pca1b)
pca1b$scores
dat1c <- cbind(dat1a,pca1b$scores[,1:2])
head(dat1c)


ggplot(dat1c, aes(x=Comp.1, y=Comp.2, col = Q87a, fill = Q87a)) + 
  stat_ellipse(aes(x=Comp.1, y=Comp.2,col=Q87a, group=Q87a), 
  type = "norm", geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, size = 1, col = "black") + theme(aspect.ratio=1)

ggplot(dat1c, aes(x=Comp.1, y=Comp.2, col = Q90a, fill = Q90a)) + 
  stat_ellipse(aes(x=Comp.1, y=Comp.2,col=Q90b, group=Q90a), 
  type = "norm", geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, size = 1, col = "black") + theme(aspect.ratio=1)

ggplot(dat1c, aes(x=Comp.1, y=Comp.2, col = Q2b, fill = Q2b)) + 
  stat_ellipse(aes(x=Comp.1, y=Comp.2,col=Q2b, group=Q2b), 
  type = "norm", geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, size = 1, col = "black") + theme(aspect.ratio=1)

ggplot(dat1c, aes(x=Comp.1, y=Comp.2, col = Q2a, fill = Q2a)) + 
  stat_ellipse(aes(x=Comp.1, y=Comp.2,col=Q2a, group=Q2a), 
  type = "norm", geom = "polygon", col = "black", alpha = 0.5) + 
  geom_point(shape = 21, size = 1, col = "black") + theme(aspect.ratio=1)



#Mean Score of Questionnaires Among Male in Different Age Groups
mtx1b.M <- matrix(0, nrow=7,ncol=9)
for (i in 1:7) 
  for (j in 1:9)
mtx1b.M[i,j] <-mean(dat1c[which(dat1c[,1]==1 & dat1c[,2]==i),2+j])

#Mean Score of Questionnaires Among Females in Different Age Groups
mtx1b.F <- matrix(0, nrow=7,ncol=9)
for (i in 1:7) 
  for (j in 1:9)
    mtx1b.F[i,j] <-mean(dat1c[which(dat1c[,1]==2 & dat1c[,2]==i),2+j])



mtx1b.M.F <- rbind (mtx1b.M, mtx1b.F)

mtx1b.M.F.labels <- c("15-24(Male)","25-34(Male)","35-44(Male)","45-54(Male)",
                      "55-64(Male)","65-74(Male)","75+(Male)", 
                      "15-24(Female)","25-34(Female)","35-44(Female)",
                      "45-54(Female)","55-64(Female)","65-74(Female)",
                      "75+(Female)") 
Age_Group_Gender <- mtx1b.M.F.labels


mtx1b.questionnaires.label <- c("Less Cheerful","Less Relaxed","Less Active"," Waking Up Tired","Disinterested",
                                "Low Energy at Work","Less Enthusiatic at Work","Time Moving Slowly While at Work","Sense of Inadequacy at Work")
                      
                      
#principal component analysis of observation means by age groups
pca.mtx1b.M.F <- princomp(mtx1b.M.F, scores=TRUE, cor=TRUE)
pca.mtx1b.M.F2 <- prcomp(mtx1b.M.F)
summary(pca.mtx1b.M.F)
loadings(pca.mtx1b.M.F)
plot(pca.mtx1b.M.F, type = "l")
screeplot(pca.mtx1b.M.F, type="line", main="Scree Plot")
biplot(pca.mtx1b.M.F, xlabs = mtx1b.M.F.labels, 
       ylabs = mtx1b.questionnaires.label, scale = 0.3, cex=0.6, 
      expand = 1.1, xlim = c(-2,2), ylim = c(-2,2))


#create cluster analysis plot
str(pca.mtx1b.M.F)
pca.mtx1b.M.F$scores
mtx1c.M.F <- cbind(mtx1b.M.F,pca.mtx1b.M.F$scores[,1:2])
head(mtx1c.M.F)
dat1d.M.F <- as.data.frame(mtx1c.M.F)

ggplot(dat1d.M.F, aes(x=Comp.1, y=Comp.2, col = Age_Group_Gender, fill = Age_Group_Gender)) + 
  geom_point(shape = 21, size = 4) + theme(aspect.ratio=1) + 
  geom_text(aes(label=Age_Group_Gender),hjust=1.2, vjust=0.3) +
  xlim(-10.0, 10.0) + 
  ylim(-3.0, 3.0) + 
  geom_hline(yintercept=0, linetype="dashed", color = "grey54") + 
  geom_vline(xintercept=0, linetype="dashed", color = "grey54")




##### Coursework Part 2 #####

library(rpart)
library(rpart.plot)
library(dplyr)
library(car)
library(carData)
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
library(randomForest)
library(gbm)




#load student performance dataset
school1=read.table("student-mat.csv",sep=";",header=TRUE)
school2=read.table("student-por.csv",sep=";",header=TRUE)
schools=merge(school1,school2,by=c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet"))

#summary of 'schools' data
summary(schools)


#format columns for schools data
dat2.raw <- schools
dat2.raw[,c(1:29)] <- lapply(dat2.raw[,c(1:29)], as.factor)
dat2.raw[,c(34:49)] <- lapply(dat2.raw[,c(34:49)], as.factor)
dat2.factor <- as.data.frame(dat2.raw)
dat2.redefined <- dat2.factor
dat2.raw[,c(1:29)] <- lapply(dat2.raw[,c(1:29)], as.numeric)
dat2.raw[,c(34:49)] <- lapply(dat2.raw[,c(34:49)], as.numeric)
dat2.coded <- as.data.frame(dat2.raw)


#format columns for school1 data (Math Scores)
dat2.math.raw <- school1
dat2.math.raw[,c(1:29)] <- lapply(dat2.math.raw[,c(1:29)], as.factor)
dat2.math.factor <- as.data.frame(dat2.math.raw)
dat2.math.redefined <- dat2.math.factor
dat2.math.raw[,c(1:29)] <- lapply(dat2.math.raw[,c(1:29)], as.numeric)
dat2.math.coded <- as.data.frame(dat2.math.raw)

#format columns for school1 data (Portuguese Scores)
dat2.por.raw <- school2
dat2.por.raw[,c(1:29)] <- lapply(dat2.por.raw[,c(1:29)], as.factor)
dat2.por.factor <- as.data.frame(dat2.por.raw)
dat2.por.redefined <- dat2.por.factor
dat2.por.raw[,c(1:29)] <- lapply(dat2.por.raw[,c(1:29)], as.numeric)
dat2.por.coded <- as.data.frame(dat2.por.raw)



#classify G3 scores according to five classes
math.G3.column <- dat2.redefined[,33]
por.G3.column <- dat2.redefined [,53]
 
math.G3.column.class <- cut(math.G3.column, c(-Inf,9,11,13,15,Inf),
                       labels=c("Fail", "Sufficient", "Satisfactory", 
                                "Good", "Excellent"))

por.G3.column.class <- cut(por.G3.column, c(-Inf,9,11,13,15,Inf),
                       labels=c("Fail", "Sufficient", "Satisfactory", 
                                "Good", "Excellent"), include_lowest = TRUE,
                                ordered_result = TRUE)


#classify G3 scores according to passes and failures (binary)
math.G3.column.binary <- cut(math.G3.column, c(-Inf,9,Inf),
                            labels=c("Fail", "Pass"))
por.G3.column.binary <- cut(por.G3.column, c(-Inf,9,Inf),
                             labels=c("Fail", "Pass"))


#merge columns for Mathematics and Portuguese 
#G3 scores, G3 scores(five classes) and G3 scores(binary)
dat2.plus.G3.class.binary <- cbind(dat2.redefined[,c(1:53)],
                                   math.G3.column.class,
                                   por.G3.column.class,
                                   math.G3.column.binary,
                                   por.G3.column.binary)

#transform columns of observations as factors
dat2.plus.G3.class.binary[,c(54:57)] <- lapply(dat2.plus.G3.class.binary[,c(54:57)], as.factor)
dat2.extended <- as.data.frame(dat2.plus.G3.class.binary)


#show summary of 'schools' with redefined observations' classes
summary(dat2.redefined)
summary(dat2.math.redefined)
summary(dat2.por.redefined)
summary(dat2.extended)

#seed setting
set.seed(2021)


#split data into train set and test set
train.dat2.math <- sample.split(dat2.extended, SplitRatio = 0.7)
trainset.dat2.math <- subset(dat2.extended, train.dat2.math == T)
testset.dat2.math <- subset(dat2.extended, train.dat2.math == F)


#stratified train test split of G3 scores by 'class' and 'binary'
trainset.math.class <- stratified(dat2.extended, c("math.G3.column.class"), 
                                   0.7, bothSets = TRUE)
testset.math.class <- trainset.math.class$SAMP2
trainset.math.binary <- stratified(dat2.extended, c("math.G3.column.binary"), 
                                   0.7, bothSets = TRUE)
testset.math.binary <- trainset.math.binary$SAMP2



#perform linear regression (LR) on Mathematics Score

reg.math.A <- lm(G3.x ~ school +sex +age +address              
   +famsize +Pstatus +Medu +Fedu                 
   +Mjob +Fjob +reason +nursery              
   +internet +guardian.x +traveltime.x +studytime.x          
   +failures.x +schoolsup.x +famsup.x +paid.x               
   +activities.x +higher.x +romantic.x +famrel.x             
   +freetime.x +goout.x +Dalc.x +Walc.x               
   +health.x +absences.x +guardian.y +traveltime.y 
   +studytime.y +failures.y +schoolsup.y +famsup.y 
   +paid.y +activities.y +higher.y +romantic.y 
   +famrel.y +freetime.y +goout.y +Dalc.y 
   +Walc.y +health.y +absences.y, trainset.dat2.math)

#show summary of LR results
summary(reg.math.A)

#LR train set error
LR.RMSE.math.A <- sqrt(mean(residuals(reg.math.A)^2))
print(LR.RMSE.math.A)

#LR model prediction
predict.reg.math.A <- predict(reg.math.A, testset.dat2.math)
reg.math.A.testset.error <- testset.dat2.math$G3.x - predict.reg.math.A
reg.math.A.testset.error.sq <- reg.math.A.testset.error^2

#LR test set error
reg.math.A.testset.RMSE <- sqrt(mean(reg.math.A.testset.error.sq))
print(reg.math.A.testset.RMSE)

#perform step-wise regression (SWLR), removing least significant variables
swlr.math.A <- step(reg.math.A, direction ="backward")
summary(swlr.math.A)
par(mfrow = c(2,2))
plot(swlr.math.A)

#based on swlr.math.A results, choose statistically significant variables (p<0.05) for LR
modified.LR.math.A <- lm(G3.x ~ Medu +guardian.x +failures.x
                         +romantic.x +freetime.x +Walc.x, trainset.dat2.math)

summary(modified.LR.math.A)


par(mfrow = c(2,2))
plot(modified.LR.math.A)
par(mfrow = c(1,1))

#multi collinearity (MC) check
vif(modified.LR.math.A)
##all GVIF values < 2, no MC detected, no follow up required

#modified LR train set error
modified.LR.RMSE.math.A <- sqrt(mean(residuals(modified.LR.math.A)^2))
print(modified.LR.RMSE.math.A)

#modified LR model prediction
modified.predict.reg.math.A <- predict(modified.LR.math.A, testset.dat2.math)
modified.reg.math.A.testset.error <- testset.dat2.math$G3.x - modified.predict.reg.math.A
modified.reg.math.A.testset.error.sq <- modified.reg.math.A.testset.error^2

#modified LR test set error
modified.reg.math.A.testset.RMSE <- sqrt(mean(modified.reg.math.A.testset.error.sq))
print(modified.reg.math.A.testset.RMSE)

#bootstrap means of all observations

dat2.sample.mean <- function(data, indices) {
  return(mean(data[indices], na.rm = T))
}
boot.math.A <- boot(dat2.extended$G3.x, statistic = dat2.sample.mean, R=10000)

plot(boot.math.A)


#logistic regression for Mathematics Score (Binary Classification Pass/Fail)
lgt2.math.C <- glm(math.G3.column.binary ~ school +sex +age +address              
                   +famsize +Pstatus +Medu +Fedu                 
                   +Mjob +Fjob +reason +nursery              
                   +internet +guardian.x +traveltime.x +studytime.x          
                   +failures.x +schoolsup.x +famsup.x +paid.x               
                   +activities.x +higher.x +romantic.x +famrel.x             
                   +freetime.x +goout.x +Dalc.x +Walc.x               
                   +health.x +absences.x +guardian.y +traveltime.y 
                   +studytime.y +failures.y +schoolsup.y +famsup.y 
                   +paid.y +activities.y +higher.y +romantic.y 
                   +famrel.y +freetime.y +goout.y +Dalc.y 
                   +Walc.y +health.y +absences.y, 
                   trainset.math.binary$SAMP1, family = "binomial")
summary(lgt2.math.C)
par(mfrow = c(2,2))
plot(lgt2.math.C)
par(mfrow = c(1,1))

#perform step wise logistic regression (LogR)
swlgt.math.C <- step(lgt2.math.C, direction = "backward")
summary(swlgt.math.C)

##based on swlgt.math.C results, choose statistically significant variables (p<0.05) for LogR
modified.swlgt.math.C <- glm(math.G3.column.binary ~ reason + guardian.x + failures.x + schoolsup.x + 
                               higher.x + romantic.x + freetime.x + guardian.y + studytime.y + 
                               schoolsup.y + absences.y, trainset.math.binary$SAMP1, 
                             family = "binomial")

summary(modified.swlgt.math.C)
par(mfrow = c(2,2))
plot(modified.swlgt.math.C)
par(mfrow = c(1,1))


#Mathematics Score (Pass/Fail) pseudo R^2
modified.swlgt.math.C.null <- modified.swlgt.math.C$null.deviance/-2
modified.swlgt.math.C.proposed <- modified.swlgt.math.C$deviance/-2
modified.swlgt.math.C.pseudoR.sq <- (modified.swlgt.math.C.null - modified.swlgt.math.C.proposed)/modified.swlgt.math.C.null
print(modified.swlgt.math.C.pseudoR.sq)

#p-value
modified.swlgt.math.C.pvalue <- 1 - pchisq(2*(modified.swlgt.math.C.proposed - modified.swlgt.math.C.null), df = (length(modified.swlgt.math.C$coefficients)-1))
print(modified.swlgt.math.C.pvalue)
#model is statistically significant


#modified LogR model prediction

swlgt.dat2.math.C.threshold <- 0.50
predict.swlgt.math.C <- predict.glm(modified.swlgt.math.C, testset.math.binary, type = "response")

swlgt.dat2.math.C.train.pass.prediction <- ifelse(predict.swlgt.math.C > swlgt.dat2.math.C.threshold, 
                                          "Fail", "Pass")

tab.cfm.swlgt.math.C <- table(swlgt.dat2.math.C.train.pass.prediction, testset.math.binary$math.G3.column.binary)
cfm.swlgt.math.C <- confusionMatrix(as.factor(swlgt.dat2.math.C.train.pass.prediction), testset.math.binary$math.G3.column.binary)
print(cfm.swlgt.math.C)




#classification and regression tree (CART) analysis for Mathematics Score, Class and Binary
tree2.math.A <- rpart(G3.x ~ school +sex +age +address              
                    +famsize +Pstatus +Medu +Fedu                 
                    +Mjob +Fjob +reason +nursery              
                    +internet +guardian.x +traveltime.x +studytime.x          
                    +failures.x +schoolsup.x +famsup.x +paid.x               
                    +activities.x +higher.x +romantic.x +famrel.x             
                    +freetime.x +goout.x +Dalc.x +Walc.x               
                    +health.x +absences.x +guardian.y +traveltime.y 
                    +studytime.y +failures.y +schoolsup.y +famsup.y 
                    +paid.y +activities.y +higher.y +romantic.y 
                    +famrel.y +freetime.y +goout.y +Dalc.y 
                    +Walc.y +health.y +absences.y, 
                    trainset.dat2.math, method = "anova", control = 
                      rpart.control(minsplit = 2, cp = 0))


tree2.math.B <- rpart(math.G3.column.class ~ school +sex +age +address              
                    +famsize +Pstatus +Medu +Fedu                 
                    +Mjob +Fjob +reason +nursery              
                    +internet +guardian.x +traveltime.x +studytime.x          
                    +failures.x +schoolsup.x +famsup.x +paid.x               
                    +activities.x +higher.x +romantic.x +famrel.x             
                    +freetime.x +goout.x +Dalc.x +Walc.x               
                    +health.x +absences.x +guardian.y +traveltime.y 
                    +studytime.y +failures.y +schoolsup.y +famsup.y 
                    +paid.y +activities.y +higher.y +romantic.y 
                    +famrel.y +freetime.y +goout.y +Dalc.y 
                    +Walc.y +health.y +absences.y, 
                    trainset.math.class$SAMP1, method = "class", control = 
                      rpart.control(minsplit = 2, cp = 0))

tree2.math.C <- rpart(math.G3.column.binary ~ school +sex +age +address              
                    +famsize +Pstatus +Medu +Fedu                 
                    +Mjob +Fjob +reason +nursery              
                    +internet +guardian.x +traveltime.x +studytime.x          
                    +failures.x +schoolsup.x +famsup.x +paid.x               
                    +activities.x +higher.x +romantic.x +famrel.x             
                    +freetime.x +goout.x +Dalc.x +Walc.x               
                    +health.x +absences.x +guardian.y +traveltime.y 
                    +studytime.y +failures.y +schoolsup.y +famsup.y 
                    +paid.y +activities.y +higher.y +romantic.y 
                    +famrel.y +freetime.y +goout.y +Dalc.y 
                    +Walc.y +health.y +absences.y, 
                    trainset.math.binary$SAMP1, method = "class", control = 
                      rpart.control(minsplit = 2, cp = 0))



#display CART (using rpart package and standard R)
rpart.plot(tree2.math.A, nn = T, main = "Maximal Tree for Mathematics Score", cex = 0.5)
plot(tree2.math.A, main = "Maximal Tree for Mathematics Score")
text(tree2.math.A, pretty=0, cex = 0.35)


#display CART result in values
print(tree2.math.A)
print(tree2.math.B)
print(tree2.math.C)

#display Complexity Parameter (CP) result of pruning sequence and 10-fold cross-validation (CV) errors
printcp(tree2.math.A)
printcp(tree2.math.B)
printcp(tree2.math.C)

#display relationship between pruning sequence and 10-fold CV errors
#note(I): the CP values on the x-axis  are based on geometric mean
#note(II): the size of trees on the x-axis indicates number of terminal nodes
plotcp(tree2.math.A)
plotcp(tree2.math.B)
plotcp(tree2.math.C)


#identify optimal CP value based on formula below
#CV error cap = minimum CV error + positive value of 1 Standard Error (SE)
tree2.math.A.min.CV.error <- tree2.math.A$cptable[which.min(tree2.math.A$cptable[,"xerror"]), "xerror"]
tree2.math.A.CV.error.cap <- tree2.math.A$cptable[which.min(tree2.math.A$cptable[,"xerror"]), "xerror"] +
  tree2.math.A$cptable[which.min(tree2.math.A$cptable[,"xerror"]), "xstd"]

tree2.math.B.min.CV.error <- tree2.math.B$cptable[which.min(tree2.math.B$cptable[,"xerror"]), "xerror"]
tree2.math.B.CV.error.cap <- tree2.math.B$cptable[which.min(tree2.math.B$cptable[,"xerror"]), "xerror"] +
  tree2.math.B$cptable[which.min(tree2.math.B$cptable[,"xerror"]), "xstd"]

tree2.math.C.min.CV.error <- tree2.math.C$cptable[which.min(tree2.math.C$cptable[,"xerror"]), "xerror"]
tree2.math.C.CV.error.cap <- tree2.math.C$cptable[which.min(tree2.math.C$cptable[,"xerror"]), "xerror"] +
  tree2.math.C$cptable[which.min(tree2.math.C$cptable[,"xerror"]), "xstd"]



#optimal CP region whose CV error falls below the CV error cap 
#obtain geometric mean of the identified CP values in the optimal region

i <- 1; j <- 4
while (tree2.math.A$cptable[i,j] > tree2.math.A.CV.error.cap) {
  i <- i + 1
}

optimal.cp.tree2.math.A <- ifelse(i > 1, sqrt(tree2.math.A$cptable[i,1] * tree2.math.A$cptable[i-1,1]), 1)


i <- 1; j <- 4
while (tree2.math.B$cptable[i,j] > tree2.math.B.CV.error.cap) {
  i <- i + 1
}

optimal.cp.tree2.math.B <- ifelse(i > 1, sqrt(tree2.math.B$cptable[i,1] * tree2.math.B$cptable[i-1,1]), 1)


i <- 1; j <- 4
while (tree2.math.C$cptable[i,j] > tree2.math.C.CV.error.cap) {
  i <- i + 1
}

optimal.cp.tree2.math.C <- ifelse(i > 1, sqrt(tree2.math.C$cptable[i,1] * tree2.math.C$cptable[i-1,1]), 1)


#prune tree based on optimal CP
tree2.math.A.pruned <- prune(tree2.math.A, cp = optimal.cp.tree2.math.A)
rpart.plot(tree2.math.A.pruned, nn = T, 
           box.palette = "green",
           main = "CART Analysis for Mathematics Score (Pruned)")

tree2.math.B.pruned <- prune(tree2.math.B, cp = optimal.cp.tree2.math.B)
rpart.plot(tree2.math.B.pruned, nn = T, cex = 0.6,
           box.palette = "green",
           main = "CART Analysis for Mathematics Score - Class (Pruned)")

tree2.math.C.pruned <- prune(tree2.math.C, cp = optimal.cp.tree2.math.C)
rpart.plot(tree2.math.C.pruned, nn = T, 
           box.palette = "green",
           main = "CART Analysis for Mathematics Score - Pass/Fail (Pruned)")


#predict Mathematics scores
predict.dat2.math.A <- predict(tree2.math.A.pruned, testset.dat2.math)
dat2.math.test.set.error <- abs(testset.dat2.math$G3.x - predict.dat2.math.A)

predict.dat2.math.B <- predict(tree2.math.B.pruned, testset.math.class, type = "class")
tab.cfm.dat2.math.B <- table(predict.dat2.math.B, testset.math.class$math.G3.column.class)
cfm.dat2.math.B <- confusionMatrix(predict.dat2.math.B, testset.math.class$math.G3.column.class)
print(cfm.dat2.math.B)

predict.dat2.math.C <- predict(tree2.math.C.pruned, testset.math.binary, type = "class")
tab.cfm.dat2.math.C <- table(predict.dat2.math.C, testset.math.binary$math.G3.column.binary)
cfm.dat2.math.C <- confusionMatrix(predict.dat2.math.C, testset.math.binary$math.G3.column.binary)
print(cfm.dat2.math.C)

#CART RMSE Mathematics Score test set error
CART.RMSE.dat2.math.A <- sqrt(mean(dat2.math.test.set.error^2))
print(CART.RMSE.dat2.math.A)

#Random Forest (RF) Mathematics Score
#Determining optimal mtry value for Random Forest

rdfr.dat2.math.optimal.mtry.ntree <-tuneRF(trainset.dat2.math, mtryStart = 1, trainset.dat2.math$G3.x, ntreeTry=500, stepFactor=2, improve=0.05,
       trace= TRUE, plot= TRUE, doBest= TRUE)


rdfr.dat2.math <- randomForest(G3.x ~ school +sex +age +address              
                          +famsize +Pstatus +Medu +Fedu                 
                          +Mjob +Fjob +reason +nursery              
                          +internet +guardian.x +traveltime.x +studytime.x          
                          +failures.x +schoolsup.x +famsup.x +paid.x               
                          +activities.x +higher.x +romantic.x +famrel.x             
                          +freetime.x +goout.x +Dalc.x +Walc.x               
                          +health.x +absences.x +guardian.y +traveltime.y 
                          +studytime.y +failures.y +schoolsup.y +famsup.y 
                          +paid.y +activities.y +higher.y +romantic.y 
                          +famrel.y +freetime.y +goout.y +Dalc.y 
                          +Walc.y +health.y +absences.y, trainset.dat2.math, 
                          mtry = rdfr.dat2.math.optimal.mtry.ntree$mtry, 
                          ntree = rdfr.dat2.math.optimal.mtry.ntree$ntree, 
                          proximity = TRUE)

rdfr.dat2.math

#RF RMSE
rdfr.RMSE.dat2.math <- sqrt(mean(rdfr.dat2.math$mse))
print(rdfr.RMSE.dat2.math)

rdfr.predict.dat2.math <- predict(rdfr.dat2.math, testset.dat2.math)
rdfr.dat2.math.test.set.error <- abs(testset.dat2.math$G3.x - rdfr.predict.dat2.math)
rdfr.testset.RMSE.dat2.math <- sqrt(mean(rdfr.dat2.math.test.set.error)^2)
print(rdfr.testset.RMSE.dat2.math)

#Variable Importance determining Mathematics Score
varImpPlot(rdfr.dat2.math, main = "Variable Importance Determining Math Scores")


#Important variables or interaction effect identified

rdfr.dat2.math.interact.optimal.mtry.ntree <-tuneRF(trainset.dat2.math[c("Medu", "guardian.x", "failures.x", "romantic.x", "freetime.x", 
                                                                       "Walc.x")], mtryStart = 1, trainset.dat2.math$G3.y, ntreeTry=500, stepFactor=2, improve=0.05,
                                                   trace= TRUE, plot= TRUE, doBest= TRUE)

rdfr.dat2.math.interact <- randomForest(G3.x ~ Medu +guardian.x +failures.x
                                        +romantic.x +freetime.x +Walc.x, trainset.dat2.math,
                                       mtry = rdfr.dat2.math.interact.optimal.mtry.ntree$mtry, 
                                       ntree = rdfr.dat2.math.interact.optimal.mtry.ntree$ntree, 
                                       proximity = TRUE)

rdfr.dat2.math.interact


#RF RMSE (LR Shortlisted Variables and Interaction considered
rdfr.RMSE.dat2.math.interact <- sqrt(mean(rdfr.dat2.math.interact$mse))
print(rdfr.RMSE.dat2.math.interact)

rdfr.predict.dat2.math.interact <- predict(rdfr.dat2.math.interact, testset.dat2.math)
rdfr.dat2.math.interact.test.set.error <- abs(testset.dat2.math$G3.y - rdfr.predict.dat2.math.interact)
rdfr.testset.RMSE.dat2.math.interact <- sqrt(mean(rdfr.dat2.math.interact.test.set.error)^2)
print(rdfr.testset.RMSE.dat2.math.interact)

#Variable Importance determining Mathematics Score
varImpPlot(rdfr.dat2.math.interact, main = "Variable Importance Determining Mathematics Scores")





#Boosting Decision Tree

boost.dat2.math <- gbm(G3.x ~ Medu +guardian.x +failures.x
                       +romantic.x +freetime.x +Walc.x, trainset.dat2.math, 
                       distribution = "gaussian", n.trees = 5000, shrinkage = 0.1,
                       interaction.depth = 2)

boost.train.predict.dat2.math <- predict(boost.dat2.math, trainset.dat2.math, n.trees = 5000)
boost.train.predict.dat2.math.RMSE <- RMSE(boost.train.predict.dat2.math,trainset.dat2.math$G3.x)
print(boost.train.predict.dat2.math.RMSE)

boost.test.predict.dat2.math <- predict(boost.dat2.math, testset.dat2.math, n.trees = 5000)
boost.test.predict.dat2.math.RMSE <- RMSE(boost.test.predict.dat2.math,testset.dat2.math$G3.x)
print(boost.test.predict.dat2.math.RMSE)



#Support Vector Regression (SVR) for Mathematics Score
#transform categorical variables into dummy variables; data into matrix

truncated.dat2.train.math <- as.data.frame(trainset.dat2.math[,c(1:30,33,34:50)])
truncated.dat2.test.math <- as.data.frame(testset.dat2.math[,c(1:30,33,34:50,53)])

dat2.math.dummy.vars <- dummyVars(~. , truncated.dat2.train.math[,-31])
dat2.math.train.dummy <- predict(dat2.math.dummy.vars, truncated.dat2.train.math[,-31])
dat2.math.dummy.training <- as.data.frame(cbind(truncated.dat2.train.math[,31], 
                                               dat2.math.train.dummy))


dat2.math.dummy.vars.test <- dummyVars(~. , truncated.dat2.test.math[,-31])
dat2.math.test.dummy <- predict(dat2.math.dummy.vars.test, truncated.dat2.test.math[,-31])
dat2.math.dummy.testing <- as.data.frame(cbind(truncated.dat2.test.math[,31],
                                              dat2.math.test.dummy))

##Support Vector function
svr.dat2.math <- svm(dat2.math.dummy.training$V1 ~ ., dat2.math.dummy.training[-1], type = "eps-regression", scale = FALSE,
                    kernel = "linear", cost = 0.01, gamma = 0.005)

##warning: high computational power required; 
##svr process may take awhile for completion

#plot svr
plot(svr.dat2.math, dat2.math.dummy.training, dat2.math.dummy.training$V1~failures.x1, fill = TRUE, grid = 50, slice = list(),
     symbolPalette = palette(), svSymbol = "x", dataSymbol = "o", main = "SVR")

#show svr details
summary(svr.dat2.math)

#SVR Train and Test RMSE
svr.train.predict.dat2.math <- predict(svr.dat2.math, dat2.math.dummy.training)
svr.train.predict.dat2.math.RMSE <- RMSE(svr.train.predict.dat2.math,dat2.math.dummy.training$V1)
print(svr.train.predict.dat2.math.RMSE)

svr.test.predict.dat2.math <- predict(svr.dat2.math, dat2.math.dummy.testing)
svr.test.predict.dat2.math.RMSE <- RMSE(svr.test.predict.dat2.math,dat2.math.dummy.testing$V1)
print(svr.test.predict.dat2.math.RMSE)









#split data into train set and test set
trainset.dat2.por <- trainset.dat2.math
testset.dat2.por <- testset.dat2.math


#stratified train test split of G3 scores by 'class' and 'binary'
trainset.por.class <- stratified(dat2.extended, c("por.G3.column.class"), 
                                  0.7, bothSets = TRUE)
testset.por.class <- trainset.por.class$SAMP2
trainset.por.binary <- stratified(dat2.extended, c("por.G3.column.binary"), 
                                   0.7, bothSets = TRUE)
testset.por.binary <- trainset.por.binary$SAMP2



#perform linear regression (LR) on Portuguese Score

reg.por.A <- lm(G3.y ~ school +sex +age +address              
                 +famsize +Pstatus +Medu +Fedu                 
                 +Mjob +Fjob +reason +nursery              
                 +internet +guardian.x +traveltime.x +studytime.x          
                 +failures.x +schoolsup.x +famsup.x +paid.x               
                 +activities.x +higher.x +romantic.x +famrel.x             
                 +freetime.x +goout.x +Dalc.x +Walc.x               
                 +health.x +absences.x +guardian.y +traveltime.y 
                 +studytime.y +failures.y +schoolsup.y +famsup.y 
                 +paid.y +activities.y +higher.y +romantic.y 
                 +famrel.y +freetime.y +goout.y +Dalc.y 
                 +Walc.y +health.y +absences.y, trainset.dat2.por)

#show summary of LR results
summary(reg.por.A)

#LR train set error
LR.RMSE.por.A <- sqrt(mean(residuals(reg.por.A)^2))
print(LR.RMSE.por.A)

#LR model prediction
predict.reg.por.A <- predict(reg.por.A, testset.dat2.por)
reg.por.A.testset.error <- testset.dat2.por$G3.y - predict.reg.por.A
reg.por.A.testset.error.sq <- reg.por.A.testset.error^2

#LR test set error
reg.por.A.testset.RMSE <- sqrt(mean(reg.por.A.testset.error.sq))
print(reg.por.A.testset.RMSE)

#perform step-wise regression (SWLR), removing least significant variables
swlr.por.A <- step(reg.por.A, direction ="backward")
summary(swlr.por.A)
par(mfrow = c(2,2))
plot(swlr.por.A)

#multi collinearity (MC) check
vif(swlr.por.A)

#based on swlr.por.A results, choose statistically significant variables (p<0.05) for LR
modified.LR.por.A <- lm(G3.y ~ school + sex + age + address + Pstatus + 
                          Medu + failures.x + schoolsup.x + activities.x + higher.x + 
                          freetime.x + Dalc.x + Walc.x + absences.x + studytime.y + 
                          failures.y, data = trainset.dat2.por)

info.modified.LR.por.A <- summary(modified.LR.por.A)
par(mfrow = c(2,2))
plot(modified.LR.por.A)
par(mfrow = c(1,1))


#multi collinearity (MC) check
vif(modified.LR.por.A)
##all GVIF values < 2, no MC detected, no follow up required

#modified LR train set error
modified.LR.RMSE.por.A <- sqrt(mean(residuals(modified.LR.por.A)^2))
print(modified.LR.RMSE.por.A)

#modified LR model prediction
modified.predict.reg.por.A <- predict(modified.LR.por.A, testset.dat2.por)
modified.reg.por.A.testset.error <- testset.dat2.por$G3.y - modified.predict.reg.por.A
modified.reg.por.A.testset.error.sq <- modified.reg.por.A.testset.error^2

#modified LR test set error
modified.reg.por.A.testset.RMSE <- sqrt(mean(modified.reg.por.A.testset.error.sq))
print(modified.reg.por.A.testset.RMSE)

#adjusted R-squared
print(info.modified.LR.por.A$adj.r.squared)

#bootstrap means of all observations

dat2.sample.mean <- function(data, indices) {
  return(mean(data[indices], na.rm = T))
}
boot.por.A <- boot(dat2.extended$G3.y, statistic = dat2.sample.mean, R=10000)

plot(boot.por.A)


#logistic regression for Portuguese Score (Binary Classification Pass/Fail)
lgt2.por.C <- glm(por.G3.column.binary ~ school +sex +age +address              
                   +famsize +Pstatus +Medu +Fedu                 
                   +Mjob +Fjob +reason +nursery              
                   +internet +guardian.x +traveltime.x +studytime.x          
                   +failures.x +schoolsup.x +famsup.x +paid.x               
                   +activities.x +higher.x +romantic.x +famrel.x             
                   +freetime.x +goout.x +Dalc.x +Walc.x               
                   +health.x +absences.x +guardian.y +traveltime.y 
                   +studytime.y +failures.y +schoolsup.y +famsup.y 
                   +paid.y +activities.y +higher.y +romantic.y 
                   +famrel.y +freetime.y +goout.y +Dalc.y 
                   +Walc.y +health.y +absences.y, 
                   trainset.por.binary$SAMP1, family = "binomial")
summary(lgt2.por.C)
par(mfrow = c(2,2))
plot(lgt2.por.C)
par(mfrow = c(1,1))

#perform step wise logistic regression (LogR)
swlgt.por.C <- step(lgt2.por.C, direction = "backward")
summary(swlgt.por.C)

##based on swlgt.por.C results, choose statistically significant variables (p<0.05) for LogR
modified.swlgt.por.C <- glm(por.G3.column.binary ~ reason + guardian.x + failures.x + schoolsup.x + 
                               higher.x + romantic.x + freetime.x + guardian.y + studytime.y + 
                               schoolsup.y + absences.y, trainset.por.binary$SAMP1, 
                             family = "binomial")

summary(modified.swlgt.por.C)
par(mfrow = c(2,2))
plot(modified.swlgt.por.C)
par(mfrow = c(1,1))


#Portuguese Score (Pass/Fail) pseudo R^2
modified.swlgt.por.C.null <- modified.swlgt.por.C$null.deviance/-2
modified.swlgt.por.C.proposed <- modified.swlgt.por.C$deviance/-2
modified.swlgt.por.C.pseudoR.sq <- (modified.swlgt.por.C.null - modified.swlgt.por.C.proposed)/modified.swlgt.por.C.null
print(modified.swlgt.por.C.pseudoR.sq)

#pseudo R squared
modified.swlgt.por.C.pseudoR.sq <- 
  1 - (modified.swlgt.por.C$deviance/modified.swlgt.por.C$null.deviance)
print(modified.swlgt.por.C.pseudoR.sq)



#p-value
modified.swlgt.por.C.pvalue <- 1 - pchisq(2*(modified.swlgt.por.C.proposed - modified.swlgt.por.C.null), df = (length(modified.swlgt.por.C$coefficients)-1))
print(modified.swlgt.por.C.pvalue)
#model is statistically significant


#modified LogR model prediction

swlgt.dat2.por.C.threshold <- 0.5
predict.swlgt.por.C <- predict.glm(modified.swlgt.por.C, testset.por.binary, type = "response")

swlgt.dat2.por.C.train.pass.prediction <- ifelse(predict.swlgt.por.C > swlgt.dat2.por.C.threshold, 
                                                  "Fail", "Pass")

tab.cfm.swlgt.por.C <- table(swlgt.dat2.por.C.train.pass.prediction, testset.por.binary$por.G3.column.binary)
cfm.swlgt.por.C <- confusionMatrix(as.factor(swlgt.dat2.por.C.train.pass.prediction), testset.por.binary$por.G3.column.binary)
print(cfm.swlgt.por.C)




#classification and regression tree (CART) analysis for Portuguese Score, Class and Binary
tree2.por.A <- rpart(G3.y ~ school +sex +age +address              
                      +famsize +Pstatus +Medu +Fedu                 
                      +Mjob +Fjob +reason +nursery              
                      +internet +guardian.x +traveltime.x +studytime.x          
                      +failures.x +schoolsup.x +famsup.x +paid.x               
                      +activities.x +higher.x +romantic.x +famrel.x             
                      +freetime.x +goout.x +Dalc.x +Walc.x               
                      +health.x +absences.x +guardian.y +traveltime.y 
                      +studytime.y +failures.y +schoolsup.y +famsup.y 
                      +paid.y +activities.y +higher.y +romantic.y 
                      +famrel.y +freetime.y +goout.y +Dalc.y 
                      +Walc.y +health.y +absences.y, 
                      trainset.dat2.por, method = "anova", control = 
                        rpart.control(minsplit = 2, cp = 0))


tree2.por.B <- rpart(por.G3.column.class ~ school +sex +age +address              
                      +famsize +Pstatus +Medu +Fedu                 
                      +Mjob +Fjob +reason +nursery              
                      +internet +guardian.x +traveltime.x +studytime.x          
                      +failures.x +schoolsup.x +famsup.x +paid.x               
                      +activities.x +higher.x +romantic.x +famrel.x             
                      +freetime.x +goout.x +Dalc.x +Walc.x               
                      +health.x +absences.x +guardian.y +traveltime.y 
                      +studytime.y +failures.y +schoolsup.y +famsup.y 
                      +paid.y +activities.y +higher.y +romantic.y 
                      +famrel.y +freetime.y +goout.y +Dalc.y 
                      +Walc.y +health.y +absences.y, 
                      trainset.por.class$SAMP1, method = "class", control = 
                        rpart.control(minsplit = 2, cp = 0))

tree2.por.C <- rpart(por.G3.column.binary ~ school +sex +age +address              
                      +famsize +Pstatus +Medu +Fedu                 
                      +Mjob +Fjob +reason +nursery              
                      +internet +guardian.x +traveltime.x +studytime.x          
                      +failures.x +schoolsup.x +famsup.x +paid.x               
                      +activities.x +higher.x +romantic.x +famrel.x             
                      +freetime.x +goout.x +Dalc.x +Walc.x               
                      +health.x +absences.x +guardian.y +traveltime.y 
                      +studytime.y +failures.y +schoolsup.y +famsup.y 
                      +paid.y +activities.y +higher.y +romantic.y 
                      +famrel.y +freetime.y +goout.y +Dalc.y 
                      +Walc.y +health.y +absences.y, 
                      trainset.por.binary$SAMP1, method = "class", control = 
                        rpart.control(minsplit = 2, cp = 0))



#display CART (using rpart package and standard R)
rpart.plot(tree2.por.A, nn = T, main = "Maximal Tree for Portuguese Score", cex = 0.5)
plot(tree2.por.A, main = "Maximal Tree for Portuguese Score")
text(tree2.por.A, pretty=0, cex = 0.35)


#display CART result in values
print(tree2.por.A)
print(tree2.por.B)
print(tree2.por.C)

#display Complexity Parameter (CP) result of pruning sequence and 10-fold cross-validation (CV) errors
printcp(tree2.por.A)
printcp(tree2.por.B)
printcp(tree2.por.C)

#display relationship between pruning sequence and 10-fold CV errors
#note(I): the CP values on the x-axis  are based on geometric mean
#note(II): the size of trees on the x-axis indicates number of terminal nodes
plotcp(tree2.por.A)
plotcp(tree2.por.B)
plotcp(tree2.por.C)


#identify optimal CP value based on formula below
#CV error cap = minimum CV error + positive value of 1 Standard Error (SE)
tree2.por.A.min.CV.error <- tree2.por.A$cptable[which.min(tree2.por.A$cptable[,"xerror"]), "xerror"]
tree2.por.A.CV.error.cap <- tree2.por.A$cptable[which.min(tree2.por.A$cptable[,"xerror"]), "xerror"] +
  tree2.por.A$cptable[which.min(tree2.por.A$cptable[,"xerror"]), "xstd"]

tree2.por.B.min.CV.error <- tree2.por.B$cptable[which.min(tree2.por.B$cptable[,"xerror"]), "xerror"]
tree2.por.B.CV.error.cap <- tree2.por.B$cptable[which.min(tree2.por.B$cptable[,"xerror"]), "xerror"] +
  tree2.por.B$cptable[which.min(tree2.por.B$cptable[,"xerror"]), "xstd"]

tree2.por.C.min.CV.error <- tree2.por.C$cptable[which.min(tree2.por.C$cptable[,"xerror"]), "xerror"]
tree2.por.C.CV.error.cap <- tree2.por.C$cptable[which.min(tree2.por.C$cptable[,"xerror"]), "xerror"] +
  tree2.por.C$cptable[which.min(tree2.por.C$cptable[,"xerror"]), "xstd"]



#optimal CP region whose CV error falls below the CV error cap 
#obtain geometric mean of the identified CP values in the optimal region

i <- 1; j <- 4
while (tree2.por.A$cptable[i,j] > tree2.por.A.CV.error.cap) {
  i <- i + 1
}

optimal.cp.tree2.por.A <- ifelse(i > 1, sqrt(tree2.por.A$cptable[i,1] * tree2.por.A$cptable[i-1,1]), 1)


i <- 1; j <- 4
while (tree2.por.B$cptable[i,j] > tree2.por.B.CV.error.cap) {
  i <- i + 1
}

optimal.cp.tree2.por.B <- ifelse(i > 1, sqrt(tree2.por.B$cptable[i,1] * tree2.por.B$cptable[i-1,1]), 1)


i <- 1; j <- 4
while (tree2.por.C$cptable[i,j] > tree2.por.C.CV.error.cap) {
  i <- i + 1
}

optimal.cp.tree2.por.C <- ifelse(i > 1, sqrt(tree2.por.C$cptable[i,1] * tree2.por.C$cptable[i-1,1]), 1)


#prune tree based on optimal CP
tree2.por.A.pruned <- prune(tree2.por.A, cp = optimal.cp.tree2.por.A)
rpart.plot(tree2.por.A.pruned, nn = T, 
           box.palette = "green",
           main = "CART Analysis for Portuguese Score (Pruned)")

tree2.por.B.pruned <- prune(tree2.por.B, cp = optimal.cp.tree2.por.B)
rpart.plot(tree2.por.B.pruned, nn = T, cex = 0.6,
           box.palette = "green",
           main = "CART Analysis for Portuguese Score - Class (Pruned)")

tree2.por.C.pruned <- prune(tree2.por.C, cp = optimal.cp.tree2.por.C)
rpart.plot(tree2.por.C.pruned, nn = T, 
           box.palette = "green",
           main = "CART Analysis for Portuguese Score - Pass/Fail (Pruned)")


#predict Portuguese scores
predict.dat2.por.A <- predict(tree2.por.A.pruned, testset.dat2.por)
dat2.por.test.set.error <- abs(testset.dat2.por$G3.y - predict.dat2.por.A)

predict.dat2.por.B <- predict(tree2.por.B.pruned, testset.por.class, type = "class")
tab.cfm.dat2.por.B <- table(predict.dat2.por.B, testset.por.class$por.G3.column.class)
cfm.dat2.por.B <- confusionMatrix(predict.dat2.por.B, testset.por.class$por.G3.column.class)
print(cfm.dat2.por.B)

predict.dat2.por.C <- predict(tree2.por.C.pruned, testset.por.binary, type = "class")
tab.cfm.dat2.por.C <- table(predict.dat2.por.C, testset.por.binary$por.G3.column.binary)
cfm.dat2.por.C <- confusionMatrix(predict.dat2.por.C, testset.por.binary$por.G3.column.binary)
print(cfm.dat2.por.C)

#CART RMSE Portuguese Score test set error
CART.RMSE.dat2.por.A <- sqrt(mean(dat2.por.test.set.error^2))
print(CART.RMSE.dat2.por.A)

#Random Forest (RF) Portuguese Score
#Determining optimal mtry value for Random Forest

rdfr.dat2.por.optimal.mtry.ntree <-tuneRF(trainset.dat2.por, mtryStart = 1, trainset.dat2.por$G3.y, ntreeTry=500, stepFactor=2, improve=0.05,
                                           trace= TRUE, plot= TRUE, doBest= TRUE)


rdfr.dat2.por <- randomForest(G3.y ~ school +sex +age +address              
                               +famsize +Pstatus +Medu +Fedu                 
                               +Mjob +Fjob +reason +nursery              
                               +internet +guardian.x +traveltime.x +studytime.x          
                               +failures.x +schoolsup.x +famsup.x +paid.x               
                               +activities.x +higher.x +romantic.x +famrel.x             
                               +freetime.x +goout.x +Dalc.x +Walc.x               
                               +health.x +absences.x +guardian.y +traveltime.y 
                               +studytime.y +failures.y +schoolsup.y +famsup.y 
                               +paid.y +activities.y +higher.y +romantic.y 
                               +famrel.y +freetime.y +goout.y +Dalc.y 
                               +Walc.y +health.y +absences.y, trainset.dat2.por, 
                               mtry = rdfr.dat2.por.optimal.mtry.ntree$mtry, 
                               ntree = rdfr.dat2.por.optimal.mtry.ntree$ntree, 
                               proximity = TRUE)


rdfr.dat2.por

#RF RMSE
rdfr.RMSE.dat2.por <- sqrt(mean(rdfr.dat2.por$mse))
print(rdfr.RMSE.dat2.por)

rdfr.predict.dat2.por <- predict(rdfr.dat2.por, testset.dat2.por)
rdfr.dat2.por.test.set.error <- abs(testset.dat2.por$G3.y - rdfr.predict.dat2.por)
rdfr.testset.RMSE.dat2.por <- sqrt(mean(rdfr.dat2.por.test.set.error)^2)
print(rdfr.testset.RMSE.dat2.por)

#Variable Importance determining Portuguese Score
varImpPlot(rdfr.dat2.por, main = "Variable Importance Determining Portuguese Scores")


#Interaction effect identified

rdfr.dat2.por.interact.optimal.mtry.ntree <-tuneRF(trainset.dat2.por[c("school", "sex", "age", "address", "Pstatus", 
                                                                         "Medu", "failures.x", "schoolsup.x", "activities.x", "higher.x", 
                                                                         "freetime.x", "Dalc.x", "Walc.x", "absences.x", "studytime.y", 
                                                                         "failures.y")], mtryStart = 1, trainset.dat2.por$G3.y, ntreeTry=500, stepFactor=2, improve=0.05,
                                          trace= TRUE, plot= TRUE, doBest= TRUE)

rdfr.dat2.por.interact <- randomForest(G3.y ~ school + sex + age + address + Pstatus + 
                                         Medu + failures.x + schoolsup.x + activities.x + higher.x + 
                                         freetime.x + Dalc.x + Walc.x + absences.x + studytime.y + 
                                         failures.y, trainset.dat2.por,
                                       mtry = rdfr.dat2.por.interact.optimal.mtry.ntree$mtry, 
                                       ntree = rdfr.dat2.por.interact.optimal.mtry.ntree$ntree, 
                                       proximity = TRUE)

rdfr.dat2.por.interact


#RF RMSE (LR Shortlisted Variables and Interaction considered)
rdfr.RMSE.dat2.por.interact <- sqrt(mean(rdfr.dat2.por.interact$mse))
print(rdfr.RMSE.dat2.por.interact)

rdfr.predict.dat2.por.interact <- predict(rdfr.dat2.por.interact, testset.dat2.por)
rdfr.dat2.por.interact.test.set.error <- abs(testset.dat2.por$G3.y - rdfr.predict.dat2.por.interact)
rdfr.testset.RMSE.dat2.por.interact <- sqrt(mean(rdfr.dat2.por.interact.test.set.error)^2)
print(rdfr.testset.RMSE.dat2.por.interact)

#Variable Importance determining Portuguese Score
varImpPlot(rdfr.dat2.por.interact, main = "Variable Importance Determining Portuguese Scores")




#Support Vector Regression for Portuguese Score
#transform categorical variables into dummy variables; data into matrix

truncated.dat2.train.por <- as.data.frame(trainset.dat2.por[,c(1:30,34:50,53)])
truncated.dat2.test.por <- as.data.frame(testset.dat2.por[,c(1:30,34:50,53)])

dat2.por.dummy.vars <- dummyVars(~. , truncated.dat2.train.por[,-48])
dat2.por.train.dummy <- predict(dat2.por.dummy.vars, truncated.dat2.train.por[,-48])
dat2.por.dummy.training <- as.data.frame(cbind(truncated.dat2.train.por[,48], 
                                           dat2.por.train.dummy))


dat2.por.dummy.vars.test <- dummyVars(~. , truncated.dat2.test.por[,-48])
dat2.por.test.dummy <- predict(dat2.por.dummy.vars.test, truncated.dat2.test.por[,-48])
dat2.por.dummy.testing <- as.data.frame(cbind(truncated.dat2.test.por[,48],
                                          dat2.por.test.dummy))

##Support Vector
svr.dat2.por <- svm(dat2.por.dummy.training$V1 ~ ., dat2.por.dummy.training[-1], type = "eps-regression", scale = FALSE,
                kernel = "linear", cost = 0.01, gamma = 0.005)

##warning: high computational power required; 
##svr process may take awhile for completion

#plot svr
plot(svr.dat2.por, dat2.por.dummy.training, dat2.por.dummy.training$V1~failures.x1, fill = TRUE, grid = 50, slice = list(),
     symbolPalette = palette(), svSymbol = "x", dataSymbol = "o", main = "SVR")

#show svr details
summary(svr.dat2.por)

#SVR Train and Test RMSE
svr.train.predict.dat2.por <- predict(svr.dat2.por, dat2.por.dummy.training)
svr.train.predict.dat2.por.RMSE <- RMSE(svr.train.predict.dat2.por,dat2.por.dummy.training$V1)
print(svr.train.predict.dat2.por.RMSE)

svr.test.predict.dat2.por <- predict(svr.dat2.por, dat2.por.dummy.testing)
svr.test.predict.dat2.por.RMSE <- RMSE(svr.test.predict.dat2.por,dat2.por.dummy.testing$V1)
print(svr.test.predict.dat2.por.RMSE)







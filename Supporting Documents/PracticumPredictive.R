##Reading in the csv file. Please set working directory to folder containing this csv file
masterdata <- read.csv("BreastCancerData2010-2012.csv", header = TRUE)

##Converting the categorical variables from numeric
factornames <- c('Marital_Status', 'Race', 'Stage', 'Subtype', 'Grade')
masterdata[,factornames] <- lapply(masterdata[,factornames] , factor)

##Creating a data frame for 2010 stage 4 patients
data2010 <- masterdata[masterdata$Year == 2010, ]
data2010stage4 <- data2010[data2010$Stage == 4, ]

##Setting up the required libraries. Please use install.packages('package_name') 
##to install these packages before calling them
library(ggplot2)
library(pROC)
library(rpart)
library(rattle)

##Stepwise model selection with logistic regression 
none = glm(EVENT ~ 1,data=data2010stage4,family="binomial"(link="logit"))
all = glm(EVENT ~ (Race + Age_Diagnosis + Marital_Status + Grade + Subtype + Tumor_Size)^2, data=data2010stage4, family="binomial")
lmodel = step(none, scope=list(lower=formula(none),upper= formula(all), direction="both"))
auc(data2010stage4$EVENT,lmodel$fitted.values)
summary(lmodel) 

##Classification Tree
tree1 <- rpart(EVENT ~ Race + Age_Diagnosis + Marital_Status + Grade + Subtype + Tumor_Size, data=data2010stage4, method = "class")
fancyRpartPlot(tree1)
out <- predict(tree1)
pred.response <- colnames(out)[max.col(out, ties.method = c("random"))]
misclasserror <- mean(data2010stage4$EVENT != pred.response)
print(misclasserror)

#10-fold cross validation
ssize = ceiling(nrow(data2010stage4)/10)
vssize = nrow(data2010stage4)-ssize
iter = 20
aucsreg = rep(0,iter)
testresponses = vector()
regpred = vector()

for(jj in 1:iter){
  foldIndices = sample(nrow(data2010stage4),size = ssize)
  testingData <- data2010stage4[foldIndices,]
  trainingData = data2010stage4[-foldIndices,]
  
  testresponses <- c(testresponses,testingData$EVENT)
  
  reg.fit = glm(formula(lmodel),data=trainingData)
  p = predict(reg.fit,newdata = testingData, type='response')
  aucsreg[jj] <- roc(testingData$EVENT,p)$auc
  regpred <- c(regpred,p)
  
}

regauc = mean(aucsreg)
print(regauc)
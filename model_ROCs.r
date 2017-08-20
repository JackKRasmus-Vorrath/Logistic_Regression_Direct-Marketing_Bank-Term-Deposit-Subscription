library(dplyr)
library(glmnet)
library(ROCR)
library(MASS)

# set the working directory
setwd("/Users/jkras/Desktop/R_Docs/R_Projects/Logistic_Regression_Direct-Marketing_Bank-Term-Deposit-Subscription")


# load the data
bank.dat <- read.csv('bank-full.csv', sep=";")

# summary of data
head(bank.dat)
str(bank.dat)
summary(bank.dat)

# manipulate data so we can run it through glmnet
c1 <- as.double(bank.dat$age)
c2 <- as.factor(bank.dat$job)
c3 <- as.factor(bank.dat$marital)
c4 <- as.factor(bank.dat$education)
c5 <- as.factor(bank.dat$default)
c6 <- as.double(bank.dat$balance)
c7 <- as.factor(bank.dat$housing)
c8 <- as.factor(bank.dat$loan)
c9 <- as.factor(bank.dat$contact)
c10 <- as.factor(bank.dat$day)
c11 <- as.factor(bank.dat$month)
c12 <- as.double(bank.dat$duration)
c13 <- as.double(bank.dat$campaign)
c14 <- as.factor(bank.dat$pdays)
c15 <- as.double(bank.dat$previous)
c16 <- as.factor(bank.dat$poutcome)
c17 <- as.factor(as.character(bank.dat$y))

bank.dat.2 <- cbind(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17)

colnames(bank.dat.2) <- c("age"
                          ,"job.type"
                          ,"marital.status"
                          ,"highest.educ"
                          ,"credit.default"
                          ,"avg.annual.balance"
                          ,"has.housing.loan"
                          ,"has.personal.loan"
                          ,"contact.type"
                          ,"last.contact.dom"
                          ,"last.contact.moy"
                          ,"last.contact.duration"
                          ,"contacts.num"
                          ,"days.passed"
                          ,"contacts.num.prev"
                          ,"prev.outcome"
                          ,"subscribed")


# train-validate sizes
n.train <- floor(0.75 * nrow(bank.dat.2)) # 75% train set
n.validate <- dim(bank.dat.2)[1] - n.train # 25% validation set

# set the seed to make your partition reproducible
set.seed(1985)
# random sample indices for train-validation split
train_indices <- sample(seq_len(nrow(bank.dat.2)), size = n.train)

# create train and validation sets
dat.train <- bank.dat.2[train_indices, ]
dat.validate <- bank.dat.2[-train_indices, ]

# train predictor and response partition
dat.train.x <- dat.train[,1:16]
#dat.train.y <- dat.train[, 17:17]

dat.train.x_2 <- as.matrix(dat.train.x[,-c(2,5,10),drop=FALSE])
dat.train.y <- subset(dat.train, select=subscribed)


n <- floor(0.75 * nrow(dat.train)) # sample size (VERY BIG, VERY SLOW!)
nloops<- 25   #number of loops
ntrains<-dim(dat.train.x)[1]  #No. of samples in training data set
model.aucs<-c() # empty vector for later plotting of AUCs
roc.perf.list <- list() # empty list for accumulating roc.perf objects
pred.list <- list()



for (i in 1:nloops){
  
  index<-sample(1:ntrains, n) # take a sample from range 1:ntrains of size 100
  model_train.x<-as.matrix(dat.train.x_2[index,]) # convert to matrix
  model_test.x<-as.matrix(dat.train.x_2[-index,]) # convert to matrix
  model_train.y<-dat.train.y[index] # isolate response sample
  model_test.y<-dat.train.y[-index]
  
  model_fit <- glmnet(model_train.x, model_train.y, family = "binomial") # create model
  fit.pred <- predict(model_fit, newx = model_test.x, type = "response") # predict y's using logreg model (creates matrix of probabilities corresponding test Xs)
  pred <- prediction(fit.pred[,1], model_test.y) # creates 'ROCR' object containing binary-ized predictions (1 or 0) and other metadata
  pred.list <- append(pred.list, pred)
  roc.perf = performance(pred, measure = "tpr", x.measure = "fpr") # creates a 'performance' object (from ROCR) containg performance metrics--true positive rate and false positive rate
  roc.perf.list <- append(roc.perf.list, roc.perf) # add roc.perf to list
  auc.train <- performance(pred, measure = "auc") # create 'performance' object (from ROCR) containing perf metric-- area under curve (AUC)
  auc.train <- auc.train@y.values # isolate y-vals from auc.train
  model.aucs[i]<-auc.train[[1]] #add y-values from auc.train for later plotting
  
}
library(caret)
library(dplyr)


# set the working directory
setwd('/Users/jkras/Desktop/R_Docs/R_Projects/Logistic_Regression_Direct-Marketing_Bank-Term-Deposit-Subscription')

# load the data
bank.dat <- read.csv('bank-full.csv', sep=";")

# summary of data
head(bank.dat)
#str(bank.dat)
#summary(bank.dat)

##---------------------------------------
## BEGIN DATA PREPARATION
##---------------------------------------

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
c18 <- as.factor(as.numeric(ifelse(c17 == "yes", 1, 0)))

bank.dat.2 <- data.frame(cbind(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c18))
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

# after dataframe, 'subscribed' reverts to numeric, so set it to factor, table flip--> (╯°□°）╯︵ ┻━┻
bank.dat.2$subscribed <- as.factor(bank.dat.2$subscribed)

as.numeric(as.character(bank.dat.2$subscribed))
##---------------------------------------
## END DATA PREPARATION
##---------------------------------------
##---------------------------------------
## BEGIN TRAIN / TEST SPLIT
##---------------------------------------
# Set seed
set.seed(1984)

# Shuffle row indices: rows
rows <- sample(nrow(bank.dat.2))

# Randomly order data
bank.dat.2 <- bank.dat.2[rows, ]

# train-test sizes
split <- round(nrow(bank.dat.2) * 0.75,0)

# Create train
bank.train <- bank.dat.2[1:split, ]

# Create test
bank.test <- bank.dat.2[(split + 1):nrow(bank.dat.2), ]

##---------------------------------------
## END TRAIN / TEST SPLIT
##---------------------------------------
##---------------------------------------
## BEGIN MODELING
##---------------------------------------

# this is a pointless line (copied this code from S.O.)
rfe_records <- bank.train

#  recursive feature elimination control object
rfe_ctrl <- rfeControl(method = "repeatedcv"
                       , repeats = 2 # how many times to repeat cv process
                       , number = 5 #  k-folds
                       , verbose = TRUE
                       , returnResamp = "all")


# number of predictors
number_predictors <- dim(rfe_records)[2]-1 # less 1 for the response var!

# partition of training set into Xs and Y objects
x <- dplyr::select(rfe_records, -subscribed) # predictors
y <- as.factor(as.numeric(rfe_records$subscribed)) # response

# run Recursive Feature Elimination
Profile <- rfe(x, y, rfeControl = rfe_ctrl
                  , sizes = c(5, 10, 14, 16 ) #c(1:number_predictors) # number of predictors that can be kept (candidate space)
                  , method="glmnet" # use glmnet
                  , family="binomial") # binary classification (i.e. logistic regression)

print(Profile) # show the results

glmProfile <- Profile

##---------------------------------------
## END MODELING
##---------------------------------------
##---------------------------------------
## BEGIN MODEL EVALUATION
##---------------------------------------

# summary of the feature selection/cv process
glmProfile

# something?
glmProfile$fit

# detail on each CV round (resampling round)
head(glmProfile$resample)

# produce the performance profile across different subset sizes
trellis.par.set(caretTheme())
plot(glmProfile, type = c("g", "o"))

x.validate <- bank.test[1:(ncol(bank.test)-1)]

y.pred <- predict(glmProfile, x.validate, type = "response")

y.pred <- as.numeric(y.pred)
y.pred <- ifelse(y.pred == 2, 1, 0)
y.actual <- as.numeric(ifelse(bank.test$subscribed == 2, 1, 0))
#errors <-  y.actual - y.pred

y.pred.2 <- prediction(y.pred, y.actual)
roc.perf = performance(y.pred.2, measure = "tpr", x.measure = "fpr")
plot(roc.perf)

auc.perf <- performance(y.pred.2, measure = "auc")




#accuracy <- 1 - sum(errors) / length(errors)
#accuracy
#my_RMSE<-caret::RMSE(y.pred,as.numeric(bank.test$subscribed))
#my_RMSE

#glmProfile$pred$mtry == 2
#attributes(glmProfile)
#glmProfile$metric
##---------------------------------------
## END MODEL EVALUATION
##---------------------------------------

# https://stackoverflow.com/questions/30425523/non-linear-regressions-with-caret-package-in-r
# https://stackoverflow.com/questions/41664432/how-to-do-recursive-feature-elimination-with-logistic-regression
# https://topepo.github.io/caret/recursive-feature-elimination.html
# https://stackoverflow.com/questions/18242692/r-package-caret-rfe-function-how-to-customize-metric-to-use-auc
# https://stackoverflow.com/questions/31138751/roc-curve-from-training-data-in-caret
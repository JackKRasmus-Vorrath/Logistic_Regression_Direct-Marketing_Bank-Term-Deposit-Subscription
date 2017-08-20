# Ian Kinskey, Dr. Jack Rasmussen-Vorath, Alice Karanja

library(caret)
library(dplyr)
library(ROCR)
library(MASS)


# set the working directory
# setwd('/Users/iankinskey/Google_Drive/Education/Data_Science/DS@SMU/Courses/AppliedStatistics_6372/Projects/project_3/notebooks')

# load the data
bank.dat <- read.csv('../data_sets/bank/bank-full.csv', sep=";")

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

# Housekeeping
rm(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, c11, c12, c13, c14, c15, c16, c17, c18) # de-clutter
rm(bank.dat) # get rid of bank.dat (free up memory)

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
bank.validate <- bank.dat.2[(split + 1):nrow(bank.dat.2), ]

# housekeeping
rm(bank.dat.2) # get rid of bank.dat.2 (free up memory)
rm(rows) # de-clutter
rm(split) # de-clutter

##---------------------------------------
## END TRAIN / TEST SPLIT
##---------------------------------------
##---------------------------------------
## BEGIN MODELING
##---------------------------------------

# number of predictors
number_predictors <- dim(bank.train)[2]-1 # less 1 for the response var!

# partition of training set into Xs and Y objects
x <- dplyr::select(bank.train, -subscribed) # predictors
y <- as.factor(as.numeric(bank.train$subscribed)) # response

myLRFuncs <- lrFuncs
myLRFuncs$summary <- twoClassSummary
rfe.ctrl <- rfeControl(functions = myLRFuncs,
                       method = "repeatedcv",
                       repeats=2,
                       # method = "cv",
                       number = 5,
                       verbose = TRUE
                       , returnResamp = "all")

train.ctrl <- trainControl(method="none",
                           classProbs=TRUE,
                           summaryFunction=twoClassSummary,
                           verbose=TRUE)
glm_rfe_ROC <- rfe(x, y,
                   sizes=c(1:number_predictors),
                   rfeControl=rfe.ctrl,
                   # family="binomial",
                   family=binomial(link="logit"), # this is the default link, but making explicit for clarity
                   method="glm", # using the generalized linear models package
                   metric="ROC", # set selection metric to receiver operating characteristic
                   trControl=train.ctrl)

# housekeeping
rm(myLRFuncs) # de-cluttering the environment
rm(number_predictors) # de-cluttering the environment
rm(rfe.ctrl) # de-cluttering the environment
rm(train.ctrl) # de-cluttering the environment



##---------------------------------------
## END MODELING
##---------------------------------------
##---------------------------------------
## BEGIN MODEL EVALUATION
##---------------------------------------

# summary of the feature selection/cv process
glm_rfe_ROC

# something?
glm_rfe_ROC$fit

# histograms of various metrics
hist(glm_rfe_ROC$resample$ROC)
hist(glm_rfe_ROC$resample$Sens)
hist(glm_rfe_ROC$resample$Spec)

# produce the performance profile across different subset sizes
trellis.par.set(caretTheme())
plot(glm_rfe_ROC, type = c("g", "o"))

x.validate <- bank.validate[1:(ncol(bank.validate)-1)]
y.validate <- ifelse(as.numeric(bank.validate$subscribed) == 2, 1, 0)
y.pred <- predict(glm_rfe_ROC, x.validate)
y.pred$pred <- as.numeric(as.character(y.pred$pred))
y.pred$pred <- ifelse(y.pred$pred == 2, 1, 0)
y.actual <- as.numeric(ifelse(bank.validate$subscribed == 2, 1, 0))

# Root Mean Square Error statistic of validation set
my_RMSE<-caret::RMSE(y.pred, y.validate)
my_RMSE

# ROC Curve
validation.prediction <- prediction(y.pred$pred, y.validate)
roc.perf = performance(validation.prediction, measure = "tpr", x.measure = "fpr")
plot(roc.perf)

# Confusion matrix (reveals how "confused" the model is)
v_actual <- as.character(ifelse(y.actual == 0, "no", "yes"))
p_class <- as.character(ifelse(y.pred[2] > 0.5, "yes", "no")) # 50% cut point (not sure what model uses)
cm <- confusionMatrix(p_class, v_actual , positive="yes") # this is giving a confusion matrix with vars in wrong order
cm

model.accuracy <- cm$overall[1] # Accuracy = classifications correct / all classifications
model.kappa <- cm$overall[2] # Kappa = (observed accuracy - expected accuracy)/(1 - expected accuracy) 
model.sens <- cm$byClass[1] # Sensitivity =  true positive / (true positive + false negative) [also called true positive rate]
model.spec <- cm$byClass[2] # Specificity = true neg / (actual 'no'  + actual 'no')
model.ppv <- cm$byClass[3] # Pos Pred Value = (sensitivity * prevalence)/((sensitivity*prevalence) + ((1-specificity)*(1-prevalence)))
model.npv <- cm$byClass[4] # Neg Pred Value  = (specificity * (1-prevalence))/(((1-sensitivity)*prevalence) + ((specificity)*(1-prevalence)))
model.prec <- cm$byClass[5] # Precision = true positive / (true positive + false positive)
model.f1 <- cm$byClass[7] # F1 = (1+beta^2)*precision*recall/((beta^2 * precision)+recall)   *where beta = 1 for this function.
model.prev <- cm$byClass[8] # Prevalence = (true positive + false negative) / sum( all cells)
model.detr <- cm$byClass[9] # Detection Rate = true positive / sum(all cells)
model.detp <- cm$byClass[10] # Detection Prevalence = (true positive + false positive ) / sum( all cells)
model.bacc <- cm$byClass[11] # Balanced Accuracy = (sensitivity+specificity)/2


# housekeeping
rm(my_RMSE)
rm(validation.accuracy)
rm(errors)
rm(x)
rm(y)
rm(y.actual)
rm(y.validate)
rm(x.validate)
rm(bank.train)
rm(bank.validate)
rm(y.pred)
rm(cm)
rm(model.accuracy)
rm(model.kappa)
rm(model.sens)
rm(model.spec)
rm(model.ppv)
rm(model.npv)
rm(model.prec)
rm(model.f1)
rm(model.prev)
rm(model.detr)
rm(model.detp)
rm(model.bacc)
rm(p_class)
rm(v_actual)

##---------------------------------------
## END MODEL EVALUATION
##---------------------------------------

# https://stackoverflow.com/questions/30425523/non-linear-regressions-with-caret-package-in-r
# https://stackoverflow.com/questions/41664432/how-to-do-recursive-feature-elimination-with-logistic-regression
# https://topepo.github.io/caret/recursive-feature-elimination.html
# https://stackoverflow.com/questions/18242692/r-package-caret-rfe-function-how-to-customize-metric-to-use-auc
# https://stackoverflow.com/questions/31138751/roc-curve-from-training-data-in-caret
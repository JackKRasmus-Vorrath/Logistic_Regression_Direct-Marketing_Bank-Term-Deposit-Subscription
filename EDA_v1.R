library(gmodels) # for pretty contingency tables
library(reshape) # for data shape transforms
library(ggplot2) # for plots


# set the working directory
setwd('/Users/jkras/Desktop/R_Docs/R_Projects/Logistic_Regression_Direct-Marketing_Bank-Term-Deposit-Subscription')

# load the data
bank.dat <- read.csv('bank-full.csv', sep=";")

# rename columns for clarity
colnames(bank.dat) <- c("age"
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

# preview/summarize data
head(bank.dat)
str(bank.dat)
summary(bank.dat)

# proportions of subscribed
table(bank.dat$subscribed)
prop.table(table(bank.dat$subscribed))

# type of job
CrossTable(bank.dat$job.type, bank.dat$subscribed)
z <- melt(data.frame(table(bank.dat$job.type, bank.dat$subscribed)))
colnames(z) <- c("job.type", "subscribed","variable","value")
ggplot(z, aes(subscribed, job.type)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Legend label", low = "red", high = "green") + 
  theme_bw()

# marital status
CrossTable(bank.dat$marital.status, bank.dat$subscribed)
z <- melt(data.frame(table(bank.dat$marital.status, bank.dat$subscribed)))
colnames(z) <- c("marital.status", "subscribed","variable","value")
ggplot(z, aes(subscribed, marital.status)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Legend label", low = "red", high = "green") + 
  theme_bw()

# highest level of education
CrossTable(bank.dat$highest.educ, bank.dat$subscribed)
z <- melt(data.frame(table(bank.dat$highest.educ, bank.dat$subscribed)))
colnames(z) <- c("highest.educ", "subscribed","variable","value")
ggplot(z, aes(subscribed, highest.educ)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Legend label", low = "red", high = "green") + 
  theme_bw()

# has a credit default in history
CrossTable(bank.dat$credit.default, bank.dat$subscribed)
z <- melt(data.frame(table(bank.dat$credit.default, bank.dat$subscribed)))
colnames(z) <- c("credit.default", "subscribed","variable","value")
ggplot(z, aes(subscribed, credit.default)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Legend label", low = "red", high = "green") + 
  theme_bw()


# has housing loan
CrossTable(bank.dat$has.housing.loan, bank.dat$subscribed)
z <- melt(data.frame(table(bank.dat$has.housing.loan, bank.dat$subscribed)))
colnames(z) <- c("has.housing.loan", "subscribed","variable","value")
ggplot(z, aes(subscribed, has.housing.loan)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Legend label", low = "red", high = "green") + 
  theme_bw()

# has personal loan
CrossTable(bank.dat$has.personal.loan,bank.dat$subscribed)
z <- melt(data.frame(table(bank.dat$has.personal.loan, bank.dat$subscribed)))
colnames(z) <- c("has.personal.loan", "subscribed","variable","value")
ggplot(z, aes(subscribed, has.personal.loan)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Legend label", low = "red", high = "green") + 
  theme_bw()

# contact type
CrossTable(bank.dat$contact.type,bank.dat$subscribed)
z <- melt(data.frame(table(bank.dat$contact.type, bank.dat$subscribed)))
colnames(z) <- c("contact.type", "subscribed","variable","value")
ggplot(z, aes(subscribed, contact.type)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Legend label", low = "red", high = "green") + 
  theme_bw()

# last contact day of month
CrossTable(bank.dat$last.contact.dom,bank.dat$subscribed)
z <- melt(data.frame(table(bank.dat$last.contact.dom, bank.dat$subscribed)))
colnames(z) <- c("last.contact.dom", "subscribed","variable","value")
ggplot(z, aes(subscribed, last.contact.dom)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Legend label", low = "red", high = "green") + 
  theme_bw()

# last contact month of year
CrossTable(bank.dat$last.contact.moy, bank.dat$subscribed)
z <- melt(data.frame(table(bank.dat$last.contact.moy, bank.dat$subscribed)))
colnames(z) <- c("last.contact.moy", "subscribed","variable","value")
ggplot(z, aes(subscribed, last.contact.moy)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Legend label", low = "red", high = "green") + 
  theme_bw()

# prev.outcome
CrossTable(bank.dat$prev.outcome, bank.dat$subscribed)
z <- melt(data.frame(table(bank.dat$prev.outcome, bank.dat$subscribed)))
colnames(z) <- c("prev.outcome", "subscribed","variable","value")
ggplot(z, aes(subscribed, prev.outcome)) +
  geom_tile(aes(fill = value)) +
  geom_text(aes(label = value), color="white") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  scale_fill_gradient("Legend label", low = "red", high = "green") + 
  theme_bw()
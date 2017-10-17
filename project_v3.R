setwd("~/Dropbox (Personal)/Coursera Data Science/Practical Machine Learning")

library(sqldf)
library(caret)
library(dplyr)

# download data
link = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
raw.dat = read.csv.sql(link, sql = "select * from file") 
dat = raw.dat %>% mutate(user = user_name %>% gsub('"', "", .) %>% factor, 
               activity = classe %>% factor) %>% 
      select_if(function(x) class(x) != "character") %>%
      select(-contains("timestamp"), -num_window)
dim(dat)

raw.dat$num_window %>% tapply(raw.dat$user_name, function(x) table(x) %>% mean)
raw.dat %>% filter(user_name == '"carlitos"') %>% group_by() 

link = "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
validation = read.csv.sql(link, sql = "select * from file")
validation = validation %>% 
              mutate(user = user_name %>% gsub('"', "", .) %>% factor) %>% 
                select_if(function(x) class(x) != "character") %>%
                select(-contains("timestamp"), -problem_id, -num_window)
dim(validation)

# double check both datasets have same covariates except activity
dplyr::setdiff(colnames(validation), colnames(dat))
dplyr::setdiff(colnames(dat), colnames(validation))

# partition data into training and test sets
set.seed(80537)
trainSet = createDataPartition(dat$activity, list = F, p = .7)
training = dat[trainSet, ]
testing = dat[-trainSet, ]

# preprocessing data
## select numeric predictors
numPred = training %>% 
          select_if(function(x) class(x) == "numeric" | 
                      class(x) == "integer")

## reconfigure training set per user
userTrain = data.frame(numPred, activity = training$activity,
                       user = training$user)

(pc = preProcess(userTrain, method = "pca", thresh = .9))
trainPC = predict(pc, userTrain)

(nzv = nearZeroVar(trainPC))
if (length(nzv) > 0) {
  trainPC = trainPC[, -nzv]
}

# model training
train_control = trainControl(method = "cv", number = 10, verboseIter = T)
tic = proc.time()
set.seed(80537)
model = train(activity ~ ., 
               data = trainPC, 
               method = "parRF", 
               trControl = train_control)
toc = proc.time()
model$results
cat("Minutes elapsed in model fitting:", (toc[3] - tic[3])/60)

pred = predict(model, testing %>% select(-activity))

CM = confusionMatrix(pred, testing$activity)

save.image("ml_project.RData")
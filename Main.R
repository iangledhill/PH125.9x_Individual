#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Author: Ian Gledhill
# Descrition: This is the main script for the individual learner 
# project.
#_____________________________________________________________________

set.seed(2039)

# Install any required packages

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("ggrepel", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(ggrepel)


multiMerge = function(filePattern, nrows){
  
  filenames = list.files(pattern = filePattern, full.names = TRUE)
  
  datalist = lapply(filenames, 
                function(fileName){
                    targetFile <- str_replace(fileName, "\\./(.*)\\.zip", "\\1")
                    print(paste("Loading file", targetFile, "from", fileName))
                    
                    unzippedFile <- unz(fileName, targetFile)
                    
                    read.csv(unzippedFile,
                             header = TRUE,
                             nrows=nrows, 
                             quote="\"",
                             sep=",",
                             colClasses=c(
                               "MONTH"="factor",
                               "ORIGIN_STATE_ABR"="factor", 
                               "ORIGIN"="factor",
                               "OP_UNIQUE_CARRIER"="factor",
                               "DEP_TIME_BLK"="factor", 
                               "DAY_OF_WEEK"="factor", 
                               "OP_CARRIER_FL_NUM"="factor"),
                             stringsAsFactors = FALSE)
                })
  
  Reduce(function(x,y) {merge(x, y, all = TRUE)}, datalist)

}

flights <- multiMerge("2018.*zip", 750000)
original_data_size <- nrow(flights)

# Retain top 20 airports
top20Airports <- flights %>% group_by(ORIGIN) %>% summarise(n = n()) %>% arrange(desc(n)) %>% top_n(20)
flights <- flights[flights$ORIGIN %in% top20Airports$ORIGIN,]

# Restate factors
flights$ORIGIN <- factor(flights$ORIGIN)
flights$OP_UNIQUE_CARRIER <- factor(flights$OP_UNIQUE_CARRIER)

sample_data_size <- 100000
flights <- flights[sample(nrow(flights), sample_data_size),] %>%
            mutate(CANCELLED = as.logical(CANCELLED))

flights <- flights %>%
            mutate(
              ACTIVE = ifelse(!is.na(DEP_DELAY), TRUE, FALSE),
              DEP_STATUS = as.factor(case_when(
                  flights$CANCELLED ~ "CANCELLED", 
                  flights$DEP_DELAY_GROUP < 0 ~ "EARLY",
                  flights$DEP_DELAY_GROUP == 0 ~ "ONTIME",
                  flights$DEP_DELAY_GROUP > 0 ~ "DELAYED",
                  TRUE ~ "UNKNOWN"))
            )

levels(flights$DAY_OF_WEEK) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Unknown")

active_flights <- flights[flights$ACTIVE,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Partitions the supplied data into a test and train data set using the 
# supplied percentage to the training data set.
#_____________________________________________________________________

test_index <- createDataPartition(y = active_flights$DEP_DELAY, 
                                  times = 1,
                                  p = 0.2, 
                                  list = FALSE)

train_set <- active_flights[-test_index,]
test_set <- active_flights[test_index,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build Train and Test
#______________________________________________________________________
# These are the attributes that a passenger would know about in advance

predictor_columns <- c("DEP_TIME_BLK", "DAY_OF_WEEK", "OP_UNIQUE_CARRIER", "ORIGIN")

# Build train data frame
y_train<-train_set[,"DEP_DELAY"] 
x_train<-train_set[, predictor_columns]
train_df<-data.frame(x=x_train,y=y_train)

# Build test data frame
y_test<-test_set[,"DEP_DELAY"] 
x_test<-test_set[, predictor_columns]
test_df<-data.frame(x=x_test)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CART training
#______________________________________________________________________

library(rpart)
library(rpart.plot)

rpart_mdl <- train(y ~ ., method="rpart", data=train_df, tuneGrid=data.frame(cp = seq(0, 0.2, len=10)), control = rpart.control(minsplit = 3, cp = 0.01), , na.action = na.exclude)

rpart_predictions <- predict(rpart_mdl, newdata=test_df)

rpart_RMSE <- RMSE(rpart_predictions, y_test)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Random Forests
#______________________________________________________________________


library(randomForest)

rf_mdl = randomForest(y ~ ., ntree=200, data=train_df, keep.forest=TRUE,importance=TRUE,oob.prox =FALSE, na.action = na.exclude)

rf_predictions <- predict(rf_mdl, newdata=test_df)

rf_RMSE <- RMSE(rf_predictions, y_test)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualizer helpers
#______________________________________________________________________

format_percent <- function(n){
  percent(n)
}

format_number <- function(n) {
  prettyNum(n, big.mark = ",")
}

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

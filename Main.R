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

library(tidyverse)
library(caret)


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
                               "ORIGIN_AIRPORT_ID"="factor",
                               "ORIGIN"="factor",
                               "ORIGIN_STATE_ABR"="factor", 
                               "DAY_OF_MONTH"="factor", 
                               "DEP_TIME_BLK"="factor", 
                               "DAY_OF_WEEK"="factor", 
                               "OP_UNIQUE_CARRIER"="factor", 
                               "OP_CARRIER_FL_NUM"="factor"),
                             stringsAsFactors = FALSE)
                })
  
  Reduce(function(x,y) {merge(x, y, all = TRUE)}, datalist)
}

flights <- multiMerge("2018.*zip", 150000)
flights <- flights[sample(nrow(flights), 30000),]

names(flights$DAY_OF_WEEK) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday", "Unknown")

cancelled_flights <- flights[is.na(flights$DEP_DELAY),]
active_flights <- flights[!is.na(flights$DEP_DELAY),]

rm(flights)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Partitions the supplied data into a test and train data set using the 
# supplied percentage to the training data set.
#_____________________________________________________________________

set.seed(755)
test_index <- createDataPartition(y = active_flights$DEP_DELAY, times = 1,
                                  p = 0.2, list = FALSE)

train_set <- active_flights[-test_index,]
test_set <- active_flights[test_index,]


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Build Train and Test
#______________________________________________________________________
# These are the attributes that are known about in advance

predictor_columns <- c("MONTH", "ORIGIN_STATE_ABR", "DISTANCE", "DAY_OF_MONTH", "DEP_TIME_BLK", "DAY_OF_WEEK", "OP_UNIQUE_CARRIER")
predictor_columns <- c("DAY_OF_WEEK")
predictor_columns <- c("MONTH", "ORIGIN_STATE_ABR", "DISTANCE", "DAY_OF_MONTH", "DEP_TIME_BLK", "DAY_OF_WEEK", "OP_UNIQUE_CARRIER")
predictor_columns <- c("DEP_TIME_BLK", "DAY_OF_WEEK", "OP_UNIQUE_CARRIER")

y_train<-train_set[,"DEP_DELAY"] 
x_train<-train_set[, predictor_columns]

train_df<-data.frame(x=x_train,y=y_train)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# CART training
#______________________________________________________________________

library(rpart)

train_rpart <- train(y ~ ., method="rpart", tuneGrid=data.frame(cp = seq(0, 0.2, len=10)), data = train_df)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Visualizer helpers
#______________________________________________________________________

format_percent <- function(n){
  percent(n)
}

format_number <- function(n) {
  prettyNum(n, big.mark = ",")
}


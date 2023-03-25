### Importing Libraries ###
install.packages("doParallel")
library(datasets)
library(caret)

# Importing dhfr dataset
data(dhfr)

# Check for missing data
sum(is.na(dhfr))

# Set seed to make sure model is reproducible
set.seed(888)

# Perform stratified random split of data
TrainingIndex = createDataPartition(dhfr$Y, p=0.8, list = FALSE)
TrainingSet = dhfr[TrainingIndex,] # Training set
TestingSet = dhfr[-TrainingIndex,] # Test set


### Random Forest ###

# Run normally without parallel processing
start.time = proc.time()
Model = train(Y ~ ., 
              data = TrainingSet, 
              method = "rf",
              tuneGrid = data.frame(mtry = seq(5, 15, by = 5))
          )
stop.time = proc.time()
run.time = stop.time - start.time
print(run.time)

# Time taken is 36.36 seconds

# Using doParallel

library(doParallel)

cl = makePSOCKcluster(5)
registerDoParallel(cl)

start.time = proc.time()
Model = train(Y ~ .,
              data = TrainingSet, 
              method = "rf",
              tuneGrid = data.frame(mtry = seq(5, 15, by = 5))
        )

stop.time = proc.time()
run.time = stop.time - start.time
print(run.time)

stopCluster(cl)

# Time taken is 9.83 seconds (3.7x faster than without parallel)

unregister_dopar <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}

# Apply model for prediction
Model.training = predict(Model, TrainingSet)

# Model performance (Displays confusion matrix and statistics)
Model.training.confusion = confusionMatrix(Model.training, TrainingSet$Y)

print(Model.training.confusion)

# Feature Importance
Importance = varImp(Model)
plot(Importance, top = 25)
plot(Importance, top = 25, col = "red")

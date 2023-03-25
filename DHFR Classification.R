### Importing Libraries ###
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


### SVM Model (polynomial kernel) ###

# Build Training Model
Model = train(Y ~ ., data = TrainingSet, 
              method = "svmPoly",
              na.action = na.omit,
              preProcess=c("scale", "center"),
              trControl = trainControl(method = "none"),
              tuneGrid = data.frame(degree=1, scale = 1, C = 1)
)


# Build CV Model
Model.cv = train(Y ~ ., data = TrainingSet,
                 method = "svmPoly",
                 na.action = na.omit,
                 preProcess = c("scale", "center"),
                 trControl = trainControl(method = "cv", number = 10),
                 tuneGrid = data.frame(degree=1, scale=1, C=1)
)

# Apply model for prediction
Model.training = predict(Model, TrainingSet) # Apply model to make prediction on Training set
Model.testing = predict(Model, TestingSet) # Apply model to make prediction on Testing set
Model.cv = predict(Model.cv, TrainingSet) # Perform cross-validation

# Model performance
Model.training.confusion = confusionMatrix(Model.training, TrainingSet$Y)
Model.testing.confusion = confusionMatrix(Model.testing, TestingSet$Y)
Model.cv.confusion = confusionMatrix(Model.cv, TrainingSet$Y)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance = varImp(Model)
plot(Importance, top = 25)




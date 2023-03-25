# Classification Model based on Iris Dataset

# Importing Libraries
library(datasets)
library(caret)

# Importing iris dataset
data(iris)

# Check for missing data
sum(is.na(iris))

# Set seed to make sure model is reproducible
set.seed(888)

# Perform stratified random split of data
TrainingIndex = createDataPartition(iris$Species, p=0.8, list = FALSE)
TrainingSet = iris[TrainingIndex,] # Training set
TestingSet = iris[-TrainingIndex,] # Test set

# Compare scatter plot of the 80 and 20 data subsets
plot(TrainingSet$Sepal.Width, TrainingSet$Sepal.Length, col = "green")
plot(TestingSet$Sepal.Width, TestingSet$Sepal.Length, col = "blue")


### SVM Model (polynomial kernel) ###

# Build Training Model
Model = train(Species ~ ., data = TrainingSet, 
              method = "svmPoly",
              na.action = na.omit,
              preProcess=c("scale", "center"),
              trControl = trainControl(method = "none"),
              tuneGrid = data.frame(degree=1, scale = 1, C = 1)
)


# Build CV Model
Model.cv = train(Species ~ ., data = TrainingSet,
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
Model.training.confusion = confusionMatrix(Model.training, TrainingSet$Species)
Model.testing.confusion = confusionMatrix(Model.testing, TestingSet$Species)
Model.cv.confusion = confusionMatrix(Model.cv, TrainingSet$Species)

print(Model.training.confusion)
print(Model.testing.confusion)
print(Model.cv.confusion)

# Feature importance
Importance = varImp(Model)
plot(Importance)
plot(Importance, col = "green")




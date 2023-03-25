# Loading Iris data set

install.packages("datasets")

library(datasets)
data(iris)

iris = datasets::iris

View(iris)


# Display Summary Statistics
#head/tail
head(iris, 4)
tail(iris, 4)

summary(iris)
summary(iris$Sepal.Length)

# Check for missing data
sum(is.na(iris))


install.packages("skimr")
library(skimr)

skim(iris)

# Group data by Species then perform skim
iris %>%
  dplyr::group_by(Species) %>%
  skim()


# Quick data visualization using base R

# Panel plots
plot(iris)
plot(iris, col = "red")

# Scatter plot
plot(iris$Sepal.Width, iris$Sepal.Length)

plot(iris$Sepal.Width, iris$Sepal.Length, col = "red")

plot(iris$Sepal.Width, iris$Sepal.Length, col = "red",
     xlab = "Sepal Width", ylab = "Sepal Length")

# Histogram
hist(iris$Sepal.Width)
hist(iris$Sepal.Width, col = "red")

# Feature plots
install.packages("caret")
library(caret)
featurePlot(x = iris[,1:4],
            y = iris$Species,
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))


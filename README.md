# R Classification Projects

Two beginner machine learning projects in R. Each one explores a dataset, then trains a model to predict a category.

| Project | Question | Files |
|---|---|---|
| Iris | Which of 3 species is this flower, based on its petal and sepal sizes? | [Iris Data Understanding.R](Iris%20Data%20Understanding.R) · [Iris Classification.R](Iris%20Classification.R) |
| DHFR | Is this chemical compound active or inactive against the DHFR enzyme? | [DHFR Data Understanding.R](DHFR%20Data%20Understanding.R) · [DHFR Classification.R](DHFR%20Classification.R) |

## How each project works

1. The **Data Understanding** script loads the data, prints summary stats, and checks for missing values.
2. The **Classification** script splits the data 80/20 into training and test sets. It trains a support vector machine (SVM) with `caret`, then scores it on the test set and with 10-fold cross-validation.

## Run it

Open a script in RStudio and run it top to bottom. The data comes with R packages, so there is nothing to download.

```r
install.packages(c("caret", "skimr"))
```

**Tools:** R, caret, skimr

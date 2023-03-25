# 1. Loading the DHFR data
install.packages("RCurl")
library(RCurl)
data(dhfr)

View(dhfr)


# 2. Check for missing data

sum(is.na(dhfr))


# 3. If data is clean, randomly introduce NA to the dataset

na.gen = function(data, n) {
  i = 1
  while (i < n+1) {
    idx1 = sample(1:nrow(data), 1)
    idx2 = sample(1:ncol(data), 1)
    data[idx1, idx2] = NA
    i = i+1
  }
  return(data)
}

# Choose 1 of the following to run (they'll probably produce the same result)

dhfr = dhfr[, -1]

dhfr = na.gen(dhfr, 100)

# dhfr = na.gen(n=100, data=dhfr)

# 4. Check again for missing data

sum(is.na(dhfr))

colSums(is.na(dhfr))

str(dhfr)

# Lists rows with missing data
missingdata = dhfr[!complete.cases(dhfr), ]

sum(is.na(missingdata))

# If above sum is 0, this means that there is no missnig data and proceed to modeling.
# If above sum is greater than 0, then proceed to #5

# 5. Handling the missing data. There are 2 options, decide and choose only 1

# 5.1. Simplyh delete all entries with missing data

clean.data = na.omit(dhfr)

sum(is.na(clean.data))

# 5.2. Imputation: Replace missing values with the columns (better method)

# MEAN
dhfr.impute = dhfr

for (i in which(sapply(dhfr.impute, is.numeric))) {
  dhfr.impute[is.na(dhfr.impute[, i]), i] = mean(dhfr.impute[, i], na.rm = TRUE)
}

sum(is.na(dhfr.impute))

# MEDIAN
dhfr.impute = dhfr

for (i in which(sapply(dhfr.impute, is.numeric))) {
  dhfr.impute[is.na(dhfr.impute[, i]), i] = median(dhfr.impute[, i], na.rm = TRUE)
}

sum(is.na(dhfr.impute))




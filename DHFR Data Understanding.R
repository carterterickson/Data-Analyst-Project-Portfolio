#### Loading dhfr data set ###

install.packages("datasets")
install.packages("skimr")
install.packages("caret")

library(datasets)
data(dhfr)

# View the data
View(dhfr)


### Display Summary Statistics ###

# head/tail
head(dhfr, 4)
tail(dhfr, 4)

# Summary
summary(dhfr)
summary(dhfr$Y)

# Check for missing data
sum(is.na(dhfr))

library(skimr)
skim(dhfr)

# Group data by Y (biological activity) then perform skim
dhfr %>%
  dplyr::group_by(Y) %>%
  skim()


### Quick data visualization using base R ###

# Scatter plot
plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol)

plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = "red")

plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = dhfr$Y) # Color by Y

plot(dhfr$moe2D_zagreb, dhfr$moe2D_weinerPol, col = "red",
     xlab = "moe2D_zagreb", ylab = "moe2D_weinerPol")

# Histogram
hist(dhfr$moe2D_zagreb)
hist(dhfr$moe2D_zagreb, col = "red")

# Feature plots
library(caret)
featurePlot(x = dhfr[,2:21],
            y = dhfr$Y,
            plot = "box",
            strip = strip.custom(par.strip.text = list(cex=.7)),
            scales = list(x = list(relation="free"),
                          y = list(relation="free")))


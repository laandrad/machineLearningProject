load("ml_project.RData")

library(sm)
# exploratory analysis
par(mfrow = c(1, 3))
training$roll_belt %>% sm.density.compare(training$activity)
title(main = "Combined User Data roll_belt")
training[which(training$user == "carlitos"), "roll_belt"] %>%
  sm.density.compare(training[which(training$user == "carlitos"), "activity"])
title(main = "Carlito's roll_belt")
training[which(training$user == "pedro"), "roll_belt"] %>%
  sm.density.compare(training[which(training$user == "pedro"), "activity"])
title(main = "Pedro's roll_belt")

library(ggplot2)
g = ggplot(data = trainPC[, 2:4], aes(x = PC1, y = PC2, colour = activity))
g + geom_point(alpha = .2)

CM$table
CM$overall[1:2]

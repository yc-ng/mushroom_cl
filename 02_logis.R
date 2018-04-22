library(tidyverse)

# Examine data ------------------------------------------------------------

mushrooms <- read.csv("data/mushrooms.csv")
str(mushrooms)
summary(mushrooms)

# Train/Test split --------------------------------------------------------

set.seed(50)
frac <- 0.7
n <- nrow(mushrooms)
shuffled <- mushrooms[sample(n), ]
train <- shuffled[1:(round(n * frac)), ]
test <- shuffled[(round(n * frac) + 1):n, ]

train_lab <- train$class
test_lab <- test$class

# remove factors with only 1 level
train_glm <- select(train, -veil.type)
test_glm <- select(test, -veil.type)

# Logistic Regression -----------------------------------------------------

mush_logi1 <- glm(class ~ ., data = train_glm, family = binomial)

mush_pred_logi <- predict(mush_logi1, newdata = test_glm, type = "response")

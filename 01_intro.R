library(tidyverse)

# Examine data ------------------------------------------------------------

mushrooms <- read.csv("data/mushrooms.csv")
str(mushrooms)
glimpse(mushrooms)
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


# alternatively, if all rows in mushrooms are unique (duplicated())
# train <- sample_frac(mushrooms, size = 0.7)
# test <- setdiff(mushrooms, train)


# Decision trees ----------------------------------------------------------

library(rpart)
library(rpart.plot)
set.seed(50)
mush_tree1 <- rpart(class ~ ., data = train, method = "class")

rpart.plot(mush_tree1, type = 4, extra = 101, under = TRUE)

#' interpreting the tree:
#' oodor == cfmpsy ~ poisonous
#' odor == aln & spore.print.color == r ~ poisonous
#' odor == aln & spore.print.color == bhknouwy ~ edible

mush_pred1 <- predict(mush_tree1, newdata = test, type = "class")
(mush_confus1 <- table(mush_pred1, test_lab))
(mush_acc1 <- sum(diag(mush_confus1) / sum(mush_confus1)))


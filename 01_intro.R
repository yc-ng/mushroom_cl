library(tidyverse)
library(rpart)
library(rpart.plot)

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

# alternatively, if all rows in mushrooms are unique (duplicated())
# train <- sample_frac(mushrooms, size = 0.7)
# test <- setdiff(mushrooms, train)


# Decision trees ----------------------------------------------------------

# train the model
set.seed(50)
mush_tree1 <- rpart(class ~ ., data = train, method = "class")

rpart.plot(mush_tree1, type = 4, extra = 101)

#' interpreting the tree:
#' oodor == cfmpsy ~ poisonous
#' odor == aln & spore.print.color == r ~ poisonous
#' odor == aln & spore.print.color == bhknouwy ~ edible

# predicting the test data
mush_pred1 <- predict(mush_tree1, newdata = test, type = "class")

# confusion table
(mush_confus1 <- table(mush_pred1, test$class, deparse.level = 2))
# accuracy
(mush_acc1 <- sum(diag(mush_confus1) / sum(mush_confus1)))
# false negative rate
(mush_fnr1 <- sum(mush_pred1 == "e" & test$class == "p") / sum(test$class == "p"))


# finding out which mushrooms were misclassified
test %>%
    add_column(mush_pred1) %>% 
    filter(class == "p" & mush_pred1 == "e") %>% 
    select(odor, spore.print.color, mush_pred1, class)
#' all 16 misclassifications had no odor and white spore prints

test %>%
    add_column(mush_pred1) %>% 
    filter(odor == "n" & spore.print.color == "w") %>% 
    summarise(correct = sum(mush_pred1 == class), 
              wrong = sum(mush_pred1 != class))
#' 207 mushrooms had no odor, white spore prints; 191 correct 16 wrong  
#' 
#' 
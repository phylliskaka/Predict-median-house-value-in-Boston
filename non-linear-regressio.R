# tidyverse is for easy data maniputation and visualization 
# caret is for easy machine learning workflow
library(tidyverse)
library(caret)
library(MASS)
library(ggplot2)
theme_set(theme_classic())

# dataset 
# use boston dataset in MASS package, for predicting median house value, based
# on the predictor variable lstat(percentage of low status of the population)

# load dataset 
data("Boston", package = "MASS")
# Split the data into training and test set 
set.seed(123)
training.samples<-Bostom$medv %>% 
  createDataParition(p=0.8, list= FALSE)
train.data <-Boston[training.samples, ]
test.data <-Boston[-training.samples,]

ggplot2(train.data, aes(lstat, medv)) + geom_point() + stat_smooth()

# Ploynomial regression 
lm(medv ~ poly(lstat, 2, raw = TRUE), data = train.data)

# Build the model 
moel <- lm(medv ~ poly(lstat, 5, raw = TRUE), data = train.data)
# Mask predictions 
predictions <-model %>% predict(test.data)
# Model performance 
data.frame(
  RMSE = RMSE(predictions, test.data$medv),
  R2 = R2(predictions, test.data$medv)
  )

ggplot2(train.data, aes(lstat, medv)) + geom_point() + stat_smooth(method = lm, 
                                                                   formula = y~poly(x, 5,raw = TRUE))

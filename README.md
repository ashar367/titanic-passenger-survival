# titanic-passenger-survival

*Predict passengers being dead/alive using titanic datasets applying R language and Random forest*

This programs explains how to use the titanic datasets to predicts if a passenger is alive or dead. The variables are-
survival(0 = No, 1 = yes)
pclass (1 = 1st, 2 = 2nd, 3 = 3rd)
age, sex, sibsp (siblings / spouses aboard the Titanic)
parch (parents / children aboard the Titanic)
ticket numer, fare (passenger fare), cabin (cabin number)
emabraked (C = Cherbourg, Q = Queenstown, S = Southampton)

The steps used to train model on window (10) in the R studio Desktop (R-Studio 8.12 build 175481) envionment are decribed below.


# Steps

# 1. Install R studio Desktop

Visit https://rstudio.com/products/rstudio/
Download and install R studio Desktop

# Download Titanic data set from Kaggle

Visit https://www.kaggle.com/c/titanic/data
Download train.csv and test.csv
Place the downloaded files in a folder (say titanic) in any drive

# 2. Set up Working Directory and R studio Environment
load the training and testing data sets in to the R studio

# 3. Data cleaning and Analysis
### Loading the train and test data

train <- read.csv("train.csv", header = TRUE)

test <- read.csv("test.csv", header = TRUE)

### 3a. pclass and survival
![Image of objects]()

### 3a. pclass and survival
![Image of objects]()


# Predicting the Survival of Titanic Passengers

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
![Image of objects](https://github.com/ashar367/titanic-passenger-survival/blob/master/image/Rplot-02.png)

### 3b. Relationship between pclass, survival, and title
![Image of objects](https://github.com/ashar367/titanic-passenger-survival/blob/master/image/pclass-survival-title.png)

### 3c. Relationship between pclass, sex, and survival
![Image of objects](https://github.com/ashar367/titanic-passenger-survival/blob/master/image/sex-pclass-survival.png)

### 3d Survival rates as sex, pclass, and age
![Image of objects](https://github.com/ashar367/titanic-passenger-survival/blob/master/image/Survival%20rates-sex-pclass-age.png)

# 4. Building Machine Learning Models
## 4a. Train a Random Forest (using pclass and title)
![Image of objects](https://github.com/ashar367/titanic-passenger-survival/blob/master/image/rf-title-pclass.png)

## 4b. Applying Random forest and finding out the important variables for prediction
The titles (Mr, Mrs, Miss, Master) has value in predicting the passenger survival as compared to pclass (1, 2, 3), family size, and parch
![Image of objects](https://github.com/ashar367/titanic-passenger-survival/blob/master/image/plot_zoom_png)

## 4c. Prediction visualizing using decision tree
![Image of objects](https://github.com/ashar367/titanic-passenger-survival/blob/master/image/Rplot-33.png)

Titles of "Mr." and "Other" are predicted to perish at an overall accuracy rate of 83.2 %.

Titles of "Master.", "Miss.", and "Mrs." in 1st & 2nd class are predicted to survive

Titles of "Master.", "Miss.", and "Mrs." in 3rd class with family sizes equal to 5, 6, 8, & 11 are predicted to perish with 100% accuracy.

Titles of "Master.", "Miss.", and "Mrs." in 3rd class with family sizes not equal to 5, 6, 8, or 11 are predicted to survive with 59.6% accuracy.

If a person has title Mr and "other", he will survive at 91 instances. 

If the title is neither Mr and "other", may be (Mrs, Miss), and if these people are in the  3 class, with alrge family size none of them will survive. Small family size, 83 instance of survival.

If the title is neither Mr and "other", may be (Mrs, Miss), and if these people are not in the  3 class, 168 instance of survival


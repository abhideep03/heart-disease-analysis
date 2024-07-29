install.packages("dplyr")
install.packages("ggplot2")
install.packages("forcats")
install.packages("rsample")
install.packages("tidyverse")
install.packages("tidymodels")
install.packages("gridExtra")
install.packages("pROC")
install.packages("tidyr")
install.packages("readr")
install.packages("caret")
install.packages("gplots")
install.packages("GGally")
install.packages("dslabs")
install.packages("lubridate")
install.packages("tidytext")
install.packages("RColorBrewer")
install.packages("randomForest")
install.packages("tictoc")
install.packages("e1071")
install.packages("ggpubr")

library(dslabs)
library(lubridate)
library(tidytext)
library("RColorBrewer")
library(randomForest)
library(tictoc)
library(e1071)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(forcats)
library(rsample)
library(tidyverse)
library(gridExtra)
library(pROC)
library(tidyr)
library(readr) 
library(caret)
library(gplots)
library(GGally)


heart <- read.csv("C:/Users/Abhideep/Movies/project/heart.csv")

head(heart)

summary(heart)

str(heart)

names = c("age", "sex", "cp", "trestbps", "chol", "fbs", "restecg", "thalach", "exang", "oldpeak", "slope", "ca", "thal", "heart_disease")
colnames(heart) <- names


heart <- heart %>%
  mutate(sex = case_when(sex == 0 ~ "female",
                         sex == 1 ~ "male")) %>%
  mutate(cp = case_when(cp == 1 ~ "typical angina",
                        cp == 2 ~ "atypical angina",
                        cp == 3 ~ "non-anginal pain",
                        cp == 4 ~ "asymptomatic")) %>%
  mutate(fbs = case_when(fbs == 1 ~ "high",
                         fbs == 0 ~ "low")) %>%
  mutate(exang = case_when(exang == 0 ~ "no",
                           exang == 1 ~ "yes")) %>%
  mutate(heart_disease = case_when(heart_disease == 0 ~ "absence",
                                   TRUE ~ "presence"))

heart <- heart %>%
  mutate(sex = as.factor(sex)) %>%
  mutate(cp = as.factor(cp)) %>%
  mutate(fbs = as.factor(fbs)) %>%
  mutate(exang = as.factor(exang)) %>%
  mutate(heart_disease = as.factor(heart_disease))

heart <- heart %>%
  select(age, sex, cp, trestbps, chol, fbs, thalach, exang, heart_disease) %>%
  rename("max_hr" = "thalach",
         "exercise_angina" = "exang") %>%
  drop_na()

colSums(is.na(heart))

glimpse(heart)

ggplot(heart, mapping = aes(x = age, fill = heart_disease)) +
  geom_histogram() +
  facet_wrap(vars(heart_disease)) +
  labs(title = "Prevelance of Heart Disease Across Age", x = "Age (years)", y = "Count", fill = "Heart Disease")

ggplot(heart, mapping = aes(x=heart_disease, fill = cp)) +
  geom_bar(position = "dodge") +
  labs(title = "Prevelance of Heart Disease for Different Chest Pain Types", x = "Heart Disease", y = "Count", fill = "Chest Pain Type")

ggplot(heart, mapping = aes(x = sex, fill = heart_disease)) +
  geom_bar(position = "fill") +
  labs(x = "Sex", y = "Heart Disease", fill = "Heart Disease") +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12))

ggplot(heart, mapping = aes(x=fbs, fill=heart_disease)) +
  geom_bar(position = "fill") +
  labs(x = "Fasting Blood Sugar", y = "Heart Disease", fill = "Heart Disease") +
  scale_x_discrete(labels = c("low", "high"))+
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12))

ggplot(heart, mapping = aes(x = exercise_angina, fill = heart_disease)) +
  geom_bar(position = "fill") +
  labs(x = "Exercise induced angina", y = "Heart Disease", fill = "Heart Disease") +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 12))

ggplot(heart, mapping = aes(x=trestbps, y=heart_disease)) +
  geom_boxplot() +
  labs(x = "Resting Blood Pressure (mm Hg)", y = "Heart Disease") +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12))

ggplot(heart, mapping = aes(x=chol, y=heart_disease)) +
  geom_boxplot() +
  labs(x = "Serum Cholestoral (mg/dl)", y = "Heart Disease") +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12))

ggplot(heart, mapping = aes(x = max_hr, y = heart_disease)) +
  geom_boxplot() +
  labs(x = "Maximum Heart Rate (bpm)", y = "Heart Disease") +
  theme(axis.text.x = element_text(size = 12), axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12), axis.text.y = element_text(size = 12))

ggcorr(heart, label = TRUE, label_size = 2.5, hjust = 1, layout.exp = 2)



# Ensure the target variable is a factor
heart$heart_disease <- as.factor(heart$heart_disease)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
splitIndex <- createDataPartition(heart$heart_disease, p = 0.8, list = FALSE)
train_set <- heart[splitIndex, ]
test_set <- heart[-splitIndex, ]



# logistic regression model
logistic_fit <- train(heart_disease ~ ., data = train_set, method = "glm", family = binomial)
print(logistic_fit)

log_predictions <- predict(logistic_fit, newdata = test_set)

# Confusion matrix to evaluate the model
logistic_cm <- confusionMatrix(log_predictions, test_set$heart_disease)

print(logistic_cm)

#svm model
ctrl <- trainControl(method = "cv", verboseIter = FALSE, number = 5)

grid_svm <- expand.grid(C = c(0.01, 0.1, 1, 10, 20))

svm_fit <- train(heart_disease ~ .,data = train_set,
                 method = "svmLinear", preProcess = c("center","scale"),
                 tuneGrid = grid_svm, trControl = ctrl)
print(svm_fit)
plot(svm_fit)

svm_predict <- predict(svm_fit, newdata = test_set)

# Confusion matrix to evaluate the model
svm_cm <- confusionMatrix(svm_predict, test_set$heart_disease)

print(svm_cm)

#knn model
ctrl <- trainControl(method = "cv", verboseIter = FALSE, number = 5)

knnFit <- train(heart_disease ~ ., 
                data = train_set, method = "knn", preProcess = c("center","scale"),
                trControl = ctrl , tuneGrid = expand.grid(k = seq(1, 20, 2)))
print(knnFit)
plot(knnFit)

knn_predictions <- predict(knnFit,newdata = test_set )

# Confusion matrix to evaluate the model
knn_cm <- confusionMatrix(knn_predictions, test_set$heart_disease )

print(knn_cm)

logistic_accuracy <- logistic_cm$overall["Accuracy"]
svm_accuracy <- svm_cm$overall["Accuracy"]
knn_accuracy <- knn_cm$overall["Accuracy"]

print(paste("Logistic Regression Accuracy: ", logistic_accuracy*100))
print(paste("SVM Accuracy: ", svm_accuracy*100))
print(paste("k-NN Accuracy: ", knn_accuracy*100))

results <- data.frame(
  Model = c("Logistic Regression", "SVM", "k-NN"),
  Accuracy = c(logistic_accuracy, svm_accuracy, knn_accuracy)
)

# Plot the results
ggplot(results, aes(x = Model, y = Accuracy, fill = Model)) +
  geom_bar(stat = "identity", width = 0.5) +
  ylim(0, 1) +
  labs(title = "Model Comparison: Accuracy",
       x = "Model",
       y = "Accuracy") +
  theme_minimal() +
  theme(legend.position = "none")


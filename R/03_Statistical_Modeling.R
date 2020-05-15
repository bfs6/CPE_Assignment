####Read in Libraries####
library(data.table)
library(tidyverse) 
library(tidymodels)
library(dplyr)
library(Hmisc)
library(ggplot2)
library(plotROC)
library(MLmetrics)


####Read in Data####
pedestrian_stops_clean <- fread("data-raw/pedestrian_stops.csv", sep = ",", header = TRUE)

##Filter Data
pedestrian_stops_clean <- 
  pedestrian_stops_clean %>%
  filter(is.na(ANNOTATION_ERRORS) == TRUE)


####Part 1####
##Build Model Matrix
model_data <- 
  pedestrian_stops_clean %>%
  modelr::model_matrix( ~ SUBJECT_RACE + SUBJECT_GENDER + SEARCH_CONDUCTED) %>%
  as.data.frame() %>% 
  select(-c("(Intercept)"))
names(model_data) <- gsub("SEARCH_CONDUCTEDYES", "SEARCH_CONDUCTED", names(model_data))
model_data$Disposition <- pedestrian_stops_clean$DISPOSITION

##Train/Test Split
set.seed(321)
split <- 
  model_data %>%
  initial_split(prop = 3/4) 
training_data <- 
  split %>%
  training()
testing_data <- 
  split %>%
  testing()  

##Build GLM
pedestrian_stop_glm <- glm(SEARCH_CONDUCTED ~ ., training_data, family = "binomial")
pedestrian_stop_glm_step <- stats::step(pedestrian_stop_glm)

##Get Predictions
preds <- stats::predict(pedestrian_stop_glm, testing_data, type = "response")
preds_step <- stats::predict(pedestrian_stop_glm_step, testing_data, type = "response")


####Part 2####
##Evaluate Models
preds_df <- data.frame(observations = testing_data$SEARCH_CONDUCTED,
                       predictions = preds,
                       predictions_step = preds_step)
preds_df$predictions_classes <- ifelse(preds_df$predictions >= 0.5, 1, 0)
preds_df$predictions_step_classes <- ifelse(preds_df$predictions_step >= 0.5, 1, 0)
accuracy <- mean(preds_df$observations == preds_df$predictions_classes)
accuracy_step <- mean(preds_df$observations == preds_df$predictions_step_classes)
table(preds_df$observations, preds_df$predictions_classes)
table(preds_df$observations, preds_df$predictions_step_classes)
confusion_mat <- caret::confusionMatrix(factor(preds_df$predictions_classes), factor(preds_df$observations))
confusion_mat_step <- caret::confusionMatrix(factor(preds_df$predictions_step_classes), factor(preds_df$observations))
rmse <- RMSE(preds_df$predictions, preds_df$observations)
rmse_step <- RMSE(preds_df$predictions_step, preds_df$observations)
auc <- AUC(preds_df$predictions, preds_df$observations)
auc_step <- AUC(preds_df$predictions_step, preds_df$observations)

##ROC Curve
ggplot(preds_df, aes(d = preds_df$observations, m = preds_df$predictions_step_classes)) + geom_roc()


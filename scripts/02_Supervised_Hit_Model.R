library(tidyverse)
#install.packages('caret')
Packages <- c("here", "caret", "rpartScore", "recipes","MASS","kernlab","naivebayes","yardstick")
lapply(Packages, library, character.only = TRUE)
#install.packages('MLmetrics')
library(MLmetrics)
#install.packages('e1071')
library(e1071)
library(tidymodels)
##load training data if its not loaded already
hit_data_final <- read_csv("data/unsupervised_results.csv") 
names(hit_data_final)
hit_data_final$hit_types <- as.factor(hit_data_final$hit_types)
str(hit_data_final)
# Keep unsupervised data as training and the post May 1 split into test and validation
set.seed(61919) #set the starting number used to create a random set of numbers. this is done so that model splits train and test same way every time.
en_train <- createDataPartition(hit_data_final$hit_types,p=.7,list=FALSE)

training <- hit_data_final[en_train,]
testing <- hit_data_final[-en_train,]

#Create the model that will be used in all of the algorithms
hit_recipe <- recipe(hit_types ~ launch_angle + launch_speed 
                     , data= training) %>% step_scale(all_predictors())

#3 repeats, 10 fold cross-validation with upsampling 
ctrl <- trainControl(method = "repeatedcv",number=10,repeats=3,sampling="up",classProbs = TRUE,
                     summaryFunction = multiClassSummary,
                     savePredictions = "final",allowParallel = TRUE)

########Fit 4 models#########
########Naive Bayes, Regression Tree, Bagged Regression Trees and Support Vector Machine########


class_nb <- train(hit_recipe, data = training, method = "naive_bayes",  trControl = ctrl,metric="logLoss") #bayesian

#class_svm <- train(hit_recipe, data = training, method = "svmLinear",  trControl = ctrl,metric="logLoss") #support vector machine

#get model performance
a <- getTrainPerf(class_nb)
a

a %>% write_csv("model_performance/training_algorithm_performance.csv") #write a summary of the training data performance

# Add predicted hit type to the training data set
results <- training %>%
  mutate(Naive_Bayes = predict(class_nb,training))

# Evaluate the performance of the three models using a confusion matrix
confusionMatrix(results$hit_types,results$Naive_Bayes) #bagged trees are overfit

# Add a prediction to the test dataset
testing_results <- testing %>%
  mutate(Naive_Bayes = predict(class_nb,testing))

#do a confusion matrix on test results and compare to training data
confusionMatrix(testing_results$hit_types,testing_results$Naive_Bayes) #good accuracy and matches training results well

#Naive Bayes is the best model
testing_results %>% write_csv("results/naive_bayes_hit_model.csv") #line-by-line data
dim(testing_results)

naive_bayes_summary <- testing_results %>% 
  ungroup() %>% 
  group_by(Naive_Bayes) %>% 
  dplyr::summarise(launch_speed=mean(launch_speed),launch_angle=mean(launch_angle),est_ba=mean(estimated_ba_using_speedangle),est_woba=mean(estimated_woba_using_speedangle))

naive_bayes_summary
a <- ggplot(testing_results, aes(x = launch_speed,y=launch_angle, 
                                 color = hit_types)) +
  geom_point(size = 1, alpha=0.5,position="jitter") +
  labs(title = "Naive Bayes Classification of Launch Speed vs Launch Angle",
       subtitle = "7 classes assigned",
       caption = "Data Source: Statcast",
       x = "Launch Speed (MPH)", y = "Launch Angle (deg)", color = "Hit Type")+
  # scale_color_discrete(labels=c("Fliner","Hard Air Contact","Hard Ground Contact",
  #                               "High Fly","Soft Air Contact","Soft Ground Contact",
  #                               "Pop up")) +
  theme(legend.key.size = unit(3,"point"))

a
ggsave("final_model_plot.png", width = 6, height = 4)
naive_bayes_summary %>% write_csv("results/naive_bayes_summarized_results.csv")

testing_results %>% filter(Naive_Bayes=="Barrel") %>% View()
head(s)
s

# Ideal to train test on 2019 data and add final model to 2020 data
final_results_contact_data <- st %>%
  mutate(Naive_Bayes = predict(class_nb,st))
final_results_contact_data %>% write_csv("data/contact_data_for_pitch_model.csv")
final_results_contact_data

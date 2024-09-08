### Random forest to forecast AECD data

#load packages and libraries
# install.packages("randomForest")
# install.packages("caret")
# install.packages("Metrics")
library("randomForest")
library("caret")
library("janitor")
library("Metrics")
library("tidyverse")
library("readxl")

#setting seed for reproducability 
set.seed(10)

#add in SEIFA data
seifa <- read.csv("Data/seifa_by_lga_2021.csv") %>% 
  clean_names() %>%
  rename("2021_lga_code" = "x2021_local_government_area_lga_code",
         "2021_lga_name" = "x2021_local_government_area_lga_name",
         "seifa_disadvantage_score" = "index_of_relative_socio_economic_disadvantage") %>% 
  select("2021_lga_code","2021_lga_name","seifa_disadvantage_score","decile") 

lga_stats <- read.csv("Data/lga_community_statistics.csv") %>% 
  mutate(across(everything(), ~ gsub("%", "", .))) %>% 
  mutate(across(-lga_name, ~ as.numeric(gsub("[^0-9.-]", "", .))))
  


#add in AECD datasets
load("Data/ATT57761.RData") 

# Train on the 2021 dataset
training_datset <- dataset_2021 %>% 
  left_join(seifa,by =c("lga_code" = "2021_lga_code")) %>% 
  left_join(lga_stats, by = c("lga_name"))

training_dataset_aecd <- training_datset  %>% 
  select((-c("lga_code","lga_name","workers_health_social","2021_lga_name"))) %>% 
  replace(is.na(.), 0) #%>% 
  #mutate(across(everything(), ~ as.numeric(gsub("[^0-9.-]", "", .))))


training_dataset_workforce <- training_datset  %>% 
  select(-c("lga_code","lga_name","no_dev_vul_2_domains","2021_lga_name")) %>% 
  replace(is.na(.), 0) #%>% 
  #mutate(across(everything(), ~ as.numeric(gsub("[^0-9.-]", "", .))))

#create base datasets for each year
base_2021 <- training_datset
base_2026 <- dataset_2026 %>%
  left_join(seifa,by =c("lga_code" = "2021_lga_code")) %>%
  left_join(lga_stats,by="lga_name") %>% 
  #select("pop_prediction","seifa_disadvantage_score","decile") %>% 
  replace(is.na(.), 0)
base_2031 <- dataset_2031 %>%
  left_join(seifa,by =c("lga_code" = "2021_lga_code")) %>% 
  left_join(lga_stats,by="lga_name") %>% 
  #select("pop_prediction","seifa_disadvantage_score","decile") %>% 
  replace(is.na(.), 0)


#### AECD MODEL #####
# Split the data into training and test sets
splitIndex <- createDataPartition(training_dataset_aecd$no_dev_vul_2_domains, p = 0.7, list = FALSE)
trainData <- training_dataset_aecd[splitIndex, ]
testData <- training_dataset_aecd[-splitIndex, ]

# Train the Random Forest model
rf_model_aecd <- randomForest(no_dev_vul_2_domains ~ ., data = trainData, ntree = 100, mtry = 35, random_state = 55)

# Make predictions on the test set
predictions_aecd <- testData %>% 
  mutate(rf_predictions = predict(rf_model_aecd, testData))

# Evaluate the model
#RMSE with population only as a predictor = 88.77
error_aecd <- rmse(predictions_aecd$rf_predictions,predictions_aecd$no_dev_vul_2_domains)
print(paste("RMSE:", round(error_aecd, 2)))

#### WORKFORCE MODEL #####
splitIndex <- createDataPartition(training_dataset_workforce$workers_health_social, p = 0.7, list = FALSE)
trainData <- training_dataset_workforce[splitIndex, ]
testData <- training_dataset_workforce[-splitIndex, ]

# Train the Random Forest model
rf_model_workforce <- randomForest(workers_health_social ~ ., data = trainData, ntree = 100, mtry = 35, random_state = 55)

# Make predictions on the test set
predictions_workforce <- testData %>% 
  mutate(rf_predictions = predict(rf_model_workforce, testData))

# Evaluate the model
#RMSE with population only as a predictor = 88.77
error_workforce <- rmse(predictions_workforce$rf_predictions,predictions_workforce$workers_health_social)
print(paste("RMSE:", round(error_workforce, 2)))


## make predictions for future years datasets
predictions_2026 <- base_2026 %>% 
  mutate(aecd_prediction = predict(rf_model_aecd,
                                   base_2026 %>% select(-c("lga_code","lga_name","2021_lga_name")))) %>% 
  mutate(workforce_prediction = predict(rf_model_workforce,
                                   base_2026 %>% select(-c("lga_code","lga_name","2021_lga_name")))) %>% 
  left_join(dataset_2021 %>% select("lga_code","workers_health_social"),by="lga_code") %>% 
  rename(workforce_2021 = workers_health_social) %>% 
  mutate(additional_workforce_need =  workforce_prediction- workforce_2021)

predictions_2031 <- base_2031 %>% 
  mutate(aecd_prediction = predict(rf_model_aecd,
                                   base_2031 %>% select(-c("lga_code","lga_name","2021_lga_name")))) %>% 
  mutate(workforce_prediction = predict(rf_model_workforce,
                                        base_2031 %>% select(-c("lga_code","lga_name","2021_lga_name")))) %>% 
  left_join(dataset_2021 %>% select("lga_code","workers_health_social"),by="lga_code") %>% 
  rename(workforce_2021 = workers_health_social) %>% 
  mutate(additional_workforce_need =  workforce_prediction- workforce_2021)

## check which LGAs need the biggest increases in health and social assistance workforce

service_demand <- predictions_2031 %>% 
  select(lga_name,additional_workforce_need) %>% 
  arrange(desc(additional_workforce_need))

head(service_demand)


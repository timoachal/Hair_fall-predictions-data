
### Loading required libraries
library(tidyverse)
library(flextable)
library(e1071)
library(flextable)
library(dlookr)
library(mice)
library(VIM)
library(gtExtras)
library(gtsummary)
library(visdat)
library(caret)
library(ggstatsplot)
library(ggcorrplot)
library(PerformanceAnalytics)
library(performance)
library(class)
library(caTools)
library(sjPlot)
library(ggeffects)
library(effectsize)
library(missRanger)
library(tidymodels)
library(ranger)

## Loading the dataset
library(juicyjuice)
library(V8)

Hair = read.csv("C:/Users/user/Documents/Hair fall Prediction/HairApp/Hair.csv")
head(Hair)
tail(Hair)

# preparation for  plots
theme_set(theme_bw()+
            theme(title = element_text(color = "#4CBB17",
                                       size = 18,
                                       face = "bold"),
                  legend.position = "bottom",
                  axis.text = 
                    element_text(size = 10,
                                 color = "#6495ED",
                                 face = "bold"),
                  axis.title = element_text(size = 12,
                                            face = "bold",
                                            colour = "#FF5733")))

## the first 5 characters                                                                                       colour = "#FF5733")))
Hair %>% 
  head(5) %>% 
  gt()

## the last 5 characters
Hair %>% 
  tail(5) %>% 
  regulartable() %>% 
  theme_box()

##Checking the data types
str(Hair)
## Checking the dimension of the dataset
dim(Hair)

## Checking for duplicates
Hair %>% 
  filter(duplicated(.)) %>% 
  view()   ### No duplicates are available in the data set


## EDA (Exploratory data analysis)
summary(Hair)


## Checking the number of Outliers in the data set
diagnose_outlier(Hair) %>% 
  flextable()



unique(Hair$Genetics)
unique(Hair$Hormonal.Changes)
unique(Hair$Medical.Conditions)
unique(Hair$Medications...Treatments)
unique(Hair$Nutritional.Deficiencies)
unique(Hair$Stress)
unique(Hair$Poor.Hair.Care.Habits)
unique(Hair$Environmental.Factors)
unique(Hair$Smoking)
unique(Hair$Weight.Loss)



#1. Age

summary(Hair$Age)

quantile(Hair$Age, c(0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,0.95))

##Plotting a Histogram of Age
Hair %>% 
  ggplot(aes(Age))+
  geom_histogram(bins = 20, fill = "steelblue")+
  geom_vline(aes(xintercept = mean(Age)), size = 1.0, colour = "red",linetype = "dashed")+
  annotate("text",x = 34.0, y = 80,
           label = paste("Mean \n ", round(mean(Hair$Age),2)),
           color = "darkred",
           size = 5)+
  annotate("text",x = 60.0, y = 80,
           label = paste("Skewness \n ", round(skewness(Hair$Age),3)),
           color = "navyblue",
           size = 3)+
  annotate("text",x = 60.0, y = 65,
           label = paste("Kurtosis \n ", round(kurtosis(Hair$Age),3)),
           color = "red",
           size = 3)+
  scale_size(range = c(12,12))+
  labs(title = "Histogram Of Age",
       x = "Age",
       y = NULL)

# checking for normality across the Age variable
Hair %>% 
  select(Age) %>% 
  normality() %>% 
  mutate(across(is.numeric, ~round(., 3))) %>% 
  regulartable()

# 2. Genetics
unique(Hair$Genetics)

Hair %>% 
  plot_frq(Genetics)+
  labs(title = "Bar plot of Genetics",
       x = "Genetics")
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))


# 3. Hormonal changes
Hair %>% 
  plot_frq(Hormonal.Changes)+
  labs(title = "Bar plot of Hormonal Changes",
       x = "Hormonal Changes")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

## 4. Medical Treatments
Hair %>% 
  plot_frq(Medications...Treatments)+
  labs(title = "Bar plot of Medications\nTreatments",
       x = "Medications Treatments")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

## 5. Nutritional deficiencies
Hair %>% 
  plot_frq(Nutritional.Deficiencies)+
  labs(title = "Bar plot of Nutritional\nDeficiencies",
       x = "Nutritional Deficiencies")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

## 6.Stress
Hair %>% 
  plot_frq(Stress)+
  labs(title = "Bar plot of Stress",
       x = "Stress")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

## 7. Medical conditions
Hair %>% 
  plot_frq(Medical.Conditions)+
  labs(title = "Medical Conditions",
       x = "Medical Conditions")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

## 8. Poor.Hair.Care.Habits
Hair %>% 
  plot_frq(Poor.Hair.Care.Habits)+
  labs(title = "Bar plot of Poor\n Hair Care Habits",
       x = "Poor HairCare Habits")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

## 9. Environmental Factors
Hair %>% 
  plot_frq(Environmental.Factors)+
  labs(title = " Bar plot of Environmental\n Factors",
       x = "Environmental Factors")+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

## 10. Smoking
Hair %>% 
  plot_frq(Smoking)+
  labs(title = " Bar plot of Smoking",
       x = "Smoking")

## 11. Weight loss
Hair %>% 
  plot_frq(Weight.Loss)+
  labs(title = " Bar plot of Weight Loss",
       x = "Weight Loss")


# 12. Hair Loss
summary(Hair$Hair.Loss)
quantile(Hair$Hair.Loss, c(0.1,0.2,0.3,.4,.5,.6,.7,.8,.9,.95))
Hair %>%
  mutate(Hair.Loss = factor(Hair.Loss,
                          levels = c(0 ,1),
                          labels = c("No",
                                     "Yes"))) %>% 
  
  ggplot(aes(Hair.Loss))+
  geom_bar(aes(fill = Hair.Loss), show.legend = F,alpha = .5)+
  labs(title = "Bar Plot of Hair Loss",
       x = "Hair Loss",
       y = "Counts")




#### DATA CLEANING
## Handling missing data

##checking for the percentage of missing data
p<- function(Hair) {sum(is.na(Hair))/length(Hair)*100}
apply(Hair,2,p)

## there are no missing values as per the above function in our data yet 
# the missing values have been coded as "no data"

# dealing with missing values and coding them to NA
Hair$Medications...Treatments[Hair$Medications...Treatments == "No Data"]<-NA
Hair$Medical.Conditions[Hair$Medical.Conditions == "No Data"]<-NA
Hair$Nutritional.Deficiencies[Hair$Nutritional.Deficiencies == "No Data"]<-NA

## Finding the visual format of the missing data
plot_na_pareto(Hair, only_na = T)


###  Changing to factors
i = 1
for (i in 4:6) {
  Hair[,i]<- as.factor(as.character(Hair[,i]))
}

## Imputation by polytomous regression
Hair_data<- mice(Hair, method = "polyreg", m =10, seed = 300)
Hairr<- complete(Hair_data,3)
Hairr$Hair.Loss<- as.factor(Hairr$Hair.Loss)
str(Hairr)

## Visualizing the data (Imputed data)
ggplot()+
  geom_bar(data = Hair, aes(Medical.Conditions), width = 0.3)+
  geom_bar(data = Hairr, aes(Medical.Conditions), fill = "red",
           position = position_nudge(x = 0.25), width = 0.3)+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))

ggplot()+
  geom_bar(data = Hair, aes(Medications...Treatments), width = 0.3)+
  geom_bar(data = Hairr, aes(Medications...Treatments), fill = "red",
           position = position_nudge(x = 0.25), width = 0.3)+
  theme(axis.text.x = element_text(angle = 65, vjust = 0.6))


Hairr<-Hairr %>% 
  select(-Id) %>% 
  mutate(Hair.Loss= case_when(Hair.Loss == "0" ~ "No",
                                                   TRUE ~ "Yes")) %>%
  mutate_all(as.factor) %>% 
  mutate(Age = as.numeric(Age))
str(Hairr)

## Doing encoding
#Hairr<-Hairr %>% 
#  select(-Id) %>% 
 # mutate(Genetics= case_when(Genetics == "Yes" ~ "0",
  #                      TRUE ~ "1")) %>% 
  #mutate(Hormonal.Changes  = case_when(Hormonal.Changes  == "Yes" ~ "0",
   #                          TRUE ~ "2")) %>%
  #mutate(Medical.Conditions = case_when(Medical.Conditions == "Ringworm"~ "0",
   #                                     Medical.Conditions == "Scalp Infection"~ "1",
    #                                    Medical.Conditions == "Thyroid Problems"~ "2",
     #                                   Medical.Conditions == "Eczema"~ "3",
      #                                  Medical.Conditions == "Psoriasis"~ "4",
       #                                 Medical.Conditions == "Seborrheic Dermatitis"~ "5",
        #                                Medical.Conditions == "Androgenetic Alopecia"~ "6",
         #                               Medical.Conditions == "Dermatosis"~ "7",
          #                              Medical.Conditions == "Alopecia Areata "~ "8",
           #                    TRUE ~ "9")) %>%
  #mutate(Medications...Treatments = case_when(Medications...Treatments == "Accutane"~ "0",
   #                                           Medications...Treatments == "Rogaine"~ "1",
    #                                          Medications...Treatments == "Antidepressants "~ "2",
     #                                         Medications...Treatments == "Antibiotics"~ "3",
      #                                        Medications...Treatments == "Chemotherapy"~ "4",
       #                                       Medications...Treatments == "Blood Pressure Medication"~ "5",
        #                                      Medications...Treatments == "Heart Medication "~ "6",
         #                                     Medications...Treatments == "Antifungal Cream"~ "7",
          #                                    Medications...Treatments == "Steroids"~ "8",
           #                  TRUE ~ "9")) %>% 
  #mutate(Nutritional.Deficiencies = case_when(Nutritional.Deficiencies == "Magnesium deficiency"~ "0",
   #                                           Nutritional.Deficiencies == "Selenium deficiency"~ "1",
    #                                          Nutritional.Deficiencies == "Selenium deficiency"~ "2",
     #                                         Nutritional.Deficiencies == "Protein deficiency"~ "3",
      #                                        Nutritional.Deficiencies == "Omega-3 fatty acids"~ "4",
       #                                       Nutritional.Deficiencies == "Biotin Deficiency"~ "5",
        #                                      Nutritional.Deficiencies == "Zinc Deficiency"~ "6",
         #                                     Nutritional.Deficiencies == "Vitamin E deficiency"~ "7",
          #                                    Nutritional.Deficiencies == "Iron deficiency"~ "8",
           #               TRUE ~ "9")) %>% 
  #mutate(Stress = case_when(Stress == "Moderate"~ "0",
   #                         Stress == "High"~ "1",
    #                       TRUE ~ "2")) %>%
  #mutate(Poor.Hair.Care.Habits = case_when(Poor.Hair.Care.Habits == "No"~ "0",
   #                              TRUE ~ "1")) %>%
  #mutate(Environmental.Factors = case_when(Environmental.Factors == "No"~ "0",
   #                           TRUE ~ "1")) %>%
  #mutate(Smoking = case_when(Smoking == "No"~ "0",
   #                    TRUE ~ "1")) %>% 
  #mutate(Weight.Loss = case_when(Weight.Loss == "No"~ "0",
   ##                          TRUE ~ "1")) 
#

## changing the factor variables to numeric so as to perfom correlation
#Hairr$Genetics<- as.numeric(Hairr$Genetics)
#Hairr$Hormonal.Changes<-as.numeric(Hairr$Hormonal.Changes)
#Hairr$Medical.Conditions<-as.numeric(Hairr$Medical.Conditions)
#Hairr$Medications...Treatments<-as.numeric(Hairr$Medications...Treatments)
#Hairr$Nutritional.Deficiencies<-as.numeric(Hairr$Nutritional.Deficiencies)
#Hairr$Stress<-as.numeric(Hairr$Stress)
#Hairr$Poor.Hair.Care.Habits<-as.numeric(Hairr$Poor.Hair.Care.Habits)
#Hairr$Environmental.Factors<-as.numeric(Hairr$Environmental.Factors)
#Hairr$Smoking<-as.numeric(Hairr$Smoking)
#Hairr$Weight.Loss<-as.numeric(Hairr$Weight.Loss)
#Hairr$Hair.Loss<-as.numeric(Hairr$Hair.Loss)
str(Hairr)

unique(Hairr$Medical.Conditions) %>% 
  table()

## set the reproducitibility
set.seed(1234)

## splitting into training and testing
tidy_split<- initial_split(Hairr, prop = 0.70,strata = Hair.Loss)
tidy_train<- training(tidy_split)
tidy_test<- testing(tidy_split)


# create folds for cross validation
tidy_folds<- vfold_cv(tidy_train, strata = Hair.Loss)

## create recipe
tidy_rec<-
  recipe(Hair.Loss ~ Genetics+Medical.Conditions+Nutritional.Deficiencies+Age+Poor.Hair.Care.Habits+Smoking, data = tidy_train) %>% 
  step_YeoJohnson(all_numeric_predictors()) %>% # correcting for skewness
  step_dummy(all_nominal(), -all_outcomes(), one_hot = TRUE) %>% #
  step_normalize(all_numeric_predictors()) # normalize predictors


tidy_boosted_model <- boost_tree(trees = tune(),
                                 min_n = tune(),
                                 learn_rate = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("xgboost")


tidy_knn_model <- nearest_neighbor(neighbors = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kknn")

boosted_grid<-grid_regular(parameters(tidy_boosted_model),levels = 5)
knn_grid<- grid_regular(parameters(tidy_knn_model),levels = 10)

bosted_tune <- tune_grid(tidy_boosted_model,
                         tidy_rec,
                         resamples = tidy_folds,
                         grid = boosted_grid)

knn_tune <- tune_grid(tidy_knn_model,
                      tidy_rec,
                      resamples = tidy_folds,
                      grid = knn_grid)



boosted_param<- bosted_tune %>% select_best(metric = "roc_auc")

knn_param<- knn_tune %>%  select_best(metric = "roc_auc")

tidy_boosted_model <- finalize_model(tidy_boosted_model, boosted_param)
tidy_knn_model<- finalize_model(tidy_knn_model, knn_param)

boosted_wf<- workflow() %>% 
  add_model(tidy_boosted_model) %>% 
  add_recipe(tidy_rec)

knn_wf<- workflow() %>% 
  add_model(tidy_knn_model) %>% 
  add_recipe(tidy_rec)


boosted_results<- last_fit(boosted_wf, tidy_split)
knn_res<- last_fit(knn_wf,tidy_split)
##Confusion matrix
boosted_results %>% 
  unnest(.predictions) %>% 
  conf_mat(truth = Hair.Loss, estimate = .pred_class) %>% 
  pluck("table") %>% 
  reshape2::melt() %>% 
  ggplot(aes(Truth, Prediction, fill = value)) +
  geom_tile(color = "black", size = 1) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) + 
  geom_text(aes(label = value)) + 
  scale_fill_gradient(low = "white", high = "blue") + 
  scale_y_discrete(limits = rev) +
  labs(
    title = "Prostate Cancer Confusion Matrix"
  )

unique(Hairr$Nutritional.Deficiencies)

knn_res %>% 
  unnest(.predictions) %>% 
  conf_mat(truth = Hair.Loss, estimate = .pred_class) %>% 
  pluck("table") %>% 
  reshape2::melt() %>% 
  ggplot(aes(Truth, Prediction, fill = value)) +
  geom_tile(color = "black", size = 1) +
  theme(
    panel.grid = element_blank(),
    panel.border = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  ) + 
  geom_text(aes(label = value)) + 
  scale_fill_gradient(low = "white", high = "blue") + 
  scale_y_discrete(limits = rev) +
  labs(
    title = "Prostate Cancer Confusion Matrix"
  )

## Seeing the roc and the accuracy

boosted_results %>% 
  unnest(.metrics) %>% 
  select(.metric, .estimate)

knn_res %>% 
  unnest(.metrics) %>% 
  select(.metric, .estimate)


final_boosted_model<- fit(boosted_wf, Hairr)

final_boosted_model<- fit(knn_wf, Hairr)
saveRDS(final_boosted_model,"hair_mode.rds")



rsconnect::setAccountInfo(name='hair-falls', token='102C2754E76FC2AC73C9667443C139BB', secret='C7bp2UfRa7aSRN4QfaknaGdWMQE65I+PzyV0BuRt')
library(rsconnect)
rsconnect::deployApp('C:/Users/user/Documents/Hair fall Prediction/HairApp/app')







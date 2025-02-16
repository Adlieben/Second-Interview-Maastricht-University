##### 0. DATA AND LIBRARIES ----
# Libraries
packages <- c("openxlsx", "tidyverse", "caret", "pROC", 
              "yardstick", "predtools", "dcurves", "gtsummary", "MASS")
install.packages(setdiff(packages, rownames(installed.packages())), 
                 dependencies = TRUE)
lapply(packages, library, character.only = TRUE)

# Load data
data <- read.xlsx("Data/covidpredict-1.xlsx") # Change path if necessary

#### 0.1 INVESTIGATE DATA ----
glimpse(data)
data %>% tbl_summary(type = all_dichotomous() ~ "categorical")

#### 1. FIT A LOGISTIC REGRESSION MODEL ----
# Separate training and test set
train <- data %>% filter(set == "dev")
test <- data %>% filter(set != "dev")

# Fit the model
model <- glm(mortality ~ age + female + comorbidity +
               rr + oxygen_sat + gcs + urea + crp, # Variables as requested
             data = train,
             family = binomial)

# Quickly check model
summary(model)
# Initial assessment suggests that gender is not significant in the model
# This however is not requested in the assignment

#### 2. CHECK MODEL FUNCTIONALITY ----
# Create predicted values for the test dataset created earlier
# Type = "response" specifies we want probabilities
test$predicted <- predict(model, test, type = "response")

# Create a confusion matrix for sensitivity, specificity, PPV and NPV
conf_matrix <- confusionMatrix(factor(round(test$predicted)), 
                               factor(test$mortality), positive = "1")
print(conf_matrix)
# Sensitivity is low, 53% indicates that nearly half of deaths go undetected
# High specificity so most patients not at risk are correctly identified as such
# PPV is decently high indicating most patients identified as deaths do actually die (at least in 2/3 of cases)
# NPV is decently high indicating predicted survival is often also correctly identified (also at least in 2/3 of cases)
# Accuracy is also acceptable, although we would like it to be higher

# Compute AUC
roc_obj <- roc(test$mortality, test$predicted)
auc_value <- auc(roc_obj)
print(auc_value)
# AUC is acceptable but not great, a lot of false positives are still returned

# Create calibration plot
calibration_plot(data = test, obs = "mortality", pred = "predicted", 
                 y_lim = c(0, 1), x_lim=c(0, 1),
                 title = "Calibration plot for validation data")
# The plot indicates that mortality is neither over- or underestimated
# So the predictions are calibrated

#### 3. DECISION CURVE ANALYSIS ----
dca_an <- dca(mortality ~ predicted, data = test)
plot(dca_an, smooth = TRUE) # smooth = TRUE added to remove statistical noise

# The curve for the model is consistently above the two others
# This means the model is useful in making a decision on whether to send someone to the ICU or not

# Using a risk threshold of 10% as given in the next question, we can also compare both approaches
dca_an %>%
  as_tibble() %>%
  filter(threshold %in% seq(0.05, 0.15, by = 0.05)) %>%
  dplyr::select(variable, threshold, net_benefit) %>%
  pivot_wider(id_cols = threshold, 
              names_from = variable,
              values_from = net_benefit)
# Net Benefit is 0.003 at 10% so the model is still better to use
NB <- 0.003 # observed above
z <- 0.1 #Risk threshold, given as 10%

# Interpretation per 1000 patients
NB*1000 # gain in true positives
((1-z)/z)*NB*1000 # reduction in false positives

# Interpretation on 1 870 640 patients (real reported incidence in 2021,found on the WHO website)
NB*1870640 # gain in true positives
((1-z)/z)*NB*1870640 # reduction in false positives

#### 4. VALUE-OF-INFORMATION ----
# The steps presented here are given in Sadatsafavi et al., 2023
n <- dim(test)[1]

#Step 1: store predicted probabilities.
pi <- test$predicted

#Step 2: The bootstrap Monte Carlo simulation
set.seed(123) # for reproducibility
N <- 10000
NBmodel <- NBall <- NBmax <- rep(0,N)
for(i in 1:N) {
  bsdata <-  test[sample(1:n, n, replace = T),]
  NBall[i] <- mean(bsdata$mortality-(1-bsdata$mortality)*z/(1-z))
  NBmodel[i] <- mean((bsdata$predicted>z)*(bsdata$mortality-(1-bsdata$mortality)*z/(1-z))) #NB of using the model
}

#Step 3: EVPI calculation
EVPI <- mean(pmax(0,NBmodel,NBall))-max(0,mean(NBmodel),mean(NBall))
EVPI # 1.131485e-05
save.image("analysis.RData") # Save for future use (avoids running MCMC several times)

# Interpretation per 1000 patients
EVPI*1000 # gain in true positives
((1-z)/z)*EVPI*1000 # reduction in false positives

# Interpretation on 1 870 640 patients (real reported incidence in 2021)
EVPI*1870640 # gain in true positives
((1-z)/z)*EVPI*1870640 # reduction in false positives

#### 5. EXPLANATION ----
# The high gain in true positives and reduction in false positives observed during the decision curve
# analysis suggest that the model should be used to support decision-making in Dutch hospitals.
# The EVPI identified during step 5 however indicates that there is little benefit to further external 
# validation as the gain in true positives and reduction in false positives is negligible in comparison
# with the size of the population at risk.

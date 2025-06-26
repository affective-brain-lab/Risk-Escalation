## General Information about this Code
# The code is divided into 3 sections:
# 1. Main experiment data processing and analysis
# 2. Validation experiment analysis
# 3. Control experiment analysis
# Distance is measured in meters.
# MaxRisk = 1 indicates trial occurs before reaching max risk; MaxRisk = 0 indicates after.

# Load necessary libraries
library(dplyr)
library(lme4)
library(ggplot2)
library(readxl)
library(readr)
library(QuantPsyc)
library(effectsize)
library(lm.beta)
library(afex)

# Set working directory
setwd("C:/Users/")



##############################Section 1 : Main Experiment ##########################

########################  Preprocessing ############################################

#ALL SUBJECTS 
Data <- read_excel("Risk Data.xlsx", sheet = 1)
Questions <- read_excel("Risk Data.xlsx", sheet = 2)



########First, calcuate number of reps till edge and slopes 

# Filter rows where MaxRisk is not 0
df_filtered <- Data %>% filter(MaxRisk != 0)

# Count the number of rows for each Subject
subject_counts <- df_filtered %>%
  group_by(Subject) %>%
  summarize(RepsTillEdge = n())

df_filtered <- df_filtered %>%
  mutate(Z_Distance= (Distance - mean(Distance)) / sd(Distance))


# Function to calculate slope for Distance with error handling
calculate_distance_slope <- function(data) {
  # Check if there are enough non-NA values to fit a model
  if (nrow(data) > 1 && sum(!is.na(data$Distance)) > 1) {
    model_distance <- lm(Distance ~ Trial, data = data)
    slope_distance <- coef(model_distance)[2] # Extract the slope
  } else {
    slope_distance <- NA # Set slope as NA if insufficient data
  }
  return(slope_distance)
}



# Apply slope calculation with error handling for each subject
distance_slopes <- df_filtered %>%
  group_by(Subject) %>%
  summarize(DistanceSlope = calculate_distance_slope(cur_data()))


# Create FinalData DataFrame with initial columns
FinalData <- data.frame(
  Subject = subject_counts$Subject,
  RepsTillEdge = subject_counts$RepsTillEdge
)


FinalData <- FinalData %>%
  mutate(Z_RepsTillEdge= (RepsTillEdge - mean(RepsTillEdge)) / sd(RepsTillEdge))



# Add DistanceSlope
FinalData <- FinalData %>%
  left_join(distance_slopes, by = "Subject")





##########################EmotionSlope

# 3. Calculate slopes of Anxiety and Excitement (excluding Trial 1 (Baseline Emotions))
df_no_trial1 <- Data %>% filter(Trial != 1)


df_no_trial1 <- df_no_trial1 %>%
  mutate(Z_Anxiety= (Anxiety - mean(Anxiety)) / sd(Anxiety))

df_no_trial1 <- df_no_trial1 %>%
  mutate(Z_Excitement= (Excitement - mean(Excitement)) / sd(Excitement))

df_no_trial1 <- df_no_trial1 %>%
  mutate(Z_Distance= (Distance - mean(Distance)) / sd(Distance))



calculate_slope <- function(data, variable) {
  # Check if the variable is constant (no change)
  if (length(unique(data[[variable]])) == 1) {
    return(NA)  # Return NA if no variability
  } else {
    model <- lm(as.formula(paste(variable, "~ Trial")), data = data)
    slope <- coef(model)[2]
    return(slope)
  }
}


# 3. Calculate AnxietySlope and ExcitementSlope for each subject
subject_slopes <- df_no_trial1 %>%
  group_by(Subject) %>%
  summarise(
    AnxietySlope = calculate_slope(cur_data(), "Anxiety"),
    ExcitementSlope = calculate_slope(cur_data(), "Excitement")
  )

# 4. Merge the slopes into FinalData
FinalData <- FinalData %>%
  left_join(subject_slopes, by = "Subject")


##formain 
# Merge with questionnaire data by Subject without mutating Sex
FinalData <- FinalData %>%
  left_join(Questions, by = "Subject")



# 7. Calculate averages and add to FinalData
average_distance <- Data %>%
  group_by(Subject) %>%
  summarize(AverageDistance = mean(Distance, na.rm = TRUE))
FinalData <- FinalData %>%
  left_join(average_distance, by = "Subject") %>%
  
  library(dplyr)


# Extract the first trial's Anxiety and Excitement scores (Trial 1) for each subject
trial1_scores <- Data %>%
  filter(Trial == 1) %>%
  dplyr::select(Subject, Anxiety, Excitement) %>%
  rename(AnxietyTrial1 = Anxiety, ExcitementTrial1 = Excitement)



FinalData <- FinalData %>%
  left_join(trial1_scores, by = "Subject")



# Add Z-scores to FinalData
FinalData <- FinalData %>%
  mutate(
    Z_DistanceSlope = (DistanceSlope - mean(DistanceSlope, na.rm = TRUE)) / sd(DistanceSlope, na.rm = TRUE),
    Z_AnxietySlope = (AnxietySlope - mean(AnxietySlope, na.rm = TRUE)) / sd(AnxietySlope, na.rm = TRUE),
    Z_ExcitementSlope = (ExcitementSlope - mean(ExcitementSlope, na.rm = TRUE)) / sd(ExcitementSlope, na.rm = TRUE),
  )

FinalData <- FinalData %>%
  mutate(Z_ExcitementTrial1= (ExcitementTrial1 - mean(ExcitementTrial1)) / sd(ExcitementTrial1))

FinalData <- FinalData %>%
  mutate(Z_AnxietyTrial1= (AnxietyTrial1 - mean(AnxietyTrial1)) / sd(AnxietyTrial1))

FinalData <- FinalData %>%
  mutate(Z_Trait_Anxiety_Score  = (Trait_Anxiety_Score   - mean(Trait_Anxiety_Score  )) / sd(Trait_Anxiety_Score  ))



#write.csv(FinalData, "FinalData.csv", row.names = FALSE)

# calcuate CI for linear models: 
confint(model, level = 0.95)
 

########################  Analyses - Main Experiment ###########################################

# Does Risk Escalate?
Distance_Model <- afex::mixed(Z_Distance ~ Trial + (1 + Trial | Subject), 
                              data = df_filtered, 
                              method = "KR")
summary(Distance_Model)

# Does Anxiety Habituate?
Anxiety_Model <- afex::mixed(Z_Anxiety ~ Trial + (1 + Trial | Subject), 
                             data = df_no_trial1, 
                             method = "KR")
summary(Anxiety_Model)

# Does Excitement Habituate?
Excitement_Model <- afex::mixed(Z_Excitement ~ Trial + (1 + Trial | Subject), 
                                data = df_no_trial1, 
                                method = "KR")
summary(Excitement_Model)

# Is Anxiety Habituation associated with Risk Escalation?
model_1 <- lm(Z_DistanceSlope ~ Z_AnxietySlope, data = FinalData)
summary(model_1)

# Is Excitement Habituation associated with Risk Escalation?
model_2 <- lm(Z_DistanceSlope ~ Z_ExcitementSlope, data = FinalData)
summary(model_2)

#Entering both excitement and anxiety into a model to predict risk 
model_2.1 <- lm(Z_DistanceSlope ~ Z_AnxietySlope+ Z_ExcitementSlope, data = FinalData)
summary(model_2.1)

# Additional Analyses
# Predicting Risk Escalation with Baseline Anxiety and Anxiety Habituation
model_3 <- lm(Z_DistanceSlope ~ Z_AnxietyTrial1 + Z_AnxietySlope, data = FinalData)
summary(model_3)

# Predicting Risk Escalation with Baseline Excitement and Excitement Habituation
model_4 <- lm(Z_DistanceSlope ~ Z_ExcitementTrial1 + Z_ExcitementSlope, data = FinalData)
summary(model_4)

# Trait Anxiety and Risk Escalation
model_5 <- lm(Z_DistanceSlope ~ Z_Trait_Anxiety_Score , data = FinalData)
summary(model_5)


# Anxiety slope correlated with Excitement Slope? 
model_6 <- lm(Z_AnxietySlope ~ Z_ExcitementSlope, data = FinalData)
summary(model_6)



##############################Section 2 : Validation Experiment ##########################

ValidationData <- read_excel("Risk Data.xlsx", sheet = 3)


ValidationData <- ValidationData %>%
  mutate(Z_Distance= (Distance - mean(Distance)) / sd(Distance))
ValidationData <- ValidationData %>%
  mutate(Z_Feeling_of_Risk= (Feeling_of_Risk - mean(Feeling_of_Risk)) / sd(Feeling_of_Risk))



# Fit the mixed-effects model for DistanceM
FeelinfofRisk_Model <- afex::mixed(Z_Feeling_of_Risk ~ Z_Distance + Trial+ (1 + Z_Distance + Trial | Subject), 
                                   data = ValidationData, 
                                   method = "KR")

summary(FeelinfofRisk_Model)




##############################Section 3 : Control Experiment ##########################
Data <- read_excel("Risk Data.xlsx", sheet = 1)

ControlData <- read_excel("Risk Data.xlsx", sheet = 4)
Filtered_Main <- Data %>% filter(MaxRisk != 0)
# Control experiment data
Filtered_Control <- ControlData %>% filter(MaxRisk != 0)



# Main experiment data
subject_counts_main <- Filtered_Main %>%
  group_by(Subject) %>%
  summarize(`Number of Rows` = n()) %>%
  mutate(Condition = "main exp")  # Add the condition column


subject_counts_control <- Filtered_Control %>%
  group_by(Subject) %>%
  summarize(`Number of Rows` = n()) %>%
  mutate(Condition = "control exp")  # Add the condition column

# Combine both data frames with main experiment first
combined_subject_counts <- bind_rows(subject_counts_main, subject_counts_control)


# Perform Kolmogorov-Smirnov test
test_result <- ks.test(combined_subject_counts$`Number of Rows` ~ Condition, data = combined_subject_counts)

# Print the test result
print(test_result)



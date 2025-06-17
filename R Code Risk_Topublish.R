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
setwd("C:/Users/hadee/Dropbox/Hadeel_Share/UCL-Share/RiskTakingProject/ExperimentFiles&Analysis/Virtual Reality 1/FinalDataPublication")

############################## Section 1 : Main Experiment ##########################

# Load data
Data <- read_excel("Risk Data.xlsx", sheet = 1)
Questions <- read_excel("Risk Data.xlsx", sheet = 2)

# Filter trials before reaching max risk
df_filtered <- Data %>% filter(MaxRisk != 0)

# Calculate repetitions till edge
subject_counts <- df_filtered %>%
  group_by(Subject) %>%
  summarize(RepsTillEdge = n())

# Z-score normalize Distance
df_filtered <- df_filtered %>%
  mutate(Z_Distance = scale(Distance))

# Function to calculate slope for Distance per subject
calculate_distance_slope <- function(data) {
  if (nrow(data) > 1 && sum(!is.na(data$Distance)) > 1) {
    slope_distance <- coef(lm(Distance ~ Trial, data = data))[2]
  } else {
    slope_distance <- NA
  }
  return(slope_distance)
}

# Apply slope calculation
distance_slopes <- df_filtered %>%
  group_by(Subject) %>%
  summarize(DistanceSlope = calculate_distance_slope(cur_data()))

# Create FinalData and add Z_RepsTillEdge
FinalData <- data.frame(
  Subject = subject_counts$Subject,
  RepsTillEdge = subject_counts$RepsTillEdge
) %>%
  mutate(Z_RepsTillEdge = scale(RepsTillEdge)) %>%
  left_join(distance_slopes, by = "Subject")

########################## Emotional Slope Calculation ##########################

# Remove Trial 1 for slope calculation
df_no_trial1 <- Data %>% filter(Trial != 1) %>%
  mutate(
    Z_Anxiety = scale(Anxiety),
    Z_Excitement = scale(Excitement),
    Z_Distance = scale(Distance)
  )

# General slope function for a variable
calculate_slope <- function(data, variable) {
  if (length(unique(data[[variable]])) == 1) {
    return(NA)
  } else {
    slope <- coef(lm(as.formula(paste(variable, "~ Trial")), data = data))[2]
    return(slope)
  }
}

# Calculate emotional slopes
subject_slopes <- df_no_trial1 %>%
  group_by(Subject) %>%
  summarise(
    AnxietySlope = calculate_slope(cur_data(), "Anxiety"),
    ExcitementSlope = calculate_slope(cur_data(), "Excitement")
  )

# Merge with FinalData
FinalData <- FinalData %>%
  left_join(subject_slopes, by = "Subject") %>%
  left_join(Questions, by = "Subject")

# Add average distance
average_distance <- Data %>%
  group_by(Subject) %>%
  summarize(AverageDistance = mean(Distance, na.rm = TRUE))

FinalData <- FinalData %>%
  left_join(average_distance, by = "Subject")

# Add Trial 1 emotion scores
trial1_scores <- Data %>%
  filter(Trial == 1) %>%
  select(Subject, Anxiety, Excitement) %>%
  rename(AnxietyTrial1 = Anxiety, ExcitementTrial1 = Excitement)

FinalData <- FinalData %>%
  left_join(trial1_scores, by = "Subject")

# Add z-scored slope and trial 1 emotion values
FinalData <- FinalData %>%
  mutate(
    Z_DistanceSlope = scale(DistanceSlope),
    Z_AnxietySlope = scale(AnxietySlope),
    Z_ExcitementSlope = scale(ExcitementSlope),
    Z_ExcitementTrial1 = scale(ExcitementTrial1),
    Z_AnxietyTrial1 = scale(AnxietyTrial1)
  )

########################  Analyses - Main Experiment ##########################

# Mixed models for escalation and emotional habituation
Distance_Model <- afex::mixed(Z_Distance ~ Trial + (1 + Trial | Subject), data = df_filtered, method = "KR")
summary(Distance_Model)

Anxiety_Model <- afex::mixed(Z_Anxiety ~ Trial + (1 + Trial | Subject), data = df_no_trial1, method = "KR")
summary(Anxiety_Model)

Excitement_Model <- afex::mixed(Z_Excitement ~ Trial + (1 + Trial | Subject), data = df_no_trial1, method = "KR")
summary(Excitement_Model)

# Linear models for predicting risk escalation
model_1 <- lm(Z_DistanceSlope ~ Z_AnxietySlope, data = FinalData)
summary(model_1)

model_2 <- lm(Z_DistanceSlope ~ Z_ExcitementSlope, data = FinalData)
summary(model_2)

model_2.1 <- lm(Z_DistanceSlope ~ Z_AnxietySlope + Z_ExcitementSlope, data = FinalData)
summary(model_2.1)

model_3 <- lm(Z_DistanceSlope ~ AnxietyTrial1 + Z_AnxietySlope, data = FinalData)
summary(model_3)

model_4 <- lm(Z_DistanceSlope ~ ExcitementTrial1 + Z_ExcitementSlope, data = FinalData)
summary(model_4)

model_5 <- lm(Z_DistanceSlope ~ Trait_Anxiety_Score, data = FinalData)
summary(model_5)

model_6 <- lm(Z_AnxietySlope ~ Z_ExcitementSlope, data = FinalData)
summary(model_6)

############################## Section 2 : Validation Experiment ##########################

ValidationData <- read_excel("Risk Data.xlsx", sheet = 3) %>%
  mutate(
    Z_Distance = scale(Distance),
    Z_Feeling_of_Risk = scale(Feeling_of_Risk)
  )

FeelinfofRisk_Model <- afex::mixed(
  Z_Feeling_of_Risk ~ Z_Distance + Trial + (1 + Z_Distance + Trial | Subject),
  data = ValidationData,
  method = "KR"
)
summary(FeelinfofRisk_Model)

############################## Section 3 : Control Experiment ##########################

ControlData <- read_excel("Risk Data.xlsx", sheet = 4)
Filtered_Main <- Data %>% filter(MaxRisk != 0)
Filtered_Control <- ControlData %>% filter(MaxRisk != 0)

# Count number of rows per subject
subject_counts_main <- Filtered_Main %>%
  group_by(Subject) %>%
  summarize(`Number of Rows` = n()) %>%
  mutate(Condition = "main exp")

subject_counts_control <- Filtered_Control %>%
  group_by(Subject) %>%
  summarize(`Number of Rows` = n()) %>%
  mutate(Condition = "control exp")

# Combine and test distribution
combined_subject_counts <- bind_rows(subject_counts_main, subject_counts_control)

# Kolmogorov-Smirnov test
test_result <- ks.test(`Number of Rows` ~ Condition, data = combined_subject_counts)
print(test_result)

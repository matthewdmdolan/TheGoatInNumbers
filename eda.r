# installing packages when not available 
install_and_load_packages <- function(packages) {
  # Check if packages are installed
  missing_packages <- packages[!sapply(packages, requireNamespace, quietly = TRUE)]
  
  # Install missing packages
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  # Load packages
  for (pkg in packages) {
    library(pkg, character.only = TRUE)
  }
}

# installing packages
packages_to_check <- c("dplyr", "ggplot2", "tidyr", "plotly", "reader", "caret", "pROC", )
install_and_load_packages(packages_to_check)


# reading in data 
folder_path <- "/Users/mattdolan/Downloads/archive (3)"
file_list <- list.files(folder_path, full.names = TRUE)

# Calling list to ensure files ingested properly
dataframes <- list()  
file_list <- list.files(folder_path, full.names = TRUE)

#looping through files and producing a df for each one
for (file in file_list) {
  if (endsWith(file, ".csv")) {  # Assuming the files are in CSV format
    file_name <- tools::file_path_sans_ext(basename(file))
    print(file_name)
    # Save dataframe with a variable name
    assign(paste0(file_name, "_df"), read.csv(file))
    head(file_name)
  }
}

# combining with gini table
events_data <- merge(events_df, ginf_df, by = "id_odsp", all.x = FALSE)
head(events_data)

# filtering events by messi 
lionel_messi_events <- events_data[grepl("lionel messi", events_data$player), ]
head(lionel_messi_events)

# Looking at total matches for  messi and plotting 
messi_matches_per_season <- lionel_messi_events %>%
  group_by(season) %>%
  summarise(matches = n_distinct(id_odsp))
p1 <- ggplot(data = messi_matches_per_season, aes(x = season, y = matches)) +
  geom_line(color = "#69b3a2") +
  geom_area(fill = "#69b3a2", alpha = 0.5) +
  labs(y = "No. of Matches", x = element_blank()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1),
        # rotate x-axis labels for better readability
        axis.title.y = element_text(size = 8), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "darkgrey", linetype = "dashed"))

ggplotly(p1)

# Filtering goals scored by Lionel Messi and plotting 
messi_goals_per_season <- lionel_messi_events %>%
  filter(is_goal == 1) %>%
  group_by(season) %>%
  summarise(goals = n())
p2 <- ggplot(data = messi_goals_per_season, aes(x = season, y = goals)) +
  geom_line(color = "#69b3a2") +
  geom_area(fill = "#69b3a2", alpha = 0.5) +
  labs(y = "No. of Goals", x=element_blank()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1),
        # rotate x-axis labels for better readability
        axis.title.y = element_text(size = 8), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "darkgrey", linetype = "dashed"))

ggplotly(p2)


# Calculate total number of matches Messi played per season
messi_matches_per_season <- lionel_messi_events %>%
  group_by(season) %>%
  # assuming id_odsp is unique for each match
  summarise(matches = n_distinct(id_odsp)) 

# Calculate total number of goals Messi scored per season
messi_goals_per_season <- lionel_messi_events %>%
  filter(is_goal == 1) %>%
  group_by(season) %>%
  summarise(goals = n())

# Merge the two data frames and calculate goals per game
messi_stats_per_season <- inner_join(messi_matches_per_season, messi_goals_per_season, by = "season") %>%
  mutate(goals_per_game = goals / matches)

# This plot looks at goals per game over the analysis period
p3 <- ggplot(data = messi_stats_per_season, aes(x = season, y = goals_per_game)) +
  geom_line(color = "#69b3a2") +
  geom_area(fill = "#69b3a2", alpha = 0.5) +
  labs(y = "Goals per Game", x=element_blank()) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1),
        axis.title.y = element_text(size = 8), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "darkgrey", linetype = "dashed"))

ggplotly(p3)


# filtering data to only show goals by messi and location of them
messi_goals_by_location <- lionel_messi_events %>%
  filter(is_goal == 1, player == "lionel messi") %>%
  group_by(location, season) %>%
  # Count unique matches with goals
  summarise(matches_with_goal = n_distinct(link_odsp)) 

# Adding location column from dictionary file for better labelling
messi_goals_by_location <- messi_goals_by_location %>%
  mutate(location_string = case_when(
    location == 1 ~ "Attacking half",
    location == 2 ~ "Defensive half",
    location == 3 ~ "Centre of the box",
    location == 4 ~ "Left wing",
    location == 5 ~ "Right wing",
    location == 6 ~ "Difficult angle and long range",
    location == 7 ~ "Difficult angle on the left",
    location == 8 ~ "Difficult angle on the right",
    location == 9 ~ "Left side of the box",
    location == 10 ~ "Left side of the six yard box",
    location == 11 ~ "Right side of the box",
    location == 12 ~ "Right side of the six yard box",
    location == 13 ~ "Very close range",
    location == 14 ~ "Penalty spot",
    location == 15 ~ "Outside the box",
    location == 16 ~ "Long range",
    location == 17 ~ "More than 35 yards",
    location == 18 ~ "More than 40 yards",
    location == 19 ~ "Not recorded",
    TRUE ~ "Unknown" # This is for safety if any other unexpected value comes
  ))

# Plotting data to show location of goals by messi and location of them 
messi_goals_by_location <- messi_goals_by_location %>% 
  mutate(location_string = reorder(location_string, -matches_with_goal)) 

p4 <- ggplot(data = messi_goals_by_location, aes(x = reorder(location_string, matches_with_goal), y = matches_with_goal)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(x=element_blank(), y = "No. of Goals") +
  # Flip coordinates for better readability with many categories
  coord_flip() +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1),
        # rotate x-axis labels for better readability
        axis.title.y = element_text(size = 8), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "darkgrey", linetype = "dashed"))

p4

# Calculate the number of goals scored by Messi against each opponent
messi_goals_by_opponent <- lionel_messi_events %>%
  filter(is_goal == 1, player == "lionel messi") %>%
  group_by(opponent) %>%
  summarise(goals = n()) %>%
  arrange(desc(goals))  # Sort by descending order of goals

head(messi_goals_by_opponent)

# Bar plot of goals scored against each opponent
p5 <- ggplot(data = messi_goals_by_opponent, aes(x = reorder(opponent, goals), y = goals)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(x=element_blank(), y = "Goals") +
  coord_flip() +  
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 40, hjust = 1),
        axis.title.y = element_text(size = 8), 
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = "darkgrey", linetype = "dashed"))

p5

# Goals from assists
messi_assists <- events_data %>%
  filter(events_data$assist_method != "None", 
         events_data$is_goal == 1,  # Assuming 'is_goal' and 'player2' are columns in lionel_messi_events
         events_data$player2 == "lionel messi") %>%
  group_by(player) %>%
  summarise(goalsfrommessiassist = n()) %>%
  arrange(desc(goalsfrommessiassist))  # Sort by descending order of attempts

# Teammates assisted the most 
p7 <- ggplot(data = messi_assists, aes(x = reorder(player, goalsfrommessiassist), y = goalsfrommessiassist)) +
  geom_bar(stat = "identity", fill = "#69b3a2") +
  labs(title = "Goals Assisted by Messi", x = "Player", y = "Goals") +
  coord_flip() +  # Flip coordinates for better readability with many categories
  theme_minimal()

p7

# Machine Learning Analysis 
# Filtered the DataFrame events_data into the DataFrame shots to include only records about attempts made at goal. The shots DataFrame will be used to train the Logistic Regression model.
shots <- events_data %>%
filter(events_data$event_type==1)
head(shots)

dim(shots)

shot_predictions <- shots[, c(15, 17, 18, 19, 20, 21, 22)]
head(shot_predictions)
dummy_vars <- dummyVars(~., data = shot_predictions, fullRank = TRUE)
head(dummy_vars)
shot_predictions <- data.frame(predict(dummy_vars, newdata = shot_predictions))

# Target and data columns
Y = shot_predictions$is_goal
head(Y)
X = shot_predictions[, -2]
head(X)

# Assuming Y is the dependent variable
partition <- createDataPartition(Y, p = 0.8, list = FALSE)

X_train <- X[partition,]
Y_train <- Y[partition]

X_test <- X[-partition,]
Y_test <- Y[-partition]

# Training the model
xgmodel <- glm(Y_train ~ ., data = X_train, family = binomial(link = 'logit'))

# Make predictions
Y_pred <- predict(xgmodel, newdata = X_test, type = "response")

# The predictions are in terms of probabilities, to convert them to class labels you can use a threshold (e.g., 0.5)
Y_pred_label <- ifelse(Y_pred > 0.5, 1, 0)

# Predicted probabilities
Y_pred_prob <- predict(xgmodel, newdata = X_test, type = "response")

Y_pred_prob

# Load the necessary libraries
library(pROC)
library(caret)

# Accuracy
accuracy <- sum(diag(table(Y_test, Y_pred_label))) / sum(table(Y_test, Y_pred_label))
print(paste0("\nAccuracy: ", round(100 * accuracy, 2)))

# ROC-AUC score
roc_obj <- roc(Y_test, Y_pred)
roc_auc <- auc(roc_obj)
print(paste0("\nROC-AUC Score: ", round(roc_auc, 4)))
roc_obj
roc_auc


# Assuming 'roc_obj' is the ROC object, let's extract the data:
roc_data <- data.frame(
    FPR = rev(roc_obj$specificities),  # False Positive Rate
    TPR = rev(roc_obj$sensitivities)  # True Positive Rate
)

head(roc_data)


# Now we can use ggplot2 to create the ROC curve:
p8 <- ggplot(roc_data, aes(x = FPR, y = TPR)) +
  geom_line() +
  labs(x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)", 
       title = "ROC Curve") +
  coord_equal() +  # This ensures the aspect ratio is 1:1
  theme_minimal()

p8

# Assuming Y_test is the actual labels and Y_pred are the predictions
# Convert Y_pred and Y_test to factors with the same levels
Y_pred <- factor(Y_pred, levels = c(0, 1))
Y_test <- factor(Y_test, levels = c(0, 1))

# Calculate confusion matrix
CM <- confusionMatrix(Y_pred, Y_test)

# Print the confusion matrix
print(CM)

# Sensitivity/recall
sensitivity <- CM$byClass["Sensitivity"]

# Specificity
specificity <- CM$byClass["Specificity"]

# Precision or Positive Predictive Value
precision <- CM$byClass["Pos Pred Value"]

# Negative Predictive Value
npv <- CM$byClass["Neg Pred Value"]

# F1 Score
f1 <- 2 * ((precision * sensitivity) / (precision + sensitivity))

# Print classification report
cat("\nClassification Report: \n")
cat("Accuracy: ", accuracy, "\n")
cat("Sensitivity: ", sensitivity, "\n")
cat("Specificity: ", specificity, "\n")
cat("Precision: ", precision, "\n")
cat("Negative Predictive Value: ", npv, "\n")
cat("F1 Score: ", f1, "\n")

# Assuming xgmodel is a trained machine learning model
xgoals <- predict(xgmodel, X, type = "response")
goals <- X

# Assuming Y is the actual values
goals$actual <- Y

# The predict function in R does not return probabilities by default
# so you might have to use a different function or specify a type argument depending on the model
# Here's a generic way to get the first column
goals$expected <- xgoals
goals$difference <- goals$expected - goals$actual



# Filtering down to just goals
goals_df <- subset(events_data, is_goal == 1)
messi_goals_df <- subset(goals_df, player == "lionel messi")
lionel_messi_xgoals <- sum(messi_goals_df$expected)
lionel_messi_actual <- sum(messi_goals_df $actual)
diff <- lionel_messi_actual - lionel_messi_xgoals
cat("\nExpected: ", round(lionel_messi_xgoals, 4), "\n")
cat("Actual: ", lionel_messi_actual, "\n")
cat("Difference: ", round(diff, 4), "\n")


# Assuming 'roc_obj' is the ROC object, let's extract the data:
roc_data <- data.frame(
  FPR = rev(roc_obj$specificities),  # False Positive Rate
  TPR = rev(roc_obj$sensitivities)  # True Positive Rate
)

# Now we can use ggplot2 to create the ROC curve:
p9 <- ggplot(roc_data, aes(x = FPR, y = TPR)) +
  geom_line() +
  labs(x = "False Positive Rate (1 - Specificity)", 
       y = "True Positive Rate (Sensitivity)", 
       title = "ROC Curve") +
  coord_equal() +  # This ensures the aspect ratio is 1:1
  theme_minimal()

p9

# Assuming Y_test is the actual labels and Y_pred are the predictions
CM <- confusionMatrix(Y_pred, Y_test)

# Print the confusion matrix
print(CM)

# Sensitivity or Recall
sensitivity <- CM$byClass["Sensitivity"]

# Specificity
specificity <- CM$byClass["Specificity"]

# Precision or Positive Predictive Value
precision <- CM$byClass["Pos Pred Value"]

# Negative Predictive Value
npv <- CM$byClass["Neg Pred Value"]

# F1 Score
f1 <- 2 * ((precision * sensitivity) / (precision + sensitivity))

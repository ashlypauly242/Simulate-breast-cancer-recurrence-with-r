
install.packages(c("readxl","janitor","dplyr","caret","pROC","rpart","rpart.plot","ranger","ggplot2"))

library(readxl)
library(janitor)
library(dplyr)
library(caret)
library(pROC)
library(rpart)
library(rpart.plot)
library(ranger)
library(ggplot2)

set.seed(515)   # reproducibility
# 1. Data Preprocessing
 
df<-read_excel("C:/Users/Ashly/OneDrive/Desktop/s2/foundations of data science/Ass2/breast_cancer_dataset.xlsx")
str(df)
head(df)
# Returns TRUE if any missing values exist
df<-clean_names(df)
any(is.na(df))
# Shows how many NA values are in each column
colSums(is.na(df))
 #PREPROCESSING

# Pattern of month abbreviations
month_pattern <- "(jan|feb|mar|apr|may|jun|jul|aug|sep|oct|nov|dec)"

# Find which columns have month names
cols_with_dates <- sapply(df, function(x)
  any(grepl(month_pattern, tolower(as.character(x)), ignore.case = TRUE))
)

cols_with_dates


df[] <- lapply(df, function(x) {
  # work on text-like columns only
  if (is.character(x) || is.factor(x)) {
    x <- as.character(x)
    # Any cell containing a month name becomes NA
    x[grepl(month_pattern, tolower(x))] <- NA
    # (Optional) trim and lower for consistency
    x <- trimws(tolower(x))
    return(x)
  } else {
    return(x)
  }
})

#verify whether the month names are removed
cols_with_dates <- sapply(df, function(x)
  any(grepl(month_pattern, tolower(as.character(x)), ignore.case = TRUE))
)
cols_with_dates

print(colSums(is.na(df)))


# 6) Impute (fill) NAs with the column MODE (most frequent value)
#    We do this for character/factor columns except the target 'class'.
mode_impute <- function(v) {
  v <- as.character(v)
  non_na <- v[!is.na(v)]
  if (length(non_na) == 0) return(v)           # nothing to impute
  mode_val <- names(sort(table(non_na), decreasing = TRUE))[1]
  v[is.na(v)] <- mode_val
  v
}

cols_to_impute <- setdiff(names(df), "class")  # don't touch target unless you really need to
df[cols_to_impute] <- lapply(df[cols_to_impute], function(col) {
  if (is.character(col) || is.factor(col)) {
    col <- mode_impute(col)
    factor(col)  # keep as factor for modeling later
  } else {
    col  # leave numeric as-is (no numeric impute here)
  }
})

# (If class exists and you want it as factor explicitly)
if ("class" %in% names(df)) df$class <- as.factor(df$class)


print(colSums(is.na(df)))
summary(df)


#EDA





ggplot(df, aes(x = tumor_size, fill = class)) +
  geom_bar(position = "dodge") +
  labs(title = "Tumor Size by Outcome", x = "Tumor Size Range", y = "Count") +
  theme_minimal()



ggplot(df, aes(x = age, fill = class)) +
  geom_bar(position = "dodge") +
  labs(title = "Age Group vs. Recurrence Status",
       x = "Age Range",
       y = "Number of Patients",
       fill = "Recurrence") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df, aes(x = breast_quad, fill = class)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Tumor Occurrence by Breast Quadrant and Recurrence Status",
    x = "Breast Quadrant",
    y = "Number of Cases",
    fill = "Recurrence Status"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



age_count <- df %>%
  group_by(age) %>%
  summarize(
    total = n(),
    recurrence_count = sum(class == "recurrence-events", na.rm = TRUE)
  )

# Plot recurrence counts by age
ggplot(age_count, aes(x = age, y = recurrence_count, group = 1)) +
  geom_line(color = "steelblue", linewidth = 1.2) +
  geom_point(size = 3, color = "darkred") +
  labs(
    title = "Recurrence Count by Age Group",
    x = "Age Range",
    y = "Number of Recurrence Cases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(df, aes(x = tumor_size, fill = class)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = percent_format()) +
  labs(
    title = "Recurrence Proportion by Tumor Size",
    x = "Tumor Size Range",
    y = "Proportion",
    fill = "Outcome"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


df$class <- as.factor(df$class)

# Optional: scale numeric features (for logistic regression)
df_scaled <- df
df_scaled$deg_malig <- scale(df$deg_malig)


install.packages("caTools")
set.seed(123)
library(caTools)
split <- sample.split(df$class, SplitRatio = 0.7)
train <- subset(df, split == TRUE)
test  <- subset(df, split == FALSE)

model_log <- glm(class ~ ., data = train, family = binomial)
summary(model_log)
pred_log <- predict(model_log, test, type = "response")
pred_class <- ifelse(pred_log > 0.5, "recurrence-events", "no-recurrence-events")
#Decision Tree
library(rpart)
library(rpart.plot)

model_tree <- rpart(class ~ ., data = train, method = "class")
rpart.plot(model_tree)
pred_tree <- predict(model_tree, test, type = "class")
# random forest

library(randomForest)
model_rf <- randomForest(class ~ ., data = train, ntree = 100)
print(model_rf)

pred_rf <- predict(model_rf, test)


#model evaluation
library(caret)
confusionMatrix(as.factor(pred_class), test$class)  # logistic regression
confusionMatrix(pred_tree, test$class)               # decision tree
confusionMatrix(pred_rf, test$class)                 # random forest

library(caret)
ctrl <- trainControl(method = "cv", number = 5)
cv_model <- train(class ~ ., data = df, method = "glm", trControl = ctrl)
cv_model

# Load necessary libraries in R

install.packages("dplyr")
install.packages("skimr")
install.packages("randomForest")

library(dplyr)
library(ggplot2)
library(skimr)
library(lubridate)
library (grid)
library(tidyverse)
library(moments)
library(corrplot)
library(caret)
library(readr)
library(tidyr)
library(modelr)
library(stats)
library(reshape2)
library(irr)
library(pROC)
library(randomForest)


#Data Preprocessing

#Reading the data
df_customer = read.csv('olist_customers_dataset.csv')
df_geolocation = read.csv('olist_geolocation_dataset.csv')  # Replace with the correct file name
head(df_geolocation)
df_items = read.csv('olist_order_items_dataset.csv')
df_payments = read.csv('olist_order_payments_dataset.csv')
df_reviews = read.csv('olist_order_reviews_dataset.csv')
df_orders = read.csv('olist_orders_dataset.csv')
df_product = read.csv('olist_products_dataset.csv')
df_seller = read.csv('olist_sellers_dataset.csv')
df_category = read.csv('product_category_name_translation.csv')

#Joining the datasets
head(df_geolocation)
head(df_orders)
inner_join_result = inner_join(df_orders, df_payments, by = "order_id")
print(inner_join_result)
df_orderpay = inner_join_result # Joined Orders and Payments datset
nrow(df_orderpay)
ncol(df_orderpay)
orders_com = left_join(df_orderpay, df_items, by = 'order_id')
head(orders_com)
df_final1 = left_join(orders_com, df_product, by = 'product_id')
head(df_category)

df_final2 = left_join(df_final1 , df_category, by = 'product_category_name' )
df_final3 = left_join(df_final2, df_customer , by = 'customer_id')
df_final4 = left_join(df_final3 , df_seller, by = 'seller_id')
colnames(df_final4)

#Checking the missing values
missing_values = sapply(df_final4, function(x) sum(is.na(x)))
print(missing_values)


df_final = left_join(df_final4, df_geolocation, by = c('customer_zip_code_prefix'='geolocation_zip_code_prefix'))
head(df_final)

#Cleaning Dataset
colnames(df_final)

#Checking the missing values
missing_values = sapply(df_final, function(x) sum(is.na(x)))
print(missing_values)

rows_with_missing_values = df_final[rowSums(is.na(df_final)) > 0, ]
print(rows_with_missing_values)

#Dropping the rows with missing values
df_cleaned = na.omit(df_final)
print(summary(df_cleaned))


#Dealing with the missing value-
missing_values = colSums(is.na(df_cleaned))
print(missing_values)
#data frame "df_cleaned" has no missing values

# Convert relevant columns to Date type
# Convert relevant date columns to POSIXct format
df_cleaned$order_approved_at = as.POSIXct(df_cleaned$order_approved_at, format="%Y-%m-%d %H:%M:%S", tz="UTC")
df_cleaned$order_delivered_customer_date = as.POSIXct(df_cleaned$order_delivered_customer_date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
df_cleaned$order_estimated_delivery_date = as.POSIXct(df_cleaned$order_estimated_delivery_date, format="%Y-%m-%d %H:%M:%S", tz="UTC")
df_cleaned$order_purchase_timestamp = as.POSIXct(df_cleaned$order_purchase_timestamp, format="%Y-%m-%d %H:%M:%S", tz="UTC")

# Create new columns
df_cleaned = df_cleaned %>%
  mutate(purchased_approved = as.numeric(difftime(order_approved_at, order_purchase_timestamp, units = "secs")),
         approved_carrier = as.numeric(as.Date(order_delivered_carrier_date) - as.Date(order_approved_at)),
         carrier_delivered = as.numeric(as.Date(order_delivered_customer_date) - as.Date(order_delivered_carrier_date)),
         delivered_estimated = as.numeric(as.Date(order_estimated_delivery_date) - as.Date(order_delivered_customer_date)),
         purchased_delivered = as.numeric(as.Date(order_delivered_customer_date) - as.Date(order_purchase_timestamp)))
# New columns are created using the available datetime columns for easy analysis of the available data.
# Purchased_approved represents the seconds taken for an order to get approved after the customer purchases it.
# approved_carrier represents the days taken for the order to go to the delivery carrier after it being approved.
# carrier_delivered represents the days taken for the order to be delivered to the customer from the date it reaches the delivery carrier.
# delivered_estimated represents the date difference between the estimated delivery date and the actual delivery date.
# purchased_delivered represents the days taken for the order to be delivered to the customer from the date the customer made the purchase.


missing_values = colSums(is.na(df_cleaned))
print(missing_values)

# Filter rows where 'approved_carrier' is less than 0
falsifiedData = which(df_cleaned$approved_carrier < 0)
df_cleaned = df_cleaned[-falsifiedData, ]

# Filter rows where 'carrier_delivered' is less than 0
falsifiedData = which(df_cleaned$carrier_delivered < 0)
df_cleaned = df_cleaned[-falsifiedData, ]

# Find indices of rows with 'order_status' as 'canceled' after dropping NA values
canceledIndex = which(!is.na(df_cleaned$order_status) & df_cleaned$order_status == 'canceled')
df_cleaned = df_cleaned[-canceledIndex, ]


# Define the date-time format
date_format = "%Y-%m-%d %H:%M:%S"

# Convert columns to POSIXct type specifying the format
date_columns = c("order_approved_at", "order_purchase_timestamp", "order_delivered_carrier_date", "order_delivered_customer_date", "order_estimated_delivery_date")
df_cleaned[date_columns] = lapply(df_cleaned[date_columns], function(x) as.POSIXct(x, format = date_format))

# Calculate the time differences and create new columns
df_cleaned$purchased_approved = as.numeric(difftime(df_cleaned$order_approved_at, df_cleaned$order_purchase_timestamp, units = "secs"))
df_cleaned$approved_carrier = as.numeric(difftime(df_cleaned$order_delivered_carrier_date, df_cleaned$order_approved_at, units = "days"))
df_cleaned$carrier_delivered = as.numeric(difftime(df_cleaned$order_delivered_customer_date, df_cleaned$order_delivered_carrier_date, units = "days"))
df_cleaned$delivered_estimated = as.numeric(difftime(df_cleaned$order_estimated_delivery_date, df_cleaned$order_delivered_customer_date, units = "days"))
df_cleaned$purchased_delivered = as.numeric(difftime(df_cleaned$order_delivered_customer_date, df_cleaned$order_purchase_timestamp, units = "days"))

head(df_cleaned)

# Get summary statistics for non-numeric columns
non_numeric_summary = summary(df_cleaned[, sapply(df_cleaned, function(x) !is.numeric(x))])

# Display the summary statistics for non-numeric columns
print(non_numeric_summary)

#summary(df_cleaned)

#rfm 
df_merged = df_cleaned
colnames(df_merged)

# Dropping multiple columns
columns_to_drop = c('order_status', 'order_item_id', 'order_approved_at', 'order_delivered_carrier_date', 
                     'order_delivered_customer_date', 'order_estimated_delivery_date', 'approved_carrier', 
                     'carrier_delivered', 'seller_id', 'shipping_limit_date', 'product_category_name', 
                     'product_name_lenght', 'product_description_lenght', 'product_photos_qty', 
                     'payment_sequential', 'seller_zip_code_prefix')

merged = df_merged%>% select(-one_of(columns_to_drop))

# Printing the first few rows of the modified dataframe
head(merged)

# Assuming 'merged' is your dataframe in R and 'purchased_approved', 'delivered_estimated', 'purchased_delivered' are columns with boolean values
#summary(merged)

final <- merged %>%
  group_by(customer_unique_id) %>%
  summarise(customer_zip_code_prefix = max(customer_zip_code_prefix),
            customer_city = first(customer_city),
            customer_state = first(customer_state),
            order_id = n_distinct(order_id),
            purchased_approved = mean(purchased_approved),
            delivered_estimated = min(delivered_estimated),
            purchased_delivered = mean(purchased_delivered),
            product_id = n_distinct(product_id),
            price = sum(price),
            freight_value = sum(freight_value),
            product_weight_g = sum(product_weight_g),
            product_length_cm = sum(product_length_cm),
            product_height_cm = sum(product_height_cm),
            product_width_cm = sum(product_width_cm),
            geolocation_lat = mean(geolocation_lat),
            geolocation_lng = mean(geolocation_lng),
            payment_type = last(payment_type),
            payment_installments = max(payment_installments),
            payment_value = sum(payment_value),
  ) %>%
  ungroup()

# Printing the first few rows of the aggregated dataframe
head(final)


# Recency
# Assuming 'merged' is your dataframe in R
recency = merged %>%
  group_by(customer_unique_id) %>%
  summarise(LastPurchaseDate = max(order_purchase_timestamp)) %>%
  mutate(Recency = as.integer(as.Date(max(merged$order_purchase_timestamp)) - as.Date(LastPurchaseDate)))

# Display the head of the recency dataframe
head(recency)

# Print the last recent date in the available dataset
recent_date = as.Date(max(merged$order_purchase_timestamp))
print(paste("The last recent date in the available dataset is:", recent_date))

# Convert 0 in recency to 1
recency$Recency = ifelse(recency$Recency == 0, 1, recency$Recency)

# Frequency
frequency = merged %>%
  group_by(customer_unique_id) %>%
  summarise(Frequency = n_distinct(order_id))

# Monetary
monetary = merged %>%
  group_by(customer_unique_id) %>%
  summarise(Monetary = sum(payment_value))


# Install and load necessary packages
if (!require("tidyverse")) {
  install.packages("tidyverse")
}
if (!require("lubridate")) {
  install.packages("lubridate")
}

# Calculate Recency
recent_date = max(merged$order_purchase_timestamp %>% as.Date)
recency = merged %>%
  group_by(customer_unique_id) %>%
  summarize(LastPurchaseDate = max(order_purchase_timestamp)) %>%
  mutate(Recency = as.integer(difftime(recent_date, LastPurchaseDate, units = "days")))

# Calculate Frequency
frequency = merged %>%
  group_by(customer_unique_id) %>%
  summarize(Frequency = n_distinct(order_id))

# Calculate Monetary
monetary = merged %>%
  group_by(customer_unique_id) %>%
  summarize(Monetary = sum(payment_value))

# Merge Recency, Frequency, and Monetary
rfm = recency %>%
  left_join(frequency, by = "customer_unique_id") %>%
  left_join(monetary, by = "customer_unique_id")

# Remove 0s from Recency
recent_date = as.Date(recent_date)
rfm$Recency = ifelse(rfm$Recency == 0, 1, rfm$Recency)

# Create a target variable for Churn
rfm$Churn = ifelse(rfm$Recency > mean(rfm$Recency, na.rm = TRUE), 1, 0)
# The 'Churn' column is derived based on the 'Recency' feature.
# It assesses customer behavior by determining if their last interaction or purchase occurred more recently than the average.

# Using a lambda function with apply to categorize customers:
# If the customer's 'Recency' is greater than the mean 'Recency', they are marked as 'Churn' (1),
# indicating potential disengagement or a higher likelihood to stop doing business.
# Otherwise, they are labeled as 'Not Churn' (0),
# suggesting ongoing engagement or a lower risk of discontinuation.

# Check the structure of 'final' and 'rfm' dataframes
str(final)
str(rfm)

# Make sure 'customer_unique_id' exists in both dataframes and has consistent values
# If not, adjust the column names or perform necessary data cleaning steps to align them

# Assuming the column names are consistent, and both dataframes have 'customer_unique_id'
# Merge the dataframes based on 'customer_unique_id' column 
#Merging the target variable with our final dataframe

final = merge(final, rfm[c("customer_unique_id", "Recency", "Monetary", "Frequency", "Churn")], by = "customer_unique_id", all.x = TRUE)

# Check the merged 'final' dataframe
str(final)
head(final)

# Check the head of the merged RFM dataframe
#head(final)

#summary(final)

final$Churn = as.factor(final$Churn)


#Checking the skewness

# Select numeric columns
numeric_cols = sapply(final, is.numeric)
numeric_data = final[, numeric_cols]

# Loop through numeric columns
for (col in names(numeric_data)) {
# Calculate skewness and standard deviation
  skew = skewness(numeric_data[[col]])
  std_dev = sd(numeric_data[[col]])
  
# Print skewness and standard deviation
  cat(paste("Skewness of", col, ":", skew, "\n"))
  cat(paste("Standard deviation of", col, ":", std_dev, "\n"))
}

# Dropping the columns which has close to 0 standard deviation 
columns_to_drop = c('customer_zip_code_prefix', 'no_of_orders', 'no_of_products', 'Frequency')

final = final[, !names(final) %in% columns_to_drop]


final_c = final
missing_percentages = colSums(is.na(final_c)) / nrow(final_c) * 100
colnames(final_c)


# Multivariate Analysis
percentage_distribution = prop.table(table(final_c$Churn)) * 100
print(percentage_distribution)

#dealing with the outliers
replace_outliers = function(x) {
  z_scores = scale(x)
  outliers = abs(z_scores) > 3  # Define threshold for outliers (e.g., 3)
  x[outliers] = median(x, na.rm = TRUE)  # Replace outliers with median (or mean)
  return(x)
}

# Apply outlier handling to numeric columns in the dataframe
numeric_columns = final_c %>% select_if(is.numeric)

final_c[ , names(numeric_columns)] = lapply(numeric_columns, function(x) {
  replace_outliers(x)
})

#correlation matrix 

head(final_c)

numeric_cols = sapply(final_c, is.numeric)
numeric_data = final_c[, numeric_cols]
corr_matrix = cor(numeric_data)
skewness_values = sapply(numeric_data, skewness)
std_deviation = sapply(numeric_columns, sd, na.rm = TRUE)

# Print standard deviation of each column
print(std_deviation)

print(skewness_values)
corrplot(corr_matrix, method = 'circle')


# Define the function for state encoding
state_encoding = function(state) {
  if (state %in% c('RS', 'SC', 'PR')) {
    return('southern')
  } else if (state %in% c('SP', 'RJ', 'MG', 'ES')) {
    return('southeastern')
  } else if (state %in% c('MT', 'MS', 'GO', 'DF')) {
    return('centralwestern')
  } else if (state %in% c('MA', 'PI', 'CE', 'RN', 'PB', 'PE', 'AL', 'SE', 'BA')) {
    return('northeastern')
  } else {
    return('northern')
  }
}

# Apply state encoding to 'customer_state' column in 'final_c'
final_c$customer_state = sapply(final_c$customer_state, state_encoding)

# Creating 'features' dataframe by copying 'final_c'
features = final_c

# Dropping unnecessary columns
features = subset(features, select = -c(customer_unique_id, customer_city, payment_value))

# Dropping 'Recency' column from 'features'
features$Recency = NULL

# Creating 'independent' dataframe without 'Churn' column
independent = subset(features, select = -Churn)

# Separating numeric and categorical columns
df_numeric = independent[, sapply(independent, is.numeric)]
df_categorical = independent[, !sapply(independent, is.numeric)]

# Converting 'Churn' to integer type for 'df_target'
df_target = as.integer(features$Churn)

# Encoding categorical variables
encoded_data = model.matrix(~ . - 1, data = df_categorical)

# Combining numeric and encoded categorical variables
X = cbind(df_numeric, encoded_data)

# Dropping 'Recency' column from 'X'
X$Recency = NULL

# Displaying the resulting 'X' dataframe
head(X)
colnames(X)

# Set seed for reproducibility
set.seed(500)
head(final_c)



######################## Model building ########################################################

# Load and preprocess data
#final_c = read_csv('final_c.csv')

# Selecting columns
columns_to_keep = c("customer_state", "purchased_approved", "delivered_estimated", 
                     "purchased_delivered", "price", "freight_value", "product_weight_g", 
                     "product_length_cm", "product_height_cm", "product_width_cm", 
                     "geolocation_lat", "geolocation_lng", "payment_type",
                     "payment_installments", "Monetary", "Churn")

final_c = final_c %>% select(all_of(columns_to_keep)) %>% drop_na()

# Check missing values
missing_values = sapply(final_c, function(x) sum(is.na(x)))
missing_columns = missing_values[missing_values > 0]
print(missing_columns)

# Using only 10% of the data
set.seed(500)
final_c_sampled = final_c %>% sample_frac(0.9)

# Separate the data into independent and dependent variables
final_independent = final_c %>% select(-Churn)
final_df_target = final_c$Churn

# Convert 'Churn' column to integer type
#final_c_sampled$Churn <- as.integer(final_c_sampled$Churn)
#head(final_c_sampled)

# Create dummy variables for categorical data
final_encoded_data = dummyVars("~ .", data = final_independent)
final_independent_encoded = predict(final_encoded_data, newdata = final_independent)
head(final_independent_encoded)
# Scale the features
preProcValues = preProcess(final_independent_encoded, method = c("center", "scale"))
final_X_scaled = predict(preProcValues, final_independent_encoded)


# Train-test split
set.seed(500)
trainIndex = createDataPartition(final_df_target, p = .8, 
                                  list = FALSE, 
                                  times = 1)
X_train = final_X_scaled[trainIndex, ]
X_test  = final_X_scaled[-trainIndex, ]
y_train = final_df_target[trainIndex]
y_test  = final_df_target[-trainIndex]

# Print the shapes of the resulting sets
cat('xtrain:', dim(X_train), '\n')
cat('ytrain:', length(y_train), '\n')
cat('xtest:', dim(X_test), '\n')
cat('ytest:', length(y_test), '\n')

summary(y_train)

##################################################

# Build the logistic regression model
logit_model = glm(y_train ~ ., data = data.frame(X_train, y_train), family = "binomial")
summary(logit_model)


logit_model_updated = glm(
  y_train ~ . - customer_statecentralwestern - customer_statenortheastern - 
    customer_statenorthern - customer_statesoutheastern - customer_statesouthern - 
    price - geolocation_lat - geolocation_lng - payment_typeboleto - 
    product_weight_g,
  family = "binomial",
  data = data.frame(X_train, y_train)
)
summary(logit_model_updated)

#aic remained the same
# AIC
AIC(logit_model)

# Predictions
y_pred_prob_train = predict(logit_model, newdata = data.frame(X_train), type = "response")
y_pred_train = ifelse(y_pred_prob_train < 0.5, 0, 1)

y_pred_prob = predict(logit_model, newdata = data.frame(X_test), type = "response")
y_pred = ifelse(y_pred_prob < 0.5, 0, 1)
head(y_pred)

# Create a Confusion Matrix
cm = confusionMatrix(factor(y_pred), factor(y_test))
print(cm)

# ROC Curve and AUC
roc_obj = roc(y_test, y_pred_prob)
plot(roc_obj, main = "ROC curve - Logistic Regression",col='red')
text(x = 0.02, y = 0.9, labels = paste("AUC Score:", round(auc(roc_obj), 4)))

auc_score = auc(roc_obj)
print(paste('AUC Score:', auc_score))

########################################

# Building the Decision Tree Model

library(rpart)
# Scale the features
preProcValues = preProcess(final_independent_encoded, method = c("center", "scale"))
final_X_scaled_df = predict(preProcValues, final_independent_encoded)


# Assuming final_X_scaled_df and final_df_target are available
set.seed(500)
trainIndex = createDataPartition(final_df_target, p = .8, list = FALSE, times = 1)
xtrain_dt = final_X_scaled_df[trainIndex, , drop = FALSE]  # Ensure it remains a dataframe
xtest_dt = final_X_scaled_df[-trainIndex, , drop = FALSE]  # Ensure it remains a dataframe
ytrain_dt = final_df_target[trainIndex]
ytest_dt = final_df_target[-trainIndex]

# Print the shapes of the resulting sets
cat('xtrain:', dim(xtrain_dt), '\n')
cat('ytrain:', length(ytrain_dt), '\n')
cat('xtest:', dim(xtest_dt), '\n')
cat('ytest:', length(ytest_dt), '\n')

# Fit the Decision Tree
decisionTree = rpart(ytrain_dt ~ ., data = data.frame(xtrain_dt, ytrain_dt), method = "class")


rpart.plot(decisionTree)

# Predictions
ypred_dt = predict(decisionTree, newdata = data.frame(xtest_dt), type = "class")

# Convert to factors ensuring they have the same levels
levels = union(levels(factor(ypred_dt)), levels(factor(ytest_dt)))
ypred_dt_factor = factor(ypred_dt, levels = levels)
ytest_dt_factor = factor(ytest_dt, levels = levels)


# Convert factor/character to numeric
ypred_numeric = as.numeric(as.character(ypred_dt_factor))
ytest_numeric = as.numeric(as.character(ytest_dt_factor))

# Create Confusion Matrix
cm = confusionMatrix(ypred_dt_factor, ytest_dt_factor)
print(cm)

# Calculate ROC curve
roc_obj = roc(ytest_numeric, ypred_numeric)


# Plot ROC curve
plot(roc_obj, main = "ROC Curve for Decision Tree", col = "red")
text(x = 0.02, y = 0.9, labels = paste("AUC Score:", round(auc(roc_obj), 4)))

# Calculate AUC
auc_val = auc(roc_obj)
cat("AUC Score:", auc_val, "\n")

########################################

#Building the Random forest Model


# Splitting the dataset
set.seed(500)
head(final_df_target)
splitIndex = createDataPartition(final_df_target, p = 0.8, list = FALSE)
xtrain_random = final_X_scaled_df[splitIndex, ]
xtest_random = final_X_scaled_df[-splitIndex, ]
ytrain_random = final_df_target[splitIndex]
ytest_random = final_df_target[-splitIndex]

# Print the shapes of the resulting sets
cat('xtrain:', dim(xtrain_random), '\n')
cat('ytrain:', length(ytrain_random), '\n')
cat('xtest:', dim(xtest_random), '\n')
cat('ytest:', length(ytest_random), '\n')


# Assuming xtrain_random, ytrain_random, xtest_random, and ytest_random are available

# Train the Random Forest model
rand_model = randomForest(xtrain_random, ytrain_random)

# Predict probabilities and classes
ypred_proba_random = predict(rand_model, xtest_random, type = "prob")
ypred_random = ifelse(ypred_proba_random[,2] > 0.5, 1, 0) # Assuming class '1' is the positive class

# Predict on the training set for evaluation
ypred_proba_random_train = predict(rand_model, xtrain_random, type = "prob")
ypred_random_train = ifelse(ypred_proba_random_train[,2] > 0.5, 1, 0)

# Confusion Matrix and heatmap
cm = confusionMatrix(factor(ypred_random), factor(ytest_random))
print(cm)

# ROC Curve
roc_obj = roc(ytest_random, ypred_proba_random[,2])
plot(roc_obj, main = "ROC curve - Random Forest Model",col='red')

text(x = 0.02, y = 0.9, labels = paste("AUC Score:", round(auc(roc_obj), 4)))







####################### Exploratory Data Analysis ############################################

#Distribution of Order Status
# create a count table of order_status
status_count <- df_orders %>% 
  count(order_status) %>% 
  mutate(pct = n/sum(n))
# reorder the order_status by the count of each status in descending order
status_count$order_status <- factor(status_count$order_status, levels = status_count$order_status[order(status_count$n, decreasing = TRUE)])
# plot the count table
ggplot(status_count, aes(order_status, n)) +
  geom_bar(stat="identity", fill = "slateblue") +
  labs(title= "Distribution of Order Status", x="Order Status", y="Count") +
  theme_minimal() +
  geom_text(aes(label = paste0(sprintf("%.1f", pct*100), "%")), size = 3.5, vjust = -0.2) +
  theme(plot.title = element_text(size = 13)) +
  theme(panel.background = element_rect(fill='white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

#combine df_orders and df_customer
df_orders <- df_orders %>% 
  left_join(df_customer, by = "customer_id")

#Define a function that can convert latitude and longitude into decimal degrees
angle2dec <- function(angle) {
  angle <- as.character(angle)
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    y[1] + y[2]/60 + y[3]/3600
  })
  return(x)
}

lat_north <- "5 16 27.8"
lat_south <- "33 45 04.21"
long_west <- "73 58 58.19"
long_east <- "34 47 35.33"

decimal_degrees_north <- angle2dec(lat_north)
decimal_degrees_south <- -angle2dec(lat_south)#the negative sign and multiplication by -1 to get south longitude
decimal_degrees_west <- -angle2dec(long_west)#the negative sign and multiplication by -1 to get West longitude
decimal_degrees_east <- -angle2dec(long_east)#the negative sign and multiplication by -1 to get West longitude

#Brazils territory
geo <- df_geolocation %>% filter(geolocation_lat <= decimal_degrees_north &
                                   geolocation_lat >= decimal_degrees_south &
                                   geolocation_lng <= decimal_degrees_east &
                                   geolocation_lng >= decimal_degrees_west)
geo <- geo %>%
  group_by(geolocation_zip_code_prefix) %>% 
  summarise_all(min) %>% 
  ungroup()

library(forcats)
# Top cities with more customers orders in Brazil
df_orders %>% 
  group_by(geolocation_city) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ggplot(aes(fct_reorder(geolocation_city,n), n, fill = fct_reorder(geolocation_city,n))) + 
  geom_bar(stat="identity", show.legend = F) +
  coord_flip() +
  labs(title = "Top 10 Brazilian Cities with More Orders", 
       x = "City", y = "Total Orders")+
  theme_minimal() +
  theme(plot.title = element_text(size = 13), 
        axis.title=element_text(size=8)) +
  theme(panel.background = element_rect(fill='white', colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_text(aes(label = n), size = 3, hjust = 1.2, color = "white")+ 
  scale_fill_brewer(palette = "Spectral")


#Total orders by state
df_orders %>% 
  group_by(geolocation_state) %>% 
  count() %>% 
  arrange(desc(n)) %>% 
  head(10) %>% 
  ggplot(aes(fct_reorder(geolocation_state,n), n, fill = fct_reorder(geolocation_state,n))) + 
  geom_bar(stat="identity", show.legend = F) +
  coord_flip() +
  labs(title = "Top 10 Brazilian State with More Orders", 
       x = "State", y = "Total Orders")+
  theme_minimal() +
  theme(plot.title = element_text(size = 13), 
        axis.title=element_text(size=8)) +
  theme(panel.background = element_rect(fill='white', colour = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  geom_text(aes(label = n), size = 3, hjust = 1.2, color = "white")+ 
  scale_fill_brewer(palette = "Spectral")


###Growth of Sales Analyis
# Growth of sales
df_order_items2 <- df_items %>% 
  group_by(order_id) %>% 
  mutate(price_2 = mean(price), 
         freight_value2 = mean(freight_value)) %>% 
  distinct(order_id, .keep_all = T) %>% 
  dplyr:: select(-price, -freight_value)

df_orders_items <- df_orders  %>% 
  left_join(df_order_items2, by = "order_id")

#combine olist_orders and olist_customer
df_orders <- df_orders %>% 
  left_join(df_customer, by = "customer_id")

# Changing the data type for datetime format
timestamp_cols <- c('order_purchase_timestamp', 'order_approved_at', 'order_delivered_carrier_date', 
                    'order_estimated_delivery_date')
df_orders[,timestamp_cols] <- lapply(df_orders[,timestamp_cols], as.POSIXct, format = "%Y-%m-%d %H:%M:%S")

# Extracting attributes for purchase date - Year and Month
df_orders <- df_orders %>% 
  mutate(order_purchase_year = year(order_purchase_timestamp),
         order_purchase_month = month(order_purchase_timestamp, label = T),
         order_purchase_year_month = format(df_orders$order_purchase_timestamp, "%Y%m"),
         order_purchase_year_month_day = format(df_orders$order_purchase_timestamp, "%Y%m%d"))

df_orders_items <- df_orders  %>% 
  left_join(df_items, by = "order_id")

df_orders<- df_orders  %>% 
  left_join(df_orders_items, by = "order_id")

sales_plot <- df_orders %>%
  group_by(order_purchase_year_month.y) %>% 
  summarise(price_sum = sum(price, na.rm = T),
            count = n()) %>% 
  ggplot(aes(order_purchase_year_month.y, price_sum, group = 1)) +
  geom_line(color = "slateblue") +
  geom_point(color = "slateblue") +
  labs(title = "Evolution of Brazilian E-Commerce : Total Orders and Total Amount Sold(R$)", 
       x = "Year-Month", y = "Total Amount Sold (R$)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 13),
        axis.text.x = element_blank(), 
        axis.title.x =element_blank(),
        axis.title.y = element_text(size = 10),
        axis.text.y=element_blank()) + 
  theme(panel.background = element_rect(fill='white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  geom_text(aes(label = paste0(round(price_sum/1000, 1), "k")), size = 3, vjust = 1.3, color = "black", position =  position_dodge(width = 1))

amount_plot <- df_orders %>%
  group_by(order_purchase_year_month.y) %>% 
  count() %>% 
  ggplot(aes(order_purchase_year_month.y, n)) +
  geom_bar(stat = "identity", fill = "slateblue") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10),
        axis.text.x = element_text(angle = 45, hjust = 1), 
        axis.title=element_text(size=8)) + 
  theme(panel.background = element_rect(fill='white'),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_text(size = 10)) +
  labs(y = "Total Orders")

library (grid)
grid.newpage()
grid.draw(rbind(ggplotGrob(sales_plot), ggplotGrob(amount_plot), size = "last"))


### Product Analysis

df_orders_items <- df_orders_items %>% 
  left_join(df_product, by = "product_id")

df_orders_items <- df_orders_items %>% 
  left_join(df_reviews, by = "order_id")

category_counts <- df_orders_items %>%
  group_by(product_category_name) %>%
  summarize(count = n())

# Sort the categories by count in descending order
category_counts <- category_counts %>%
  arrange(desc(count))

# Create a bar plot
ggplot(head(category_counts,10), aes(x = reorder(product_category_name, -count), y = count)) +
  geom_bar(stat = "identity", fill = "#bcbddc") +
  labs(x = "Product Category", y = "Frequency") +
  coord_flip() +  # Horizontal bars for better readability
  theme_minimal() +
  ggtitle("Distribution of Product Categories")






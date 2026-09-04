###### load libraries #####
library(readr);
library(dplyr);
library(ggplot2)
library(tibble);
library(sqldf);
library(lubridate);
library(class);
library(MASS);
library(rpart);
library(rpart.plot);
library(randomForest);
library(caret);

##### get csv files #####
customers_df <- read_csv("MavenMarket_Customers.csv")
products_df <- read_csv("MavenMarket_Products.csv")
regions_df <- read_csv("MavenMarket_Regions.csv")
returns_df <- read_csv("MavenMarket_Returns_1997-1998.csv")
stores_df <- read_csv("MavenMarket_Stores.csv")
transactions97_df <- read_csv("MavenMarket_Transactions_1997.csv")
transactions98_df <- read_csv("MavenMarket_Transactions_1998.csv")
predData <- read_csv("predData.csv")

# ===== convert dates and remove duplicates ======
returns_df = distinct(returns_df)
transactions97_df = distinct(transactions97_df)
transactions98_df = distinct(transactions98_df)
customers_df$acct_open_date = mdy(customers_df$acct_open_date);
returns_df$return_date = mdy(returns_df$return_date);
transactions97_df$stock_date = mdy(transactions97_df$stock_date);
transactions97_df$transaction_date = mdy(transactions97_df$transaction_date);
transactions98_df$stock_date = mdy(transactions98_df$stock_date);
transactions98_df$transaction_date = mdy(transactions98_df$transaction_date);
transactions_df = rbind(transactions97_df, transactions98_df)

# Convert categorical attributes to factors
predData <- predData %>%
  mutate(
    marital_status = as.factor(marital_status),
    gender = as.factor(gender),
    store_type = as.factor(store_type),
    education = as.factor(education),
    member_card = as.factor(member_card),
    occupation = as.factor(occupation),
    homeowner = as.factor(homeowner),
    highOrLow = as.factor(highOrLow)
  )


# ============== prediction Models ==============
# Split the data into training and testing sets
set.seed(124)
percen_train = 2/3
idx_split = sample(1:nrow(predData), nrow(predData)*percen_train)
train_data = predData[idx_split,]
remaining_data = predData[-idx_split,]
percen_valid = 1/2
valid_split = sample(1:nrow(remaining_data), nrow(remaining_data)*percen_valid)
valid_data <- remaining_data[valid_split, ]
test_data <- remaining_data[-valid_split, ]

# Handle categorical variables
# One-hot encode categorical variables for KNN
train_knn <- model.matrix(highOrLow ~ ., data = train_data)[, -1]
valid_knn <- model.matrix(highOrLow ~ ., data = valid_data)[, -1]
test_knn <- model.matrix(highOrLow ~ ., data = test_data)[, -1]

train_labels <- train_data$highOrLow
valid_labels <- valid_data$highOrLow
test_labels <- test_data$highOrLow

# Step 3: Train and tune models
# KNN: Normalize data and train
train_knn <- scale(train_knn)
valid_knn <- scale(valid_knn, center = attr(train_knn, "scaled:center"),
                   scale = attr(train_knn, "scaled:scale"))

knn_results <- data.frame(k = integer(), accuracy = numeric())
for (k in seq(1, 25)) {
  pred_knn <- knn(train = train_knn, test = valid_knn, cl = train_labels, k = k)
  # calculate accuracy, sensitivity, and specificity
  acc <- mean(pred_knn == valid_labels)
  sensitivity <- sum(pred_knn == 1 & valid_labels == 1) / sum(valid_labels == 1)
  specificity <- sum(pred_knn == 0 & valid_labels == 0) / sum(valid_labels == 0)
  knn_results <- rbind(knn_results, data.frame(k = k, accuracy = acc, sensitivity = 
                                                 sensitivity, specificity = specificity))
}
# plot k vs sensitivity and specificity
ggplot(knn_results, aes(x = k)) +
  geom_line(aes(y = sensitivity, color = "Sensitivity")) +
  geom_line(aes(y = specificity, color = "Specificity")) +
  labs(
    title = "KNN Model: Sensitivity and Specificity vs. K",
    x = "K",
    y = "Value"
  ) +
  scale_color_manual(values = c("Sensitivity" = "blue", "Specificity" = "red")) +
  theme_minimal()
# print accuracy, sensitivity, and specificity for best k
best_k <- knn_results$k[which.max(knn_results$sensitivity)]
print("KNN Results:")
print(paste("Best K:", best_k))
print(paste("Accuracy:", round(knn_results$accuracy[best_k], 2)))
print(paste("Sensitivity:", round(knn_results$sensitivity[best_k], 2)))
print(paste("Specificity:", round(knn_results$specificity[best_k], 2)))

# LDA
lda_model <- lda(highOrLow ~ ., data = train_data)
lda_pred <- predict(lda_model, valid_data)$class
# print confusion matrix using table
confusion_matrix_lda <- table(lda_pred, valid_labels)
# calculate accuracy, sensitivity, and specificity
accuracy_lda <- sum(diag(confusion_matrix_lda)) / sum(confusion_matrix_lda)
sensitivity_lda <- confusion_matrix_lda[2, 2] / sum(valid_labels == 1)
specificity_lda <- confusion_matrix_lda[1, 1] / sum(valid_labels == 0)
# print results
print("LDA Results:")
print(paste("Accuracy:", round(accuracy_lda, 2)))
print(paste("Sensitivity:", round(sensitivity_lda, 2)))
print(paste("Specificity:", round(specificity_lda, 2)))

# CART
cart_model <- rpart(highOrLow ~ ., data = train_data, method = "class",
                    parms=list(split="information"),
                    minsplit=2, minbucket=1, cp=-1, na.action=na.omit)
# Determine the number of complexity parameter (CP) values
n_tree <- nrow(cart_model$cptable)

# Initialize a result dataframe to store the number of nodes, CP values, validation error rates, sensitivity, and specificity
df_res <- data.frame(nbr_node = 0, 
                     cp = cart_model$cptable[,1], 
                     error_rates_val = 0,
                     sensitivity = 0,
                     specificity = 0) 

# Loop through each CP value to prune the tree and evaluate performance
for (ii in 1:n_tree){
  # Step 1: Prune the tree
  pruned.tree <- prune(cart_model, cp = cart_model$cptable[ii, 1]) 
  
  # Step 2: Predict on validation set
  validation.pred <- predict(pruned.tree, valid_data, type = "class")
  
  # Step 3: Create confusion matrix
  pred.results <- table(valid_data$highOrLow, validation.pred) 
  
  # Step 4: Calculate error rate
  error_rate_val  <- 1 - sum(diag(pred.results)) / sum(pred.results)
  
  # Calculate sensitivity and specificity
  sensitivity_val <- pred.results[2, 2] / sum(valid_data$highOrLow == 1)
  specificity_val <- pred.results[1, 1] / sum(valid_data$highOrLow == 0)
  
  # Store the computed metrics
  df_res[ii, 'error_rates_val'] <- error_rate_val
  df_res[ii, 'nbr_node'] <- nrow(pruned.tree$frame)
  df_res[ii, 'sensitivity'] <- sensitivity_val
  df_res[ii, 'specificity'] <- specificity_val
}

# Print the computed validation error rates, sensitivity, and specificity
print(df_res)
# find best cp
min_error_rate <- which.min(cart_model$cptable[, "xerror"])
best_cp <- which( cart_model$cptable[, "xerror"] < 
                    (cart_model$cptable[min_error_rate, "xerror"] +
                       cart_model$cptable[min_error_rate, "xstd"]) )[1]
# print best cp value and number of nodes
print(paste("Best CP value: ", cart_model$cptable[best_cp, "CP"],
            "Number of nodes: ", df_res[best_cp, "nbr_node"]))
# prune the tree
cart_model <- prune(cart_model, cp = cart_model$cptable[best_cp, "CP"])
# predict on validation set
cart_pred <- predict(cart_model, valid_data, type = "class")
# print confusion matrix using table
confusion_matrix_cart <- table(cart_pred, valid_labels)
# calculate accuracy, sensitivity, and specificity
accuracy_cart <- sum(diag(confusion_matrix_cart)) / sum(confusion_matrix_cart)
sensitivity_cart <- confusion_matrix_cart[2, 2] / sum(valid_labels == 1)
specificity_cart <- confusion_matrix_cart[1, 1] / sum(valid_labels == 0)
# print accuracy, sensitivity, and specificity
print("CART Results:")
print(paste("Accuracy:", round(accuracy_cart, 2)))
print(paste("Sensitivity:", round(sensitivity_cart, 2)))
print(paste("Specificity:", round(specificity_cart, 2)))
# plot pruned tree
rpart.plot(cart_model, extra = 101, type = 4, fallen.leaves = F)

# Random Forest
rf_model <- randomForest(highOrLow ~ ., data = train_data, ntree = 100)
rf_pred <- predict(rf_model, valid_data)
# print confusion matrix using table
confusion_matrix_rf <- table(rf_pred, valid_labels)
# calculate accuracy, sensitivity, and specificity
accuracy_rf <- sum(diag(confusion_matrix_rf)) / sum(confusion_matrix_rf)
sensitivity_rf <- confusion_matrix_rf[2, 2] / sum(valid_labels == 1)
specificity_rf <- confusion_matrix_rf[1, 1] / sum(valid_labels == 0)
# print accuracy, sensitivity, and specificity
print("Random Forest Results:")
print(paste("Accuracy:", round(accuracy_rf, 2)))
print(paste("Sensitivity:", round(sensitivity_rf, 2)))
print(paste("Specificity:", round(specificity_rf, 2)))
      
# Evaluate Models
# Function to calculate sensitivity, specificity, and F1-Score
evaluate_model <- function(actual, predicted) {
  conf_matrix <- table(actual, predicted)
  sensitivity <- conf_matrix$class["Sensitivity"]
  specificity <- conf_matrix$class["Specificity"]
  f1 <- conf_matrix$byClass["F1"]
  list(Sensitivity = sensitivity, Specificity = specificity, F1 = f1)
}

# Evaluate all models
knn_eval <- evaluate_model(valid_labels, knn(train_knn, valid_knn, train_labels, k = best_k))
lda_eval <- evaluate_model(valid_data$highOrLow, lda_pred)
cart_eval <- evaluate_model(valid_data$highOrLow, cart_pred)
rf_eval <- evaluate_model(valid_data$highOrLow, rf_pred)

# Print evaluations
print(knn_eval)
print(lda_eval)
print(cart_eval)
print(rf_eval)

# Test final model
# Use Random Forest for final testing
final_rf_pred <- predict(rf_model, test_data)
test_eval <- evaluate_model(test_data$highOrLow, final_rf_pred)
print(test_eval)


# ============== Average Income by Region ==============
# convert income range to numeric
numeric_income = customers_df |> 
  mutate(
    income_min = ifelse(grepl("-", yearly_income),
                        as.numeric(sub("K.*", "", sub("\\$", "", yearly_income))) * 1000,
                        ifelse(grepl("\\+", yearly_income), 150000, NA)),
    income_max = ifelse(grepl("-", yearly_income),
                        as.numeric(sub("K","",sub(".*- \\$", "", yearly_income))) * 1000,
                        ifelse(grepl("\\+", yearly_income), 150000, NA)),
    income_midpoint = (income_min + income_max) / 2
  )

# group customers by state and income
inc_state_customers = customers_df |>  
  group_by(customer_state_province, yearly_income) |> 
  summarise(
    count = n())
inc_state_cust_num = numeric_income |>  
  group_by(customer_state_province, income_midpoint) |> 
  summarise(
    count = n())

# avg inicome midpoint
avg_income_state = numeric_income |> 
  group_by(customer_state_province) |> 
  summarise(
    avg_income=mean(income_midpoint,na.rm = T)
    )

# plot bar graph for avg income
ggplot(avg_income_state, aes(x = reorder(customer_state_province, avg_income), y = avg_income)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip()


colSums(is.na(transactions98_df));




# ============== Returns ==============
# Join returns_df with stores_df to get region information
returns_with_regions <- returns_df %>%
  left_join(stores_df, by = "store_id") %>%
  left_join(regions_df, by = "region_id")

# Join with products_df to get product information
returns_with_products <- returns_with_regions %>%
  left_join(products_df, by = "product_id")

# 1. Distribution of Returns by Region
returns_by_region <- returns_with_products %>%
  group_by(store_state) %>%
  summarise(total_returns = sum(quantity, na.rm = TRUE)) 

# Plot distribution of returns by region
ggplot(returns_by_region, aes(x = reorder(store_state, -total_returns), y = total_returns)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(
    title = "Total Returns by Region",
    x = "Region",
    y = "Total Returns"
  ) +
  theme_minimal() +
  coord_flip()

# 2. Distribution of Returns by Product Type
returns_by_product_type <- returns_with_products %>%
  group_by(product_brand, product_name) %>%
  summarise(total_returns = sum(quantity, na.rm = TRUE)) %>%
  arrange(desc(total_returns))

# Plot top 10 products by total returns
top_returns_by_product <- returns_by_product_type %>%
  slice_head(n = 10)  # Select top 10 products

ggplot(top_returns_by_product, aes(x = reorder(product_name, -total_returns), y = total_returns)) +
  geom_bar(stat = "identity", fill = "darkorange") +
  labs(
    title = "Top 10 Products by Total Returns",
    x = "Product Name",
    y = "Total Returns"
  ) +
  theme_minimal() +
  coord_flip()

# 3. Correlation Between Product Price and Return Likelihood
# Calculate total returns for each product
product_return_data <- returns_with_products %>%
  group_by(product_id, product_retail_price) %>%
  summarise(total_returns = sum(quantity, na.rm = TRUE)) %>%
  ungroup()

# Plot scatterplot of product price vs total returns
ggplot(product_return_data, aes(x = product_retail_price, y = total_returns)) +
  geom_point(color = "purple", alpha = 0.6) +
  geom_smooth(method = "lm", color = "red", se = FALSE) +
  labs(
    title = "Correlation Between Product Price and Return Likelihood",
    x = "Product Retail Price ($)",
    y = "Total Returns"
  ) +
  theme_minimal()

# Correlation coefficient
correlation <- cor(product_return_data$product_retail_price, product_return_data$total_returns, use = "complete.obs")
print(paste("Correlation between Product Price and Return Likelihood:", round(correlation, 2)))


# ============== Customer Revs =============
transactions_with_prices <- transactions_df %>%
  left_join(products_df, by = "product_id")

# Calculate the total revenue for each transaction
transactions_with_prices <- transactions_with_prices %>%
  mutate(revenue = quantity * product_retail_price)

# Aggregate the revenue by customer
customer_revenue_df <- transactions_with_prices %>%
  group_by(customer_id) %>%
  summarise(total_revenue = sum(revenue, na.rm = TRUE))


######### Functions ########
{
  # find NAs in col
my_df <-  numeric_income
my_field <-  "income_max"
my_filter <- my_df |> filter(
  is.na(my_df[[my_field]])
)
sum(is.na(my_filter))
}

{
  # duplicates: returns, trans97, trans 98
  my_df = transactions98_df
  nrow(my_df) == nrow(distinct(my_df))
}
{
  # find dup indices
  ind = which(duplicated(my_df))
}
# ===== Cust Prod Preference (Impossible) =====
# Identify Customer Preferences
# Join transactions_df with products_df to get product types
transactions_with_product_types <- transactions_df %>%
  left_join(products_df, by = "product_id")
# Calculate the total quantity purchased by each customer for each product type
customer_product_preferences <- transactions_with_product_types %>%
  group_by(customer_id, product_brand) %>%
  summarise(total_quantity = sum(quantity, na.rm = TRUE)) %>%
  arrange(customer_id, desc(total_quantity))

# Identify the preferred product type for each customer
customer_preferences <- customer_product_preferences %>%
  group_by(customer_id) %>%
  slice(1) %>%  # most preferred
  ungroup()

# Calculate Total Revenue by Product Type
# Join customer_revenue_df with customer_preferences to get preferred product types
customer_revenue_with_preferences <- customer_revenue_df %>%
  left_join(customer_preferences, by = "customer_id")

# Compare Revenues
# Aggregate the total revenue by preferred product type
revenue_by_product_type <- customer_revenue_with_preferences %>%
  group_by(product_brand) %>%
  summarise(total_revenue = sum(total_revenue, na.rm = TRUE))

#### Availability to Sales of Brands ###
# Calculate the Availability of Each Brand
# Aggregate the total quantity available for each brand
brand_availability <- transactions_df %>%
  left_join(products_df, by = "product_id") %>%
  group_by(product_brand) %>%
  summarise(total_quantity_available = sum(quantity, na.rm = TRUE))

# Standardize the Revenue by Availability
# Join the revenue_by_product_type with brand_availability
standardized_revenue <- revenue_by_product_type %>%
  left_join(brand_availability, by = "product_brand") %>%
  mutate(standardized_revenue = total_revenue / total_quantity_available)

# Print the resulting data frame
print(standardized_revenue)

# Step 1: Calculate the Total Revenue and Total Quantity Sold for Each Brand
brand_revenue_quantity <- transactions_df %>%
  left_join(products_df, by = "product_id") %>%
  mutate(revenue = quantity * product_retail_price) %>%
  group_by(product_brand) %>%
  summarise(
    total_revenue = sum(revenue, na.rm = TRUE),
    total_quantity_sold = sum(quantity, na.rm = TRUE)
  )

# Step 2: Calculate the Average Revenue per Unit Sold
brand_revenue_quantity <- brand_revenue_quantity %>%
  mutate(average_revenue_per_unit = total_revenue / total_quantity_sold)

#### Availability with # Units Sold ###
# Calculate the Total Quantity Available for Each Brand
brand_availability <- transactions_df %>%
  left_join(products_df, by = "product_id") %>%
  group_by(product_brand) %>%
  summarise(total_quantity_available = sum(quantity, na.rm = TRUE))

# Calculate the Total Quantity Sold for Each Brand
brand_revenue_quantity <- transactions_df %>%
  left_join(products_df, by = "product_id") %>%
  mutate(revenue = quantity * product_retail_price) %>%
  group_by(product_brand) %>%
  summarise(
    total_revenue = sum(revenue, na.rm = TRUE),
    total_quantity_sold = sum(quantity, na.rm = TRUE)
  )

# Combine These Metrics
brand_comparison <- brand_revenue_quantity %>%
  left_join(brand_availability, by = "product_brand") %>%
  mutate(
    availability_vs_sold = total_quantity_sold / total_quantity_available
  )



# ============== Education ============
# Aggregate transactions by region and education level
education_transactions <- transactions97_df %>%
  inner_join(customers_df, by = "customer_id") %>%  # Join transactions with customer details
  group_by(customer_state_province, education) %>%  # Group by region and education level
  summarise(total_transactions = sum(quantity, na.rm = TRUE)) %>%  # Sum transactions
  ungroup()

# Plot: Distribution of transactions by education level for each region
ggplot(education_transactions, aes(x = education, y = total_transactions, fill = education)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ customer_state_province, scales = "free_y")   # Create separate panels for each region
# +labs(
#   title = "Distribution of Transactions by Education Level Across Regions",
#   x = "Education Level",
#   y = "Total Transactions"
# ) +
# theme_minimal() +
# theme(axis.text.x = element_text(angle = 45, hjust = 1))
## Transaction 98
# Aggregate transactions by region and education level
education_transactions <- transactions98_df %>%
  inner_join(customers_df, by = "customer_id") %>%  # Join transactions with customer details
  group_by(customer_state_province, education) %>%  # Group by region and education level
  summarise(
    n = n(),
    total_transactions = sum(quantity, na.rm = TRUE),
    avg_transactions = sum(quantity, na.rm = TRUE)/n()) %>%  # Sum transactions
  ungroup()

# Plot: Distribution of transactions by education level for each region
ggplot(education_transactions, aes(x = education, y = n, fill = education)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ customer_state_province, scales = "free_y")  # Create separate panels for each region




        
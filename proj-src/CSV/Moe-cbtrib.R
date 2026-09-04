## Monthly Expenditure Per Customer ##

# Below are some data-sets that look into monthly consumer spending
# Throughout the code, I created data sets that were derived from previous, larger data sets by grouping
# The intermediate data sets (the ones I used to create data sets) still offer valuable information
# If you feel that there are some tweaks or there are other features worth adding, feel free to and let the group chat know!

# NOTE: This data-set only includes customer that made a transaction in the year 1998. I reccommend keeping the data state-specific,
# otherwise the data-set would be too massive. It is also good to keep things state-specific as it eliminates the chance of different
# shopping behaviours across different states from skewing the data.

library(tidyverse)

data <- read_csv("transaction_data.csv")


data$transaction_date <- as.Date(data$transaction_date, "%m/%d/%Y")

data2 <- mutate(data,
                month = month(transaction_date))

state_specific_data <- filter(data2, customer_state_province == "CA")

nbr_rows <- nrow(test_data)

unique_cust_ids <- unique(test_data[, 2])
unique_cust_ids_vec <- pull(unique_cust_ids, 1)

cust_ids <- c()

for (i in 1:length(unique_cust_ids_vec)) {
  for (j in 1:12) {
    cust_ids <- append(cust_ids, unique_cust_ids_vec[i])
  }
}

join_onto <- tibble(month = rep(seq(1:12), length(unique_cust_ids_vec)),
                    customer_id = cust_ids)

data_to_join <- state_specific_data[, c(14, 2:13)]

joined_data <- join_onto %>%
  left_join(data_to_join, by = c("customer_id", "month"))


missing_ids <- !complete.cases(joined_data[, 3])

joined_data2 <- joined_data

joined_data2[missing_ids, 3] <- 0


# Creating data-set to observe customer-level monthly expenditures
# You can use "grouped_data" to observe trends between months, (eg. what months have the most total expenditure)?

grouped_data <- joined_data2 %>%
  group_by(customer_id, month) %>%
  summarise("monthly_expenditure" = sum(expenditure))

grouped_data <- mutate(grouped_data,
                       "visited_store" = ifelse(monthly_expenditure > 0, 1, 0))

# Creating data set with customer's average monthly spending, the standard deviation of the spending between the months,
# the number of times they visit monthly, and finally, joining customer-specific data (income, age, etc.)

# The standard deviation could be interesting to look into, however, it needs to somehow be scaled to make fair comparisons.

final_data <- grouped_data %>%
  group_by(customer_id) %>%
  summarise("avg_monthly_expenditure" = mean(monthly_expenditure),
            "sd_monthly_expenditure" = sd(monthly_expenditure),
            "num_monthly_visit" = sum(visited_store))

cust_info <- read_csv("customerinfo.csv")

cust_info2 <- filter(cust_info, customer_state_province == "CA")

# Joining the customer information. If there are more customer features that you are interested in looking at (occupation, account age, etc.),
# just import the CSV file with the relevant information, and then use inner_join by customer_id (similar to SQL). Other features also do not
# have to be customer specific, but make sure to use the appropriate join function (left_join, right_join) to account for the differences in size
# of the datasets.

finData <- inner_join(final_data, cust_info2, by = "customer_id")

finData <- finData[, -10]

write_csv(finData, "finData.csv")

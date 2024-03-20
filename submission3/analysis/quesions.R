
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, ggplot2, dplyr, lubridate, stringr, readxl, data.table, gdata)


# Load required libraries
library(dplyr)
library(ggplot2)

# Load the dataset
tax_data <- readRDS("data/output/TaxBurden_Data.rds")

 
tax_data <- tax_data %>% group_by(state) %>% arrange(state, Year) %>%
  mutate(price_cpi_2012 = cost_per_pack*(cpi_2012/index),
         total_tax_cpi_2012=tax_dollar*(cpi_2012/index),
         ln_tax_2012=log(total_tax_cpi_2012),
         ln_sales=log(sales_per_capita),
         ln_price_2012=log(price_cpi_2012))
 
#Question 1
# Filter data for years 1970 to 1985
tax_data_filtered <- tax_data %>%
  filter(Year >= 1970 & Year <= 1985)

# Create the tax_change column
tax_data_filtered <- tax_data_filtered %>%
  arrange(state, Year) %>%
  group_by(state) %>%
  mutate(tax_change = ifelse(tax_state - lag(tax_state) > 0, 1, 0))

# Calculate the proportion of states with a change in their cigarette tax for each year
proportions <- tax_data_filtered %>%
  group_by(Year) %>%
  summarise(Proportion = mean(tax_change, na.rm = TRUE), .groups = 'drop')

# Create a bar graph of the proportions
question1_graph <- ggplot(proportions, aes(x = Year, y = Proportion)) +
  geom_bar(stat = "identity") +
  labs(title = "Proportion of States with a Change in Tax (1970-1985)",
       x = "Year", y = "Proportion") +
  theme_minimal()

question1_graph


#Question 2: 

library(ggplot2)

# Assuming your tax_data dataset has columns like "Year", "tax_dollar", and "cost_per_pack"
# Adjust column names accordingly if needed

# Calculate average tax and average price per year
average_tax <- tax_data %>%
  group_by(Year) %>%
  summarize(avg_tax = mean(total_tax_cpi_2012, na.rm = TRUE))

average_price <- tax_data %>%
  group_by(Year) %>%
  summarize(avg_price = mean(price_cpi_2012, na.rm = TRUE))

# Merge the two datasets based on the "Year" column
merged_data <- merge(average_tax, average_price, by = "Year", all = TRUE)


# Plotting and storing as an object
question2_graph <- ggplot(merged_data, aes(x = Year)) +
  geom_line(aes(y = avg_tax, color = "Average Tax"), size = 1.5) +
  geom_line(aes(y = avg_price, color = "Average Price"), size = 1.5) +
  labs(title = "Average Tax and Price of Cigarettes (1970-2018)",
       x = "Year",
       y = "Average Amount (in 2012 dollars)",
       color = "Legend") +
  scale_color_manual(values = c("Average Tax" = "blue", "Average Price" = "red")) +
  theme_minimal()

question2_graph

#Question 3

library(dplyr)
library(ggplot2)

# Calculate the price increase for each state
price_increase <- tax_data %>%
  group_by(state) %>%
  summarize(price_increase = last(price_cpi_2012) - first(price_cpi_2012))

# Identify the 5 states with the highest price increases
top_states <- price_increase %>%
  top_n(5, wt = price_increase)

# Filter the data for the top 5 states
top_states_data <- tax_data %>%
  filter(state %in% top_states$state)

# Calculate the average number of packs sold per capita for each year
avg_packs_per_capita <- top_states_data %>%
  group_by(Year) %>%
  summarize(avg_packs_per_capita = mean(sales_per_capita, na.rm = TRUE))

# Plotting and storing as an object
question3_graph <- ggplot(avg_packs_per_capita, aes(x = Year, y = avg_packs_per_capita)) +
  geom_line(size = 1.5) +
  labs(title = "Average of Top 5 States with Highest Price Increases",
       x = "Year",
       y = "Average Packs Sold Per Capita",
       color = "State") +
  theme_minimal()

question3_graph


#Question 4 
library(dplyr)
library(ggplot2)


# Calculate the price increase for each state
price_increase <- tax_data %>%
  group_by(state) %>%
  summarize(price_increase = last(price_cpi_2012) - first(price_cpi_2012))

# Identify the 5 states with the lowest price increases
bottom_states <- price_increase %>%
  top_n(5, wt = -price_increase)  # Using negative weight to select the smallest increases

# Filter the data for the bottom 5 states
bottom_states_data <- tax_data %>%
  filter(state %in% bottom_states$state)

# Calculate the average number of packs sold per capita for each year
avg_packs_per_capita <- bottom_states_data %>%
  group_by(Year) %>%
  summarize(avg_packs_per_capita = mean(sales_per_capita, na.rm = TRUE))

# Plotting and storing as an object
question4_graph <- ggplot(avg_packs_per_capita, aes(x = Year, y = avg_packs_per_capita)) +
  geom_line(size = 1.5) +
  labs(title = "Average of Top 5 States with Lowest Price Increases",
       x = "Year",
       y = "Average Packs Sold Per Capita",
       color = "State") +
  theme_minimal()

question4_graph

#Question 6 

# Filter data for the time period from 1970 to 1990
data_subset <- tax_data %>%
  filter(Year >= 1970 & Year <= 1990)

#tax_data <- tax_data %>% group_by(state) %>% arrange(state, Year) %>%
  #mutate(price_cpi_2012 = cost_per_pack*(cpi_2012/index),
         #total_tax_cpi_2012=tax_dollar*(cpi_2012/index),
         #ln_tax_2012=log(total_tax_cpi_2012),
         #ln_sales=log(sales_per_capita),
         #ln_price_2012=log(price_cpi_2012))

model_1 <- lm(ln_sales ~ ln_price_2012, data=data_subset)
summary(model_1)

# Tidy the regression summary for model_1
tidy_model_1 <- broom::tidy(model_1)

# Store the coefficients and their standard errors for model_1
coefficients_model_1 <- tidy_model_1$estimate
std_errors_model_1 <- tidy_model_1$std.error
beta_labels <- c("Intercept", "Log Price")

# Combine coefficients and standard errors into a data frame for model_1
regression_table <- data.frame(Label = beta_labels, Coefficient = coefficients_model_1, `Standard Error` = std_errors_model_1)

# Print the regression table for model_1
print(regression_table)


#tax_data <- tax_data %>% group_by(state) %>% arrange(state, Year) %>%
  #mutate(price_cpi_2012 = cost_per_pack*(cpi_2012/index),
         #total_tax_cpi_2012=tax_dollar*(cpi_2012/index),
         #ln_tax_2012=log(total_tax_cpi_2012),
         #ln_sales=log(sales_per_capita),
         #ln_price_2012=log(price_cpi_2012))

#Question 7 
# Install and load the AER package if not already installed

install.packages("AER")
library(AER)
install.packages("fixest")

# Filter data for the time period from 1970 to 1990
data_subset <- tax_data %>%
  filter(Year >= 1970 & Year <= 1990)

library(fixest)

# Estimate the IV model
iv_model <- feols(ln_sales ~ 1 | ln_price_2012 ~ ln_tax_2012, data = data_subset)


# Tidy the regression summary for model2
tidy_model_iv <- broom::tidy(iv_model)

# Store the coefficients and their standard errors for model2
coefficients_model_iv <- tidy_model_iv$estimate
std_errors_model_iv <- tidy_model_iv$std.error
beta_labels_iv <- c("Intercept", "Log Price")


# Combine coefficients and standard errors into a data frame for model2
iv_results <- data.frame(Label = beta_labels_iv, Coefficient = coefficients_model_iv, `Standard Error` = std_errors_model_iv)

# Print the regression table for model2
print(iv_results)


#Question 8 

# Filter data for the time period from 1970 to 1990
data_subset <- tax_data %>%
  filter(Year >= 1970 & Year <= 1990)

#tax_data <- tax_data %>% group_by(state) %>% arrange(state, Year) %>%
  #mutate(price_cpi_2012 = cost_per_pack*(cpi_2012/index),
         #total_tax_cpi_2012=tax_dollar*(cpi_2012/index),
         #ln_tax_2012=log(total_tax_cpi_2012),
         #ln_sales=log(sales_per_capita),
         #ln_price_2012=log(price_cpi_2012))


library(broom)

# First stage regression
first_stage <- lm(ln_price_2012 ~ ln_tax_2012, data = data_subset)
first_stage_summary <- tidy(first_stage)

# Reduced-form regression
reduced_form <- lm(ln_sales ~ ln_tax_2012, data = data_subset)
reduced_form_summary <- tidy(reduced_form)

# Combine the summaries into a single data frame
regression_summary <- rbind(
  c("First Stage", NA, NA),
  c("Intercept", first_stage_summary$estimate[1], first_stage_summary$std.error[1]),
  c("Log Tax", first_stage_summary$estimate[2], first_stage_summary$std.error[2]),
  c("Reduced Form", NA, NA),
  c("Intercept", reduced_form_summary$estimate[1], reduced_form_summary$std.error[1]),
  c("Log Tax", reduced_form_summary$estimate[2], reduced_form_summary$std.error[2])
)

# Convert to data frame
regression_summary8 <- as.data.frame(regression_summary)

# Rename columns
names(regression_summary8) <- c("Regression/Variable", "Coefficient", "Standard Error")

# Print the regression summary
print(regression_summary8)



#Question 9
##Repeat question 6

# Filter data for the time period from 1991 to 2015
data_subset_2 <- tax_data %>%
  filter(Year >= 1991 & Year <= 2015)

data_subset_2 <- na.omit(data_subset_2)


model_1_2 <- lm(ln_sales ~ ln_price_2012, data=data_subset_2)
summary(model_1_2)

# Tidy the regression summary for model_1
tidy_model_1_2 <- broom::tidy(model_1_2)

# Store the coefficients and their standard errors for model_1
coefficients_model_1_2 <- tidy_model_1_2$estimate
std_errors_model_1_2 <- tidy_model_1_2$std.error
beta_labels2 <- c("Intercept", "Log Price")

# Combine coefficients and standard errors into a data frame for model_1
regression_table_2 <- data.frame(Label = beta_labels2, Coefficient = coefficients_model_1_2, `Standard Error` = std_errors_model_1_2)

# Print the regression table for model_1
print(regression_table_2)


##Repeat Quesion 7
# Filter data for the time period from 1991 to 2015
data_subset_2 <- tax_data %>%
  filter(Year >= 1991 & Year <= 2015)

data_subset_2 <- na.omit(data_subset_2)


library(fixest)

# Estimate the IV model
iv_model_2 <- feols(ln_sales ~ 1 | ln_price_2012 ~ ln_tax_2012, data = data_subset_2)

summary(iv_model_2)



# Tidy the regression summary for model2
tidy_model_iv2 <- broom::tidy(iv_model_2)

# Store the coefficients and their standard errors for model2
coefficients_model_iv2 <- tidy_model_iv2$estimate
std_errors_model_iv2 <- tidy_model_iv2$std.error
beta_labels_iv2 <- c("Intercept", "Log Price")


# Combine coefficients and standard errors into a data frame for model2
iv_results_2 <- data.frame(Label = beta_labels_iv2, Coefficient = coefficients_model_iv2, `Standard Error` = std_errors_model_iv2)

# Print the regression table for model2
print(iv_results_2) 


##Repeat Question 8
# Filter data for the time period from 1991 to 2015
data_subset_2 <- tax_data %>%
  filter(Year >= 1991 & Year <= 2015)



# First stage regression
first_stage2 <- lm(ln_price_2012 ~ ln_tax_2012, data = data_subset_2)
first_stage_summary2 <- tidy(first_stage2)

# Reduced-form regression
reduced_form2 <- lm(ln_sales ~ ln_tax_2012, data = data_subset_2)
reduced_form_summary2 <- tidy(reduced_form2)


# Combine the summaries into a single data frame
regression_summary2 <- rbind(
  c("First Stage", NA, NA),
  c("Intercept", first_stage_summary2$estimate[1], first_stage_summary2$std.error[1]),
  c("Log Tax", first_stage_summary2$estimate[2], first_stage_summary2$std.error[2]),
  c("Reduced Form", NA, NA),
  c("Intercept", reduced_form_summary2$estimate[1], reduced_form_summary2$std.error[1]),
  c("Log Tax", reduced_form_summary2$estimate[2], reduced_form_summary2$std.error[2])
)


# Convert to data frame
regression_summary8_2 <- as.data.frame(regression_summary2)

# Rename columns
names(regression_summary8_2) <- c("Regression/Variable", "Coefficient", "Standard Error")

# Print the regression summary
print(regression_summary8_2)


rm(list=c("tax_data", "data_subset", "data_subset_2", "merged_data","tax_data_filtered", "data_subset_2", "top_states", "top_states_data"))
save.image("submission3/Hw3_workspace.Rdata")

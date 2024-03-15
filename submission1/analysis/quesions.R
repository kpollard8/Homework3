
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

# Determine which states had a change in cigarette tax each year
tax_changes <- tax_data_filtered %>%
  group_by(Year) %>%
  summarize(Change = n_distinct(state[tax_state > lag(tax_state)]))

# Calculate the proportion of states with a tax change in each year
tax_changes <- tax_changes %>%
  mutate(Proportion = Change / n_distinct(tax_data_filtered$state))


# Plotting and storing as an object
question1_graph <- ggplot(tax_changes, aes(x = as.factor(Year), y = Proportion)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Proportion of States with a Change in Cigarette Tax (1970-1985)",
       x = "Year",
       y = "Proportion") +
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
  geom_line(aes(y = avg_price, color = "Average Price"), linetype = "dashed", size = 1.5) +
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
  labs(title = "Average Packs Sold Per Capita (Top 5 States with Highest Price Increases)",
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
  labs(title = "Average Packs Sold Per Capita (Top 5 States with Lowest Price Increases)",
       x = "Year",
       y = "Average Packs Sold Per Capita",
       color = "State") +
  theme_minimal()

question4_graph

#Question 6 

# Filter data for the time period from 1970 to 1990
data_subset <- tax_data %>%
  filter(Year >= 1970 & Year <= 1990)

# Log-transform sales_per_capita and cost_per_pack
data_subset$log_sales <- log(data_subset$sales_per_capita)
data_subset$log_prices <- log(data_subset$cost_per_pack)

# Perform log-log regression
elasticity_model <- lm(log_sales ~ log_prices, data = data_subset)

# Display regression summary
summary(elasticity_model)

#Question 7 
# Install and load the AER package if not already installed

install.packages("AER")
library(AER)


# Filter data for the time period from 1970 to 1990
data_subset <- tax_data %>%
  filter(Year >= 1970 & Year <= 1990)

# Log-transform sales, prices, and tax_dollar
data_subset$log_sales <- log(data_subset$sales_per_capita)
data_subset$log_prices <- log(data_subset$cost_per_pack)
data_subset$ln_tax_2012 <- log(data_subset$tax_dollar)

# Perform instrumental variable regression
iv_model <- ivreg(log_sales ~ log_prices | ln_tax_2012, data = data_subset)

# Display instrumental variable regression summary
summary(iv_model)


#Question 8 

# Filter data for the time period from 1970 to 1990
data_subset <- tax_data %>%
  filter(Year >= 1970 & Year <= 1990)

# Log-transform sales, prices, and tax_dollar
data_subset$log_sales <- log(data_subset$sales_per_capita)
data_subset$log_prices <- log(data_subset$cost_per_pack)
data_subset$ln_tax_2012 <- log(data_subset$tax_dollar)

# Perform instrumental variable regression
iv_model <- ivreg(log_sales ~ log_prices | ln_tax_2012, data = data_subset)

# Extract first stage and reduced-form results
first_stage_results <- coef(iv_model)[c("log_prices", "log_tax_dollar")]
reduced_form_results <- coef(iv_model)[c("(Intercept)", "log_prices")]

first_stage_results
reduced_form_results
# Display results
cat("First Stage Results:\n")
print(first_stage_results)

cat("\nReduced-Form Results:\n")
print(reduced_form_results)


#Question 9
##Repeat question 6

# Filter data for the time period from 1991 to 2015
data_subset_2 <- tax_data %>%
  filter(Year >= 1991 & Year <= 2015)

data_subset_2 <- na.omit(data_subset)

# Log-transform sales_per_capita and cost_per_pack
data_subset_2$log_sales <- log(data_subset_2$sales_per_capita)
data_subset_2$log_prices <- log(data_subset_2$cost_per_pack)

# Perform log-log regression
elasticity_model2 <- lm(log_sales ~ log_prices, data = data_subset_2)

# Display regression summary
summary(elasticity_model2)


##Repeat Quesion 7
# Filter data for the time period from 1991 to 2015
data_subset_2 <- tax_data %>%
  filter(Year >= 1991 & Year <= 2015)

data_subset_2 <- na.omit(data_subset)

# Log-transform sales, prices, and tax_dollar
data_subset_2$log_sales <- log(data_subset_2$sales_per_capita)
data_subset_2$log_prices <- log(data_subset_2$cost_per_pack)
data_subset_2$ln_tax_2012 <- log(data_subset_2$tax_dollar)

# Perform instrumental variable regression
iv_model_2 <- ivreg(log_sales ~ log_prices | ln_tax_2012, data = data_subset_2)

# Display instrumental variable regression summary
summary(iv_model_2)

# Make into a table
# Extract coefficient estimates from the model summary
coefficients2 <- coef(summary(iv_model_2))

# Display coefficient estimates in a table format
print(coefficients2)

##Repeat Question 8
# Filter data for the time period from 1991 to 2015
data_subset_2 <- tax_data %>%
  filter(Year >= 1991 & Year <= 2015)

data_subset_2 <- na.omit(data_subset)

# Log-transform sales, prices, and tax_dollar
data_subset_2$log_sales <- log(data_subset_2$sales_per_capita)
data_subset_2$log_prices <- log(data_subset_2$cost_per_pack)
data_subset_2$ln_tax_2012 <- log(data_subset_2$tax_dollar)

# Perform instrumental variable regression
iv_model_2 <- ivreg(log_sales ~ log_prices | ln_tax_2012, data = data_subset_2)

# Extract first stage and reduced-form results
first_stage_results2 <- coef(iv_model_2)[c("log_prices", "log_tax_dollar")]
reduced_form_results2 <- coef(iv_model_2)[c("(Intercept)", "log_prices")]

# Display results
print(first_stage_results2)
#log_prices is -0.7626495  

print(reduced_form_results2)
#intercept is   5.1575124 and log_prices is -0.7626495 

rm(list=c("tax_data", "data_subset", "merged_data","tax_data_filtered", "data_subset_2", "top_states", "top_states_dates"))
save.image("submission1/Hw3_workspace.Rdata")

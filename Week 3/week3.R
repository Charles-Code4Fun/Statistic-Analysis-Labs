################################################################################
# Week 3 Lab Reference Code
# Charles Z. Liu
################################################################################

# Load library
library(ggplot2)
library(dplyr)

# Read Data
customer_data <- read.csv("customer_data.csv", stringsAsFactors = FALSE)
str(customer_data)

# ggplot scatter
ggplot(customer_data, aes(x = Age, y = Salary)) +
  geom_point() +
  theme_minimal()


# Histogram
ggplot(customer_data, aes(x = Age)) +
  geom_histogram(binwidth = 5, 
                 fill = "skyblue", 
                 color = "black") +
  labs(title = "Age Distribution",
       x = "Age",
       y = "Count") +
  theme_minimal()



# Churn Histogram Analysis
ggplot(customer_data, aes(x = Churn)) +
  geom_bar(fill = "salmon") +
  labs(title = "Churn Count",
       x = "Churn (0=Retained,1=Churned)",
       y = "Count") +
  theme_minimal()

ggplot(customer_data, aes(x = factor(Churn))) +
  geom_bar(fill = "salmon") +
  labs(title = "Churn Count",
       x = "Churn (0=Retained,1=Churned)",
       y = "Count") +
  theme_minimal()

ggplot(customer_data, aes(x = factor(Churn))) +
  geom_bar(aes(y = ..prop.., group = 1),
           fill = "steelblue") +
  ylab("Proportion") +
  theme_minimal()


# Discuss the difference
ggplot(customer_data, aes(x = factor(Churn), y = Salary)) +
  geom_boxplot(fill = "lightgreen") +
  labs(title = "Salary Distribution by Churn",
       x = "Churn Status",
       y = "Salary") +
  theme_minimal()

# Computing
mean(customer_data$Salary)
median(customer_data$Salary)


# Relationship between Churn and service volume
ggplot(customer_data, aes(x = factor(E_IND), fill = factor(Churn))) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Churn by Number of Services",
       x = "Number of Services",
       y = "Proportion",
       fill = "Churn") +
  theme_minimal()


# Compute the AverageSalary
library(dplyr)
customer_data %>%
  group_by(Churn) %>%
  summarise(AverageSalary = mean(Salary, na.rm = TRUE))

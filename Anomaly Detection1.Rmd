# 1. Load the Data
# Display the first 6 rows of the dataset

# path = (C:/Users/USER-PC/Downloads/Supermarket_Sales_Forecasting - Sales.csv)

sales = read.csv('C:/Users/USER-PC/Downloads/Supermarket_Sales_Forecasting - Sales.csv')
head(sales) 

# 2. Data Exploration
# Check the dimensions of the dataset
dim(sales)

# Check descriptive statistics of the dataframe
summary(sales)

# Check the datatypes of all the variables in the dataframe
sapply(sales, class)

# Count the unique values in the variables
length(unique(sales$Date))
length(unique(sales$Sales))

# 3. Data Cleaning
# A. Check for missing values
length(which(is.na(sales)))
# There are no missing values

# B. Check for duplicates
duplicated_rows <- sales[duplicated(sales),]
duplicated_rows
# There are no duplicated rows

# C. Change the datatype of Date from character to date and have the date in the format yyyymmdd

# This puts the date in the correct format
sales <- transform(sales, Date = format(as.Date(Date, '%m/%d/%Y'), '%Y/%m/%d'))

# Check whether the datatype has changes
sapply(sales, class)

# Change the Date datatype from character to numeric
sales <- transform(sales, Date = as.Date(Date))

# Confrim that it is in the correct datatype
sapply(sales, class)

# View the first 6 rows of the dataset
head(sales)


# D. Check for outliers by plotting boxplots for the numerical variables.

boxplot(sales$Date, 
        data = sales,
        main="Boxplot for Date",
        col="orange",
        border="brown"
)

boxplot(sales$Sales, 
        data = sales,
        main="Boxplot for Sales",
        col="orange",
        border="brown"
)

# The sales variable has a few outliers. I'll not remove them as they'll be useful in anomaly detection.

# 4. Exploratory Data Analysis
# A. Univariate Analysis: Numerical Variables

# Histograms

Sales <- sales$Sales
hist(Sales)

# B. Univariate Analysis: Categorical Variables

# Bar charts

date <- sales$Date

date_frequency <- table(date)

barplot(date_frequency, col="blue",
        main="Dates Chart",border="red")

# C. Bivariate Analysis: Categorical Variable and Numerical variable

# Line Graph

library(ggplot2)
ggplot(sales, aes(x=Date, y=Sales, color=Sales)) + 
  geom_line()


# 5. Implementing the Solution: Anomaly Detection

# Ordering the dataset by Date

sales = sales %>% arrange(Date)
head(sales)

# Get the average sales per day since there are many records for each date.

sales = aggregate(Sales ~ Date, sales, mean)
head(sales)

# Converting data frame to a tibble time (tbl_time)

library(tibbletime)
sales = tbl_time(sales, Date)
class(sales)

# Check the dimensions of the reduced data set

dim(sales)


# A. Using Anomalize Method

# Load the required packages

library(anomalize)
library(dplyr)
library(tibble)

# Decompose data using time_decompose() function in anomalize package.
# We will use stl method which extracts seasonality.

sales_ts = sales %>%
  as_tibble()
sales_ts %>%
  time_decompose(Sales, method = "stl",  frequency = "auto", trend = "auto") %>%  
  anomalize(remainder, method = "gesd", alpha = 0.05, max_anoms = 0.1) %>% 
  plot_anomaly_decomposition()


# Plot the data again by recomposing data

sales_ts %>% 
  time_decompose(Sales) %>% 
  anomalize(remainder) %>% 
  time_recompose() %>%  
  plot_anomalies(time_recomposed = TRUE, ncol = 3, alpha_dots = 0.5)


# Extract the anomalies

anomalizesales = sales_ts %>% 
  time_decompose(Sales) %>%  
  anomalize(remainder) %>%  
  time_recompose() %>%  
  filter(anomaly == 'Yes')

anomalizesales

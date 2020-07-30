# 1. Load the Data
# Display the first 6 rows of the dataset
supermarket <- read.csv('http://bit.ly/CarreFourDataset')
head(supermarket)

# 2. Data Exploration
# Check the dimensions of the dataset
dim(supermarket)

# Display the structure of the R object
str(supermarket)

# Check descriptive statistics of the dataframe
summary(supermarket)

# Check the datatypes of all the variables in the dataframe
sapply(supermarket, class)

# Count the unique values in the variables
length(unique(supermarket$Invoice.ID))
length(unique(supermarket$Branch))
length(unique(supermarket$Customer.type))
length(unique(supermarket$Gender))
length(unique(supermarket$Product.line))
length(unique(supermarket$Unit.price))
length(unique(supermarket$Quantity))
length(unique(supermarket$Tax))
length(unique(supermarket$Date))
length(unique(supermarket$Time))
length(unique(supermarket$Payment))
length(unique(supermarket$cogs))
length(unique(supermarket$gross.margin.percentage))
length(unique(supermarket$gross.income))
length(unique(supermarket$Rating))
length(unique(supermarket$Total))
# The variables Payment, Product line, Gender, Customer Type and Branch are discrete variables.

# 3. Data Cleaning
# A. Drop unnecessary columns
# I'll drop the gross income column since it's the same as the tax column and the 
# invoice ID column since it is of no significance in this dataset.
# I'll also drop the Year and Gross Margin Percentage columns as they only have one constant value.

supermarket1= subset(supermarket, select = -c(Invoice.ID, gross.income, gross.margin.percentage) )
head(supermarket1)

# B. Check for missing values
length(which(is.na(supermarket1)))
# There are no missing values

# C. Check for duplicates
duplicated_rows <- supermarket1[duplicated(supermarket1),]
duplicated_rows
# There are no duplicated rows


# D. Split columns

library(tidyr)
library(lubridate)

supermarket2 <- separate(supermarket1, Date, c("Month", "Day", "Year"))
head(supermarket2)

supermarket3 <- separate(supermarket2, Time, c("Hour", "Minute"))
head(supermarket3)

sapply(supermarket3, class)

# E. Change datatypes of Month, Day, Year, Hour and Minute into numeric

supermarket3 <- transform(supermarket3, Month = as.numeric(Month), 
                          Day = as.numeric(Day), Year = as.numeric(Year), Hour = as.numeric(Hour),
                          Minute = as.numeric(Minute))

sapply(supermarket3, class)

# Drop the Year column as well since it has a constant value

supermarket3= subset(supermarket3, select = -c(Year) )
head(supermarket3)


# F. Check for outliers. Plot boxplots for the numerical variables.

boxplot(supermarket3$Unit.price, 
        data = supermarket3,
        main="Boxplot for Unit Price",
        col="orange",
        border="brown"
)

boxplot(supermarket3$Quantity, 
        data = supermarket3,
        main="Boxplot for Quantity",
        col="orange",
        border="brown"
)

boxplot(supermarket3$Tax, 
        data = supermarket3,
        main="Boxplot for Tax",
        col="orange",
        border="brown"
)


boxplot(supermarket3$cogs, 
        data = supermarket3,
        main="Boxplot for cogs",
        col="orange",
        border="brown"
)


boxplot(supermarket3$Rating, 
        data = supermarket3,
        main="Boxplot for Rating",
        col="orange",
        border="brown"
)

boxplot(supermarket3$Total, 
        data = supermarket3,
        main="Boxplot for Total",
        col="orange",
        border="brown"
)
# From the boxplots, the total, cogs and tax variables have outliers. 
# It is normal to have goods that cost higher than others, and this in turn affects the gross income, the total and the tax.
# I won't be removing the outliers in this dataset.

# 4. Univariate Analysis
# A. Numerical variables
# Histograms

Unit_Price <- supermarket3$Unit.price
hist(Unit_Price)

Quantity <- supermarket3$Quantity
hist(Quantity)

Tax <- supermarket3$Tax
hist(Tax)

Cost_of_Goods_Sold <- supermarket3$cogs
hist(Cost_of_Goods_Sold)


Rating <- supermarket3$Rating
hist(Rating)

Total <- supermarket3$Total
hist(Total)

# B. Categorical Variables
# Bar charts

Branch <- supermarket3$Branch

branch_frequency <- table(Branch)

barplot(branch_frequency, col="blue",
        main="Branches Chart",border="red")


Customer_type <- supermarket3$Customer.type

customer_frequency <- table(Customer_type)

barplot(customer_frequency, col="blue",
        main="Customer Type Chart",border="red")


gender <- supermarket3$Gender

gender_frequency <- table(gender)

barplot(gender_frequency, col="blue",
        main="Gender Chart",border="red")


Product <- supermarket3$Product.line

product_frequency <- table(Product)

barplot(product_frequency, col="blue",
        main="Product Line Chart",border="red")


month <- supermarket3$Month

month_frequency <- table(month)

barplot(month_frequency, col="blue",
        main="Months Chart",border="red")



day <- supermarket3$Day

day_frequency <- table(day)

barplot(day_frequency, col="blue",
        main="Days Chart",border="red")


hour <- supermarket3$Hour

hour_frequency <- table(hour)

barplot(hour_frequency, col="blue",
        main="Hours Chart",border="red")


min <- supermarket3$Minute

min_frequency <- table(min)

barplot(min_frequency, col="blue",
        main="Minutes Chart",border="red")


payment <- supermarket3$Payment

payment_frequency <- table(payment)

barplot(payment_frequency, col="blue",
        main="Payment Method Chart",border="red")



# 5. Bivariate Analysis
# A. Categorical and categorical variables
# Double Bar Charts

counts <- table(supermarket3$Branch, supermarket3$Product.line)
barplot(counts, main="Product Line by Branch",
        xlab="Total", col=c("darkblue","red", "pink"),
        legend = rownames(counts), beside=TRUE)

counts <- table(supermarket3$Branch, supermarket3$Customer.type)
barplot(counts, main="Customer Type by Branch",
        xlab="Total", col=c("darkblue","red", "pink"),
        legend = rownames(counts), beside=TRUE)

counts <- table(supermarket3$Customer.type, supermarket3$Payment)
barplot(counts, main="Customer Type and Payment",
        xlab="Total", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)

counts <- table(supermarket3$Gender, supermarket3$Product.line)
barplot(counts, main="Gender and Product Line",
        xlab="Total", col=c("darkblue","red"),
        legend = rownames(counts), beside=TRUE)


# Encode the categorical variables to be numerical so we can check for correlation

encode_ordinal <- function(x, order = unique(x)) {
  x <- as.numeric(factor(x, levels = order))
  x
}


table(supermarket3[["Branch"]], encode_ordinal(supermarket3[["Branch"]]), useNA = "ifany")

table(supermarket3[["Customer.type"]], encode_ordinal(supermarket3[["Customer.type"]]), useNA = "ifany")

table(supermarket3[["Gender"]], encode_ordinal(supermarket3[["Gender"]]), useNA = "ifany")

table(supermarket3[["Product.line"]], encode_ordinal(supermarket3[["Product.line"]]), useNA = "ifany")

table(supermarket3[["Payment"]], encode_ordinal(supermarket3[["Payment"]]), useNA = "ifany")

supermarket4 <- supermarket3
supermarket4[["Branch_encoded"]] <- encode_ordinal(supermarket3[["Branch"]])
supermarket4[["Customer.type_encoded"]] <- encode_ordinal(supermarket3[["Customer.type"]])
supermarket4[["Gender_encoded"]] <- encode_ordinal(supermarket3[["Gender"]])
supermarket4[["Product.line_encoded"]] <- encode_ordinal(supermarket3[["Product.line"]])
supermarket4[["Payment_encoded"]] <- encode_ordinal(supermarket3[["Payment"]])
head(supermarket4)

# Drop the categorical columns

df= subset(supermarket4, select = -c(Branch, Customer.type, Gender, Product.line, Payment) )
head(df)

# Confirm the datatypes of the encoded columns

sapply(df, class)

# B. Numerical and numerical variables

# Correlation matrix

matrix <- cor(df)
round(matrix, 2)


library(corrplot)

# Run the corrplot function

df.cor = cor(df, method = c("spearman"))
corrplot(df.cor)



# 6. Feature Selection

# A. Filter Methods

# Install and load the caret package

suppressWarnings(
  suppressMessages(if
                   (!require(caret, quietly=TRUE))
    install.packages("caret")))
library(caret)


# Find the correlation matrix

correlationMatrix <- cor(df)
correlationMatrix

# Find attributes that are highly correlated

highlyCorrelated <- findCorrelation(correlationMatrix, cutoff=0.70)

highlyCorrelated

names(df[,highlyCorrelated])

# Removing Redundant Features 
 
df2<-df[-highlyCorrelated]


# Perform a graphical comparison between the original dataframe and the one without
# redundant features.

corrplot(correlationMatrix, order = "hclust")
corrplot(cor(df2), order = "hclust")

# The variables tax and cogs are highly correlated to total.
# Drop the tax and cogs column as their sum makes up the total column

df3 = subset(df, select = -c(Tax, cogs)) 
head(df3)

# Find the correlation matrix

correlationMatrix1 <- cor(df3)
correlationMatrix1

corrplot(correlationMatrix1, order = "hclust")

# From the correlation matrix, we can see that unit price and quantity are
# still highly correlated to Total.
# Drop unit price and quantity features.

df4 = subset(df, select = -c(Unit.price, Quantity, Tax, cogs)) 
head(df4)

# Find the correlation matrix

correlationMatrix2 <- cor(df4)
correlationMatrix2

corrplot(correlationMatrix2, order = "hclust")

# The correlation plot generated does not have any features that are highly 
# correlated.

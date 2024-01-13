
```{r}
# Loading necessary libraries
install.packages("caret")
install.packages("knitr")
install.packages("car")
install.packages("corrplot")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("tidyr")
install.packages("Hmisc")
install.packages("psych")
install.packages("countrycode")
install.packages("randomForest")
install.packages("moments")
install.packages("RVAideMemoire")
install.packages("gplots")


library(gplots)
library(RVAideMemoire)
library(moments)
library(randomForest)
library(corrplot)
library(caret)
library(knitr)
library(car)
library(ggplot2)
library(dplyr)
library(tidyr)
library(Hmisc)
library(psych)
library(countrycode)
```

## **Loading the data set**

```{r}
options(warn = -1)
```

```{r}
# Reading the data set into a data frame

economic <- read.csv("economic-data.csv", sep = ",", header = TRUE)
head(economic)
```

```{r}
str(economic)
```

### **Data Preprocessing**

```{r}
# Renaming columns with shorter and more readable names

colnames(economic) <- c(
  "Country", "Year", "Inflation", "CPI", "GDP_Growth",
  "GDP", "Per_Capita", "Per_Capita_Growth", "Unemployment",
  "Population", "Labor_Force", "FDI_NetInflows", "FDI_Percentage",
  "Exports", "Imports", "Political_Stability", "Corruption_Control",
  "Government_Effectiveness", "Regulatory_Quality", "Rule_of_Law",    "Technology_Exports")


# Displaying the updated column names
colnames(economic)

```

```{r}
# Extracting the names of all columns except the first two
columns_to_convert <- names(economic)[-c(1, 2)]

# Converting the selected columns to numeric
economic[columns_to_convert] <- lapply(economic[columns_to_convert], as.numeric)

```

```{r}
# Identifying missing values

missing_values <- colSums(is.na(economic))
missing_values

```

```{r}
# Defining a function to replace missing values

replace_missing_values <- function(
    data, column_name, country_column, missing_values) 
{missing_row_index <- which(is.na(data[[column_name]]))

countries_with_missing <- data[[country_column]][missing_row_index]

for (country in unique(countries_with_missing)) {
  mean_value <- mean(data[[column_name]][data[[country_column]] == country], na.rm = TRUE)
  
  missing_in_country <- which(data[[country_column]] == country & is.na(data[[column_name]]))
  
  data[[column_name]][missing_in_country] <- mean_value}

return(data)
}
```

```{r}
# Handing missing values

economic <- replace_missing_values(economic,"Inflation", "Country", missing_values)

economic <- replace_missing_values(economic, "CPI", "Country", missing_values)

economic <- replace_missing_values(economic, "Unemployment", "Country", missing_values)
```

```{r}
head(economic)
```

```{r}
# Creating a new column called "continent"
economic$continent <- countrycode(economic$Country, origin = "country.name", destination = "continent")

# Moving the continent column to be the third in the data frame
economic <- economic[, c(1, 2, ncol(economic), 3:(ncol(economic) - 1))]

head(economic)


```

```{r}
# Visualizing continent distribution 
ggplot(economic, aes(x = continent)) +
  geom_bar() +
  labs(
    title = "Distribution of Continents",
    x = "Continent",
    y = "Count")
```

```{r}
# Checking to see number of countries in each continent
continent_counts <- economic %>%
  group_by(continent) %>%
  summarise(unique_country_count = n_distinct(Country))

print(continent_counts)

```

#### Checking for outliers

```{r}
# Creating box plots for various economic indicators

boxplot(economic$Per_Capita, main = "Per Capita", ylab = "Per Capita", col = "skyblue")

boxplot(economic$GDP, main = "Gross Domestic Product", ylab = "GDP", col = "lightgreen")

boxplot(economic$GDP_Growth, main = "Technological Exports", ylab = "Technology_Exports", col = "lightcoral")

boxplot(economic$Inflation, main = "Inflation", ylab = "Inflation", col = "gold")

boxplot(economic$Unemployment, main = "Unemployment", ylab = "Unemployment", col = "lightblue")

boxplot(economic$CPI, main = "Consumer Price Index", ylab = "CPI", col = "plum")

boxplot(economic$Population, main = "Political_Stability", ylab = "Population", col = "lightpink")

boxplot(economic$Labor_Force, main = "Labor Force", ylab = "Labor Force", col = "lightsteelblue")


```

## **Descriptive Statistical Analysis**

```{r}
describe(economic)
```

```{r}
# Creating a function to summarize and visualize statistics for each continent
visualizeStats <- function(data, variable_name) {
  descriptive_stats <- data %>%
    dplyr::group_by(continent) %>%
    dplyr::summarize(
      mean_value = mean(!!sym(variable_name), na.rm = TRUE),
      median_value = median(!!sym(variable_name), na.rm = TRUE),
      sd_value = sd(!!sym(variable_name), na.rm = TRUE),
      skewness_value = skewness(!!sym(variable_name), na.rm = TRUE))
  
  # Display the results
  print(descriptive_stats)
  
  # Visualizing the statistics
  bar_plot <- ggplot(descriptive_stats, aes(x = continent)) +
    geom_bar(aes(y = mean_value), fill = "skyblue", stat = "identity") +
    labs(title = paste("Mean", variable_name, "across continents"), x = "Continent", y = "Mean Value")
  
  sd_plot <- ggplot(descriptive_stats, aes(x = continent)) +
    geom_bar(aes(y = sd_value), fill = "coral", stat = "identity") +
    labs(title = paste("Standard Deviation of", variable_name, "Across Continents"), x = "Continent", y = "SD Value")
  
  density_plot <- ggplot(data, aes(x = .data[[variable_name]])) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "lightblue", color = "black") +
    geom_density(alpha = 0.5, fill = "orange") +
    geom_vline(aes(xintercept = mean(.data[[variable_name]])), color = "blue", linetype = "dashed", size = 1) +
    geom_vline(aes(xintercept = median(.data[[variable_name]])), color = "red", linetype = "dashed", size = 1) +
    labs(title = paste("Distribution of", variable_name, "with Skewness"),
         x = paste(variable_name, "(current US$)"),
         y = "Density")
  
  box_plot <- ggplot(data, aes(x = continent)) +
    geom_boxplot(aes(y = .data[[variable_name]])) +
    labs(title = "Distribution Across Continents", x = "Continent", y = "Value")
  
  print(bar_plot)
  print(sd_plot)
  print(density_plot)
  print(box_plot)
}
```

```{r}
# Initializing the function on GDP

visualizeStats(economic, "GDP")
```

```{r}
# Initializing the function on GDP_Growth

visualizeStats(economic, "GDP_Growth")
```

```{r}
visualizeStats(economic, "Inflation")
```

```{r}
visualizeStats(economic, "Unemployment")
```

```{r}
visualizeStats(economic, "Population")
```

```{r}
visualizeStats(economic, "Political_Stability")
```

```{r}
visualizeStats(economic, "Corruption_Control")
```

```{r}
visualizeStats(economic, "Government_Effectiveness")
```

### **Exploratory Data Analysis (EDA)**

```{r}
install.packages("gplots")
library(gplots)
```

```{r}
# Visualizing GDP across the years

ggplot(economic, aes(x = Year, y = GDP_Growth, fill = continent)) +
  geom_area() +
  labs(title = "Gross Domestic Product", x = "Year", y = "GDP (current US$)")

```

```{r}
# Visualizing Inflation levels

ggplot(economic, aes(x = Year, y = Inflation, fill = continent)) +
  geom_area() +
  labs(title = "Inflation Across Continents Over Years", x = "Year", y = "Inflation")

```

```{r}
# Visualizing Consumer Price Index

ggplot(economic, aes(x = Year, y = CPI, fill = continent)) +
  geom_area() +
  labs(title = "CPI Across Continents Over Years", x = "Year", y = "Inflation")

```

```{r}
# Visualizing continental unemployment levels

ggplot(economic, aes(x = Year, y = Unemployment, fill = continent)) +
  geom_area() +
  labs(title = "Unemployment", x = "Year", y = "Unemployment")

```

```{r}
ggplot(economic, aes(x=Country, y=Unemployment)) +
  geom_point(size=3) +
  geom_segment(aes(x=Country,
                   xend=Country,
                   y=0,
                   yend=Unemployment)) +
  labs(title="Unemployment Figures",
       subtitle="Youth Unemployment Rise for each country",
       caption="Youth Unemployment Rise") +
  theme(axis.text.x = element_text(angle=65, vjust=0.6))
```

```{r}
# Visualizing Population numbers

ggplot(economic, aes(x = Year, y = Population, fill = continent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Population",
       x = "Year",
       y = "Population")
```

```{r}
# High technology exports across continents

ggplot(economic, aes(x = Year, y = Technology_Exports, fill = continent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Technology Exports Across Continents Over Years",
       x = "Year",
       y = "Technology Export")

ggplot(economic, aes(x = Technology_Exports, y = Country, color = Year)) +
  geom_point() +
  labs(
    title = "Technology Exports Across Countries Over Years",
    x = "Technology Export (in Billions USD)",
    y = "Year"
  )
```

```{r}
# Foreign Direct Investment 

ggplot(economic, aes(x = Year, y = FDI_NetInflows, fill = continent)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Foreign Direct Investment (% of GDP)", x = "Year", y = "FDI")

```

```{r}
# Political Stability and Absence of Violence/Terrorism

ggplot(economic, aes(x = Year, y = `Political_Stability`, group = `continent`, color = `continent`)) +
  geom_boxplot() +
  labs(title = "Political Stability over time",
       x = "Year",
       y = "Political Stability")
```

### **Checking for Normal Distribution**

```{r}
# Function to create histograms and Q-Q plots

check_normality <- function(variable_name) {
  p1 <- ggplot(economic, aes(x = .data[[variable_name]])) +
    geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram for", variable_name),
         x = variable_name,
         y = "Frequency") +
    theme_minimal()
  
  p2 <- ggplot(economic, aes(x = .data[[variable_name]])) +
    geom_density(fill = "blue", alpha = 0.7) +
    labs(title = paste("Density Plot for", variable_name),
         x = variable_name,
         y = "Density") +
    theme_minimal()
  
  print(p1)
  print(p2)
}
```

```{r}
check_normality("CPI")
```

```{r}
check_normality("Inflation")
```

```{r}
check_normality("GDP_Growth")
```

```{r}
check_normality("Unemployment")
```

### **Shapiro--Wilk test**

```{r}

# Function to perform Shapiro-Wilk test

shapiro_test_all <- function(data) {
  cat("Shapiro-Wilk Test Results\n\n")
  
  for (variable_name in colnames(data)) {
    if (is.numeric(data[[variable_name]])) {
      shapiro_test_result <- shapiro.test(data[[variable_name]])
      cat("Variable:", variable_name, "\n")
      cat("  W =", shapiro_test_result$statistic, ", p-value =", shapiro_test_result$p.value, "\n")
      
      # Check if we should reject or accept the null hypothesis
      if (shapiro_test_result$p.value > 0.05) {
        cat("  Result: Do not reject the null hypothesis (data may be normally distributed)\n\n")
      } else {
        cat("  Result: Reject the null hypothesis (data may not be normally distributed)\n\n")
      }
    }
  }
}
```

```{r}
shapiro_test_all(economic)
```

**Observation:**
  
  The P-value of all the variables are less than the significance level 0.05, hence we proceed to **reject the Null Hypothesis** and conclude that the data does not follow a normal distribution.

This also means we will be using **Spearman Correlation Method.**
  
  ### **Conducting Transformations**
  
  To make the data set become normally distributed by applying **logarithmic** transformation

```{r}
economic_log <- economic

# Selecting numeric columns for transformation
numeric_columns <- sapply(economic_log, is.numeric)

# Applying logarithmic transformation to the numeric columns
economic_log[numeric_columns] <- lapply(economic_log[numeric_columns], function(x) log(x + 1))

# Checking the distributions after transformation
par(mfrow = c(2, 2))
for (col in names(economic_log[numeric_columns])) {
  hist(economic_log[[col]], main = col, col = "lightblue")
}
```

**Observation:**
  
  Using Log Transformation, it appears the data set is now as close as it gets to becoming normally distributed.

```{r}
shapiro.test(economic_log$Unemployment)
```

```{r}
# Identifying missing values

missing_values <- colSums(is.na(economic_log))
missing_values
```

```{r}
## Handing missing values

economic_log <- replace_missing_values(economic_log, "Inflation", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "GDP_Growth", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Per_Capita_Growth", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "FDI_NetInflows", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "FDI_Percentage", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Political_Stability", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Corruption_Control", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Government_Effectiveness", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Regulatory_Quality", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Rule_of_Law", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Rule_of_Law", "Country", missing_values)

```

```{r}
missing_values <- economic_log[is.na(economic_log$FDI_NetInflows) | is.na(economic_log$Political_Stability), c("Country", "FDI_NetInflows", "Political_Stability")]

# Print the result
print(missing_values)
```

## **Correlation Analysis**

```{r}
# Creating the Spearman correlation matrix
spearman_cor  <- cor(economic[, 4:ncol(economic)], method="spearman")

corrplot(spearman_cor , method = "color")
```

*Checking correlation of different indicator groups with GDP (all countries)*
  
  ```{r}

# Selecting inflation-related variables
inflation_var <- economic[, c("GDP", "GDP_Growth", "Per_Capita", "Per_Capita_Growth", "Inflation", "CPI")]

# Computing the correlation matrix
inflation_matrix <- cor(inflation_var, method="spearman")

corrplot(cor(inflation_var), method = "number", type = "upper")

```

```{r}

# Selecting labour-related variables
labour_var <- economic[, c("GDP", "GDP_Growth", "Per_Capita", "Per_Capita_Growth", "Unemployment", "Population", "Labor_Force")]

# Computing the correlation matrix
labour_matrix <- cor(labour_var, method="spearman")

corrplot(cor(labour_var), method = "number", type = "upper")

```
```{r}

# Selecting trade-related variables
trade_var <- economic[, c("GDP", "GDP_Growth", "Per_Capita", "Per_Capita_Growth", "Imports","Exports", "Technology_Exports")]

# Computing the correlation matrix
trade_matrix <- cor(trade_var, method="spearman")

corrplot(cor(trade_var), method = "number", type = "upper")

```

```{r}
# Selecting foreign investment variables
investment_var <- economic[, c("GDP", "GDP_Growth", "Per_Capita", "Per_Capita_Growth", "FDI_NetInflows", "FDI_Percentage")]

# Compute correlation matrix
investment_cor_matrix <- cor(investment_var, method="spearman")

corrplot(cor(investment_var), method = "number", type = "upper")

```

```{r}
# Selecting governance-related variables
govt_var <- economic[, c("GDP", "GDP_Growth", "Per_Capita", "Per_Capita_Growth", "Political_Stability",
                         "Corruption_Control", "Government_Effectiveness", "Regulatory_Quality", "Rule_of_Law")]

# Computing the correlation matrix
govt_matrix <- cor(govt_var, method="spearman")

corrplot(cor(govt_matrix), method = "number", type = "upper")

```


## **Hypothesis Testing**

### Hypothesis Testing with Correlation

**Hypothesis 1:** There is a positive correlation between inflation and unemployment.

```{r}

# Testing the correlation between Inflation and Unemployment 

cor_result <- cor.test(economic$Inflation, economic$Unemployment, method = "spearman", exact = FALSE)
print(cor_result)

# Decision Rule
if (cor_result$p.value < 0.05) 
{cat("Reject the null hypothesis. There is no evidence of a negative correlation.")} else {
  cat("Fail to reject the null hypothesis. There is enough evidence of a negative correlation.")}

```


**Hypothesis 2:** GDP growth is positively correlated with foreign direct investment.

```{r}
cor_result <- cor.test(economic$FDI_NetInflows, economic$GDP, method = "spearman", exact = FALSE)

print(cor_result)

# Decision Rule
if (cor_result$p.value < 0.05) 
{cat("Reject the null hypothesis. There is significant correlation.")} else {
  cat("Fail to reject the null hypothesis. There is no evidence of a correlation.")}

```

**Hypothesis 3:** Government effectiveness is positively correlated with regulatory quality.

```{r}
cor_result <- cor.test(economic$Government_Effectiveness, economic$Regulatory_Quality, 
                       method = "spearman", exact = FALSE)

print(cor_result)

# Decision Rule
if (cor_result$p.value < 0.05) 
{cat("Reject the null hypothesis. There is significant correlation.")} else {
  cat("Fail to reject the null hypothesis. There is no evidence of a correlation.")}
```

**Hypothesis 4:** GDP is positively correlated with exports of goods and services.

```{r}

cor_result <- cor.test(economic$GDP, economic$Exports, method = "spearman", exact = FALSE)

print(cor_result)

# Decision Rule
if (cor_result$p.value < 0.05) 
{cat("Reject the null hypothesis. There is significant correlation.")} else {
  cat("Fail to reject the null hypothesis. There is no evidence of a correlation.")}

```

### **Hypothesis Testing using ANOVA**

```{r}
# Randomly selecting a country from each continent

set.seed(300)

selected_countries <- economic_log %>%
  group_by(continent) %>%
  slice_sample(n = 2) %>%
  ungroup()

hypothesis <- economic[economic$Country %in% 
                         selected_countries$Country, ]

print(selected_countries)

```

#### Inflation

> Null Hypothesis (H0): There is no significant difference in the mean Inflation across continents.

> Alternative Hypothesis (H1): There is a significant difference in the mean Inflation across continents.

```{r}

# Checking for outlines
boxplot(Inflation ~ continent, data=hypothesis, xlab="Continents", 
        ylab="Inflation", main="Inflation Across continents")

```
```{r}
ggplot(hypothesis, aes(x = continent, y = Inflation, fill = continent)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Mean Inflation Across Continents",
       x = "Continent",
       y = "Mean inflation") +
  theme_minimal()
```

```{r}
# Checking for normal distribution
byf.shapiro(Inflation  ~ continent, data=hypothesis)
```

```{r}
# Checking for homogeneity of variances

bartlett.test(Inflation ~ Country, data=hypothesis)
```

```{r}
# Conducting a post-hoc with Welch

oneway.test(Inflation ~ Country,data=hypothesis, var.equal = FALSE)

```

```{r}
# Running a Post-Hoc Test

inflation_anova <- aov(Inflation ~ continent, data = hypothesis)
TukeyHSD(inflation_anova, which = "continent")

```

#### Gross Domestic Product

> Null Hypothesis (H0): There is no significant difference in the mean GDP across continents.
> Alternative Hypothesis (H1): There is a significant difference in the mean GDP across continents.

```{r}
# Checking for outliers

boxplot(GDP ~ continent, data=hypothesis,xlab="Continents", 
        ylab="GDP",main="GDP across continents")

```
```{r}
ggplot(hypothesis, aes(x = continent, y = GDP, fill = continent)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Mean GDP Across Continents",
       x = "Continent",
       y = "Mean GDP") +
  theme_minimal()
```

```{r}
# Checking for normal distribution
byf.shapiro(GDP ~ continent, data=hypothesis)
```

```{r}

# Checking for homogeneity of variances
bartlett.test(GDP ~ Country, data=hypothesis)

```


```{r}

# Conducting a post-hoc with Welch
oneway.test(GDP ~ Country,data=hypothesis, var.equal = FALSE)

```

```{r}
# Running a Post-Hoc Test

GDP_anova <- aov(GDP ~ continent, data = hypothesis)
TukeyHSD(GDP_anova, which = "continent")

```

#### Unemployment

> Null Hypothesis (H0): There is no significant difference in the mean Unemployment rates across continents.
> Alternative Hypothesis (H1): There is a significant difference in the mean Unemployment rates across continents.

```{r}
# Checking for outliers

boxplot(Unemployment ~ continent, data=hypothesis, xlab="Continents", 
        ylab="Unemployment", main="Unemployment across continents")
```
```{r}
ggplot(hypothesis, aes(x = continent, y = Unemployment, fill = continent)) +
  geom_bar(stat = "summary", fun = "mean", position = "dodge") +
  labs(title = "Mean Unemployment Across Continents",
       x = "Continent",
       y = "Mean Unemployment") +
  theme_minimal()
```

```{r}
# Checking for normal distribution
byf.shapiro(Unemployment ~ continent, data=hypothesis)
```

```{r}

# Checking for homogeneity of variances
bartlett.test(Unemployment ~ Country, data=hypothesis)

```

```{r}
# Conducting a posthoc with Welch
oneway.test(Unemployment ~ Country,data=hypothesis, var.equal = FALSE)

```

```{r}
# Running a Post-Hoc Test

Unemployment_anova <- aov(Unemployment ~ continent, data = hypothesis)
TukeyHSD(Unemployment_anova, which = "continent")

```

### **Checking for Normal Distribution**

```{r}
# Function to create histograms and Q-Q plots

check_normality <- function(variable_name) {
  p1 <- ggplot(economic, aes(x = .data[[variable_name]])) +
    geom_histogram(binwidth = 10, fill = "blue", color = "black", alpha = 0.7) +
    labs(title = paste("Histogram for", variable_name),
         x = variable_name,
         y = "Frequency") +
    theme_minimal()
  
  p2 <- ggplot(economic, aes(x = .data[[variable_name]])) +
    geom_density(fill = "blue", alpha = 0.7) +
    labs(title = paste("Density Plot for", variable_name),
         x = variable_name,
         y = "Density") +
    theme_minimal()
  
  print(p1)
  print(p2)
}
```

```{r}
check_normality("CPI")
```

```{r}
check_normality("Inflation")
```

```{r}
check_normality("GDP_Growth")
```

```{r}
check_normality("Unemployment")
```

### **Shapiro--Wilk test**

```{r}

# Function to perform Shapiro-Wilk test

ultimate_shapiro_test <- function(data) {
  cat("Shapiro-Wilk Test Results\n\n")
  
  for (variable_name in colnames(data)) {
    if (is.numeric(data[[variable_name]])) {
      shapiro_test_result <- shapiro.test(data[[variable_name]])
      cat("Variable:", variable_name, "\n")
      cat("  W =", shapiro_test_result$statistic, ", p-value =", shapiro_test_result$p.value, "\n")
      
      # Check if we should reject or accept the null hypothesis
      if (shapiro_test_result$p.value > 0.05) {
        cat("  Result: Do not reject the null hypothesis (data may be normally distributed)\n\n")
      } else {
        cat("  Result: Reject the null hypothesis (data may not be normally distributed)\n\n")
      }
    }
  }
}
```

```{r}
ultimate_shapiro_test(economic)
```

**Observation:**
  
  The P-value of all the variables are less than the significance level 0.05, hence we proceed to **reject the Null Hypothesis** and conclude that the data does not follow a normal distribution.


### **Conducting Transformations**

To make the data set become normally distributed by applying **logarithmic** transformation

```{r}

# Creating a copy of the original data frame
economic_log <- economic

# Apply logarithmic transformation
numeric_columns <- sapply(economic_log, is.numeric)
economic_log[numeric_columns] <- lapply(economic_log[numeric_columns], function(x) log(x + 1))

# Plot histograms after transformation
par(mfrow = c(2, 2))
# Plot histograms after transformation
for (col in names(economic_log[numeric_columns])) {
  hist(economic_log[[col]], main = col, col = "lightblue")}

```

**Observation:**
  
  Using Log Transformation, it appears the data set is now as close as it gets to becoming normally distributed.

```{r}
# Identifying missing values

missing_values <- colSums(is.na(economic_log))
missing_values
```

```{r}
## Handing missing values

economic_log <- replace_missing_values(economic_log, "Inflation", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "GDP_Growth", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Per_Capita_Growth", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "FDI_NetInflows", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "FDI_Percentage", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Political_Stability", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Corruption_Control", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Government_Effectiveness", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Regulatory_Quality", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Rule_of_Law", "Country", missing_values)

economic_log <- replace_missing_values(economic_log, "Rule_of_Law", "Country", missing_values)

```

```{r}
missing_values <- economic_log[is.na(economic_log$FDI_NetInflows) | is.na(economic_log$Political_Stability), c("Country", "FDI_NetInflows", "Political_Stability")]

# Print the result
print(missing_values)
```


## **Regression Analysis**

### Trade Balances vs GDP

```{r}
# Randomly selecting a country from each continent

set.seed(200)

selected_countries <- economic_log %>%
  group_by(continent) %>%
  slice_sample(n = 1) %>%
  ungroup()

print(selected_countries)
```

```{r}

# Creating a data subset for the selected countries using our transformed data

economicIndicators <- economic_log[
  economic_log$Country %in% selected_countries$Country,
  c("Country", "continent", "Per_Capita", "GDP", "Labor_Force", "Exports", 
    "Imports", "Technology_Exports")]

economicIndicators_Africa <- economicIndicators[economicIndicators$continent == "Africa", ]
economicIndicators_Americas <- economicIndicators[economicIndicators$continent == "Americas", ]
economicIndicators_Asia <- economicIndicators[economicIndicators$continent == "Asia", ]
economicIndicators_Europe <- economicIndicators[economicIndicators$continent == "Europe", ]

```


**Africa**
  
  ```{r}
# Fitting an SLR for a single country in Africa
ecoModel1 <- lm(GDP ~ Exports, data = economicIndicators_Africa)
print(summary(ecoModel1))


# Plotting the regression line
plot( GDP ~ Exports, data = economicIndicators_Africa,
      col = "blue",
      main = "Regression: GDP & Exports for Africa",
      xlab = "Exports",
      ylab = "GDP ($billion)")

abline(ecoModel1, col="red")



## Checking SLR assumptions
plot(ecoModel1, 1)

plot(ecoModel1, 2)

plot(ecoModel1, 3)
```

**Americas**
  
  ```{r}
# Fitting an SLR for a single country in Americas
ecoModel2 <- lm(GDP ~ Exports, data = economicIndicators_Americas)
print(summary(ecoModel2))


# Plotting the regression line
plot( GDP ~ Exports, data = economicIndicators_Americas,
      col = "blue",
      main = "Regression: GDP & Exports for Americas",
      xlab = "Exports",
      ylab = "GDP ($billion)")

abline(ecoModel2, col="red")


## Checking SLR assumptions
plot(ecoModel2, 1)
plot(ecoModel2, 2)
plot(ecoModel2, 3)
```

**Asia**
  
  ```{r}

# Fitting an SLR for a single country in Asia
ecoModel3 <- lm(GDP ~ Exports, data = economicIndicators_Asia)
print(summary(ecoModel3))

# Plotting the regression line
plot( GDP ~ Exports, data = economicIndicators_Asia,
      col = "blue",
      main = "Regression: GDP & Exports for Asia",
      xlab = "Exports",
      ylab = "GDP ($billion)")

abline(ecoModel3, col="red")


## Checking SLR assumptions
plot(ecoModel3, 1)
plot(ecoModel3, 2)
plot(ecoModel3, 3)
```

**Europe**
  
  ```{r}
ecoModel4 <- lm(GDP ~ Exports, data = economicIndicators_Europe)
print(summary(ecoModel4))


# Plotting the regression line
plot( GDP ~ Exports, data = economicIndicators_Europe,
      col = "blue",
      main = "Regression: GDP & Exports for Europe",
      xlab = "Exports",
      ylab = "GDP ($billion)")

abline(ecoModel4, col="red")

## Checking SLR assumptions
plot(ecoModel4, 1)
plot(ecoModel4, 2)
plot(ecoModel4, 3)
```

#### Simple Linear Regression

(Based on all countries)

```{r}

ecoModel5 <- lm(GDP ~ Exports, data = economic_log)
print(summary(ecoModel5))


# Plotting the regression line
plot( GDP ~ Exports, data = economic_log,
      col = "blue",
      main = "Regression: GDP & Exports",
      xlab = "Exports",
      ylab = "GDP ($billion)")

abline(ecoModel5, col="red")

```
```{r}
## Checking all regression assumptions
plot(ecoModel5, 1)

plot(ecoModel5, 2)

residuals <- residuals(ecoModel5)
hist(residuals, main = "Histogram of Residuals", col = "red")

plot(ecoModel5, 3)
```

Multiple Linear Regression

```{r}
# Fitting a Multiple Linear Regression model
ecoModel6 <- lm(GDP ~ Exports + Imports , data = economic_log)
print(summary(ecoModel6))

# Adding an extra independent variable to the model
ecoModel7 <- lm(GDP ~ Exports + Imports + Technology_Exports   , data = economic_log)
print(summary(ecoModel7))

# Adding Imports to the model
ecoModel8 <- lm(GDP ~ Exports + Imports + Labor_Force, data = economic_log)
print(summary(ecoModel8))
```

```{r}
# Plotting the regression line
ggplot(ecoModel7, aes(x = Exports, y = GDP)) +
  geom_point(aes(color = Imports)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  labs(title = "Multiple Linear Regression",
       x = "Exports",
       y = "Gross Domestic Product",
       color = "Imports") +
  theme_minimal()
```

```{r}
## Checking SLR assumptions

plot(ecoModel7, 1)
plot(ecoModel7, 2)
plot(ecoModel7, 3)
```

### Government Indices vs GDP Per Capita
**Multiple Linear Regression**
  
  ```{r}
# Selecting inflation-related variables
social_var <- economic_log[, c("Per_Capita", "Corruption_Control", "Political_Stability", 
                               "Government_Effectiveness", "Regulatory_Quality", "Rule_of_Law")]

# Computing correlation matrix
social_matrix <- cor(social_var, method="spearman")
corrplot(cor(social_var), method = "number", type = "upper")
``` 

```{r}
pairs(social_var, lower.panel = NULL, pch = 19,cex = 0.2)
```


```{r}
# Fitting the Multiple Linear Regression model
socModel1 <- lm(Per_Capita ~ Government_Effectiveness + 
                  Corruption_Control, data = economic_log)

print(summary(socModel1))

# Adding an extra independent variable to the mix
socModel2 <- lm(Per_Capita ~ Government_Effectiveness + 
                  Corruption_Control + Political_Stability, data = economic_log)
print(summary(socModel2))

# Adding Regulatory Quality to the mix
socModel3 <- lm(Per_Capita ~ Government_Effectiveness + Corruption_Control 
                + Political_Stability + Regulatory_Quality, data = economic_log)
print(summary(socModel3))

```

```{r}
# Plotting the regression line

ggplot(socModel3, aes(x = Government_Effectiveness, y = Per_Capita)) +
  geom_point(aes(color = Regulatory_Quality)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  labs(title = "Multiple Linear Regression",
       x = "Government Effectiveness",
       y = "GDP Per Capita",
       color = "Regulatory Quality") +
  theme_minimal()
```
```{r}

## Checking all regression assumptions
plot(socModel3, 1)

plot(socModel3, 2)

residuals <- residuals(socModel3)
hist(residuals, main = "Histogram of Residuals", col = "lightblue")

plot(socModel3, 3)

```

### **Inflation Dynamics and GDP Per Capita**

```{r}
# Select inflation-related variables
inflation_var <- economic_log[, c("GDP", "GDP_Growth", "Per_Capita", "Per_Capita_Growth", "Inflation", "CPI")]

# Compute correlation matrix
inflation_cor_matrix <- cor(inflation_var, method="spearman")
corrplot(cor(inflation_var), method = "number", type = "upper")
```


```{r}
pairs(inflation_var, lower.panel = NULL, pch = 19,cex = 0.2)
```

#### Multiple Linear Regression

```{r}
## Initiating an MLR with CPI and Inflation

inflationModel2 <- lm(Per_Capita ~ CPI + Inflation, data = economic_log)
summary.lm(inflationModel2)

# Plotting the regression line
ggplot(inflationModel2, aes(x = Inflation, y = Per_Capita)) +
  geom_point(aes(color = CPI)) +
  geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +
  labs(title = "Multiple Linear Regression",
       x = "Inflation Rate",
       y = "Per Capita",
       color = "Consumer Price Index (CPI)") +
  theme_minimal()


## Checking for Residuals’ Independence

plot(inflationModel2, 1)
plot(inflationModel2, 2)
plot(inflationModel2, 3)
```

#### Polynomial Regression

```{r}
inflationModel3 <- lm(Per_Capita ~ poly(Inflation, 2) + poly(CPI, 2), data = economic_log)
summary(inflationModel3)


# Plotting the regression line
ggplot(economic_log, aes(x = Inflation, y = Per_Capita, color = "Inflation")) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 3), se = FALSE) +
  labs(title = "Quadratic Polynomial Regression",
       x = "Inflation and CPI",
       y = "Per Capita") +
  geom_point(aes(x = CPI, y = Per_Capita, color = "CPI")) +
  geom_smooth(aes(x = CPI, y = Per_Capita), method = "lm", formula = y ~ poly(x, 2), se = FALSE) +
  scale_color_manual(values = c("Inflation" = "blue", "CPI" = "red"))
```

```{r}
ggplot(economic_log, aes(x = Inflation, y = Per_Capita)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, 2), se = FALSE, color = "blue") +
  labs(title = "Quadratic Polynomial Regression - Inflation",
       x = "Inflation",
       y = "Per Capita")
```

```{r}

## Checking all regression assumptions
plot(inflationModel3, 1)

plot(inflationModel3, 2)

residuals <- residuals(inflationModel3)
hist(residuals, main = "Histogram of Residuals", col = "lightblue")

plot(inflationModel3, 3)

```

## **Time Series Analysis**

```{r}
install.packages("forecast")
install.packages("zoo")
install.packages("tseries")
install.packages("TTR")

library(TTR)
library(tseries)
library(zoo)
library(forecast)
```

### Time Series Analysis on GDP

```{r}
# Randomly selecting countries from each continent

set.seed(20)

selected_countries <- economic_log %>%
  group_by(continent) %>%
  slice_sample(n = 1) %>% 
  ungroup()

print(selected_countries)
```

```{r}
# Creating a data subset for the selected countries

GDP_Data <- economic_log[economic_log$Country %in% selected_countries$Country, 
                         c("Country", "continent", "GDP", "GDP_Growth")]

GDP_Data_Africa <- GDP_Data[GDP_Data$continent == "Africa", ]
GDP_Data_Americas <- GDP_Data[GDP_Data$continent == "Americas", ]
GDP_Data_Asia <- GDP_Data[GDP_Data$continent == "Asia", ]
GDP_Data_Europe <- GDP_Data[GDP_Data$continent == "Europe", ]
```


##### **AFRICA (ETHIOPIA) FORECAST**

```{r}

# Creating a time series object for yearly data
seriesAfrica <- ts(GDP_Data_Africa$GDP_Growth, start = c(2008), end = c(2022))

# Plotting the time series
plot.ts(seriesAfrica)
```

```{r}
# Calculate and comparing Simple Moving Average (SMA) with different sizes 
GDPseriesSMA3 <- SMA(seriesAfrica, n=3)
plot.ts(GDPseriesSMA3)

GDPseriesSMA8 <- SMA(seriesAfrica, n=8)
plot.ts(GDPseriesSMA8)
```

```{r}
# Using Holt-Winters Exponential Smoothing (with trend component)

GDPseries_smoothing2 <-  HoltWinters(seriesAfrica, gamma=FALSE)
GDPseries_smoothing2
plot(GDPseries_smoothing2)

```

```{r}
# Computing the Sum of Squared Errors (SSE)
GDPseries_smoothing2$SSE
```

```{r}

# Forecasting the next 15  years 
GDPseries_forecast2 <- forecast(GDPseries_smoothing2, h=15)
GDPseries_forecast2
plot(GDPseries_forecast2)

```

```{r}

# Plotting autocorrelation function
acf(GDPseries_forecast2$residuals, lag.max=20 , na.action = na.pass)

# Performing the Ljung-Box test 
Box.test(GDPseries_forecast2$residuals, lag=10, type="Ljung-Box")
```

```{r}
plotForecastErrors <- function(forecasterrors)
{
  # make a histogram of the forecast errors:
  mybinsize <- IQR(forecasterrors,na.rm=TRUE)/4
  mysd   <- sd(forecasterrors,na.rm=TRUE)
  mymin  <- min(forecasterrors,na.rm=TRUE) - mysd*5
  mymax  <- max(forecasterrors,na.rm=TRUE) + mysd*3
  # generate normally distributed data with mean 0 and standard deviation mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # make a red histogram of the forecast errors, with the normally distributed data overlaid:
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # freq=FALSE ensures the area under the histogram = 1
  # generate normally distributed data with mean 0 and standard deviation mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # plot the normal curve as a blue line on top of the histogram of forecast errors:
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
```


```{r}
# Plotting the time series of residuals and forecast errors

plot.ts(GDPseries_forecast2$residuals)

GDPseries_forecast2$residuals <-GDPseries_forecast2$residuals[!is.na(GDPseries_forecast2$residuals)]
plotForecastErrors(GDPseries_forecast2$residuals)
```

##### **AMERICAS (MEXICO) FORECAST**

```{r}
# Transform Data to Time Series
seriesAmericas <- ts(GDP_Data_Americas$GDP_Growth, start = c(2008), end = c(2022))

# Plot the time series
plot.ts(seriesAmericas)
```

```{r}
# Using Holt-Winters Exponential Smoothing (with trend component)

GDPseries_smoothing <-  HoltWinters(seriesAmericas, gamma=FALSE)
GDPseries_smoothing
```

```{r}
GDPseries_smoothing$SSE
plot(GDPseries_smoothing)
```

```{r}
GDPseries_forecast <- forecast(GDPseries_smoothing, h=19)
GDPseries_forecast
plot(GDPseries_forecast)
```

```{r}

acf(GDPseries_forecast$residuals, lag.max=20 , na.action = na.pass)

Box.test(GDPseries_forecast$residuals, lag=20, type="Ljung-Box")

```

```{r}
plot.ts(GDPseries_forecast$residuals)


GDPseries_forecast$residuals <-GDPseries_forecast$residuals[!is.na(GDPseries_forecast$residuals)]
plotForecastErrors(GDPseries_forecast$residuals)
```

##### **ASIA (CHINA) FORECAST**

```{r}
# Transform Data to Time Series
seriesAsia <- ts(GDP_Data_Asia$GDP_Growth, start = c(2008), end = c(2022))

# Plot the time series
plot.ts(seriesAsia)
```

```{r}
# Using Holt-Winters Exponential Smoothing (with trend component)

GDPseries_smoothing3 <-  HoltWinters(seriesAsia, gamma=FALSE)
GDPseries_smoothing3
```

```{r}
GDPseries_smoothing3$SSE
plot(GDPseries_smoothing3)
```

```{r}
GDPseries_forecast3 <- forecast(GDPseries_smoothing3, h=5)
GDPseries_forecast3
plot(GDPseries_forecast3)
```

```{r}

acf(GDPseries_forecast3$residuals, lag.max=20 , na.action = na.pass)

Box.test(GDPseries_forecast3$residuals, lag=20, type="Ljung-Box")

```

```{r}
plot.ts(GDPseries_forecast3$residuals)


GDPseries_forecast3$residuals <-GDPseries_forecast3$residuals[!is.na(GDPseries_forecast3$residuals)]
plotForecastErrors(GDPseries_forecast3$residuals)
```

##### **EUROPE (GERMANY) FORECAST**

```{r}
# Transform Data to Time Series
seriesEurope <- ts(GDP_Data_Europe$GDP_Growth, start = c(2008), end = c(2022))

# Plot the time series
plot.ts(seriesEurope)
```

```{r}
# Using Holt-Winters Exponential Smoothing (with trend component)

GDPseries_smoothing4 <-  HoltWinters(seriesEurope, gamma=FALSE)
GDPseries_smoothing4
```

```{r}
GDPseries_smoothing4$SSE
plot(GDPseries_smoothing4)
```

```{r}
GDPseries_forecast4 <- forecast(GDPseries_smoothing4, h=19)
GDPseries_forecast4
plot(GDPseries_forecast4)
```

```{r}

acf(GDPseries_forecast4$residuals, lag.max=20 , na.action = na.pass)

Box.test(GDPseries_forecast4$residuals, lag=20, type="Ljung-Box")

```

```{r}
plot.ts(GDPseries_forecast4$residuals)

GDPseries_forecast4$residuals <-GDPseries_forecast4$residuals[!is.na(GDPseries_forecast4$residuals)]
plotForecastErrors(GDPseries_forecast4$residuals)
```

##### **ARIMA MODEL**

```{r}

seriesAfrica_diff1 <- diff(seriesAfrica, differences=1)

plot.ts(seriesAfrica_diff1)
```

```{r}
# Exploring stationarity with two differences

seriesAfrica_diff2 <- diff(seriesAfrica, differences=2)
plot.ts(seriesAfrica_diff2)
```

```{r}
# Finding appropriate values of p and q

# Plotting a correlogram

acf(seriesAfrica_diff2, lag.max=20)
acf(seriesAfrica_diff2, lag.max=20, plot=FALSE)
```

```{r}
# Plotting a partial correlogram

pacf(seriesAfrica_diff2, lag.max=20)
pacf(seriesAfrica_diff2, lag.max=20, plot=FALSE)
```

```{r}
# Fitting an ARIMA model to the time series
seriesAfrica_arima <- arima(seriesAfrica, order=c(0,1,1))
seriesAfrica_arima
```

```{r}
# Generating forecasts using the ARIMA model

seriesAfrica_forecasts <- forecast(seriesAfrica_arima, h=5)
seriesAfrica_forecasts
plot(seriesAfrica_forecasts)

```

```{r}
acf(seriesAfrica_forecasts$residuals, lag.max=20)
Box.test(seriesAfrica_forecasts$residuals, lag=14, type="Ljung-Box")
```

```{r}
plot.ts(seriesAfrica_forecasts$residuals)

plotForecastErrors(seriesAfrica_forecasts$residuals)
```

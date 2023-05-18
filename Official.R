

#Setting a global variable for my data
MyData <- survey_results_public_correct
install.packages("RColorBrewer")

# Load Libraries we are going to use
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)
library(stringr)
library(quantmod)
library(priceR)
library(treemap)
library(RColorBrewer)
library(maps)
library(countrycode)
library(tidygraph)
library(ggraph)
library(igraph)

### Where did people take the Survey from

##TreeMap of Top 30 Countries
color_palette <- brewer.pal(8, "Set3")

# Remove any leading or trailing white space
MyData$Country <- trimws(MyData$Country)

# Replace any missing values with "Unknown"
MyData$Country[is.na(MyData$Country)] <- "Unknown"

# Count the number of responses from each country
country_counts <- table(MyData$Country)

# Convert to data frame for plotting
country_counts_df <- as.data.frame(country_counts)
names(country_counts_df) <- c("Country", "Count")

# Filter the top 30 countries
top_countries <-
  country_counts_df[order(-country_counts_df$Count),][1:30,]

# Create a color palette with RColorBrewer
color_palette <- brewer.pal(8, "Set2")

# Create treemap
treemap(
  top_countries,
  index = c("Country", "Count"),
  vSize = "Count",
  title = "Top 30 Countries where the results are from",
  fontsize.labels = c(15, 12),
  fontcolor.labels = c("white", "white"),
  align.labels = list(c("center", "center"),
                      c("right", "bottom")),
  overlap.labels = 0.5,
  inflate.labels = F,
  palette = color_palette
)


### Top 10 Most Popular Programming languages

# Split the Languages worked column
split_langs <- strsplit(MyData$LanguageHaveWorkedWith, split = ";")

# Unlist to create a single vector of languages
unlisted_langs <- unlist(split_langs)

# Count the number of times each language appears
lang_counts <- table(unlisted_langs)

# Calculate total count of all languages
total_count <- sum(lang_counts)

# Sort in descending order and take the top 10
top_langs <- sort(lang_counts, decreasing = TRUE)[1:10]

# Convert to data frame for plotting
top_langs_df <- as.data.frame(table(unlisted_langs))
names(top_langs_df) <- c("language", "count")

# Filter to only include top languages
top_langs_df <-
  top_langs_df[top_langs_df$language %in% names(top_langs), ]

# Calculate percentages
top_langs_df$percentage <- (top_langs_df$count / total_count) * 100

# Reorder languages by count in ascending order
top_langs_df$language <-
  factor(top_langs_df$language, levels = rev(names(top_langs)))

# Plot
ggplot(top_langs_df, aes(y = language, x = percentage)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust = -0.1, size =
              3.5) +
  labs(y = "Programming Language", x = "Percentage (%)",
       title = "Top 10 Programming Languages by Usage Percentage") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

###Top 10 Programing Languages Developers want to use

# Split the Languages worked column
split_langs1 <- strsplit(MyData$LanguageWantToWorkWith, split = ";")

# Unlist to create a single vector of languages
unlisted_langs1 <- unlist(split_langs1)

# Count the number of times each language appears
lang_counts1 <- table(unlisted_langs1)

# Calculate total count of all languages
total_count1 <- sum(lang_counts1)

# Sort in descending order and take the top 10
top_langs1 <- sort(lang_counts1, decreasing = TRUE)[1:10]

# Convert to data frame for plotting
top_langs_df1 <- as.data.frame(table(unlisted_langs1))
names(top_langs_df1) <- c("language", "count")

# Filter to only include top languages
top_langs_df1 <-
  top_langs_df1[top_langs_df1$language %in% names(top_langs1), ]

# Calculate percentages
top_langs_df1$percentage <-
  (top_langs_df1$count / total_count1) * 100

# Reorder languages by count in ascending order
top_langs_df1$language <-
  factor(top_langs_df1$language, levels = rev(names(top_langs1)))

# Plot
ggplot(top_langs_df1, aes(y = language, x = percentage)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), hjust = -0.1, size =
              3.5) +
  labs(y = "Programming Language", x = "Percentage (%)",
       title = "Top 10 Programming Languages that developers want to use in 2023") +
  theme_minimal() + theme(plot.title = element_text(hjust = 0.5))

##How many respondents are working remotely

# Remove rows where RemoteWork is NA or blank
MyData <-
  MyData[!is.na(MyData$RemoteWork) & MyData$RemoteWork != "",]

# Get counts of each category
remote_counts <- table(MyData$RemoteWork)

# Convert to data frame for ggplot2
remote_counts_df <- as.data.frame(remote_counts)
names(remote_counts_df) <- c("RemoteWork", "Count")

# Create the pie chart

ggplot(remote_counts_df, aes(x = "", y = Count, fill = RemoteWork)) +
  geom_bar(width = 1,
           stat = "identity",
           color = "white") +
  coord_polar("y", start = 0) +
  scale_fill_brewer(palette = "Dark2") +
  theme_minimal() +
  labs(title = "Distribution of Work Modes", fill = "Work Mode") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 20, face = "bold"),
    legend.title = element_text(size = 12, face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  geom_text(
    aes(label = paste0(
      "Count: ", Count, "\n", round(Count / sum(Count) * 100, 1), "%"
    )),
    position = position_stack(vjust = 0.5),
    size = 3,
    color = "white",
    fontface = "bold"
  )

##Educational Attainment

# Count the number of occurrences for each education level
edlevel_counts <- table(MyData$EdLevel)

# Convert to data frame
data <- as.data.frame(edlevel_counts)
names(data) <- c("category", "count")

# Compute percentages
data$fraction <- data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n = -1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$category, "\n value: ", data$count)

# Make the plot
ggplot(data, aes(
  fill = category,
  ymax = ymax,
  ymin = ymin,
  xmax = 4,
  xmin = 3
)) +
  geom_rect() +
  scale_fill_brewer(palette = "Set1") +
  coord_polar(theta = "y") +
  xlim(c(-1, 4)) +
  theme_minimal()


##How did People Learn to Code. By age range

##People learning how to code by Age Range
# Split LearnCode into multiple rows
MyData <- MyData %>%
  mutate(LearnCode = str_split(LearnCode, ";")) %>%
  unnest(LearnCode)

MyData <- MyData %>% drop_na(LearnCode, Age)

# Create the plot
ggplot(MyData, aes(x = LearnCode)) +
  geom_bar(aes(fill = Age), position = "dodge") +
  labs(x = "Learning Method", y = "Number of Respondents", fill = "Age Range") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  coord_flip()  # Makes the bars horizontal; remove this if you want vertical bars

###Years of Professional Coding vs Total salary per year

#Data cleaning

##Creating dataset for first task
ProExpVsSalary <- MyData %>%
  select(YearsCodePro, Currency, CompTotal)

ProExpVsSalary <- ProExpVsSalary %>%
  na.omit()

ProExpVsSalary <- ProExpVsSalary %>%
  mutate(abbreviation = str_extract(ProExpVsSalary$Currency, "\\b[A-Z]{3}\\b"))

##Filtering out depending by type of currency
# Find the number of unique variables in your data
unique_count <- length(unique(ProExpVsSalary$Currency))

# Print the number of unique variables
print(unique_count)
glimpse(ProExpVsSalary)

currency_datasets <-
  split(ProExpVsSalary, ProExpVsSalary$abbreviation)

# Access individual datasets
euro_dataset <- currency_datasets$EUR
usd_dataset <- currency_datasets$USD
gbp_dataset <- currency_datasets$GBP
jpy_dataset <- currency_datasets$JPY
cny_dataset <- currency_datasets$CNY



process_dataset <- function(dataset) {
  # Convert to numeric
  dataset$YearsCodePro <- as.numeric(dataset$YearsCodePro)
  dataset$CompTotal <- as.numeric(dataset$CompTotal)
  
  # Calculate quartiles and IQR for CompTotal
  Q1 <- quantile(dataset$CompTotal, 0.25)
  Q3 <- quantile(dataset$CompTotal, 0.75)
  IQR <- Q3 - Q1
  
  # Remove outliers
  dataset <-
    subset(dataset, CompTotal > (Q1 - 1.5 * IQR) &
             CompTotal < (Q3 + 1.5 * IQR))
  
  return(dataset)
}

# Now you can process each dataset like this:
euro_dataset <- process_dataset(euro_dataset)
usd_dataset <- process_dataset(usd_dataset)
gbp_dataset <- process_dataset(gbp_dataset)
jpy_dataset <- process_dataset(jpy_dataset)
cny_dataset <- process_dataset(cny_dataset)



#Create the plot for each function
create_plot <- function(dataset, x_var, y_var, currency) {
  x_range <- range(dataset[[x_var]], na.rm = TRUE)
  y_range <- range(dataset[[y_var]], na.rm = TRUE)
  
  ggplot(dataset, aes_string(x = x_var, y = y_var)) +
    geom_hex(bins = 50) +
    labs(title = paste(
      "Years of Professional Coding vs Total Salary per Year (",
      currency,
      ")"
    )) +
    xlab("Years of Professional Coding") +
    ylab(paste("Total Salary per Year (", currency, ")")) +
    scale_x_continuous(breaks = pretty(x_range, n = 10)) +
    scale_y_continuous(breaks = pretty(y_range, n = 10)) +
    theme(
      axis.title.x = element_text(size = 12),
      axis.title.y = element_text(size = 12),
      axis.text.x = element_text(size = 10, angle = 45),
      axis.text.y = element_text(size = 10)
    )
}

# Now you can create the plots for your datasets like this:

create_plot(euro_dataset, "YearsCodePro", "CompTotal", "Euro")
create_plot(usd_dataset, "YearsCodePro", "CompTotal", "USD")
create_plot(gbp_dataset, "YearsCodePro", "CompTotal", "GBP")
create_plot(jpy_dataset, "YearsCodePro", "CompTotal", "JPY")
create_plot(cny_dataset, "YearsCodePro", "CompTotal", "CNY")

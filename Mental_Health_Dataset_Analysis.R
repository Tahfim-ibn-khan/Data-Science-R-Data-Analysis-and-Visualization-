install.packages("ggplot2")
install.packages("reshape2")
install.packages("dplyr")
install.packages("GGally")

library(GGally)
library(ggplot2)
library(reshape2)
library(dplyr)

Mental_Health_Dataset <- read.csv("/Users/tahfimibnkhan/Desktop/mental_health_and_technology_usage_2024.csv")
str(Mental_Health_Dataset)

categorical_vars <- c('Gender', 'Stress_Level', 'Support_Systems_Access', 'Work_Environment_Impact', 'Online_Support_Usage')

for (var in categorical_vars) {
  print(paste("Chi-Squared Test for Mental_Health_Status and", var))
  chi_test <- chisq.test(table(Mental_Health_Dataset[[var]], Mental_Health_Dataset$Mental_Health_Status))
  print(chi_test)
}

unique(Mental_Health_Dataset$Mental_Health_Status)
Mental_Health_Dataset$Mental_Health_Status_Num <- as.numeric(factor(Mental_Health_Dataset$Mental_Health_Status))

numeric_vars <- c('Age', 'Technology_Usage_Hours', 'Social_Media_Usage_Hours', 'Gaming_Hours', 'Screen_Time_Hours', 'Sleep_Hours', 'Physical_Activity_Hours')

for (var in numeric_vars) {
  print(paste("Kendall's Correlation for Mental_Health_Status and", var))
  kendall_corr <- cor(Mental_Health_Dataset[[var]], Mental_Health_Dataset$Mental_Health_Status_Num, method = 'kendall')
  print(kendall_corr)
}

ggplot(Mental_Health_Dataset, aes(x = Age, y = Mental_Health_Status_Num)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggtitle("Scatter Plot: Mental_Health_Status vs Age")

ggplot(Mental_Health_Dataset, aes(x = Technology_Usage_Hours, y = Mental_Health_Status_Num)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggtitle("Scatter Plot: Mental_Health_Status vs Technology_Usage_Hours")

ggplot(Mental_Health_Dataset, aes(x = Social_Media_Usage_Hours, y = Mental_Health_Status_Num)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggtitle("Scatter Plot: Mental_Health_Status vs Social_Media_Usage_Hours")

ggplot(Mental_Health_Dataset, aes(x = Gaming_Hours, y = Mental_Health_Status_Num)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggtitle("Scatter Plot: Mental_Health_Status vs Gaming_Hours")

ggplot(Mental_Health_Dataset, aes(x = Gaming_Hours, y = Mental_Health_Status_Num)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggtitle("Scatter Plot: Mental_Health_Status vs Gaming_Hours")

ggplot(Mental_Health_Dataset, aes(x = Screen_Time_Hours, y = Mental_Health_Status_Num)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggtitle("Scatter Plot: Mental_Health_Status vs Screen_Time_Hours")

ggplot(Mental_Health_Dataset, aes(x = Sleep_Hours, y = Mental_Health_Status_Num)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggtitle("Scatter Plot: Mental_Health_Status vs Sleep_Hours")

ggplot(Mental_Health_Dataset, aes(x = Physical_Activity_Hours, y = Mental_Health_Status_Num)) +
  geom_point() +
  geom_smooth(method = 'lm') + 
  ggtitle("Scatter Plot: Mental_Health_Status vs Physical_Activity_Hours")

ggplot(Mental_Health_Dataset, aes(x = Gender, fill = Mental_Health_Status)) +
  geom_bar(position = "dodge") + 
  ggtitle("Bar Plot: Mental_Health_Status vs Gender") +
  labs(x = "Gender", y = "Count")

numeric_cols <- c("Age", "Technology_Usage_Hours", "Social_Media_Usage_Hours", "Gaming_Hours", "Screen_Time_Hours", "Sleep_Hours", "Physical_Activity_Hours")

ggpairs(Mental_Health_Dataset, 
        columns = numeric_cols, 
        mapping = aes(color = Mental_Health_Status),
        title = "Scatterplot Matrix for Mental Health Dataset")

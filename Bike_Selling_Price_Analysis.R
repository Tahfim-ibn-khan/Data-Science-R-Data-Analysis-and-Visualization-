library(ggplot2)
library(dplyr)
library(reshape2)
library(plotly)
library(ggiraphExtra)
library(fmsb)
library(scales) 
library(ggpubr)

data <- read.csv("/Users/tahfimibnkhan/Desktop/BIKE_DETAILS.csv")

data$ex_showroom_price[is.na(data$ex_showroom_price)] <- median(data$ex_showroom_price, na.rm = TRUE)
str(data)


hist_plot <- ggplot(data, aes(x = selling_price)) +
  geom_histogram(binwidth = 20000, fill = "blue", color = "black", alpha = 0.7) +  
  geom_freqpoly(binwidth = 20000, color = "orange", size = 1) +  
  geom_density(aes(y = ..count..), color = "red", size = 1.5) +  
  geom_vline(aes(xintercept = mean(selling_price, na.rm = TRUE)), color = "green", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(selling_price, na.rm = TRUE)), color = "orange", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = as.numeric(names(sort(table(selling_price), decreasing=TRUE)[1]))), 
             color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Histogram of Selling Price with Line Graph",
       x = "Selling Price",
       y = "Count") +
  annotate("text", x = max(data$selling_price) * 0.7, y = 50, 
           label = paste("Green->Mean:", round(mean(data$selling_price, na.rm = TRUE), 2),
                         "\nOrange->Median:", round(median(data$selling_price, na.rm = TRUE), 2),
                         "\nPurple->Mode:", round(as.numeric(names(sort(table(data$selling_price), decreasing=TRUE)[1])), 2)),
           color = "black", size = 4) +
  scale_x_continuous(labels = comma)

print(hist_plot)



hist_plot_km_driven <- ggplot(data, aes(x = km_driven)) +
  geom_histogram(binwidth = 20000, fill = "blue", color = "black", alpha = 0.7) +  
  geom_freqpoly(binwidth = 20000, color = "orange", size = 1) + 
  geom_density(aes(y = ..count..), color = "red", size = 1.5) +  
  geom_vline(aes(xintercept = mean(km_driven, na.rm = TRUE)), color = "green", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(km_driven, na.rm = TRUE)), color = "orange", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = as.numeric(names(sort(table(km_driven), decreasing=TRUE)[1]))), 
             color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Line Histogram of Kilometers Driven",
       x = "Kilometers Driven",
       y = "Count") +
  annotate("text", x = max(data$km_driven) * 0.7, y = 50, 
           label = paste("Green->Mean:", round(mean(data$km_driven, na.rm = TRUE), 2),
                         "\nOrange->Median:", round(median(data$km_driven, na.rm = TRUE), 2),
                         "\nPurple->Mode:", round(as.numeric(names(sort(table(data$km_driven), decreasing=TRUE)[1])), 2)),
           color = "black", size = 4) +
  scale_x_continuous(labels = comma)

print(hist_plot_km_driven)



hist_plot_ex_showroom_price <- ggplot(data, aes(x = ex_showroom_price)) +
  geom_histogram(binwidth = 20000, fill = "blue", color = "black", alpha = 0.7) +  
  geom_freqpoly(binwidth = 20000, color = "orange", size = 1) +  
  geom_density(aes(y = ..count..), color = "red", size = 1.5) +  
  geom_vline(aes(xintercept = mean(ex_showroom_price, na.rm = TRUE)), color = "green", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = median(ex_showroom_price, na.rm = TRUE)), color = "orange", linetype = "dashed", size = 1) +
  geom_vline(aes(xintercept = as.numeric(names(sort(table(ex_showroom_price), decreasing=TRUE)[1]))), 
             color = "purple", linetype = "dashed", size = 1) +
  labs(title = "Line Histogram of Ex Showroom Price",
       x = "Ex Showroom Price",
       y = "Count") +
  annotate("text", x = max(data$ex_showroom_price) * 0.7, y = 50, 
           label = paste("Green->Mean:", round(mean(data$ex_showroom_price, na.rm = TRUE), 2),
                         "\nOrange->Median:", round(median(data$ex_showroom_price, na.rm = TRUE), 2),
                         "\nPurple->Mode:", round(as.numeric(names(sort(table(data$ex_showroom_price), decreasing=TRUE)[1])), 2)),
           color = "black", size = 4) +
  scale_x_continuous(labels = comma)

print(hist_plot_ex_showroom_price)



bar_plot <- ggplot(data, aes(x = seller_type)) +
  geom_bar(fill = "skyblue", color = "black") +
  labs(title = "Bar Graph of Seller Type", x = "Seller Type", y = "Count") +
  scale_y_continuous(labels = comma) # format axis to avoid scientific notation
print(bar_plot)

bar_plot <- ggplot(data, aes(x = owner)) +
  geom_bar(fill = "pink", color = "black") +
  labs(title = "Bar Graph of Owner Type", x = "Owner", y = "Count") +
  scale_y_continuous(labels = comma)
print(bar_plot)

box_plot <- ggplot(data, aes(x = "", y = km_driven)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of Kilometers Driven", x = "", y = "Kilometers Driven") +
  scale_y_continuous(labels = comma)
print(box_plot)

box_plot <- ggplot(data, aes(x = "", y = selling_price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of selling price", x = "", y = "selling_price") +
  scale_y_continuous(labels = comma)
print(box_plot)

box_plot <- ggplot(data, aes(x = "", y = ex_showroom_price)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of ex showroom price", x = "", y = "ex_showroom_price") +
  scale_y_continuous(labels = comma)
print(box_plot)

box_plot <- ggplot(data, aes(x = "", y = year)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of year", x = "", y = "year") +
  scale_y_continuous(labels = comma)
print(box_plot)

scatter_plot_ex <- ggplot(data, aes(x = selling_price, y = ex_showroom_price)) +
  geom_point(color = "blue") +
  geom_smooth(method = 'lm', se = FALSE, color = "red") +
  ggtitle("Scatter Plot: selling_price vs ex_showroom_price") +
  stat_cor(method = "pearson", label.x = max(data$selling_price)*0.6, label.y = max(data$ex_showroom_price)*0.9) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)
print(scatter_plot_ex)

scatter_plot_km <- ggplot(data, aes(x = selling_price, y = km_driven)) +
  geom_point(color = "blue") +
  geom_smooth(method = 'lm', se = FALSE, color = "red") +
  ggtitle("Scatter Plot: selling_price vs km_driven") +
  stat_cor(method = "pearson", label.x = max(data$selling_price)*0.6, label.y = max(data$km_driven)*0.9) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)
print(scatter_plot_km)

scatter_plot_year <- ggplot(data, aes(x = selling_price, y = year)) +
  geom_point(color = "blue") +
  geom_smooth(method = 'lm', se = FALSE, color = "red") +
  ggtitle("Scatter Plot: selling_price vs year") +
  stat_cor(method = "pearson", label.x = max(data$selling_price)*0.6, label.y = max(data$year)*0.9) +
  scale_x_continuous(labels = comma) +
  scale_y_continuous(labels = comma)
print(scatter_plot_year)

set.seed(123)
sampled_data <- data %>% sample_frac(0.1)

scatter_plot <- ggplot(sampled_data, aes(x = km_driven, y = selling_price, color = owner)) +
  geom_point(size = 3, alpha = 0.6, position = position_jitter(width = 5000, height = 10000)) +
  geom_text(data = sampled_data %>%
              group_by(owner) %>%
              summarise(km_driven = mean(km_driven), selling_price = mean(selling_price)),
            aes(label = owner), vjust = -1, size = 5, color = "black") +
  labs(title = "Scatterplot of Selling Price vs Kilometers Driven by Owner", x = "Kilometers Driven", y = "Selling Price") +
  theme_minimal()

print(scatter_plot)


violin_plot <- ggplot(data, aes(x = seller_type, y = selling_price)) +
  geom_violin(fill = "lightgreen", color = "black") +
  stat_summary(fun.data = "median_hilow", geom = "pointrange", color = "black") +  
  stat_summary(fun = median, geom = "point", shape = 23, size = 2, color = "white", fill = "white") +  
  labs(title = "Violin Plot of Selling Price by Seller Type", x = "Seller Type", y = "Selling Price") +
  scale_y_continuous(labels = comma) +
  theme_minimal()  
print(violin_plot)

violin_plot <- ggplot(data, aes(x = owner, y = selling_price)) +
  geom_violin(fill = "lightgreen", color = "black") +
  stat_summary(fun.data = "median_hilow", geom = "pointrange", color = "black") +  
  stat_summary(fun = median, geom = "point", shape = 23, size = 2, color = "white", fill = "white") +  
  labs(title = "Violin Plot of Selling Price by Owner", x = "owner", y = "Selling Price") +
  scale_y_continuous(labels = comma) +
  theme_minimal()  
print(violin_plot)




agg_data <- data %>%
  group_by(year) %>%
  summarise(avg_selling_price = mean(selling_price, na.rm = TRUE))

line_plot <- ggplot(agg_data, aes(x = year, y = avg_selling_price)) +
  geom_line(color = "orange", size = 1) +  
  geom_point(color = "red", size = 3, alpha = 0.8) +  
  geom_text(aes(label = round(avg_selling_price, 0)), vjust = -0.5, color = "red", size = 3, check_overlap = TRUE) +
  labs(title = "Average Selling Price by Year", x = "Year", y = "Average Selling Price") +
  scale_y_continuous(labels = comma) + 
  theme_minimal() + 
  theme(
    panel.grid.major = element_line(color = "lightblue"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text.x = element_text(color = "blue"),  
    axis.text.y = element_text(color = "black")  
  )

print(line_plot)

agg_data <- data %>%
  group_by(km_driven) %>%
  summarise(avg_selling_price = mean(selling_price, na.rm = TRUE))

line_plot <- ggplot(agg_data, aes(x = km_driven, y = avg_selling_price)) +
  geom_line(color = "orange", size = 1) +  
  geom_point(color = "red", size = 3, alpha = 0.8) +  
  geom_text(aes(label = round(avg_selling_price, 0)), vjust = -0.5, color = "red", size = 3, check_overlap = TRUE) +
  labs(title = "Average Selling Price by km_driven", x = "km_driven", y = "Average Selling Price") +
  scale_y_continuous(labels = comma) + 
  theme_minimal() + 
  theme(
    panel.grid.major = element_line(color = "lightblue"),  
    axis.title.x = element_text(size = 12, face = "bold"),  
    axis.title.y = element_text(size = 12, face = "bold"),  
    axis.text.x = element_text(color = "blue"),  
    axis.text.y = element_text(color = "black")  
  )

print(line_plot)


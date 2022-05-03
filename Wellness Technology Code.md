---
Title: "Wellness Technology Case Study"
Author: "Kgothatso K"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


# Background

This project is part of the Wellness Technology Case Study.

# Install and load packages

```{r}
install.packages("tidyverse")
install.packages("skimr")
install.packages("janitor")
install.packages("lubridate")
install.packages("plotly")
install.packages("ggcorrplot")
```

```{r}
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(plotly)
library(htmlwidgets)
library(ggcorrplot)
```

# Import daily data

```{r}
wellness_tech_activity_daily <- read_csv("dailyActivity_merged.csv")
wellness_tech_sleep_daily <- read_csv("sleepDay_merged.csv")
```

#Inspect the data

```{r}
head(wellness_tech_activity_daily)
head(wellness_tech_sleep_daily)
```

```{r}
str(wellness_tech_activity_daily)
str(wellness_tech_sleep_daily)
```

#Transform and merge daily tables

##Activity daily data

```{r}
wellness_tech_activity_daily$ActivityDate <- as.Date(wellness_tech_activity_daily$ActivityDate, 
                                                     format = "%m/%d/%Y")
```

##Sleep daily data

```{r}
wellness_tech_sleep_daily$SleepDay <- as.Date(wellness_tech_sleep_daily$SleepDay, 
                                              format = "%m/%d/%Y")
```

```{r}
colnames(wellness_tech_sleep_daily)[2] <- "ActivityDate"
```

##Merge daily data

```{r}
wellness_tech_data_daily <- merge(wellness_tech_activity_daily, 
                                  wellness_tech_sleep_daily, 
                                  by = c("Id", "ActivityDate"), 
                                  all = TRUE)
```

#Save the daily data table

```{r}
write.csv(wellness_tech_data_daily,"wellness_tech_data_daily_R.csv", row.names = FALSE)
```

# Import hourly data

```{r}
wellness_tech_calories_hourly <- read_csv("hourlyCalories_merged.csv")
wellness_tech_intensities_hourly <- read_csv("hourlyIntensities_merged.csv")
wellness_tech_steps_hourly <- read_csv("hourlySteps_merged.csv")
```

#Inspect the data

```{r}
head(wellness_tech_calories_hourly)
head(wellness_tech_intensities_hourly)
head(wellness_tech_steps_hourly)
```

```{r}
str(wellness_tech_calories_hourly)
str(wellness_tech_intensities_hourly)
str(wellness_tech_steps_hourly)
```

#Transform and merge hourly tables

##Calories hourly data

```{r}
wellness_tech_calories_hourly$ActivityDate <- as.Date(wellness_tech_calories_hourly$ActivityHour, 
                                                     format = "%m/%d/%Y")

wellness_tech_calories_hourly$ActivityHour <- hour(mdy_hms(wellness_tech_calories_hourly$ActivityHour))
```

##Intensities hourly data

```{r}
wellness_tech_intensities_hourly$ActivityDate <- as.Date(wellness_tech_intensities_hourly$ActivityHour, format = "%m/%d/%Y")

wellness_tech_intensities_hourly$ActivityHour <- hour(mdy_hms(wellness_tech_intensities_hourly$ActivityHour))

```

##Activity hourly data

```{r}
wellness_tech_steps_hourly$ActivityDate <- as.Date(wellness_tech_steps_hourly$ActivityHour, format = "%m/%d/%Y")

wellness_tech_steps_hourly$ActivityHour <- hour(mdy_hms(wellness_tech_steps_hourly$ActivityHour))
```

##Merge hourly data

```{r}
data_list <- list(wellness_tech_calories_hourly, 
             wellness_tech_intensities_hourly, 
             wellness_tech_steps_hourly)

wellness_tech_data_hourly <- data_list %>% 
  
  reduce(full_join, by = c("Id", "ActivityHour", "ActivityDate"))
```

#Save the hourly data table

```{r}
write.csv(wellness_tech_data_hourly,"wellness_tech_data_hourly_R.csv", row.names = FALSE)
```

# Import minute data

```{r}
wellness_tech_calories_minute <- read_csv("minuteCaloriesNarrow_merged.csv")
wellness_tech_intensities_minute <- read_csv("minuteIntensitiesNarrow_merged.csv")
wellness_tech_steps_minute <- read_csv("minuteStepsNarrow_merged.csv")
wellness_tech_sleep_minute <- read_csv("minuteSleep_merged.csv")
wellness_tech_mets_minute <- read_csv("minuteMETsNarrow_merged.csv")
```

#Inspect the data

```{r}
head(wellness_tech_calories_minute)
head(wellness_tech_intensities_minute)
head(wellness_tech_steps_minute)
head(wellness_tech_sleep_minute)
head(wellness_tech_mets_minute)
```

```{r}
str(wellness_tech_calories_minute)
str(wellness_tech_intensities_minute)
str(wellness_tech_steps_minute)
str(wellness_tech_sleep_minute)
str(wellness_tech_mets_minute)
```

#Transform and merge minute tables

##Calories minute data

```{r}
wellness_tech_calories_minute$ActivityDate <- as.Date(wellness_tech_calories_minute$ActivityMinute, 
                                                     format = "%m/%d/%Y")

hour = hour(mdy_hms(wellness_tech_calories_minute$ActivityMinute))
minute = minute(mdy_hms(wellness_tech_calories_minute$ActivityMinute))
seconds = second(mdy_hms(wellness_tech_calories_minute$ActivityMinute))

wellness_tech_calories_minute$ActivityTime <- paste(hour, minute, seconds, sep = ":")
```

```{r}
wellness_tech_calories_minute <- wellness_tech_calories_minute %>% 
  
  select(Id, Calories, ActivityDate, ActivityTime)
```

##Intensities minute data

```{r}
wellness_tech_intensities_minute$ActivityDate <- as.Date(wellness_tech_intensities_minute$ActivityMinute, 
                                                     format = "%m/%d/%Y")

hour = hour(mdy_hms(wellness_tech_intensities_minute$ActivityMinute))
minute = minute(mdy_hms(wellness_tech_intensities_minute$ActivityMinute))
seconds = second(mdy_hms(wellness_tech_intensities_minute$ActivityMinute))

wellness_tech_intensities_minute$ActivityTime <- paste(hour, minute, seconds, sep = ":")
```

```{r}
wellness_tech_intensities_minute <- wellness_tech_intensities_minute %>% 
  
  select(Id, Intensity, ActivityDate, ActivityTime)
```

##Steps minute data

```{r}
wellness_tech_steps_minute$ActivityDate <- as.Date(wellness_tech_steps_minute$ActivityMinute, 
                                                     format = "%m/%d/%Y")

hour = hour(mdy_hms(wellness_tech_steps_minute$ActivityMinute))
minute = minute(mdy_hms(wellness_tech_steps_minute$ActivityMinute))
seconds = second(mdy_hms(wellness_tech_steps_minute$ActivityMinute))

wellness_tech_steps_minute$ActivityTime <- paste(hour, minute, seconds, sep = ":")
```

```{r}
wellness_tech_steps_minute <- wellness_tech_steps_minute %>% 
  
  select(Id, Steps, ActivityDate, ActivityTime)
```

##Sleep minute data

```{r}
wellness_tech_sleep_minute$ActivityDate <- as.Date(wellness_tech_sleep_minute$date, 
                                                     format = "%m/%d/%Y")

hour = hour(mdy_hms(wellness_tech_sleep_minute$date))
minute = minute(mdy_hms(wellness_tech_sleep_minute$date))
seconds = second(mdy_hms(wellness_tech_sleep_minute$date))

wellness_tech_sleep_minute$ActivityTime <- paste(hour, minute, seconds, sep = ":")
```

```{r}
wellness_tech_sleep_minute <- wellness_tech_sleep_minute %>% 
  
  select(Id, value, logId, ActivityDate, ActivityTime)
```

```{r}
data_list <- list(wellness_tech_calories_hourly, 
             wellness_tech_intensities_hourly, 
             wellness_tech_steps_hourly)

wellness_tech_data_hourly <- data_list %>% 
  
  reduce(full_join, by = c("Id", "ActivityHour", "ActivityDate"))
```

##Mets minute data

```{r}
wellness_tech_mets_minute$ActivityDate <- as.Date(wellness_tech_mets_minute$ActivityMinute, 
                                                     format = "%m/%d/%Y")

hour = hour(mdy_hms(wellness_tech_mets_minute$ActivityMinute))
minute = minute(mdy_hms(wellness_tech_mets_minute$ActivityMinute))
seconds = second(mdy_hms(wellness_tech_mets_minute$ActivityMinute))

wellness_tech_mets_minute$ActivityTime <- paste(hour, minute, seconds, sep = ":")
```

```{r}
wellness_tech_mets_minute <- wellness_tech_mets_minute %>% 
  
  select(Id, METs, ActivityDate, ActivityTime)
```

##Merge minute data

```{r}
data_list <- list(wellness_tech_calories_minute, 
             wellness_tech_intensities_minute, 
             wellness_tech_steps_minute,
             wellness_tech_sleep_minute,
             wellness_tech_mets_minute)

wellness_tech_data_minute <- data_list %>% 
  
  reduce(full_join, by = c("Id", "ActivityTime", "ActivityDate"))
```

#Save the minute data table

```{r}
write.csv(wellness_tech_data_minute,"wellness_tech_data_minute_R.csv", row.names = FALSE)
```

#Analyse the data

##Import the data

```{r}
wellness_tech_data_daily <- read_csv("wellness_tech_data_daily_R.csv")
wellness_tech_data_hourly <- read_csv("wellness_tech_data_hourly_R.csv")
wellness_tech_data_minute <- read_csv("wellness_tech_data_minute_R.csv")
```

##Identify and investigate unique values

```{r}
view(wellness_tech_data_daily %>% 
       
       summarise(count_id = length(unique(Id)), 
                 count_date = length(unique(ActivityDate))))

view(wellness_tech_data_hourly %>% 
       
       summarise(count_id = length(unique(Id)), 
                 count_date = length(unique(ActivityDate))))

view(wellness_tech_data_minute %>% 
       
       summarise(count_id = length(unique(Id)), 
                 count_date = length(unique(ActivityDate))))
```

```{r}
view(unique(wellness_tech_data_daily$ActivityDate))

view(unique(wellness_tech_data_hourly$ActivityDate))

view(unique(wellness_tech_data_minute$ActivityDate))
```

##Identify and remove na values

```{r}
view(colSums(is.na(wellness_tech_data_daily)))

view(colSums(is.na(wellness_tech_data_hourly)))

view(colSums(is.na(wellness_tech_data_minute)))
```

```{r}
wellness_tech_data_daily <- wellness_tech_data_daily[1:15]

wellness_tech_data_minute <- wellness_tech_data_minute[c(1:6, 9)]
```

```{r}
wellness_tech_data_minute <- na.omit(wellness_tech_data_minute)
```

#Visualise the data

##Correlation matrix daily data

```{r}
corr_matrix <- dplyr::select_if(wellness_tech_data_daily, is.numeric)

view(correlation <- cor(corr_matrix, use = "complete.obs"))
```

```{r}
plot_static = ggcorrplot(correlation, hc.order = TRUE, type = "lower")
```

```{r}
plot_interact = ggplotly(plot_static)
```

```{r}
saveWidget(plot_interact, "Correlation Matrix Daily Data.html")
```

##Correlation matrix hourly data

```{r}
corr_matrix <- dplyr::select_if(wellness_tech_data_hourly, is.numeric)

view(correlation <- cor(corr_matrix, use = "complete.obs"))
```

```{r}
plot_static = ggcorrplot(correlation, hc.order = TRUE, type = "lower")
```

```{r}
plot_interact = ggplotly(plot_static)
```

```{r}
saveWidget(plot_interact, "Correlation Matrix Hourly Data.html")
```

##Correlation matrix minute data

```{r}
corr_matrix <- dplyr::select_if(wellness_tech_data_minute, is.numeric)

view(correlation <- cor(corr_matrix, use = "complete.obs"))
```

```{r}
plot_static = ggcorrplot(correlation, hc.order = TRUE, type = "lower")
```

```{r}
plot_interact = ggplotly(plot_static)
```

```{r}
saveWidget(plot_interact, "Correlation Matrix Minute Data.html")
```

##Static boxplot daily data

```{r}
wellness_tech_data_daily %>% 

  ggplot(aes(x = as.factor(ActivityDate), y = TotalSteps)) + 
  
  geom_boxplot(aes(color = as.factor(ActivityDate))) + 
  
  geom_point(alpha = 0.25) + 
  
  stat_summary(fun = mean, geom = "point", shape = 2, size = 2, color = "blue") +
  
  stat_summary(fun = mean, geom = "smooth", group = 1, color = "blue") +
  
  scale_color_hue(name = "Activity Date") + 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 90)) +
  
  theme(plot.title = element_text(hjust = 0.5)) + 
  
  labs(title = "Total Steps Against Activity Date", 
         x = "Activity Date", 
         y = "Total Steps")

ggsave('Boxplot Total Steps Against Acivity Date.png',
       width=16,
       height=8)
```

```{r}
wellness_tech_data_daily %>% 
  
  ggplot(aes(x = as.factor(ActivityDate), y = SedentaryMinutes)) + 
  
  geom_boxplot(aes(color = as.factor(ActivityDate))) + 
  
  geom_point(alpha = 0.25) + 
  
  stat_summary(fun = mean, geom = "point", shape = 2, size = 2, color = "blue") +
  
  stat_summary(fun = mean, geom = "smooth", group = 1, color = "blue") +
  
  scale_color_hue(name = "Activity Date") + 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 90)) +
  
  theme(plot.title = element_text(hjust = 0.5)) + 
  
  labs(title = "Sedentary Minutes Against Activity Date", 
         x = "Activity Date", 
         y = "Sedentary Minutes")

ggsave('Boxplot Sedentary Minutes Against Acivity Date.png',
       width=16,
       height=8)
```

##Interactive boxplot daily data

```{r}
plot_static = wellness_tech_data_daily %>% 

  ggplot(aes(x = as.factor(ActivityDate), 
             y = TotalSteps, 
             text = paste0("Activity Date: ", as.factor(ActivityDate), 
                           "\nTotal Steps: ", TotalSteps))) + 
  
  geom_boxplot(aes(color = as.factor(ActivityDate))) + 
  
  geom_point(alpha = 0.25) + 
  
  #stat_summary(fun = mean, geom = "point", shape = 2, size = 2, color = "blue") +
  
  #stat_summary(fun = mean, geom = "smooth", group = 1, color = "blue") +
  
  scale_color_hue(name = "Activity Date") + 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 90)) +
  
  theme(plot.title = element_text(hjust = 0.5)) + 
  
  labs(title = "Total Steps Against Activity Date", 
         x = "Activity Date", 
         y = "Total Steps")
```

```{r}
plot_interact = ggplotly(plot_static, tooltip = "text")
```

```{r}
saveWidget(plot_interact, "Boxplot Total Steps Against Activity Date.html")
```

```{r}
plot_static = wellness_tech_data_daily %>% 
  
  ggplot(aes(x = as.factor(ActivityDate), 
             y = SedentaryMinutes, 
             text = paste0("Activity Date: ", as.factor(ActivityDate), 
                           "\nSedentary Minutes: ", SedentaryMinutes))) + 
  
  geom_boxplot(aes(color = as.factor(ActivityDate))) + 
  
  geom_point(alpha = 0.25) + 
  
  #stat_summary(fun = mean, geom = "point", shape = 2, size = 2, color = "blue") +
  
  #stat_summary(fun = mean, geom = "smooth", group = 1, color = "blue") +
  
  scale_color_hue(name = "Activity Date") + 
  
  theme_minimal() + 
  
  theme(axis.text.x = element_text(angle = 90)) +
  
  theme(plot.title = element_text(hjust = 0.5)) + 
  
  labs(title = "Sedentary Minutes Against Activity Date", 
         x = "Activity Date", 
         y = "Sedentary Minutes")
```

```{r}
plot_interact = ggplotly(plot_static, tooltip = "text")
```

```{r}
saveWidget(plot_interact, " Boxplot Sedentary Minutes Against Activity Date.html")
```

##Static boxplot hourly data

```{r}
wellness_tech_data_hourly %>% 
  
  ggplot(aes(x = as.factor(ActivityHour), y = StepTotal)) + 
  
  geom_boxplot(aes(color = as.factor(ActivityHour))) + 
  
  stat_summary(fun = mean, geom = "point", shape = 2, size = 2, color = "blue") +
  
  stat_summary(fun = mean, geom = "smooth", group = 1, color = "blue") +
  
  scale_color_hue(name = "Activity Hour") + 
  
  theme_minimal() + 
  
  theme(plot.title = element_text(hjust = 0.5)) + 
  
  labs(title = "Total Steps Against Activity Hour", 
         x = "Activity Hour", 
         y = "Total Steps")

ggsave('Boxplot Total Steps Against Acivity Hour.png',
       width=16,
       height=8)
```

##Interactive boxplot hourly data

```{r}
plot_static = wellness_tech_data_hourly %>% 
  
  ggplot(aes(x = as.factor(ActivityHour), 
             y = StepTotal, 
             text = paste0("Activity Hour: ", as.factor(ActivityHour), 
                           "\nTotal Steps: ", StepTotal))) + 
  
  geom_boxplot(aes(color = as.factor(ActivityHour))) + 
  
  #stat_summary(fun = mean, geom = "point", shape = 2, size = 2, color = "blue") +
  
  #stat_summary(fun = mean, geom = "smooth", group = 1, color = "blue") +
  
  scale_color_hue(name = "Activity Hour") + 
  
  theme_minimal() + 
  
  theme(plot.title = element_text(hjust = 0.5)) + 
  
  labs(title = "Total Steps Against Activity Hour", 
         x = "Activity Hour", 
         y = "Total Steps")
```

```{r}
plot_interact = ggplotly(plot_static, tooltip = "text")
```

```{r}
saveWidget(plot_interact, "Boxplot Total Steps Against Activity Hour.html")
```

#installed the tidyverse and here packages
install.packages("readODS")
install.packages("tidyverse") #installed the tidyverse and here packages
install.packages("dplyr")
install.packages("naniar")
install.packages("ggplot2")
install.packages("plotly")

#load the packages in R script
library(tidyverse)
library(dplyr)
library(naniar)
library(ggplot2)
library(readODS)
library(plotly)

#load dataframe 
data <- readODS::read_ods("raw data/sarh0114.ods", 2, range = "A6:O166")

#names of variables
names(data)

#view the first rows of raw data
head(data)

str(data) #view structure of data
glimpse(data) #view data types

#join columns
joined_columns <- data %>% 
  mutate(
    Year_Quarter = paste(Year, Quarter, sep = " ") #combine these Year and Quarter column
  ) %>% 
  select(year_quarter, everything(), -Year, -Quarter) #reorder columns and remove original column

str(joined_columns) #view structure of data
glimpse(joined_columns) #view data types?
view(joined_columns)

#convert variable as categorical
clean_data<- joined_columns %>% 
  mutate(
    Tasking_Location = as.factor(`Tasking Location Type`),
    Portland = as.numeric(Portland)
  )

str(clean_data)  

#view any missing variables and filter out any NA
colSums(is.na(clean_data))
#filter NA 
filtered_data<- clean_data %>% 
  select_if(~ !any(is.na(.)))

#reorder data and filter out         
reordered_data<- filtered_data %>% 
  select(year_quarter, 
         Tasking_Location,
         everything(),
         -`Tasking Location Type`,
  )    

#removal of subsets
subsets_data<- reordered_data %>% 
  filter(Tasking_Location != "Total") %>% #deleted total level
  select(-Total) #deleted total variable

#structure of data
view(subsets_data) 
str(subsets_data) 
glimpse(subsets_data)

#pivot the data
pivot_data<- subsets_data %>% 
  pivot_longer(
    cols = c(Caernarfon:Sumburgh),
    names_to = "base",
    values_to = "score"#excluded total and pivoted data
  )

#summary of data
summary(pivot_data)

#summary table
pivot_data %>%
  group_by(base) %>%              
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE),
    n = n(),
    min_score = min(score, na.rm = TRUE),
    max_score = max(score, na.rm = TRUE)
  )

# function to identify outliers
outlier_function <- function(x) {
  Quantile_1 <- quantile(x, 0.25, na.rm = TRUE)
  Quantile_3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Quantile_3-Quantile_1
  lower <- Quantile_1 - 1.5*IQR
  upper <- Quantile_3 + 1.5*IQR
  return(x[x < lower | x > upper])
}

#identified outliers
outlier_function(pivot_data$score)

#function to filter outliers and return a non-outlers
outlier_fun <- function(x) {
  Quantile_1 <- quantile(x, 0.25, na.rm = TRUE)
  Quantile_3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Quantile_3-Quantile_1
  lower <- Quantile_1 - 1.5*IQR
  upper <- Quantile_3 + 1.5*IQR
  return(x[x >= lower | x <= upper]) 
}

#removed outliers
outlier_fun(pivot_data$score)



#name the cleaned data
df<- outlier_fun(pivot_data$score)
#saved cleaned data
clean_df <- pivot_data[pivot_data$score %in% df, ]

#view the strucutres and data types
view(clean_df)
str(clean_df)
glimpse(clean_df)

# summary table of cleaned data
clean_df %>%
  group_by(base) %>%              
  summarise(
    mean_score = mean(score, na.rm = TRUE),
    sd_score = sd(score, na.rm = TRUE),
    n = n(),
    min_score = min(score, na.rm = TRUE),
    max_score = max(score, na.rm = TRUE)
  )

#summary of cleaned data
summary(clean_df)

#rename the dataframe
processed_df<- clean_df

#view first few rows of processed data
head(processed_df)

#save copy of processed data
write.csv(processed_df, "processed_df.csv", row.names = FALSE)

#visualisation of stacked bar graph 
p_bar<- ggplot(processed_df, 
               aes(x = factor(year_quarter),
                   y = score,
                   fill = base))+
  geom_bar(stat = "identity",
           position = "stack")+
  facet_wrap(~ Tasking_Location)+
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5))
ggplotly(p_bar, tooltip = "text")


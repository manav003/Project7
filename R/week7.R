# R Studio API Code
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(lubridate)
library(ggplot2)
library(GGally)
# Data Import and Cleaning
week7_tbl <- as.tibble(read_csv(file = "../data/week3.csv")) %>% 
  mutate(timeStart = ymd_hms(timeStart), timeEnd = ymd_hms(timeEnd)) %>%  
  mutate(condition = factor(condition, levels = c("A", "B", "C"), labels = c("Block A", "Block B", "Control")), gender = factor(gender, levels = c("M", "F"), labels = c("Male", "Female"))) %>% 
  
  
  filter(q6 == 1) %>% 
  select(-q6)

# Visualization
ggpairs(week7_tbl[,5:13]) #???
ggplot(week7_tbl, aes(x=timeStart, y= q1)) + 
  geom_point() +
  labs(x = "Date of Experiment", y = "Q1 Score")

ggplot(week7_tbl, aes(x= q1, y = q2, col = gender)) +
  geom_jitter()
ggplot(week7_tbl, aes(x = q1, y = q2)) +
  geom_jitter() +
  facet_grid(. ~ gender) +
  labs(x = "Score on Q1", y = "Score on Q2")

ggplot(week7_tbl, aes(x = gender, y = 60 * (timeEnd - timeStart))) +
  geom_boxplot() +
  labs(x = "Gender", y = "Time Elapsed (secs)")


ggplot(week7_tbl, aes(x = q5, y = q7, col = condition)) + 
  geom_jitter(width = 0.05) + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Score on Q5", y = "Score on Q7", col = "Experimental Condition") + 
  theme(legend.position = "bottom", panel.background = element_rect(fill = "white"), legend.background = element_rect(fill = "grey87"))

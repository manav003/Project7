# R Studio API Code
library(rstudioapi)
setwd(dirname(getActiveDocumentContext()$path))

# Libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

# Data Import and Cleaning
week7_tbl <- as.tibble(read_csv(file = "../data/week3.csv")) %>% 
  mutate(timeStart = ymd_hms(timeStart), timeEnd = ymd_hms(timeEnd)) %>%  
  mutate(condition = factor(condition, levels = c("A", "B", "C"), labels = c("Block A", "Block B", "Control")), gender = factor(gender, levels = c("F", "M"), labels = c("Female", "Male"))) %>% 
  
  
  filter(q6 == 1) %>% 
  select(-q6)

# Visualization




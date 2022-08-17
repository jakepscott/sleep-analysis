# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(janitor)

# Load data ---------------------------------------------------------------
df_raw <- read_csv(here("data/sleep-tracker.csv"))
df <- df_raw %>% 
  clean_names() %>% 
  select(date:hours)

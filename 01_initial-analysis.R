# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(glue)

# Load data ---------------------------------------------------------------
df_raw <- read_csv(here("data/sleep-tracker.csv"), col_types = c("cccc"))
df <- df_raw %>% 
  clean_names() %>% 
  select(date:hours) %>% 
  mutate(date = mdy(date),
         wake = case_when(nchar(wake) %in% c(1,2) ~ glue("{wake}:00"),
                          T ~ wake),
         sleep_first_two = case_when(nchar(sleep) == 5 ~ as.double(substr(sleep, 1,2)),
                                     nchar(sleep) == 4 ~ as.double(substr(sleep, 1,1))),
         sleep = case_when(sleep_first_two >= 8 & sleep_first_two <= 11 ~ glue("{sleep} PM"),
                           T ~ glue("{sleep} AM")),
         wake_first_two = case_when(nchar(wake) == 5 ~ as.double(substr(wake, 1,2)),
                                    nchar(wake) == 4 ~ as.double(substr(wake, 1,1))),
         wake = case_when(wake_first_two < 12 ~ glue("{wake} AM"),
                          T ~ glue("{wake} PM")),
         sleep_test  = parse_date_time(sleep, '%I:%M %p'),
         wake_test  = parse_date_time(wake, '%I:%M %p'),
         wake_test = case_when(str_detect(sleep, "PM") & str_detect(wake, "AM") ~ wake_test + days(1),
                               T ~ wake_test),
         hours_test = difftime(wake_test, sleep_test)) %>%
  select(-contains("first")) 
         
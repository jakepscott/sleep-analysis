# Load Libs ---------------------------------------------------------------
library(tidyverse)
library(here)
library(janitor)
library(lubridate)
library(glue)
library(zoo)
library(ggiraph)
theme_set(theme_bw())

# Load data ---------------------------------------------------------------
df_raw <- read_csv(here("data/sleep-tracker.csv"), col_types = c("cccc"))
df <- df_raw %>% 
  clean_names() %>% 
  select(date:hours) %>% 
  rename(hours_org = hours) %>% 
  mutate(date = as.Date(mdy(date)),
         wake = case_when(nchar(wake) %in% c(1,2) ~ glue("{wake}:00"),
                          T ~ wake),
         sleep_first_two = case_when(nchar(sleep) == 5 ~ as.double(substr(sleep, 1,2)),
                                     nchar(sleep) == 4 ~ as.double(substr(sleep, 1,1))),
         sleep = case_when(sleep_first_two >= 8 & sleep_first_two <= 11 ~ glue("{sleep} PM"),
                           date == as.Date("2021-05-10") ~ glue("{sleep} AM"), # One day when I went to bed at 10am
                           T ~ glue("{sleep} AM")),
         wake_first_two = case_when(nchar(wake) == 5 ~ as.double(substr(wake, 1,2)),
                                    nchar(wake) == 4 ~ as.double(substr(wake, 1,1))),
         wake = case_when(wake_first_two < 12 & wake_first_two > 2 ~ glue("{wake} AM"),
                          date == as.Date("2021-05-10") ~ glue("{wake} PM"), # One day when I woke up at 2pm
                          T ~ glue("{wake} PM")),
         sleep_test  = parse_date_time(sleep, '%I:%M %p'),
         wake_test  = parse_date_time(wake, '%I:%M %p'),
         wake_test = case_when(str_detect(sleep, "PM") & str_detect(wake, "AM") ~ wake_test + days(1),
                               T ~ wake_test),
         hours = case_when(# Account for day I went to bed at 10am and woke up at 2pm
                           date == as.Date("2021-05-10") ~ difftime(sleep_test, wake_test),
                           T ~ difftime(wake_test, sleep_test))) %>%
  select(-contains("first")) %>% 
  mutate(avg_sleep = rollmean(hours, k = 7, align = "right", fill = NA),
         year = year(date))


# Simple plot -------------------------------------------------------------
df %>% 
  ggplot(aes(date)) +
  geom_col_interactive(aes(y = hours, tooltip = glue("{date}\n{hours}")),
                       fill = "lightblue") +
  geom_line(aes(y = avg_sleep), 
            color = "red",
            lwd = 1) +
  geom_hline(yintercept = 8, color = "black", linetype = "dashed")


# By year -----------------------------------------------------------------
df %>% 
  ggplot(aes(date)) +
  geom_col_interactive(aes(y = hours, tooltip = glue("{date}\n{hours}")),
                       fill = "lightblue") +
  geom_line(aes(y = avg_sleep), 
            color = "red",
            lwd = 1) +
  geom_hline(yintercept = 8, color = "black", linetype = "dashed") +
  facet_wrap(~ year, scales = "free_x", ncol = 1)


# over 8 ------------------------------------------------------------------
df %>% 
  mutate(diff_from_8 = hours - 8) %>% 
  ggplot(aes(date, diff_from_8)) +
  geom_col(aes(fill = diff_from_8 > 0))


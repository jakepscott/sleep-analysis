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
         year = year(date),
         day_of_week = weekdays(date),
         day_of_week = factor(day_of_week,
                      levels = c("Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")),
         day_of_month = day(date),
         month = month(date),
         month = factor(month,
                        levels = c(1:12),
                        labels = month.name))


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


# by day of week ----------------------------------------------------------
df %>% 
  group_by(day_of_week) %>% 
  summarise(mean = mean(hours)) %>% 
  ggplot(aes(day_of_week, mean)) +
  geom_col()


# 2022 heatmap ------------------------------------------------------------
df %>% 
  ggplot() +
  geom_tile(aes(x = day_of_month, y = fct_rev(month), fill = as.numeric(hours))) +
  # Add viridis colors
  scale_fill_viridis_c(option = "inferno") + 
  scale_x_continuous(breaks=seq(1,31,by=2)) +
  # Force all the tiles to have equal widths and heights
  coord_equal() +
  # Add nice labels
  labs(x = "Day of the month", y = NULL,
       title = "Hours of Sleep per Night",
       fill = "Hours of \nsleep",
       caption = "Plot: @jakepscott2020") +
  facet_wrap(~year, ncol=1) +
  # Use a cleaner theme
  theme_minimal(base_family = "Roboto Condensed", base_size = 12) +
  theme(panel.grid = element_blank(),
        # Bold, bigger title
        plot.title = element_text(face = "bold", size = rel(1.2)),
        # Plain, slightly bigger subtitle that is grey
        plot.subtitle = element_text(face = "plain", size = rel(.9), color = "grey70"),
        # Italic, smaller, grey caption that is left-aligned
        plot.caption = element_text(face = "italic", size = rel(0.9), 
                                    color = "grey70"),
        # Bold legend titles
        legend.title = element_text(size = rel(.8)),
        # Bold, slightly larger facet titles that are left-aligned for the sake of repetition
        strip.text = element_text(face = "bold", size = rel(.7), hjust = 0),
        strip.background.x = element_blank(),
        axis.title = element_blank(),
        # Add a light grey background to the facet titles, with no borders
        strip.background = element_blank(),
        # Add a thin grey border around all the plots to tie in the facet titles
        panel.border = element_rect(color = "grey90", fill = NA),
        plot.title.position = "plot",
        legend.key = element_rect(fill = "white", colour = "white"))

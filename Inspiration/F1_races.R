#load libraries
library(tidyverse)
library(tidytuesdayR)
library(ggtext)
library(maielitheme)
library(lubridate)
library(gghighlight)

#load data
tuesdata <- tidytuesdayR::tt_load(2021, week = 37)
drivers <- tuesdata$drivers
races <- tuesdata$races
results <- tuesdata$results

#get current point system into a tibble
curr_points_system <- results %>% 
  filter(raceId == max(raceId)) %>% 
  filter(points > 0) %>% 
  select(positionOrder, points)

#combine results with current points system, driver DOB, and race date
f1_points_age <- results %>% 
  left_join(races, by = "raceId") %>% 
  left_join(drivers, by = "driverId") %>% 
  unite(driver_name, c("forename", "surname"), sep = " ") %>% 
  left_join(curr_points_system) %>% 
  mutate(points = replace_na(points, 0)) %>% 
  mutate(age = as.double(date - dob)/365.25) %>% 
  select(resultId, driver_name, points, age) %>%
  arrange(age) %>% 
  group_by(driver_name) %>% 
  mutate(cumulative_points = cumsum(points)) %>% 
  select(-points)


f1_points_age %>% 
  filter(driver_name == "Max Verstappen") %>% 
  summarise(max(age))

f1_points_age %>% 
  ggplot(aes(x = age, y = cumulative_points)) +
  scale_x_continuous(breaks = c(20, 30, 40, 50), labels = paste(seq(20, 50, by = 10), "years old")) +
  geom_line(aes(group = driver_name, color = driver_name)) +
  scale_color_manual(values = c("Max Verstappen" = "#FF9B00",
                                "Lewis Hamilton" = "#00D2BE",
                                "Sebastian Vettel" = "#DC0000")) +
  annotate("text", y = 1400, x = 23, label = "Max Verstappen", family = "Roboto Condensed", fontface = "bold", color = "#FF9B00", vjust = 0, hjust = 1, lineheight = 1) +
  annotate("text", y = 3950, x = 37.5, label = "Lewis Hamilton", family = "Roboto Condensed", fontface = "bold", color = "#00D2BE", vjust = 0, hjust = 0, lineheight = 1) +
  annotate("text", y = 3000, x = 35, label = "Sebastian Vettel", family = "Roboto Condensed", fontface = "bold", color = "#DC0000", vjust = 0, hjust = 0, lineheight = 1) +
  annotate("text", y = 4000, x = 24.2, label = "Before the age of 24,\nMax Verstappen has earned\n1,386.5 points.\n\nBy 24, Sebastian Vettel\nearned 567 points, and\nLewis Hamilton earned\n207 points.", family = "Roboto Condensed", color = "#292929", vjust = 1, hjust = 0, lineheight = 1) +
  geom_vline(xintercept = 23.9, size = .5, color = "#f77f00", linetype="dashed") +
  annotate("point", y = 1386, x = 23.9, color = "#FF9B00", size = 3) +
  annotate("point", y = 567, x = 23.9, color = "#DC0000", size = 3) +
  annotate("point", y = 207, x = 23.9, color = "#00D2BE", size = 3) +
  labs(x = "Driver Age",
       y = "Cumulative F1 Points",
       title = "Will <span style = 'color:#FF9B00;'>Max Verstappen's</span> quick rise at a young age lead him to <br>be the all time points leader in Formula 1 history?",
       subtitle = "The chart below shows the cumulative points earned by age for each F1 driver since 1950.<br>Lewis Hamilton and Sebastian Vettel have the highest point totals in F1 history. Where will Max end up when he is their age?",
       caption = "note: points are based on the current points scoring system and do not include
       points for fastest lap or sprint qualifying.
       
       data source: Ergast API via Sara Stoudt and Data is Plural
       analysis and visualization for #tidytuesday by @mikemaieli") +
  theme_mm_light() +
  theme(plot.title = element_markdown(),
        plot.subtitle = element_markdown(),
        legend.position = "none")
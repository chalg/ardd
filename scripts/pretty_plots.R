# Load Libraries ----
# Spruce up a couple of plots from arrd.rmd
# Load required libraries
library(tidyverse)   # core Tidyverse - Loads dplyr & purrr
library(ggridges)    # https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(janitor)     # simple functions for examining and cleaning dirty data
library(scales)      # provides internal scaling infrastructure used by ggplot2
library(patchwork)   # makes combining separate ggplots ridiculously simple

# Set a consistent ggplot theme
theme_set(theme_minimal())
library(showtext)
library(ggtext)

showtext_opts(dpi = 300)
showtext_auto(enable = TRUE)

font_add_google("Fira Sans Condensed", "fira sans")
theme_set(theme_bw(base_size = 12, base_family = "fira sans"))

font_add_google("Oswald", "oswald")
# font_add_google("Open Sans", "open")
font_add_google("Cabin", "cabin")
font_add_google("Acme", "acme")

# Set colours for some plots
my_colours <- c("#CC79A7", "#56B4E9", "#009E73", "salmon", "peru", "peachpuff4")

# Read data ----

ardd_fatalities <- read_csv("data/ardd_fatalities_apr2023.csv")
ardd_fatal_crash <- read_csv("data/ardd_fatal_crashes_apr2023.csv")
est_pop <- read_csv("~/R/ardd/data/estimated_population.csv")
state_area <- read_csv("~/R/ardd/data/state_area.csv")

glimpse(ardd_fatalities)
glimpse(ardd_fatal_crash)

# Join datasets
fatal <- ardd_fatal_crash %>% left_join(ardd_fatalities)

# Data Wrangling ----

# Clean if necessary and fix column names via janitor package
fatal <- fatal %>% remove_empty(which = c("rows", "cols")) %>% 
  clean_names()

# Perform overview of variables
glimpse(fatal)

# Create new column to contain hour number, which is easier to analyse,
fatal <- fatal %>% 
  separate(time, c("hour", NA), ":", remove = F) %>% 
  mutate(hour = hour %>% as.numeric())

fatal

fatal %>% get_dupes(crash_id) %>% head(25) %>% print(n = 25)

# Remove duplicates for certain visualisations
fatal_unique <- fatal %>% distinct(crash_id, .keep_all = TRUE)

# Use fatal tibble, otherwise atributes will be removed for each crash_id that we need here
fatal %>% filter(!gender == "Unspecified",
                 age >= 16, !gender == -9,
                 road_user %in% c("Driver", "Motorcycle rider")) %>% 
  group_by(age, gender) %>% 
  summarise(f_count = n()) %>%    # must use count, otherwise overstated
  ungroup() %>% 
  arrange(age, gender) %>% 
  ggplot(aes(x = age, y = f_count)) +
  coord_cartesian(xlim = c(16, 100), ylim = c(1, 900)) +
  geom_line(aes(colour = gender), size = 1) +
  scale_x_continuous(breaks = seq(16, 100, 4)) +
  scale_y_continuous(breaks = seq(0, 900, 50)) +
  scale_colour_manual(values = my_colours) +
  labs(x = "Age", y = "Number of Fatalities",
       title = 'Australian Driver and Motor Cycle Rider Fatalities by Age and Gender',
       subtitle = "Statistics for <span style='color:#56B4E9'>**Males**</span> and <span style='color:#CC79A7'>**Females**</span>, 1989-2023",
       # subtitle = "1989-2023",
       colour = "Gender",
       caption = "DatViz: @GrantChalmers | Source: Australian Road Deaths Database (ARDD)
       https://data.gov.au/dataset/ds-dga-5b530fb8-526e-4fbf-b0f6-aa24e84e4277/details?q=crash") +
  theme(plot.title = element_text(color = "gray12", family = "oswald"),
        # plot.subtitle = element_text(size = 10, color = "gray20"),
        plot.subtitle = element_markdown(size = 9, family = "cabin"),
        axis.title = element_text(size = 10), legend.position = "none",
        axis.text.x = element_text(angle = 0, size = 9),
        plot.caption = element_text(size = 7.25, color = "gray50", family = "oswald"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

ggsave("age_gender_fatalities_pretty.png", path = "images", width = 7, height = 5)

# Drivers only

fatal %>% filter(!gender == "Unspecified",
                 age >= 16, !gender == -9,
                 road_user %in% c("Driver")) %>% 
  group_by(age, gender) %>% 
  summarise(f_count = n()) %>%    # must use count, otherwise overstated
  ungroup() %>% 
  arrange(age, gender) %>% 
  ggplot(aes(x = age, y = f_count)) +
  coord_cartesian(xlim = c(16, 100), ylim = c(1, 750)) +
  geom_line(aes(colour = gender), size = 1) +
  scale_x_continuous(breaks = seq(16, 100, 4)) +
  scale_y_continuous(breaks = seq(0, 750, 50)) +
  scale_colour_manual(values = my_colours) +
  labs(x = "Age", y = "Number of Fatalities",
       title = 'Australian Driver Fatalities by Age and Gender',
       subtitle = "Statistics for <span style='color:#56B4E9'>**Males**</span> and <span style='color:#CC79A7'>**Females**</span>, 1989-2023",
       # subtitle = "1989-2023",
       colour = "Gender",
       caption = "DatViz: @GrantChalmers | Source: Australian Road Deaths Database (ARDD)
       https://data.gov.au/dataset/ds-dga-5b530fb8-526e-4fbf-b0f6-aa24e84e4277/details?q=crash") +
  theme(plot.title = element_text(color = "gray12", family = "oswald"),
        # plot.subtitle = element_text(size = 10, color = "gray20"),
        plot.subtitle = element_markdown(size = 9, family = "cabin"),
        axis.title = element_text(size = 10), legend.position = "none",
        axis.text.x = element_text(angle = 0, size = 9),
        plot.caption = element_text(size = 7.25, color = "gray50", family = "oswald"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

ggsave("age_gender_driver_fatalities_pretty.png", path = "images", width = 7, height = 5)

# Histograms

# Review count of fatalities by year, excluding 2023
hist3 <- fatal %>%
  filter(year < 2023) %>% 
  ggplot(aes(x = year)) +
  # scale_x_continuous(limits = c(1989, 2022), breaks = seq(1989, 2022, 2)) +
  geom_histogram(binwidth = 1, fill = my_colours[1], alpha = 0.70) +
  labs(x = "Year", y = "Count",
       title = 'Australian Road Fatalities (1989-2022)') +
  # scale_fill_manual(values = my_colours) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "bottom",
        axis.text.x = element_text(angle = 0, size = 9),
        strip.text.x = element_text(size = 10, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 8.5, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

# Review count of fatalities by hour
hist4 <- fatal %>%
  filter(year < 2023,
         between(hour, 0, 23)) %>% 
  ggplot(aes(x = hour)) +
  scale_x_continuous(breaks = seq(0, 23, 3)) +
  geom_histogram(binwidth = 1, fill = my_colours[2], alpha = 0.70) +
  labs(x = "Hour", y = NULL) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "bottom",
        axis.text.x = element_text(angle = 0, size = 9),
        strip.text.x = element_text(size = 10, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 8.5, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

# Order the days correctly
fatal$dayweek <- factor(fatal$dayweek,
                        levels = c("Monday", "Tuesday",
                                   "Wednesday", "Thursday",
                                   "Friday", "Saturday",
                                   "Sunday"))

# Review count of fatalities by Day of the week
bar1 <- fatal %>% 
  filter(year < 2023) %>% 
  ggplot(aes(x = dayweek)) +
  geom_bar(fill = my_colours[3], alpha = 0.70) +
  labs(x = "Day of Week", y = "Count",
       caption = "DatViz: @GrantChalmers | Source: Australian Road Deaths Database (ARDD)
       https://data.gov.au/dataset/ds-dga-5b530fb8-526e-4fbf-b0f6-aa24e84e4277/details?q=crash") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "bottom",
        axis.text.x = element_text(angle = 00, size = 9),
        strip.text.x = element_text(size = 10, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 7, color = "gray50", family = "oswald"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

(hist3 | hist4) / bar1

ggsave("fatal_histograms_patchwork.png", path = "images", width = 7, height = 5)

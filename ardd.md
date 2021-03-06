Exploratory Data Analysis of Australian Road Fatalities
================
Grant Chalmers
Created 22 Feb, 2020; Last Update 27 February, 2020

  - [Preparation](#preparation)
      - [Setup](#setup)
      - [Read in data](#read-in-data)
  - [Data Description](#data-description)
      - [Data Cleaning](#data-cleaning)
  - [Visualisations](#visualisations)
      - [Histograms](#histograms)
      - [Boxplots](#boxplots)
      - [Lineplots (trends)](#lineplots-trends)
      - [Bespoke Visualisations](#bespoke-visualisations)

## Preparation

### Setup

``` r
# Load required libraries
library(tidyverse)   # core Tidyverse - Loads dplyr & purrr
library(lubridate)   # date & time manipulation
library(ggridges)    # https://cran.r-project.org/web/packages/ggridges/vignettes/introduction.html
library(janitor)     # simple functions for examining and cleaning dirty data
library(scales)      # provides internal scaling infrastructure used by ggplot2
library(patchwork)   # makes combining separate ggplots ridiculously simple

# Set a consistent ggplot theme
theme_set(theme_minimal())
```

### Read in data

``` r
# Load the Data
ardd_fatalities <- read_csv("~/R/ardd/data/ardd_fatalities.csv")
ardd_fatal_crash <- read_csv("~/R/ardd/data/ardd_fatal_crashes.csv")
est_pop <- read_csv("~/R/ardd/data/estimated_population.csv")
state_area <- read_csv("~/R/ardd/data/state_area.csv")

glimpse(ardd_fatalities)
```

    #> Observations: 51,001
    #> Variables: 23
    #> $ `Crash ID`                      <dbl> 20203010, 20201006, 20203001, ...
    #> $ State                           <chr> "Qld", "NSW", "Qld", "Vic", "Q...
    #> $ Month                           <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    #> $ Year                            <dbl> 2020, 2020, 2020, 2020, 2020, ...
    #> $ Dayweek                         <chr> "Sunday", "Sunday", "Friday", ...
    #> $ Time                            <drtn> 00:00:00, 04:25:00, 23:00:00,...
    #> $ `Crash Type`                    <chr> "Single", "Single", "Pedestria...
    #> $ `Bus Involvement`               <chr> "No", "No", "No", "-9", "No", ...
    #> $ `Heavy Rigid Truck Involvement` <chr> "No", "No", "No", "-9", "No", ...
    #> $ `Articulated Truck Involvement` <chr> "No", "No", "No", "-9", "No", ...
    #> $ `Speed Limit`                   <dbl> 100, 110, 10, -9, 70, 100, -9,...
    #> $ `Road User`                     <chr> "Passenger", "Passenger", "Ped...
    #> $ Gender                          <chr> "Male", "Male", "Male", "Male"...
    #> $ Age                             <dbl> 16, 19, 17, 39, 27, 27, 25, 39...
    #> $ `National Remoteness Areas`     <chr> "Inner Regional Australia", "M...
    #> $ `SA4 Name 2016`                 <chr> "Darling Downs - Maranoa", "Sy...
    #> $ `National LGA Name 2017`        <chr> "Toowoomba (R)", "Wollondilly"...
    #> $ `National Road Type`            <chr> "Sub-arterial Road", "National...
    #> $ `Christmas Period`              <chr> "No", "No", "No", "No", "Yes",...
    #> $ `Easter Period`                 <chr> "No", "No", "No", "No", "No", ...
    #> $ `Age Group`                     <chr> "0_to_16", "17_to_25", "17_to_...
    #> $ `Day of week`                   <chr> "Weekend", "Weekend", "Weekend...
    #> $ `Time of day`                   <chr> "Night", "Night", "Night", "Ni...

``` r
glimpse(ardd_fatal_crash)
```

    #> Observations: 45,876
    #> Variables: 20
    #> $ `Crash ID`                      <dbl> 20202014, 20205007, 20205002, ...
    #> $ State                           <chr> "Vic", "WA", "WA", "WA", "NSW"...
    #> $ Month                           <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, ...
    #> $ Year                            <dbl> 2020, 2020, 2020, 2020, 2020, ...
    #> $ Dayweek                         <chr> "Tuesday", "Saturday", "Wednes...
    #> $ Time                            <drtn> 22:00:00, 14:40:00, 10:00:00,...
    #> $ `Crash Type`                    <chr> "Pedestrian", "Multiple", "Sin...
    #> $ `Number Fatalities`             <dbl> 1, 2, 1, 2, 1, 1, 1, 1, 1, 1, ...
    #> $ `Bus \nInvolvement`             <chr> "-9", "No", "No", "No", "No", ...
    #> $ `Heavy Rigid Truck Involvement` <chr> "-9", "No", "No", "No", "No", ...
    #> $ `Articulated Truck Involvement` <chr> "-9", "No", "No", "No", "No", ...
    #> $ `Speed Limit`                   <dbl> -9, 110, 110, 110, 70, 50, 110...
    #> $ `National Remoteness Areas`     <chr> "Inner Regional Australia", NA...
    #> $ `SA4 Name 2016`                 <chr> "Latrobe - Gippsland", NA, NA,...
    #> $ `National LGA Name 2017`        <chr> "Bass Coast (S)", NA, NA, NA, ...
    #> $ `National Road Type`            <chr> "Arterial Road", NA, NA, NA, "...
    #> $ `Christmas Period`              <chr> "No", "No", "No", "No", "No", ...
    #> $ `Easter Period`                 <chr> "No", "No", "No", "No", "No", ...
    #> $ `Day of week`                   <chr> "Weekday", "Weekend", "Weekday...
    #> $ `Time of Day`                   <chr> "Night", "Day", "Day", "Day", ...

``` r
# Join datasets
fatal <- ardd_fatal_crash %>% left_join(ardd_fatalities)
```

## Data Description

The Australian Road Deaths Database (ARDD) is maintained and published
by the Bureau of Infrastructure, Transport and Regional Economics
(BITRE). It commenced in 1989 and is updated on a monthly basis. The
ARDD contains basic demographic and crash details of people who have
died in an Australian road crash. Every fatal road traffic crash in
Australia is in scope, and information is included for all people who
were killed.

It is published in two forms:

  - Fatalities: each record is a killed person
  - Crashes: each record is a fatal crash

The database can be found
[here](https://bitre.gov.au/statistics/safety/fatal_road_crash_database.aspx),
csv files can be found
[here](https://data.gov.au/dataset/ds-dga-5b530fb8-526e-4fbf-b0f6-aa24e84e4277/details?q=crash),
data dictionary can be found
[here](https://bitre.gov.au/statistics/safety/files/ARDD_Dictionary_V3.pdf).

Estimated population can be sourced from
[here](https://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3101.0Jun%202019?OpenDocument)
(TABLE 4. Estimated Resident Population, States and Territories
(Number))

The data is already in a tidy format.

### Data Cleaning

``` r
# Clean if necessary and fix column names via janitor package
fatal <- fatal %>% remove_empty(which = c("rows", "cols")) %>% 
  clean_names()

# Perform overview of variables
glimpse(fatal)
```

    #> Observations: 50,985
    #> Variables: 26
    #> $ crash_id                      <dbl> 20202014, 20205007, 20205007, 20...
    #> $ state                         <chr> "Vic", "WA", "WA", "WA", "WA", "...
    #> $ month                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    #> $ year                          <dbl> 2020, 2020, 2020, 2020, 2020, 20...
    #> $ dayweek                       <chr> "Tuesday", "Saturday", "Saturday...
    #> $ time                          <drtn> 22:00:00, 14:40:00, 14:40:00, 1...
    #> $ crash_type                    <chr> "Pedestrian", "Multiple", "Multi...
    #> $ number_fatalities             <dbl> 1, 2, 2, 1, 2, 2, 1, 1, 1, 1, 1,...
    #> $ bus_involvement               <chr> "-9", "No", "No", "No", "No", "N...
    #> $ heavy_rigid_truck_involvement <chr> "-9", "No", "No", "No", "No", "N...
    #> $ articulated_truck_involvement <chr> "-9", "No", "No", "No", "No", "N...
    #> $ speed_limit                   <dbl> -9, 110, 110, 110, 110, 110, 70,...
    #> $ national_remoteness_areas     <chr> "Inner Regional Australia", NA, ...
    #> $ sa4_name_2016                 <chr> "Latrobe - Gippsland", NA, NA, N...
    #> $ national_lga_name_2017        <chr> "Bass Coast (S)", NA, NA, NA, NA...
    #> $ national_road_type            <chr> "Arterial Road", NA, NA, NA, NA,...
    #> $ christmas_period              <chr> "No", "No", "No", "No", "No", "N...
    #> $ easter_period                 <chr> "No", "No", "No", "No", "No", "N...
    #> $ day_of_week                   <chr> "Weekday", "Weekend", "Weekend",...
    #> $ time_of_day                   <chr> "Night", "Day", "Day", "Day", "D...
    #> $ bus_involvement_2             <chr> "-9", "No", "No", "No", "No", "N...
    #> $ road_user                     <chr> "Pedestrian", "Driver", "Passeng...
    #> $ gender                        <chr> "Male", "Female", "Female", "Mal...
    #> $ age                           <dbl> 52, 64, 95, 21, 37, 41, 67, 56, ...
    #> $ age_group                     <chr> "40_to_64", "40_to_64", "75_or_o...
    #> $ time_of_day_2                 <chr> "Night", "Day", "Day", "Day", "D...

``` r
# Create new column to contain hour number, which is easier to analyse, 
# duplicated columns created in join function previously
fatal <- fatal %>% mutate(hour = hour(fatal$time)) %>% 
  select(-ends_with("_2")) %>% glimpse()
```

    #> Observations: 50,985
    #> Variables: 25
    #> $ crash_id                      <dbl> 20202014, 20205007, 20205007, 20...
    #> $ state                         <chr> "Vic", "WA", "WA", "WA", "WA", "...
    #> $ month                         <dbl> 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,...
    #> $ year                          <dbl> 2020, 2020, 2020, 2020, 2020, 20...
    #> $ dayweek                       <chr> "Tuesday", "Saturday", "Saturday...
    #> $ time                          <drtn> 22:00:00, 14:40:00, 14:40:00, 1...
    #> $ crash_type                    <chr> "Pedestrian", "Multiple", "Multi...
    #> $ number_fatalities             <dbl> 1, 2, 2, 1, 2, 2, 1, 1, 1, 1, 1,...
    #> $ bus_involvement               <chr> "-9", "No", "No", "No", "No", "N...
    #> $ heavy_rigid_truck_involvement <chr> "-9", "No", "No", "No", "No", "N...
    #> $ articulated_truck_involvement <chr> "-9", "No", "No", "No", "No", "N...
    #> $ speed_limit                   <dbl> -9, 110, 110, 110, 110, 110, 70,...
    #> $ national_remoteness_areas     <chr> "Inner Regional Australia", NA, ...
    #> $ sa4_name_2016                 <chr> "Latrobe - Gippsland", NA, NA, N...
    #> $ national_lga_name_2017        <chr> "Bass Coast (S)", NA, NA, NA, NA...
    #> $ national_road_type            <chr> "Arterial Road", NA, NA, NA, NA,...
    #> $ christmas_period              <chr> "No", "No", "No", "No", "No", "N...
    #> $ easter_period                 <chr> "No", "No", "No", "No", "No", "N...
    #> $ day_of_week                   <chr> "Weekday", "Weekend", "Weekend",...
    #> $ time_of_day                   <chr> "Night", "Day", "Day", "Day", "D...
    #> $ road_user                     <chr> "Pedestrian", "Driver", "Passeng...
    #> $ gender                        <chr> "Male", "Female", "Female", "Mal...
    #> $ age                           <dbl> 52, 64, 95, 21, 37, 41, 67, 56, ...
    #> $ age_group                     <chr> "40_to_64", "40_to_64", "75_or_o...
    #> $ hour                          <int> 22, 14, 14, 10, 15, 15, 14, 12, ...

``` r
# There are quite a few duplicates
# fatal %>% get_dupes(crash_id)
# Remove
fatal_unique <- fatal %>% distinct(crash_id, .keep_all = TRUE)
```

## Visualisations

### Histograms

``` r
# Review total fatality breakdown per crash, exclude extreme outliers on x-axis
hist1 <- fatal_unique %>%
  filter(!is.na(number_fatalities)) %>% 
  ggplot(aes(x = number_fatalities)) +
  geom_histogram(binwidth = 1, fill = my_colours[1], alpha = 0.70) +
  scale_x_continuous(lim = c(0, quantile(fatal_unique$number_fatalities, .999, na.rm = TRUE))) +
  scale_y_continuous() +
  labs(x = "Number of Fatalities", y = "Count",
       title = 'Australian Road Fatalities (1989-2019)') +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "bottom",
        axis.text.x = element_text(angle = 0, size = 9),
        strip.text.x = element_text(size = 10, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 8.5, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))
  
# Review count of speed limit zones where a fatality was recorded
hist2 <- fatal_unique %>%
  filter(speed_limit>= 0 & speed_limit <= 130) %>% 
  ggplot(aes(x = speed_limit)) +
  scale_x_continuous(limits = c(10, 130), breaks = seq(10, 130, 10)) +
  geom_histogram(binwidth = 10, fill = my_colours[2], alpha = 0.70) +
  labs(x = "Speed Limit", y = "Count") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "bottom",
        axis.text.x = element_text(angle = 0, size = 9),
        strip.text.x = element_text(size = 10, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 8.5, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

hist1 / hist2
```

<img src=https://github.com/chalg/ardd/blob/master/images/histograms_patchwork.png  />

``` r
ggsave("histograms_patchwork.png", plot = last_plot(), path = "images")
```

In the first plot above it is obvious that the vast majority of total
fatalities involved only one fatality.

From the second plot it is clear that most fatalities occurred in the
60, 80, 100 & 110 km/h speed limit zones, which makes sense considering
those are the most common.

``` r
# Review count of fatalties by year, excluding 2017
hist3 <- fatal %>% 
ggplot(aes(x = year)) +
  scale_x_continuous(limits = c(1989, 2019), breaks = seq(1989, 2019, 3)) +
  geom_histogram(binwidth = 1, fill = my_colours[1], alpha = 0.70) +
  labs(x = "Year", y = "Count",
       title = 'Australian Road Fatalities (1989-2019)') +
  # scale_fill_manual(values = my_colours) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "bottom",
        axis.text.x = element_text(angle = 45, size = 9),
        strip.text.x = element_text(size = 10, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 8.5, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

# Review count of fatalties by hour
hist4 <- fatal %>%
  ggplot(aes(x = hour)) +
  scale_x_continuous(limits = c(0, 23), breaks = seq(0, 23, 3)) +
  geom_histogram(binwidth = 1, fill = my_colours[2], alpha = 0.70) +
  labs(x = "Hour", y = "Count") +
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

# Review count of fatalties by Day of the week
bar1 <- ggplot(aes(x = dayweek), data = fatal) +
    geom_bar(fill = my_colours[3], alpha = 0.70) +
  labs(x = "Day of Week", y = "Count") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "bottom",
        axis.text.x = element_text(angle = 00, size = 9),
        strip.text.x = element_text(size = 10, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 8.5, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

(hist3 | hist4) / bar1
```

<img src=https://github.com/chalg/ardd/blob/master/images/histograms2_patchwork.png  />

``` r
ggsave("histograms2_patchwork.png", plot = last_plot(), path = "images")
```

In the first plot above there is a clear downward trend in Australian
road fatality numbers from 1989 to 2019, which is a very favourable
trend.

Note: 2020 was excluded because it is not a full year of data.

The count of fatalities per hour show that most fatalities occur in the
afternoon peak hour.

The count of fatalities per day of the week shows that considerably more
fatalities occur on weekends, which is not surprising. The increase
during the week is interesting and maybe a reflection of increased
activity, drink driving, or general distraction as the weekend
approaches.

### Boxplots

``` r
# Create boxplot to view overall Age and Road_User statistics
box1 <- fatal %>% 
  filter(!is.na(road_user),
         !road_user %in% c("Other/-9"),
         !age == -9) %>% 
  ggplot(aes(x = road_user, y = age)) +
  geom_boxplot(varwidth = F, fill = my_colours[3], alpha = 0.70) +
  coord_flip() +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  stat_summary(fun.y = mean, geom = 'point', shape = 4) +
  labs(y = "Age", x = "Road User") +
  # scale_fill_manual(values = my_colours) +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "bottom",
        axis.text.x = element_text(angle = 0, size = 9),
        strip.text.x = element_text(size = 10, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 8.5, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

# Focus boxplot on Age and Gender statistics for drivers and riders only
box2 <- fatal %>% filter(!gender == "Unknown",
                 !age == -9, !gender == -9,
                 !is.na(number_fatalities),
                 road_user %in% c("Driver", "Motorcyle rider")) %>%
  ggplot(aes(x = gender, y = age)) +
  geom_boxplot(fill = my_colours[1], alpha = 0.70) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  stat_summary(fun.y = mean, geom = 'point', shape = 4) +
  labs(x = "Gender", y = "Age",
       title = 'Australian Road Fatalities (1989-2019)') +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "bottom",
        axis.text.x = element_text(angle = 0, size = 9),
        strip.text.x = element_text(size = 10, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 8.5, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))


# Boxplot on State and Age to see age differences across states
box3 <- fatal %>% filter(!gender == "Unknown",
                 !age == -9, !gender == -9,
                 !is.na(number_fatalities),
                 road_user %in% c("Driver", "Motorcyle rider")) %>%
  ggplot(aes(x = state, y = age)) +
  geom_boxplot(fill = my_colours[2], alpha = 0.70) +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(breaks = seq(0, 100, 10)) +
  stat_summary(fun.y = mean, geom = 'point', shape = 4) +
   labs(x = "State", y = "Age") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "bottom",
        axis.text.x = element_text(angle = 0, size = 9),
        strip.text.x = element_text(size = 10, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 8.5, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

(box2 + box3) / box1
```

<img src="https://github.com/chalg/ardd/blob/master/images/boxplots_patchwork.png"  />

``` r
ggsave("boxplots_patchwork.png", plot = last_plot(), path = "images")
```

``` r
# Always use fatal_unique when aggregating the number_fatalities field, otherwise
# it will be overstated.
fatal_unique %>%
  filter(year < 2020,
         !is.na(number_fatalities)) %>% 
  group_by(year) %>% 
  summarise(sum_fatal = sum(number_fatalities)) %>% #write_csv("oz_road_fatalities.csv")
  ggplot(aes(x = year, y = sum_fatal)) +
  geom_line(col = "purple", size = 1) +
  geom_smooth(method = 'loess', color = 'green') +
  scale_x_continuous(breaks = seq(1989, 2019, 2)) +
  scale_y_continuous(labels = scales::comma) +
  labs(x = "Year", y = "Number of Fatalities",
       title = "Australian Road Fatalities (1989-2019)",
       caption = "Source: @GrantChalmers | Australian Road Deaths Database (ARDD)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "right",
        axis.text.x = element_text(angle = 0, size = 9),
        plot.caption = element_text(size = 9, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))
```

<img src="https://github.com/chalg/ardd/blob/master/images/oz_road_fatalities.png"  />

``` r
ggsave("oz_road_fatalities.png", plot = last_plot(), path = "images")
```

    #> Saving 8 x 6 in image

### Lineplots (trends)

Driver and Motor Cycle Rider Fatalities by Age and Gender.

``` r
# Overall count of fatalities by Age for Driver and Motor Cycle Rider
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
       subtitle = "1989-2019",
       colour = "Gender",
       caption = "Source: @GrantChalmers | Australian Road Deaths Database (ARDD)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "right",
        axis.text.x = element_text(angle = 0, size = 9),
        plot.caption = element_text(size = 9, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))
```

<img src="https://github.com/chalg/ardd/blob/master/images/age_gender_fatalities.png"  />

``` r
ggsave("age_gender_fatalities.png", plot = last_plot(), path = "images")
```

Fatalities by road user, age and gender.

``` r
# Overall count of fatalities by road user, age and gender
fatal %>% filter(!gender == "Unspecified",
                 !road_user == "Other/-9",
                 age >= 16, !gender == -9) %>% 
  group_by(age, gender, road_user) %>% 
  summarise(f_count = n()) %>%    # must use count, otherwise overstated
  ungroup() %>% 
  arrange(age, gender, road_user) %>% 
  ggplot(aes(x = age, y = f_count)) +
  geom_line(aes(colour = road_user), size = 1, alpha = 0.75) +
  scale_x_continuous(breaks = seq(16, 100, 4)) +
  scale_y_continuous(breaks = seq(0, 700, 50)) +
  scale_colour_manual(values = my_colours) +
  facet_wrap(~ gender, scales = "free_y") +
  labs(x = "Age", y = "Number of Fatalities",
       title = 'Australian Road Fatalities by Road User, Age and Gender (1989-2019)',
       colour = "Road User",
       caption = "Source: @GrantChalmers | Australian Road Deaths Database (ARDD)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "bottom",
        axis.text.x = element_text(angle = 90, size = 9),
        strip.text.x = element_text(size = 10, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 8.5, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))
```

<img src="https://github.com/chalg/ardd/blob/master/images/road_user_age_gender_fatalities.png"  />

``` r
ggsave("road_user_age_gender_fatalities.png", plot = last_plot(), path = "images")
```

Australian Road Fatalities by State.

``` r
# Plot line over time with 3 year intervals on the x axis
# because it is incomplete
fatal_unique %>% 
  group_by(year, state) %>% 
  summarise(sum_fatal_state_yr = sum(number_fatalities)) %>%
  ggplot(aes(x = year, y = sum_fatal_state_yr)) +
  geom_line(col = "purple", size = 1) +
  geom_smooth(method = 'loess', color = 'green') +
  scale_x_continuous(breaks = seq(1989, 2019, 3), limits = c(1989, 2019)) + 
  facet_wrap( ~ state, scales = "free_y", ncol = 2) +
  labs(x = "Year", y = "Number of Fatalities",
       title = 'Australian Road Fatalities by State (1989-2019)',
       caption = "Source: @GrantChalmers | Australian Road Deaths Database (ARDD)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "right",
        axis.text.x = element_text(angle = 45, size = 9),
        strip.text.x = element_text(size = 9, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 9, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))
```

<img src="https://github.com/chalg/ardd/blob/master/images/fatalities_by_state.png"  />

``` r
ggsave("fatalities_by_state.png", plot = last_plot(), path = "images")
```

The plot above shows the number of fatalities per year split by state.
Here we can see a downward trend in every state, although some trends
are more consistent than others. The ACT, NT and TAS seem to be more
variable, whereas, NSW, VIC and SA look more smooth and linear.

Note: the y scale is different for each plot based on population and
fatalities.

### Bespoke Visualisations

``` r
# Create new dataframe to hold summarised state fatalities
fatal_state <- fatal_unique %>% 
  group_by(state) %>%
  summarise(sum_fatal_state = sum(number_fatalities)) %>%
  ungroup() %>% 
  mutate(state = toupper(state)) %>%    # consistency for joining
  arrange(state)

glimpse(fatal_state)
```

    #> Observations: 8
    #> Variables: 2
    #> $ state           <chr> "ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC",...
    #> $ sum_fatal_state <dbl> 462, 15832, 1586, 10021, 4391, 1494, 11205, 6010

``` r
fatal_state_apop <- fatal_state %>% 
  left_join(est_pop) %>% 
  left_join(state_area) %>% glimpse()
```

    #> Joining, by = "state"
    #> Joining, by = "state"

    #> Observations: 8
    #> Variables: 4
    #> $ state           <chr> "ACT", "NSW", "NT", "QLD", "SA", "TAS", "VIC",...
    #> $ sum_fatal_state <dbl> 462, 15832, 1586, 10021, 4391, 1494, 11205, 6010
    #> $ est_pop         <dbl> 426709, 8089526, 245869, 5095100, 1751693, 534...
    #> $ area            <dbl> 2358.2, 800810.8, 1348094.3, 1730172.1, 984274...

``` r
# Create variable to contain the number of years to help calculate yearly
# fatality rate. Remove 1 year as 2017 is incomplete
years <- max(fatal_unique$year) - min(fatal_unique$year) - 1

# Add fatality rate per 100,000 people per year and estimated population density.
fatal_state_apop <- fatal_state_apop %>% 
  mutate(fatal_rate = sum_fatal_state / est_pop * 10 ^ 5 / years,
         est_pop_density = est_pop / area,
         mean_fatal_rate = mean(fatal_rate))

# Review fatality rate per state
plt1 <- fatal_state_apop %>% 
  
  ggplot(aes(x = state, y = fatal_rate)) +
  scale_y_continuous(breaks = seq(0, 25, 5)) +
  geom_bar(stat = 'identity', fill = my_colours[5]) +
  geom_hline(aes(yintercept = mean_fatal_rate), colour="#BB0000",
             linetype="dashed") +
  annotate("text", x = "WA", y = fatal_state_apop$mean_fatal_rate + 0.5,
           label = paste0("Mean == ", round(fatal_state_apop$mean_fatal_rate, 3)),
           parse = TRUE, size = 2.5, colour = "grey40") +
  scale_fill_manual(values = my_colours) +
  labs(x = "State", y = "Fatalities per year for every 100,000 people",
       title = 'Australian Road Fatality Rate by State (1989-2019)') +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "right",
        axis.text.x = element_text(angle = 0, size = 9),
        strip.text.x = element_text(size = 9, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 9, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))
  

plt2 <- fatal_state_apop %>% 
  
  ggplot(aes(x = fatal_rate, y = est_pop_density)) +
  scale_y_continuous(trans = log10_trans()) +
  geom_point(aes(colour = fatal_rate), size = 5, alpha = 0.70) + 
  geom_text(aes(label = state), hjust = 0, vjust = 0) +
  geom_line(colour = my_colours[5], linetype = 2) +
  scale_colour_gradient(low = my_colours[3], high = my_colours[4]) +
  labs(x = "Fatalities per year for every 100,000 people",
       y = "Est. Population Density (persons/km2) log scale",
       title = 'Est. Population Density by Fatality Rate',
       caption = "Source: @GrantChalmers | Australian Road Deaths Database (ARDD)") +
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "none",
        axis.text.x = element_text(angle = 0, size = 9),
        strip.text.x = element_text(size = 9, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 7, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))
  
# Combine plots using patchwork
plt <- plt1 + plt2
plt
```

<img src="https://github.com/chalg/ardd/blob/master/images/fatality_rate.png"  />

``` r
# Save file and increase dimensions so all labels fit.
ggsave("fatality_rate.png", path = "images", plot = last_plot(), width = 10, height = 6)
```

Population density is calculated based on ABS estimated population (link
provided earlier) and land area for each state (Population Estimates by
Statistical Area Level 2, 2005 to 2015)
[here](http://www.abs.gov.au/AUSSTATS/abs@.nsf/DetailsPage/3218.02014-15?OpenDocument).

-----

# Check to see if fatalities went down in March 2020 due to COVID-19 lockdown.
# Nothing noticeable.
# Overall
fatal_unique %>%
  filter(month == 4) %>% 
  group_by(year, month) %>% 
  summarise(sum_fatal_state_yr = sum(number_fatalities)) %>%
  ggplot(aes(x = year, y = sum_fatal_state_yr)) +
  geom_line(col = "purple", size = 1) +
  geom_point() +
  geom_smooth(method = 'loess', color = 'green') +
  scale_x_continuous(breaks = seq(1989, 2020, 1), limits = c(1989, 2020)) + 
  # facet_wrap( ~ state, scales = "free_y", ncol = 2) +
  labs(x = "Year", y = "Number of Fatalities",
       title = 'Australian Road Fatalities in April (1989-2020)',
       # subtitle = "Month of March totals",
       caption = "Source: @GrantChalmers | Australian Road Deaths Database (ARDD)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "right",
        axis.text.x = element_text(angle = 90, size = 9),
        strip.text.x = element_text(size = 9, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 9, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

ggsave("ardd_april_totals.png", path = "images", width = 6, height = 4)

# Will be very interesting to look at the easter_period next month as the
# categorical fill variable!!

my_colours <- c("Single"     = "gold2",
                "Pedestrian" = "peru",
                "Multiple"   = "#CC79A7")

fatal_unique %>%
  filter(month == 12) %>% 
         # easter_period == "Yes") %>% 
  group_by(year, month, crash_type) %>% 
  summarise(sum_fatal_yr = sum(number_fatalities)) %>%# view()
  ggplot(aes(x = year, y = sum_fatal_yr, fill = crash_type)) +
  geom_col(col = "grey", size = 0.5, alpha = 0.9) +
  geom_text(aes(label = sum_fatal_yr), size = 2, position = position_stack(vjust = 0.5)) +
  # geom_point() +
  # geom_smooth(method = 'loess', color = 'green') +
  scale_x_continuous(breaks = seq(1989, 2020, 1)) +
  scale_fill_manual(values = my_colours) +
  # facet_wrap( ~ state, scales = "free_y", ncol = 2) +
  labs(x = "Year", y = "Number of Fatalities",
       title = 'Australian Road Fatalities in December (1989-2020)',
       fill = "Crash type",
       # subtitle = "Month of March totals",
       caption = "Source: @GrantChalmers | Australian Road Deaths Database (ARDD)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = c(0.70, 0.85),
        legend.direction = "horizontal",
        axis.text.x = element_text(angle = 90, size = 9),
        strip.text.x = element_text(size = 9, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 9, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

ggsave("ardd_dec_totals_crash_type.png", path = "images", width = 6, height = 4)

fatal_unique %>%
  filter(month == 4) %>% 
  group_by(year, month, state) %>% 
  summarise(sum_fatal_state_yr = sum(number_fatalities)) %>% 
  arrange(sum_fatal_state_yr) %>% 
  head(10)

# By State
fatal_unique %>%
  filter(month == 4) %>% 
  group_by(year, month, state) %>% 
  summarise(sum_fatal_state_yr = sum(number_fatalities)) %>%
  ggplot(aes(x = year, y = sum_fatal_state_yr)) +
  geom_line(col = "purple", size = 1) +
  geom_smooth(method = 'loess', color = 'green') +
  scale_x_continuous(breaks = seq(1989, 2020, 1), limits = c(1989, 2020)) + 
  facet_wrap( ~ state, scales = "free_y", ncol = 2) +
  labs(x = "Year", y = "Number of Fatalities",
       title = 'Australian Road Fatalities by State in April (1989-2020)',
       caption = "Source: @GrantChalmers | Australian Road Deaths Database (ARDD)") +
  theme(plot.title = element_text(size = 12, face = "bold"),
        plot.subtitle = element_text(size = 10, color = "gray20"),
        axis.title = element_text(size = 10), legend.position = "right",
        axis.text.x = element_text(angle = 90, size = 9),
        strip.text.x = element_text(size = 9, colour = "darkgreen", face = "bold"),
        plot.caption = element_text(size = 9, color = "gray50", face = "italic"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

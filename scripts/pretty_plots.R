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
       subtitle = "Statistics for <span style='color:#56B4E9'>Males</span> and <span style='color:#CC79A7'>Females</span>, 1989-2023",
       # subtitle = "1989-2023",
       colour = "Gender",
       caption = "DatViz: @GrantChalmers | Source: Australian Road Deaths Database (ARDD)
       https://data.gov.au/dataset/ds-dga-5b530fb8-526e-4fbf-b0f6-aa24e84e4277/details?q=crash") +
  theme(plot.title = element_text(color = "gray12", family = "oswald"),
        # plot.subtitle = element_text(size = 10, color = "gray20"),
        plot.subtitle = element_markdown(face = "bold", size = 7.5, family = "cabin"),
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
       subtitle = "Statistics for <span style='color:#56B4E9'>Males</span> and <span style='color:#CC79A7'>Females</span>, 1989-2023",
       # subtitle = "1989-2023",
       colour = "Gender",
       caption = "DatViz: @GrantChalmers | Source: Australian Road Deaths Database (ARDD)
       https://data.gov.au/dataset/ds-dga-5b530fb8-526e-4fbf-b0f6-aa24e84e4277/details?q=crash") +
  theme(plot.title = element_text(color = "gray12", family = "oswald"),
        # plot.subtitle = element_text(size = 10, color = "gray20"),
        plot.subtitle = element_markdown(face = "bold", size = 7.5, family = "cabin"),
        axis.title = element_text(size = 10), legend.position = "none",
        axis.text.x = element_text(angle = 0, size = 9),
        plot.caption = element_text(size = 7.25, color = "gray50", family = "oswald"),
        legend.title = element_text(size = 9), legend.text = element_text(size = 9))

ggsave("age_gender_driver_fatalities_pretty.png", path = "images", width = 7, height = 5)


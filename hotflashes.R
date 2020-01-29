library(lubridate)
library(tidyverse)
library(sugrrants)

source(here::here("setup.R"))

# read in data
hotflashes <- read_csv(here::here("data", "hotflashes.csv")) %>%
  mutate(
    datetime = parse_date_time(datetime, orders = "mdyHM"),
    hour = hour(datetime),
    hour_fact = factor(
      hour,
      levels = 0:23,
      labels = c("midnight", paste0(1:11, "am"), "noon", paste0(1:11, "pm"))
    ),
    weekday = wday(datetime, label = TRUE),
    date = date(datetime)
  )

hotflashes <- hotflashes %>%
  filter(between(date, mdy("07-01-2019"), mdy("09-30-2019")))

hourly <- hotflashes %>%
  group_by(date, hour) %>%
  tally() %>%
  ungroup() %>%
  complete(date, hour) %>%
  mutate(n = ifelse(is.na(n), 0, n))

max_hr <- max(hourly$n)

daily <- hourly %>%
  group_by(date) %>%
  summarise(tot = sum(n)) %>%
  right_join(hourly, by = "date") %>%
  crossing(y = seq(.5, max_hr + .5, 1))

dis_vals <- daily %>%
  select(date, tot) %>%
  distinct()

# polar coordinates plot
ggplot(hotflashes) +
  geom_bar(aes(hour_fact, fill = weekday)) +
  coord_polar() +
  scale_x_discrete(drop = FALSE, name = NULL) +
  geom_vline(xintercept = seq(0.5, 23.5), col = "#F282BC", alpha = 0.5) +
  scale_fill_brewer(palette = "PuRd", direction = -1) +
  theme_bc_aware() +
  theme(
    panel.grid.major.x = element_blank(),
    axis.text.y = element_blank()
  ) +
  ylab(NULL) +
  ggtitle("Hot flashes by time of day and day of week")

grid::grid.raster(ribbon,
                  x = .95, y = .95,
                  just = c("right", "top"),
                  width = unit(2, "inches")
)

grid::grid.raster(logo,
                  x = 1, y = 0,
                  just = c("right", "bottom"),
                  width = unit(1.5, "inches")
)

# calendar plots (separately, combined manually)
daily %>%
  filter(between(date, mdy("07-01-2019"), mdy("07-31-2019"))) %>%
  ggplot() +
  geom_raster(aes(x = hour, y = y, fill = tot)) +
  geom_line(aes(x = hour, y = n), col = "#333333") +
  geom_text(
    data = dis_vals, x = 11.5, y = max_hr + .5, col = "#333333",
    aes(label = tot)
  ) +
  facet_calendar(~date, ncol = 1) +
  scale_x_continuous(
    labels = levels(hotflashes$hour_fact)[c(1, 7, 13, 19)],
    breaks = (0:23)[c(1, 7, 13, 19)]
  ) +
  scale_y_continuous(breaks = 0:max_hr, limits = c(0, max_hr + 1)) +
  coord_cartesian(expand = FALSE) +
  labs(y = NULL, title = "Trends in hot flashes over time", x = NULL) +
  scale_fill_distiller(palette = "PuRd", direction = 1) +
  theme_bc_aware() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

daily %>%
  filter(between(date, mdy("08-01-2019"), mdy("08-31-2019"))) %>%
  ggplot() +
  geom_raster(aes(x = hour, y = y, fill = tot)) +
  geom_line(aes(x = hour, y = n), col = "#333333") +
  geom_text(
    data = dis_vals, x = 11.5, y = max_hr + .5, col = "#333333",
    aes(label = tot)
  ) +
  facet_calendar(~date, ncol = 1) +
  labs(y = NULL, x = NULL, title = " ") +
  scale_x_continuous(
    labels = levels(hotflashes$hour_fact)[c(1, 7, 13, 19)],
    breaks = (0:23)[c(1, 7, 13, 19)]
  ) +
  scale_y_continuous(breaks = 0:max_hr, limits = c(0, max_hr + 1)) +
  coord_cartesian(expand = FALSE) +
  scale_fill_distiller(palette = "PuRd", direction = 1) +
  theme_bc_aware() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

daily %>%
  filter(between(date, mdy("09-01-2019"), mdy("09-30-2019"))) %>%
  ggplot() +
  geom_raster(aes(x = hour, y = y, fill = tot)) +
  geom_line(aes(x = hour, y = n), col = "#333333") +
  geom_text(
    data = dis_vals, x = 11.5, y = max_hr + .5, col = "#333333",
    aes(label = tot)
  ) +
  labs(y = NULL, x = NULL, title = " ") +
  facet_calendar(~date, ncol = 1) +
  scale_x_continuous(
    labels = levels(hotflashes$hour_fact)[c(1, 7, 13, 19)],
    breaks = (0:23)[c(1, 7, 13, 19)]
  ) +
  scale_y_continuous(breaks = 0:max_hr, limits = c(0, max_hr + 1)) +
  coord_cartesian(expand = FALSE) +
  scale_fill_distiller(palette = "PuRd", direction = 1) +
  theme_bc_aware() +
  theme(legend.position = "none", axis.text.x = element_text(angle = 45, hjust = 1))

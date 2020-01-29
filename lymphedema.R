library(tidyverse)

source(here::here("setup.R"))

arms <- tribble(
  ~"date", ~"meas right", ~"meas left", ~"calc left", ~"calc right", ~"param left", ~"param right", ~"ldex", ~"res left", ~"res right",
  "2018-07-31", "15.4/15.7/18/20.5/23.3/25/25/24/25/26.5/27.7/29.2", "15.8/16/17.5/19.8/22.5/24/24.8/24/25/26.5/28/29.1", 1874, 1909, 2878, 3017, -4.2, 331.3, 304.2,
  "2018-09-24", "16/16/18/20.5/23.5/25/25.6/25/26.2/27/29/29", "15.3/16.5/17.8/20.7/23/24.5/25.5/25.5/26.5/28/29.2/31", 2032, 1993, 2662, 2809, -6.1, 303.4, 287.1,
  "2019-01-10", "15.3/17/18.5/21.9/24/25.2/25.2/24.4/25.5/27/28.5/30", "15.5/16.1/18.5/20.8/23/24/24.5/24.5/26/27/28.5/30", 1950, 2008, 2411, 2531, -2.1, 313, 291.9,
  "2019-04-10", "15.6/17/19.1/21/24/24.5/24.3/25/27.5/29.1/31.5/33.3", "15.2/17.4/19.7/22.7/24.7/25.5/24.8/25.1/27/28.5/30.7/32.5", NA, NA, 3133, 3314, 2, 374.5, 368.7,
  "2019-08-14", "15.7/16.9/19/21/23/24/24/24.8/27/29.2/31.5/33.5", "15.5/17/19.4/22/24.7/25/24.5/25.5/27/29/30.5/33", NA, NA, 3253, 3358, -0.5, 411.8, 395
) %>%
  janitor::clean_names() %>%
  mutate(
    date = parse_date(date),
    inc = paste(1:12, collapse = "/")
  ) %>%
  separate_rows(meas_right, meas_left, inc, convert = TRUE) %>%
  pivot_longer(-c(date, ldex, inc),
               names_to = c(".value", "side"),
               names_sep = "_",
               values_drop_na = FALSE
  ) %>%
  mutate(
    rcent = meas / 2,
    lcent = -rcent
  ) %>%
  group_by(date, side) %>%
  mutate(
    lag_meas = lag(meas),
    val = 4 * (meas^2 + meas * lag_meas + lag_meas^2)
  ) %>%
  mutate(new_vol = sum(val, na.rm = TRUE) / (12 * pi)) %>%
  arrange(inc) %>%
  ungroup()


vis <- arms %>%
  group_by(date, side) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  mutate(
    y = as.numeric(factor(date)) * 2 + 35,
    x = as.numeric(factor(date)) * .75,
    x = ifelse(side == "left", -x, x)
  )

ggplot(arms) +
  geom_path(aes(rcent/pi, inc * 4, col = factor(date), group = date)) +
  geom_path(aes(lcent/pi, inc * 4, col = factor(date), group = date)) +
  geom_label(
    data = vis,
    aes(
      x = x, y = y, col = factor(date),
      label = paste0(round(new_vol, 0), " mL")
    ),
    show.legend = FALSE
  ) +
  facet_grid(cols = vars(side)) +
  scale_color_manual(values = purps[-c(1:2)], name = "date") +
  labs(
    x = "cm from center",
    y = "cm from wrist",
    title = "Arm measurements for lymphedema monitoring"
  ) +
  theme_bc_aware() +
  theme(
    panel.grid = element_blank(),
    legend.position = "top"
  )

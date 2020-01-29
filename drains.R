library(lubridate)
library(tidyverse)
library(RcppRoll)
library(ggrepel)

source(here::here("setup.R"))

drains <- tribble(
  ~date, ~time, ~one, ~two, ~three,
  "08/07/2018", "6:25 pm", 28, 15, 25,
  "08/08/2018", "10:30 am", 45, 35, 30,
  "08/08/2018", "6:30 pm", 15, 10, 15,
  "08/09/2018", "10:45 am", 25, 10, 20,
  "08/09/2018", "9:30 pm", 13, 5, 20,
  "08/10/2018", "10:30 am", 10, 10, 9,
  "08/10/2018", "9:30 pm", 5, 7, 7,
  "08/11/2018", "9:30 am", 5, 15, 5,
  "08/11/2018", "9:30 pm", 9, 7, 6,
  "08/12/2018", "9:45 am", 4, 20, 21,
  "08/12/2018", "9:45 pm", 8, 5, 20,
  "08/13/2018", "7:30 am", 5, 13, 15,
  "08/13/2018", "9:45 pm", NA, 4, 20,
  "08/14/2018", "11:10 am", NA, 11, 20,
  "08/14/2018", "9:00 pm", NA, 4, 24,
  "08/15/2018", "9:45 pm", NA, 11, 36,
  "08/16/2018", "9:15 pm", NA, 11, 27,
  "08/17/2018", "9:30 pm", NA, 10, 13,
  "08/18/2018", "10:15 pm", NA, 10, 12,
  "08/19/2018", "9:45 pm", NA, 7, 12,
  "08/20/2018", "10:10 pm", NA, 6, 11,
  "08/22/2018", "10:30 pm", NA, 16, 19,
  "08/23/2018", "10:00 pm", NA, 18, 20,
  "08/24/2018", "10:45 pm", NA, 13, 9,
  "08/25/2018", "1:00 pm", NA, 40, 15,
  "08/25/2018", "10:10 pm", NA, 14, 6,
  "08/26/2018", "9:30 pm", NA, 10, 13,
  "08/27/2018", "11:00 pm", NA, 7, 14,
  "08/28/2018", "10:30 pm", NA, NA, 20,
  "08/29/2018", "11:30 pm", NA, NA, 16,
  "08/30/2018", "11:00 pm", NA, NA, 16
) %>%
  unite(date_time, c(date, time), sep = " ") %>%
  mutate(
    date_time = mdy_hm(date_time),
    prev_time = lag(date_time),
    diff_time = as.numeric(date_time - prev_time, units = "hours")
  )

drains$diff_time[1] <- 6

drains <- drains %>%
  mutate(
    rate1 = one / diff_time,
    rate2 = two / diff_time,
    rate3 = three / diff_time,
    one_tot = cumsum(one),
    two_tot = cumsum(two),
    three_tot = cumsum(three)
  )

times <- data.frame(date_time = seq(
  from = drains$date_time[1] - hours(6) + minutes(5),
  to = drains$date_time[nrow(drains)],
  by = "30 min"
))

drains_long <- full_join(drains, times) %>%
  arrange(date_time) %>%
  fill(rate1:rate3, .direction = "up") %>%
  mutate(
    avg1 = 24 * roll_mean(rate1, n = 48, align = "right", fill = T),
    avg2 = 24 * roll_mean(rate2, n = 48, align = "right", fill = T),
    avg3 = 24 * roll_mean(rate3, n = 48, align = "right", fill = T)
  ) %>%
  mutate(
    avg1 = case_when(
      near(avg1, 24) ~ rate1 * 24,
      TRUE ~ avg1
    ),
    avg2 = case_when(
      near(avg2, 24) ~ rate2 * 24,
      TRUE ~ avg2
    ),
    avg3 = case_when(
      near(avg3, 24) ~ rate3 * 24,
      TRUE ~ avg3
    )
  ) %>%
  gather(avg1:avg3, key = "drain", value = "avg")


drains_cum <- drains %>%
  gather(one_tot:three_tot, key = "drain", value = "total") %>%
  mutate(
    drain = fct_recode(drain,
                       "1" = "one_tot",
                       "2" = "two_tot",
                       "3" = "three_tot"
    ),
    Drain = fct_relevel(drain, "3", after = Inf)
  )

drains_ind <- drains %>%
  gather(one:three, key = "drain", value = "mL") %>%
  mutate(
    drain = fct_recode(drain,
                       "1" = "one",
                       "2" = "two",
                       "3" = "three"
    ),
    Drain = fct_relevel(drain, "3", after = Inf)
  )

ggplot(data = filter(drains_long, date_time > ymd_hms("2018-08-08 11:30:00"))) +
  geom_line(aes(date_time, avg, col = drain)) + ylim(0, 80) +
  ylab("24-hour average (mL/day)") + xlab("Date") +
  scale_color_manual(
    values = purps[c(3, 5, 7)],
    name = "Drain", labels = c("1", "2", "3")
  ) +
  ggtitle("Average drain output over previous 24 hours") +
  theme_bc_aware()

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

ggplot(data = drains_cum) + geom_line(aes(date_time, total, col = Drain)) +
  ylab("Total drainage (mL)") + xlab("Date") +
  ggtitle("Total drain output (since discharge)") +
  scale_color_manual(
    values = purps[c(3, 5, 7)],
    name = "Drain", labels = c("1", "2", "3")
  ) +
  theme_bc_aware()

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

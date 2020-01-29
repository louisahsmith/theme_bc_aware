library(tidyverse)
library(lubridate)
library(ggrepel)
library(gganimate)

source(here::here("setup.R"))

claims <- read_csv(here::here( "data", "claims.csv")) %>%
  select(-X1)

# add some more from 2018 that were added since I scraped that data
claims <- tribble(
  ~date, ~type, ~total_vals,
  "11/07/18", "Medical", "Total $0.00 $16,033.24 $5,496.82",
  "11/08/18", "Medical", "Total $0.00 $33.00 $123.51",
  "11/14/18", "Medical", "Total $0.00 $4,044.80 $1,296.95",
  "11/26/18", "Medical", "Total $35.00 $200.00 $105.09",
  "11/28/18", "Medical", "Total $0.00 $16,413.24 $5,645.29",
  "11/28/18", "Pharmacy", "Total $14.30 $119.99 $0.00",
  "12/06/18", "Pharmacy", "Total $0.00 $355.99 $15.13",
  "12/09/18", "Pharmacy", "Total $1.53 $11.99 $0.00",
  "12/12/18", "Pharmacy", "Total $2.84 $11.99 $0.00",
  "12/12/18", "Medical", "Total $0.00 $3,861.80 $1,295.67",
  "12/17/18", "Medical", "Total $0.00 $1,611.00 $874.86",
  "12/19/18", "Medical", "Total $0.00 $16386.24 $5531.48"
) %>%
  bind_rows(claims)

claims <- claims %>% filter(type != "Dental")

claims_dat <- claims %>%
  mutate(total_vals = str_remove(total_vals, "Total ")) %>%
  separate(total_vals, sep = " ", into = c("owed", "charged", "covered")) %>%
  mutate_at(vars(owed:covered), parse_number) %>%
  arrange(date) %>%
  mutate(
    date = mdy(date),
    type = factor(type),
    cum_charged = cumsum(charged),
    cum_covered = cumsum(covered),
    cum_owed = cumsum(owed)
  ) %>%
  group_by(type) %>%
  mutate(
    cum_charged_type = cumsum(charged),
    cum_covered_type = cumsum(covered),
    cum_owed_type = cumsum(owed),
    owed_type = sum(owed, na.rm = TRUE),
    charged_type = sum(charged, na.rm = TRUE),
    covered_type = sum(covered, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  add_count(date) %>%
  mutate(cum_count = cumsum(n)) %>%
  ungroup()

bills_total <- claims_dat %>%
  gather(cum_charged:cum_owed, key = "category", value = "cumsum") %>%
  mutate(category = factor(category))

bills_type <- claims_dat %>%
  gather(cum_charged_type:cum_owed_type, key = "category", value = "cumsum_type") %>%
  mutate(category = fct_recode(category,
                               "cum_charged" = "cum_charged_type",
                               "cum_covered" = "cum_covered_type",
                               "cum_owed" = "cum_owed_type"
  ))

bills_long <- claims_dat %>%
  left_join(bills_total) %>%
  left_join(bills_type)

dates <- tribble(
  ~date, ~event,
  "03/07/2018", "Started chemo",
  "04/04/2018", "Switched chemo regimens",
  "06/27/2018", "Switched back",
  "07/11/2018", "Final chemo",
  "08/06/2018", "Surgery",
  "09/17/2018", "Radiation mapping",
  "10/16/2018", "Final radiation",
  "12/17/2018", "Second surgery"
) %>%
  mutate(date = mdy(date)) %>%
  left_join(y = filter(bills_long, type == "Medical", category == "cum_charged"), by = "date") %>%
  select(c(date, event, cumsum)) %>%
  group_by(event) %>%
  top_n(n = 1, wt = cumsum)

final_vals <- bills_long %>%
  filter(date == max(date))

ggplot(data = bills_long) +
  geom_line(aes(x = date, y = cumsum, col = category)) +
  geom_text_repel(
    data = dates, aes(x = date, y = cumsum, label = event),
    hjust = "inward", col = purps[7]
  ) +
  geom_label(data = final_vals, aes(
    x = date, y = cumsum,
    label = scales::dollar(cumsum),
    col = category
  ), hjust = "outward", show.legend = FALSE) +
  coord_cartesian(clip = "off") +
  labs(y = NULL, x = NULL, title = "Cumulative medical expenses, 2018") +
  scale_color_manual(
    values = purps[c(7, 5, 3)], name = NULL,
    labels = c("Hospital charged", "Insurance paid", "I paid")
  ) +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "2 months", date_labels = "%b") +
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

# animated graph
bills_long$cumsum_dol <- scales::dollar(bills_long$cumsum)

p_anim <- ggplot(bills_long, aes(date, cumsum, group = category, col = category)) +
  geom_line() +
  transition_reveal(date) +
  geom_segment(aes(xend = max(bills_long$date), yend = cumsum),
               linetype = 2, colour = "grey"
  ) +
  geom_point(size = 2) +
  geom_text(aes(
    x = max(bills_long$date),
    label = cumsum_dol, col = category
  ), hjust = 0) +
  coord_cartesian(clip = "off") +
  labs(title = "2018 Medical Bills", y = NULL, x = NULL) +
  theme_minimal() +
  scale_y_continuous(labels = scales::dollar) +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  scale_color_brewer(type = "qual", palette = 1, name = NULL, labels = c("Charged", "Covered", "Owed")) +
  theme(plot.margin = margin(5.5, 40, 5.5, 5.5))

#install.packages("gifski")
gif <- animate(p_anim,
               fps = 5,
               renderer = gifski_renderer(loop = TRUE),
               ref_frame = -1
)

gif
